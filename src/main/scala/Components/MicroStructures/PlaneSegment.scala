package Components.MicroStructures

import Components.Particles.UnitSpeedParticle
import Common.Helpers._
import breeze.linalg._

import scala.util.Try

/**
 * A plane segment defined by the area outlined by four points O, P, Q, R.
 *
 * @param pointO
 * @param pointP
 * @param pointQ
 * @param pointR
 * @param billiardCellLength the length of the billiard cell this segment is contained in
 * @param collisionDirection either 1 or -1 to determine the direction of particle paths after colliding with this segment
 */
case class PlaneSegment(
                         pointO: DenseVector[Double],
                         pointP: DenseVector[Double],
                         pointQ: DenseVector[Double],
                         pointR: DenseVector[Double],
                         billiardCellLength: Double,
                         collisionDirection: Double = 1
                       ) extends MicroStructureSegment {
  import PlaneSegment._

  private val PQ = pointQ - pointP
  private val PR = pointR - pointP
  private val QR = pointR - pointQ
  val normalVector: DenseVector[Double] = cross(PQ, PR)

  val constantDim: Int = idxOfConstantDim(pointO, pointP, pointQ, pointR)

  // max min xyz in plane
  val (minRange, maxRange) = planeMinMax(pointO, pointP, pointQ, pointR)

  /**
   * Helper function to double-check collision points
   * @param path an incoming particle path
   * @return boolean if the endpoint of the path is within the segment
   */
  def pathEndpointIsInPlane(path: UnitSpeedParticle): Boolean = {
    val ep = path.endpoint.toArray
    if (
      ep.zip(minRange.toArray).map{case (v, minV) => (minV <= v) || withinTolerance(minV, v)}.reduce(_ && _)
        &&
      ep.zip(maxRange.toArray).map{case (v, maxV) => (v <= maxV) || withinTolerance(maxV, v)}.reduce(_ && _)
    ) true
    else false
  }

  /**
   *
   * @param path incoming particle path
   * @return the time until collision with segment if it exists and is positive. otherwise, throws exception
   */
  override def getTimeToCollision(path: UnitSpeedParticle): Double = {
    val tNumerator = normalVector dot (pointP - path.origin)
    val tDenumerator = normalVector dot path.pathDirection

    // check for zeroes
    if (withinTolerance(tDenumerator, 0D) || withinTolerance(tNumerator, 0D)) throw NoValidCollision("No solution for t")

    val timeToCollision = tNumerator / tDenumerator

    // assure t is positive
    if (timeToCollision < 0) throw NoValidCollision("Collision time is not positive")

    timeToCollision
  }

  /**
   * Depending on the specification of collisionDirection for the wall segment, either the pass-through or shifted particle direction is returned
   * Here we always specify the shifted particle direction in order to mimic periodicity in the billiard cell
   * @param path incoming particle path
   * @param timeToCollision the time-to-collision with segment
   * @return the post-collision vector (as a particle object)
   */
  override def getPostCollisionPath(path: UnitSpeedParticle, timeToCollision: Double): UnitSpeedParticle = {

    val pathAtCollision = path.moveAlongPath(timeToCollision)

    if (!pathEndpointIsInPlane(pathAtCollision)) throw NoValidCollision("Collision point not in plane")

    val collisionPoint = pathAtCollision.endpoint.copy
    val directionAtCollision = pathAtCollision.pathDirection.copy

    collisionPoint.update(constantDim, collisionPoint(constantDim) + collisionDirection*billiardCellLength)
    val postCollisionVectorEndpoint = collisionPoint + (0.5 *:* directionAtCollision)

    val pathToCollision = new UnitSpeedParticle(origin = collisionPoint, endpoint = postCollisionVectorEndpoint)
      .scaledPathToLength(0.1D)

    pathToCollision
  }

}

object PlaneSegment {
  case class SpecificationError(s: String) extends Exception(s)
  case class NoValidCollision(s: String) extends Exception(s)

  /**
   * Finds the index where the plane segment is constant
   * Parameters are the four points specifying the segment
   */
  def idxOfConstantDim(
                        a: DenseVector[Double],
                        b: DenseVector[Double],
                        c: DenseVector[Double],
                        d: DenseVector[Double]): Int = {
    val constantIndexes: Seq[Int] = for (
      i <- 0.until(a.length)
      if (a(i) == b(i)) && (b(i) == c(i)) && (c(i) == d(i)) && (d(i) == a(i))
    ) yield i

    if (constantIndexes.length != 1) throw SpecificationError("No constant dimension for plane")

    constantIndexes.head
  }

  /**
   * Finds the min and max in the x,y,z directions
   * Parameters are the four points specifying the segment
   */
  def planeMinMax(
                   a: DenseVector[Double],
                   b: DenseVector[Double],
                   c: DenseVector[Double],
                   d: DenseVector[Double]
                 ): (DenseVector[Double], DenseVector[Double]) = {
    val M = DenseMatrix(a,b,c,d)

    val minVec: DenseVector[Double] = M(::, *).map(dv => min(dv)).t.toDenseVector
    val maxVec: DenseVector[Double] = M(::, *).map(dv => max(dv)).t.toDenseVector

    (minVec, maxVec)
  }

}