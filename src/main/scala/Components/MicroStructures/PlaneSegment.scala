package Components.MicroStructures

import Components.Particles.UnitSpeedParticle
import Common.Helpers._
import breeze.linalg._

import scala.util.Try

case class PlaneSegment(
                         pointO: DenseVector[Double],
                         pointP: DenseVector[Double],
                         pointQ: DenseVector[Double],
                         pointR: DenseVector[Double],
                         billiardCellLength: Double
                       ) extends MicroStructureSegment {
  import PlaneSegment._

  private val PQ = pointQ - pointP
  private val PR = pointR - pointP
  private val QR = pointR - pointQ
  val normalVector: DenseVector[Double] = cross(PQ, PR)

  val constantDim: Int = idxOfConstantDim(pointO, pointP, pointQ, pointR)

  // max min xyz in plane
  val (minRange, maxRange) = planeMinMax(pointO, pointP, pointQ, pointR)

  def pathEndpointIsInPlane(p: UnitSpeedParticle): Boolean = {
    val ep = p.endpoint.toArray
    if (
      ep.zip(minRange.toArray).map{case (v, minV) => (minV <= v) || withinTolerance(minV, v)}.reduce(_ && _)
        &&
      ep.zip(maxRange.toArray).map{case (v, maxV) => (v <= maxV) || withinTolerance(maxV, v)}.reduce(_ && _)
    ) true
    else false
  }

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


  // "pass through" reflection
  override def getPostCollisionPath(
                                     V: UnitSpeedParticle,
                                     timeToCollision: Double): UnitSpeedParticle = {
    val pathOrigin = V.origin
    val pathEnd = V.endpoint

    pathOrigin.update(constantDim, pathOrigin(constantDim) - billiardCellLength)
    pathEnd.update(constantDim, pathEnd(constantDim) - billiardCellLength)

    // TODO: this can be generalized
    val pathToCollision = new UnitSpeedParticle(origin = pathOrigin, endpoint = pathEnd)

    if (!pathEndpointIsInPlane(pathToCollision)) throw NoValidCollision("Collision point not in plane")

    pathToCollision
  }

}

object PlaneSegment {
  case class SpecificationError(s: String) extends Exception(s)
  case class NoValidCollision(s: String) extends Exception(s)

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