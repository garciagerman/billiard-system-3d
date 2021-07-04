package Components.MicroStructures

import Components.Particles.UnitSpeedParticle
import Common.Helpers._
import breeze.linalg._

import scala.util.Try

case class PlaneSegment(
                         corner_a: DenseVector[Double],
                         corner_b: DenseVector[Double],
                         corner_c: DenseVector[Double],
                         corner_d: DenseVector[Double],
                         billiardCellLength: Double
                       ) extends MicroStructureSegment {
  import PlaneSegment._

  val planeUnitNormal: DenseVector[Double] = cross(corner_b - corner_a, corner_c - corner_b)

  val constantDim: Int = idxOfConstantDim(corner_a, corner_b, corner_c, corner_d)

  // max min xyz in plane
  val (minRange, maxRange) = planeMinMax(corner_a, corner_b, corner_c, corner_d)

  private def pathEndpointIsInPlane(p: UnitSpeedParticle): Boolean = {
    val ep = p.endpoint.toArray
    if (
      ep.zip(minRange.toArray).map{case (v, minV) => (minV <= v) || withinTolerance(minV, v)}.reduce(_ && _)
        &&
      ep.zip(maxRange.toArray).map{case (v, maxV) => (v <= maxV) || withinTolerance(maxV, v)}.reduce(_ && _)
    ) true
    else false
  }

  override def getTimeToCollision(V: UnitSpeedParticle): Double = {
    // describe plane as a vector X = CORNER_A + m*Q1 + n*Q2
    val Q1 = corner_a - corner_b
    val Q2 = corner_a - corner_c

    // particle described as Y = ORIGIN - timeToCollision*DIRECTION
    // time to collision is timeToCollision in: [Q1|Q2|-DIRECTION] * [m, n, timeToCollision]^T = [ORIGIN - CORNER_A]
    // set: j = [Q1|Q2|-DIRECTION]  and let k denote its inverse
    val O_rhs = V.origin - corner_a
    val j = DenseMatrix(Q1, Q2, -1D*V.pathDirection)

    val invertJ = Try(inv(j))

    // check if matrix is invertible
    if (invertJ.isFailure) throw NoValidCollision("Matrix is not invertible")

    val mntVector = invertJ.get * O_rhs
    val timeToCollision = mntVector(2)

    // check if timeToCollision is positive
    if (withinTolerance(timeToCollision, 0D) || timeToCollision <= 0) throw NoValidCollision("Invalid collision time")

    timeToCollision
  }


  // "pass through" reflection
  override def getPostCollisionPath(
                                     V: UnitSpeedParticle,
                                     timeToCollision: Double): UnitSpeedParticle = {
    val pathO = V.origin
    val pathE = V.endpoint

    pathO.update(constantDim, pathO(constantDim) - billiardCellLength)
    pathE.update(constantDim, pathE(constantDim) - billiardCellLength)

    // TODO: this can be generalized
    val pathToCollision = new UnitSpeedParticle(origin = pathO, endpoint = pathE)

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