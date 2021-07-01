package Components.MicroStructures

import Components.Particles.UnitSpeedParticle
import breeze.linalg._

import scala.util.{Failure, Success, Try}

case class PlaneSegment(
                         corner_a: DenseVector[Double],
                         corner_b: DenseVector[Double],
                         corner_c: DenseVector[Double],
                         corner_d: DenseVector[Double],
                         staticIdxVec: DenseVector[Double],
                         billiardCellLength: Double
                       ) extends MicroStructureSegment {

  val planeUnitNormal: DenseVector[Double] = cross(corner_b - corner_a, corner_c - corner_b)

  override def getTimeToCollide(V: UnitSpeedParticle): Double = {

    // describe plane as a vector X = CORNER_A + m*Q1 + n*Q2
    val Q1 = corner_a - corner_b
    val Q2 = corner_a - corner_c

    // particle described as Y = ORIGIN - t*DIRECTION
    // time to collision is t in: [Q1|Q2|-DIRECTION] * [m, n, t]^T = [ORIGIN - CORNER_A]
    // set: j = [Q1|Q2|-DIRECTION]  and let k denote its inverse
    val O_rhs = V.origin - corner_a
    val j = DenseMatrix(Q1, Q2, -1D*V.pathDirection)

    Try(inv(j)) match {
      // no collision: return 0
      case Failure(_) => 0D
      // get t
      case Success(k) => val result = k * O_rhs; result(2)
    }
  }


  // "pass through" reflection
  override def getExitVector(V: UnitSpeedParticle): UnitSpeedParticle = {

    val t = getTimeToCollide(V)
    // t needs to be nonzero
    require(t > 10e-4)

    val shiftOrigin = V.origin - (billiardCellLength *:* staticIdxVec)
    val shiftEnd = V.endpoint - (billiardCellLength *:* staticIdxVec)

    new UnitSpeedParticle(origin = shiftOrigin, endpoint = shiftEnd)
  }


}

object PlaneSegment {

}