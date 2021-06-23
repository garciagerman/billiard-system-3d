package Components
import Particle._
import scala.util.{Try, Failure, Success}
import breeze.linalg._

case class PlaneSegment(
                         point: DenseVector[Double],
                         P: Particle,
                         Q: Particle,
                         staticIdxVec: DenseVector[Double],
                         billiardCellLength: Double
                       ) extends MicroStructureSegment {

  val unit_normal: Particle = cross_product(P, Q).unitVector

//  val Vector(n1, n2, n3) = unit_normal.direction.toVector
//  val Vector(p1, p2, p3) = P.direction
//  val Vector(q1, q2, q3) = Q.direction

  override def getTimeToCollide(V: Particle): Double = {

    val A = DenseVector(P.endpoint.toArray)
    val B = DenseVector(Q.endpoint.toArray)
    val C = DenseVector(point.toArray)

    val q1 = A - B
    val q2 = A -C
    val Dir = DenseVector(V.direction.toArray)
    val m = DenseVector(V.origin.toArray) - DenseVector(point.toArray)

    val J = DenseMatrix(q1, q2, -1D*Dir)
    val J_inverse = Try(inv(J))

    J_inverse match {
      case Failure(_) => 0D
      case Success(i) => {
        val result: DenseVector[Double] = i*m
        result(2)
      }
    }
  }


  // "pass through" reflection
  override def getExitVector(V: Particle): Particle = {

    val t = getTimeToCollide(V)
    // t needs to be nonzero
    require(t > 10e-4)

    //val Array(o1, o2, o3) = V.origin.toArray
    //val shiftOrigin = V.origin.zipWithIndex.map(e => if (e._2 == staticIdxVec) e._1 - billiardCellLength else e._1)
    //val shiftEnd = V.endpoint.zipWithIndex.map(e => if (e._2 == staticIdxVec) e._1 - billiardCellLength else e._1)
    val shiftOrigin = V.origin - (billiardCellLength *:* staticIdxVec)
    val shiftEnd = V.endpoint - (billiardCellLength *:* staticIdxVec)

    new Particle(origin = shiftOrigin, endpoint = shiftEnd)
  }


}

object PlaneSegment {

}