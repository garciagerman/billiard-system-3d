package Components.Particles

import breeze.linalg._

import scala.util.Try

case class UnitSpeedParticle(origin: DenseVector[Double], endpoint: DenseVector[Double]) {
  val direction: DenseVector[Double] = origin - endpoint

  val square_norm: Double = norm(direction)

  def dot(W: UnitSpeedParticle): Double = direction dot W.direction

  def unitVector: UnitSpeedParticle = {
    val stepsForUnit: Double = Try(1/square_norm).getOrElse(0)

    //new UnitSpeedParticle(origin, origin.lazyZip(endpoint.map(stepsForUnit*_)).map(_+_))
    new UnitSpeedParticle(origin, origin + (stepsForUnit *:* endpoint))

  }

  def scalar_multiplication(c: Double): UnitSpeedParticle = {
    val stepsForMultiplication: Try[Double] = Try(c/square_norm)
    require(stepsForMultiplication.isSuccess)

    new UnitSpeedParticle(origin, origin + (stepsForMultiplication.get *:* endpoint))
  }

}

object UnitSpeedParticle {

  def apply[T: Numeric](W: DenseVector[T]): UnitSpeedParticle = new UnitSpeedParticle(DenseVector.zeros[Double](3), W.map(_.asInstanceOf[Double]))
  def apply[T: Numeric](V: DenseVector[T], W: DenseVector[T]): UnitSpeedParticle = UnitSpeedParticle(V.map(_.asInstanceOf[Double]), W.map(_.asInstanceOf[Double]))

  def cross_product(A: UnitSpeedParticle, B: UnitSpeedParticle): UnitSpeedParticle = {

    val aCrossB: DenseVector[Double] = cross(A.direction, B.direction)

    new UnitSpeedParticle(DenseVector.zeros[Double](3),  aCrossB)

  }
}
