package Components

import scala.math.{pow, sqrt}
import scala.util.Try
import breeze.linalg._

case class Particle(origin: DenseVector[Double], endpoint: DenseVector[Double]) {
  val direction: DenseVector[Double] = origin - endpoint //origin.lazyZip(endpoint).map(_-_)

  val square_norm: Double = norm(direction) //sqrt(direction.map(pow(_, 2)).sum)

  def dot(W: Particle): Double = direction dot W.direction  //direction.zip(W.direction).map{ case (v,w) => v*w }.sum

  def unitVector: Particle = {
    val stepsForUnit: Double = Try(1/square_norm).getOrElse(0)

    //new Particle(origin, origin.lazyZip(endpoint.map(stepsForUnit*_)).map(_+_))
    new Particle(origin, origin + (stepsForUnit *:* endpoint))

  }

  def scalar_multiplication(c: Double): Particle = {
    val stepsForMultiplication: Try[Double] = Try(c/square_norm)
    require(stepsForMultiplication.isSuccess)

    new Particle(origin, origin + (stepsForMultiplication.get *:* endpoint))
  }

}

object Particle {

  def apply[T: Numeric](W: DenseVector[T]): Particle = new Particle(DenseVector.zeros[Double](3), W.map(_.asInstanceOf[Double]))
  def apply[T: Numeric](V: DenseVector[T], W: DenseVector[T]): Particle = Particle(V.map(_.asInstanceOf[Double]), W.map(_.asInstanceOf[Double]))

  //def sumVectors(V: DenseVector[Double], W: DenseVector[Double]): DenseVector[Double] = V.lazyZip(W).map(_+_)

  def cross_product(A: Particle, B: Particle): Particle = {
//    val Vector(a1, a2, a3) = A.direction
//    val Vector(b1, b2, b3) = B.direction
//
//    val c1 = a2*b3 - a3*b2
//    val c2 = a1*b3 - a3*b1
//    val c3 = a1*b2 - a2*b1
//
//    new Particle(Vector(0D, 0D, 0D), Vector(c1, -c2, c3))
    val aCrossB: DenseVector[Double] = cross(A.direction, B.direction)

    new Particle(DenseVector.zeros[Double](3),  aCrossB)

  }
}
