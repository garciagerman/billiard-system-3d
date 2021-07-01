package Components.Particles

import breeze.linalg._
import scala.util.Try
import Components.Utilities.Helpers._

case class UnitSpeedParticle(origin: DenseVector[Double], endpoint: DenseVector[Double]) {
  import UnitSpeedParticle._

  val pathDirection: DenseVector[Double] = endpoint - origin
  val pathLength: Double = norm(pathDirection)

  def dotOfDirections(W: UnitSpeedParticle): Double = pathDirection dot W.pathDirection

  def scaledPathToLength(newLength: Double): UnitSpeedParticle = {
    val divideByPathLen = Try(newLength/pathLength)

    if (divideByPathLen.isFailure || withinTolerance(pathLength, 0)) {
      throw UnableToScalePath("Path length is too close to zero")
    }
    val timeToNewLength: Double = divideByPathLen.get

    new UnitSpeedParticle(origin, origin + (timeToNewLength *:* pathDirection))
  }
}

object UnitSpeedParticle {
  def apply[T: Numeric](W: DenseVector[T]): UnitSpeedParticle = new UnitSpeedParticle(DenseVector.zeros[Double](3), W.map(_.asInstanceOf[Double]))
  def apply[T: Numeric](V: DenseVector[T], W: DenseVector[T]): UnitSpeedParticle = UnitSpeedParticle(V.map(_.asInstanceOf[Double]), W.map(_.asInstanceOf[Double]))

  case class UnableToScalePath(s: String) extends Exception(s)
}
