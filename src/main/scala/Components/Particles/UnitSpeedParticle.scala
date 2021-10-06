package Components.Particles

import breeze.linalg._
import scala.util.Try
import Common.Helpers._

case class UnitSpeedParticle(origin: DenseVector[Double], endpoint: DenseVector[Double]) {
  import UnitSpeedParticle._

  val pathDirection: DenseVector[Double] = endpoint - origin
  val pathLength: Double = norm(pathDirection)

  def dotOfDirections(W: UnitSpeedParticle): Double = pathDirection dot W.pathDirection

  def moveAlongPath(timeToMove: Double): UnitSpeedParticle = {
    new UnitSpeedParticle(origin, origin + (timeToMove *:* pathDirection ))
  }

  def scaledPathToLength(newLength: Double): UnitSpeedParticle = {
    val divideByPathLen = Try(newLength/pathLength)

    if (divideByPathLen.isFailure || withinTolerance(pathLength, 0)) {
      throw UnableToScalePath("Path length is too close to zero")
    }
    val timeToNewLength: Double = divideByPathLen.get

    //new UnitSpeedParticle(origin, origin + (timeToNewLength *:* pathDirection))
    moveAlongPath(timeToNewLength)
  }
}

object UnitSpeedParticle {
  def apply[T: Numeric](endpoint: DenseVector[T]): UnitSpeedParticle = {
    UnitSpeedParticle(DenseVector.zeros[Double](3), endpoint.map(_.asInstanceOf[Double]))
  }

  def apply[T: Numeric](origin: DenseVector[T], endpoint: DenseVector[T]): UnitSpeedParticle = {
    UnitSpeedParticle(origin.map(_.asInstanceOf[Double]), endpoint.map(_.asInstanceOf[Double]))
  }
  case class UnableToScalePath(s: String) extends Exception(s)
}
