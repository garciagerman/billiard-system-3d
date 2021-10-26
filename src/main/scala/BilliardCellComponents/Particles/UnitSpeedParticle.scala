package BilliardCellComponents.Particles

import breeze.linalg._
import scala.util.Try
import Common.Utility._

/**
 * A particle path defined by an origin and an endpoint, i.e. a mathematical vector.
 */
case class UnitSpeedParticle(origin: DenseVector[Double], endpoint: DenseVector[Double]) {
  import UnitSpeedParticle._

  val pathDirection: DenseVector[Double] = endpoint - origin
  val pathLength: Double = norm(pathDirection)

  // Helper function
  def dotOfDirections(W: UnitSpeedParticle): Double = pathDirection dot W.pathDirection

  /**
   * Keeps origin, but shifts the endpoint of a particle specified by t along the path of direction
   * i.e. new endpoint is (Origin + t*Direction)
   * @param timeToMove
   * @return a new particle path along the direction of the input path
   */
  def moveAlongPath(timeToMove: Double): UnitSpeedParticle = {
    val originCopy = origin.copy
    new UnitSpeedParticle(originCopy, originCopy + (timeToMove *:* pathDirection ))
  }

  /**
   * Keeps origin but the path is scaled to a specified length
   * @param newLength
   * @return a new particle path with specified length
   */
  def scaledPathToLength(newLength: Double): UnitSpeedParticle = {
    val divideByPathLen = Try(newLength/pathLength)

    if (divideByPathLen.isFailure || withinTolerance(pathLength, 0)) {
      throw UnableToScalePath("Path length is too close to zero")
    }
    val timeToNewLength: Double = divideByPathLen.get

    //new UnitSpeedParticle(origin, origin + (timeToNewLength *:* pathDirection))
    moveAlongPath(timeToNewLength)
  }

  /**
   * Returns the angle between W and the direction of the particle path
   */
  def angleBetweenVectors(W: DenseVector[Double]): Double = {
    val cosineSimilarity = (pathDirection dot W) / (norm(pathDirection) * norm(W))

    math.acos(cosineSimilarity)
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
