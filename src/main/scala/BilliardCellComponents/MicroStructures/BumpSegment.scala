package BilliardCellComponents.MicroStructures

import scala.math.pow
import breeze.linalg._
import Common.Utility._
import BilliardCellComponents.Particles.UnitSpeedParticle
import Common.Utility

case class BumpSegment(center: DenseVector[Double], radius: Double) extends MicroStructureSegment {

  case class NoExitVector(s: String) extends Exception(s)

  override def pathEndpointInSegment(path: UnitSpeedParticle): Boolean = {
    val endPoint = path.endpoint.copy

    withinTolerance(norm(endPoint -center), radius)
  }

  /**
   * A point X lies on the bump if ||X-C||^2 = R^2, where C is the center and R is the radius
   * For a particle path parametrized by L(t) = O + t*Dir, the point of intersection is whenever
   * (L(t)-C) dot (L(t)-C) = R^2
   * This gives us the quadratic equation a*t^2 + bt + c = 0,
   * where
   * a = dir dot dir
   * b = 2 * (dir dot (O - C))
   * c = ((O - C) dot (O - C)) - R^2
   * @return the time of collision, if it exists
   */
  override def getTimeToCollision(path: UnitSpeedParticle): Double = {

    val a = path.dotOfDirections(path)
    val b = 2 * (path.pathDirection dot (path.origin - center))
    val c = pow(norm(path.origin - center), 2) - pow(radius, 2)

    quadraticSolver(a, b, c)
  }

  /**
   * Returns specular reflected path after collision with bump
   * Uses the Rodrigues formula to rotate incoming particle about the tangent normal
   * See here: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
   */
  override def getPostCollisionPath(collisionParticlePath: UnitSpeedParticle): UnitSpeedParticle = {

    val reflectedParticlePath = UnitSpeedParticle(
      origin = collisionParticlePath.endpoint,
      endpoint = collisionParticlePath.origin
    )

    val tangentDir = collisionParticlePath.endpoint - center

    val rotatedEndPoint = {
      val rotationAngle = math.Pi

      val s1 = math.cos(rotationAngle) *:* reflectedParticlePath.pathDirection
      val s2 = math.sin(rotationAngle) *:* cross(tangentDir, reflectedParticlePath.pathDirection)
      val s3 = (1D - math.cos(rotationAngle)*(tangentDir dot reflectedParticlePath.pathDirection)) *:* tangentDir

      s1 + s2 + s3
    }

    UnitSpeedParticle(reflectedParticlePath.origin, rotatedEndPoint)
  }
}

object BumpSegment {
  case class SpecificationError(s: String) extends Exception(s)
}
