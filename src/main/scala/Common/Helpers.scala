package Common

import breeze.linalg._
import Components.Particles.UnitSpeedParticle

object Helpers {

  case class NoValidCollision(s: String) extends Exception(s)

  def withinTolerance(x: Double, y: Double, precision: Double = 1e-4): Boolean = {
    if ((x - y).abs <= precision) true else false
  }

  def vectorWithinTolerance(x: DenseVector[Double], y: DenseVector[Double], precision: Double = 1e-4): Boolean = {
    if (norm(x - y) <= precision) true else false
  }

  def pathWithinTolerance(a: UnitSpeedParticle, b: UnitSpeedParticle): Boolean = {
    vectorWithinTolerance(a.origin, b.origin) &&
      vectorWithinTolerance(a.endpoint, b.endpoint) &&
      vectorWithinTolerance(a.pathDirection, b.pathDirection)
  }

}
