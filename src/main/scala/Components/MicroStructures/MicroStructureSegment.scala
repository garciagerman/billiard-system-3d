package Components.MicroStructures

import Components.Particles.UnitSpeedParticle

trait MicroStructureSegment {

  // time T for collision from path path origin
  def getTimeToCollision(path: UnitSpeedParticle): Double

  // verify if collision point is allowed, e.g. within the domain of the segment
  //def validCollisionEndpoint(path: UnitSpeedParticle, timeToCollision: Double): Boolean

  // particle path after collision
  def getPostCollisionPath(V: UnitSpeedParticle, t: Double): UnitSpeedParticle
}
