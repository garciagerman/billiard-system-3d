package Components.MicroStructures

import Components.Particles.UnitSpeedParticle

trait MicroStructureSegment {

  // time T for collision from path path origin
  def getTimeToCollision(path: UnitSpeedParticle): Double

  // verify if point is allowed, e.g. within the domain of the segment
  def pathEndpointInSegment(path: UnitSpeedParticle): Boolean

  // particle path after collision with segment
  def getPostCollisionPath(path: UnitSpeedParticle): UnitSpeedParticle
}
