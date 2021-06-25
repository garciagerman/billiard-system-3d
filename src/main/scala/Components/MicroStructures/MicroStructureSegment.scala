package Components.MicroStructures

import Components.Particles.UnitSpeedParticle

trait MicroStructureSegment {

  // time T for collision from V start point
  // if zero then there is no collision
  def getTimeToCollide(V: UnitSpeedParticle): Double

  def getExitVector(V: UnitSpeedParticle): UnitSpeedParticle
}
