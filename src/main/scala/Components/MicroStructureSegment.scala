package Components

trait MicroStructureSegment {

  // time T for collision from V start point
  // if zero then there is no collision
  def getTimeToCollide(V: Particle): Double

  def getExitVector(V: Particle): Particle
}