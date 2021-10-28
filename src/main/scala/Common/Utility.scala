package Common

import breeze.linalg._
import Common.Global._
import BilliardCellComponents.Particles.UnitSpeedParticle

object Utility {

  case class NoValidCollision(s: String) extends Exception(s)

  // check tolerance between numeric doubles
  def withinTolerance(x: Double, y: Double, precision: Double = defaultNumericalPrecision): Boolean = {
    if ((x - y).abs <= precision) true else false
  }

  // check tolerance between vectors of numerics
  def vectorWithinTolerance(x: DenseVector[Double], y: DenseVector[Double], precision: Double = defaultNumericalPrecision): Boolean = {
    if (norm(x - y) <= precision) true else false
  }

  // check tolerance between two unit paths (checks all defining properties: origin, endpoint, and direction)
  def pathWithinTolerance(a: UnitSpeedParticle, b: UnitSpeedParticle, precision: Double = defaultNumericalPrecision): Boolean = {
    vectorWithinTolerance(a.origin, b.origin, precision) &&
      vectorWithinTolerance(a.endpoint, b.endpoint, precision) &&
      vectorWithinTolerance(a.pathDirection, b.pathDirection, precision)
  }

  // samples directions in the lower half of the unit sphere (where z < 0)
  def randomSampleEnteringDirections: DenseVector[Double] = {
    import scala.util.Random

    val x: Double = Random.between(-1D,1D)
    val y: Double = Random.between(-1D,1D)
    val z: Double = Random.between(-1D,0D)

    val entryVector: DenseVector[Double] = DenseVector(x,y,z)
    val entryVectorNorm: Double = norm(entryVector)

    entryVector /:/ entryVectorNorm
  }

  /**
   * Solves for the quadratic a*x^2 + b*x + c = 0
   * @return The smallest positive solution
   */
  def quadraticSolver(a: Double, b: Double, c: Double): Double = {
    val determinant = {
      val d_ =  b*b-4.0*a*c
      if (withinTolerance(d_, 0D)) 0D else d_
    }

    if (determinant < 0) throw NoValidCollision("Determinant is negative.")

    val multiplySign = if (b < 0.0) 1 else -1

    val solutionOne: Double = (-b + multiplySign*math.sqrt(determinant)) / (2*a)
    val solutionTwo: Double = c / (a*solutionOne)

    val validSolutions = Array(solutionOne, solutionTwo)
      .filter(_ > 0D )
      .filter(!withinTolerance(_, 0D))
      .sortWith(_ < _)

    if (validSolutions.isEmpty) throw NoValidCollision("Solutions are not valid.")

    validSolutions.head
    }

  // times a block in seconds
  def time[R](block: => R): R = {
    val t0: Long = System.nanoTime()
    val result = block    // call-by-name
    val t1: Long = System.nanoTime()

    println(s"Elapsed time: ${math.round((t1.toDouble - t0.toDouble).toDouble / (1e9*60))} min")
    result
  }
}
