package Components

import Components.BumpsSegment.quadraticSolver
import breeze.linalg._

import scala.math.pow

case class BumpsSegment(center: DenseVector[Double], radius: Double) extends MicroStructureSegment {

  // TODO: move this to a common directory
  case class NoExitVector(s: String)  extends Exception(s)


  override def getTimeToCollide(V: Particle) = {

    val a = V.square_norm
    val b = 2 * (V.direction dot (V.origin - center))
    val c = pow(norm(V.origin - center), 2) - pow(radius, 2)

    val t = quadraticSolver(a, b, c)

    if (t > 0) t else 0D

  }

  // specular reflection
  override def getExitVector(V:Particle): Particle = {

    val t = getTimeToCollide(V)
    // t needs to be nonzero
    require(t > 10e-4)

    val collisionParticle: Particle = new Particle(V.origin, V.origin + (t *:* V.direction))

    val tangentPlaneNormal: DenseVector[Double] = (center - collisionParticle.endpoint) /:/ norm(center - collisionParticle.endpoint)

    val velocityNormalComp: Double = tangentPlaneNormal dot collisionParticle.direction

    if (velocityNormalComp > 0) throw NoExitVector("")

    val constant: Double = 2*velocityNormalComp/pow(norm(tangentPlaneNormal), 2)
    val newDir: DenseVector[Double] = constant *:* collisionParticle.direction

    new Particle(collisionParticle.origin, collisionParticle.origin + (1D *:* newDir))
      .unitVector

  }


}


object BumpsSegment {

  def quadraticSolver(a: Double, b: Double, c: Double): Double = {

    import scala.util.Try
    import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
    import org.apache.commons.math3.analysis.solvers.LaguerreSolver

    val polynomial: PolynomialFunction = new PolynomialFunction(Array[Double](c, b, a))
    val laguerreSolver = new LaguerreSolver()

    Try(laguerreSolver.solve(100, polynomial, 0, Double.MaxValue))
      .getOrElse(0D)

  }
}
