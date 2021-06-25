package Run

import Components.Particles.UnitSpeedParticle
import breeze.linalg.DenseVector


object RunSimulation {
  def main(args: Array[String]): Unit = {
    val V = UnitSpeedParticle(DenseVector(1D, 2D, 3D))
    println(s"Starting with ${V.toString}")
    println(s"Norm is ${V.square_norm}")
  }

}
