package Common

import breeze.linalg._

object Helpers {

  def withinTolerance(x: Double, y: Double, precision: Double = 1e-4): Boolean = {
    if ((x - y).abs <= precision) true else false
  }

  def vectorWithinTolerance(x: DenseVector[Double], y: DenseVector[Double], precision: Double = 1e-4): Boolean = {
    if (norm(x - y) <= precision) true else false
  }

}
