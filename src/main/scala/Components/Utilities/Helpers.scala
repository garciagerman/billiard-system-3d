package Components.Utilities

object Helpers {

  def withinTolerance(x: Double, y: Double, precision: Double = 1e-4): Boolean = {
    if ((x - y).abs <= precision) true else false
  }

}
