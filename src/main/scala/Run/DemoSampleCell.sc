//

import breeze.linalg.DenseVector
import Components.BilliardCells._
import Components.MicroStructures._
import scala.util.{Try, Success, Failure}
import Components.Particles.UnitSpeedParticle

val entryExitWall = new PlaneSegment(
  DenseVector(-0.5D, 0.5D, 1D),
  DenseVector(0.5D, 0.5D, 1D),
  DenseVector(0.5D, -0.5D, 1D),
  DenseVector(-0.5D, -0.5D, 1D),
  billiardCellLength = 1,
  collisionDirection = 1D
)

val northWall = new PlaneSegment(
  DenseVector(-0.5D, 0.5D, 1D),
  DenseVector(-0.5D, 0.5D, 0D),
  DenseVector(-0.5D, -0.5D, 0D),
  DenseVector(-0.5D, -0.5D, 1D),
  billiardCellLength = 1,
  collisionDirection = 1D
)

val eastWall = new PlaneSegment(
  DenseVector(-0.5D, 0.5D, 1),
  DenseVector(-0.5D, 0.5D, 0D),
  DenseVector(0.5D, 0.5D, 0D),
  DenseVector(0.5D, 0.5D, 1D),
  billiardCellLength = 1,
  collisionDirection = -1D
)

val southWall = new PlaneSegment(
  DenseVector(0.5D, 0.5D, 1),
  DenseVector(0.5D, 0.5D, 0D),
  DenseVector(0.5D, -0.5D, 0D),
  DenseVector(0.5D, -0.5D, 1D),
  billiardCellLength = 1,
  collisionDirection = -1D
)

val westWall = new PlaneSegment(
  DenseVector(0.5D, -0.5D, 1),
  DenseVector(0.5D, -0.5D, 0D),
  DenseVector(-0.5D, -0.5D, 0D),
  DenseVector(-0.5D, -0.5D, 1D),
  billiardCellLength = 1,
  collisionDirection = +1D
)

// Require radius >= 1.
// Center bump at (0, 0, 1-R)
val bottomBump = new BumpSegment(center = DenseVector(0D, 0D, 0D), radius = 1D)

val sampleCell = GeneralBilliardCell(
  bumpSegments = List(bottomBump),
  wallSegments = List(northWall, eastWall, southWall, westWall),
  entryAndExitSegment = entryExitWall
)

println("Cell defined successfully.")

//sampleCell.singleParticleSimulation()

val totalSimulations = 1000
println(s"Will perform $totalSimulations billiard cell simulations...")

val exitAngles: List[UnitSpeedParticle] = {0 until totalSimulations}
  .map{ _ => Try(sampleCell.singleParticleSimulation(10000))}
  .filter(_.isSuccess)
  .map(_.get)
  .toList

val numSuccessfulSimulations = exitAngles.length
println(s"Successful simulations ${numSuccessfulSimulations}")
println(s"Success Rate ${100 * (numSuccessfulSimulations.toDouble / totalSimulations.toDouble)}%")

// exit directions
val exitDirections: List[DenseVector[Double]] = exitAngles
  .map(_.pathDirection)

// angles of the exit directions w.r.t. the x-axis
val anglesWithXAxis: DenseVector[Double] = DenseVector(
  exitAngles
    .map(_.angleBetweenVectors(DenseVector(1D, 0D, 0D)))
    .sorted
    :_*
)

// angles of the exit directions w.r.t. the y-axis
val anglesWithYAxis: DenseVector[Double] = DenseVector(
  exitAngles
    .map(_.angleBetweenVectors(DenseVector(0D, 1D, 0D)))
    .sorted
    :_*
)