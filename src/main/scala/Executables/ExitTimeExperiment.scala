package Executables

import BilliardCellComponents.BilliardCells.GeneralBilliardCell
import BilliardCellComponents.MicroStructures.{BumpSegment, PlaneSegment}
import Common.Global._
import ChannelSurfaces.CylindricalChannel
import Common.Global
import breeze.linalg.{DenseMatrix, DenseVector, csvwrite, linspace}

import java.io.File
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try


object ExitTimeExperiment {

  case class SimulationResults(
    bumpRadius: Double,
    channelHalfLength: Double,
    meanExitTime: Double
  )

  // helper function to save simulation results
  def exportResultsToCSV(simulationData: Array[SimulationResults]): Unit = {
    import java.time.LocalDateTime
    import java.time.format.DateTimeFormatter

    val currentDateTime = DateTimeFormatter
      .ofPattern("yyyy_MM_dd_HH_mm")
      .format(LocalDateTime.now)

    val fileName: String = s"\\mean_exit_time_results_$currentDateTime.csv"

    val dataAsMatrix: DenseMatrix[Double] = DenseMatrix(
      simulationData
        .map(t => Array(t.bumpRadius, t.channelHalfLength, t.meanExitTime))
      :_*
    )

    csvwrite(new File(Global.outputPath + fileName), dataAsMatrix, separator = ',')
  }

  /**
   * Returns the default billiard cell: unit length, width, and height centered at the origin in xyz.
   * By default, the maximum height of the bump is at z = 0.5 in order to reduce edge cases of collisions.
   * This specification requires the bump radius be larger than 2
   * @param bumpRadius the radius of the bump in the center of the billiard cell
   * @return
   */
  def getBumpsBilliardCell(bumpRadius: Double): GeneralBilliardCell = {
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

    // Require radius >= 2
    val bottomBump = new BumpSegment(center = DenseVector(0D, 0D, maxBilliardCellBumpHeight - bumpRadius), radius = bumpRadius)

    val bumpsBilliardCell = GeneralBilliardCell(
      bumpSegments = List(bottomBump),
      wallSegments = List(northWall, eastWall, southWall, westWall),
      entryAndExitSegment = entryExitWall
    )

    bumpsBilliardCell
  }

  // helper function for getting exit time of a single particle in a channel
  private def getSingleMeanExitTimeFromChannel(channel: CylindricalChannel, numberOfChannelParticles: Int = defaultNumberOfChannelParticles): Double = {

    val rawExitTimes: Array[Double] = {0 until numberOfChannelParticles}
      .map{ _ => Try(channel.simulateParticleExitTime)}
      .filter(_.isSuccess)
      .map(_.get)
      .toArray

    val successCount = rawExitTimes.length.toDouble

    rawExitTimes.sum / successCount
  }

  /**
   *
   * @param rangeOfChannelHalfLengths: an array of channel half lengths
   * @param channelRadius: a fixed channel radius for all simulations
   * @param microSurface: billiard cell specifying the surface micro-structure
   * @param numberOfChannelParticles: the number of particles entering the channel
   * @return Array of tuples of the form (channel half length, mean exit time)
   */
  def meanExitTimeForRange(
                            rangeOfChannelHalfLengths: Array[Double],
                            channelRadius: Double = defaultChannelRadius,
                            microSurface: GeneralBilliardCell,
                            numberOfChannelParticles: Int = defaultNumberOfChannelParticles): Array[SimulationResults] = {

    val futureMeanExitTimes: Array[Future[SimulationResults]] = rangeOfChannelHalfLengths.map {
          halfLen => Future {

            val cylinder = new CylindricalChannel(channelHalfLength = halfLen, channelRadius = channelRadius, microSurface)
            val meanExitTimeOfParticles: Double = getSingleMeanExitTimeFromChannel(cylinder, numberOfChannelParticles)

            SimulationResults(channelRadius, halfLen, meanExitTimeOfParticles)
        }
      }

    val result: Future[List[SimulationResults]] = Future.sequence(futureMeanExitTimes.toList)

    Await
      .result(result, Duration.Inf)
      .toArray
  }

  /**
   * Main Executable to perform a sequence of simulations
   */
  def main(args: Array[String]): Unit = {

    val channelHalfLengths = linspace(a = 10D, b = 50D, length = 10).toArray

    // override default number of channel particles for DEMO
    val numberOfChannelParticles = 500

    val bumpBilliardCell = getBumpsBilliardCell(10D)

    val meanExitTimes: Array[SimulationResults] = meanExitTimeForRange(channelHalfLengths, 1D, bumpBilliardCell, numberOfChannelParticles)

    println(s"Number of total samples ${meanExitTimes.length}")

    println(s"Exporting results to ${Global.outputPath}...")
    exportResultsToCSV(meanExitTimes)
    println("...export done!")
  }

}
