package Common

import breeze.linalg.DenseVector

import java.util.concurrent.Executors
import scala.concurrent._

object Global {

  // default numerical precision
  val defaultNumericalPrecision: Double = 1e-4

  // Constant variables involving billiard cell
  // max collisions within a billiard cell
  val maxBilliardCellCollisions: Int = 1000
  // max retries for calculating post-collision velocity/direction
  val maxBilliardCellRetries: Int = 5
  val maxBilliardCellBumpHeight:Double = 0.5D

  // Constant variables involving cylindrical channel
  // default channel radius
  val defaultChannelRadius: Double = 1D
  // default number of particles entering the billiard (sample size used to compute mean exit time)
  val defaultNumberOfChannelParticles: Int = 1000


  // implicit execution context
  val defaultNumberOfThreads = 4
  implicit val xc: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(defaultNumberOfThreads))

  // i, j, k basis vectors
  val jBase: DenseVector[Double] = DenseVector(0D, 1D, 0D)
  val iBase: DenseVector[Double] = DenseVector(1D, 0D, 0D)
  val kBase: DenseVector[Double] = DenseVector(0D, 0D, 1D)
}
