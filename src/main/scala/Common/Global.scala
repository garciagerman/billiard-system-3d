package Common

import breeze.linalg.DenseVector

import java.util.concurrent.Executors
import scala.concurrent._
import com.typesafe.config.{Config, ConfigFactory}

object Global {
  val conf: Config = ConfigFactory.load("application")

  // implicit execution context
  val defaultNumberOfThreads: Int = conf.getInt("number_of_threads")
  implicit val xc: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(defaultNumberOfThreads))

  // default numerical precision
  val defaultNumericalPrecision: Double = conf.getDouble("numerical_precision")

  val outputPath: String = conf.getString("output_path")

  // max collisions allowed within a billiard cell
  val maxBilliardCellCollisions: Int = conf.getInt("billiard_cell.max_collisions")

  // max retries for calculating post-collision velocity/direction
  val maxBilliardCellRetries: Int = conf.getInt("billiard_cell.max_retries")

  val maxBilliardCellBumpHeight: Double = conf.getDouble("billiard_cell.bump_height")

  // default channel radius
  val defaultChannelRadius: Double = conf.getDouble("cylindrical_channel.radius")

  // default number of particles entering the billiard (sample size used to compute mean exit time)
  val defaultNumberOfChannelParticles: Int = conf.getInt("cylindrical_channel.particle_samples")

  // i, j, k basis vectors
  val jBase: DenseVector[Double] = DenseVector(0D, 1D, 0D)
  val iBase: DenseVector[Double] = DenseVector(1D, 0D, 0D)
  val kBase: DenseVector[Double] = DenseVector(0D, 0D, 1D)
}
