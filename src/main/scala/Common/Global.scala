package Common

import breeze.linalg._
import scala.concurrent._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.Executors
import com.typesafe.config.{Config, ConfigFactory}

object Global {

  val currentDateTime: String = DateTimeFormatter
    .ofPattern("yyyy_MM_dd_HH_mm")
    .format(LocalDateTime.now)

  val conf: Config = ConfigFactory.load("application")

  // implicit execution context
  val defaultNumberOfThreads: Int = conf.getInt("number_of_threads")
  implicit val xc: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(defaultNumberOfThreads))

  // default numerical precision
  val defaultNumericalPrecision: Double = conf.getDouble("numerical_precision")
  
  // max collisions allowed within a billiard cell
  val maxBilliardCellCollisions: Int = conf.getInt("billiard_cell.max_collisions")

  // max retries for calculating post-collision velocity/direction
  val maxBilliardCellRetries: Int = conf.getInt("billiard_cell.max_retries")

  val maxBilliardCellBumpHeight: Double = conf.getDouble("billiard_cell.bump_height")

  // default channel radius
  val channelRadius: Double = conf.getDouble("cylindrical_channel.radius")

  // default number of particles entering the billiard (sample size used to compute mean exit time)
  val numberOfChannelParticles: Int = conf.getInt("cylindrical_channel.particle_samples")

  // specifications for sampling of the interval [min half len, max half len]
  val halfLengthLower: Double = conf.getDouble("cylindrical_channel.min_half_len")
  val halfLengthUpper: Double = conf.getDouble("cylindrical_channel.max_half_len")
  val halfLengthNumPartitions: Int = conf.getInt("cylindrical_channel.half_len_num_partitions")

  // i, j, k basis vectors
  val jBase: DenseVector[Double] = DenseVector(0D, 1D, 0D)
  val iBase: DenseVector[Double] = DenseVector(1D, 0D, 0D)
  val kBase: DenseVector[Double] = DenseVector(0D, 0D, 1D)
}
