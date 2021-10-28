//

import breeze.linalg.{DenseVector, linspace}
import Common.Global._
import Common.Utility._
import Executables.ExitTimeExperiment._

println("Timing performance of functions.")

val numberOfChannelSizes = 8
val channelHalfLengthRange: Array[Double] = linspace(a=10, b=100, length = numberOfChannelSizes).toArray

// billiard cell radius
val bumpRadius = 10D

println(s"Number of samples for mean exit time $defaultNumberOfChannelParticles")
println(s"Number of half lengths samples for mean exit time $numberOfChannelSizes")

println(s"Do sequential function...")

val sequentialMeanExitTime: Array[(Double, Double)] = time {
  meanExitTimeForRange(
    rangeOfChannelHalfLengths = channelHalfLengthRange,
    microSurface = getBumpsBilliardCell(bumpRadius = bumpRadius)
  )
}

println(s"Do asynchronous function...")

val asynchronousMeanExitTime: Array[(Double, Double)] = time {
  meanExitTimeForRangeAsynchronous(
    rangeOfChannelHalfLengths = channelHalfLengthRange,
    microSurface = getBumpsBilliardCell(bumpRadius = bumpRadius)
  )
}

// OUTPUT
