package ChannelSurfaces

import breeze.linalg._

import scala.util.control.Breaks._
import scala.util.{Failure, Success, Try}
import Common.Utility._
import Common.Global._
import BilliardCellComponents.MicroStructures._
import BilliardCellComponents.Particles.UnitSpeedParticle
import BilliardCellComponents.BilliardCells.GeneralBilliardCell

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class CylindricalChannel(
                               channelHalfLength: Double,
                               channelRadius: Double,
                               surfaceMicroStructure: GeneralBilliardCell) {
  import CylindricalChannel._

  // origin of all particle paths
  val particleEntryPoint: DenseVector[Double] = DenseVector(0D, 0D, channelRadius)

  // next collision time inside channel
  def pathCollisionTime(path: UnitSpeedParticle): Double = {
    val xzOnly = iBase + kBase

    // coefficients for quadratic function
    val A = math.pow(norm(xzOnly *:* path.pathDirection), 2)
    val B = 2 * ((xzOnly *:* path.origin) dot (xzOnly *:* path.pathDirection))
    val C = math.pow(norm(xzOnly *:* path.origin), 2) - math.pow(channelRadius, 2)

    quadraticSolver(A, B, C)
  }

  // time when path passed exited the channel
  def pathPassedHalfLength(path: UnitSpeedParticle): Double = {
    val yPathDirection: Double = path.pathDirection(1)
    val yPathDirectionSign: Double = math.signum(yPathDirection)

    (yPathDirectionSign*channelHalfLength - path.origin(1)) / yPathDirection
  }

  // check if particle endpoint is outside channel
  def pathOutsideChannel(path: UnitSpeedParticle): Boolean = {
    val yCoordinate: Double = math.abs(path.endpoint(1))

    (yCoordinate > channelHalfLength) && !withinTolerance(yCoordinate, channelHalfLength)
  }

  // post-collision direction needs to be in the same direction as norma vector for proper motion within the channel
  def getUpdatedPathDirection(path: UnitSpeedParticle, candidateDirection: DenseVector[Double]): DenseVector[Double] = {
    val normalVectorAtCollision = gradient(path.endpoint)

    val candidatePathDirectionSigns: Array[Double] = candidateDirection
      .toArray
      .map {
      directionCoordinate => if (withinTolerance(directionCoordinate, 0D)) 1D else math.signum(directionCoordinate)
    }

    val correctPathDirectionSigns: Array[Double] = normalVectorAtCollision
      .toArray
      .map {
        directionCoordinate => if (withinTolerance(directionCoordinate, 0D)) 1D else math.signum(directionCoordinate)
      }

    val directionCorrectorMultipliers: DenseVector[Double] = DenseVector(
      candidatePathDirectionSigns
        .zip(correctPathDirectionSigns)
        .map(tuple => if (withinTolerance(tuple._1, tuple._2)) 1D else -1D)
        .toArray
      :_*
    )

    directionCorrectorMultipliers *:* candidateDirection
  }

  private def getPathMotionUntilExit: List[UnitSpeedParticle] = {
    // particle path history
    val particlePathHistory: ArrayBuffer[UnitSpeedParticle] = scala.collection.mutable.ArrayBuffer[UnitSpeedParticle]()

    // current path only
    val currentPathStack: mutable.Stack[UnitSpeedParticle] = scala.collection.mutable.Stack[UnitSpeedParticle]()

    // randomized entry particle
    val particleEntryPath = UnitSpeedParticle.pathFromOriginAndDirection(
      origin = particleEntryPoint,
      direction = randomSampleEnteringDirections
    )

    // add entry to current stack
    currentPathStack.append(particleEntryPath)

    while (currentPathStack.nonEmpty) {
      // get path up to collision with the channel
      val currentPath = currentPathStack.pop()

      val currentPathToCollision = currentPath.scaledPathToLength(pathCollisionTime(currentPath))

      // check if current path has exited the segment
      if (pathOutsideChannel(currentPathToCollision)) {

        // shorten path to exit endpoint
        val updatedPath = currentPathToCollision.scaledPathToLength(pathPassedHalfLength(currentPathToCollision))

        // add to history
        particlePathHistory += updatedPath
      }
      else {
        // add to history
        particlePathHistory += currentPathToCollision

        // reset path for next iteration
        val directionFromMicroStructure: DenseVector[Double] = retry(maxBilliardCellRetries)(surfaceMicroStructure.singleParticleSimulation()).pathDirection.copy

        // post-collision direction needs to be in the same direction as norma vector for proper motion within the channel
        val updatedDirection: DenseVector[Double] = getUpdatedPathDirection(currentPathToCollision, directionFromMicroStructure)

        // next path within channel with updated direction
        val nextPath = UnitSpeedParticle.pathFromOriginAndDirection(
          origin = currentPathToCollision.endpoint.copy,
          direction = updatedDirection
        )
        currentPathStack.append(nextPath)
      }
    }

    particlePathHistory.toList
  }


  // get exit time
  def simulateParticleExitTime: Double = {
    val particlePathHistory: List[UnitSpeedParticle] = getPathMotionUntilExit

    val timeUntilChannelExit: Double = particlePathHistory
      .map(p => norm(p.pathDirection))
      .sum

    timeUntilChannelExit
  }
}

object CylindricalChannel {

  // retry function
  @tailrec
  def retry[T](n: Int)(fn: => T): T = {
    util.Try { fn } match {
      case util.Success(x) => x
      case _ if n > 1 => retry(n - 1)(fn)
      case util.Failure(e) => throw e
    }
  }

  // evaluates the gradient of the channel surface eqn. at a point
  def gradient(point: DenseVector[Double]): DenseVector[Double] = {
    val x: Double = point(0)
    val z: Double = point(2)

    (-x * iBase) + (-z * kBase)
  }
}
