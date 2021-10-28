package BilliardCellComponents.BilliardCells

import breeze.linalg._
import scala.util.control.Breaks._
import scala.util.{Failure, Success, Try}
import Common.Utility._
import Common.Global._
import BilliardCellComponents.MicroStructures._
import BilliardCellComponents.Particles.UnitSpeedParticle

case class GeneralBilliardCell(
                                bumpSegments: List[BumpSegment],
                                wallSegments: List[PlaneSegment],
                                entryAndExitSegment: PlaneSegment
                        ) {

  import GeneralBilliardCell._

  val cellSegments: List[(MicroStructureSegment, Int)] = {
    List(entryAndExitSegment) ++ wallSegments ++ bumpSegments
  }.zipWithIndex

  /**
   * For each micro-structure segment in the billiard cell return the collision time to that segment from the entryPath
   * Only returns segments with positive collision times and with valid collision points in segments
   * @return A case class in the form ( micro-structure segment index,  micro-structure segment, collision time, collision path)
   */
  private def getCollidingSegments(entryPath: UnitSpeedParticle): List[SimulationParameters] = {
    val dummyTime = -47D
    val dummyPath = UnitSpeedParticle(DenseVector(-47D, -47D, -47D))

    val allSimulationParameters = cellSegments.map {
      case (segment, segmentIndex) =>
        val (timeToCollision, collisionPath): (Double, UnitSpeedParticle) = Try(segment.getTimeToCollision(entryPath)) match {
          case Success(tempTime) =>
            if ((tempTime > 0D) && !withinTolerance(tempTime, 0D, precision = 1e-3)){
              val tempCollisionPath = entryPath.moveAlongPath(tempTime)
              if (segment.pathEndpointInSegment(tempCollisionPath)) (tempTime, tempCollisionPath) // valid collision
              else (dummyTime, dummyPath) // if collision point is not in segment, not a valid collision
            }
            else {
              (dummyTime, dummyPath) // if collision time is not positive, not a valid collision
            }
          case Failure(_) => (dummyTime, dummyPath)
        }

        SimulationParameters(segmentIndex, segment, timeToCollision, collisionPath)
    }
      .filter(params => (params.collisionTime > 0D) && (!withinTolerance(params.collisionTime, 0D, precision = 1e-3)))
      .sortWith(_.collisionTime < _.collisionTime)

    allSimulationParameters
  }

  // helper function to return and validate collision segments
  private def singleCollisionGetter(incomingParticlePath: UnitSpeedParticle): SimulationParameters = {
    val allCandidateSegments: List[SimulationParameters] = getCollidingSegments(incomingParticlePath)

    // check if candidate list is empty
    if (allCandidateSegments.isEmpty) throw SpecificationException("Double-check Segments in Cell")

    // return the parameters segment with the min collision time
    allCandidateSegments.head
  }

  // simulate collisions in the billiard cell until the particle exits the cell. return the exit path
  private def getExitPath(incomingParticlePath: UnitSpeedParticle, maxIterations: Int): UnitSpeedParticle = {
    var iterationCounter = 0
    val initParams = singleCollisionGetter(incomingParticlePath)

    // placeholder for parameters during the simulation
    val parameterStack = scala.collection.mutable.Stack[SimulationParameters](initParams)
    val exitPathStack = scala.collection.mutable.Stack[UnitSpeedParticle]()

    breakable {
      while (parameterStack.nonEmpty) {

        // check iteration counter
        if (iterationCounter > maxIterations) break()

        // get current parameters of simulation
        val currentParams = parameterStack.pop()

        // if segment is exit vector, end
        if (currentParams.segmentIndex == 0) {
          exitPathStack.append(currentParams.collisionPath)

          // exit out of the loop
          break
        }
        // otherwise reset incoming particle path
        else {
          // get the post-collision path
          val nextPath = currentParams.segment.getPostCollisionPath(currentParams.collisionPath)

          // get the candidate segments for the post-collision path
          val nextParams: SimulationParameters = singleCollisionGetter(nextPath)
          parameterStack.append(nextParams)
        }

        // update iteration counter
        iterationCounter += 1
      }
    }

    if (exitPathStack.isEmpty) throw SpecificationException("No valid path after max iterations")

    exitPathStack.pop()
  }

  def singleParticleSimulation(maxIterations: Int = maxBilliardCellCollisions): UnitSpeedParticle = {
    // initiate a random entry vector
    val entryPathOrigin: DenseVector[Double] = entryAndExitSegment.generateRandomPoint()
    val entryPathDirection: DenseVector[Double] = randomSampleEnteringDirections

    val incomingParticlePath = UnitSpeedParticle.pathFromOriginAndDirection(entryPathOrigin, entryPathDirection)

    // return the path of the particle exiting the billiard cell
    val result: UnitSpeedParticle = getExitPath(incomingParticlePath, maxIterations)

    result
  }
}

object GeneralBilliardCell {
  case class SimulationParameters(
                                   segmentIndex: Int,
                                   segment: MicroStructureSegment,
                                   collisionTime: Double,
                                   collisionPath: UnitSpeedParticle
                                 )

  case class SpecificationException(s: String) extends Exception(s)
}
