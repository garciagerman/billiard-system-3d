package Components.BilliardCells

import breeze.linalg._
import scala.util.control.Breaks._
import scala.util.{Failure, Success, Try}
import Components.MicroStructures._
import Components.Particles.UnitSpeedParticle
import Components.BilliardCells.FourBumpsCell.{SimulationParameters, SpecificationException}

case class FourBumpsCell(
                        bumpSegments: List[BumpSegment],
                        wallSegments: List[PlaneSegment],
                        entryAndExitSegment: PlaneSegment
                        ) {

  import FourBumpsCell._

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
            if (tempTime > 0) {
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
      .filter(_.collisionTIme > 0)
      .sortWith(_.collisionTIme < _.collisionTIme)

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
    val exitPathStack =  scala.collection.mutable.Stack[UnitSpeedParticle]()

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
          val nextParams: SimulationParameters = singleCollisionGetter(currentParams.collisionPath)
          parameterStack.append(nextParams)
        }

        // update iteration counter
        iterationCounter += 1
      }
    }

    if (exitPathStack.isEmpty) throw SpecificationException("No valid path after max iterations")

    exitPathStack.pop()
  }

  def singleParticleSimulation(maxIterations: Int = 10000): Unit = {
    val segmentCollisionIndexes: Seq[Int] = 0 to maxIterations
    val segmentCollisionPaths = scala.collection.mutable.ArrayBuffer[UnitSpeedParticle]()

    val entryPathOrigin: DenseVector[Double] = entryAndExitSegment.generateRandomPoint()
    val entryPathDirection: DenseVector[Double] = randomSampleEnteringDirections

    val incomingParticlePath = UnitSpeedParticle(
      origin = entryPathOrigin,
      endpoint = entryPathOrigin + (0.25D *:* entryPathDirection)
    )

    // return the path of the particle exiting the billiard cell
    getExitPath(incomingParticlePath, maxIterations)
  }
}

object FourBumpsCell {
  case class SimulationParameters(
                                 segmentIndex: Int,
                                 segment: MicroStructureSegment,
                                 collisionTIme: Double,
                                 collisionPath: UnitSpeedParticle
                                 )

  case class SpecificationException(s: String) extends Exception(s)

  def randomSampleEnteringDirections: DenseVector[Double] = {
    import scala.util.Random

    val x: Double = Random.between(-1D,1D)
    val y: Double = Random.between(-1D,1D)
    val z: Double = Random.between(-1D,0D)

    val entryVector: DenseVector[Double] = DenseVector(x,y,z)
    val entryVectorNorm: Double = norm(entryVector)

    entryVector /:/ entryVectorNorm
  }
}
