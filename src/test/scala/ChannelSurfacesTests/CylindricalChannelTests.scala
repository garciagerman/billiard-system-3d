package ChannelSurfacesTests

import BilliardCellComponents.BilliardCells.GeneralBilliardCell

import math.sqrt
import breeze.linalg._
import org.scalatest._
import Common.Utility._
import BilliardCellComponents.Particles.UnitSpeedParticle
import BilliardCellComponents.MicroStructures.{BumpSegment, PlaneSegment}
import ChannelSurfaces.CylindricalChannel
import PlaneSegment._

class CylindricalChannelTests extends FunSuite {

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

  val sampleCell: GeneralBilliardCell = GeneralBilliardCell(
    bumpSegments = List(bottomBump),
    wallSegments = List(northWall, eastWall, southWall, westWall),
    entryAndExitSegment = entryExitWall
  )


  test("Channel - collision time") {

    val cylinder = new CylindricalChannel(channelHalfLength = 10D, channelRadius = 1D, sampleCell)

    val channelEntryPath = UnitSpeedParticle(cylinder.particleEntryPoint, DenseVector(0D, 2D, 0D))

    val expectedCollisionTime = 2D
    val actualCollisionTime = cylinder.pathCollisionTime(channelEntryPath)

    assert(withinTolerance(expectedCollisionTime, actualCollisionTime))
  }

  test("Channel - channel exit time") {

    val cylinder = new CylindricalChannel(channelHalfLength = 2D, channelRadius = 1, sampleCell)

    val channelEntryPath = UnitSpeedParticle(cylinder.particleEntryPoint, DenseVector(0D, 2D, 0D))

    val expectedExitTime = 1D
    val actualExitTime = cylinder.pathPassedHalfLength(channelEntryPath)
    assert(withinTolerance(expectedExitTime, actualExitTime))
  }
}
