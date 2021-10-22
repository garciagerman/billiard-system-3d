package ComponentTests

import math.sqrt
import breeze.linalg._
import org.scalatest._
import Common.Helpers.{vectorWithinTolerance, withinTolerance}
import Components.Particles.UnitSpeedParticle
import Components.MicroStructures.PlaneSegment
import PlaneSegment._

class PlaneSegmentTests extends FunSuite {

  test("Helpers - index of constant dimension") {
    val A = DenseVector(0D, 0D, 0D)
    val B = DenseVector(0D, 1D, 0D)
    val C = DenseVector(0D, 1D, 1D)
    val D = DenseVector(0D, 0D, 1D)

    val actualIndex = idxOfConstantDim(A, B, C, D)
    val expectedIndex = 0

    assert(expectedIndex == actualIndex)
  }

  test("Helpers - index of constant dimension when mis-specified") {
    val A = DenseVector(0D, 0D, 1D)
    val B = DenseVector(0D, 0D, 1D)
    val C = DenseVector(0D, 0D, 1D)
    val D = DenseVector(0D, 0D, 1D)

    assertThrows[SpecificationError](idxOfConstantDim(A, B, C, D))
  }

  test("Helpers - lin alg min/max") {
    val A = DenseVector(1D, 0D, 1D)
    val B = DenseVector(0D, 2D, 1D)
    val C = DenseVector(0D, 0D, 3D)
    val D = DenseVector(4D, 0D, 1D)

    val M = DenseMatrix(A, B, C, D)

    val actualMin: DenseVector[Double] = M(::, *).map(dv => min(dv)).t.toDenseVector
    val actualMax: DenseVector[Double] = M(::, *).map(dv => max(dv)).t.toDenseVector

    val expectedMin = DenseVector(0D, 0D, 1D)
    val expectedMax = DenseVector(4D, 2D, 3D)

    assert(actualMin == expectedMin)
    assert(actualMax == expectedMax)
  }

  test ("CollisionTime - time to collision") {
    val segment = PlaneSegment(
      DenseVector(0D, 0D, 0D),
      DenseVector(0D, 1D, 0D),
      DenseVector(0D, 1D, 1D),
      DenseVector(0D, 0D, 1D),
      1D
    )

    val initPath = UnitSpeedParticle(
      origin = DenseVector(0.5D, 0.5D, 0.5D),
      endpoint = DenseVector(0.25D, 0.5D, 0.5D)
    )

    val expectedTimeToCollide = 2.0
    val actualTimeToCollide = segment.getTimeToCollision(initPath)

    assert(actualTimeToCollide == expectedTimeToCollide )
  }

  test ("PostCollisionVector - simple path") {
    val segment = PlaneSegment(
      DenseVector(0D, 0D, 0D),
      DenseVector(0D, 1D, 0D),
      DenseVector(0D, 1D, 1D),
      DenseVector(0D, 0D, 1D),
      billiardCellLength = 1D,
      collisionDirection = 1D
    )

    val incomingParticle = UnitSpeedParticle(
      origin = DenseVector(0.5D, 0.5D, 0.5D),
      endpoint = DenseVector(0.25D, 0.5D, 0.5D)
    )

    val actualPostCollisionOrigin = segment
      .getPostCollisionPath(incomingParticle, 2.0)
      .origin

    val expectedPostCollisionOrigin = DenseVector(1.0D, 0.5D, 0.5D)

    assert(actualPostCollisionOrigin == expectedPostCollisionOrigin)
  }


  test ("PostCollisionVector - alternate direction ") {
    val segment = PlaneSegment(
      DenseVector(1D, 0D, 0D),
      DenseVector(1D, 0D, 1D),
      DenseVector(1D, 1D, 0D),
      DenseVector(1D, 1D, 1D),
      billiardCellLength = 1D,
      collisionDirection = -1D
    )

    val incomingParticle = UnitSpeedParticle(
      origin = DenseVector(0.5D, 0.5D, 0.5D),
      endpoint = DenseVector(0.75D, 0.5D, 0.5D)
    )

    val actualPostCollisionOrigin = segment
      .getPostCollisionPath(incomingParticle, segment.getTimeToCollision(incomingParticle))
      .origin

    val expectedPostCollisionOrigin = DenseVector(0D, 0.5D, 0.5D)

    assert(actualPostCollisionOrigin == expectedPostCollisionOrigin)
  }

  test ("PostCollisionVector - complex path") {
    val segment = PlaneSegment(
      DenseVector(0D, 0D, 0D),
      DenseVector(0D, 1D, 0D),
      DenseVector(0D, 1D, 1D),
      DenseVector(0D, 0D, 1D),
      billiardCellLength = 1D,
      collisionDirection = 1D
    )

    val incomingParticle = UnitSpeedParticle(
      origin = DenseVector(0.9D, 0.5D, 0.5D),
      endpoint = DenseVector(0.45D, 0.4D, 0.3D)
    )

    val actualPostCollisionOrigin = segment
      .getPostCollisionPath(incomingParticle, segment.getTimeToCollision(incomingParticle))
      .origin

    val expectedPostCollisionOrigin = DenseVector(1D, 0.3D, 0.1D)

    assert(vectorWithinTolerance(actualPostCollisionOrigin, expectedPostCollisionOrigin))
  }


}
