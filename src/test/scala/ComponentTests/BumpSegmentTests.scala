package ComponentTests

import org.scalatest._
import Common.Utility._
import breeze.linalg.DenseVector
import BilliardCellComponents.MicroStructures.BumpSegment
import BilliardCellComponents.Particles.UnitSpeedParticle

class BumpSegmentTests extends FunSuite {

  test("bumpsCollision - time to collision") {
    val bumpCenter = DenseVector(0D, 0D, 0D)
    val bumpRadius = 1D
    val bump = BumpSegment(bumpCenter, bumpRadius)

    val incomingPath = UnitSpeedParticle(
      origin = DenseVector(0D, 1D, 2D),
      endpoint = DenseVector(0D, 1D, 1D)
    )

    val actualTimeToCollide = bump.getTimeToCollision(incomingPath)

    val expectedTimeToCollide = 2.0

    assert(withinTolerance(actualTimeToCollide, expectedTimeToCollide))
  }

  test("bumpsCollision - post collision vector") {
    val bumpCenter = DenseVector(0D, 0D, 0D)
    val bumpRadius = 1D
    val bump = BumpSegment(bumpCenter, bumpRadius)

    val incomingPath = UnitSpeedParticle(
      origin = DenseVector(0D, 1D, 2D),
      endpoint = DenseVector(0D, 1D, 1D)
    )
    val collisionPath = incomingPath.moveAlongPath(2)

    val actualPostCollisionPath = bump.getPostCollisionPath(collisionPath)

    val expectedPostCollisionPath = UnitSpeedParticle(
      origin = DenseVector(0D, 1D, 0D),
      endpoint = DenseVector(0D, 1D, -2D)
    )

    assert(pathWithinTolerance(actualPostCollisionPath, expectedPostCollisionPath))
  }

}
