package ComponentTests

import org.scalatest._
import Components.MicroStructures.BumpSegment
import BumpSegment._
import Components.Particles.UnitSpeedParticle
import Common.Helpers._
import breeze.linalg.DenseVector

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

    val actualPostCollisionPath = bump.getPostCollisionPath(incomingPath, 2)

    val expectedPostCollisionPath = UnitSpeedParticle(
      origin = DenseVector(0D, 1D, 0D),
      endpoint = DenseVector(0D, 1D, -2D)
    )

    assert(pathWithinTolerance(actualPostCollisionPath, expectedPostCollisionPath))
  }

}
