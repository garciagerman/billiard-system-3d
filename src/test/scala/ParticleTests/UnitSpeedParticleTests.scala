package ParticleTests

import math.sqrt
import breeze.linalg._
import org.scalatest._
import Common.Utility._
import BilliardCellComponents.Particles.UnitSpeedParticle, UnitSpeedParticle._

class UnitSpeedParticleTests extends FunSuite {

  test("pathDirection/length of particle path"){

    val particle = UnitSpeedParticle(DenseVector(1D, 1D, 1D), DenseVector(2D,3D,4D))

    val actualNorm = particle.pathLength
    val expectedNorm = sqrt(14D)

    // check if norms are equals
    assert(actualNorm == expectedNorm)

    val actualDirection = particle.pathDirection
    val expectedDirection = DenseVector(1D, 2D, 3D)

    // check if pathDirection is as expected
    assert(withinTolerance(norm(actualDirection - expectedDirection), 0))
  }

  test("dot product of pathDirection between two paths") {
    val particleA = UnitSpeedParticle(DenseVector(2D,4D,6D))
    val particleB = UnitSpeedParticle(DenseVector(1D,2D,3D))

    val actualDotOfDirections = particleA.dotOfDirections(particleB)
    val expectedDotOfDirections = 28D

    assert(withinTolerance(actualDotOfDirections, expectedDotOfDirections))
  }

  test("scale length path of particle to unit") {

    val particle = UnitSpeedParticle(DenseVector(2D,2D,2D), DenseVector(1D,2D,3D))

    val actualUnitPath = particle.scaledPathToLength(1D)
    val expectedPath = UnitSpeedParticle(DenseVector(2D, 2D, 2D), DenseVector(2D, 2D, 2D) + ((1/sqrt(2)) *:* DenseVector(-1D, 0D, 1D)))

    // check if two path objects are equal
    assert(actualUnitPath == expectedPath)

    // check if path length is one (within tolerance)
    assert(withinTolerance(actualUnitPath.pathLength, 1D))

    // check if pathDirection vectors are close (within tolerance)
    assert(
      withinTolerance(norm(actualUnitPath.pathDirection - expectedPath.pathDirection), 0)
    )
  }

  test("scale length when throwing exception") {
    val particle = UnitSpeedParticle(DenseVector(0D, 0D, 0D))

    assertThrows[UnableToScalePath](particle.scaledPathToLength(47D))
  }
}
