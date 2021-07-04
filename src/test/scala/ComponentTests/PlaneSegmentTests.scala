package ComponentTests

import org.scalatest._
import Components.MicroStructures.PlaneSegment._
import Common.Helpers.withinTolerance
import breeze.linalg._
import math.sqrt

class PlaneSegmentTests extends FunSuite {

  test("index of constant dimension") {
    val A = DenseVector(0D, 0D, 0D)
    val B = DenseVector(0D, 1D, 0D)
    val C = DenseVector(0D, 1D, 1D)
    val D = DenseVector(0D, 0D, 1D)

    val actualIndex = idxOfConstantDim(A, B, C, D)
    val expectedIndex = 0

    assert(expectedIndex == actualIndex)
  }

  test("index of constant dimension when mis-specified") {
    val A = DenseVector(0D, 0D, 1D)
    val B = DenseVector(0D, 0D, 1D)
    val C = DenseVector(0D, 0D, 1D)
    val D = DenseVector(0D, 0D, 1D)

    assertThrows[SpecificationError](idxOfConstantDim(A, B, C, D))
  }

  test("lin alg min/max") {
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

}
