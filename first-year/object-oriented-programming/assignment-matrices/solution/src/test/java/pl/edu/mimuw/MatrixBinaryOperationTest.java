package pl.edu.mimuw;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;
import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.constant;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.zero;
import static pl.edu.mimuw.matrix.MatrixCellValue.cell;
import static pl.edu.mimuw.matrix.Shape.matrix;

public class MatrixBinaryOperationTest {

  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testPlusMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.plus(r).data();

    final var expectedResult = new double[][]{
      new double[]{2, 4, 6},
      new double[]{8, 10, 12},
    };

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testMinusMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.minus(r).data();

    final var expectedResult = new double[][]{
      new double[]{0, 0, 0},
      new double[]{0, 0, 0},
    };

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixTransposedShapeArgumentProvider.class)
  void testTimesMatrices(IDoubleMatrix l, IDoubleMatrix r) {
    final var result = l.times(r).data();

    final var expectedResult = new double[][]{
      new double[]{22, 28},
      new double[]{49, 64},
    };

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testTimesScalar(IDoubleMatrix m) {
    final var result = m.times(2).minus(m).data();
    final var expectedResult = m.data();

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testTimesMinusScalar(IDoubleMatrix m) {
    final var result = m.times(-2).plus(m).data();
    final var expectedResult = m.times(-1).data();

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testPlusMinusScalar(IDoubleMatrix m) {
    final var result = m.plus(42).minus(42).data();
    final var expectedResult = m.data();

    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testMinusPlusScalar(IDoubleMatrix m) {
    final var result = m.plus(42).minus(42).data();
    final var expectedResult = m.data();

    assertArrayEquals(expectedResult, result);
  }

  @Test
  void testPlusSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 42),
      cell(767, 123_123, 24),
      cell(999_999, 999_999_999, 66)
    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 24),
      cell(767, 123_123, 42)
    );
    final var result = l.plus(r);

    assertEquals(66, result.get(0, 0));
    assertEquals(66, result.get(767, 123_123));
    assertEquals(66, result.get(999_999, 999_999_999));
  }

  @Test
  void testMinusSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 42),
      cell(767, 123_123, 24),
      cell(999_999, 999_999_999, 66)
    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 24),
      cell(767, 123_123, 42)
    );
    final var result = l.minus(r);

    assertEquals(18, result.get(0, 0));
    assertEquals(-18, result.get(767, 123_123));
    assertEquals(66, result.get(999_999, 999_999_999));
  }

  @Test
  void testTimesSparseMatrices() {
    final var l = DoubleMatrixFactory.sparse(
      matrix(1_000_000, 1_000_000_000),
      cell(0, 0, 3),
      cell(0, 213, 2),
      cell(0, 555_555, 66),

      cell(456_456, 1, 7),
      cell(456_456, 321, 8),
      cell(456_456, 444_444, 66)

    );
    final var r = DoubleMatrixFactory.sparse(
      matrix(1_000_000_000, 1_000_000),
      cell(0, 0, 4),
      cell(213, 0, 5),
      cell(666_666, 0, 66),

      cell(1, 456_456, 9),
      cell(321, 456_456, 10),
      cell(444_445, 456_456, 66)
    );
    final var result = l.times(r);

    assertEquals(22, result.get(0, 0));
    assertEquals(143, result.get(456_456, 456_456));
    assertEquals(0, result.get(42, 42));
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixSameArgumentProvider.class)
  void testZeroMatrixTimes(IDoubleMatrix l, IDoubleMatrix r) {
    final var z = zero(matrix(3, 2));
    final var result = l.times(z).times(r).data();
    final var expectedResult = new double[][]{
      new double[]{0, 0, 0},
      new double[]{0, 0, 0},
    };
    assertArrayEquals(expectedResult, result);
  }

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testZeroMatrixTimes(IDoubleMatrix m) {
    final var shape = m.shape();
    final var z = zero(matrix(shape.rows, shape.columns));
    final var expectedResult = m.data();
    assertArrayEquals(expectedResult, z.plus(m).data());
    assertArrayEquals(expectedResult, m.plus(z).data());
  }

  /** My tests **/

  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testConstantMatrixMinus(IDoubleMatrix m){
    final var shape = m.shape();
    final var m1 = constant(m.shape(), 2);
    final var m2 = constant(m.shape(), 1);
    final var result = m1.minus(m2);

    assertArrayEquals(m2.data(), result.data());


  }
  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testZeroPlusMatrix(IDoubleMatrix m){
    final var shape = m.shape();
    final var m1 = zero(shape);
    final var m2 = zero(shape);
    final var m3 = constant(shape, 1);
    final var resultM = m1.plus(0);
    final var result2 = resultM.plus(1);
    assertArrayEquals(result2.data(), m3.data());
    assertArrayEquals(m1.data(), m2.data());
  }
  @ParameterizedTest
  @ArgumentsSource(TestMatrixArgumentProvider.class)
  void testConstantTimesMatrix(IDoubleMatrix m){
    final var constM = constant(matrix(2, m.shape().rows), 2);
    final var result = constM.times(m);
    assertArrayEquals(result.data()[0], result.data()[1]);

  }
}
