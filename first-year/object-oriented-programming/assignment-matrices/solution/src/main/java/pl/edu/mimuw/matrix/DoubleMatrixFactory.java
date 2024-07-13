package pl.edu.mimuw.matrix;

import pl.edu.mimuw.matrix.types.*;

public class DoubleMatrixFactory {


  //Intellij IDEA was used in the project.
  private DoubleMatrixFactory() {
  }


  public static IDoubleMatrix sparse(Shape shape, MatrixCellValue... values){
    return SparseMatrix.getSparseMatrix(shape, values);
  }

  public static IDoubleMatrix full(double[][] values) {
    return FullMatrix.getFullMatrix(values);
  }

  public static IDoubleMatrix identity(int size) {
    return IdentityMatrix.getIdentityMatrix(size);
  }

  public static IDoubleMatrix diagonal(double... diagonalValues) {
    return DiagonalMatrix.getDiagonalMatrix(diagonalValues);
  }

  public static IDoubleMatrix antiDiagonal(double... antiDiagonalValues) {
    return AntiDiagonalMatrix.getAntiDiagonalMatrix(antiDiagonalValues);
  }

  public static IDoubleMatrix vector(double... values){
    return Vector.getVector(values);
  }

  public static IDoubleMatrix zero(Shape shape) {
    return ZeroMatrix.getZeroMatrix(shape);
  }
  public static IDoubleMatrix constant(Shape shape, double constant){
    return ConstantMatrix.getConstantMatrix(shape, constant);
  }
}
