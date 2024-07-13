package pl.edu.mimuw;

import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.MatrixCellValue;
import pl.edu.mimuw.matrix.Shape;
import pl.edu.mimuw.matrix.types.*;
import pl.edu.mimuw.matrix.types.Vector;

import java.util.*;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.sparse;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.*;
import static pl.edu.mimuw.matrix.MatrixCellValue.*;
import static pl.edu.mimuw.matrix.Shape.*;

//Intellij IDEA was used in the project.
public class Main {
  public static void main(String[] args) {
    Shape shape = matrix(10, 10);
    Random r = new Random();

    AntiDiagonalMatrix antiM = (AntiDiagonalMatrix) antiDiagonal(generateRow(shape.rows));
    ConstantMatrix constM = (ConstantMatrix) constant(shape, r.nextInt(100));
    DiagonalMatrix diagM = (DiagonalMatrix) diagonal(generateRow(shape.rows));
    FullMatrix fullM = (FullMatrix) full(generateMatrix(shape.rows, shape.columns));
    IdentityMatrix idM = (IdentityMatrix) identity(shape.rows);
    SparseMatrix sparseM = (SparseMatrix) sparse(shape,
            cell(1, 1, r.nextInt(100)),
            cell(1, 2, r.nextInt(100)),
            cell(3, 3, r.nextInt(100)),
            cell(8, 4, r.nextInt(100)),
            cell(9, 5, r.nextInt(100)),
            cell(4, 6, r.nextInt(100)),
            cell(5, 7, r.nextInt(100)),
            cell(5, 8, r.nextInt(100)),
            cell(5, 9, r.nextInt(100))
    );
    Vector vector = (Vector) DoubleMatrixFactory.vector(generateRow(shape.rows));
    ZeroMatrix zeroM = (ZeroMatrix) zero(shape);
    double scalar = r.nextInt(100);

    System.out.println("AntiDiagonal matrix: " + antiM);
    System.out.println("***");
    System.out.println("ConstantMatrix: " + constM);
    System.out.println("***");
    System.out.println("DiagonalMatrix: " + diagM);
    System.out.println("***");
    System.out.println("FullMatrix: " + fullM);
    System.out.println("***");
    System.out.println("IdentityMatrix: " + idM);
    System.out.println("***");
    System.out.println("SparseMatrix: " + sparseM);
    System.out.println("***");
    System.out.println("Vector: " + vector);
    System.out.println("***");
    System.out.println("ZeroMatrix: " + zeroM);


    System.out.println("Multiplication: full x sparse");
    System.out.println(fullM.times(sparseM));
    System.out.println("Multiplication: sparse x sparse");
    System.out.println(sparseM.times(sparseM));
    System.out.println("Multiplication: sparse x " + scalar);
    System.out.println(sparseM.times(scalar));
    System.out.println("Multiplication: constant x " + scalar);
    System.out.println(constM.times(scalar));
    System.out.println("Sum: full + full");
    System.out.println(fullM.plus(fullM));
    System.out.println("Sum: full + " + scalar);
    System.out.println(fullM.plus(scalar));
    System.out.println("Subtraction: diagonal - antiDiagonal");
    System.out.println(diagM.minus(antiM));
    System.out.println("Subtraction: zero - " + scalar);
    System.out.println(zeroM.minus(scalar));



  }
  public static double[][] generateMatrix(int rows, int columns){
    Random r = new Random();
    double[][] matrix = new double[rows][rows];
    for(int i=0;i<rows;i++) {
      for (int j = 0; j < columns; j++) {
        matrix[i][j] = r.nextInt(100);
      }
    }
    return matrix;
  }

  public static double[] generateRow(int length){
    Random r = new Random();
    double[] matrix = new double[length];
    for(int i=0;i<length;i++) {
        matrix[i] = r.nextInt(100);
    }
    return matrix;
  }

}
