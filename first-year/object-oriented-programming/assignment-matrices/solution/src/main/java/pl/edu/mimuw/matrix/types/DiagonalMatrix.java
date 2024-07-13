package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import java.util.Arrays;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.diagonal;

public final class DiagonalMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final double[] diagonalValues;
    private final Shape shape;

    private DiagonalMatrix(double... diagonalValues){
        this.diagonalValues = diagonalValues;
        this.shape = Shape.matrix(diagonalValues.length, diagonalValues.length);

    }

    public static DiagonalMatrix getDiagonalMatrix(double... diagonalValues){
        assert diagonalValues != null;
        assert diagonalValues.length >= 2;
        return new DiagonalMatrix(diagonalValues);
    }

    public double[] getDiagonalValues(){
        return diagonalValues;
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);

        if(other.getClass() == IdentityMatrix.class){
            return this;
        }
        if(other.getClass() == DiagonalMatrix.class){
            double[] finalValues = new double[diagonalValues.length];

            for(int i = 0; i < diagonalValues.length; i++){
                finalValues[i] = diagonalValues[i] * ((DiagonalMatrix) other).diagonalValues[i];
            }
            return diagonal(finalValues);
        }
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        if(scalar == 0){
            return DoubleMatrixFactory.zero(this.shape());
        }
        double[] finalValues = new double[diagonalValues.length];
        for(int i = 0; i < diagonalValues.length; i++){
            finalValues[i] = diagonalValues[i] * scalar;
        }

        return diagonal(finalValues);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        if(other.getClass() == DiagonalMatrix.class){
            double[] finalValues = new double[diagonalValues.length];
            for(int i = 0; i < diagonalValues.length; i++){
                finalValues[i] = diagonalValues[i] + ((DiagonalMatrix) other).getDiagonalValues()[i];
            }

            return diagonal(finalValues);
        }
        if(other.getClass() == IdentityMatrix.class){
            double[] newDiagonalValues = new double[diagonalValues.length];
            for(int i = 0; i < newDiagonalValues.length; i++){
                newDiagonalValues[i] = diagonalValues[i] + 1;
            }
            return diagonal(newDiagonalValues);
        }
        return super.plus(other);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times(-1));
    }

    @Override
    public double get(int row, int column) {
        getAssert(row, column);
        return row == column ? diagonalValues[row] : 0;
    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][shape.rows];
        for(int i = 0; i < shape.rows; i++){
            data[i][i] = diagonalValues[i];
        }
        return data;
    }

    @Override
    public double normOne() {
        return Arrays.stream((Arrays.stream(diagonalValues).map(Math::abs).toArray())).max().getAsDouble();
    }

    @Override
    public double normInfinity() {
        return normOne();
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(Arrays.stream((Arrays.stream(diagonalValues).map(x -> Math.pow(x, 2)).toArray())).sum());
    }

    @Override
    public Shape shape() {
        return shape;
    }
}
