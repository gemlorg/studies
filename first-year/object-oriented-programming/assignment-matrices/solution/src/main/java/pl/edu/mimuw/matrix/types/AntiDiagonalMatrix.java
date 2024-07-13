package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import java.util.Arrays;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.antiDiagonal;

public class AntiDiagonalMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final double[] antiDiagonalValues;
    private final Shape shape;


    private AntiDiagonalMatrix(double... antiDiagonalValues){
        this.antiDiagonalValues = antiDiagonalValues;
        this.shape = Shape.matrix(antiDiagonalValues.length, antiDiagonalValues.length);
    }
    public static AntiDiagonalMatrix getAntiDiagonalMatrix(double... antiDiagonalValues){
        assert antiDiagonalValues != null;
        assert antiDiagonalValues.length >= 2;
        return new AntiDiagonalMatrix(antiDiagonalValues);
    }

    public double[] getAntiDiagonalValues(){
        return antiDiagonalValues;
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        if(other.getClass() == AntiDiagonalMatrix.class){
            double[] finalValues = new double[antiDiagonalValues.length];
            for(int i = 0; i < antiDiagonalValues.length; i++){
                finalValues[i] = antiDiagonalValues[i] * ((AntiDiagonalMatrix) other).getAntiDiagonalValues()[i];
            }
            return antiDiagonal(finalValues);
        }
        if(other.getClass() == DiagonalMatrix.class){
            double[] finalValues = new double[antiDiagonalValues.length];
            for(int i = 0; i < antiDiagonalValues.length; i++){
                finalValues[i] = antiDiagonalValues[i] * ((DiagonalMatrix) other).getDiagonalValues()[((DiagonalMatrix) other).getDiagonalValues().length - i - 1];
            }
            return antiDiagonal(finalValues);
        }
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        if(scalar == 0){
            return DoubleMatrixFactory.zero(shape);
        }
        double[] finalValues = new double[antiDiagonalValues.length];
        for(int i = 0; i < antiDiagonalValues.length; i++){
            finalValues[i] = antiDiagonalValues[i] * scalar;
        }
        return antiDiagonal(finalValues);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        if(other.getClass() == AntiDiagonalMatrix.class){
            double[] finalValues = new double[antiDiagonalValues.length];
            for(int i = 0; i < antiDiagonalValues.length; i++){
                finalValues[i] = antiDiagonalValues[i] + ((AntiDiagonalMatrix) other).getAntiDiagonalValues()[i];
            }
            return antiDiagonal(finalValues);
        }
        return super.plus(other);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times(-1));
    }


    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return row == shape.rows - 1 - column ? antiDiagonalValues[row] : 0;
    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][shape.rows];
        for(int i = 0; i < shape.rows; i++){
            data[i][shape.rows - 1 - i] = antiDiagonalValues[i];
        }
        return data;
    }

    @Override
    public double normOne() {
        return Arrays.stream((Arrays.stream(antiDiagonalValues).map(Math::abs).toArray())).max().getAsDouble();
    }

    @Override
    public double normInfinity() {
        return normOne();
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(Arrays.stream((Arrays.stream(antiDiagonalValues).map(x -> Math.pow(x, 2)).toArray())).sum());
    }

    @Override
    public Shape shape() {
        return shape;
    }
}