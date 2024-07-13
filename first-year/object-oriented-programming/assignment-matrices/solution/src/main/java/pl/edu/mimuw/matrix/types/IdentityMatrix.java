package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import java.util.Arrays;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.diagonal;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.zero;

public class IdentityMatrix extends AbstractMatrix implements IDoubleMatrix {

    private final Shape shape;

    private IdentityMatrix(int size){
        this.shape = Shape.matrix(size, size);
    }
    public static IdentityMatrix getIdentityMatrix(int size){
        return new IdentityMatrix(size);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);

        if(other.getClass() == IdentityMatrix.class){
            return this;
        }
        if(other.getClass() == DiagonalMatrix.class){
            return other;
        }
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        if(scalar == 0){
            return zero(shape());
        }
        double[] values = new double[this.shape().rows];
        Arrays.fill(values, scalar);
        return diagonal(values);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        return other.plus(this);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
     return super.plus(scalar);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times(-1));
    }

    @Override
    public IDoubleMatrix minus(double scalar) {
        return plus(scalar * -1);
    }

    @Override
    public double get(int row, int column) {
        getAssert(row, column);
        return row == column ? 1 : 0;
    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][shape.rows];
        for(int i = 0; i < shape.rows; i++){
            data[i][i] = 1;
        }
        return data;
    }

    @Override
    public double normOne() {
        return 1;
    }

    @Override
    public double normInfinity() {
        return 1;
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(shape.rows);
    }

    @Override
    public Shape shape() {
        return shape;
    }
}
