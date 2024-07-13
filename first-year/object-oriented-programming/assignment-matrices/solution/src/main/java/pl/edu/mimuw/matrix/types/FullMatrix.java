package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

public final class FullMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final double[][] values;
    private final Shape shape;

    private FullMatrix(double[][] values){
        this.values = values;
        this.shape = Shape.matrix(values.length, values[0].length);

    }
    public static FullMatrix getFullMatrix(double[][] values){
        assert values != null;
        assert values.length >= 2;
        assert values[0].length >= 1;
        for(int i = 0; i < values.length - 1; i++){
            assert values[i] != null;
            assert values[i].length == values[i + 1].length; // check if such matrix could exist
        }
        return new FullMatrix(values);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return super.times(scalar);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        return super.plus(other);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return super.plus(scalar);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {

        return super.plus(other.times(-1));
    }


    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return this.values[row][column];
    }

    @Override
    public double[][] data() {
        return this.values;
    }

    @Override
    public double normOne() {
        double max = 0;
        for(int col = 0; col < shape.columns; col++){
            double sum = 0;
            for(int row = 0; row < values.length; row++){
                sum += Math.abs(this.get(row, col));
            }
            if (sum > max) max = sum;
        }
        return max;
    }

    @Override
    public double normInfinity() {
        double max = 0;
        for(int row = 0; row < shape.rows; row++){
            double sum = 0;
            for(int col = 0; col < shape.columns; col++){
                sum += Math.abs(this.get(row, col));
            }
            if (sum > max) max = sum;
        }
        return max;
    }

    @Override
    public double frobeniusNorm() {
        double sum = 0;
        for(int row = 0; row < shape.rows; row++){
            for(int col = 0; col < shape.columns; col++){
                sum += Math.pow(this.get(row, col), 2);
            }

        }
        return Math.sqrt(sum);
    }

    @Override
    public Shape shape() {
        return shape;
    }
}
