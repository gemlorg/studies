package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.constant;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.zero;

public class ZeroMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final Shape shape;
    private ZeroMatrix(Shape shape){
        this.shape = shape;
    }

    public static ZeroMatrix getZeroMatrix(Shape shape){
        assert shape != null;
        return new ZeroMatrix(shape);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        return zero(Shape.matrix(shape.rows, other.shape().columns));
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return this;
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        return other;
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return constant(shape, scalar);
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
        super.getAssert(row, column);
        return 0;
    }

    @Override
    public double[][] data() {
        return new double[shape.rows][shape.columns];
    }

    @Override
    public double normOne() {
        return 0;
    }

    @Override
    public double normInfinity() {
        return 0;
    }

    @Override
    public double frobeniusNorm() {
        return 0;
    }

    @Override
    public Shape shape() {
        return this.shape;
    }

    @Override
    public String toString(){
        if(shape.rows <= 2){
            return super.toString();
        }
        String firstString = "[" + (0 + ", ").repeat(shape.rows - 1) + 0 + "]";
        return "[" + firstString + "..." + firstString + "]";
    }

}
