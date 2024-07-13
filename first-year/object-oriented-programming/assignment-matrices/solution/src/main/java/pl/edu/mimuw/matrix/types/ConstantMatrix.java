package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import java.util.Arrays;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.constant;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.zero;

public class ConstantMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final double constant;
    private final Shape shape;
    private ConstantMatrix(Shape shape, double constant){
        this.shape = shape;
        this.constant = constant;
    }

    public static IDoubleMatrix getConstantMatrix(Shape shape, double constant){
        assert shape != null;
        if(constant == 0){
            return zero(shape);
        }
        return new ConstantMatrix(shape, constant);

    }

    public double getConstant(){
        return constant;
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        if(other.getClass() == ConstantMatrix.class){
            return times(((ConstantMatrix) other).getConstant());
        }
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        if(scalar == 0){
            return zero(shape);
        }
        return constant(shape, constant * scalar);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        return other.plus(constant);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        if(scalar == 0){
            return constant(shape, constant);
        }
        return constant(shape, constant + scalar);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times( -1));
    }

    @Override
    public IDoubleMatrix minus(double scalar) {
        return plus(scalar * -1);
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return constant;
    }

    @Override
    public double[][] data() {
        double[][] data =  new double[shape.rows][shape.columns];
        for(int i = 0; i < shape.rows; i++){
             Arrays.fill(data[i], constant);
        }
        return data;
    }

    @Override
    public double normOne() {
        return Math.abs(constant) * shape.columns;
    }

    @Override
    public double normInfinity() {
        return Math.abs(constant) * shape.rows;
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(Math.abs(constant) * shape.rows * shape.columns);
    }

    @Override
    public Shape shape() {
        return shape;
    }

    @Override
    public String toString(){
        if(shape.rows <= 2){
            return super.toString();
        }
        String firstString = "[" + (constant + ", ").repeat(shape.rows - 1) + constant + "]";
        return "[" + firstString + "..." + firstString + "]";
    }
}
