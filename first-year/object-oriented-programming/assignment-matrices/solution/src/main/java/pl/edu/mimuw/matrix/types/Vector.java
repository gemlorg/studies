package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.DoubleMatrixFactory;
import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import java.util.Arrays;

public class Vector extends AbstractMatrix implements IDoubleMatrix {
    private final double[] values;
    private final Shape shape;
    private Vector(double... values){
        this.values = values;
        this.shape = Shape.vector(values.length);
    }
    public static Vector getVector(double... values){
        assert values != null;
        assert values.length >= 1;
        return new Vector(values);
    }

    public double[] getValues(){
        return values;
    }


    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        double[] copy = new double[values.length];
        for(int i = 0; i < values.length; i++){
            copy[i] = values[i] * scalar;
        }
        return DoubleMatrixFactory.vector(copy);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        double[] finalValues = new double[values.length];
        for(int i = 0; i < values.length; i++){
            finalValues[i] = values[i] + other.data()[i][0];
        }
        return DoubleMatrixFactory.vector(finalValues);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        double[] finalValues = new double[values.length];
        for(int i = 0; i < values.length; i++){
            finalValues[i] = values[i] + scalar;
        }
        return DoubleMatrixFactory.vector(finalValues);
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
        return values[column];

    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][1];
        for(int i = 0; i < shape.columns; i++){
            data[i][0] = values[i];
        }
        return data;
    }

    @Override
    public double normOne() {
        return Arrays.stream(Arrays.stream(values).map(Math::abs).toArray()).sum();
    }

    @Override
    public double normInfinity() {
        return Arrays.stream(Arrays.stream(values).map(Math::abs).toArray()).max().getAsDouble();
    }

    @Override
    public double frobeniusNorm() {
        return Math.sqrt(Arrays.stream((Arrays.stream(values).map(x -> Math.pow(x, 2)).toArray())).sum());
    }

    @Override
    public Shape shape() {
        return shape;
    }

    @Override
    public String toString(){
        StringBuilder finalString = new StringBuilder("[");
        for(double value : values){
            finalString.append("[").append(value).append("], ");

        }
        return finalString.substring(0, finalString.length() - 2) + "]";
    }
}
