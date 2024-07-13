package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.*;

public abstract class AbstractMatrix implements IDoubleMatrix{



    //assert such cell exists in a matrix

    void getAssert(int row, int column){
        assert row < shape().rows && row >= 0;
        assert column < shape().columns && column >= 0;
    }

    //assert matrices could be multiplied
    void timesMatrixAssert(IDoubleMatrix other){
        assert shape().columns == other.shape().rows;
    }
    @Override
    public IDoubleMatrix times(IDoubleMatrix other){
        Shape otherShape = other.shape();
        if(other.getClass() == ZeroMatrix.class){
            return zero(Shape.matrix(shape().rows, otherShape.columns));
        }
        if(other.getClass() == IdentityMatrix.class){
            return this;
        }

        double[][] data = data();
        double[][] otherData = other.data();
        double [][] result = new double[shape().rows][otherShape.columns];

        for(int row = 0; row < shape().rows; row++){
            for(int col = 0; col < otherShape.columns; col++){
                double sum = 0;
                for(int i = 0; i < shape().columns; i++){
                    sum += data[row][i] * otherData[i][col];
                }
                result[row][col] = sum;
            }
        }
        return full(result);
    }
    
    @Override
    public IDoubleMatrix times(double scalar) {
        if (scalar == 0) {
            return zero(shape());
        }
        double[][] thatValues = data();
        double[][] finalValues = new double[shape().rows][shape().columns];
        for (int row = 0; row < shape().rows; row++) {
            for (int col = 0; col < shape().columns; col++) {
                finalValues[row][col] = thatValues[row][col] * scalar;
            }
        }
        return full(finalValues);
    }

    public void assertMatircesEqual(IDoubleMatrix other){
        assert shape().rows == other.shape().rows && shape().columns == other.shape().columns;
    }
    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (other.getClass() == ZeroMatrix.class) {
            return this;
        }
        if (this.getClass() == ZeroMatrix.class) {
            return other;
        }

        double[][] thatValues = data();
        double[][] otherValues = other.data();
        double[][] finalValues = new double[shape().rows][shape().columns];

        for (int row = 0; row < shape().rows; row++) {
            for (int col = 0; col < shape().columns; col++) {
                finalValues[row][col] = thatValues[row][col] + otherValues[row][col];

            }
        }
        return full(finalValues);
    }
    @Override
    public IDoubleMatrix plus(double scalar) {
        if (scalar == 0) {
            return this;
        }
        double[][] thatValues = data();
        double[][] finalValues = new double[shape().rows][shape().columns];
        for (int row = 0; row < shape().rows; row++) {
            for (int col = 0; col < shape().columns; col++) {
                finalValues[row][col] += thatValues[row][col] + scalar;
            }
        }
        return full(finalValues);
    }
    
    @Override
    public IDoubleMatrix minus(double scalar){
        return plus(scalar * -1);
    }
    
    @Override
    public String toString(){
        double[][] data = data();
        String[][] returnString = new String[data.length][data[0].length];
        String[] retStr = new String[data.length];
        for(int i = 0; i < returnString.length; i++){
            for(int col = 0; col < data()[0].length; col++){
                returnString[i][col] = Double.toString(data[i][col]);
            }
            retStr[i] = "[" + String.join(", ", returnString[i] ) + "]";
        }
        return "[" + String.join(", ", retStr) + "]";
    }
}
