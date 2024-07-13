package pl.edu.mimuw.matrix.types;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.MatrixCellValue;
import pl.edu.mimuw.matrix.Shape;

import java.util.*;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.*;
import static pl.edu.mimuw.matrix.MatrixCellValue.cell;


public class SparseMatrix extends AbstractMatrix implements IDoubleMatrix {
    private final MatrixCellValue[] values;
    private final Shape shape;
    private SparseMatrix(Shape shape, MatrixCellValue... values){
        this.shape = shape;
        this.values = values;
    }
    public static SparseMatrix getSparseMatrix(Shape shape, MatrixCellValue... values){
        assert shape != null;
        assert values != null;
        assert values.length >= 1;
        for(MatrixCellValue value : values){
            assert value.row < shape.rows && value.row >= 0;
            assert value.column < shape.columns && value.column >= 0;
        }
        return new SparseMatrix(shape, values);
    }

    public MatrixCellValue[] getValues(){
        return values;
    }


    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        super.timesMatrixAssert(other);
        if(other.getClass() == SparseMatrix.class){
            //grouping all cells in 1st SM by row.
            Map <Integer, ArrayList <MatrixCellValue>> groupByRow1stM = new Hashtable<>();
            for(MatrixCellValue cell : values){
                groupByRow1stM.computeIfAbsent(cell.row, k -> new ArrayList<>());
                ArrayList <MatrixCellValue> newValue = groupByRow1stM.get(cell.row);
                newValue.add(cell);
                groupByRow1stM.put(cell.row, newValue);
            }

            //grouping all cells in 2nd SM by column
            Map <Integer, ArrayList <MatrixCellValue>> groupByCol2ndM = new Hashtable<>();
            for(MatrixCellValue cell : ((SparseMatrix) other).getValues()){
                groupByCol2ndM.computeIfAbsent(cell.column, k -> new ArrayList<>());

                ArrayList <MatrixCellValue> newCol = groupByCol2ndM.get(cell.column);
                newCol.add(cell);
                groupByCol2ndM.put(cell.column, newCol);
            }

            ArrayList <MatrixCellValue> newValuesList = new ArrayList<>();
            for(Integer row : groupByRow1stM.keySet()){
                for(Integer col : groupByCol2ndM.keySet()){
                    double sum = 0;
                    for(MatrixCellValue cell1 : groupByRow1stM.get(row)){
                        Optional<MatrixCellValue> cell2Match = groupByCol2ndM.get(col).stream().filter(x -> x.row == cell1.column).findFirst();
                        if (cell2Match.isPresent()) {
                            sum += cell1.value * cell2Match.get().value;
                        }
                    }
                    newValuesList.add(cell(row, col, sum));
                }

            }
            return sparse(Shape.matrix(shape.rows, other.shape().columns), newValuesList.toArray(new MatrixCellValue[0]));

        }
        return super.times(other);
    }




    @Override
    public IDoubleMatrix times(double scalar) {
        if(scalar == 0){
            return zero(shape());
        }
        MatrixCellValue[] finalCells = new MatrixCellValue[values.length];
        for(int i = 0; i < values.length; i++){
            finalCells[i] = cell(values[i].row, values[i].column, values[i].value * scalar);
        }
        return sparse(shape(), finalCells);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        super.assertMatircesEqual(other);
        if(other.getClass() == SparseMatrix.class){
            MatrixCellValue[] otherValues = ((SparseMatrix) other).getValues();
            MatrixCellValue[] finalCells = new MatrixCellValue[values.length + otherValues.length];
            System.arraycopy(values, 0, finalCells, 0, values.length);
            System.arraycopy(otherValues, 0, finalCells, values.length, otherValues.length);
            for(int otherValuesIndex = 0; otherValuesIndex < otherValues.length; otherValuesIndex++){
                for(int i = 0; i < values.length; i++){
                    if(finalCells[i].row == otherValues[otherValuesIndex].row && finalCells[i].column == otherValues[otherValuesIndex].column){
                        finalCells[i] = cell(finalCells[i].row, finalCells[i].column, finalCells[i].value + otherValues[otherValuesIndex].value);
                        finalCells[values.length + otherValuesIndex] = null;
                    }
                }
            }
            MatrixCellValue[] returnObject = new MatrixCellValue[Arrays.stream(finalCells).filter(Objects::nonNull).toArray().length];
            int n = 0;
            for (MatrixCellValue finalCell : finalCells) {
                if (finalCell != null) {
                    returnObject[n] = finalCell;
                    n++;
                }
            }
            return sparse(shape(), returnObject);
        }

        return super.plus(other);

    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        if(scalar == 0){
            return zero(shape());
        }
        MatrixCellValue[] finalCells = new MatrixCellValue[values.length];
        for(int i = 0; i < values.length; i++){
            finalCells[i] = cell(values[i].row, values[i].column, values[i].value + scalar);
        }
        return sparse(shape(), finalCells);
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
        for(MatrixCellValue point : values){
            if(point.contains(row, column)){
                return point.value;
            }
        }
        return 0;
    }

    @Override
    public double[][] data() {
        double[][] data = new double[shape.rows][shape.columns];
        for(MatrixCellValue cell : values){
            data[cell.row][cell.column] = cell.value;
        }
        return data;
    }

    @Override
    public double normOne() {
        double[] valuesNorm = new double[shape.columns];
        for(MatrixCellValue cell : values){
            valuesNorm[cell.column] += Math.abs(cell.value);
        }
        return Arrays.stream(valuesNorm).max().getAsDouble();
    }

    @Override
    public double normInfinity() {
        double[] valuesNorm = new double[shape.rows];
        for(MatrixCellValue cell : values){
            valuesNorm[cell.row] += Math.abs(cell.value);
        }
        return Arrays.stream(valuesNorm).max().getAsDouble();
    }

    @Override
    public double frobeniusNorm() {
        double[] valuesL = Arrays.stream(values).mapToDouble(x -> x.value).toArray();
        return Math.sqrt(Arrays.stream((Arrays.stream(valuesL).map(x -> Math.pow(x, 2)).toArray())).sum());
    }

    @Override
    public Shape shape() {
        return this.shape;
    }

    @Override
    public String toString(){
        if(shape.rows <= 3){
            return super.toString();
        }

        Arrays.sort(values, Comparator.comparingInt((MatrixCellValue o) -> o.row).thenComparingInt(o -> o.row));
        String[] zerRow = new String[shape.columns];
        Arrays.fill(zerRow, "0");
        StringBuilder finalString = new StringBuilder("[");
        if(values[0].row < 3 && values[0].row > 0){
             finalString.append((Arrays.toString(zerRow) + ", ").repeat(values[0].row - 1)).append(Arrays.toString(zerRow));
        }else{
            finalString.append("[").append(Arrays.toString(zerRow)).append("...").append(Arrays.toString(zerRow));
        }
        for(int i = 0; i < values.length; i++){
            if(i == values.length - 1 || (i < values.length - 1 && values[i].row != values[i + 1].row)) {
                String[] currentRow = new String[shape.columns];
                Arrays.fill(currentRow, "0");
                String zeroString = Arrays.toString(currentRow.clone());
                for (MatrixCellValue cell : values) {
                    if (cell.row == values[i].row) {
                        currentRow[cell.column] = Double.toString(cell.value);
                    }
                }
                String row = Arrays.toString(currentRow);

                if(i != 0){
                    finalString.append(", ").append(row);
                }else{
                    finalString.append(row);
                }

                if (i == values.length - 1) {
                    if (shape.rows - values[i].row >= 3) {
                        finalString.append(", ").append(zeroString).append("...").append(zeroString);
                    } else {
                        finalString.append((", " + zeroString).repeat(shape.rows - values[i].row - 1));
                    }
                } else if(values[i].row != values[i + 1].row){
                    if (values[i + 1].row - values[i].row >= 3 ) {
                        finalString.append(", ").append(zeroString).append("...").append(zeroString);
                    } else {
                        finalString.append((", " + zeroString).repeat(values[i + 1].row - values[i].row - 1));
                    }
                }
            }

        }
        return finalString + "]";
    }
}
