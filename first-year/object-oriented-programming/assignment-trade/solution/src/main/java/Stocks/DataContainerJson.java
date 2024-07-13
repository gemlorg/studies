package Stocks;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;

//Contains data for certain day
public class DataContainerJson {
    private int day;
    private JsonNode pricesAverage;
    private JsonNode pricesMax;
    private JsonNode pricesMin;
    private ArrayList<JsonNode> workers;
    private ArrayList<JsonNode> speculates;




    public DataContainerJson(int day, JsonNode pricesAverage, JsonNode pricesMax, JsonNode pricesMin, ArrayList<JsonNode> workers, ArrayList<JsonNode> speculates){
        this.day = day;
        this.pricesAverage = pricesAverage;
        this.pricesMax = pricesMax;
        this.pricesMin = pricesMin;
        this.workers = workers;
        this.speculates = speculates;


    }
    public DataContainerJson(){}


    public int getDay() {
        return day;
    }

    public void setDay(int day) {
        this.day = day;
    }

    public JsonNode getPricesAverage() {
        return pricesAverage;
    }

    public void setPricesAverage(JsonNode pricesAverage) {
        this.pricesAverage = pricesAverage;
    }

    public JsonNode getPricesMax() {
        return pricesMax;
    }

    public void setPricesMax(JsonNode pricesMax) {
        this.pricesMax = pricesMax;
    }

    public JsonNode getPricesMin() {
        return pricesMin;
    }

    public void setPricesMin(JsonNode pricesMin) {
        this.pricesMin = pricesMin;
    }

    public ArrayList<JsonNode> getWorkers() {
        return workers;
    }

    public void setWorkers(ArrayList<JsonNode> workers) {
        this.workers = workers;
    }

    public ArrayList<JsonNode> getSpeculates() {
        return speculates;
    }

    public void setSpeculates(ArrayList<JsonNode> speculates) {
        this.speculates = speculates;
    }



}
