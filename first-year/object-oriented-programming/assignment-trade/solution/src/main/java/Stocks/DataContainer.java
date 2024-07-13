package Stocks;

import Players.Speculate;
import Players.Worker;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.w3c.dom.ls.LSOutput;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Dictionary;

//Contains data for certain day
public class DataContainer {
    private int day;
    private PricesRecord pricesAverage;
    private PricesRecord pricesMax;
    private PricesRecord pricesMin;
    private ArrayList<Worker> workers;
    private ArrayList<Speculate> speculates;


    public DataContainer() throws JsonProcessingException {}


    public DataContainer(int currentDay) throws JsonProcessingException {
        this.day = currentDay;
        this.pricesMin = new PricesRecord();
        this.pricesMax = new PricesRecord();
        this.pricesAverage = new PricesRecord();

    }
    public void passOffer(String type ,int amount, double price){
        pricesMin.passMin(type, price);
        pricesAverage.passAverage(type, amount, price);
        pricesMax.passMax(type, price);

    }
    public double getPrice(String mode, String type) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        switch (mode){
            case "max":
                return (double) pricesMax.getClass().getDeclaredMethod(type).invoke(pricesMax);
            case "min":
                return (double) pricesMin.getClass().getDeclaredMethod(type).invoke(pricesMin);
            case "avg":
                return (double) pricesAverage.getClass().getDeclaredMethod(type).invoke(pricesAverage);
            default:
                return 0;
        }
    }

    public void addWorkers(ArrayList<Worker> workers){
        this.workers = workers;
    }

    public void addSpeculates(ArrayList<Speculate> speculates){
        this.speculates = speculates;
    }

    public JsonNode toJson() throws IOException {
        DataContainerJson json = new DataContainerJson();
        json.setDay(day);
        json.setPricesMin(pricesMin.toJson());
        json.setPricesAverage(pricesAverage.toJson());
        json.setPricesMax(pricesMax.toJson());
        ArrayList<JsonNode> workersJson = new ArrayList<>();
        ArrayList<JsonNode> speculatesJson = new ArrayList<>();
        for (Worker worker : workers){
            workersJson.add(worker.toJson());
        }
        for(Speculate speculate : speculates){
            speculatesJson.add(speculate.toJson());
        }
        json.setWorkers(workersJson);
        json.setSpeculates(speculatesJson);

        ObjectMapper mapper = new ObjectMapper();
        mapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        String jsonInString = mapper.writeValueAsString(json);
        ObjectNode jsonObject = (ObjectNode) new ObjectMapper().readTree(jsonInString);
        return jsonObject;
    }

    public void checkPrices(){
        pricesMin.checkPrices(day, "min");
        pricesAverage.checkPrices(day, "avg");
        pricesMax.checkPrices(day, "max");
    }

    public int getDay() {
        return day;
    }

    public void setDay(int day) {
        this.day = day;
    }

    public PricesRecord getPricesAverage() {
        return pricesAverage;
    }

    public void setPricesAverage(PricesRecord pricesAverage) {
        this.pricesAverage = pricesAverage;
    }

    public PricesRecord getPricesMax() {
        return pricesMax;
    }

    public void setPricesMax(PricesRecord pricesMax) {
        this.pricesMax = pricesMax;
    }

    public PricesRecord getPricesMin() {
        return pricesMin;
    }

    public void setPricesMin(PricesRecord pricesMin) {
        this.pricesMin = pricesMin;
    }

    public ArrayList<Worker> getWorkers() {
        return workers;
    }

    public void setWorkers(ArrayList<Worker> workers) {
        this.workers = workers;
    }

    public ArrayList<Speculate> getSpeculates() {
        return speculates;
    }

    public void setSpeculates(ArrayList<Speculate> speculates) {
        this.speculates = speculates;
    }

    @Override
    public String toString() {
        return "DataContainer{" +
                "day=" + day +
                ", pricesAverage=" + pricesAverage +
                ", pricesMax=" + pricesMax +
                ", pricesMin=" + pricesMin +
                ", workers=" + workers +
                ", speculates=" + speculates +
                '}';
    }

}
