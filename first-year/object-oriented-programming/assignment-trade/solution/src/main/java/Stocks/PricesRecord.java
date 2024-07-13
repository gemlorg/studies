package Stocks;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.Objects;

import static java.lang.Double.NaN;

public class PricesRecord {
     private double food;
     private double clothes;
     private double tools;
     private double programs;

    private int foodCounter = 0;
    private int clothesCounter = 0;
    private int toolsCounter = 0;
    private int programsCounter = 0;


    public PricesRecord(double food, double clothes, double tools, double programs) {
        this.food = food;
        this.clothes = clothes;
        this.tools = tools;
        this.programs = programs;
    }

    public PricesRecord() {
        this.food = 0;
        this.clothes = 0;
        this.tools = 0;
        this.programs = 0;

    }
    public void passMax(String type, double price){
        switch (type){
            case "food":
                if(price > food || food == 0){
                    food = price;
                }
                break;
            case "clothes":
                if(price > clothes || clothes == 0){
                    clothes = price;
                }
                break;
            case "tools":
                if(price > tools || tools == 0){
                    tools = price;
                }
                break;
            case "programs":
                if(price > programs || programs == 0){
                    programs = price;
                }
                break;
        }
    }
    public void passMin(String type, double price){
        switch (type){
            case "food":
                if(price < food || food == 0){
                    food = price;
                }
                break;
            case "clothes":
                if(price < clothes || clothes == 0){
                    clothes = price;
                }
                break;
            case "tools":
                if(price < tools || tools == 0){
                    tools = price;
                }
                break;
            case "programs":
                if(price < programs || programs == 0){
                    programs = price;
                }
                break;
        }
    }

    public void passAverage(String type, int amount, double price){
        if(amount == 0){
            return;
        }
        switch (type) {
            case "food" -> {
                food *= foodCounter;
                food += amount * price;
                foodCounter += amount;
                if (foodCounter == 0) {
                    break;

                }
                food /= foodCounter;
            }
            case "clothes" -> {
                clothes *= clothesCounter;
                clothes += amount * price;
                clothesCounter += amount;
                if (clothesCounter == 0) {
                    break;
                }
                clothes /= clothesCounter;
            }
            case "tools" -> {
                tools *= toolsCounter;
                tools += amount * price;
                toolsCounter += amount;
                if (toolsCounter == 0) {
                    break;
                }
                tools /= toolsCounter;
            }
            case "programs" -> {
                programs *= programsCounter;
                programs += amount * price;
                programsCounter += amount;
                if (programsCounter == 0) {
                    break;
                }
                programs /= programsCounter;
            }
        }
    }

    public double food() {
        return food;
    }

    public double clothes() {
        return clothes;
    }

    public double tools() {
        return tools;
    }

    public double programs() {
        return programs;
    }

    public record pricesJson(double food, double clothes, double tools, double programs){}

    public JsonNode toJson() throws IOException {
        pricesJson temp = new pricesJson(food, clothes, tools, programs);
        ObjectMapper mapper = new ObjectMapper();
        mapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        String tempToString = mapper.writeValueAsString(temp);
        return new ObjectMapper().readTree(tempToString);
    }

    public void checkPrices(int day, String mode){

        JsonNode data = GameData.dataFromDay(day - 1);
        switch (mode) {
            case "max" -> {
                if (food <= 0 || Double.isNaN(food)) {
                    food = data.get("pricesMax").get("food").asDouble();
                }
                if (clothes <= 0 || Double.isNaN(clothes)) {
                    clothes = data.get("pricesMax").get("clothes").asDouble();
                }
                if (tools <= 0 || Double.isNaN(tools)) {
                    tools = data.get("pricesMax").get("tools").asDouble();
                }
                if (programs <= 0 || Double.isNaN(programs)) {
                    programs = data.get("pricesMax").get("programs").asDouble();
                }
            }
            case "avg" -> {
                if (food <= 0 || Double.isNaN(food)) {
                    food = data.get("pricesAverage").get("food").asDouble();
                }
                if (clothes <= 0 || Double.isNaN(clothes)) {
                    clothes = data.get("pricesAverage").get("clothes").asDouble();
                }
                if (tools <= 0 || Double.isNaN(tools)) {
                    tools = data.get("pricesAverage").get("tools").asDouble();
                }
                if (programs <= 0 || Double.isNaN(programs)) {
                    programs = data.get("pricesAverage").get("programs").asDouble();
                }
            }
            case "min" -> {
                if (food <= 0 || Double.isNaN(food)) {
                    food = data.get("pricesMin").get("food").asDouble();
                }
                if (clothes <= 0 || Double.isNaN(clothes)) {
                    clothes = data.get("pricesMin").get("clothes").asDouble();
                }
                if (tools <= 0 || Double.isNaN(tools)) {
                    tools = data.get("pricesMin").get("tools").asDouble();
                }
                if (programs <= 0 || Double.isNaN(programs)) {
                    programs = data.get("pricesMin").get("programs").asDouble();
                }
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        var that = (PricesRecord) obj;
        return Double.doubleToLongBits(this.food) == Double.doubleToLongBits(that.food) &&
                Double.doubleToLongBits(this.clothes) == Double.doubleToLongBits(that.clothes) &&
                Double.doubleToLongBits(this.tools) == Double.doubleToLongBits(that.tools) &&
                Double.doubleToLongBits(this.programs) == Double.doubleToLongBits(that.programs);
    }

    @Override
    public int hashCode() {
        return Objects.hash(food, clothes, tools, programs);
    }

    @Override
    public String toString() {
        return "PricesRecord[" +
                "food=" + food + ", " +
                "clothes=" + clothes + ", " +
                "tools=" + tools + ", " +
                "programs=" + programs + ']';
    }




}
