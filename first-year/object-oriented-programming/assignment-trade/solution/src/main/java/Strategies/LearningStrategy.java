package Strategies;

import Players.Strategies.ProductionReserve;
import Stocks.GameData;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.Random;

public class LearningStrategy {
    private final String strategy;
    JsonNode data;
    public LearningStrategy(JsonNode learning){
        strategy = learning.get("type").asText();
        data = learning;
    }
    public boolean makeDecision(int currentDay, ProductionReserve reserve){
        switch (strategy){
            case "hardWorker":
                return false;
            case "saving":
                int diamondLimit = data.get("diamondLimit").asInt();
                return reserve.getDiamonds() > diamondLimit;


            case "periodic":
                int period = data.get("period").asInt();
                return currentDay % period == 0;
            case "student":
                int amount = data.get("productionReserve").asInt();
                int trackLength = data.get("period").asInt();
                if(currentDay <= trackLength){
                    trackLength = currentDay;
                }
                double averagePrice = 0;
                double foodCost;
                for(int i = currentDay - 1; i >= currentDay - trackLength; i--){
                    foodCost = GameData.dataFromDay(i).get("pricesAverage").get("food").asDouble();
                    averagePrice += foodCost;
                }
                averagePrice /= trackLength;

                return 100 * amount * averagePrice <= reserve.getDiamonds();

            case "random":
                Random r = new Random();
                double m = r.nextDouble();
                return m > (1 - 1/(double)(3 + currentDay));

            default:
                System.out.println("Something went wrong in LearningStrategy");
                return false;
        }

    }

}
