package Strategies;

import Players.Strategies.Career;
import Players.Strategies.ProductionReserve;
import Players.Strategies.Productivity;
import com.fasterxml.jackson.databind.JsonNode;
import Stocks.GameData;

import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Random;

public class ProductionStrategy {
    private final String strategy;
    private int historyPerspective;

    public ProductionStrategy(JsonNode strategyInfo, JsonNode perspective){



        strategy = strategyInfo.get("type").asText();

        try {
            historyPerspective = perspective.asInt();
        }catch(NullPointerException e){
            try{
                historyPerspective = strategyInfo.get("historyPerspective").asInt();

            }catch (NullPointerException b){

            }
        }

    }
    public String decideWhatToMake(int currentDay, Career career, Productivity productivity, ProductionReserve reserve) {
        Random r = new Random();
        int diamonds;
        int food;
        int clothes;
        int tools;
        int programs;
        int maximum;
        JsonNode data;
        switch (strategy) {
            case "closeSighted":

                    JsonNode averagePrices = GameData.dataFromDay(currentDay - 1).get("pricesAverage");
                    double max = 0;
                    for (JsonNode node : averagePrices) {
                        if (node.asDouble() > max) {
                            max = node.asDouble();
                        }
                    }

                Iterator<String> ee =  averagePrices.fieldNames();
                    while(ee.hasNext()) {
                        String nextField = ee.next();
                        if(averagePrices.get(nextField).asDouble() >= max){
                            return nextField;
                        }
                    }
            case "greedy":
                data = GameData.dataFromDay(currentDay - 1).get("pricesAverage");
                diamonds = productivity.diamonds(career, reserve);
                food = productivity.food(career, reserve) * data.get("food").asInt();
                clothes = productivity.clothes(career, reserve) * data.get("clothes").asInt();
                tools = productivity.tools(career, reserve) * data.get("tools").asInt();
                programs = productivity.programs(career, reserve) * data.get("programs").asInt();
                maximum = Arrays.stream((new int[] {diamonds, food, clothes, tools, programs})).max().getAsInt();
                return diamonds == maximum ? "diamonds" : food == maximum ? "food" : clothes == maximum ? "clothes" : tools == maximum ? "tools" : "programs";

            case "average":
                if(currentDay <= historyPerspective){
                    return new String[]{"clothes", "tools", "programs", "diamonds", "food"}[r.nextInt(4)];
                }
                 food = 0;
                clothes = 0;
                tools = 0;
                programs = 0;

                for(int i = currentDay - 1; i >= currentDay - historyPerspective; i--){
                    data = GameData.dataFromDay(i).get("pricesAverage");
                    food += data.get("food").asInt();
                    clothes += data.get("clothes").asInt();
                    tools += data.get("tools").asInt();
                    programs += data.get("programs").asInt();
                }
                maximum = Arrays.stream((new int[] {food, clothes, tools, programs})).max().getAsInt();
                return food == maximum ? "food" : clothes == maximum ? "clothes" : tools == maximum ? "tools" : "programs";

            case "perspective":
                if(currentDay < historyPerspective){
                    historyPerspective = currentDay;
                }
                data = GameData.dataFromDay(currentDay - 1).get("pricesAverage");
                food = data.get("food").asInt();
                clothes = data.get("clothes").asInt();
                tools = data.get("tools").asInt();
                programs = data.get("programs").asInt();

                if(historyPerspective != 0) {
                    data = GameData.dataFromDay(currentDay - historyPerspective).get("pricesAverage");
                    food -= data.get("food").asInt();
                    clothes -= data.get("clothes").asInt();
                    tools -= data.get("tools").asInt();
                    programs -= data.get("programs").asInt();
                }
                maximum = Arrays.stream((new int[] {food, clothes, tools, programs})).max().getAsInt();
                return food == maximum ? "food" : clothes == maximum ? "clothes" : tools == maximum ? "tools" : "programs";


            case "random":
                return new String[]{"clothes", "tools", "programs", "diamonds", "food"}[r.nextInt(5)];
            default:
                System.out.println("ProductionStrategy went wrong");
                return null;
        }
    }

}
