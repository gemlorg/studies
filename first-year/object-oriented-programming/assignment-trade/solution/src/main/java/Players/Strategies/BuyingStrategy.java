package Players.Strategies;

import Players.Strategies.Offers.WorkerOffer;
import Players.Worker;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;

public class BuyingStrategy {
    private final String strategy;
    private int numberOfTools;


    public BuyingStrategy(JsonNode strategy){

        this.strategy = strategy.asText();
        try {
            this.numberOfTools = strategy.get("numberOfTools").asInt();
        }catch (Exception NullPointerException){
            this.numberOfTools = 0;
        }

    }
    public void buy(int currentDay, int id, ProductionReserve reserve, ArrayList<WorkerOffer> buyOffers, ArrayList<WorkerOffer> sellOffers){
        int wealth = reserve.getDiamonds();
        buyOffers.add(WorkerOffer.newOffer(1, 100, "food", id, wealth));
        switch (strategy){
            case "technophobe":
                break;

            case "gadgeteer":
                int programsToBuy = 0;
                for(WorkerOffer offer : sellOffers){
                    programsToBuy += offer.getQuantity();
                }
                buyOffers.add(WorkerOffer.newOffer(1, programsToBuy, "programs", id, wealth));
            case "mechanised":
                buyOffers.add(WorkerOffer.newOffer(1,  numberOfTools, "tools", id, wealth));

            case "cleaner":
                    Dictionary<Integer, Integer> workerClothes = reserve.getClothes();
                    int sumOfClothes = 0;
                    Enumeration e = workerClothes.keys();
                    while(e.hasMoreElements()){
                        int nextKey = (int) e.nextElement();
                        if(nextKey > 1){
                            sumOfClothes += workerClothes.get(nextKey);
                        }

                    }
                    if(sumOfClothes < 100){
                        buyOffers.add(WorkerOffer.newOffer(1, 100 - sumOfClothes, "clothes", id, wealth));
                    }

                break;
        }
    }
}
