package Players.Strategies;

import Players.Strategies.Offers.SpeculateOffer;
import Stocks.GameData;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;
import java.util.Enumeration;

public class SpeculateCareer {
    private String career;
    private int historyTrack;
    public SpeculateCareer(JsonNode careerInfo, JsonNode history){
        career = careerInfo.asText();
        try{
            career = careerInfo.get("type").asText();
        }catch(NullPointerException ignored){}
        try{
            historyTrack = history.asInt();
        }catch (Exception NullPointerException){
            try{
                careerInfo.get("historyTrack").asInt();
            }catch(NullPointerException ignored) {
            }
        }
    }
    public void addOffers(int currentDay, ProductionReserve reserve, int id, ArrayList<SpeculateOffer> sellOffers, ArrayList<SpeculateOffer> buyOffers){
        double foodPrice = 0;
        double clothesPrice = 0;
        double toolsPrice = 0;
        double programsPrice = 0;
        switch (career) {
            case "average":
                if(historyTrack > currentDay){
                    historyTrack = currentDay;
                }
                if(historyTrack == 0){
                    historyTrack = 1;
                }
                foodPrice = 0;
                clothesPrice = 0;
                toolsPrice = 0;
                programsPrice = 0;
                for(int d = currentDay - 1; d >= currentDay - historyTrack; d--){
                    JsonNode prices = GameData.dataFromDay(d).get("pricesAverage");
                    foodPrice += prices.get("food").asDouble() / (double) historyTrack;
                    clothesPrice += prices.get("clothes").asDouble() / (double) historyTrack;
                    toolsPrice += prices.get("tools").asDouble() / (double) historyTrack;
                    programsPrice += prices.get("programs").asDouble() / (double) historyTrack;
                }
                buyOffers.add(SpeculateOffer.newOffer(1, 100, "food", id, reserve.getDiamonds(), reserve.getFood() == 0 ? foodPrice * 0.95 : foodPrice * 0.9));
                sellOffers.add(SpeculateOffer.newOffer(1, reserve.getFood(), "food", id, reserve.getDiamonds(), foodPrice * 1.1));

                buyOffers.add(SpeculateOffer.newOffer(1, 100, "clothes", id, reserve.getDiamonds(), reserve.getClothes().isEmpty() ? clothesPrice * 0.95 : clothesPrice * 0.9));
                if(!reserve.getClothes().isEmpty()){
                    Enumeration<Integer> e = reserve.getClothes().keys();
                    while(e.hasMoreElements()){
                        int nextKey = e.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getClothes().get(nextKey), "clothes", id, reserve.getDiamonds(), clothesPrice * 1.1));
                    }
                }
                buyOffers.add(SpeculateOffer.newOffer(1, 100, "tools", id, reserve.getDiamonds(), reserve.getTools().isEmpty() ? toolsPrice * 0.95 : toolsPrice * 0.9));
                if(!reserve.getTools().isEmpty()){
                    Enumeration<Integer> ee = reserve.getTools().keys();
                    while(ee.hasMoreElements()){
                        int nextKey = ee.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getTools().get(nextKey), "tools", id, reserve.getDiamonds(), toolsPrice * 1.1));
                    }
                }

                buyOffers.add(SpeculateOffer.newOffer(1, 100, "programs", id, reserve.getDiamonds(), reserve.getPrograms().isEmpty() ? programsPrice * 0.95 : programsPrice * 0.9));
                if(!reserve.getPrograms().isEmpty()){
                    Enumeration<Integer> eee = reserve.getPrograms().keys();
                    while(eee.hasMoreElements()){
                        int nextKey = eee.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getPrograms().get(nextKey), "programs", id, reserve.getDiamonds(), programsPrice * 1.1));
                    }
                }

                reserve.emptyRenewables();

            case "convex":

                if(currentDay >= 3){
                    JsonNode one = GameData.dataFromDay(currentDay - 1).get("pricesAverage");
                    JsonNode two = GameData.dataFromDay(currentDay - 2).get("pricesAverage");
                    JsonNode three = GameData.dataFromDay(currentDay - 3).get("pricesAverage");
                    foodPrice = 0;
                    clothesPrice = 0;
                    toolsPrice = 0;
                    programsPrice = 0;
                    for(int d = currentDay - 1; d >= currentDay - 3; d--){
                        JsonNode prices = GameData.dataFromDay(d).get("pricesAverage");
                        foodPrice += prices.get("food").asDouble() / (double) historyTrack;
                        clothesPrice += prices.get("clothes").asDouble() / (double) historyTrack;
                        toolsPrice += prices.get("tools").asDouble() / (double) historyTrack;
                        programsPrice += prices.get("programs").asDouble() / (double) historyTrack;
                    }

                    if(one.get("food").asInt() < two.get("food").asInt() && three.get("food").asInt() < two.get("food").asInt()) {
                        buyOffers.add(SpeculateOffer.newOffer(1, 100, "food", id, reserve.getDiamonds(), foodPrice * 0.9));
                    }
                    if(one.get("food").asInt() > two.get("food").asInt() && three.get("food").asInt() > two.get("food").asInt()) {
                        sellOffers.add(SpeculateOffer.newOffer(1, reserve.getFood(), "food", id, reserve.getDiamonds(), foodPrice * 1.1));
                        reserve.emptyFood();
                    }
                    if(one.get("clothes").asInt() < two.get("clothes").asInt() && three.get("clothes").asInt() < two.get("clothes").asInt()) {
                        buyOffers.add(SpeculateOffer.newOffer(1, 100, "clothes", id, reserve.getDiamonds(), clothesPrice * 0.9));
                    }
                    if(one.get("clothes").asInt() > two.get("clothes").asInt() && three.get("clothes").asInt() > two.get("clothes").asInt()) {
                        if (!reserve.getClothes().isEmpty()) {
                            Enumeration<Integer> e = reserve.getClothes().keys();
                            while (e.hasMoreElements()) {
                                int nextKey = e.nextElement();
                                sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getClothes().get(nextKey), "clothes", id, reserve.getDiamonds(), clothesPrice * 1.1));
                            }
                        }
                        reserve.emptyClothes();
                    }

                    if(one.get("tools").asInt() < two.get("tools").asInt() && three.get("tools").asInt() < two.get("tools").asInt()) {
                        buyOffers.add(SpeculateOffer.newOffer(1, 100, "tools", id, reserve.getDiamonds(), toolsPrice * 0.9));
                    }
                    if(one.get("tools").asInt() > two.get("tools").asInt() && three.get("tools").asInt() > two.get("tools").asInt()) {
                        if (!reserve.getTools().isEmpty()) {
                            Enumeration<Integer> ee = reserve.getTools().keys();
                            while (ee.hasMoreElements()) {
                                int nextKey = ee.nextElement();
                                sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getTools().get(nextKey), "tools", id, reserve.getDiamonds(), toolsPrice * 1.1));
                            }
                        }
                        reserve.emptyTools();
                    }

                    if(one.get("programs").asInt() < two.get("programs").asInt() && three.get("programs").asInt() < two.get("programs").asInt()) {
                        buyOffers.add(SpeculateOffer.newOffer(1, 100, "programs", id, reserve.getDiamonds(), programsPrice * 0.9));
                    }
                    if(one.get("programs").asInt() > two.get("programs").asInt() && three.get("programs").asInt() > two.get("programs").asInt()) {
                        if (!reserve.getPrograms().isEmpty()) {
                            Enumeration<Integer> eee = reserve.getPrograms().keys();
                            while (eee.hasMoreElements()) {
                                int nextKey = eee.nextElement();
                                sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getPrograms().get(nextKey), "programs", id, reserve.getDiamonds(), programsPrice * 1.1));
                            }
                        }
                        reserve.emptyPrograms();
                    }
                }
                break;
            case "marketGuard":
                if(currentDay == 1){
                    break;
                }
                JsonNode data = GameData.dataFromDay(currentDay - 1).get("pricesAverage");
                foodPrice = data.get("food").asDouble() * GameData.getStockVolume(currentDay, "food") / Math.max(1, GameData.getStockVolume(currentDay - 1, "food"));
                clothesPrice = data.get("clothes").asDouble() * GameData.getStockVolume(currentDay, "clothes") / Math.max(1, GameData.getStockVolume(currentDay - 1, "clothes"));
                toolsPrice = data.get("tools").asDouble() * GameData.getStockVolume(currentDay, "tools") / Math.max(1, GameData.getStockVolume(currentDay - 1, "tools"));
                programsPrice = data.get("programs").asDouble() * GameData.getStockVolume(currentDay, "programs") / Math.max(1, GameData.getStockVolume(currentDay - 1, "programs"));

                buyOffers.add(SpeculateOffer.newOffer(1, 100, "food", id, reserve.getDiamonds(), foodPrice * 0.9));
                sellOffers.add(SpeculateOffer.newOffer(1, reserve.getFood(), "food", id, reserve.getDiamonds(), foodPrice * 1.1));

                buyOffers.add(SpeculateOffer.newOffer(1, 100, "clothes", id, reserve.getDiamonds(), clothesPrice * 0.9));
                if(!reserve.getClothes().isEmpty()){
                    Enumeration<Integer> e = reserve.getClothes().keys();
                    while(e.hasMoreElements()){
                        int nextKey = e.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getClothes().get(nextKey), "clothes", id, reserve.getDiamonds(), clothesPrice * 1.1));
                    }
                }
                buyOffers.add(SpeculateOffer.newOffer(1, 100, "tools", id, reserve.getDiamonds(),toolsPrice * 0.9));
                if(!reserve.getTools().isEmpty()){
                    Enumeration<Integer> ee = reserve.getTools().keys();
                    while(ee.hasMoreElements()){
                        int nextKey = ee.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getTools().get(nextKey), "tools", id, reserve.getDiamonds(), toolsPrice * 1.1));
                    }
                }

                buyOffers.add(SpeculateOffer.newOffer(1, 100, "programs", id, reserve.getDiamonds(), programsPrice * 0.9));
                if(!reserve.getPrograms().isEmpty()){
                    Enumeration<Integer> eee = reserve.getPrograms().keys();
                    while(eee.hasMoreElements()){
                        int nextKey = eee.nextElement();
                        sellOffers.add(SpeculateOffer.newOffer(nextKey, reserve.getPrograms().get(nextKey), "programs", id, reserve.getDiamonds(), programsPrice * 1.1));
                    }
                }

                reserve.emptyRenewables();











        }
    }

    public String career() {
        return career;
    }
}
