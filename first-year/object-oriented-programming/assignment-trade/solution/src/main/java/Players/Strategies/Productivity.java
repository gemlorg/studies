package Players.Strategies;

import Players.Strategies.Offers.WorkerOffer;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;
import java.util.Enumeration;

public class Productivity {
    private final double diamondRate;
    private final double foodRate;
    private final double clothesRate;
    private final double toolsRate;
    private final double programsRate;

    ArrayList<Integer> effects = new ArrayList<>();
    public Productivity(JsonNode productivity){
        diamondRate = productivity.get("diamonds").asDouble();
        foodRate = productivity.get("food").asDouble();
        clothesRate = productivity.get("clothes").asDouble();
        toolsRate = productivity.get("tools").asDouble();
        programsRate = productivity.get("programs").asDouble();
    }
    public int diamonds(Career career, ProductionReserve reserve){
        double efficiencyRate = 100;
        for (int effect : effects){
            efficiencyRate += effect;
        }
        Enumeration<Integer> e = reserve.getTools().keys();
        while(e.hasMoreElements()) {
            int quality = e.nextElement();
            efficiencyRate += quality * reserve.getTools().get(quality);
        }
        if(career.getMainCareer() == "miner"){
            int level = career.getLevel(career.getMainCareer());
            efficiencyRate += level == 0 ? 0 : level == 1 ? 50 : level == 2 ? 150 :  300 + 25 * (level - 3);
        }
        return (int) (diamondRate * efficiencyRate / 100);
    }
    public int food(Career career, ProductionReserve reserve){
        double efficiencyRate = 100;
        for (int effect : effects){
            efficiencyRate += effect;
        }
        Enumeration<Integer> e = reserve.getTools().keys();
        while(e.hasMoreElements()) {
            int quality = e.nextElement();
            efficiencyRate += quality * reserve.getTools().get(quality);
        }
        if(career.getMainCareer() == "farmer"){
            int level = career.getLevel(career.getMainCareer());
            efficiencyRate += level == 0 ? 0 : level == 1 ? 50 : level == 2 ? 150 :  300 + 25 * (level - 3);
        }
        return (int) (foodRate * efficiencyRate / 100);
    }
    public int clothes(Career career, ProductionReserve reserve){
        double efficiencyRate = 100;
        for (int effect : effects){
            efficiencyRate += effect;
        }
        Enumeration<Integer> e = reserve.getTools().keys();
        while(e.hasMoreElements()) {
            int quality = e.nextElement();
            efficiencyRate += quality * reserve.getTools().get(quality);
        }
        if(career.getMainCareer() == "craftsman"){
            int level = career.getLevel(career.getMainCareer());
            efficiencyRate += level == 0 ? 0 : level == 1 ? 50 : level == 2 ? 150 :  300 + 25 * (level - 3);
        }
        int programmerBonus = 0;

        Enumeration<Integer> e1 = reserve.getPrograms().elements();
        while(e1.hasMoreElements()){
            int nextElement = e1.nextElement();
            programmerBonus += nextElement;
        }

        return (int) (clothesRate * efficiencyRate / 100) + programmerBonus;
    }
    public int tools(Career career, ProductionReserve reserve){
        double efficiencyRate = 100;
        for (int effect : effects){
            efficiencyRate += effect;
        }
        Enumeration<Integer> e2 = reserve.getTools().keys();
        while(e2.hasMoreElements()) {
            int quality = e2.nextElement();
            efficiencyRate += quality * reserve.getTools().get(quality);
        }
        if(career.getMainCareer() == "engineer"){
            int level = career.getLevel(career.getMainCareer());
            efficiencyRate += level == 0 ? 0 : level == 1 ? 50 : level == 2 ? 150 :  300 + 25 * (level - 3);
        }
        int programmerBonus = 0;

        Enumeration<Integer> e1 = reserve.getPrograms().elements();
        while(e1.hasMoreElements()){
            programmerBonus += e1.nextElement();
        }
        return (int) (toolsRate * efficiencyRate / 100) + programmerBonus;
    }
    public int programs(Career career, ProductionReserve reserve){
        double efficiencyRate = 100;
        for (int effect : effects){
            efficiencyRate += effect;
        }
        Enumeration<Integer> e = reserve.getTools().keys();
        while(e.hasMoreElements()) {
            int quality = e.nextElement();
            efficiencyRate += quality * reserve.getTools().get(quality);
        }
        if(career.getMainCareer() == "programmer"){
            int level = career.getLevel(career.getMainCareer());
            efficiencyRate += level == 0 ? 0 : level == 1 ? 50 : level == 2 ? 150 :  300 + 25 * (level - 3);
        }
        int programmerBonus = 0;

        return (int) (programsRate * efficiencyRate / 100) + programmerBonus;
    }
    public void makeProduction(String productionToMake, ProductionReserve reserve, Career career, ArrayList<WorkerOffer> sellOffers, int id){
        int productionMade;
        int sumOfPrograms;
        int levelOfProduction;
        switch  (productionToMake) {
            case "diamonds":
                productionMade = diamonds(career, reserve);
                reserve.changeDiamonds(productionMade);
                break;
            case "food":
                productionMade = food(career, reserve);
                sellOffers.add(WorkerOffer.newOffer(1, productionMade, "food", id, reserve.getDiamonds()));
                break;
            case "clothes":
                productionMade = clothes(career, reserve);
                sumOfPrograms = 0;
                Enumeration<Integer> eee = reserve.getPrograms().elements();
                while(eee.hasMoreElements()){
                    int quantity = eee.nextElement();
                    sumOfPrograms += quantity;
                }
                productionMade -= sumOfPrograms;
                levelOfProduction = career.getMainCareer().equals("craftsman") ? career.getLevel(career.getMainCareer()) : 1;
                sellOffers.add(WorkerOffer.newOffer(levelOfProduction, productionMade, "clothes", id, reserve.getDiamonds()));

                if(career.getMainCareer() == "programmer"){
                    Enumeration<Integer> e2 = reserve.getPrograms().keys();
                    while(e2.hasMoreElements()){
                        int quality = e2.nextElement();
                        sellOffers.add(WorkerOffer.newOffer(quality, reserve.getPrograms().get(quality), "clothes", id, reserve.getDiamonds()));
                    }
                }else{
                    sellOffers.add(WorkerOffer.newOffer(1, sumOfPrograms, "clothes", id, reserve.getDiamonds()));
                }
                reserve.emptyPrograms();
                break;
            case "tools":
                productionMade = tools(career, reserve);
                sumOfPrograms = 0;
                Enumeration<Integer> eeee2 = reserve.getPrograms().elements();
                while(eeee2.hasMoreElements()){
                    int quantity = eeee2.nextElement();
                    sumOfPrograms += quantity;
                }
                productionMade -= sumOfPrograms;
                levelOfProduction = career.getMainCareer().equals("engineer") ? career.getLevel(career.getMainCareer()) : 1;
                sellOffers.add(WorkerOffer.newOffer(levelOfProduction, productionMade, "tools", id, reserve.getDiamonds()));

                if(career.getMainCareer().equals("programmer")){
                    Enumeration<Integer> e22 = reserve.getPrograms().keys();
                    while(e22.hasMoreElements()){
                        int quality = e22.nextElement();
                        sellOffers.add(WorkerOffer.newOffer(quality, reserve.getPrograms().get(quality), "tools", id, reserve.getDiamonds()));
                    }
                }else{
                    sellOffers.add(WorkerOffer.newOffer(1, sumOfPrograms, "tools", id, reserve.getDiamonds()));
                }
                reserve.emptyPrograms();
                break;
            case "programs":
                productionMade = programs(career, reserve);
                levelOfProduction = career.getMainCareer().equals("programmer") ? career.getLevel(career.getMainCareer()) : 1;
                sellOffers.add(WorkerOffer.newOffer(levelOfProduction, productionMade, "programs", id, reserve.getDiamonds()));
                break;
        }
        reserve.emptyTools();
        effects = new ArrayList<>();
    }

    public void addEffect(int percent){
        effects.add(percent);
    }


}
