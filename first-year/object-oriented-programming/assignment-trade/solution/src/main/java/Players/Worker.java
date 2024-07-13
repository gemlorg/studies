package Players;

import Players.Strategies.*;
import Players.Strategies.Offers.WorkerOffer;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;

import static Stocks.GameParameters.gameInfo;

public class Worker extends AbstractPlayer implements IPlayer {
    int punishmentForNoClothes = 10;
    Career career;
    LearningStrategy learningStrategy;
    BuyingStrategy buyStrategy;
    ProductionStrategy productionStrategy;
    Productivity productivity;
    boolean isStudying = false;
    ArrayList<WorkerOffer> sellOffers = new ArrayList<>();
    ArrayList<WorkerOffer> buyOffers = new ArrayList<>();
    private int starve = 0;
    private boolean dead = false;

    JsonNode rawInfo;
    private Worker(JsonNode workerInfo){
        super(workerInfo.get("id").asInt(), workerInfo.get("resources"));
        rawInfo = workerInfo;
        career = new Career(workerInfo.get("career"), workerInfo.get("level"), workerInfo.get("careerStrategy")); // holds a list of skills, decides how to as well

        buyStrategy = new BuyingStrategy(workerInfo.get("buyingStrategy")); //what to buy at the end of the day
        productivity = new Productivity(workerInfo.get("productivity")); // how effective worker is at certain production
        productionStrategy = new ProductionStrategy(workerInfo.get("productionStrategy"), workerInfo.get("historyPerspective")); // decides what to make this day and makes it
        learningStrategy = new LearningStrategy(workerInfo.get("learning")); //student, hardworking etc. Only makes descision if study this day

        try{
            punishmentForNoClothes = gameInfo.get("punishmentForNoClothes").asInt();
        }catch (NullPointerException e){

        }




    }

    public static Worker getWorker(JsonNode info){
        return new Worker(info);
    }

    public void makeMove(int currentDay){
        if(dead){
            return;
        }
        boolean whatToDo = learningStrategy.makeDecision(currentDay, reserve); // study if true. work/bet otherwise
        if(whatToDo){
            career.study(currentDay);
            isStudying = true;
        }else{

            String productionToMake = productionStrategy.decideWhatToMake(currentDay, career, productivity, reserve);
            productivity.makeProduction(productionToMake, reserve, career, sellOffers, id);
            buyStrategy.buy(currentDay, id, reserve, buyOffers, sellOffers);
        }
    }
    public ArrayList<WorkerOffer> getSellOffers(){
        return sellOffers;
    }

    public ArrayList<WorkerOffer> getBuyOffers(){return buyOffers;}

    public void die(){
        reserve.emptyDiamonds();
        dead = true;
    }

    public void endDay(int currentDay){
        if(dead){
            return;
        }
        sellOffers = new ArrayList<>();
        buyOffers = new ArrayList<>();

        if(isStudying){
            starve = 0;
            return;
        }
        if(reserve.getFood() < 100){
            reserve.emptyFood();
            starve ++;
            if(starve == 1){
                productivity.addEffect(-100);
            }else if(starve == 2){
                productivity.addEffect(-300);
            }else{
                die();
            }
        }else{
            reserve.changeFood(-100);
        }

        reserve.useClothes();

        if(reserve.getClothesSum() < 100){
            productivity.addEffect(-punishmentForNoClothes);
        }
    }
    public record ReserveRecord(int diamonds, int food, Dictionary<Integer, Integer> clothes, Dictionary<Integer, Integer> tools, Dictionary<Integer, Integer> programs){}

    public record workerJson(int id, int level, String career, JsonNode buyingStrategy, JsonNode productionStrategy, JsonNode learning, JsonNode careerStrategy, JsonNode productivity, ReserveRecord reserve){}

    public JsonNode toJson() throws IOException {
        ReserveRecord tempReserve = new ReserveRecord(reserve.getDiamonds(), reserve.getFood(), reserve.getClothes(), reserve.getTools(), reserve.getPrograms());
        workerJson json = new workerJson(id, career.getLevel(career.getMainCareer()), career.getMainCareer(),
                rawInfo.get("buyingStrategy"), rawInfo.get("productionStrategy"), rawInfo.get("learning"), rawInfo.get("careerStrategy"), rawInfo.get("productivity"), tempReserve);

        ObjectMapper mapper = new ObjectMapper();
        mapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        String jsonString = mapper.writeValueAsString(json);
        return  new ObjectMapper().readTree(jsonString);

    }


}
