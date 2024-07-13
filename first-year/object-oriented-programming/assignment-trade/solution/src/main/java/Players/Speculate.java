package Players;

import Players.Strategies.Offers.SpeculateOffer;
import Players.Strategies.SpeculateCareer;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.util.ArrayList;

public class Speculate extends AbstractPlayer {
    SpeculateCareer career;
    ArrayList<SpeculateOffer> sellOffers = new ArrayList<>();
    ArrayList<SpeculateOffer> buyOffers = new ArrayList<>();

    private Speculate(JsonNode speculateInfo){
        super(speculateInfo.get("id").asInt(), speculateInfo.get("resources"));
        career = new SpeculateCareer(speculateInfo.get("career"), speculateInfo.get("historyTrack"));
    }

    public static Speculate getSpeculate(JsonNode speculateInfo){
        return new Speculate(speculateInfo);
    }

    public void makeMove(int currentDay){
        career.addOffers(currentDay, reserve, id, sellOffers, buyOffers);
    }
    public ArrayList<SpeculateOffer> getSellOffers(){
        return sellOffers;
    }
    public ArrayList<SpeculateOffer> getBuyOffers(){
        return buyOffers;
    }

    public record SpeculateJson(int id, String career, Worker.ReserveRecord reserve){}
    public JsonNode toJson() throws IOException {
        Worker.ReserveRecord tempReserve = new Worker.ReserveRecord(reserve.getDiamonds(), reserve.getFood(), reserve.getClothes(), reserve.getTools(), reserve.getPrograms());
        SpeculateJson json = new SpeculateJson(id, career.career(), tempReserve);
        ObjectMapper mapper = new ObjectMapper();
        mapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        String jsonInString = mapper.writeValueAsString(json);
        ObjectNode jsonObject = (ObjectNode) new ObjectMapper().readTree(jsonInString);
        return jsonObject;
    }
    public void endDay(){
        sellOffers = new ArrayList<>();
        buyOffers = new ArrayList<>();
    }
}
