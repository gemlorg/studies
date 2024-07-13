package Players;

import Players.Strategies.ProductionReserve;
import com.fasterxml.jackson.databind.JsonNode;

public abstract class AbstractPlayer implements IPlayer{
    int id;
    ProductionReserve reserve;

    protected AbstractPlayer(int id, JsonNode reserve){
        this.id = id;
        this.reserve = new ProductionReserve(reserve); // list of all items worker has
    }

    public static void transferDiamonds(AbstractPlayer p1, AbstractPlayer p2, int amount){
        p1.reserve.changeDiamonds(-amount);
        p2.reserve.changeDiamonds(amount);
    }

    public static void addProduction(AbstractPlayer p, int amount, int quality,  String type){
        p.reserve.change(amount, quality, type);
    }
    public void addProduction(int amount, int quality, String type){
        this.reserve.change(amount, quality, type);
    }

    public int getId(){
        return id;
    }
    public boolean compareId(int otherId){
        return id == otherId;
    }

    @Override
    public String toString(){
        String finalString = "{ id: " + id + ", diamonds: " + reserve.getDiamonds() + ", food: " + reserve.getFood() + ", clothes: " + reserve.getClothes()
         + ", tools: " + reserve.getTools() + ", programs: " + reserve.getPrograms() + "}";
        return finalString;

    }
}
