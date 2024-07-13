package Players.Strategies;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;

public class ProductionReserve {
    private int diamonds;
    private int food;
    private Dictionary<Integer, Integer> clothes = new Hashtable<>();
    private Dictionary<Integer, Integer> tools = new Hashtable<>();
    private Dictionary<Integer, Integer> programs = new Hashtable<>();

    public ProductionReserve(JsonNode resources){
        diamonds = resources.get("diamonds").asInt();
        food = resources.get("food").asInt();
        clothes.put(1, resources.get("clothes").asInt());
        tools.put(1, resources.get("tools").asInt());
        programs.put(1, resources.get("programs").asInt());
    }

    public int getDiamonds() {
        return diamonds;
    }

    public int getFood() {
        return food;
    }

    public Dictionary<Integer, Integer> getClothes() {
        return clothes;
    }

    public Dictionary<Integer, Integer> getTools() {
        return tools;
    }

    public Dictionary<Integer, Integer> getPrograms() {
        return programs;
    }



    public void changeDiamonds(int diamonds) {
        this.diamonds += diamonds;
    }

    public void changeFood(int food) {
        this.food += food;
    }

    public void setClothes(int level, int clothes) {
        this.clothes.put(level*level, this.clothes.get(level) + clothes); // check
    }

    public void setTools(int level, int tools) {
        this.tools.put(level, this.tools.get(level) + tools); // check
    }

    public void setPrograms(int level, int programs) {
        this.programs.put(level, this.programs.get(level) + programs); // check
    }
    public void emptyTools(){
        tools = new Hashtable<>();
    }
    public void emptyPrograms(){
        programs = new Hashtable<>();
    }
    public void emptyFood(){
        food = 0;
    }

    public void emptyClothes(){
        clothes = new Hashtable<>();
    }

    public void emptyRenewables(){
        food = 0;
        clothes = new Hashtable<>();
        tools = new Hashtable<>();
        programs = new Hashtable<>();
    }

    public void change(int amount, int quality, String type){
        int originalAmount = 0;
        switch(type){
            case "food":
                food += amount;
                break;
            case "diamonds":
                diamonds += amount;
                break;
            case "clothes":
                try{
                    originalAmount = clothes.get(quality);
                }catch (NullPointerException e){

                }
                clothes.put(quality * quality, originalAmount + amount);
                break;
            case("tools"):
                try{
                    originalAmount = tools.get(quality);
                }catch (NullPointerException e){

                }
//                System.out.println("puts " + amount + " tools");
                tools.put(quality, originalAmount + amount);
                break;
            case("programs"):
                try{
                    originalAmount = programs.get(quality);
                }catch (NullPointerException e){

                }
                programs.put(quality, originalAmount + amount);
                break;

        }
    }
    public void emptyDiamonds(){
        diamonds = 0;
    }

    public void useClothes(){
        Dictionary<Integer, Integer> newClothes = new Hashtable<>();
        Enumeration<Integer> e = clothes.keys();
        while(e.hasMoreElements()){
            int nextKey = e.nextElement();
            if(nextKey > 1){
                newClothes.put(nextKey - 1, clothes.get(nextKey));
            }
        }
        clothes = newClothes;
    }
    public int getClothesSum(){
        int sum = 0;
        Enumeration e = clothes.elements();
        while(e.hasMoreElements()){
            sum += (int) e.nextElement();
        }
        return sum;
    }
}
