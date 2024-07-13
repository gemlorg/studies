package Players.Strategies;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Hashtable;

public class Career {
    private final Dictionary<String, Integer> careers = new Hashtable<String, Integer>();
    private final String mainCareer;
    private final String careerStrategy;
    public Career(JsonNode career, JsonNode level, JsonNode careerStrategy){
        careers.put("programmer", 1);
        careers.put("farmer", 1);
        careers.put("miners", 1);
        careers.put("craftsman", 1);
        careers.put("engineer", 1);
        mainCareer = career.asText();
        careers.put(mainCareer, level.asInt());
        this.careerStrategy = careerStrategy.asText();
    }

    public int getLevel(String career){return career == mainCareer ? careers.get(career) : 0;}

    public String getMainCareer(){return mainCareer;}

    public void study(int currentDay){
        if(careerStrategy == "revolutionary" && (currentDay % 7 == 0)){
            //changes his path but if it's the same then plus 1
        }else{
            careers.put(mainCareer, careers.get(mainCareer) + 1);
        }
    }
}
