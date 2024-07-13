package Stocks;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;

public class GameData {
    static protected Dictionary<Integer, JsonNode> dataPastDays = new Hashtable<>();
    static protected Dictionary<Integer, Dictionary<String, Integer>> stockVolumeList = new Hashtable<>();

    private GameData(){}


    public static void addDataForDay(int dayNumber, JsonNode data){
        dataPastDays.put(dayNumber, data);
    }

    public static JsonNode dataFromDay(int day){
        assert day <= dataPastDays.size() && day >= 0;

        return dataPastDays.get(day);
    }
    public  static void addStockVolume(int day, Dictionary<String, Integer> data){
        stockVolumeList.put(day, data);
    }
    public static void addStockVolume(int day, String production, int quantity){
        Dictionary<String, Integer> original = new Hashtable<>();
        try {
            original = stockVolumeList.get(day);
        }catch(Exception NullPointerException){
            original = new Hashtable<>();
        }
        if(original == null){
            original = new Hashtable<>();
        }

        original.put(production, quantity + (original.get(production) == null ? 0 : original.get(production)));
        stockVolumeList.put(day, original);
    }
    public static int getStockVolume(int day, String production) {
        try {
            return stockVolumeList.get(day).get(production);
        }catch (NullPointerException e){
            return 0;
        }
    }



}
