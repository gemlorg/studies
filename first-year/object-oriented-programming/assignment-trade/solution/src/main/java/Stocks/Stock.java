package Stocks;

import JsonHandler.Handler;
import Players.AbstractPlayer;
import Players.Speculate;
import Players.Strategies.Offers.SpeculateOffer;
import Players.Worker;
import Players.Strategies.Offers.WorkerOffer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Objects;

import static Stocks.GameParameters.*;

public class Stock implements IStock{

    private final int gameLength;
    private final String type;
    private int currentDay = 1;
    private final JsonNode prices;
    private final ArrayList<Worker> workers = new ArrayList<>();
    private final ArrayList<Speculate> speculates = new ArrayList<>();

    JsonNode todayData;
    public static String in;
    public static String out;

    protected Stock(String in, String out){
        Stock.in = in;
        Stock.out = out;
        type = gameInfo.get("stockExchange").asText();
        gameLength = gameInfo.get("gameLength").asInt();
        prices = gameInfo.get("prices");
        for(JsonNode worker: workersInfo){
            Worker player = Worker.getWorker(worker);
            workers.add(player);
        }

        for(JsonNode speculate: speculatesInfo){
            Speculate player = Speculate.getSpeculate(speculate);
            speculates.add(player);
        }

    }






    public static Stock setStock(String in, String out) throws IOException {
        FileWriter file = new FileWriter(out);
        file.write("");
        file.close();
        Handler.main(in);
        return new Stock(in, out);
    }



    public void play() throws IOException {
        addZeroDayData();
        while(currentDay <= gameLength){
            makeMove();
            printInfo();

            currentDay += 1;

        }

    }

    public void makeMove() throws IOException {
        DataContainer current = new DataContainer(currentDay);
        ArrayList<WorkerOffer> allWorkersSellOffers = new ArrayList<>(); // list of all sell offers of the workers
        ArrayList<WorkerOffer> allWorkersBuyOffers = new ArrayList<>(); // list of all buy offers of the workers
        ArrayList<SpeculateOffer> allSpeculatesSellOffers = new ArrayList<>(); // list of all buy offers of the workers
        ArrayList<SpeculateOffer> allSpeculatesBuyOffers = new ArrayList<>(); // list of all buy offers of the workers

        for(Worker worker : workers){
            worker.makeMove(currentDay);
            ArrayList<WorkerOffer> workerSellOffers = worker.getSellOffers();
            ArrayList<WorkerOffer> workerBuyOffers = worker.getBuyOffers();
            for(WorkerOffer offer : workerSellOffers){
                allWorkersSellOffers.add(offer);
                //add data about stock volume
                GameData.addStockVolume(currentDay, offer.getTypeOfProduction(), offer.getQuantity());
            }
            allWorkersBuyOffers.addAll(workerBuyOffers);

        }
        for(Speculate speculate : speculates){
            speculate.makeMove(currentDay);
            ArrayList<SpeculateOffer> speculateSellOffers = speculate.getSellOffers();
            ArrayList<SpeculateOffer> speculateBuyOffers = speculate.getBuyOffers();
            allSpeculatesSellOffers.addAll(speculateSellOffers);
            allSpeculatesBuyOffers.addAll(speculateBuyOffers);
        }
        compareOffers(allWorkersSellOffers, allWorkersBuyOffers, allSpeculatesSellOffers, allSpeculatesBuyOffers, current);

        for(Worker worker : workers){
            worker.endDay(currentDay);
        }
        for(Speculate speculate : speculates){
            speculate.endDay();
        }
        current.addWorkers(workers);
        current.addSpeculates(speculates);

        todayData = current.toJson();

        GameData.addDataForDay(currentDay, todayData);

    }

    public void printInfo() throws IOException {

        FileWriter file = new FileWriter(out, true);
        ObjectMapper mapper = new ObjectMapper();
        String pretty = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(todayData);
        if(currentDay == 1){
            file.write("[");
        }
        file.write(pretty);
        if(currentDay == gameLength){
            file.write("]");
        }else {
            file.write(",");
            file.write("\n");
        }
        file.close();

    }

    public void addZeroDayData() throws IOException {


        ArrayList<JsonNode> workersList = new ArrayList<>();
        ArrayList<JsonNode> speculatesList = new ArrayList<>();
        for(JsonNode workerNode : workersInfo){
            workersList.add(workerNode);
        }
        for(JsonNode speculateNode : speculatesInfo){
            speculatesList.add(speculateNode);
        }
        ObjectMapper mapper = new ObjectMapper();
        DataContainerJson newData = new DataContainerJson(0, prices, prices, prices, workersList, speculatesList);
        String zeroDayString = mapper.writeValueAsString(newData);
        ObjectNode zeroDayNode = (ObjectNode) new ObjectMapper().readTree(zeroDayString);
        GameData.addDataForDay(0, zeroDayNode);
    }

    public Worker findWorker(int workerId){
        for(Worker worker : workers){
            if(worker.compareId(workerId)){
                return worker;
            }
        }
        System.out.println("Find worker went wrong");
        return null;
    }
    public Speculate findSpeculate(int speculateId){
        for(Speculate speculate : speculates){
            if(speculate.compareId(speculateId)){
                return speculate;
            }
        }
        System.out.println("Find speculate went wrong");
        return null;
    }

    @Override
    public void compareOffers(ArrayList<WorkerOffer> workerSell, ArrayList<WorkerOffer> workerBuy, ArrayList<SpeculateOffer> speculateSell, ArrayList<SpeculateOffer> speculateBuy, DataContainer current) {
        String tempType = type == "balanced" ? new String[] {"socialist", "capitalist"}[currentDay % 2] : type;
        if(tempType.equals("capitalist")) {
            speculateBuy.sort((o1, o2) -> {
                if (o1.getWealth() != o2.getWealth()) {
                    return o1.getWealth() > o2.getWealth() ? -1 : 1;
                } else {
                    return o1.getId() < o2.getId() ? -1 : 1;
                }
            });

            workerBuy.sort((o1, o2) -> {
                if (o1.getWealth() != o2.getWealth()) {
                    return o1.getWealth() > o2.getWealth() ? -1 : 1;
                } else {
                    return o1.getId() < o2.getId() ? -1 : 1;
                }
            });
        }
        if(tempType.equals("socialist")){
            speculateBuy.sort((o1, o2) -> {
                if (o1.getWealth() != o2.getWealth()) {
                    return o1.getWealth() < o2.getWealth() ? -1 : 1;
                } else {
                    return o1.getId() < o2.getId() ? -1 : 1;
                }
            });

            workerBuy.sort((o1, o2) -> {
                if (o1.getWealth() != o2.getWealth()) {
                    return o1.getWealth() < o2.getWealth() ? -1 : 1;
                } else {
                    return o1.getId() < o2.getId() ? -1 : 1;
                }
            });

        }


        workerSell.sort((o1, o2) -> {
            if(o1.getQuality() != o2.getQuality() ){
                return o1.getQuality() > o2.getQuality() ? -1 : 1;
            }else{
                return o1.getQuantity() < o2.getQuantity() ? -1 : 1;
            }
        });

        speculateSell.sort((o1, o2) -> {
            if(o1.getQuality() != o2.getQuality() ){
                return o1.getQuality() > o2.getQuality() ? -1 : 1;
            }else{
                return o1.getQuantity() < o2.getQuantity() ? -1 : 1;
            }
        });


        for(WorkerOffer buyOffer : workerBuy){
            Worker buyOfferOwner = findWorker(buyOffer.getId());
            for(SpeculateOffer sellOffer : speculateSell){
                if(!Objects.equals(buyOffer.getTypeOfProduction(), sellOffer.getTypeOfProduction())){
                    continue;
                }
                Speculate sellOfferOwner = findSpeculate(sellOffer.getId());
                int dealVolume = Math.min(buyOffer.getQuantity(), sellOffer.getQuantity());
                if(dealVolume <= 0){
                    continue;
                }
                AbstractPlayer.transferDiamonds(buyOfferOwner, sellOfferOwner, (int)(dealVolume * sellOffer.getPrice()));
                buyOfferOwner.addProduction(dealVolume, sellOffer.getQuality(), sellOffer.getTypeOfProduction());
                buyOffer.changeQuantity(-dealVolume);
                sellOffer.changeQuantity(-dealVolume);
                current.passOffer(sellOffer.getTypeOfProduction(), dealVolume, sellOffer.getPrice());

                if(buyOffer.getQuantity() == 0){
                    break;
                }
            }
        }
        workerBuy.removeIf(x -> x.getQuantity() == 0);
        speculateSell.removeIf(x -> x.getQuantity() == 0);

        for(SpeculateOffer buyOffer : speculateBuy){
            Speculate buyOfferOwner = findSpeculate(buyOffer.getId());
            if(buyOffer.getQuantity() <= 0){
                continue;
            }
            for(WorkerOffer sellOffer : workerSell){
                if(!Objects.equals(buyOffer.getTypeOfProduction(), sellOffer.getTypeOfProduction())){
                    continue;
                }
                if(sellOffer.getQuantity() <= 0){
                    continue;
                }
                Worker sellOfferOwner = findWorker(sellOffer.getId());
                int dealVolume = Math.min(buyOffer.getQuantity(), sellOffer.getQuantity());
                AbstractPlayer.transferDiamonds(buyOfferOwner, sellOfferOwner, (int)(dealVolume * buyOffer.getPrice()));
                buyOfferOwner.addProduction(dealVolume, sellOffer.getQuality(), sellOffer.getTypeOfProduction());
                buyOffer.changeQuantity(-dealVolume);
                sellOffer.changeQuantity(-dealVolume);
                current.passOffer(sellOffer.getTypeOfProduction(), dealVolume, buyOffer.getPrice());
                if(buyOffer.getQuantity() == 0){
                    break;
                }
            }
        }
        workerSell.removeIf(x -> x.getQuantity() == 0);
        speculateBuy.removeIf(x -> x.getQuantity() == 0);


        current.checkPrices();
        for(WorkerOffer offer : workerSell){
            Worker offerOwner = findWorker(offer.getId());
            double minPrice;
            try {
                minPrice = current.getPrice("min", offer.getTypeOfProduction());
            } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }

            offerOwner.addProduction((int) (offer.getQuantity() * minPrice), 1, "diamonds");



        }

        for(SpeculateOffer offer : speculateSell){
            if(offer.getQuantity() <= 0){
                continue;
            }
            Speculate offerOwner = findSpeculate(offer.getId());
            offerOwner.addProduction(offer.getQuantity(), offer.getQuality(), offer.getTypeOfProduction());
        }
        current.checkPrices();
    }
}
