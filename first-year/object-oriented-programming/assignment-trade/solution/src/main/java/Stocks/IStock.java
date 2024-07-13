package Stocks;

import Players.Strategies.Offers.SpeculateOffer;
import Players.Strategies.Offers.WorkerOffer;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.io.IOException;
import java.util.ArrayList;

public interface IStock {

    void play() throws IOException;
    void makeMove() throws IOException;
    void printInfo() throws IOException;

    void compareOffers(ArrayList<WorkerOffer> workerSell, ArrayList<WorkerOffer> workerBuy, ArrayList<SpeculateOffer> speculateSell, ArrayList<SpeculateOffer> speculateBuy, DataContainer current);



}
