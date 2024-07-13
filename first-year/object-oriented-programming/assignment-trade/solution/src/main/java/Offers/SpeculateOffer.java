package Offers;

import Players.Speculate;
import Players.Strategies.Offers.WorkerOffer;
import Players.Strategies.ProductionReserve;

public class SpeculateOffer extends WorkerOffer {
    private double price;
    private SpeculateOffer(int quality, int quantity, String typeOfProduction, int id, int wealth, double price){
        super(quality, quantity, typeOfProduction, id, wealth);
        this.price = price;
    }

    public double getPrice() {
        return price;
    }

    public void setPrice(double price) {
        this.price = price;
    }


    public static SpeculateOffer newOffer(int quality, int quantity, String typeOfProduction, int id, int wealth, double price){
        return new SpeculateOffer(quality, quantity, typeOfProduction, id, wealth, price);
    }


}
