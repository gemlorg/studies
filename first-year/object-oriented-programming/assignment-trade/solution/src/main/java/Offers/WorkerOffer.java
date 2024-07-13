package Offers;

public class WorkerOffer {
    private final int quality;
    private int quantity;
    private final String typeOfProduction;
    private final int id;
    private final int wealth;

    protected WorkerOffer(int quality, int quantity, String typeOfProduction, int id, int wealth) {
        this.quality = quality;
        this.quantity = quantity;
        this.typeOfProduction = typeOfProduction;
        this.id = id;
        this.wealth = wealth;
    }
    public static WorkerOffer newOffer(int quality, int quantity, String typeOfProduction, int id, int wealth){
        return new WorkerOffer(quality, quantity, typeOfProduction, id, wealth);
    }

    public int getQuality() {
        return quality;
    }

    public int getQuantity() {
        return quantity;
    }

    public String getTypeOfProduction() {
        return typeOfProduction;
    }

    public int getId() {
        return id;
    }

    public int getWealth() {
        return wealth;
    }

    public void changeQuantity(int change){
        quantity += change;
    }

    @Override
    public String toString() {
        return "WorkerOffer{" +
                "quality=" + quality +
                ", quantity=" + quantity +
                ", typeOfProduction='" + typeOfProduction + '\'' +
                ", id=" + id +
                ", wealth=" + wealth +
                '}';
    }
}
