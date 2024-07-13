import Stocks.Stock;


import java.io.IOException;


public class Main {
    public static String INPUT_FILE = "src/main/java/inFile.json";
    public static final String OUT_FILE = "src/main/java/outFile.json";

    public static void main(String[] args) throws IOException {
        Stock game = Stock.setStock(INPUT_FILE, OUT_FILE);
        game.play();


    }
}
