package Stocks;

import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Dictionary;

import static java.util.logging.Level.parse;

public class GameParameters {
    private static final JsonNode data;
    static {
        try {
            data = JsonReader.getData();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public static final JsonNode gameInfo = data.get("info");
    public static final JsonNode workersInfo = data.get("workers");
    public static final JsonNode speculatesInfo = data.get("speculates");

    public GameParameters() throws IOException {
    }

}
