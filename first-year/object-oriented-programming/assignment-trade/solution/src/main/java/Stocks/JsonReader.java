package Stocks;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static Stocks.Stock.in;

public class JsonReader {
    private static final ObjectMapper ObjectMapper = getDefaultObjectMapper();

    private static ObjectMapper getDefaultObjectMapper(){
        ObjectMapper defaultObjectMapper = new ObjectMapper();

        return defaultObjectMapper;
    }
    static JsonNode parse(String src) throws IOException {
        return ObjectMapper.readTree(src);
    }
    public static JsonNode getData() throws IOException {
        return parse(new String(Files.readAllBytes(Paths.get(in))));}
}
