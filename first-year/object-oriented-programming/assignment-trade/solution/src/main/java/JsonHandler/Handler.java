package JsonHandler;


import java.io.*;



public class Handler {


    public static void main(String in) {
        BufferedReader reader = null;
        BufferedWriter writer = null;
        String originalFilePath = in;
        String originalFileContent = "";
        try {


            reader = new BufferedReader(new FileReader(originalFilePath));

            String currentReadingLine = reader.readLine();


            while (currentReadingLine != null) {
                originalFileContent += currentReadingLine + System.lineSeparator();
                currentReadingLine = reader.readLine();
            }

            String modifiedFileContent = originalFileContent.replaceAll("dlugosc", "gameLength")
                    .replaceAll("gielda", "stockExchange").replaceAll("ceny", "prices")
                    .replaceAll("programy", "programs").replaceAll("jedzenie", "food")
                    .replaceAll("ubrania", "clothes").replaceAll("narzedzia", "tools")
                    .replaceAll("robotnicy", "workers").replaceAll("poziom", "level")
                    .replaceAll("kariera", "career").replaceAll("kupowanie", "buyingStrategy")
                    .replaceAll("produkcja", "productionStrategy").replaceAll("uczenie", "learning")
                    .replaceAll("\"typ\"", "\"type\"").replaceAll("zapas", "productionReserve")
                    .replaceAll("okres", "period").replaceAll("zmiana", "careerStrategy")
                    .replaceAll("produktywnosc", "productivity").replaceAll("diamenty", "diamonds")
                    .replaceAll("zasoby", "resources").replaceAll("spekulanci", "speculates")
                    .replaceAll("socjalistyczna", "socialist").replaceAll("kapitalistyczna ", "capitalist")
                    .replaceAll("zrównoważona", "balanced").replaceAll("programista", "programmer")
                    .replaceAll("rolnik", "farmer").replaceAll("górnik", "miner")
                    .replaceAll("rzemieślnik", "craftsman").replaceAll("inżynier", "engineer")
                    .replaceAll("technofob", "technofobe").replaceAll("czyścioszek", "cleaner")
                    .replaceAll("zmechanizowany", "mechanised").replaceAll("gadzeciarz", "gadgeteer")
                    .replaceAll("chciwy", "greedy").replaceAll("średniak", "average")
                    .replaceAll("perspektywiczny", "prospective").replaceAll("osowy", "random")
                    .replaceAll("krótkowzroczny", "nearsighted").replaceAll("pracuś", "hardWorker")
                    .replaceAll("oszczędny", "saver").replaceAll("okresowy", "periodicLearner")
                    .replaceAll("rozkładowy", "random").replaceAll("konserwatysta", "conservative")
                    .replaceAll("rewolucjonista", "revolutionary").replaceAll("", "")
                    .replaceAll("wypukly", "convex").replaceAll("regulujący", "regulator")
                    .replaceAll("średni", "average").replaceAll("oszczędny", "saving")
                    .replaceAll("historiaPerspektywy", "historyPerspective").replaceAll("oszczędny", "saving")
                    .replaceAll("historiaSredniejProdukcji", "historyTrack").replaceAll("wypukli", "convex")
                    .replaceAll("średni", "average").replaceAll("regulującyRynek", "marketGuard")
                    .replaceAll("liczbaNarzedzi", "numberOfTools").replaceAll("historia_sredniej_produkcji ", "historyTrack")
                    .replaceAll("limit_diamentów", "diamondLimit").replaceAll("kara_za_brak_ubrań", "punishmentForNoClothes")
                    .replaceAll("liczba_narzedzi", "numberOfTools").replaceAll("historia_perspektywy", "historyPerspective")
                    .replaceAll("regulujący_rynek", "marketGuard").replaceAll("kara_za_brak_ubran", "punishmentForNoClothes")
                    .replaceAll("", "").replaceAll("", "")
                    .replaceAll("", "").replaceAll("", "")
                    .replaceAll("", "").replaceAll("", "")
                    .replaceAll("", "").replaceAll("", "");

            writer = new BufferedWriter(new FileWriter(originalFilePath));

            writer.write(modifiedFileContent);

        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {

            try {
                if (reader != null) {
                    reader.close();
                }

                if (writer != null) {
                    writer.close();
                }

            } catch (IOException e) {
            }
        }
    }

}

