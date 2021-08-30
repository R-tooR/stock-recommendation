import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import recommendation.recommender.Recommender;
import recommendation.recommender.RecommenderManufacture;
import recommendation.recommender.RecommenderType;
import updater.DatabaseUpdater;
import recommendation.recommender.SubgraphRecommender;

import java.io.*;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.HashMap;

public class Client {


    public static void main(String[] args) {
        String hostname = "127.0.0.1";
        int port = Integer.parseInt("6666");
        ObjectMapper mapper = new ObjectMapper();
        Recommender recommender = RecommenderManufacture.createSubgraphRecommender();
//        Recommender recommender = RecommenderManufacture.createFullGraphRecommender();
        DatabaseUpdater updater = new DatabaseUpdater(0.1);
        updater.initialize();
        TypeReference<HashMap<String, Object>> typeRef
                = new TypeReference<HashMap<String, Object>>() {};
        try (Socket socket = new Socket(hostname, port)) {

            InputStream input = socket.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(input));
            while(true) {
                String time = reader.readLine();
                System.out.println(mapper.readValue(time, typeRef));
                var x = recommender.step(mapper.readValue(time, typeRef), 0);
                System.out.println("Result is: " + x);
                updater.update();
            }

        } catch (UnknownHostException ex) {

            System.out.println("Server not found: " + ex.getMessage());

        } catch (IOException ex) {

            System.out.println("I/O error: " + ex.getMessage());
        }
    }
}
