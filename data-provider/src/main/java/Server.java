import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class Server {
    private Map<String, Stock> stocks = new ConcurrentHashMap<>();
    private boolean initialized = false;

    Server() throws IOException {
        System.out.println(new java.io.File(".").getCanonicalPath());
        Server.walkin(new File(new java.io.File(".").getCanonicalPath() + "\\data-provider\\src\\main\\resources"), stocks);
    }

    private Map<String, Double> step() {
        if (!initialized) stocks.forEach((name, stock) -> stock.initialize());
        else stocks.forEach((name, stock) -> stock.update());
        Map<String, Double> map = new HashMap<>();

        for (Map.Entry<String, Stock> entry : stocks.entrySet()) {
            map.put(entry.getKey().trim().split("\\.")[0], entry.getValue().getStockScore());
        }

        return map.entrySet().stream().filter(x -> !x.getValue().equals(Double.NaN)).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    public static void main(String[] args) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        Server s = new Server();
        int port = Integer.parseInt("6666");

        try (ServerSocket serverSocket = new ServerSocket(port)) {

            System.out.println("Server is listening on port " + port);

            Socket socket = serverSocket.accept();

            System.out.println("Connection with recommendation engine established!");

            OutputStream output = socket.getOutputStream();
            PrintWriter writer = new PrintWriter(output, true);

            while (true) {
                writer.println(mapper.writeValueAsString(s.step()));
                Thread.sleep(2000);
            }

        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
            ex.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static void walkin(File dir, Map<String, Stock> map) {

        File[] listFile = dir.listFiles();
        System.out.println(Arrays.toString(listFile));
        if (listFile != null) {
            for (File file : listFile) {
                if (file.isDirectory()) {
                    System.out.println("|\t\t");
                    walkin(file, map);
                } else {
                    map.put(file.getName(), new Stock(file.getAbsolutePath()));
                }
            }
        }
    }
}
