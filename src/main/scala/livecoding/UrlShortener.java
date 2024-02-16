package livecoding;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;

public class UrlShortener {
    private final String label = "https://revolut-interview.com";
    private final Random random;
    private final Map<String, String> map;

    public UrlShortener(Random random, Map<String, String> map) {

        ThreadLocalRandom x  = ThreadLocalRandom.current();

        this.random = random;
        this.map = map;
    }

    public String shortenUrl(String originalUrl) {
        if (originalUrl.matches("https?:(www\\.)?//\\w+\\.\\w+([?\\w&=#@\\d])+")) {
            String shortUrl = buildRandomShortUrl();
            map.putIfAbsent(shortUrl, originalUrl);
            return label + "/" + shortUrl;
        } else {
            throw new IllegalArgumentException("Please provide the valid url");
        }
    }

    public String redirect(String shortUrl) {
        String longUrl = map.get(shortUrl);
        if (longUrl == null)
            throw new IllegalStateException("Not found");
        return longUrl;
    }

    private String buildRandomShortUrl() {
        StringBuilder sb = new StringBuilder(10);
        for (int i = 0; i < sb.capacity(); i++) {
            sb.append(randomCharacter());
        }
        return sb.toString();
    }

    private char randomCharacter() {
        return random.nextInt(0, 2) % 2 == 0 ?
                (char) (random.nextInt(0, 26) + 'a') :
                (char) (random.nextInt(0, 26) + 'A');
    }

    public static void main(String[] args) {
        UrlShortener urlShortener = new UrlShortener(new Random(), new ConcurrentHashMap<>());
        System.out.println(urlShortener.shortenUrl("https://revolut.com"));
    }
}

// how much chars?
// which chars do we use for a shortening?
// how strict should be the regexp & do we process only urls?
// Random is thread safe, but ThreadLocalRandom has better performance
// ConcurrentHashMap
