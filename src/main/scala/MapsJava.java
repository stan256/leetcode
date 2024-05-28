import java.util.HashMap;
import java.util.*;

public class MapsJava {

    // Pair Sums
    int numberOfWays(int[] arr, int k) {
        Map<Integer, Integer> map = new HashMap<>();
        int answer = 0;
        for (int i: arr) {
            map.put(i, map.getOrDefault(i, 0) + 1);
        }
        for (int i: arr) {
            if (i == k/2 && map.containsKey(i) && map.get(i) > 1) {
                answer += map.get(i);
                map.remove(i);
            } else if (map.containsKey(k - i)) {
                answer++;
                map.remove(i);
            }
        }
        return answer;
    }

    public static void main(String[] args) {
        System.out.println(new MapsJava().numberOfWays(new int[]{1, 5, 3, 3, 3}, 6));
    }
}
