import java.util.*;
import java.util.Arrays;

public class Java4Test {

    public static int[] productExceptSelf(int[] nums) {
        int n = nums.length;
        int[] left = new int[n];
        int[] right = new int[n];
        int[] ans = new int[n];

        left[0] = 1;
        right[n - 1] = 1;

        for (int i = 1; i < n; i++) {
            left[i] = left[i - 1] * nums[i - 1];
        }

        for (int j = n - 2; j >= 0; j--) {
            right[j] = right[j + 1] * nums[j + 1];
        }

        for (int q = 0; q < n; q++) {
            ans[q] = left[q] * right[q];
        }

        return ans;

    }

    public static List<Integer> intersection(int[][] nums) {
        Map<Integer, Integer> counts = new HashMap<>();
        for (int[] arr : nums) {
            for (int x : arr) {
                counts.put(x, counts.getOrDefault(x, 0) + 1);
            }
            System.out.println();
        }

        int n = nums.length;
        List<Integer> ans = new ArrayList<>();
        for (int key : counts.keySet()) {
            if (counts.get(key) == n) {
                ans.add(key);
            }
        }

        Collections.sort(ans);
        return ans;
    }

    public static void main(String[] args) {
//        System.out.println(Arrays.toString(productExceptSelf(new int[]{1, 2, 3, 4})));
        System.out.println(intersection(new int[][]{
                new int[]{3, 1, 2, 4, 5},
                new int[]{1, 2, 3, 4},
                new int[]{3, 4, 5, 6},
        }));
    }
}
