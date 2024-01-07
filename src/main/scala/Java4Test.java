import java.util.*;

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

    public static int subarraySum(int[] nums, int k) {
        Map<Integer, Integer> counts = new HashMap<>();
        counts.put(0, 1);

        int ans = 0;
        int curr = 0;

        for (int num : nums) {
            curr += num;
            ans += counts.getOrDefault(curr - k, 0);
            counts.put(curr, counts.getOrDefault(curr, 0) + 1);
        }

        return ans;
    }


    public static void main(String[] args) {
    }
}

