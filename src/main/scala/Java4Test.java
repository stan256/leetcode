import java.math.BigDecimal;
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

    // 3. Longest Substring Without Repeating Characters
    public int lengthOfLongestSubstring(String s) {
        int left = 0, right = 0, max = 0;
        Map<Character, Integer> map = new HashMap<>(28);
        while (right < s.length()) {
            char key = s.charAt(right++);
            map.put(key, map.getOrDefault(key, 0) + 1);
            while (!map.values().stream().allMatch(x -> x == 1 || x == 0)) {
                map.put(s.charAt(left), map.get(s.charAt(left)) - 1);
                left++;
            }
            max = Math.max(max, right - left);
        }
        return max;
    }


    // 8. String to Integer (atoi)
    public int myAtoi(String s) {
        char[] chars = s.trim().toCharArray();
        StringBuilder sb = new StringBuilder();
        boolean nonZeroDigitMet = false;
        boolean anyDigitMet = false;
        boolean negativeNumber = false;

        for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            if (c == '-') {
                if (anyDigitMet || i > 0) return response(negativeNumber, sb);
                else negativeNumber = true;
            } else if (c == '+') {
                if (anyDigitMet || i > 0) return response(negativeNumber, sb);
                else negativeNumber = false;
            } else if (c == '\s') {
                if (anyDigitMet || i > 0) return response(negativeNumber, sb);
            } else if (isDigit(c)) {
                anyDigitMet = true;

                if (nonZeroDigitMet) {
                    if (isDigit(c)) {
                        sb.append(c);
                    }
                } else {
                    if (isNonZeroDigit(c)) {
                        sb.append(c);
                        nonZeroDigitMet = true;
                    }
                }
            } else {
                return response(negativeNumber, sb);
            }
        }

        return response(negativeNumber, sb);
    }

    private static int response(boolean negativeNumber, StringBuilder sb) {
        if (sb.isEmpty()) sb.append(0);

        BigDecimal result = new BigDecimal(sb.toString());
        if (result.compareTo(BigDecimal.valueOf(Integer.MAX_VALUE)) > 0) {
            if (negativeNumber) return -Integer.MAX_VALUE - 1;
            else return Integer.MAX_VALUE;
        } else {
            if (negativeNumber) return -Integer.parseInt(sb.toString());
            else return Integer.parseInt(sb.toString());
        }
    }

    private boolean isDigit(char c) {
        return c >= 48 && c <= 57;
    }

    private boolean isNonZeroDigit(char c) {
        return c >= 49 && c <= 57;
    }


    public static void main(String[] args) {
//        System.out.println(new Java4Test().lengthOfLongestSubstring("dd"));
    }
}

