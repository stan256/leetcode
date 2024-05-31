import java.math.BigDecimal;
import java.util.*;

public class ArraysJava {

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

    // 26. Remove Duplicates from Sorted Array
    public int removeDuplicates(int[] nums) {
        if (nums.length == 1)
            return 1;

        int left = 0;
        int right = 1;
        int limit = nums.length;

        while (right < limit) {
            if (nums[left] == nums[right]) {
                int i = left;
                int j = right;
                for (; j < limit; i++, j++) {
                    nums[i] = nums[j];
                }
                limit--;
            } else {
                left++;
                right++;
            }
        }

        return limit;
    }

    // 88. Merge Sorted Array
    public void merge(int[] nums1, int m, int[] nums2, int n) {
        List<Integer> res = new ArrayList<>(m + n + 1);
        int i = 0, j = 0;
        while (i < m && j < n) {
            if (nums1[i] == nums2[j]) {
                res.add(nums1[i++]);
                res.add(nums2[j++]);
            } else if (nums1[i] < nums2[j]) res.add(nums1[i++]);
            else res.add(nums2[j++]);
        }
        while (i < m) res.add(nums1[i++]);
        while (j < n) res.add(nums2[j++]);

        for (int k = 0; k < res.size(); k++) {
            nums1[k] = res.get(k);
        }
    }

    // 76. Minimum Window Substring
    // NOT COMPLETED
    public String minWindow(String s, String t) {
        String result = "";

        Map<Character, Integer> chars = countChars(t);
        Map<Character, Integer> current = new HashMap<>();

        int left = 0, right = 0;
        while (right < s.length()) {
            char rightChar = s.charAt(right);
            current.put(rightChar, current.getOrDefault(rightChar, 0) + 1);
            right++;

            while (containsAllChars(chars, current) && left < right) {
                if (result.isEmpty() || result.length() > left - right)
                    result = s.substring(left, right);
                char leftChar = s.charAt(left++);
                current.put(leftChar, current.get(leftChar) - 1);
            }

        }

        return result;
    }

    private boolean containsAllChars(Map<Character, Integer> x, Map<Character, Integer> shouldContainAll) {
        return x.entrySet().stream().allMatch(entry -> shouldContainAll.getOrDefault(entry.getKey(), -1) >= entry.getValue());
    }

    private Map<Character, Integer> countChars(String input) {
        Map<Character, Integer> map = new HashMap<>();
        for (int i = 0; i < input.length(); i++) {
            map.put(input.charAt(i), map.getOrDefault(input.charAt(i), 0) + 1);
        }
        return map;
    }


    // 1460. Make Two Arrays Equal by Reversing Subarrays
    public boolean canBeEqual(int[] target, int[] arr) {
        Map<Integer, Integer> tCount = new HashMap<>();
        Map<Integer, Integer> arrCount = new HashMap<>();

        for (int t : target) {
            tCount.put(t, tCount.getOrDefault(t, 0) + 1);
        }
        for (int a : arr) {
            arrCount.put(a, arrCount.getOrDefault(a, 0) + 1);
        }

        for (Map.Entry<Integer, Integer> e : tCount.entrySet()) {
            if (arrCount.getOrDefault(e.getKey(), -1) != e.getValue())
                return false;
        }
        return true;
    }

    int[] countSubarrays(int[] arr) {
        // Write your code here

        int[] result = new int[arr.length];
        for (int i = 0; i < arr.length; i++) {
            int left = 0;
            int right = arr.length - 1;
            int value = arr[i];

            for (int j = 0; j < arr.length; j++) {
                if (arr[j] > value && j < i) left = j;
                if (arr[j] > value && j > i) right = j;
            }

            result[i] = right - left;
        }

        return result;
    }

    public static List<Integer> performOperations(List<Integer> arr, List<List<Integer>> operations) {
        // Write your code here
        for (List<Integer> o : operations) {
            int left = o.get(0);
            int right = o.get(1);
            while (left < right) {
                int temp = arr.get(right);
                arr.set(right, arr.get(left));
                arr.set(left, temp);
                left++;
                right--;
            }
        }
        return arr;
    }
}

