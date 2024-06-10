import java.util.*;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Arrays;

public class BinarySearchTreeJava {
    public static class TreeNode {
        int val;
        TreeNode left;
        TreeNode right;

        TreeNode() {
        }

        TreeNode(int val) {
            this.val = val;
        }

        TreeNode(int val, TreeNode left, TreeNode right) {
            this.val = val;
            this.left = left;
            this.right = right;
        }
    }


    // 98. Validate Binary Search Tree
    public boolean isValidBST(TreeNode root) {
        return dfs(root, null, null);
    }

    private boolean dfs(TreeNode root, Integer low, Integer high) {
        if (root == null) return true;
        if (low != null && root.val <= low || high != null && root.val >= high) return false;
        return dfs(root.left, low, root.val) && dfs(root.right, root.val, high);
    }


    // 114. Flatten Binary Tree to Linked List
    public void flatten(TreeNode root) {
        Queue<TreeNode> queue = new LinkedList<>();
        queue(queue, root);
        TreeNode temp = new TreeNode();
        while (!queue.isEmpty()) {
            temp.right = queue.poll();
            temp.left = null;
            temp = temp.right;
        }
    }

    public void queue(Queue<TreeNode> queue, TreeNode root) {
        if (null == root) {
            return;
        }
        queue.add(root);
        queue(queue, root.left);
        queue(queue, root.right);
    }

    // 199. Binary Tree Right Side View
    public List<Integer> rightSideView(TreeNode root) {
        if (root == null)
            return new ArrayList<>();

        List<Integer> result = new ArrayList<>();
        LinkedList<TreeNode> queue = new LinkedList<>();
        queue.add(root);

        while (!queue.isEmpty()) {
            int size = queue.size();
            System.out.println(size);
            result.add(queue.getLast().val);

            for (int i = 0; i < size; i++) {
                TreeNode node = queue.poll();

                if (node.left != null) queue.add(node.left);
                if (node.right != null) queue.add(node.right);
            }
        }


        return result;
    }

    // 1283. Find the Smallest Divisor Given a Threshold
    public int smallestDivisor(int[] nums, int threshold) {
        int left = 1;
        int right = Collections.max(Arrays.stream(nums)
                .boxed()
                .toList());

        while (left <= right) {
            int result = 0;
            int middle = (left + right)/2;

            for (int x : nums) {
                result += (int) Math.ceil(x * 1.0 / middle);
            }

            if (result < threshold) {
                right = middle - 1;
            } else {
                left = middle + 1;
            }
        }

        return left;
    }


    public static void main(String[] args) {
        System.out.println(new BinarySearchTreeJava().smallestDivisor(new int[]{ 1,2,5,9}, 6));
    }
}
