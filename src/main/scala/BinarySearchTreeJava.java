import java.util.LinkedList;
import java.util.Queue;

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


    public static void main(String[] args) {
//        TreeNode treeNode = new TreeNode(2, new TreeNode(1), new TreeNode(3));
        TreeNode treeNode = new TreeNode(Integer.MIN_VALUE, new TreeNode(Integer.MIN_VALUE), new TreeNode(Integer.MIN_VALUE));
        new BinarySearchTreeJava().flatten(treeNode);
        System.out.println();
// 1 1
    }
}
