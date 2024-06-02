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


    public static void main(String[] args) {
//        TreeNode treeNode = new TreeNode(2, new TreeNode(1), new TreeNode(3));
        TreeNode treeNode = new TreeNode(Integer.MIN_VALUE, new TreeNode(Integer.MIN_VALUE), null);
        System.out.println(new BinarySearchTreeJava().isValidBST(treeNode));
// 1 1
    }
}
