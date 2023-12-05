

object BinaryTree extends App {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }


  // 226. Invert Binary Tree
  def invertTree(root: TreeNode): TreeNode = {
    if (root == null) return root
    new TreeNode(root.value, invertTree(root.right), invertTree(root.left))
  }

  // 104. Maximum Depth of Binary Tree
  def maxDepth(root: TreeNode): Int = {
    def rec(node: TreeNode, depth: Int): Int =
      if (node == null)
        depth
      else
        Math.max(rec(node.left, depth + 1), rec(node.right, depth + 1))
    rec(root, 0)
  }



}
