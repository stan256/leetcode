
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

  // 235. Lowest Common Ancestor of a Binary Search Tree
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    def find(node: TreeNode): TreeNode =
      (p, q) match {
        case (a, b) if a.value < node.value && b.value < node.value => find(node.left)
        case (a, b) if a.value > node.value && b.value > node.value => find(node.right)
        case _ => node
      }

    find(root)
  }

  //  println(lowestCommonAncestor(root, bbigger1, bsmaller1).value)

  // 102. Binary Tree Level Order Traversal
  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null)
      return List()

    val map = scala.collection.mutable.Map.empty[Int, List[Int]]

    def update(depth: Int, node: TreeNode): Unit = {
      if (!map.contains(depth))
        map.put(depth, List())
      map.update(depth, map(depth) :+ node.value)
      if (node.left != null)
        update(depth + 1, node.left)
      if (node.right != null)
        update(depth + 1, node.right)
    }

    update(0, root)
    map.values.toList
  }

  // 543. Diameter of Binary Tree
  def diameterOfBinaryTree(root: TreeNode): Int = {
    //                                 result, longest
    def count(node: TreeNode): (Int, Int) = {
      if (node.left == null && node.right == null) {
        (0, 0)
      } else if (node.left == null && node.right != null) {
        val right = count(node.right)
        (Math.max(right._1, right._2 + 1), right._2 + 1)
      } else if (node.left != null && node.right == null) {
        val left = count(node.left)
        (Math.max(left._1, left._2 + 1), left._2 + 1)
      } else {
        val left = count(node.left)
        val right = count(node.right)
        val res = (left._2 + right._2 + 2 :: left._1 :: right._1 :: Nil).max
        val longest = Math.max(left._2, right._2) + 1
        (res, longest)
      }
    }

    count(root)._1
  }

  //  val a = new TreeNode(2)
  //  val bbigger1 = new TreeNode(7)
  //  val bsmaller1 = new TreeNode(5)
  //  val b = new TreeNode(6, bsmaller1, bbigger1)
  //  val root = new TreeNode(4, a, b)
//  val root = new TreeNode(4,
//    new TreeNode(-7),
//    new TreeNode(-3,
//      new TreeNode(-9,
//        new TreeNode(9, new TreeNode(6, new TreeNode(0, new TreeNode(-1)), new TreeNode(6, new TreeNode(-4)))),
//        new TreeNode(6, new TreeNode(0, new TreeNode(-1)), new TreeNode(6, new TreeNode(-4, new TreeNode(-2))))
//      ),
//      new TreeNode(-3, new TreeNode(-4))
//    )
//  )

}
