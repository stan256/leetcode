
object BinaryTree extends App {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }


  // 100. Same Tree
  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p != null && q != null)
      p.value == q.value && isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
    else if (p == null && q == null)
      true
    else
      false
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

  // 110. Balanced Binary Tree
  def isBalanced(root: TreeNode): Boolean = {
    false
  }
//  println(isBalanced(???))


  // 112. Path Sum
  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
    if (root == null) return false

    if (root.value - targetSum == 0 && root.left == null && root.right == null)
      true
    else
      (root.left != null && hasPathSum(root.left, targetSum - root.value)) || (root.right != null) && hasPathSum(root.right, targetSum - root.value)
  }

  // 1448. Count Good Nodes in Binary Tree
  def goodNodes(root: TreeNode): Int = {
    def step(node: TreeNode, prevValue: Int): Int = {
      if (node == null)
        return 0

      step(node.left, Math.max(node.value, prevValue)) + step(node.right, Math.max(node.value, prevValue)) + (if (node.value >= prevValue) 1 else 0)
    }

    step(root, root.value)
  }


  // 236. Lowest Common Ancestor of a Binary Tree
  def lowestCommonAncestor_2(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    if (root == null)
      return null

    if (root == p || root == q)
      return root

    val l = lowestCommonAncestor_2(root.left, p, q)
    val r = lowestCommonAncestor_2(root.right, p, q)

    if (l != null && r != null)
      return root

    if (l != null) l else r
  }

  // 111. Minimum Depth of Binary Tree
  def minDepth(root: TreeNode): Int = {
    def dfs(node: TreeNode, k: Int): Int = {
      if (node.left == null && node.right == null)
        k
      else if (node.left != null && node.right != null) {
        val leftLength = if (node.left != null) dfs(node.left, k+1) else 0
        val rightLength = if (node.right != null) dfs(node.right, k+1) else 0
        Math.min(leftLength, rightLength)
      } else if (node.left != null)
        dfs(node.left, k+1)
      else
        dfs(node.right, k+1)
    }

    if (root == null) 0
    else dfs(root, 1)
  }

  // 1026. Maximum Difference Between Node and Ancestor
  def maxAncestorDiff(root: TreeNode): Int = {

    def dfs(node: TreeNode, min: Int, max: Int, maxDiff: Int): Int = {
      if (node == null)
        return maxDiff

      val subResult = Math.max(Math.abs(min - node.value), Math.abs(max - node.value))

      Math.max(
        dfs(node.left,  Math.min(min, node.value), Math.max(max, node.value), Math.max(subResult, maxDiff)),
        dfs(node.right, Math.min(min, node.value), Math.max(max, node.value), Math.max(subResult, maxDiff))
      )
    }

    dfs(root, root.value, root.value, 0)
  }

  def bfs_print(root: TreeNode): Unit = {
    val queue = scala.collection.mutable.Queue.empty[TreeNode]
    queue.append(root)

    while (queue.nonEmpty) {
      val nodesInCurrentLevel = queue.size

      for (_ <- 0 until nodesInCurrentLevel) {
        val node = queue.removeLast()
        println(node.value)
        if (node.left != null) queue.prepend(node.left)
        if (node.right != null) queue.prepend(node.right)
      }
    }
  }

  // 199. Binary Tree Right Side View
  def rightSideView(root: TreeNode): List[Int] = {
    if (root == null)
      return List.empty

    var res = List.empty[Int]
    val queue = collection.mutable.Queue.empty[TreeNode]
    queue.append(root)

    while (queue.nonEmpty) {
      val size = queue.length
      res = res :+ queue.last.value

      for (_ <- 0 until size) {
        val node = queue.removeHead()
        if (node.left != null) queue.append(node.left)
        if (node.right != null) queue.append(node.right)
      }
    }
    res
  }

}
