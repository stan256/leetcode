
object BinaryTree extends App {
  case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
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

  // 102. Binary Tree Level Order Traversal bfs
  def levelOrder_bfs(root: TreeNode): List[List[Int]] = {
    if (root == null) return List()

    val queue = collection.mutable.Queue.empty[TreeNode]
    val lb = collection.mutable.ListBuffer.empty[List[Int]]
    queue.enqueue(root)

    while (queue.nonEmpty) {
      lb.addOne(queue.toList.map(_.value))
      val size = queue.size
      for (i <- 0 until size) {
        val node = queue.dequeue()
        if (node.left != null) queue.enqueue(node.left)
        if (node.right != null) queue.enqueue(node.right)
      }
    }
    lb.toList
  }


  // 1485. Clone Binary Tree With Random Pointer
  class Node(var _value: Int, _left: Node = null, _right: Node = null, _random: Node = null) {
    var value: Int = _value
    var left: Node = _left
    var right: Node = _right
    var random: Node = _random
  }
  class NodeCopy(var _value: Int, _left: NodeCopy = null, _right: NodeCopy = null, _random: NodeCopy = null) {
    var value: Int = _value
    var left: NodeCopy = _left
    var right: NodeCopy = _right
    var random: NodeCopy = _random
  }
  def copyRandomBinaryTree(root: Node): NodeCopy = {
    val map = collection.mutable.HashMap.empty[Node, NodeCopy]

    def copy(node: Node): NodeCopy = {
      if (node == null) return null
      if (!map.contains(node)) {
        map.put(node, new NodeCopy(node.value, null, null, null))
        map(node).left = map.getOrElse(node.left, copy(node.left))
        map(node).right = map.getOrElse(node.right, copy(node.right))
        map(node).random = map.getOrElse(node.random, copy(node.random))
      }
      map(node)
    }

    copy(root)
  }

  // 1490. Clone N-ary Tree
  class TNode(var _value: Int) {
    var value: Int = _value
    var children: List[TNode] = List()
  }
  def cloneTree(root: TNode): TNode = {
    val map = collection.mutable.Map.empty[TNode, TNode]

    def copy(node: TNode): TNode = {
      if (node == null) return null
      if (!map.contains(node)) {
        map.put(node, new TNode(node.value))
        map(node).children = node.children.map(copy)
      }
      map(node)
    }

    copy(root)
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

  // 236. Lowest Common Ancestor of a Binary Tree
  def lowestCommonAncestor_3(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    if (root == null || root == p || root == q) return root
    var answer: TreeNode = null

    def dfs(node: TreeNode): Boolean = {
      if (node == null) return false

      val left = if (dfs(node.left)) 1 else 0
      val right = if (dfs(node.right)) 1 else 0
      val mid = if (node == p || node == q) 1 else 0

      if (mid + left + right == 2) answer = node
      left + right + mid > 0
    }

    dfs(root)
    answer
  }

  // 111. Minimum Depth of Binary Tree
  def minDepth(root: TreeNode): Int = {
    def dfs(node: TreeNode, k: Int): Int = {
      if (node.left == null && node.right == null)
        k
      else if (node.left != null && node.right != null) {
        val leftLength = if (node.left != null) dfs(node.left, k + 1) else 0
        val rightLength = if (node.right != null) dfs(node.right, k + 1) else 0
        Math.min(leftLength, rightLength)
      } else if (node.left != null)
        dfs(node.left, k + 1)
      else
        dfs(node.right, k + 1)
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
        dfs(node.left, Math.min(min, node.value), Math.max(max, node.value), Math.max(subResult, maxDiff)),
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

  // 515. Find Largest Value in Each Tree Row
  def largestValues(root: TreeNode): List[Int] = {
    if (root == null)
      return List.empty

    var res = List.empty[Int]
    val queue = collection.mutable.Queue.empty[TreeNode]
    queue append root

    while (queue.nonEmpty) {
      val size = queue.size
      res = res :+ queue.map(_.value).max

      for (_ <- 0 until size) {
        val node = queue.removeLast()
        if (node.left != null) queue prepend node.left
        if (node.right != null) queue prepend node.right
      }
    }
    res
  }

  // 1302. Deepest Leaves Sum
  def deepestLeavesSum(root: TreeNode): Int = {
    if (root == null)
      return 0

    var res = 0
    val queue = collection.mutable.Queue.empty[TreeNode]
    queue append root
    while (queue.nonEmpty) {
      val size = queue.size
      res = queue.map(_.value).sum
      for (_ <- 0 until size) {
        val node = queue.removeLast()
        if (node.left != null) queue.prepend(node.left)
        if (node.right != null) queue.prepend(node.right)
      }
    }
    res
  }

  // 103. Binary Tree Zigzag Level Order Traversal
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null)
      return List.empty

    val queue = scala.collection.mutable.Queue.empty[TreeNode]
    var result = List.empty[List[Int]]
    var level = 1
    queue append root

    while (queue.nonEmpty) {
      val size = queue.size
      result = result :+ (if (level % 2 == 1) queue.toList.map(_.value) else queue.toList.map(_.value).reverse)
      level += 1

      for (_ <- 0 until size) {
        val node = queue.removeHead()
        if (node.left != null) queue.append(node.left)
        if (node.right != null) queue.append(node.right)
      }
    }
    result
  }

  // 938. Range Sum of BST
  def rangeSumBST(root: TreeNode, low: Int, high: Int): Int = {
    var result = 0

    def dfs(node: TreeNode): Unit = {
      if (node == null)
        return

      if (node.value >= low) dfs(node.left)
      if (node.value >= low && node.value <= high) result += node.value
      if (node.value <= high) dfs(node.right)
    }

    dfs(root)
    result
  }


  // 701. Insert into a Binary Search Tree
  def insertIntoBST(root: TreeNode, x: Int): TreeNode = {
    if (root == null) return TreeNode(x)
    var parent = root

    while (parent != null) {
      if (parent.value > x) {
        if (parent.left != null) parent = parent.left
        else {
          parent.left = TreeNode(x)
          return root
        }
      } else {
        if (parent.right != null) parent = parent.right
        else {
          parent.right = TreeNode(x)
          return root
        }
      }
    }

    root
  }

  // 270. Closest Binary Search Tree Value
  def closestValue(root: TreeNode, target: Double): Int = {
    def rec(node: TreeNode, currentMin: Int): Int = {
      val minDiff = Math.abs(currentMin - target)
      val currDiff = Math.abs(node.value - target)

      val min = if (minDiff == currDiff) Math.min(currentMin, node.value)
      else if (minDiff >= currDiff) node.value
      else currentMin

      if (node.left == null && node.right == null) {
        min
      } else {
        if (node.left != null && node.right != null) {
          if (node.value - target > 0) rec(node.left, min)
          else rec(node.right, min)
        } else if (node.left == null)
          rec(node.right, min)
        else
          rec(node.left, min)
      }
    }

    rec(root, root.value)
  }

  // 226. Invert Binary Tree
  def invertTree_2(root: TreeNode): TreeNode = {
    def dfs(node: TreeNode): Unit = {
      if (node == null) return

      if (node.left != null && node.right != null) {
        val temp = node.left
        node.left = node.right
        node.right = temp
      } else if (node.left != null) {
        node.right = node.left
        node.left = null
      } else if (node.right != null) {
        node.left = node.right
        node.right = null
      }
      dfs(node.left)
      dfs(node.right)
    }

    dfs(root)
    root
  }

  // 380. Insert Delete GetRandom O(1)
  class RandomizedSet {
    import collection.mutable

    val array = mutable.ArrayBuffer.empty[Int]
    val map = mutable.HashMap.empty[Int, Int]

    def insert(value: Int): Boolean = {
      if (map.contains(value)) false
      else {
        map.put(value, array.size)
        array += value
        true
      }
    }

    def remove(value: Int): Boolean = {
      if (!map.contains(value)) false
      else {
        val last = array.last
        val index = map(value)
        array(index) = last
        map(last) = index
        map.remove(value)
        array.dropRightInPlace(1)
        true
      }
    }

    def getRandom(): Int = array(scala.util.Random.nextInt(array.length))

  }

  // 381. Insert Delete GetRandom O(1) - Duplicates allowed
  class RandomizedCollection() {

    import util.Random
    import collection.mutable

    val map = mutable.HashMap.empty[Int, mutable.HashSet[Int]]
    val array = mutable.ArrayBuffer.empty[Int]

    def insert(value: Int): Boolean = {
      if (!map.contains(value)) map.put(value, mutable.HashSet.empty[Int])
      map(value) += array.length
      array += value
      map(value).size == 1
    }

    def remove(value: Int): Boolean = {
      if (!map.contains(value) || map(value).isEmpty) false
      else {
        val removeId = map(value).head
        map(value).remove(removeId)
        val last = array.last
        array(removeId) = last
        map(last) += removeId
        map(last) -= array.length - 1
        array.dropRightInPlace(1)
        true
      }
    }

    def getRandom(): Int = array(Random.nextInt(array.length))
  }

  // 235. Lowest Common Ancestor of a Binary Search Tree
  def lowestCommonAncestor_4(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var answer: TreeNode = null

    def dfs(node: TreeNode): (Boolean, Boolean) = {
      if (node == null) return (false, false)

      var left = false
      var right = false

      if (node == p) left = true
      else if (node == q) right = true

      val leftDfs = dfs(node.left)
      val rightDfs = dfs(node.right)

      if (leftDfs._1 || rightDfs._1) left = true
      if (leftDfs._2 || rightDfs._2) right = true

      if (left && right && answer == null) answer = node
      (left, right)
    }

    dfs(root)
    answer
  }

  // 110. Balanced Binary Tree
  def isBalanced(root: TreeNode): Boolean = {
    if (root == null) return true

    def height(node: TreeNode): Int =
      if (node == null) 0
      else 1 + Math.max(height(node.left), height(node.right))

    Math.abs(height(root.left) - height(root.right)) <= 1 && isBalanced(root.left) && isBalanced(root.right)
  }


  // 98. Validate Binary Search Tree
  def isValidBST(root: TreeNode): Boolean = {
    def dfs(node: TreeNode, low: Long, high: Long): Boolean = {
      if (node == null) true
      else if (node.value <= low || node.value >= high) false
      else dfs(node.left, low, node.value) && dfs(node.right, node.value, high)
    }

    dfs(root, Int.MinValue.toLong - 1, Int.MaxValue.toLong + 1)
  }

  // 543. Diameter of Binary Tree
  def diameterOfBinaryTree_2(root: TreeNode): Int = {
    var result = 0

    def dfs(node: TreeNode): Int = {
      if (node == null) return 0

      val left = if (node.left != null) dfs(node.left) + 1 else 0
      val right = if (node.right != null) dfs(node.right) + 1 else 0

      result = result.max(left + right)
      left.max(right)
    }

    dfs(root)

    result
  }

}
