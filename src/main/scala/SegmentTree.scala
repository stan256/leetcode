
object SegmentTree extends App {
  // position:
  //   left 2*n + 1
  //   right 2*n + 2
  // get parent (i - 1)/2
  // length of output array - next (if not equals to power of 2) power of 2 * 2 - 1

  def lengthOfSegmentTree(arrLength: Int): Int = {
    val height = (Math.log(arrLength) / Math.log(2)).toInt + 1
    val tree_nodes = Math.pow(2, height + 1).toInt
    tree_nodes
  }

  // -----qLow----low-----high----qHigh----- - full overlap
  // -----low----qlow-----high----qHigh----- - partial overlap
  // -----qLow---qHigh----low----high------- - no overlap

//  private val array = Array(18, 17, 13, 19, 15, 11, 20, 12, 33, 25)
//  val segmentTree = new MaxSegTree(array)
//  println(segmentTree.tree.mkString("Array(", ", ", ")"))
//  println(segmentTree.query(0, 4, 0, array.length - 1, 0))
//  println("Update: 13 -> 100")
//  segmentTree.update(0, 2, 100, 0, array.length)
//  println(segmentTree.tree.mkString("Array(", ", ", ")"))
//  println(segmentTree.query(0, 4, 0, array.length - 1, 0))


  private val array = new NumArray(Array(-2, 0, 3, -5, 2, -1))
  println(array.tree.mkString(", "))
  println(array.sumRange(0, 2))
}

class MaxSegTree(val array: Array[Int]) {
  val tree: Array[Int] = build(array)

  def query(qLow: Int, qHigh: Int, low: Int, high: Int, index: Int): Int = {
    if (qHigh < low || qLow > high)
      return Int.MinValue
    if (qLow <= low && qHigh >= high)
      return tree(index)
    val middle = low + (high - low) / 2
    Math.max(query(qLow, qHigh, low, middle, 2 * index + 1), query(qLow, qHigh, middle + 1, high, 2 * index + 2))
  }

  private def build(arr: Array[Int]): Array[Int] = {
    val tree = Array.ofDim[Int](4 * arr.length)

    def fill(start: Int, end: Int, index: Int): Unit =
      if (start == end)
        tree(index) = arr(start)
      else {
        val middle = start + (end - start) / 2
        fill(start, middle, index * 2 + 1)
        fill(middle + 1, end, index * 2 + 2)
        tree(index) = Math.max(tree(index * 2 + 1), tree(index * 2 + 2))
      }

    fill(0, arr.length - 1, 0)

    tree
  }

  def update(segmentTreeIndex: Int, arrayIndex: Int, value: Int, start: Int, end: Int): Unit =
    if (start == end) {
      tree(segmentTreeIndex) = value
      array(arrayIndex) = value
    } else {
      val middle = start + (end - start) / 2
      if (start <= arrayIndex && arrayIndex <= middle)
        update(2 * segmentTreeIndex + 1, arrayIndex, value, start, middle)
      else
        update(2 * segmentTreeIndex + 2, arrayIndex, value, middle + 1, end)
      tree(segmentTreeIndex) = Math.max(tree(2 * segmentTreeIndex + 1), tree(2 * segmentTreeIndex + 2))
    }
}

class NumArray(_nums: Array[Int]) {
  val tree = build(_nums)
  println(tree.mkString("Array(", ", ", ")"))

  def build(arr: Array[Int]): Array[Int] = {
    val tree = Array.ofDim[Int](4 * arr.length)

    def fill(left: Int, right: Int, index: Int): Unit = {
      if (left == right) tree(index) = arr(left)
      else {
        val middle = left + (right - left)/2
        fill(left, middle, 2 * index + 1)
        fill(middle + 1, right, 2 * index + 2)
        tree(index) = tree(2 * index + 1) + tree(2 * index + 2)
      }
    }
    fill(0, arr.length - 1, 0)
    tree
  }

  def query(index: Int, qLeft: Int, qRight: Int, left: Int, right: Int): Int = {
    if (qRight < left || qLeft > right)
      return 0
    if (qLeft <= left && qRight >= right)
      return tree(index)

    val middle = left + (right - left)/2
    query(index * 2 + 1, qLeft, qRight, left, middle) + query(index * 2 + 2, qLeft, qRight, middle + 1, right)
  }

  def sumRange(left: Int, right: Int): Int = {
    query(0, left, right, 0, _nums.length - 1)
  }
}

// 2080. Range Frequency Queries
class RangeFreqQuery(arr: Array[Int]) {
  private val tree = build()

  def build(): Array[Map[Int, Int]] = {
    val tree: Array[Map[Int, Int]] = Array.ofDim(4 * arr.length)

    def fill(left: Int, right: Int, index: Int): Unit = {
      if (left == right) tree(index) = Map(arr(left) -> 1)
      else {
        val middle = left + (right - left) / 2
        fill(left, middle, index * 2 + 1)
        fill(middle + 1, right, index * 2 + 2)
        val s1 = tree(index * 2 + 1).toSeq
        val s2 = tree(index * 2 + 2).toSeq
        tree(index) = (s1 ++ s2).foldLeft(Map.empty[Int, Int]) {
          (map, pair) =>
            if (map.contains(pair._1)) map + (pair._1 -> (map(pair._1) + pair._2))
            else map + pair
        }
      }
    }

    fill(0, arr.length - 1, 0)
    tree
  }

  def update(left: Int, right: Int, nodeIndex: Int, arrayIndex: Int, value: Int, prevValue: Int): Unit = {
    if (left == right) {
      val map = tree(nodeIndex)
      tree(nodeIndex) = map +
        (prevValue -> (map(prevValue) - 1)) +
        (value -> (map.getOrElse(value, 0) + 1))
      arr(arrayIndex) = value
    } else {
      val middle = left + (right - left) / 2
      if (left <= nodeIndex && nodeIndex <= middle)
        update(left, middle, nodeIndex * 2 + 1, arrayIndex, value, prevValue)
      else
        update(middle + 1, right, nodeIndex * 2 + 2, arrayIndex, value, prevValue)
      tree(nodeIndex) = merge(tree(nodeIndex * 2 + 1), tree(nodeIndex * 2 + 2)) // todo doesn't work ._.
    }
  }

  private def merge(m1: Map[Int, Int], m2: Map[Int, Int]): Map[Int, Int] =
    (m1.toSeq ++ m2.toSeq).foldLeft(collection.mutable.Map.empty[Int, Int]) {
      (map, pair) =>
        map.put(pair._1, map.getOrElse(pair._1, 0) + pair._2)
        map
    }.toMap

  private def subQuery(qLeft: Int, qRight: Int, left: Int, right: Int, index: Int, value: Int): Int = {
    if (qLeft > right || qRight < left) return 0
    if (qLeft <= left && qRight >= right) return tree(index).getOrElse(value, 0)
    val middle = left + (right - left) / 2
    subQuery(qLeft, qRight, left, middle, index * 2 + 1, value) +
      subQuery(qLeft, qRight, middle + 1, right, index * 2 + 2, value)
  }

  def query(left: Int, right: Int, value: Int): Int = {
    subQuery(left, right, 0, arr.length - 1, 0, value)
  }

}

/**
 * Your NumArray object will be instantiated and called as such:
 * val obj = new NumArray(nums)
 * val param_1 = obj.sumRange(left,right)
 */