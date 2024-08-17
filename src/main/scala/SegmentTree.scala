
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

  private val array = Array(18, 17, 13, 19, 15, 11, 20, 12, 33, 25)
  val segmentTree = new MaxSegTree(array)
  println(segmentTree.tree.mkString("Array(", ", ", ")"))
  println(segmentTree.query(0, 4, 0, array.length - 1, 0))
  println("Update: 13 -> 100")
  segmentTree.update(0, 2, 100, 0, array.length)
  println(segmentTree.tree.mkString("Array(", ", ", ")"))
  println(segmentTree.query(0, 4, 0, array.length - 1, 0))
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