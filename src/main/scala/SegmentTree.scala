
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

  def build(arr: Array[Int]): Array[Int] = {
    val tree = Array.ofDim[Int](4 * arr.length)

    def fill(index: Int, left: Int, right: Int): Unit = {
      if (left == right) tree(index) = arr(left)
      else {
        val middle = left + (right - left)/2
        fill(2 * index + 1, left, middle)
        fill(2 * index + 2, middle + 1, right)
        tree(index) = tree(2 * index + 1) + tree(2 * index + 2)
      }
    }
    fill(0, 0, arr.length - 1)

    tree
  }

  // -----qLow----low-----high----qHigh----- - full overlap
  // -----low----qlow-----high----qHigh----- - partial overlap
  // -----qLow---qHigh----low----high------- - no overlap

  def query(tree: Array[Int], qLow: Int, qHigh: Int, low: Int, high: Int, index: Int): Int = {
    if (qHigh < low || high < qLow)
      return 0
    if (qLow <= low && qHigh >= high)
      return tree(index)
    val mid = low + (high - low)/2
    query(tree, qLow, qHigh, low, mid, 2 * index + 1) + query(tree, qLow, qHigh, mid + 1, high, 2 * index + 2)
  }

  private val arr = Array(18, 17, 13, 19, 15, 11, 20, 12, 33, 25)
  private val tree: Array[Int] = build(arr)
  println(query(tree, 1, 2, 0, arr.length, 0))
  println(tree.mkString("Array(", ", ", ")"))
}