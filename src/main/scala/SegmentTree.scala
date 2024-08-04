
object SegmentTree extends App {
  // position:
  //   left 2*n + 1
  //   right 2*n + 2
  // get parent (i - 1)/2
  // length of output array - next (if not equals to power of 2) power of 2 * 2 - 1


  def buildSegmentTree(data: Array[Int]): Array[Int] = {
    val tree = Array.ofDim[Int](nextPowerOf2(data.length) * 2 - 1)

    def fillTree(left: Int, right: Int, index: Int): Unit = {
      if (left == right) {
        tree(index) = data(left)
        return
      }

      val leftIndex = 2 * index + 1
      val rightIndex = 2 * index + 2
      val mid = left + (right - left)/2

      fillTree(left, mid, leftIndex)
      fillTree(mid + 1, right, rightIndex)
      tree(index) = Math.min(tree(leftIndex), tree(rightIndex))
    }

    fillTree(0, data.length - 1, 0)

    tree
  }

  def nextPowerOf2(n: Int): Int = {
    var value = n - 1
    value |= value >> 1
    value |= value >> 2
    value |= value >> 4
    value |= value >> 8
    value |= value >> 16
    value + 1
  }

  println(buildSegmentTree(Array(-1, 2, 4, 0)).mkString("Array(", ", ", ")"))
}
