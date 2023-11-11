object General extends App {
  // 704. Binary Search
  def binarySearch(arr: Array[Int], n: Int): Int = {
    var left = 0
    var right = arr.length - 1

    while (left <= right) {
      val middleIndex = (left + right)/2

      val middle = arr(middleIndex)
      if (n == middle)
        return middleIndex
      else if (n > middle)
        left = middleIndex + 1
      else
        right = middleIndex - 1
    }

    -1
  }

  println(binarySearch(Array(1, 3, 4, 5, 8, 12, 14, 19, 25, 44, 55, 66, 67, 90, 119), 14))
  println(binarySearch(Array(-1,0,3,5,9,12), 12))


}
