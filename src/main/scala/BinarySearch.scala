


object BinarySearch extends App {

  // 704. Binary Search
  def binarySearch(arr: Array[Int], n: Int): Int = {
    var left = 0
    var right = arr.length

    while (left <= right) {
      val i = arr((left + right) / 2)
      if (i == n)
        return (left + right) / 2
      if (i > n)
        right = (left + right) / 2 - 1
      else
        left = (left + right) / 2 + 1
    }
    -1
  }

//  println(binarySearch(Array(1, 3, 4, 5, 8, 12, 14, 19, 25, 44, 55, 66, 67, 90, 119), 119))
//  println(binarySearch(Array(-1, 0, 3, 5, 9, 12), 12))


  // 74. Search a 2D Matrix
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    var (top, bottom) = (0, matrix.length - 1)

    while (top <= bottom) {
      val middle = (top + bottom)/2
      val middleRow = matrix(middle)

      (middleRow(0), middleRow(middleRow.length - 1)) match {
        case (a, b) if a <= target && target <= b =>
          return binarySearch(middleRow, target) != -1
        case (a, b) if a >= target && b >= target =>
          bottom = middle - 1
        case (a, b) if a <= target && b <= target =>
          top = middle + 1
      }
    }

    false
  }
//  println(searchMatrix(Array(Array(1,3,5,7), Array(10,11,16,20), Array(23,30,34,60)), 60))

  // 153. Find Minimum in Rotated Sorted Array
  def findMin(nums: Array[Int]): Int = {
    var (left, right) = (0, nums.length -1)
    var middle = (left + right)/2

    while(nums(left) > nums(right)) {
      val middleElement = nums(middle)
      val leftElement = nums(left)

      if (middleElement < leftElement)
        right = middle
      else
        left = middle + 1

      middle = (left + right)/2
    }
    nums(left)
  }
//  println(findMin(Array(5,1,2,3,4)))
//  println(findMin(Array(4,5,6,7,0,1,2)))
//  println(findMin(Array(3,1,2)))
//  println(findMin(Array(2,1)))
//  println(findMin(Array(1)))
}
