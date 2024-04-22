import scala.collection.mutable


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

  // 33. Search in Rotated Sorted Array
  def search(nums: Array[Int], target: Int): Int = {
    var (left, right) = (0, nums.length - 1)

    while (left <= right) {
      val middle = (left + right) / 2
      if (nums(middle) == target)
        return middle

      /*
      4 options where can be the target:                   Searching in:
      left part is sorted & the item is there              left part
      left part is sorted & the item is in right part      right part
      right part is sorted & the item is there             right part
      right part is sorted & the item is in left part      left part
      * */

      if ((nums(left) <= target && target < nums(middle)) || (nums(middle) < nums(right) && (nums(middle) > target || nums(right) < target))) {
        right = middle - 1
        if (right < 0)
          return -1
      } else {
        left = middle + 1
        if (left >= nums.length)
          return -1
      }

    }

    -1
  }
//  println(search(Array(3,4,5,6,7,8,1,2), 2))
//  println(search(Array(6,7,1,2,3,4,5), 6))
//  println(search(Array(4,5,6,7,0,1,2), 0))
//  println(search(Array(4,5,6,7,0,1,2), 3))
//  println(search(Array(0,1,2), 2))
//  println(search(Array(4,5,6,7,0,1,2), 4))
//  println(search(Array(4,5,6,7,0,1,2), 2))
//  println(search(Array(1,2), 2))
//  println(search(Array(2,1), 2))
//  println(search(Array(2), 2))

  // 981. Time Based Key-Value Store
  class TimeMap() {
    val map = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.Seq[(Int, String)]]

    def set(key: String, value: String, timestamp: Int): Unit = {
      val set = map.getOrElse(key, scala.collection.mutable.Seq.empty[(Int, String)])
      map.put(key, set :+ (timestamp, value))
    }

    def get(key: String, timestamp: Int): String =
      map.get(key).fold(""){
        list => {
          var (left, right) = (0, list.size - 1)

          var result = ""
          while (left <= right) {
            val middle = (left + right)/2

            if (list(middle)._1 <= timestamp) {
              result = list(middle)._2
              left = middle + 1
            } else {
              right = middle - 1
            }
          }
          println(result)
          result
        }
      }
  }

  // 2300. Successful Pairs of Spells and Potions
  def successfulPairs(spells: Array[Int], potions: Array[Int], success: Long): Array[Int] = {
    val sortedPotions = potions.sorted
    var arr = Array.empty[Int]

    for (spell <- spells) {
      val threshold = 1.0 * success / spell
      val i = search(sortedPotions, threshold)
      arr = arr :+ potions.length - i
    }

    arr
  }

  def search(arr: Array[Int], i: Double): Int = {
    var left = 0
    var right = arr.length - 1

    while (left <= right) {
      val mid = (right - left)/2 + left
      if (arr(mid) < i) left = mid + 1
      else right = mid - 1
    }

    left
  }

  // 35. Search Insert Position
  def searchInsert(nums: Array[Int], target: Int): Int = {
    var left = 0
    var right = nums.length - 1

    while (left <= right) {
      val mid = (left + right)/2
      if (nums(mid) == target) return mid
      else if (nums(mid) > target) right = mid - 1
      else left = mid + 1
    }

    left
  }

  // 2389. Longest Subsequence With Limited Sum
  def answerQueries(nums: Array[Int], queries: Array[Int]): Array[Int] = {
    val preSum = nums.sorted.foldLeft(Array.empty[Int])((arr, x) => arr :+ arr.lastOption.getOrElse(0) + x)
    var answer = Array.empty[Int]
    for (q <- queries) {
      val i = checkInsert(preSum, q)
      println(i)
      answer = answer :+ i
    }

    answer
  }

  def checkInsert(nums: Array[Int], target: Int): Int = {
    var left = 0
    var right = nums.length - 1

    while (left <= right) {
      val mid = (left + right)/2
      if (nums(mid) > target) right = mid - 1
      else left = mid + 1
    }

    left
  }

  // 875. Koko Eating Bananas
  def minEatingSpeed(piles: Array[Int], hours: Int): Int = {
    var min = 1
    var max = piles.max

    while (min <= max) {
      val middle = min + (max-min)/2
      val ans = piles.foldLeft(0l)((a, b) => a + Math.ceil(b * 1.0 / middle).toInt)
      if (ans <= hours) max = middle - 1
      else min = middle + 1
    }

    min
  }
}
