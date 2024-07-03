
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
      val middle = (top + bottom) / 2
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
    var (left, right) = (0, nums.length - 1)
    var middle = (left + right) / 2

    while (nums(left) > nums(right)) {
      val middleElement = nums(middle)
      val leftElement = nums(left)

      if (middleElement < leftElement)
        right = middle
      else
        left = middle + 1

      middle = (left + right) / 2
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
      map.get(key).fold("") {
        list => {
          var (left, right) = (0, list.size - 1)

          var result = ""
          while (left <= right) {
            val middle = (left + right) / 2

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
      val mid = (right - left) / 2 + left
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
      val mid = (left + right) / 2
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
      val mid = (left + right) / 2
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
      val middle = min + (max - min) / 2
      val ans = piles.foldLeft(0l)((a, b) => a + Math.ceil(b * 1.0 / middle).toInt)
      if (ans <= hours) max = middle - 1
      else min = middle + 1
    }

    min
  }

  /*
  *
  * 1 4
  * 2 10
  * */


  // 1631. Path With Minimum Effort
  // FIXME - did not complete
  def minimumEffortPath(heights: Array[Array[Int]]): Int = {
    var right = heights.flatten.max
    var left = 0

    while (left <= right) {
      val middle = left + (right - left) / 2
      val isPossible = check(heights, right)
      if (isPossible) right = middle - 1
      else left = middle + 1
    }

    left
  }


  private val xLeft = -1 -> 0
  private val xRight = 1 -> 0
  private val yDown = 0 -> -1
  private val yUp = 0 -> 1

  val directions = Set(xLeft, xRight, yDown, yUp)

  def check(heights: Array[Array[Int]], maxEffort: Int): Boolean = {
    case class Coord(row: Int, col: Int)

    var seen = Set.empty[Coord]
    val stack = collection.mutable.Stack.empty[Coord]

    seen = seen + Coord(0, 0)
    stack.push(Coord(0, 0))

    while (stack.nonEmpty) {
      val originalPoint = stack.pop()
      if (originalPoint.row == heights.length - 1 && originalPoint.col == heights(0).length - 1)
        return true

      for (direction <- directions) {
        val newX = originalPoint.col + direction._1
        val newY = originalPoint.row + direction._2

        def valid(row: Int, col: Int) = 0 <= row && row < heights.length && 0 <= col && col < heights(0).length

        val isValid = valid(newX, newY)
        val contains = seen.contains(Coord(newX, newY))
        if (isValid && !contains) {
          if (Math.abs(heights(newY)(newX) - heights(originalPoint.row)(originalPoint.col)) <= maxEffort) {
            seen = seen + Coord(newX, newY)
            stack.push(Coord(newX, newY))
          }
        }
      }
    }

    false
  }

  // println(minimumEffortPath(Array(Array(1,2,2),Array(3,8,2),Array(5,3,5))))

  // 1870. Minimum Speed to Arrive on Time
  def minSpeedOnTime(dist: Array[Int], hour: Double): Int = {
    val n = dist.length
    val maxSpeed = 10000000

    def inTime(speed: Int): Boolean = {
      var time = 0d
      for (i <- 0 until n - 1) {
        val timeToFinish = dist(i).toDouble / speed
        time += Math.ceil(timeToFinish)
      }
      time += dist(n - 1).toDouble / speed
      time <= hour
    }

    var left = 1
    var right = maxSpeed
    while (left < right) {
      val mid = (left + right) / 2
      if (inTime(mid)) right = mid
      else left = mid + 1
    }

    if (inTime(left)) left else -1
  }

  // 1283. Find the Smallest Divisor Given a Threshold
  def smallestDivisor(nums: Array[Int], threshold: Int): Int = {
    var left = 1
    var right = nums.max

    while (left <= right) {
      val middle = (left + right) / 2
      var intermediate = 0
      for (x <- nums) {
        intermediate += Math.ceil(x.toDouble / middle).toInt
      }
      if (intermediate <= threshold) right = middle - 1
      else left = middle + 1
    }

    left
  }

  // 557. Reverse Words in a String III
  def reverseWords_2(s: String): String = {
    def reverseWord(str: String): String = {
      var left = 0
      var right = str.length - 1
      val sb = new StringBuilder(str)

      while (left < right) {
        val temp = sb.charAt(right)
        sb.setCharAt(right, sb.charAt(left))
        sb.setCharAt(left, temp)
        left += 1
        right -= 1
      }

      sb.toString
    }

    s.split("\\s").map(reverseWord).mkString(" ")
  }

  // 557. Reverse Words in a String III
  def reverseWords(s: String): String = {
    var left, right = 0
    val arr = s.toCharArray

    while (right < s.length) {
      if (arr(right) == ' ' || right == s.length - 1) {
        val temp = right

        if (arr(right) == ' ')
          right = right - 1

        while (left < right) {
          val t = arr(left)
          arr(left) = arr(right)
          arr(right) = t
          left += 1
          right -= 1
        }

        left = temp + 1
        right = temp + 1
      } else {
        right += 1
      }
    }

    new String(arr)
  }

  // 917. Reverse Only Letters
  def reverseOnlyLetters(s: String): String = {
    var left = 0
    var right = s.length - 1
    val array = s.toCharArray

    while (left < right) {
      if (!s.charAt(left).isLetter || !s.charAt(right).isLetter) {
        if (!s.charAt(left).isLetter) left += 1
        if (!s.charAt(right).isLetter) right -= 1
      } else {
        val c = array(left)
        array(left) = array(right)
        array(right) = c
        left += 1
        right -= 1
      }
    }

    new String(array)
  }

  // 2540. Minimum Common Value
  def getCommon(nums1: Array[Int], nums2: Array[Int]): Int = {
    var i1, i2 = 0

    while (true) {
      if (nums1(i1) == nums2(i2))
        return nums1(i1)
      else {
        if (nums1(i1) > nums2(i2)) {
          i2 += 1
          if (i2 == nums2.length)
            return -1
        } else {
          i1 += 1
          if (i1 == nums1.length)
            return -1
        }
      }
    }

    -1
  }

  // 283. Move Zeroes
  def moveZeroes(nums: Array[Int]): Unit = {
    var reader = 0
    var writer = 0
    var counter = 0

    while (reader < nums.length) {
      if (nums(reader) == 0) {
        counter += 1
        reader += 1
      } else {
        nums(writer) = nums(reader)
        reader += 1
        writer += 1
      }
    }

    val offset = nums.length - counter
    for (i <- 0 until counter) {
      nums(offset + i) = 0
    }
  }

  // 2000. Reverse Prefix of Word
  def reversePrefix(word: String, ch: Char): String = {
    var start = 0
    var end = 0
    var position = -1

    val arr = word.toCharArray

    while (end < word.length && position == -1) {
      if (word.charAt(end) == ch)
        position = end
      end += 1
    }

    if (position != -1) {
      while (start < position) {
        val temp = arr(start)
        arr(start) = arr(position)
        arr(position) = temp
        start += 1
        position -= 1
      }
    }


    new String(arr)
  }

  // 209. Minimum Size Subarray Sum
  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    var left, right = 0
    var res = 0
    var sum = 0

    while (right < nums.length) {
      sum += nums(right)
      while (sum >= target) {
        sum -= nums(left)
        res = if (res == 0) right - left + 1 else Math.min(res, right - left + 1)
        left += 1
      }
      right += 1
    }

    res
  }
}
