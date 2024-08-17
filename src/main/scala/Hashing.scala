
object Hashing extends App {
  // 1832. Check if the Sentence Is Pangram
  def checkIfPangram(sentence: String): Boolean = {
    val set = sentence.toSet
    ('a' to 'z').forall(set.contains)
  }
  //  println(checkIfPangram("thequickbrownfoxjumpsoverthelazydog"))
  //  println(checkIfPangram("leetcode"))

  // 268. Missing Number
  def missingNumber(nums: Array[Int]): Int = {
    val set = nums.toSet
    (0 to nums.length).find(x => !set(x)).get
  }
  //  println(missingNumber(Array(3,0,1)))

  def missingNumber_arithmeticProgression(nums: Array[Int]): Int = nums.length * (nums.length + 1) / 2 - nums.sum
  //  println(missingNumber_arithmeticProgression(Array(3,0,1)))

  // 1426. Counting Elements
  def countElements(arr: Array[Int]): Int = {
    val set = arr.toSet
    arr.fold[Int](0) { (r, x) => if (set(x + 1)) r + 1 else r }
  }
  //  println(countElements(Array(1,2,3)))
  //  println(countElements(Array(1,3, 5,5)))

  // 1426. Counting Elements
  def findWinners(matches: Array[Array[Int]]): List[List[Int]] = {
    val map = scala.collection.mutable.HashMap.empty[Int, Int]
    matches.foreach(x => {
      val looser = x(1)
      val winner = x(0)
      map.put(looser, map.getOrElse(looser, 0) + 1)
      if (!map.contains(winner)) map.put(winner, 0)
    })
    val haveNotLostAny = map.filter(x => x._2 == 0).keys.toList
    val lostOne = map.filter(x => x._2 == 1).keys.toList
    haveNotLostAny.sorted :: lostOne.sorted :: Nil
  }

  // println(findWinners(Array(Array(1, 3), Array(2, 3), Array(3, 6), Array(5, 6), Array(5, 7), Array(4, 5), Array(4, 8), Array(4, 9), Array(10, 4), Array(10, 9))))

  // 1133. Largest Unique Number
  def largestUniqueNumber(nums: Array[Int]): Int = nums.foldLeft(Map.empty[Int, Int])((acc, x) => acc.updated(x, acc.getOrElse(x, 0) + 1)).filter(_._2 == 1).keys.maxOption.getOrElse(-1)
  // println(largestUniqueNumber(Array(5,7,3,9,4,9,8,3,1)))

  // 1189. Maximum Number of Balloons
  def maxNumberOfBalloons(text: String): Int = {
    val chars = text.filter("balloon".contains(_)).foldLeft(Map.empty[Char, Int])((map, c) => map.updated(c, map.getOrElse(c, 0) + 1)).map {
      case (d, numb) if d == 'l' || d == 'o' => numb / 2
      case (_, numb) => numb
    }
    if (chars.size == "balloon".distinct.length)
      chars.min
    else
      0
  }
  // println(maxNumberOfBalloons("lloo"))
  // println(maxNumberOfBalloons("nlaebolko"))
  // println(maxNumberOfBalloons("loonbalxballpoon"))


  // 560. Subarray Sum Equals K
  def subarraySum(nums: Array[Int], k: Int): Int = {
    var count = 0
    var answer = 0
    val map = collection.mutable.HashMap.empty[Int, Int]
    map.put(0, 1)

    nums.foreach(i => {
      count += i
      answer += map.getOrElse(count - k, 0)
      map.put(count, map.getOrElse(count, 0) + 1)
    })

    answer
  }
  // println(subarraySum(Array(1,1,1), 2))

  // 1248. Count Number of Nice Subarrays
  def numberOfSubarrays(nums: Array[Int], k: Int): Int = {
    val map = collection.mutable.HashMap.empty[Int, Int]
    map.put(0, 1)
    var count = 0
    var answer = 0

    nums.foreach(x => {
      count += x % 2
      answer += map.getOrElse(count - k, 0)
      map.put(count, map.getOrElse(count, 0) + 1)
    })

    answer
  }

  // println(numberOfSubarrays(Array(1, 1, 2, 1, 1), 3))

  // 930. Binary Subarrays With Sum
  def numSubarraysWithSum(nums: Array[Int], goal: Int): Int = {
    var count, ans = 0
    val map = collection.mutable.HashMap.empty[Int, Int]
    map.put(0, 1)

    nums.foreach(x => {
      count += x
      ans += map.getOrElse(count - goal, 0)
      map.put(count, map.getOrElse(count, 0) + 1)
    })

    ans
  }
  // println(numSubarraysWithSum(Array(1,0,1,0,1), 2))
  // println(numSubarraysWithSum(Array(0,0,0,0,0), 0))

  // 525. Contiguous Array
  def findMaxLength(nums: Array[Int]): Int = {
    var count, ans = 0
    val map = collection.mutable.HashMap.empty[Int, Int]
    map.put(0, -1)

    nums.map { case 0 => -1; case _ => 1 }.zipWithIndex.foreach(x => {
      count += x._1
      if (map.contains(count)) {
        val len = x._2 - map(count)
        ans = Math.max(ans, len)
      } else {
        map.put(count, x._2)
      }
    })

    ans
  }

  //  println(findMaxLength(Array(0, 1, 0)))
  //  println(findMaxLength(Array(0, 1, 0, 1, 0)))
  //  println(findMaxLength(Array(0, 1)))

  // 383. Ransom Note
  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    var magazineMap = magazine.foldLeft(Map.empty[Char, Int])((map, x) => map + (x -> (map.getOrElse(x, 0) + 1)))
    ransomNote.forall(c => {
      magazineMap = magazineMap + (c -> (magazineMap.getOrElse(c, 0) - 1))
      magazineMap.get(c).exists(x => x >= 0)
    })
  }
  //  println(canConstruct("aa", "bb"))
  //  println(canConstruct("aa", "ab"))
  //  println(canConstruct("aab", "bbaa"))

  // 771. Jewels and Stones
  def numJewelsInStones(jewels: String, stones: String): Int = {
    val set = jewels.toSet
    stones.foldLeft(0)((counter, x) => if (set(x)) counter + 1 else counter)
  }
  //  println(numJewelsInStones("aA", "aAAbbbb"))
  //  println(numJewelsInStones("z", "ZZ"))

  case class ListNode(
                       val value: Int,
                       val key: Int,
                       var prev: ListNode = null,
                       var next: ListNode = null
                     )

  // 146. LRU Cache
  class LRUCache(_capacity: Int) {
    var head = ListNode(-1, -1)
    var tail = ListNode(-1, -1)
    val map = collection.mutable.HashMap.empty[Int, ListNode]
    head.next = tail
    tail.prev = head

    def add(node: ListNode): Unit = {
      val prevEnd = tail.prev
      prevEnd.next = node
      node.prev = prevEnd
      node.next = tail
      tail.prev = node
    }

    def remove(node: ListNode): Unit = {
      node.prev.next = node.next
      node.next.prev = node.prev
    }

    def get(key: Int): Int = {
      if (!map.contains(key)) return -1
      val node = map(key)
      remove(node)
      add(node)
      return node.value
    }

    def put(key: Int, value: Int): Unit = {
      if (map.contains(key)) remove(map(key))

      val node = ListNode(value, key)
      map.put(key, node)
      add(node)

      if (map.size > _capacity) {
        val r = head.next
        remove(r)
        map.remove(r.key)
      }
    }
  }
}
