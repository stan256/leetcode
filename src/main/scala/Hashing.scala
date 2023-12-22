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
}