import scala.collection.mutable

object Arrays extends App {

  def twoSum_binarySearch(nums: Array[Int], target: Int): Array[Int] = {
    /*
    * add indices
    * sort by value
    * iterate though the array of tuples
    * for each x => 'search' y where x + y = target && x.index != y.index
    * use binary search for 'search'
    * use indices in tuple as response
    * */
    import scala.collection.Searching._

    val result = Array.fill(2)(0)
    val sortedWithIndices: Array[(Int, Int)] = nums.zipWithIndex.sortBy(_._1)

    sortedWithIndices.find(t => {
      val v = t._1
      val i = t._2

      // 2nd argument in search doesn't matter
      sortedWithIndices.search((target - v, -1))(Ordering.by(_._1)) match {
        case f@Found(fi) if sortedWithIndices(f.foundIndex)._2 != i => {
          result(0) = sortedWithIndices(fi)._2
          result(1) = t._2
          true
        }
        case Found(_) => false
        case InsertionPoint(_) => false
      }
    })

    result
  }
  //  println(twoSum(Array(3,2,3), 6).mkString("Array(", ", ", ")"))

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val arr = Array.ofDim[Int](2)
    val map = mutable.Map[Int, Seq[Int]]()

    for ((x, i) <- nums.zipWithIndex) {
      val diff = target - x
      val seq = map.getOrElse(diff, Seq[Int]())
      if (diff == x && seq.size > 1) {
        arr(0) = seq.head
        arr(1) = seq(1)
      } else if (seq.nonEmpty) {
        arr(0) = i
        arr(1) = seq.head
      } else {
        map.addOne(x, Seq(i))
      }
    }

    arr
  }

//  println(twoSum(Array(3,2,3), 6).mkString("Array(", ", ", ")"))
//  println(twoSum(Array(2, 15, 7, 11), 9).mkString("Array(", ", ", ")"))


  // 49. Group Anagrams
  def groupAnagrams(strs: Array[String]): List[List[String]] = {

    /*
    going through strs and mapping each element to entry: str -> sorted chars of it
    going through map and collecting it by sorted arrays
    * */

    val sorted = strs.map(str => str -> str.toCharArray.sorted.mkString)
    val map = mutable.Map[String, Seq[String]]()
    for ((a, b) <- sorted) {
      val value = map.getOrElse(b, Seq()) :+ a
      map.update(b, value)
    }

    map.values.toList.map(_.toList)
  }
//  println(groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))

}
