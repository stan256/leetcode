
object Backtracking extends App {

  // 46. Permutations
  def permute(nums: Array[Int]): List[List[Int]] = {
    import collection.mutable

    var answer = Seq.empty[mutable.Seq[Int]]
    var curr = mutable.Buffer.empty[Int]

    def backtracking(): Unit = {
      if (curr.length == nums.length) {
        answer = answer :+ curr.clone()
      } else {
        for (num <- nums) {
          if (!curr.contains(num)) {
            val i = curr.length
            curr = curr.appended(num)
            backtracking()
            curr.remove(i)
          }
        }
      }
    }

    backtracking()
    answer.map(_.toList).toList
  }

  println(permute(Array(1,2,3)))
}
