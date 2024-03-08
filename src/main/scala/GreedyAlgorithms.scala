object GreedyAlgorithms extends App {
  // 2126. Destroying Asteroids
  def asteroidsDestroyed(mass: Int, asteroids: Array[Int]): Boolean = {
    asteroids.sorted.foldLeft[Long](mass) { (mass, x) => if (mass < x) mass - x else mass + x } > 0
  }
}
