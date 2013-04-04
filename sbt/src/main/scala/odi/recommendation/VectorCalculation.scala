package odi.recommendation

trait VectorCalculation {
  def cosinusSimilarity(vector1: Vector[Double], vector2: Vector[Double]): Double = {
    require(vector1.length == vector2.length, " Vector must be of same length")
    val numerator = vectorProduct(vector1, vector2)
    val denominator = vectorLength(vector1) * vectorLength(vector2)
    numerator / denominator
  }

  def vectorProduct(vector1: Vector[Double], vector2: Vector[Double]): Double = {
    vector1.zip(vector2).foldLeft(0.0)((result: Double, current: (Double, Double)) => result + current._1*current._2)
  }

  def vectorLength(vector: Vector[Double]): Double = {
    Math.sqrt(vector.map(Math.pow(_, 2)).sum)
  }
}
