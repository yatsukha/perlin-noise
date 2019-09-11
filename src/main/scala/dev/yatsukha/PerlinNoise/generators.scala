package dev.yatsukha

package object generators {
  import scala.util.Random

  def gradientStream(rng: Random): Stream[(Double, Double)] = {
    val x = rng.between(-1.0, 1.0)
    var y = rng.between(-1.0, 1.0)

    val len = math.sqrt(x * x + y * y)

    // avoid corner bias
    if (len > 1.0)
      gradientStream(rng)
    else
      (x / len, y / len) #:: gradientStream(rng)
  }

  // guaranteed O(1) indexed access
  def gradientField(rng: Random, dim: (Int, Int))
    : IndexedSeq[IndexedSeq[(Double, Double)]] = {

    val stream = gradientStream(rng)
    Vector((for (i <- 0 until dim._1) yield stream.drop(i * dim._2).take(dim._2).toVector): _*)
  }
  
}