package dev.yatsukha.perlin 

package object generators {
  import scala.util.Random

  def gradientStream(rng: Random): Stream[(Double, Double)] = {
    val x = rng.between(-1.0, 1.0)
    var y = math.sqrt(1 - x * x)

    if (rng.nextBoolean())
      y *= -1


    (x, y) #:: gradientStream(rng)
  }

}


