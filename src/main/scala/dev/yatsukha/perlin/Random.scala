package dev.yatsukha.perlin

package object random {
  import scala.util.Random

  def gradientStream(rng: Random): Stream[(Double, Double)] =
    (rng.between(-1.0, 1.0), rng.between(-1.0, 1.0)) #:: gradientStream(rng)

}


