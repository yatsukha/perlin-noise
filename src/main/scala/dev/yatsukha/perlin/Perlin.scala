package dev.yatsukha.perlin

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

object Perlin {
  import scala.util.Random
  import conversions._

  //TODO: def generateNoise

  def main(args: Array[String]): Unit = {
    if (args.length != 3) {
      println("args: rows columns ticks")
      return
    }

    var rows  = 0
    var cols  = 0
    var ticks = 0

    try {
      rows  = args(0).toInt
      cols  = args(1).toInt
      ticks = args(2).toInt
    } catch {
      case _: Throwable => { println("arguments should be numbers"); return }
    }

    val rnd = new Random(System.currentTimeMillis)

    val gradients = 
      (0 until rows).map(x => (0 until cols).map(y => (x, y)).zip(generators.gradientStream(rnd)))

    val noise = 
      (0 until ((rows - 1) * ticks))
        .map(_ / ticks.toDouble)
        .map(x => (0 until ((cols - 1) * ticks)).map(_ / ticks.toDouble).map(y => {
          val xBounds = (x.toInt, (x + 1).toInt)
          val yBounds = (y.toInt, (y + 1).toInt)

          val interpWeight = (x - xBounds._1.toDouble, y - yBounds._1.toDouble)

          (
            ((x - xBounds._1, y - yBounds._1) * gradients(xBounds._1)(yBounds._1)._2,
              (x - xBounds._2, y - yBounds._1) * gradients(xBounds._2)(yBounds._1)._2)
                .scale(interpWeight._1),
            ((x - xBounds._1, y - yBounds._2) * gradients(xBounds._1)(yBounds._2)._2,
              (x - xBounds._2, y - yBounds._2) * gradients(xBounds._2)(yBounds._2)._2)
                .scale(interpWeight._1)
          ).scale(interpWeight._2)
          
        }))

    def merge(noise: Seq[Seq[Double]])(point: (Int, Int), scale: Int): Double = {
      var counter = scale
      var value = 0.0

      while (counter != 0) {
        value = value + (noise(point._1 / counter)(point._2 / counter) + 1.0) * counter.toDouble
        counter = counter / 2
      }

      value / scale.toDouble
    }

    var extremes = (-1.0, 1.0)
    val img = new BufferedImage(noise.head.length, noise.length, BufferedImage.TYPE_INT_RGB)
  
    for (x <- noise.indices)
      for (y <- noise(x).indices) {
        val n = noise(x)(y)
        extremes = (extremes._1 max n, extremes._2 min n)
        val clr = (200.0 *  merge(noise)((x, y), 64)).toInt - 256
        //println(clr)
        if (clr < 0 || clr > 255) {
          println(clr)
          return
        }
        img.setRGB(y, x, new Color(clr, clr, clr).getRGB)
      }

    ImageIO.write(img, "jpg", new File("test.jpg"))
    println(s"extremes: $extremes")
  }
}




