package dev.yatsukha

import perlin._

object Main {
  import scala.util.Random
  import perlin.conversions._

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
    val txt = Array('!', '~', '-', '\\', '_', '|', '/')

    val gradients = 
      (0 until rows).map(x => (0 until cols).map(y => (x, y)).zip(random.gradientStream(rnd)))

    val noise = 
      (0 until ((rows - 1) * ticks))
        .map(_ / ticks.toDouble)
        .map(x => (0 until ((cols - 1) * ticks)).map(_ / ticks.toDouble).map(y => {
          val xBounds = (x.toInt, (x + 1).toInt)
          val yBounds = (y.toInt, (y + 1).toInt)

          val interpWeight = (x - xBounds._1, y - yBounds._1)

          (
            ((x - xBounds._1, y - yBounds._1) * gradients(xBounds._1)(yBounds._1)._2,
              (x - xBounds._2, y - yBounds._1) * gradients(xBounds._2)(yBounds._1)._2)
                .scale(interpWeight._1),
            ((x - xBounds._1, y - yBounds._2) * gradients(xBounds._1)(yBounds._2)._2,
              (x - xBounds._2, y - yBounds._2) * gradients(xBounds._2)(yBounds._2)._2)
                .scale(interpWeight._1)
          ).scale(interpWeight._2)
          
        }))

    var extremes = (-1.0, 1.0)
  
    for (x <- noise.indices) {
      for (y <- noise(x).indices) {
        val n = noise(x)(y)
        extremes = (extremes._1 max n, extremes._2 min n)
        print(txt(((n + 1.0) * (txt.length.toDouble / 2.0)).toInt))
      }

      println()
    }

    println(s"extremes: $extremes")
  }
}




