package dev.yatsukha

object Main extends App {
  if (args.length != 4) {
    println("args: rows columns ticks filename")
    System.exit(-1)
  }

  val (r, c, t, f) = try {
    (args(0).toInt, args(1).toInt, args(2).toInt, args(3))
  } catch {
    case _: Throwable => { 
      println("invalid args"); 
      System.exit(-1); 

      (() => ???)() // hackerman
    }
  }

  println(s"image resolution: ${c * t}x${r * t}")

  // ( ͡◉ ͜ʖ ͡◉)
  val gf = generators.gradientField(
    new scala.util.Random(System.currentTimeMillis), 
    (r + 1, c + 1)
  )

  val perlin = 
    (0 until (r * t))
      .map(_.toDouble / t.toDouble)
      .map(
        x =>
          (0 until (c * t))
            .map(_.toDouble / t.toDouble)
            .map(y => noise.noise(gf, (x, y)))
            // expand the average [0.3, 0.7] signal energy to full range
            .map(util.exaggerate(_)) 
            //.map(_ + 1) // move to [0, 2]
            //.map(_ / 2) // scale to [0, 1]
      )

  val image = new Image(perlin.head.length, perlin.length)

  for (x <- perlin.indices)
    for (y <- perlin.head.indices) {
      val color = 0.7 *
        util.marble(
          noise.turbulence(perlin, 32)(x, y)
        )(
          x.toDouble / perlin.length, y.toDouble / perlin.head.length
        )() + 0.3

      // sand colors
      image.set(x, y)((212.0 * color).toInt, (125.0 * color).toInt, (65.0 * color).toInt)
    }

  image.write(f)
  println(s"saved to $f")
}