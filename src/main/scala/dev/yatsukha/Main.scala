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

      (() => ???)() // bet you can't figure this out
    }
  }

  println(s"image resolution: ${c * t}x${r * t}")
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
            //.map(util.exaggerate(_)) 
            .map(_ + 1) // move to [0, 2]
            .map(_ / 2) // scale to [0, 1]
      )

  val image = new Image(perlin.head.length, perlin.length)

  for (x <- perlin.indices)
    for (y <- perlin.head.indices) {
      val color = // stonelike structure generation
        (
          (0.85 * math.abs(
            2 * util.marble(
              noise.turbulence(perlin, 32)(x, y)
            )(
              x.toDouble / perlin.length, y.toDouble / perlin.head.length
            )(
              20.0, (10.0, 20.0)
            ) 
          - 1.0) + 0.15) * 255.9
        ).toInt

      image.set(x, y)(color, color, color)
    }

  try {
    image.write(f)
    println(s"saved to $f")
  } catch {
    case _: Throwable => println("invalid filename")
  }
}