package observatory


import com.sksamuel.scrimage.{Pixel, RGBColor}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  import Visualization._
  test("interpolate color min") {

    val expected =  Color(255,0,0)
    val scale = List((1.0,Color(255,0,0)), (17.38421216002193,Color(0,0,255)))
    val value = 1.0
   check(interpolateColor(scale, value) == expected)
  }

  test("interpolate color max") {

    val expected =  Color(0,0,255)
    val scale = List((1.0,Color(255,0,0)), (17.38421216002193,Color(0,0,255)))
    val value = 17.38421216002193
    check(interpolateColor(scale, value) == expected)
  }
  test("interpolate color middle") {

    val scale = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    val expected =  Color(128,0,128)
    val value = 1.0737418235E9
    check(interpolateColor(scale, value) == expected)
  }

  test("interpolate color middle 2") {
    val expected = Color(128,0,128)
    val scale = List((-2.147483648E9,Color(255,0,0)), (0.0,Color(0,0,255)))
    val value = -1.073741824E9
    check(interpolateColor(scale, value) == expected)
  }

  test("interpolate color overflow") {
    val expected = Color(0,0,255)
    val scale = List((-2.147483648E9,Color(255,0,0)), (0.0,Color(0,0,255)))
    val value = 2.0
    check(interpolateColor(scale, value) == expected)
  }

  test("interpolate color underflow") {

    val expected =  Color(255,0,0)
    val scale = List((1.0,Color(255,0,0)), (17.38421216002193,Color(0,0,255)))
    val value = -17.38421216002193
    check(interpolateColor(scale, value) == expected)
  }

  test("distance") {
    val x = Location(0, 0)
    val y = Location(2, 2)
    val expected = 314
    check(distance(x, y).toInt == expected)
  }

  test("predict temperature") {
    val temperature = Seq((Location(0.0,0.0),10.0))
    val location = Location(90.0, -180.0)
    val expected = 10.0
    check(predictTemperature(temperature, location) == expected)
  }

  test("pixelize") {
    val temperatures = Seq(
      (Location(1.0,-90.0),79.00817904343145),
      (Location(-1.0,0.0),11.153706043181472)
    )

    val colors = Seq(
      (79.00817904343145,Color(255,0,0)),
      (11.153706043181472,Color(0,0,255))
    )

    val pixels = pixelize(360, 4, temperatures, colors)
    pixels.filter(_._3.argb != 0).foreach(println)

    println("### "  + pixels(360 * 3 + 180))
    check(pixels(360 * 1 + 90)._3.argb != 0)
    check(pixels(360 * 3 + 180)._3.argb != 0)
  }

  test("visualize") {
    val temperatures = Seq(
      (Location(1.0,-90.0),79.00817904343145),
      (Location(-1.0,0.0),11.153706043181472)
    )

    val colors = Seq(
      (79.00817904343145,Color(255,0,0)),
      (11.153706043181472,Color(0,0,255))
    )
    println(visualize(temperatures, colors))
    visualize(temperatures, colors).output("target/visualize.png")
    visualize(temperatures, colors).foreach((x, y, p) => if(p.argb != 0) println(s"$x, $y, ${p.toColor}"))

  }

  test("pixel") {
    val pixel1 = Pixel(1, 2, 3, 0)
    val pixel2 = Pixel(1, 2, 3, 1)
    val pixel3 = Pixel(1, 2, 3, 254)
    val pixel4 = Pixel(1, 2, 3, 100)
    val pixel5 = Pixel(1, 2, 3, 500)
    println(pixel1)
    println(pixel2)
    println(pixel3)
    println(pixel4)
    println(pixel5)
    printf("%x", (255 << 24))
  }

  test("visualize 2") {
    val temperatures = Seq(
      (Location(45.0,-90.0),0.0),
      (Location(-45.0,0.0),62.38972210851827)
    )
    val colors = Seq(
      (0.0,Color(255,0,0)),
      (62.38972210851827,Color(0,0,255))
    )
    visualize(temperatures, colors).foreach((x, y, p) => if(p.argb != 0) println(s"$x, $y, ${p.toColor}"))
    println(visualize(temperatures, colors))
  }

  test("visualize 3") {

    val t = Seq(
      (Location(45.0,-90.0),-60.74435267469656),
      (Location(-45.0,0.0),26.43807771123359))

    val c = Seq(
      (-60.74435267469656,Color(255,0,0)),
      (26.43807771123359,Color(0,0,255)))

    val expected = RGBColor(255, 0, 0, 255)
    val wrong = RGBColor(0, 0, 255, 255)
    val img = visualize(t, c)
    img.output("target/visualize3.png")
    img.foreach((x, y, p) =>
      if(x == 0 && y == 0) {
        println(p.toColor)
        val argb = p.toColor.toARGBInt
        check(argb - expected.toARGBInt < argb - wrong.toARGBInt)
      }
    )
  }
  test("visualized 4") {

    val t = Seq(
      (Location(45.0,-90.0),-1.0),
      (Location(-45.0,0.0),-73.29083592230589)
    )

    val c = Seq(
      (-1.0,Color(255,0,0)),
        (-73.29083592230589,Color(0,0,255))
    )

    val expected = RGBColor(255, 0, 0, 255)
    val wrong = RGBColor(0, 0, 255, 255)
    val img = visualize(t, c)
    img.output("target/visualize4.png")
    img.foreach((x, y, p) =>
      if(x == 0 && y == 0) {
        println(p.toColor)
        val argb = p.toColor.toARGBInt
        check(argb - expected.toARGBInt < argb - wrong.toARGBInt)
      }
    )

  }
}
