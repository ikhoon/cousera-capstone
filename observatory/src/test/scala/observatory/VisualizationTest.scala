package observatory


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

}
