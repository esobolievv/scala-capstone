package observatory

import com.sksamuel.scrimage.Image
import org.junit.Assert._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {

  lazy private val colors: Iterable[(Temperature, Color)] = Seq(
    (100.0, Color(255, 255, 255)),
    (50.0, Color(0, 0, 0)),
    (0.0, Color(255, 0, 128)))

  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  @Test
  def testPredictTemperature(): Unit = {
    val temp = Visualization.predictTemperature(avarageTempPerLocation, Location(10.24, 10.5))
    assertEquals(-3.02, temp, 0.009)
  }


  @Test
  def testVisualize(): Unit = {
    val image: Image = Visualization.visualize(avarageTempPerLocation, colors)
    assertTrue(image.pixels.length == 360 * 180)
  }

  @Test
  def testInterpolateColor(): Unit = {
    assert(Visualization.interpolateColor(colors, 50.0) == Color(0, 0, 0))
    assert(Visualization.interpolateColor(colors, 0.0) == Color(255, 0, 128))
    assert(Visualization.interpolateColor(colors, -10.0) == Color(255, 0, 128))
    assert(Visualization.interpolateColor(colors, 200.0) == Color(255, 255, 255))
    assert(Visualization.interpolateColor(colors, 75.0) == Color(128, 128, 128))
    assert(Visualization.interpolateColor(colors, 25.0) == Color(128, 0, 64))
  }


}
