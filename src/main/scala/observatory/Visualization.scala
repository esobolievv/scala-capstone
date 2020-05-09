package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.math.{max, min, round}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val temps = Future(
      for (lon <- 90 until -90 by -1; lat <- -180 until 180)
        yield {
          val c = interpolateColor(colors, predictTemperature(temperatures, Location(lon, lat)))
          Pixel(c.red, c.green, c.blue, 255)
        }
    )
    Image.apply(360, 180, Await.result(temps, 20 seconds).toArray)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    // according to the formula https://en.wikipedia.org/wiki/Inverse_distance_weighting
    val temps = temperatures.par

    def op(pair1: (Double, Double), pair2: (Double, Double)) = (pair1._1 + pair2._1, pair1._2 + pair2._2)

    lazy val reduce = temps.map { case (loc: Location, temp: Temperature) =>
      val idw = loc.calcIdw(location)
      (idw, idw * temp)
    }.reduce(op)

    // first check if we have exact location match, distance which is near to 1 km, otherwise interpolate
    val filtered = temps.map(pair => (pair._1.calcDist(location), pair._2)).filter(_._1 < 1)
    if (filtered.nonEmpty) filtered.minBy(_._1)._2 else reduce._2 / reduce._1
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val pointsSorted = points.toSeq.sortBy(_._1)

    val leftEnd = (Double.NegativeInfinity, pointsSorted.head._2)
    val rightEnd = (Double.PositiveInfinity, pointsSorted.last._2)
    val range: Seq[(Temperature, Color)] = leftEnd +: pointsSorted :+ rightEnd

    val value1: Seq[(Temperature, Color)] = range
      .sliding(2).toParArray
      .find(t => t.head._1 <= value && t.last._1 >= value)
      .get

    val (temp1, color1): (Temperature, Color) = value1.head
    val (temp2, color2): (Temperature, Color) = value1.last

    val fraction = if (math.abs(value - temp1) == Double.PositiveInfinity) 1 else (value - temp1) / (temp2 - temp1)

    Color(
      min(max(round(color1.red * (1 - fraction)) + round(color2.red * fraction), 0), 255).toInt,
      min(max(round(color1.green * (1 - fraction)) + round(color2.green * fraction), 0), 255).toInt,
      min(max(round(color1.blue * (1 - fraction)) + round(color2.blue * fraction), 0), 255).toInt
    )

  }

}

