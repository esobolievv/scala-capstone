package observatory

import com.sksamuel.scrimage.{Image, Pixel}

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
    val (width, height) = (360, 180)
    val pixelsNumber = width * height
    val pixels: Array[Pixel] = new Array(pixelsNumber)

    for (i <- 0 until pixelsNumber) {
      val location = Location(90 - Math.round(Math.floor(i / width)), (i % width) - 180)
      val temperature = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      pixels(i) = Pixel(color.red, color.green, color.blue, 1)
    }

    Image(width, height, pixels)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    // according to the formula https://en.wikipedia.org/wiki/Inverse_distance_weighting
    def iDw(temperatures: Iterable[(Location, Temperature)], location: Location): Double = {
      def op(pair1: (Double, Double), pair2: (Double, Double)): (Double, Double) = (pair1._1 + pair2._1, pair1._2 + pair2._2)

      temperatures.par.map { case (loc, temp) =>
        val idw = loc.calcIdw(location)
        (temp * idw, idw)
      }.par.aggregate[(Double, Double)]((0.0, 0.0))(op, op) match {
        case (weightingSum, weighting) => weightingSum / weighting
      }
    }

    // first check if we have exact location match, distance which is near to 1 km, otherwise interpolate
    val res = temperatures.par
      .find(loc => loc._1 == location || loc._1.calcDist(location) < 1.0).map(_._2)
    res.getOrElse(iDw(temperatures, location))
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    def linearInterpolate(smaller: Option[(Temperature, Color)], greater: Option[(Temperature, Color)]): Color = (smaller, greater) match {
      case (Some((t1, Color(red1, green1, blue1))), Some((t2, Color(red2, green2, blue2)))) =>
        val ratio = (value - t1) / (t2 - t1)
        Color(math.round(red1 + (red2 - red1) * ratio).toInt,
          math.round(green1 + (green2 - green1) * ratio).toInt,
          math.round(blue1 + (blue2 - blue1) * ratio).toInt)
      case (Some(left), None) => left._2
      case (None, Some(right)) => right._2
      case _ => Color(0, 0, 0)
    }

    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None =>
        val (smaller, greater) = points.toList.sortBy(_._1).partition(_._1 < value)
        linearInterpolate(smaller.reverse.headOption, greater.headOption)
    }

  }

}

