package observatory

import scala.collection.parallel.ParMap

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  val MinLat: Int = -89
  val MaxLat: Int = 90
  val MinLon: Int = -180
  val MaxLon: Int = 179

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                     is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    gridLocation => {
      val temps = temperatures.par.map(makeGrid).map(_(gridLocation))
      temps.sum / temps.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val grid: ParMap[GridLocation, Double] = {
      for {
        lat <- MinLat to MaxLat
        lon <- MinLon to MaxLon
      } yield GridLocation(lat, lon)
    }.par
      .map(loc => loc -> Visualization.predictTemperature(temperatures, Location(loc.lat, loc.lon)))
      .toMap

    gridLocation => grid(gridLocation)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    gridLocation => makeGrid(temperatures)(gridLocation) - normals(gridLocation)
  }


}

