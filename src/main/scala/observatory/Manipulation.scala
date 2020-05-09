package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  private val memoized = scala.collection.mutable.Map.empty[GridLocation, Temperature]

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                     is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    gridLocation: GridLocation => {
      val temps = temperatures.par.map(makeGrid).map(_ (gridLocation))
      temps.sum / temps.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    gridLocation: GridLocation => makeGrid(temperatures)(gridLocation) - normals(gridLocation)
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    gridLocation: GridLocation =>
      memoized.get(gridLocation) match {
        case Some(temp: Temperature) => temp
        case None =>
          memoized += (gridLocation -> Visualization.predictTemperature(temperatures, gridLocation.getLocation))
          memoized(gridLocation)
      }
  }

}