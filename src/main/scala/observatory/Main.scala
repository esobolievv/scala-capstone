package observatory

object Main extends App {

  val path = "/1975.csv"
  val fullPath: String = getClass.getResource(path).getPath

  val locatedTemperatures = Extraction.locateTemperatures(2015, temperaturesFile = "/2015.csv")
  val locatedTemperatureEvr = Extraction.locationYearlyAverageRecords(locatedTemperatures)
}
