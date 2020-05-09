package observatory

import observatory.Interaction._
import org.junit.{Ignore, Test}

trait Visualization2Test extends MilestoneSuite {

  val colorScale = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  @Test
  @Ignore
  def generateTails(): Unit = {
    val avgTemperatures = Extraction
      .locationYearlyAverageRecords(Extraction
        .locateTemperatures(2015, "/stations.csv", "/2015.csv")
      )
    for (x <- 0 to 7; y <- 0 to 7) {
      println((x, y))
      tile(avgTemperatures, colorScale, Tile(3, x, y)).output(s"target/temperatures/2015/3/$x-$y.png")
    }
  }
}
