package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {

  namedMilestoneTest("data extraction", 1) _

  @Test
  def testHappyPath(): Unit = {
    val temperatures = Extraction.locateTemperatures(2015, temperaturesFile = "/2015.csv")
    assertEquals(36, temperatures.size)
    assertEquals(2, Extraction.locationYearlyAverageRecords(temperatures).size)
  }

}
