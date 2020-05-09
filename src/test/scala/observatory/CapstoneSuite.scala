package observatory

import java.time.LocalDate

import org.junit.Assert._

class CapstoneSuite
  extends ExtractionTest
    with VisualizationTest
    with InteractionTest
    with ManipulationTest
    with Visualization2Test
    with Interaction2Test

trait MilestoneSuite {

  lazy val temperatures: Iterable[(LocalDate, Location, Temperature)] = Extraction
    .locateTemperatures(2015, temperaturesFile = "/2015.csv")

  lazy val avarageTempPerLocation: Iterable[(Location, Temperature)] = Extraction
    .locationYearlyAverageRecords(temperatures)

  def namedMilestoneTest(milestoneName: String, level: Int)(block: => Unit): Unit =
    if (Grading.milestone >= level) {
      block
    } else {
      fail(s"Milestone $level ($milestoneName) is disabled. To enable it, set the 'Grading.milestone' value to '$level'.")
    }

}

