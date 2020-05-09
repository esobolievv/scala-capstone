package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {

  namedMilestoneTest("data extraction", 1) _

  @Test
  def testHappyPath(): Unit = {
    assertEquals(36, temperatures.size)
    assertEquals(2, avarageTempPerLocation.size)
  }

}
