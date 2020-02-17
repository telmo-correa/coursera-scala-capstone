package observatory

import java.time.LocalDate

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  @Test def `Extraction can be instantiated`: Unit = {
    val instantiatable = try {
      Extraction
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a Extraction object")
  }

  @Test def `Extraction.locateTemperatures works for test year`: Unit = {
    val testTemperatures: Iterable[(LocalDate, Location, Temperature)] =
      Extraction.locateTemperatures(2050,
        "/test_stations.csv",
        "/test_year.csv"
      )

    val sampleData = testTemperatures.head
    assertEquals("Incorrect year in locate temperatures", 2050, sampleData._1.getYear)
  }

  @Test def `Extraction.locationYearlyAverageRecords works on sample data`: Unit = {
    val sampleData: Iterable[(LocalDate, Location, Temperature)] = Seq(
      (LocalDate.of(1975, 1, 1), Location(10, 20), 10.0),
      (LocalDate.of(1975, 1, 2), Location(10, 20), 15.0),
      (LocalDate.of(1975, 1, 3), Location(30, 30), 20.0)
    )

    val expectedResult: Iterable[(Location, Temperature)] = Seq(
      (Location(10, 20), 12.5),
      (Location(30, 30), 20.0)
    )
    val actualResult = Extraction.locationYearlyAverageRecords(sampleData)
    assertEquals(expectedResult, actualResult)
  }
}
