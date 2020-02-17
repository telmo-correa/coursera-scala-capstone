package observatory

import org.junit.Assert._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  @Test def `Visualization can be instantiated`: Unit = {
    val instantiatable = try {
      Visualization
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a Visualization object")
  }

  @Test def `Visualization.predictTemperature works for sample data`: Unit = {
    val testData: Iterable[(Location, Temperature)] = Seq(
      (Location(10, 10), 20.0),
      (Location(20, 20), 10.0)
    )

    val expectedResults: Iterable[(Location, Temperature)] = Seq(
      (Location(10, 10), 20.0),
      (Location(20, 20), 10.0),
      (Location(15, 15), 14.94346697),
      (Location(-10, -10), 16.90043206)
    )

    for ((testLocation, expectedTemperature) <- expectedResults) {
      assertEquals(
        "Unexpected predicted temperature for " + testLocation,
        expectedTemperature,
        Visualization.predictTemperature(testData, testLocation),
        1e-8
      )
    }
  }

  @Test def `Visualization.interpolateColor works for sample data`: Unit = {
    val testColors: Iterable[(Temperature, Color)] = Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 0, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 5))
    )

    val expectedResults: Iterable[(Temperature, Color)] = Seq(
      (6, Color(128, 128, 128)),
      (0, Color(0, 0, 255)),
      (60, Color(255, 255, 255)),
      (61, Color(255, 255, 255)),
      (-60, Color(0, 0, 5)),
      (-61, Color(0, 0, 5))

    )

    for ((testTemperature, expectedColor) <- expectedResults) {
      assertEquals(
        "Unexpected interpolated color for " + testTemperature,
        expectedColor,
        Visualization.interpolateColor(testColors, testTemperature)
      )
    }
  }

  @Test def `Visualization.visualize works for sample data`: Unit = {
    val testTemperatures: Iterable[(Location, Temperature)] = Seq(
      (Location(0, 0), 50.0),
      (Location(20, 20), 10.0),
      (Location(50, -30), -20.0),
      (Location(-85, 5), -50.0),
      (Location(85, -5), -50.0)
    )

    val testColors: Iterable[(Temperature, Color)] = Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 0, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )

    val testWidth = 360
    val testHeight = 180
    val image = Visualization.visualize(testTemperatures, testColors)
    assertEquals("Unexpected image dimensions",
      (testWidth, testHeight), (image.width, image.height))

    //image.output(new java.io.File("target/test-image.png"))
  }
}
