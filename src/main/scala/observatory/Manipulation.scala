package observatory

import java.util.concurrent.ConcurrentHashMap
import java.util.function.Function


/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    memoize[GridLocation, Temperature] {
      gridLoc: GridLocation => Visualization.predictTemperature(temperatures, Location(gridLoc.lat, gridLoc.lon))
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val projectedTempFunctions = temperaturess map makeGrid
    gridLoc: GridLocation => {
      val yearTemperatures = (projectedTempFunctions map (_ (gridLoc))).toSeq
      yearTemperatures.sum / yearTemperatures.length
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature):
  GridLocation => Temperature = {
    val projectedTempFunction = makeGrid(temperatures)
    gridLoc: GridLocation => {
      val normalTemp = normals(gridLoc)
      val projectedTemp = projectedTempFunction(gridLoc)

      projectedTemp - normalTemp
    }
  }


  /**
    * Helper for memoization using Java's ConcurrentHashMap
    *
    * @param f Scala function to be memoized
    * @tparam I Input type of memoized function
    * @tparam O Output type of memoized function
    * @return A function that stores all results in a memoized ConcurrentHashMap
    */
  private def memoize[I, O](f: I => O): I => O = {
    val memory = new ConcurrentHashMap[I, O]()
    val javaFunction = new Function[I, O] {
      override def apply(key: I): O = f(key)
    }

    key: I => memory.computeIfAbsent(key, javaFunction)
  }

}