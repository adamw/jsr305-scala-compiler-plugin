import javax.annotation.Nonnull

/**
 * @author Adam Warski (adam at warski dot org)
 */
object Example1 {
  def parameterMustBeNull(@Nonnull parameter : Object)(param2 : Object): Int = {
    // Here an if should be generated, which checks if the parameter is null.
    println("Shouldn't get here!")
    1
  }

  def main(args: Array[String]) {
    parameterMustBeNull(null)(null)
  }
}