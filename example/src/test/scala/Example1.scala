import javax.annotation.Nonnull

/**
 * @author Adam Warski (adam at warski dot org)
 */

object Example1 {
  def parameterMustBeNull(@Nonnull parameter : Object)(param2 : Object): Int = {
    println("Shouldn't get here!")
    1
  }

  def main(args: Array[String]) {
    parameterMustBeNull(null)(null)
  }

  def test(@Nonnull param: Object) {
    if (param == null) {
      throw new IllegalArgumentException("param ")
    }
    3
  }
}