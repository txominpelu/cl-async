import com.viadeo.data.main.Macros
import com.viadeo.data.model.NetworkPhotoNews

object Test extends App {

  /*val a = Macros.hello({
    def f (a: Int, b: Int) = a + b
    val b = 2
    val c = 3
    val a = f(b + 1, c + 2)
  })*/

  def f (a: Int, b: Int) = a + b
  val b = 2
  val c = 3

  val a = Macros.hello({f(b + 1, c + 2)})

  import scala.reflect.runtime.{universe => ru}

  val runtimeMirror = ru.runtimeMirror(getClass().getClassLoader())

  class C { def x = 2 }

  val im = runtimeMirror.reflect(new C)
}