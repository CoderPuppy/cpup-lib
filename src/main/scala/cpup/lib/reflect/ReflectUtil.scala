package cpup.lib.reflect

import scala.reflect.runtime.{universe => ru}

object ReflectUtil {
	def tpe[T](obj: T) = ru.runtimeMirror(obj.getClass.getClassLoader).staticClass(obj.getClass.getName).selfType
}
