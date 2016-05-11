package cpup.lib.reflect

import scala.reflect.runtime.{universe => ru}

object ReflectUtil {
	def tpe[T](cla: Class[T]) = ru.runtimeMirror(cla.getClassLoader).staticClass(cla.getName).selfType
	def annotation(tree: ru.Tree) = (tree.tpe, tree.children.tail.map { a =>
		val a_ = a.asInstanceOf[ru.AssignOrNamedArg]
		(a_.lhs.asInstanceOf[ru.Ident].name.toString, a_.rhs match {
			case l: ru.Literal => l.value.value
		})
	}.toMap)
	def findAnnotation[A](sym: ru.Symbol)(implicit tt: ru.TypeTag[A]) = {
		sym.annotations.map(_.tree).find(_.tpe =:= tt.tpe).map(ReflectUtil.annotation(_)._2)
	}
}
