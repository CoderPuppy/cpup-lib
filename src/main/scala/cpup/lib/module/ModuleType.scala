package cpup.lib.module

import java.lang.annotation.Annotation

import com.typesafe.config.Config
import org.slf4j.Logger

case class ModuleType[T](final val cla: Class[T]) {
	override def toString: String = s"ModuleType<${cla.getName}>"

	lazy val id = Option(cla.getAnnotation(classOf[ModuleID])).map(_.id).getOrElse(cla.getName)
	lazy val multi = Option(cla.getAnnotation(classOf[MultiModule])).map(_.clazzs.filter(cla.isAssignableFrom).asInstanceOf[Array[Class[T]]])
}
