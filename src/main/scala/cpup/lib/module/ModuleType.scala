package cpup.lib.module

import java.lang.annotation.Annotation

import com.typesafe.config.Config
import org.slf4j.Logger

case class ModuleType[T](implicit final val cla: Class[T]) {
	override def toString: String = s"ModuleType<${cla.getName}>"

	lazy val id = Option(cla.getAnnotation(classOf[ModuleID])).map(_.id).getOrElse(cla.getName)
	lazy val loadable = reasons.isEmpty
	lazy val constructor = {
		val constructors = cla.getConstructors.filter(_.getParameterTypes.forall(c => c.isAssignableFrom(classOf[Config]) || c.isAssignableFrom(classOf[Logger])))
		if(constructors.isEmpty)
			None
		else
			Some(constructors.head)
	}
	lazy val reasons = cla.getAnnotations.toList.flatMap((a) => {
		ModuleLoader.canLoadHandlers.get(a.annotationType).toList.flatMap(_.asInstanceOf[(Annotation) => List[String]](a))
	}) ++ (constructor match {
		case Some(c) => List()
		case None => List(s"no valid constructor, options: ${cla.getConstructors.map(c => s"(${c.getParameterTypes.map(_.getName).mkString(", ")})").mkString(" | ")}")
	})
}
