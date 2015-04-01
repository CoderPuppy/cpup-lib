package cpup.lib.module

import java.lang.annotation.Annotation

import com.typesafe.config.Config
import org.slf4j.Logger

case class ModuleType[T](implicit final val cla: Class[T]) {
	override def toString: String = s"ModuleType<${cla.getName}>"

	lazy val id = Option(cla.getAnnotation(classOf[ModuleID])).map(_.id).getOrElse(cla.getName)
	lazy val loadable = reasons.isEmpty
	lazy val reasons = {
		val reasons = cla.getAnnotations.toList.flatMap((a) => {
			ModuleLoader.canLoadHandlers.get(a.annotationType).toList.flatMap(_.asInstanceOf[(Annotation) => List[String]](a))
		})
		if(reasons.isEmpty) {
			try {
				cla.getConstructor(classOf[Config], classOf[Logger])
				List()
			} catch {
				case e: NoSuchMethodException => {
					List(s"no valid constructor (constructors: ${
						cla.getConstructors.map("(" + _.getParameterTypes.map(_.getSimpleName).mkString(", ") + ")").mkString(" | ")
					})")
				}
			}
		} else {
			reasons
		}
	}
}
