package cpup.lib.module

import java.lang.annotation.Annotation
import scala.language.existentials

import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}

case class ModuleSpec[T <: AnyRef](final val typ: ModuleType[T], private final val _parent: ModuleSpec[_ <: AnyRef], private final val _provide: Boolean = false) {
	override def toString = s"#ModuleSpec<$id>"

	override def hashCode = typ.hashCode * (if(parent eq this) 1 else parent.hashCode)
	override def equals(o: scala.Any) = o match {
		case o: ModuleSpec[_] => {
			o.typ == typ && (((o.parent eq o) && (parent eq this)) || o.parent == parent)
		}
		case _ => false
	}

	private lazy val _id: String = s"${parent.id}/${typ.id}"
	def id = _id
	def parent = _parent
	def provide = _provide
	private lazy val _config = if(parent.config.hasPath(typ.id)) parent.config.getConfig(typ.id) else ConfigFactory.empty
	def config: Config = _config
	final val enabled = !config.hasPath("enabled") || config.getBoolean("enabled")
	private lazy val _reasons = typ.cla.getAnnotations.toList.flatMap((a) => {
		ModuleLoader.canLoadHandlers.get(a.annotationType).toList.flatMap(_.asInstanceOf[(ModuleSpec[_], Annotation) => List[String]](this, a))
	}) ++ (constructor match {
		case Some(c) => List()
		case None => List(s"no valid constructor, options: ${typ.cla.getConstructors.map(c => s"(${c.getParameterTypes.map(_.getName).mkString(", ")})").mkString(" | ")}")
	}) ++ (if(enabled) List() else List("disabled"))
	def reasons = _reasons
	private lazy val _logger = LoggerFactory.getLogger(id)
	def logger = _logger

	private lazy val _constructor = typ.cla.getConstructors.find(_.getParameterTypes.forall(c => {
//		println(ModuleLoader.providedModule(this, ModuleLoader.moduleType(c.asInstanceOf[Class[_ <: AnyRef]])))
		c.isAssignableFrom(classOf[Config]) ||
			c.isAssignableFrom(classOf[Logger]) ||
			ModuleLoader.providedModule(this, ModuleLoader.moduleType(c.asInstanceOf[Class[_ <: AnyRef]]))._1.isDefined
	}))
	def constructor = _constructor

	private lazy val _parents = {
		var _parents = List[ModuleSpec[_ <: AnyRef]]()
		var _parent = parent
		while(_parent.parent != _parent) {
			_parent = _parent.parent
			_parents ++= List(_parent)
		}
		_parents
	}
	def parents = _parents
}
