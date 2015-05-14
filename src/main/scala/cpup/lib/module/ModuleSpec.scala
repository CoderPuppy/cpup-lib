package cpup.lib.module

import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory

case class ModuleSpec[T <: AnyRef](final val typ: ModuleType[T], private final val _parent: ModuleSpec[_ <: AnyRef]) {
	override def toString = s"#ModuleSpec<$id>"

	private lazy val _id: String = s"${parent.id}/${typ.id}"
	def id = _id
	def parent = _parent
	private lazy val _config = if(parent.config.hasPath(typ.id)) parent.config.getConfig(typ.id) else ConfigFactory.empty
	def config: Config = _config
	final val enabled = !config.hasPath("enabled") || config.getBoolean("enabled")
	private lazy val _reasons = if(enabled) {
		typ.reasons
	} else {
		typ.reasons ++ List("disabled")
	}
	def reasons = _reasons
	private lazy val _logger = LoggerFactory.getLogger(id)
	def logger = _logger
}
