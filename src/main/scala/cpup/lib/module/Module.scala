package cpup.lib.module

import cpup.lib.logging.SubLogger
import org.slf4j.{Logger, LoggerFactory}

trait Module[I] {
	protected val _name = getClass.getSimpleName.replaceFirst("\\$$", "")
	def fullName: String = parent.map(_.fullName + "/").getOrElse("") + name
	def name: String = _name
	def parent: Option[Module[_]] = None
	def logger: Logger = parent.map((parent) => {
		new SubLogger(name, parent.logger)
	}).getOrElse(LoggerFactory.getLogger(name))
	def get: I
	def canLoad: CanLoad = CanLoad(true)
	def load {
		if(!canLoad.toBoolean) {
			throw new RuntimeException(s"Attempt to load an unloadable module: $fullName reason: " + canLoad.messages.mkString(", "))
		}
	}
}