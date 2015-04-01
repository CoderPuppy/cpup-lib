package cpup.lib.config

import java.io._

import com.typesafe.config.{ConfigValueType, ConfigFactory, Config}

class ConfigLoader(final val path: File, final val regener: (String) => Option[InputStream]) {
	private var _config: Config = null
	def config = _config
	def regen(name: String = "default") {
		val input = regener(name).getOrElse(throw new FileNotFoundException(s"invalid config default: $name, for: ${path.getName}"))
		pipe(input, new FileOutputStream(path))
		loadConfig
	}

	private def loadConfig {
		if(!path.exists) throw new FileNotFoundException(s"something broke with ${path.getAbsolutePath}")
		_config = ConfigFactory.parseFileAnySyntax(path)
	}

	private def pipe(is: InputStream, os: OutputStream) {
		var n = 0
		val buffer = Array.ofDim[Byte](1024)
		var continue = true
		while(continue) {
			n = is.read(buffer)
			if(n > -1)
				os.write(buffer, 0, n)
			else
				continue = false
		}
		os.close
	}

	def load {
		if(path.exists)
			loadConfig
		else
			regen()
		while(_config.hasPath("regenerate-config")) {
			val regenVal = _config.getValue("regenerate-config")
			regen(if(regenVal.valueType == ConfigValueType.STRING) {
				regenVal.unwrapped.asInstanceOf[String]
			} else "default")
		}
	}
}

object ConfigLoader {
	def apply(path: File, regen: (String) => Option[InputStream]): Config = {
		val loader = new ConfigLoader(path, regen)
		loader.load
		loader.config
	}
}
