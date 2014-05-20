package cpup.lib.logging

import org.slf4j.{Marker, Logger}

case class SubLogger(name: String, parent: Logger) extends Logger {
	override def getName = s"${parent.getName}/$name"

	override def error(marker: Marker, msg: String, t: Throwable) {
		parent.error(marker, s"[$name] $msg", t)
	}

	override def error(marker: Marker, format: String, arguments: Object*) {
		parent.error(marker, s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def error(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.error(marker, s"[{}] $format", name, arg1, arg2)
	}

	override def error(marker: Marker, format: String, arg: Object) {
		parent.error(marker, s"[{}] $format", name: Any, arg: Any)
	}

	override def error(marker: Marker, msg: String) {
		parent.error(marker, s"[$name] $msg")
	}

	override def isErrorEnabled(marker: Marker) = parent.isErrorEnabled(marker)

	override def error(msg: String, t: Throwable) {
		parent.error(s"[$name] $msg", t)
	}

	override def error(format: String, arguments: Object*) {
		parent.error(s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def error(format: String, arg1: Object, arg2: Object) {
		parent.error(s"[{}] $format", name, arg1, arg2)
	}

	override def error(format: String, arg: Object) {
		parent.error(s"[{}] $format", name: Any, arg: Any)
	}

	override def error(msg: String) {
		parent.error(s"[$name] $msg")
	}

	override def isErrorEnabled = parent.isErrorEnabled

	override def warn(marker: Marker, msg: String, t: Throwable) {
		parent.warn(marker, s"[$name] $msg", t)
	}

	override def warn(marker: Marker, format: String, arguments: Object*) {
		parent.warn(marker, s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def warn(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.warn(marker, s"[{}] $format", name, arg1, arg2)
	}

	override def warn(marker: Marker, format: String, arg: Object) {
		parent.warn(marker, s"[{}] $format", name: Any, arg: Any)
	}

	override def warn(marker: Marker, msg: String) {
		parent.warn(marker, s"[$name] $msg")
	}

	override def isWarnEnabled(marker: Marker) = parent.isWarnEnabled(marker)

	override def warn(msg: String, t: Throwable) {
		parent.warn(s"[$name] $msg", t)
	}

	override def warn(format: String, arg1: Object, arg2: Object) {
		parent.warn(s"[{}] $format", name, arg1, arg2)
	}

	override def warn(format: String, arguments: Object*) {
		parent.warn(s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def warn(format: String, arg: Object) {
		parent.warn(s"[{}] $format", name: Any, arg: Any)
	}

	override def warn(msg: String) {
		parent.warn(s"[$name] $msg")
	}

	override def isWarnEnabled = parent.isWarnEnabled

	override def info(marker: Marker, msg: String, t: Throwable) {
		parent.info(marker, s"[$name] $msg", t)
	}

	override def info(marker: Marker, format: String, arguments: Object*) {
		parent.info(marker, s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def info(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.info(marker, s"[{}] $format", name, arg1, arg2)
	}

	override def info(marker: Marker, format: String, arg: Object) {
		parent.info(marker, s"[{}] $format", name: Any, arg: Any)
	}

	override def info(marker: Marker, msg: String) {
		parent.info(marker, s"[$name] $msg")
	}

	override def isInfoEnabled(marker: Marker) = parent.isInfoEnabled(marker)

	override def info(msg: String, t: Throwable) {
		parent.info(s"[$name] $msg", t)
	}

	override def info(format: String, arguments: Object*) {
		parent.info(s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def info(format: String, arg1: Object, arg2: Object) {
		parent.info(s"[{}] $format", name, arg1, arg2)
	}

	override def info(format: String, arg: Object) {
		parent.info(s"[{}] $format", name: Any, arg: Any)
	}

	override def info(msg: String) {
		parent.info(s"[$name] $msg")
	}

	override def isInfoEnabled = parent.isInfoEnabled

	override def debug(marker: Marker, msg: String, t: Throwable) {
		parent.debug(marker, s"[$name] $msg", t)
	}

	override def debug(marker: Marker, format: String, arguments: Object*) {
		parent.debug(marker, s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def debug(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.debug(marker, s"[{}] $format", name, arg1, arg2)
	}

	override def debug(marker: Marker, format: String, arg: Object) {
		parent.debug(marker, s"[{}] $format", name: Any, arg: Any)
	}

	override def debug(marker: Marker, msg: String) {
		parent.debug(marker, s"[$name] $msg")
	}

	override def isDebugEnabled(marker: Marker) = parent.isDebugEnabled(marker)

	override def debug(msg: String, t: Throwable) {
		parent.debug(s"[$name] $msg", t)
	}

	override def debug(format: String, arguments: Object*) {
		parent.debug(s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def debug(format: String, arg1: Object, arg2: Object) {
		parent.debug(s"[{}] $format", name, arg1, arg2)
	}

	override def debug(format: String, arg: Object) {
		parent.debug(s"[{}] $format", name: Any, arg: Any)
	}

	override def debug(msg: String) {
		parent.debug(s"[$name] $msg")
	}

	override def isDebugEnabled = parent.isDebugEnabled

	override def trace(marker: Marker, msg: String, t: Throwable) {
		parent.trace(marker, s"[$name] $msg", t)
	}

	override def trace(marker: Marker, format: String, argArray: Object*) {
		parent.trace(marker, s"[{}] $format", List(name) ++ argArray: _*)
	}

	override def trace(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.trace(marker, s"[{}] $format", name, arg1, arg2)
	}

	override def trace(marker: Marker, format: String, arg: Object) {
		parent.trace(marker, s"[{}] $format", name: Any, arg: Any)
	}

	override def trace(marker: Marker, msg: String) {
		parent.trace(marker, s"[$name] $msg")
	}

	override def isTraceEnabled(marker: Marker) = parent.isTraceEnabled(marker)

	override def trace(msg: String, t: Throwable) {
		parent.trace(s"[$name] $msg", t)
	}

	override def trace(format: String, arguments: Object*) {
		parent.trace(s"[{}] $format", List(name) ++ arguments: _*)
	}

	override def trace(format: String, arg1: Object, arg2: Object) {
		parent.trace(s"[{}] $format", name, arg1, arg2)
	}

	override def trace(format: String, arg: Object) {
		parent.trace(s"[{}] $format", name: Any, arg: Any)
	}

	override def trace(msg: String) {
		parent.trace(s"[$name] $msg")
	}

	override def isTraceEnabled = parent.isTraceEnabled
}