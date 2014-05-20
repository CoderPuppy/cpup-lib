package cpup.lib.logging

import org.slf4j.{Marker, Logger}

case class SubLogger(name: String, parent: Logger) extends Logger {
	protected def wrapMsg(msg: String) = s"[$name]: $msg"
	protected def wrapFormat(format: String) = s"[{}]: $format"

	override def getName = s"${parent.getName}/$name"

	override def error(marker: Marker, msg: String, t: Throwable) {
		parent.error(marker, wrapMsg(msg), t)
	}

	override def error(marker: Marker, format: String, arguments: Object*) {
		parent.error(marker, wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def error(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.error(marker, wrapFormat(format), name, arg1, arg2)
	}

	override def error(marker: Marker, format: String, arg: Object) {
		parent.error(marker, wrapFormat(format), name: Any, arg: Any)
	}

	override def error(marker: Marker, msg: String) {
		parent.error(marker, wrapMsg(msg))
	}

	override def isErrorEnabled(marker: Marker) = parent.isErrorEnabled(marker)

	override def error(msg: String, t: Throwable) {
		parent.error(wrapMsg(msg), t)
	}

	override def error(format: String, arguments: Object*) {
		parent.error(wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def error(format: String, arg1: Object, arg2: Object) {
		parent.error(wrapFormat(format), name, arg1, arg2)
	}

	override def error(format: String, arg: Object) {
		parent.error(wrapFormat(format), name: Any, arg: Any)
	}

	override def error(msg: String) {
		parent.error(wrapMsg(msg))
	}

	override def isErrorEnabled = parent.isErrorEnabled

	override def warn(marker: Marker, msg: String, t: Throwable) {
		parent.warn(marker, wrapMsg(msg), t)
	}

	override def warn(marker: Marker, format: String, arguments: Object*) {
		parent.warn(marker, wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def warn(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.warn(marker, wrapFormat(format), name, arg1, arg2)
	}

	override def warn(marker: Marker, format: String, arg: Object) {
		parent.warn(marker, wrapFormat(format), name: Any, arg: Any)
	}

	override def warn(marker: Marker, msg: String) {
		parent.warn(marker, wrapMsg(msg))
	}

	override def isWarnEnabled(marker: Marker) = parent.isWarnEnabled(marker)

	override def warn(msg: String, t: Throwable) {
		parent.warn(wrapMsg(msg), t)
	}

	override def warn(format: String, arg1: Object, arg2: Object) {
		parent.warn(wrapFormat(format), name, arg1, arg2)
	}

	override def warn(format: String, arguments: Object*) {
		parent.warn(wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def warn(format: String, arg: Object) {
		parent.warn(wrapFormat(format), name: Any, arg: Any)
	}

	override def warn(msg: String) {
		parent.warn(wrapMsg(msg))
	}

	override def isWarnEnabled = parent.isWarnEnabled

	override def info(marker: Marker, msg: String, t: Throwable) {
		parent.info(marker, wrapMsg(msg), t)
	}

	override def info(marker: Marker, format: String, arguments: Object*) {
		parent.info(marker, wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def info(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.info(marker, wrapFormat(format), name, arg1, arg2)
	}

	override def info(marker: Marker, format: String, arg: Object) {
		parent.info(marker, wrapFormat(format), name: Any, arg: Any)
	}

	override def info(marker: Marker, msg: String) {
		parent.info(marker, wrapMsg(msg))
	}

	override def isInfoEnabled(marker: Marker) = parent.isInfoEnabled(marker)

	override def info(msg: String, t: Throwable) {
		parent.info(wrapMsg(msg), t)
	}

	override def info(format: String, arguments: Object*) {
		parent.info(wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def info(format: String, arg1: Object, arg2: Object) {
		parent.info(wrapFormat(format), name, arg1, arg2)
	}

	override def info(format: String, arg: Object) {
		parent.info(wrapFormat(format), name: Any, arg: Any)
	}

	override def info(msg: String) {
		parent.info(wrapMsg(msg))
	}

	override def isInfoEnabled = parent.isInfoEnabled

	override def debug(marker: Marker, msg: String, t: Throwable) {
		parent.debug(marker, wrapMsg(msg), t)
	}

	override def debug(marker: Marker, format: String, arguments: Object*) {
		parent.debug(marker, wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def debug(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.debug(marker, wrapFormat(format), name, arg1, arg2)
	}

	override def debug(marker: Marker, format: String, arg: Object) {
		parent.debug(marker, wrapFormat(format), name: Any, arg: Any)
	}

	override def debug(marker: Marker, msg: String) {
		parent.debug(marker, wrapMsg(msg))
	}

	override def isDebugEnabled(marker: Marker) = parent.isDebugEnabled(marker)

	override def debug(msg: String, t: Throwable) {
		parent.debug(wrapMsg(msg), t)
	}

	override def debug(format: String, arguments: Object*) {
		parent.debug(wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def debug(format: String, arg1: Object, arg2: Object) {
		parent.debug(wrapFormat(format), name, arg1, arg2)
	}

	override def debug(format: String, arg: Object) {
		parent.debug(wrapFormat(format), name: Any, arg: Any)
	}

	override def debug(msg: String) {
		parent.debug(wrapMsg(msg))
	}

	override def isDebugEnabled = parent.isDebugEnabled

	override def trace(marker: Marker, msg: String, t: Throwable) {
		parent.trace(marker, wrapMsg(msg), t)
	}

	override def trace(marker: Marker, format: String, argArray: Object*) {
		parent.trace(marker, wrapFormat(format), List(name) ++ argArray: _*)
	}

	override def trace(marker: Marker, format: String, arg1: Object, arg2: Object) {
		parent.trace(marker, wrapFormat(format), name, arg1, arg2)
	}

	override def trace(marker: Marker, format: String, arg: Object) {
		parent.trace(marker, wrapFormat(format), name: Any, arg: Any)
	}

	override def trace(marker: Marker, msg: String) {
		parent.trace(marker, wrapMsg(msg))
	}

	override def isTraceEnabled(marker: Marker) = parent.isTraceEnabled(marker)

	override def trace(msg: String, t: Throwable) {
		parent.trace(wrapMsg(msg), t)
	}

	override def trace(format: String, arguments: Object*) {
		parent.trace(wrapFormat(format), List(name) ++ arguments: _*)
	}

	override def trace(format: String, arg1: Object, arg2: Object) {
		parent.trace(wrapFormat(format), name, arg1, arg2)
	}

	override def trace(format: String, arg: Object) {
		parent.trace(wrapFormat(format), name: Any, arg: Any)
	}

	override def trace(msg: String) {
		parent.trace(wrapMsg(msg))
	}

	override def isTraceEnabled = parent.isTraceEnabled
}