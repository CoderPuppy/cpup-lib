package cpup.lib.arguments

import org.slf4j.helpers.MessageFormatter

case class FormatOutput(output: (String) => Unit) extends Output {
	def doOutput(format: String, data: Any*) {
		val msg = MessageFormatter.arrayFormat(format, data.toArray.map(_.asInstanceOf[AnyRef]))
		output(msg.getMessage)
		Option(msg.getThrowable) match {
			case Some(t) => t.getStackTrace.map(_.toString).foreach(output)
			case None =>
		}
	}

	override def write(format: String, data: Any*) = doOutput(format, data: _*)
	override def error(format: String, data: Any*) = doOutput(s"E: $format", data: _*)
}
