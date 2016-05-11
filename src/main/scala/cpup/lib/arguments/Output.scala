package cpup.lib.arguments

trait Output {
	def write(msg: String, data: Any*)
	def error(msg: String, data: Any*)
}
