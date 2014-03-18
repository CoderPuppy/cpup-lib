package cpup.lib

object Util {
	def checkNull[R](obj: R, default: R): R = {
		if(obj == null) {
			default
		} else {
			obj
		}
	}
}