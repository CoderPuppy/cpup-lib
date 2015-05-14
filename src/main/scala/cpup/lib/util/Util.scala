package cpup.lib.util

import scala.collection.mutable.ListBuffer

object Util {
	def checkNull[R](obj: R, default: R): R = {
		if(obj == null) {
			default
		} else {
			obj
		}
	}

	def ancestors(_cla: Class[_]) = {
		var cla = _cla
		val ancestors = new ListBuffer[Class[_]]
		while(cla != null) {
			ancestors += cla
			cla = cla.getSuperclass
		}
		ancestors.toList
	}
}