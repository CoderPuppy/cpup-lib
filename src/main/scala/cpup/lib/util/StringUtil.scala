package cpup.lib.util

object StringUtil {
	case class Pre(val pre: String) {
		def unapply(v: String) = if(v.startsWith(pre)) Some(v.substring(pre.length)) else None
	}
	case class Suff(val suff: String) {
		def unapply(v: String) = if(v.endsWith(suff)) Some(v.substring(0, v.length - suff.length)) else None
	}
}
