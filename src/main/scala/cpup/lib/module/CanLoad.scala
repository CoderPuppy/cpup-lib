package cpup.lib.module

case class CanLoad(toBoolean: Boolean, messages: CanLoad.Message*) {
	def withResult(bool: Boolean) = CanLoad(bool, messages: _*)
	def addMessages(msgs: CanLoad.Message*) = CanLoad(toBoolean, messages ++ msgs: _*)
	def withMessages(msgs: CanLoad.Message*) = CanLoad(toBoolean, msgs: _*)

	def &&(other: CanLoad) = this and other
	def and(other: CanLoad) = withResult(toBoolean && other.toBoolean).addMessages(other.messages: _*)

	def ||(other: CanLoad) = this or other
	def or(other: CanLoad) = withResult(toBoolean || other.toBoolean).addMessages(other.messages: _*)

	def negate = !this
	def unary_! = withResult(!toBoolean).withMessages(messages.map(NotMessage): _*)
}

object CanLoad {
	trait Message {}
}