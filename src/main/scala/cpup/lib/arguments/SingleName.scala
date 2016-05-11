package cpup.lib.arguments

trait SingleName {
	if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name")

	def name: String
}
