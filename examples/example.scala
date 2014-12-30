package scalado.examples

object Example extends App {
	import scalado._

	effect[Option, Int] {
		val foo = 1
		val bar = 2
		foo + bar

		val baz = 3
		bar + baz

		run(Option(5))
		run(Option(6))

		foo + baz
	}
}
