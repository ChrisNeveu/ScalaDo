import reflect.macros.blackbox.Context
import scala.reflect.internal.annotations.compileTimeOnly

package object scalado {
   // Macros are not constained by their return types and can return more
   // specific types than indicated. This can probably be leveraged to
   // change this into a single type parameter function taking and returning
   // Any instead of A without losing functionality.
   def effect[M[_], A](a : A) : M[A] = macro Impl.effect[M, A]

   @compileTimeOnly("Cannot run outside of an effect block")
   def run[M[_], A](m : M[A]) : A = sys.error("Unexpected run found.")

   private object Impl {
      def effect[M[_], A](context : Context)(a : context.Expr[A]) : context.Expr[M[A]] =
         (new EffectImplementation { val c : context.type = context }).effect(a)
   }
}
