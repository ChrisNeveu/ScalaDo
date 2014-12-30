import reflect.macros.blackbox.Context
import scala.reflect.internal.annotations.compileTimeOnly

package object scalado {
   def effect[M[_], A](a : A) : M[A] = macro Impl.effect[M, A]

   @compileTimeOnly("Cannot run outside of an effect block")
   def run[M[_], A](m : M[A]) : A = sys.error("Unexpected run found.")

   private object Impl {
      def effect[M[_], A](c : Context)(a : c.Expr[A]) : c.Expr[M[A]] = {
         import c.universe._
         println(showRaw(a.tree))
         val newTree = a.tree match {
            case Block(lines, last) ⇒
               val newLines = mappify(c)(lines ++ List(last))
               c.Expr[M[A]](Block(newLines.init, newLines.last))
            case t ⇒ c.Expr[M[A]](t)
         }
         println(".............\n" + newTree + "\n.............")
         newTree
      }

      def splitWhen[A](l : List[A])(predicate : A ⇒ Boolean) : (List[A], List[A]) =
         l match {
            case Nil                       ⇒ (Nil, Nil)
            case (a :: as) if predicate(a) ⇒ (List(a), as)
            case (a :: as) ⇒
               val (first, second) = splitWhen(as)(predicate)
               (a :: first, second)
         }

      def mappify(c : Context)(trees : List[c.Tree]) : List[c.Tree] = {
         import c.universe._

         // The boolean indicates whether the mappified list contains
         // a monadic value.
         def _mappify(trees : List[c.Tree]) : (List[c.Tree], Boolean) =
            splitWhen(trees)(isRun(c)) match {
               case (ts, Nil) ⇒ (ts, false)
               case (ts, mapped) ⇒
                  val m = ts.last
                  val (_mapped, flatMap) = _mappify(mapped)
                  val newLastExpr =
                     if (flatMap)
                        mkFlatMap(c)(m, Block(_mapped.init, _mapped.last))
                     else
                        mkMap(c)(m, Block(_mapped.init, _mapped.last))
                  (ts.init ++ List(newLastExpr), true)
            }
         _mappify(trees)._1
      }

      def valDef2Function(c : Context)(vd : c.universe.ValDef) : c.universe.Function = {
         import c.universe._

         val ValDef(mods, name, tt, tree) = vd
         Function(List(ValDef(mods, name, tt, EmptyTree)), tree)
      }

      def mkMap(c : Context)(tree : c.Tree, other : c.Tree) : c.Tree = {
         import c.universe._
         tree match {
            case f @ Function(_, _) ⇒ Apply(
               Select(extractRun(c)(tree).get, TermName("map")),
               List(other))
            case _ ⇒ Apply(
               Select(extractRun(c)(tree).get, TermName("map")),
               List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x$1"), TypeTree(), EmptyTree)), other)))
         }
      }

      def mkFlatMap(c : Context)(tree : c.Tree, other : c.Tree) : c.Tree = {
         import c.universe._
         Apply(
            Select(extractRun(c)(tree).get, TermName("flatMap")),
            List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x$1"), TypeTree(), EmptyTree)), other))
         )
      }

      def isRun(c : Context)(tree : c.Tree) : Boolean =
         extractRun(c)(tree).isDefined

      def hasRun(c : Context)(tree : c.Tree) : Boolean = {
         import c.universe._

         tree match {
            case t if extractRun(c)(t).isDefined ⇒ true
            case ValDef(_, _, _, t)              ⇒ hasRun(c)(t)
            case Block(ts, t)                    ⇒ hasRun(c)(t) || ts.map(hasRun(c)).foldRight(false)((a, b) ⇒ a || b)
            case _                               ⇒ false
         }
      }

      def extractRun(c : Context)(tree : c.Tree) : Option[c.Tree] = {
         import c.universe._
         tree match {
            case Apply(Ident(TermName("run")), List(t)) ⇒ Some(t)
            case Apply(
               TypeApply(
                  Select(
                     Select(Ident(TermName("scalado")), TermName("package")),
                     TermName("run")),
                  List(TypeTree(), TypeTree())),
               List(t)) ⇒ Some(t)
            case _ ⇒ None
         }
      }

   }
}

/*
 Block(
   List(
     ValDef(Modifiers(), TermName("foo"), TypeTree(), Literal(Constant(1))),
     ValDef(Modifiers(), TermName("bar"), TypeTree(), Literal(Constant(2))),
     Apply(Select(Ident(TermName("foo")), TermName("$plus")), List(Ident(TermName("bar")))),
     ValDef(Modifiers(), TermName("baz"), TypeTree(), Literal(Constant(3))),
     Apply(Select(Ident(TermName("bar")), TermName("$plus")), List(Ident(TermName("baz"))))),
   Apply(Select(Ident(TermName("foo")), TermName("$plus")), List(Ident(TermName("baz")))))

 Block(
   List(
     ValDef(Modifiers(), TermName("foo"), TypeTree(), Literal(Constant(1))),
     ValDef(Modifiers(), TermName("bar"), TypeTree(), Literal(Constant(2))),
     Apply(Select(Ident(TermName("foo")), TermName("$plus")), List(Ident(TermName("bar")))),
     ValDef(Modifiers(), TermName("baz"), TypeTree(), Literal(Constant(3))),
     Apply(Select(Ident(TermName("bar")), TermName("$plus")), List(Ident(TermName("baz")))),
     Apply(
       TypeApply(
         Select(
           Apply(TypeApply(Select(Select(Ident(scala), scala.Option), TermName("apply")), List(TypeTree())), List(Literal(Constant(5)))),
           TermName("map")),
         List(TypeTree())),
       List(Function(List(ValDef(Modifiers(PARAM), TermName("x$1"), TypeTree(), EmptyTree)), Literal(Constant(6)))))),
   Apply(Select(Ident(TermName("foo")), TermName("$plus")), List(Ident(TermName("baz")))))
 */
