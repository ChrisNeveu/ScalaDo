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

         splitWhen(trees)(isRun(c)) match {
            case (ts, Nil) ⇒ ts
            case (ts, mapped) ⇒
               val m = ts.last
               ts.init ++ List(mkMap(c)(m, Block(mapped.init, mapped.last)))
         }
      }

      def mkMap(c : Context)(tree : c.Tree, other : c.Tree) : c.Tree = {
         import c.universe._
         Apply(
            Select(extractRun(c)(tree).get, TermName("map")),
            List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x$1"), TypeTree(), EmptyTree)), other))
         )
      }

      def mkFlatMap(c : Context)(tree : c.Tree, other : c.Tree) : c.Tree = {
         import c.universe._
         Apply(Select(tree, TermName("flatMap")), List(other))
      }

      def isRun(c : Context)(tree : c.Tree) : Boolean = {
         import c.universe._
         println("Tree--------------\n" + showRaw(tree) + "\n------------------")
         val foo = extractRun(c)(tree).isDefined
         println("isRun: " + foo + "\n------------------")
         foo
      }

      def extractRun(c : Context)(tree : c.Tree) : Option[c.Tree] = {
         import c.universe._
         tree match {
            case Apply(Ident(TermName("run")), List(t)) ⇒ Some(t)
            case Apply(TypeApply(Select(Select(Ident(TermName("scalado")), TermName("package")), TermName("run")), List(TypeTree(), TypeTree())), List(t)) ⇒ Some(t)
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
