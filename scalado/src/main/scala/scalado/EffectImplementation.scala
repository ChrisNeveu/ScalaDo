package scalado

import reflect.macros.blackbox.Context

private abstract class EffectImplementation {
   val c : Context
   import c.universe._
   import c.abort

   def effect[M[_], A](a : Expr[A]) : Expr[M[A]] = {
      println(showRaw(a.tree))
      val newTree = a.tree match {
         case Block(lines, last) ⇒
            val newLines = mappify(lines ++ List(last))
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

   def mappify(trees : List[Tree]) : List[Tree] = {

      // The boolean indicates whether the mappified list contains
      // a monadic value.
      def _mappify(trees : List[Tree]) : (List[Tree], Boolean) =
         splitWhen(trees)(isRun) match {
            case (ts, Nil) ⇒ (ts, false)
            case (ts, mapped) ⇒
               val m = ts.last
               val (_mapped, flatMap) = _mappify(mapped)
               val newLastExpr =
                  if (flatMap)
                     mkFlatMap(m, Block(_mapped.init, _mapped.last))
                  else
                     mkMap(m, Block(_mapped.init, _mapped.last))
               (ts.init ++ List(newLastExpr), true)
         }
      _mappify(trees)._1
   }

   def uniqueName : String = ???

   // Rewrites a tree which contains a run statement to a map on the run value.
   def rewrite(tree : Tree, rest : List[Tree]) : Tree = {

      def _rewrite(tree : Tree) : (List[(String, Tree)], Tree) = tree match {
         case Apply(
            TypeApply(
               Select(
                  Select(Ident(TermName("scalado")), TermName("package")),
                  TermName("run")),
               List(TypeTree(), TypeTree())),
            List(t)) ⇒
            val name = uniqueName
            (List((name, t)), Ident(TermName(uniqueName)))

         case Select(_tree, prop) ⇒
            val (names, newTree) = _rewrite(_tree)
            (names, Select(newTree, prop))

         case ValDef(mod, name, tt, _tree) ⇒
            val (names, newTree) = _rewrite(_tree)
            (names, ValDef(mod, name, tt, _tree))

         case _ ⇒ abort(NoPosition, "Unexpected run found.")
      }

      ???
   }

   def valDef2Function(vd : ValDef) : Function = {

      val ValDef(mods, name, tt, tree) = vd
      Function(List(ValDef(mods, name, tt, EmptyTree)), tree)
   }

   def mkMap(tree : Tree, other : Tree) : Tree = {
      tree match {
         case f @ Function(_, _) ⇒ Apply(
            Select(extractRun(tree).get, TermName("map")),
            List(other))
         case _ ⇒ Apply(
            Select(extractRun(tree).get, TermName("map")),
            List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x$1"), TypeTree(), EmptyTree)), other)))
      }
   }

   def mkFlatMap(tree : Tree, other : Tree) : Tree = {
      Apply(
         Select(extractRun(tree).get, TermName("flatMap")),
         List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x$1"), TypeTree(), EmptyTree)), other))
      )
   }

   def isRun(tree : Tree) : Boolean =
      extractRun(tree).isDefined

   def hasRun(tree : Tree) : Boolean = {

      tree match {
         case t if extractRun(t).isDefined ⇒ true
         case ValDef(_, _, _, t)           ⇒ hasRun(t)
         case Block(ts, t)                 ⇒ hasRun(t) || ts.map(hasRun).foldRight(false)((a, b) ⇒ a || b)
         case _                            ⇒ false
      }
   }

   def extractRun(tree : Tree) : Option[Tree] = {
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
