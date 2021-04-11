package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Exercise with DoWith ***/

  /*** Rename bound variables in e ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | A(_) => doreturn(e)

      /***
        * ren(env,e1) ~ [W,B]
        *
        * Map is used to return an expression DoWith[W,B]
        * Similar process to mapWith, map here when invoked takes f:r where r => B
        * and returns [W,B], where W is our state and B is the new expression returned
        *
        */

      case Print(e1) => ren(env,e1) map {
        ren_e1 => Print(ren_e1)
      }

      /***
        * ren(env,e1) ~ [W,R]
        *
        * Map is used to return an expression DoWith[W,B]
        * Similar process to mapWith, map here when invoked takes f:r where r => B
        * and returns [W,B], where W is our state and B is the new expression returned
        */

      case Unary(uop, e1) => ren(env, e1) map {
        ren_e1 => Unary(uop, ren_e1)
      }

      /***
        * ren(env,e1) ~ [W,R]
        *
        * Map is used to return an expression DoWith[W,B]
        * Similar process to mapWith, map here when invoked takes f:r where r => B
        * and returns [W,B], where W is our state and B is the new expression returned
        */

      case Binary(bop, e1, e2) => ren(env,e1) flatMap {
        ren_e1 => ren(env, e2) map {
          ren_e2 => Binary(bop, ren_e1, ren_e2)
        }
      }


      /***
        * ren(env,e1) ~ [W,R]
        *
        * flatMap is used for the first two expressions because we want to be able to return a DoWith[W,B] Computation.
        * Here our DoWith[W,R] is ren(env,e1). Since each expression needs to be renamed, flatMap is chained for each
        * expression except the last, where we use "map" to return the new expression (B) as part of the chained [W,B].
        */

      case If(e1, e2, e3) => ren(env,e1) flatMap {
        ren_e1 => ren(env, e2) flatMap {
          ren_e2 => ren(env, e3) map {
            ren_e3 => If(ren_e1, ren_e2, ren_e3)
          }
        }
      }


      /***
        * Lifted from previous lab4, since we only want to perform a computation without a change of state [W]
        * we wrap computation in a doreturn to obtain our result R
        *
        */
      case Var(x) => doreturn(if(env.contains(x)) Var(env(x)) else Var(x))


      /***
        * ren(env,e1) ~ [W,R]
        *
        * Same as If statement, flatMap is used for the first two expressions because we want to be able to return a DoWith[W,B] Computation.
        * Here our DoWith[W,R] is ren(env,e1). Since each expression needs to be renamed, flatMap is chained for each
        * expression except the last, where we use "map" to return the new expression (B) as part of the chained [W,B].
        *
        * instead of val envp = extend(env, y, yp) and  Decl(mode, yp, ren(env, e1), ren(envp, e2))
        * x is renamed directly in the environment env + (x -> newX)
        */

      case Decl(m, x, e1, e2) => fresh(x) flatMap {
        newX => ren(env, e1) flatMap {
          ren_e1 => ren(env + (x -> newX), e2) map {
            ren_e2 => Decl(m, newX,  ren_e1, ren_e2)
          }
        }
      }

      case Function(p, params, tann, e1) =>
      {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {

          /***
            * For this function, w is of type DoWith[W, R], where R is a tuple
            * of (Optional String, and a Map of [String, String]
            * If p is none (no function name). Then we must return [W,R] of the form (None, and env [String, String]
            */
          case None => doreturn(None, env)

          /***
            * If this function is some name x, get a fresh value for x
            * and map the new value of x to the old value in the the environment
            */
          case Some(x) => fresh(x) map { newX => (Some(newX), env + (x -> newX)) }
        }

        /***
          *  Bind to env2 an environment that extends env1 with bindings for params.
          *  In otherwords, after adding the function name with return type from environment 1
          *  add any "new" parameters from the list of params supplied by the argument called params.
          *
          *  environment is our accumulator or starting point.  Params will be what we operate on
          *
          *  Flat map indicates we will calling a DoWith Function
          */

        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {

            /***
              * Note the fold right function takes a DoWith of state "W" and a tuple
              * Tuple_1 -> List of [String, MType] which means we might be expanding the environment
              * Tuple_2 -> Map of [String, String[ which is our environment env
              */
            case ((x,mty), acc) => acc flatMap {
              case (newParamTypes, newEnv) => fresh(x) map{
                newX => ((newX,mty)::newParamTypes, extend(newEnv, x ,newX))
              }
            }
          } flatMap {

            /***
              * Now implement all the new values and environments
              * and return a new function expression
              */

            case (newParamTypes2, newEnv2)  =>  ren(newEnv2, e1) map {
              ren_e1 => Function(pp, newParamTypes2, tann, ren_e1)
            }
          }
        }
      }

      case Call(e1, args) => ren(env, e1) flatMap (
        ren_e1 => mapWith(args)(expression => ren(env, expression)) map (
          newArguments => Call(ren_e1, newArguments)
          ))

      case Obj(fields) =>mapWith(fields)(expression => ren(env,expression._2) map (
        newExpression1 => (expression._1, newExpression1))) map (
        newfields => Obj(newfields))

      case GetField(e1, f) => ren(env, e1) map {
        ren_e1 => GetField(ren_e1,f)
      }

      case Assign(e1, e2) =>ren(env, e1) flatMap {
        ren_e1 => ren(env, e2) map {
          ren_e2 => Assign(ren_e1, ren_e2)
        }
      }

    }
    ren(env, e)
  }


  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget[Int] flatMap {
        (i) =>
          val xp = "x" + i
          doput(i+1) map { (_) => xp}
      }
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }


  /*** Helper: mapFirst to DoWith ***/
  /***
    *
    * First => Use doreturn to return empty list of Type[B] with state W
    * Second => create anonymous function (elementA, acc) that returns acc (DoWith[W,B])
    * Third => use flatmap on acc, flatmap[B](f:R => DW[W,B]):DW[W,B]
    * Fourth => Flatmap takes B (an Empty List B) and returns DW[W,B]
    * Fifth => take f(elementA)
    * Sixth => map element back to listB, map[B](f:R =>[B]):DW[W,B]
    *
    */

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      (elementA, acc) => acc flatMap{
        someEmptyListB  =>  f(elementA) map {newValueB => newValueB :: someEmptyListB}
      }
    }
  }

  /**
    * Same process as above except for last step (step 6)
    * map { newValueCD => someEmptyMapCD + newValueCD}
    * Note: adding to an empty map uses different notation than a list
    *
    */

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map[C,D]())) {

      (someAB, acc)=> acc flatMap{

        someEmptyMapCD => f(someAB) map { newValueCD => someEmptyMapCD + newValueCD}
      }
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  /***
    * [W, A] are the generic parameter types that the mapFirstWith Function can take
    * more specifically it can take [list, Double] etc..
    *
    */
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {

    /**
      * previously...
      * case Nil => doreturn(l)
      * Which one yields a better score?
      */

    case Nil => doreturn(Nil)

    /***
      * Run the function on the head h
      */
    case h :: t => f(h) match {

      /***
        * Function returns Some( DoWith[W,A] )
        *
        * Run the "map" function from DoWith.Scala
        */
      case Some(someDoWith) => someDoWith map (someValue => someValue::t)
      case None => mapFirstWith(t)(f) map (someExpression => h::someExpression)


    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }


  /**
    * Refer to ValBind and RefBind pg 9 of lab pdf
    */

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case MConst => true
    case MName => true
    case MVar => true
    case MRef if isLExpr(e) => true
    case _  => false
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => lookup(env, x).t

      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }

      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }

      case Binary(Plus, e1, e2) => ((typeof(env, e1)), (typeof(env, e2))) match {
        case(TNumber, TNumber) => TNumber
        case(TString, TString) => TString
        case(TString, t2) => err(t2, e2)
        case(TNumber, t2) => err(t2, e2)
        case(t1, t2) => err(t1, e1)
      }

      case Binary(Minus|Times|Div, e1, e2) => ((typeof(env, e1)), (typeof(env, e2))) match {
        case(TNumber, TNumber) => TNumber
        case(TNumber, t2) => err(t2, e2)
        case(tgot, _) => err(tgot, e1)
      }

      case Binary(Eq|Ne, e1, e2) => ((typeof(env, e1)), (typeof(env, e2))) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TBool, TBool) => TBool
        case (TUndefined, TUndefined) => TBool
        case (TObj(f1), TObj(f2)) if(!hasFunctionTyp(TObj(f1)) && !hasFunctionTyp(TObj(f2))) => TBool
        case (t1, t2) => err(t1, e1)
      }

      case Binary(Lt|Le|Gt|Ge, e1, e2) => ((typeof(env, e1)), (typeof(env, e2))) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber, t2) => err(t2, e2)
        case (TString, t2) => err(t2, e2)
        case (t1, t2) => err(t1, e1)
      }

      case Binary(And|Or, e1, e2) => ((typeof(env, e1)), (typeof(env, e2))) match {
        case (TBool, TBool) => TBool
        case (TBool, t2) => err(t2, e2)
        case (t1, t2) => err(t1, e1)
      }

      //      case Binary(Seq, e1, e2) => typeof(env, e2) match {
      //        case TNumber => TNumber
      //        case TString => TString
      //        case TBool => TBool
      //        case TUndefined => TUndefined
      //        case TObj(field) => TObj(field)
      //        case TFunction(params, _) => typeof(env, e2)
      //        case t2 => err(t2, e2)
      //      }

      case Binary(Seq, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (t1, t2) => t2
      }

      case If(e1, e2, e3) => (typeof(env, e1), typeof(env, e2), typeof(env, e3)) match {
        case (TBool, t2, t3) => if(t2==t3) t2 else err(t3, e3)
        case (t1,_,_) => err(t1, e1)
      }

      case Obj(fields) => TObj(fields mapValues(exp => typeof(env, exp)))

      case GetField(e1, fname) => typeof(env, e1) match {
        case (TObj(fields)) => if(fields.contains(fname)) fields(fname) else throw err(typeof(env, e1), e1)
        case _ => throw StaticTypeError(TUndefined, e1, e)
      }

      /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) =>if (isBindex(m, e1)) typeof(extend(env, x, MTyp(m,typeof(env, e1))), e2) else err(typeof(env, e1), e1)

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(p), Some(tann)) => extend(env, p, MTyp(MConst, TFunction(params, tann)))
          case (Some(p), None) => err(TUndefined, e1)
          case (None, tann) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1) {
          case (env1, (str, mtyp)) => {
            extend(env1, str, mtyp)
          }
        }

        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case None => TFunction(params, t1)
          case Some(tp) => if(tp==t1) TFunction(params, t1) else err(tp, e1)
        }
      }

      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params zip args).foreach {
            case (param, arg) =>
              val mode_param = param._2.m
              val typ_param = param._2.t
              val typ_arg = typeof(env, arg)
              if((typ_param!=typ_arg) || !isBindex(mode_param, arg)) err(typ_arg, e1)
          }
          tret
        case _ => err(TUndefined, e1)
      }

      /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) =>
        val t1 = typeof(env, e1)
        lookup(env, x) match {
          case MTyp(m, typ_x) => if ((m==MVar || m==MRef) && (t1==typ_x)) t1 else err(TUndefined, e1)
          case _ => err(TUndefined, e1)
        }
      case Assign(GetField(e1, fname), e2) =>
        val t1 = typeof(env, e2)
        typeof(env, GetField(e1, fname)) match {
          case typ_f => if(typ_f == t1) typ_f else err(TUndefined, e1)
        }
      case Assign(_, _) => err(TUndefined, e)

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(v1), S(v2)) => bop match {
        case Lt => (v1) < (v2)
        case Le => (v1) <= (v2)
        case Gt => (v1) > (v2)
        case Ge => (v1) >= (v2)
      }
      case (N(v1), N(v2)) => bop match {
        case Lt => (v1) < (v2)
        case Le => (v1) <= (v2)
        case Gt => (v1) > (v2)
        case Ge => (v1) >= (v2)
      }
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  /* x is variable name, e is expression is being substituted into, esub is what is being substituted in*/
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | A(_) => e
      case Print(e1) => Print(subst(e1))

      /***** Cases from Lab 4 */
      case Unary(uop, e1) => Unary(uop, substitute(e1, esub, x))
      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, esub, x), substitute(e2, esub, x))
      case If(e1, e2, e3) => If(substitute(e1, esub, x), substitute(e2, esub, x), substitute(e3, esub, x))
      case Var(y) => if(x == y) esub else e
      case Decl(mode, y, e1, e2) => if(x == y) Decl(mode, y, substitute(e1, esub, x), e2) else Decl(mode, y, substitute(e1, esub, x), substitute(e2, esub, x))

      case Function(None, params, tann, e1) =>
        val result=(params.exists(z => {z._1 ==x}))
        if(result) e else Function(None, params, tann, substitute(e1, esub, x))

      case Function(Some(y), params, tann, e1) =>
        val result2=(params.exists(z=> {z._1 ==x}))
        if(result2 | (x==y)) e else Function(Some(y), params, tann, substitute(e1, esub, x))


      case Call(e1, args) => Call(substitute(e1, esub, x), args.map(newArguments => substitute(newArguments, esub, x)))
      case Obj(fields) => Obj(fields.mapValues(x=> subst(x)))
      case GetField(e1, fname) => GetField(subst(e1), fname)

      /***** New cases for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1),subst(e2))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains(x)) fresh(x + "$") else x
      rename[Unit](e)(Nil){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e)) // change this line when you implement capture-avoidance
  }

  /* Check whether or not an expression is reducible given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => !isValue(e)
    case MVar => !isValue(e)
    case MRef => !isLValue(e)
    case MName => false
  }

  //Will obtain the bound variable from the expression
  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MConst => doreturn(e)
      case MName => doreturn(e)
      case MRef => doreturn(e)
      case MVar => memalloc(e) map { a => Unary(Deref, a)}
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {


      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      /***** Cases needing adapting from Lab 3. */


      /**
        * is this right?
        * /**Returns DoWith[Mem, Exp]*/
        */
      case Unary(Neg, v1) if isValue(v1) => v1 match {
        case N(n1) => doreturn(N(-n1))
      }

      /**Returns DoWith[Mem, Exp]*/
      case Unary(Not, v1) if isValue(v1) => v1 match {
        case B(b1) => doreturn(B(!b1))
      }

      /**Returns DoWith[Mem, Exp]
        *
        * doGet returns [Mem, Mem]
        * Map allows us to return any e2 to the DoWith
        * */
      case Binary(Seq, e1, e2) if (isValue(e1)) => doreturn(e2) /*why does e1 have to be a value here, I thought sequence short circuits*/


      /***
        * Remember e can be any complex expression
        * Binary Plus Includes both str and num expressions
        */

      /**Plus String and Number**/
      case Binary(Plus, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (N(n1), N(n2)) => doreturn(N(n1 + n2))
        case (S(s1),S(s2)) => doreturn(S(s1 + s2))
      }
      case Binary(Minus, N(n1), N(n2)) => doreturn(N(n1 - n2))
      case Binary(Div, N(n1), N(n2)) => doreturn(N(n1 / n2))
      case Binary(Times, N(n1), N(n2)) => doreturn(N(n1 * n2))


      case Binary(Lt, N(n1), N(n2)) => doreturn(B(inequalityVal(Lt, N(n1), N(n2))))
      case Binary(Lt, S(s1), S(s2)) => doreturn(B(inequalityVal(Lt, S(s1), S(s2))))
      case Binary(Gt, N(n1), N(n2)) => doreturn(B(inequalityVal(Gt, N(n1), N(n2))))
      case Binary(Gt, S(s1), S(s2)) => doreturn(B(inequalityVal(Gt, S(s1), S(s2))))
      case Binary(Le, N(n1), N(n2)) => doreturn(B(inequalityVal(Le, N(n1), N(n2))))
      case Binary(Le, S(s1), S(s2)) => doreturn(B(inequalityVal(Le, S(s1), S(s2))))
      case Binary(Ge, N(n1), N(n2)) => doreturn(B(inequalityVal(Ge, N(n1), N(n2))))
      case Binary(Ge, S(s1), S(s2)) => doreturn(B(inequalityVal(Ge, S(s1), S(s2))))

      case Binary(Eq, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (v1, v2) => doreturn(B(v1 == v2))
      }
      case Binary(Ne, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (v1, v2) => doreturn(B(v1 != v2))
      }



      /**Do And true/false  **/
      case Binary(And, B(b1), e2) => b1 match {
        case (true) => doreturn(e2)
        case (false) => doreturn(B(false))
      }

      /** Do Or True/False **/
      case Binary(Or, B(b1), e2) => b1 match {
        case (true) => doreturn(B(true))
        case (false) => doreturn(e2)
      }

      /** Do If True/False **/
      case If(B(b1), e2, e3) => b1 match {
        case (true) => doreturn(e2)
        case (false) => doreturn(e3)
      }

      /**Do Decl**/
      case Decl(m, x, e1, e2) if !isRedex(m, e1) =>
        val b_dw = getBinding(m,e1)
        b_dw map {
        dw_val => substitute(e2, dw_val, x)
      }


      /***** More cases here */
      /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        memalloc(e)

      case GetField(a @ A(_), f) => doget map {m => m(a) match {
        case Obj(fields) => fields(f)
        }
      }


      /***** New cases for Lab 5. */
      case Unary(Deref, a@A(_)) =>
        doget map {
        m => m(a)
      }


      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m + (a -> v) } map { _ => v }

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        domodify[Mem] {
          (m) => m(a) match { /*match what's stored at memory address a*/
            case Obj(fields) => m + (a -> Obj(fields + ((f,v)))) /*extend memory to map address a new object with key value pair f,v*/
          }
        } map { _ => v } /* returns dowith[Mem, v]*/

      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args
        if (pazip forall  { case (((_, MTyp(m, _)), arguments)) => !isRedex(m, arguments)}) {
          val dwep = pazip.foldRight( doreturn(e) : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => getBinding(mi, ei) flatMap {
              newExpressioni => dwacc map {
                newExpression => substitute(newExpression, newExpressioni, xi)}}
          }
          p match {
            case None => dwep
            case Some(x) => dwep map { newExpression2 => substitute(newExpression2, v, x)}
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            case (param@(_: String, MTyp(m, _)), arg: Expr) if isRedex(m, arg) => Some(step(arg) map {
              newArguments => (param, newArguments)})
            case _ => None
          }
          dwpazipp map {pa => Call(v,pa.unzip._2)}
        }
      }

      /* Inductive Cases: Search Rules */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }


      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */

      /**Search Unary **/
      case Unary(uop, e1) => step(e1) map {
        e1p => Unary(uop, e1p)
      }

      /**Search Binary 1 **/
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map {
        e2p => Binary(bop, v1, e2p)}

      /**Search Binary 2**/
      case Binary(bop, e1, e2) => step(e1) map {
        e1p => Binary(bop, e1p, e2)}

      /**Search If**/
      case If(e1, e2, e3) => step(e1) map {
        e1p => If(e1p, e2, e3)}


      /** Cases needing adapting from Lab 4 **/


      /**Search Get Field***/
      case GetField(e1, f) => step(e1) map {
        e1p => GetField(e1p, f)
      }

      /***Search Object***/
      case Obj(fields) => fields find {
        case (_, e1) => !isValue(e1) } match {
        case Some((f1, e1)) => step(e1) map
          { e1p => Obj(fields + (f1 -> e1p))}
      }

      /***Search Decl***/
      case Decl(mode, x, e1, e2) if isRedex(mode, e1) => step(e1) map {
        e1p => Decl(mode, x, e1p, e2)}


      /**Search Call**/
      case Call(e1, args) => step(e1) map {
        e1p => Call(e1p, args)
      }


      /** New cases for Lab 5.  **/



      /***Search Assign 1**/
      case Assign(e1, e2) if !isLValue(e1) => step(e1) map {
        e1p => Assign(e1p, e2)}

      /***Search Assign 2***/
      case Assign(e1, e2) => step(e2) map {
        e2p=> Assign(e1, e2p)
      }

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
  /* Do nothing by default. Change to attempt extra credit. */
    e

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
