package jsy.student

import jsy.lab5.Lab5Like
import jsy.lab5.Parser.parse
import jsy.lab5.ast._
import jsy.tester.JavascriptyTester
import jsy.util.DoWith
import jsy.util.DoWith._
import org.scalatest._

class Lab5Spec(lab5: Lab5Like) extends FlatSpec {
  import lab5._

  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith(l1) { (i: Int) => if (i < 0) Some(doreturn(-i)) else None }
     assertResult((true,gold1)) { dowith(true) }
     assertResult((42,gold1)) { dowith(42) }
  }

  "mapWith(List)" should "map the elements of a list in a DoWith" in {
    val l = List(1, 2, 3, 4, 5)
    val r1 = l.map { i => i + 1 }

    def dowith1[W]: DoWith[W,List[Int]] = mapWith(l) { i: Int => doreturn(i + 1) }
    assertResult((true,r1)) { dowith1(true) }
    assertResult((42,r1)) { dowith1(42) }

    assertResult((2 * l.length + 1, r1)) {
      val dw: DoWith[Int,List[Int]] = mapWith(l) { i: Int =>
        domodify[Int](s => s + 2) map { _ => i + 1 }
      }
      dw(1)
    }
  }

  "rename" should "rename in a DoWith" in {
    val e1 = parse("const a = 1 + a; a")
    val e1p = parse("const aa = 1 + a; aa")

    assertResult((1,e1p)) {
      rename(empty, e1){ x => domodify[Int](n => n + 1) map { _ => x + x } }(0)
    }
  }

  "uniquify" should "uniquify with a counter for each variable" in {
    val e1 = parse("const a = 1; a")
    val e1p = parse("const a1 = 1; a1")
    val e2 = parse("const b = 2; b")
    val e2p = parse("const b0 = 2; b0")
    val e = Decl(MConst, "a", e1, e2)
    val ep = Decl(MConst, "a0", e1p, e2p)
    assertResult(ep) { uniquify(e) }
  }


  /* Tests based on rules */

  "DoNeg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = Unary(Neg, e1)
    assertResult( N(-5) ) {
      val (_, r) = step(e2)(memempty)
      r
    }
  }

  "DoNot" should "return the negation of a number value" in {
    assertResult( B(false) ) {
      val (_, r) = step(Unary(Not, B(true)))(memempty)
      r
    }
  }

  "DoSeq" should "return the second thing" in {
    val e1 = N(5)
    val e2 = N(6)
    assertResult( N(6) ) {
      val (_, r) = step(Binary(Seq, e1, e2))(memempty)
      r
    }
    assertResult(N(-6)) {
      iterateStep(Binary(Seq, e1, Unary(Neg, e2)))
    }
  }

  "DoArith" should "Do Arithematic" in {
    val e1 = N(5)
    val e2 = N(6)
    assertResult( N(11) ) {
      val (_, r) = step(Binary(Plus, e1, e2))(memempty)
      r
    }
    assertResult(N(30)) {
      iterateStep(Binary(Times, e1, e2))
    }
  }

  "DoPlusString" should "Concat Strings" in {
    val e1 = S("hi")
    val e2 = S(" there")
    assertResult(S( "hi there" )) {
      val (_, r) = step(Binary(Plus, e1, e2))(memempty)
      r
    }
  }


  "TypeVar" should "perform TypeVar" in {
    val xtype = MTyp(MConst, TNumber)
    val tenvx = extend(empty, "x", xtype)

    assertResult(TNumber) {
      typeof(tenvx, Var("x"))
    }
  }

  "TypeNeg" should "be a number" in {
    assertResult(TNumber) {
      typeof(empty, Unary(Neg, N(4)))
    }
  }


  "TypeArith" should "be a number" in {
    assertResult(TNumber) {
      typeof(empty, Binary(Plus, N(3), N(4)))
    }
    assertResult(TNumber) {
      typeof(empty, Binary(Minus, N(3), N(4)))
    }
    assertResult(TNumber) {
      typeof(empty, Binary(Times, N(3), N(4)))
    }
    assertResult(TNumber) {
      typeof(empty, Binary(Div, N(3), N(4)))
    }
    assertResult(TNumber) {
      typeof(empty, Binary(Minus, Binary(Plus, N(3), N(4)), N(2)))
    }
  }

  it should "throw an error" in{
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Div, N(3), S("hi")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Minus, N(3), S("hi")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Times, S("hi"), N(4)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Plus, N(3), B(true)))
    }
  }

  "TypePlusString" should "be a string" in {
    assertResult(TString) {
      typeof(empty, Binary(Plus, S("hi"), S("hi")))
    }
  }

  it should "throw an error" in {
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Plus, S("hi"), B(true)))
    }

    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Plus, S("hi"), N(3)))
    }
  }

  "TypeInequalityNumber" should "be a bool" in {
    assertResult(TBool) {
      typeof(empty, Binary(Gt, N(3), N(4)))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Lt, N(3), N(4)))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Le, N(3), N(4)))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Lt, N(3), N(4)))
    }
  }

  it should "throw an error" in {
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Ge, N(3), S("hi")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Lt, N(3), S("hi")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Gt, S("hi"), N(4)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty,Binary(Le, N(3), B(true)))
    }
  }

  "TypeInequalityString" should "be a bool" in {
    assertResult(TBool) {
      typeof(empty, Binary(Gt, S("hi"), S("Yo")))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Ge, S("hi"), S("Yo")))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Lt, S("hi"), S("Yo")))
    }
    assertResult(TBool) {
      typeof(empty, Binary(Le, S("hi"), S("Yo")))
    }
  }

  "TypeEquality" should "return bool if things have the same type" in {
    assertResult(TBool){
      typeof(empty, Binary(Eq, N(1), N(2)))
    }
    assertResult(TBool){
      typeof(empty, Binary(Eq, B(true), B(false)))
    }
    assertResult(TBool){
      typeof(empty, Binary(Eq, S("hi"), S("yo")))
    }
    assertResult(TBool){
      typeof(empty, Binary(Eq, Undefined, Undefined))
    }
    assertResult(TBool){
      typeof(empty, Binary(Ne, N(1), N(2)))
    }
    assertResult(TBool){
      typeof(empty, Binary(Ne, B(true), B(false)))
    }
    assertResult(TBool){
      typeof(empty, Binary(Ne, S("hi"), S("yo")))
    }
    assertResult(TBool){
      typeof(empty, Binary(Ne, Undefined, Undefined))
    }


  }

  "DoAssignField" should "return the right hand expression" in {
    val(m, ao) = step(Obj(Map("a"-> N(1), "b" -> B(false))))(memempty)
    assert(ao== A(1))
    val(m2, r2) = step(Assign(GetField(ao, "a"), N(2)))(m)
    assert(r2==N(2))
  }

  it should "throw otherwise" in {
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Eq, N(1), S("2")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Eq, B(true), S("2")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Eq, S("undefined"), Undefined))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Ne, N(1), S("2")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Ne, B(true), S("2")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(Ne, S("undefined"), Undefined))
    }
  }

  "TypeAndOr" should "take two bools" in {
    assertResult(TBool){
      typeof(empty,Binary(And, B(true), B(false)))
    }
    assertResult(TBool){
      typeof(empty,Binary(And, B(false), B(false)))
    }
    assertResult(TBool){
      typeof(empty,Binary(Or, B(true), B(false)))
    }
    assertResult(TBool){
      typeof(empty,Binary(Or, B(false), B(false)))
    }
  }

  it should "throw otherwise" in {
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(And, B(true), S("hello")))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(And, B(true), N(1)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, Binary(And, N(2), B(false)))
    }
  }

  "TypeIf" should "Only allow bool for e1" in {
    assertResult(TBool){
      typeof(empty, If(B(true), B(true), B(false)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, If(N(1), B(true), B(false)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, If(S("HI"), B(true), B(false)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, If(Undefined, B(true), B(false)))
    }
  }

  it should "Enforce the same type of e2 and e3" in {
    assertResult(TString){
      typeof(empty, If(B(true), S("hi"), S("bye")))
    }
    assertResult(TNumber){
      typeof(empty, If(B(true), N(2), N(3)))
    }
    assertResult(TUndefined){
      typeof(empty, If(B(true), Undefined, Undefined))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, If(B(true), B(true), N(2)))
    }
    assertThrows[StaticTypeError]{
      typeof(empty, If(B(true), S("hi"), N(2)))
    }
  }

  "TypeDecl" should "return the type given the environment" in {
    assertResult(TNumber){
      typeof(empty, Decl(MConst, "x", N(30), Var("x")))
    }
    assertResult(TBool){
      typeof(empty, Decl(MConst, "x", B(true), Binary(And,Var("x"), Var("x"))))
    }
  }

  "TypeFunction" should "perform Typefunc" in {
    assertResult(TFunction(List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber))), TNumber)) {
      typeof(empty, parse("function fst(x: number, y: number): number { return x }"))
    }
    assertResult(TFunction(List(("x", MTyp(MConst, TBool)), ("y", MTyp(MConst, TNumber))), TBool)) {
      typeof(empty, parse("function fst(x: bool, y: number): bool { return x }"))
    }

    assertResult(TFunction(List(("x", MTyp(MConst, TNumber))), TNumber)) {
      typeof(empty, parse("function (x: number) { return x }"))
    }
  }



  "TypeCall" should "perform typecall" in {
    val params = List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber)))
    val e = Binary(Plus, Var("x"), Var("y"))
    assertResult(TNumber) {
      typeof(empty, Call(Function(None, params, None, e),List(N(1), N(2))))
    }
  }

  it should "throw if types don't match" in {
    val params = List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber)))
    val e = Binary(Plus, Var("x"), Var("y"))
    assertThrows[StaticTypeError]{
      typeof(empty, Call(Function(None, params, None, e),List(B(true), N(2))))
    }
  }

  "TypeObject" should "perform typeobject" in {
    val obj = Obj(Map("a" -> N(3), "b" -> B(true), "c" -> S("hi"), "u" -> Undefined))
    assertResult(TObj(Map("a" -> TNumber, "b" -> TBool, "c" -> TString, "u" -> TUndefined))){
      typeof(empty, obj)
    }
  }

  it should "support nesting" in {
    val obj = Obj(Map("a" -> N(3), "o" -> Obj(Map("a" -> B(false)))))
    assertResult(TObj(Map("a" -> TNumber, "o" -> TObj(Map("a" -> TBool))))){
      typeof(empty, obj)
    }
  }

  "TypeGetField" should "do its job" in {
    val obj = Obj(Map("a" -> N(3), "b" -> B(true), "c" -> S("hi"), "u" -> Undefined))
    assertResult(TUndefined){
      typeof(empty, GetField(obj, "u"))
    }
  }

  it should "also support nesting" in {
    val obj = Obj(Map("a" -> N(3), "o" -> Obj(Map("a" -> B(false)))))
    assertResult(TBool){
      typeof(empty, GetField(GetField(obj, "o"), "a"))
    }
  }

  "TypeAssignVar" should "check for types in the type environment" in {
    val tenv: TEnv = Map[String,MTyp](("x" -> MTyp(MVar, TNumber)))
    assertResult(TNumber){
      typeof(tenv, Assign(Var("x"), N(3)))
    }
  }

  it should "throw if the type doesn't match" in {
    val tenv: TEnv = Map[String,MTyp](("x" -> MTyp(MVar, TNumber)))
    assertThrows[StaticTypeError]{
      typeof(tenv, Assign(Var("x"), S("Hi")))
    }
  }

  "TypeAssignField" should "make sure the types match" in {
    val obj = Obj(Map("a" -> N(3), "b" -> B(true), "c" -> S("hi"), "u" -> Undefined))
    assertResult(TNumber){
      typeof(empty, Assign(GetField(obj, "a"), N(4)))
    }
    assertResult(TBool){
      typeof(empty, Assign(GetField(obj, "b"), B(false)))
    }
  }

  it should "throw otherwise" in {
    val obj = Obj(Map("a" -> N(3), "b" -> B(true), "c" -> S("hi"), "u" -> Undefined))
    assertThrows[StaticTypeError]{
      typeof(empty, Assign(GetField(obj, "a"), B(true)))
    }
  }
  // Probably want to write some tests for castOk, typeInfer, substitute, and step.

  "TypeDecl" should "allow declarations that make sense" in {
    assertResult(TNumber){
      typeof(empty, Decl(MConst, "x", N(1), Var("x")))
    }
    assertResult(TBool){
      typeof(empty, Decl(MName, "x", Binary(And, B(true), B(true)), Var("x")))
    }
    assertResult(TString){
      typeof(empty, Decl(MVar, "x", Binary(Plus, S("h"), S("i")), Var("x")))
    }
    val tenv: TEnv = Map[String,MTyp](("x" -> MTyp(MVar, TNumber)))
    assertResult(TNumber){
      typeof(tenv, Decl(MRef, "r", Var("x"), Var("r")))
    }

    val tenv1: TEnv = Map[String,MTyp](("o" -> MTyp(MConst, TObj(Map("a" -> TBool)))))
    val o = Obj(Map(("a" -> B(true))))
    assertResult(TBool){
      typeof(tenv1, Decl(MRef, "r", GetField(o, "a"), Var("r")))
    }
  }

  it should "not allow bindings to nonexistent variables" in {
    assertThrows[NoSuchElementException]{
      typeof(empty, Decl(MConst, "x", Var("y"), Var("x")))
    }
    assertThrows[NoSuchElementException]{
      typeof(empty, Decl(MName, "x", Var("y"), Var("x")))
    }
    assertThrows[NoSuchElementException]{
      typeof(empty, Decl(MVar, "x", Var("y"), Var("x")))
    }
    assertThrows[NoSuchElementException]{
      typeof(empty, Decl(MRef, "x", Var("y"), Var("x")))
    }
  }

  it should "not allow binding refs to rvalues" in {
    assertThrows[StaticTypeError]{
      typeof(empty, Decl(MRef, "r", N(3), Var("r")))
    }
  }

}

// An adapter class to pass in your Lab5 object.
class Lab5SpecRunner extends Lab5Spec(jsy.student.Lab5)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab5.
// The test expects a corresponding .ans file with the expected result.
class Lab5JsyTests extends JavascriptyTester(None, "lab5", jsy.student.Lab5)

class Lab5Suite extends Suites(
  new Lab5SpecRunner,
  new Lab5JsyTests
)