package jsy.student

import jsy.lab2.Lab2Like
import jsy.lab2.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

/*
 * This file contains a number of *Spec classes that define a set of
 * tests.
 *
 * All of the tests are gathered together in Lab2Suite.
 */

class Lab2Spec(lab2: Lab2Like) extends FlatSpec {
  import lab2._

  "And" should "return true only if both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(And,t,t)) === t)
    assert(eval(Binary(And,t,f)) === f)
    assert(eval(Binary(And,f,t)) === f)
    assert(eval(Binary(And,f,f)) === f)
  } 
 
  it should "return non-intuitive results from differing types" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(5))
  }


  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("foo"))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S(""))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("foo"))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S(""))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite B true undefined" in {
    val e1 = B(true)
    val e2 = Undefined
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite B false N zero" in {
    val e1 = B(false)
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(false))
  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("foo"))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === S(""))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero B false v2" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite N N greater than" in {
    val e1 = N(3)
    val e2 = N(5)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N undefined" in {
    val e1 = N(5)
    val e2 = Undefined
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite N zero undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }

  // undefined

  it should "test suite Undefined S word" in {
    val e1 = Undefined
    val e2 = S("foo")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined S number" in {
    val e1 = Undefined
    val e2 = S("5")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined S empty" in {
    val e1 = Undefined
    val e2 = S("")
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined B true" in {
    val e1 = Undefined
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined B false" in {
    val e1 = Undefined
    val e2 = B(false)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined true N" in {
    val e1 = Undefined
    val e2 = N(3)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined N zero" in {
    val e1 = Undefined
    val e2 = N(0)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined undefined" in {
    val e1 = Undefined
    val e2 = Undefined
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === Undefined)
  }


  "Or" should "return true if either or both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(Or,t,t)) === t)
    assert(eval(Binary(Or,f,t)) === t)
    assert(eval(Binary(Or,t,f)) === t)
    assert(eval(Binary(Or,f,f)) === f)
  }

  it should "test suite B true undefined" in {
    val e1 = B(true)
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S empty and undefined" in {
    val e1 = S("")
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite B false undefined" in {
    val e1 = B(false)
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite N 0 and undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite N undefined" in {
    val e1 = N(5)
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 ===  N(5))
  }

  it should "test suite N zero undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === Undefined)
  }

  it should "test suite Undefined S word" in {
    val e1 = Undefined
    val e2 = S("foo")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("foo"))
  }

  it should "test suite Undefined S number" in {
    val e1 = Undefined
    val e2 = S("5")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite Undefined S empty" in {
    val e1 = Undefined
    val e2 = S("")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S(""))
  }

  it should "test suite Undefined B true" in {
    val e1 = Undefined
    val e2 = B(true)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite Undefined B false" in {
    val e1 = Undefined
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined true N" in {
    val e1 = Undefined
    val e2 = N(3)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite Undefined N zero" in {
    val e1 = Undefined
    val e2 = N(0)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite Undefined undefined" in {
    val e1 = Undefined
    val e2 = Undefined
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === Undefined)
  }
  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }



  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("foo"))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === S(""))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(0))
  }

  "Plus" should "add two number values and return a number" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(5))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("55"))
  }


  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5foo"))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("55"))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5true"))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5false"))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("53"))
  }

  it should "test suite S N dec" in {
    val e1 = S("5")
    val e2 = N(3.33)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("53.33"))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("50"))
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5undefined"))
  }

  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("truefoo"))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("true5"))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("true"))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(2))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(4))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(1))
  }

//  it should "test suite B true undefined" in {
//    val e1 = B(true)
//    val e2 = Undefined
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 == N(Double.NaN))
//  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5foo"))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("0foo"))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("55"))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("05"))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("5"))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("0"))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(6))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(8))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(0))
  }

//  it should "test suite N undefined" in {
//    val e1 = N(5)
//    val e2 = Undefined
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

//  it should "test suite N zero undefined" in {
//    val e1 = N(0)
//    val e2 = Undefined
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  // undefined

  it should "test suite Undefined S word" in {
    val e1 = Undefined
    val e2 = S("foo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("undefinedfoo"))
  }

  it should "test suite Undefined S number" in {
    val e1 = Undefined
    val e2 = S("5")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("undefined5"))
  }

  it should "test suite Undefined S empty" in {
    val e1 = Undefined
    val e2 = S("")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("undefined"))
  }

//  it should "test suite Undefined B true" in {
//    val e1 = Undefined
//    val e2 = B(true)
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B false" in {
//    val e1 = Undefined
//    val e2 = B(false)
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined true N" in {
//    val e1 = Undefined
//    val e2 = N(3)
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined N zero" in {
//    val e1 = Undefined
//    val e2 = N(0)
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined undefined" in {
//    val e1 = Undefined
//    val e2 = Undefined
//    val e3 = eval(Binary(Plus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  "Minus" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

//  it should "test suite S S word" in {
//    val e1 = S("5")
//    val e2 = S("foo")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(4))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

//  it should "test suite S undefined" in {
//    val e1 = S("5")
//    val e2 = Undefined
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  //b

//  it should "test suite B true S word" in {
//    val e1 = B(true)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-4))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-2))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(1))
  }

//  it should "test suite B true undefined" in {
//    val e1 = B(true)
//    val e2 = Undefined
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  // n

//  it should "test suite N S word" in {
//    val e1 = N(5)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

//  it should "test suite N zero S word" in {
//    val e1 = N(0)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-5))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(4))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-1))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-3))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(0))
  }

//  it should "test suite N undefined" in {
//    val e1 = N(5)
//    val e2 = Undefined
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N zero undefined" in {
//    val e1 = N(0)
//    val e2 = Undefined
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  // undefined
//
//  it should "test suite Undefined S word" in {
//    val e1 = Undefined
//    val e2 = S("foo")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S number" in {
//    val e1 = Undefined
//    val e2 = S("5")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S empty" in {
//    val e1 = Undefined
//    val e2 = S("")
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B true" in {
//    val e1 = Undefined
//    val e2 = B(true)
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B false" in {
//    val e1 = Undefined
//    val e2 = B(false)
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined true N" in {
//    val e1 = Undefined
//    val e2 = N(3)
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined N zero" in {
//    val e1 = Undefined
//    val e2 = N(0)
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined undefined" in {
//    val e1 = Undefined
//    val e2 = Undefined
//    val e3 = eval(Binary(Minus, e1, e2))
//    assert(e3 === Undefined)
//  }

  "Times" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(25))
  }

//  it should "test suite S S word" in {
//    val e1 = S("5")
//    val e2 = S("foo")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(25))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(15))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

//  it should "test suite S undefined" in {
//    val e1 = S("5")
//    val e2 = Undefined
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  //b

//  it should "test suite B true S word" in {
//    val e1 = B(true)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

//  it should "test suite B true undefined" in {
//    val e1 = B(true)
//    val e2 = Undefined
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  // n
//
//  it should "test suite N S word" in {
//    val e1 = N(5)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N zero S word" in {
//    val e1 = N(0)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 ===N(Double.NaN))
//  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(25))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(15))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(0))
  }
//
//  it should "test suite N undefined" in {
//    val e1 = N(5)
//    val e2 = Undefined
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N zero undefined" in {
//    val e1 = N(0)
//    val e2 = Undefined
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  // undefined
//
//  it should "test suite Undefined S word" in {
//    val e1 = Undefined
//    val e2 = S("foo")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S number" in {
//    val e1 = Undefined
//    val e2 = S("5")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S empty" in {
//    val e1 = Undefined
//    val e2 = S("")
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B true" in {
//    val e1 = Undefined
//    val e2 = B(true)
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B false" in {
//    val e1 = Undefined
//    val e2 = B(false)
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined true N" in {
//    val e1 = Undefined
//    val e2 = N(3)
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined N zero" in {
//    val e1 = Undefined
//    val e2 = N(0)
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined undefined" in {
//    val e1 = Undefined
//    val e2 = Undefined
//    val e3 = eval(Binary(Times, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  "Div" should "divide two number values and return a number" in {
    val e1 = N(4)
    val e2 = N(2)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(2))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1))
  }


//  it should "test suite S S word" in {
//    val e1 = S("5")
//    val e2 = S("foo")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(5))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

  it should "test suite S N" in {
    val e1 = S("9")
    val e2 = N(3)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(3))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

//  it should "test suite S undefined" in {
//    val e1 = S("5")
//    val e2 = Undefined
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  //b
//
//  it should "test suite B true S word" in {
//    val e1 = B(true)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(0.2))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(.5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(2))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

//  it should "test suite B true undefined" in {
//    val e1 = B(true)
//    val e2 = Undefined
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  // n
//
//  it should "test suite N S word" in {
//    val e1 = N(5)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N zero S word" in {
//    val e1 = N(0)
//    val e2 = S("foo")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

//  it should "test suite N zero S empty" in {
//    val e1 = N(0)
//    val e2 = S("")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

//  it should "test suite N B true" in {
//    val e1 = N(5)
//    val e2 = B(true)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(5))
//  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

//  it should "test suite N zero B false" in {
//    val e1 = N(0)
//    val e2 = B(false)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

//  it should "test suite N N" in {
//    val e1 = N(6)
//    val e2 = N(3)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(2))
//  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(0))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

//  it should "test suite N zero N zero" in {
//    val e1 = N(0)
//    val e2 = N(0)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N undefined" in {
//    val e1 = N(5)
//    val e2 = Undefined
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite N zero undefined" in {
//    val e1 = N(0)
//    val e2 = Undefined
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  // undefined
//
//  it should "test suite Undefined S word" in {
//    val e1 = Undefined
//    val e2 = S("foo")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S number" in {
//    val e1 = Undefined
//    val e2 = S("5")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined S empty" in {
//    val e1 = Undefined
//    val e2 = S("")
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B true" in {
//    val e1 = Undefined
//    val e2 = B(true)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined B false" in {
//    val e1 = Undefined
//    val e2 = B(false)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined true N" in {
//    val e1 = Undefined
//    val e2 = N(3)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined N zero" in {
//    val e1 = Undefined
//    val e2 = N(0)
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }
//
//  it should "test suite Undefined undefined" in {
//    val e1 = Undefined
//    val e2 = Undefined
//    val e3 = eval(Binary(Div, e1, e2))
//    assert(e3 === N(Double.NaN))
//  }

  "Arithmetic Operators" should "produce non-intuitive solutions given differing expression types" in {
    val e1 = B(true)
    val e2 = N(7)
    assert(eval(Binary(Plus,e1,e2)) == N(8))
  }

  "Eq" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "return boolean for number and boolean" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 == B(false))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true undefined" in {
    val e1 = B(true)
    val e2 = Undefined
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N zero and string zero" in {
    val e1 = N(0)
    val e2 = S("0")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 == B(false))
  }

  it should "test suite N undefined" in {
    val e1 = N(5)
    val e2 = Undefined
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  // undefined

  it should "test suite Undefined S word" in {
    val e1 = Undefined
    val e2 = S("foo")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined S number" in {
    val e1 = Undefined
    val e2 = S("5")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined S empty" in {
    val e1 = Undefined
    val e2 = S("")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined B true" in {
    val e1 = Undefined
    val e2 = B(true)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined B false" in {
    val e1 = Undefined
    val e2 = B(false)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined true N" in {
    val e1 = Undefined
    val e2 = N(3)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined N zero" in {
    val e1 = Undefined
    val e2 = N(0)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined undefined" in {
    val e1 = Undefined
    val e2 = Undefined
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  //todo: review

  "Lt" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }


  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true undefined" in {
    val e1 = B(true)
    val e2 = Undefined
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N undefined" in {
    val e1 = N(5)
    val e2 = Undefined
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  // undefined

  it should "test suite Undefined S word" in {
    val e1 = Undefined
    val e2 = S("foo")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined S number" in {
    val e1 = Undefined
    val e2 = S("5")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined S empty" in {
    val e1 = Undefined
    val e2 = S("")
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined B true" in {
    val e1 = Undefined
    val e2 = B(true)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined B false" in {
    val e1 = Undefined
    val e2 = B(false)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined true N" in {
    val e1 = Undefined
    val e2 = N(3)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined N zero" in {
    val e1 = Undefined
    val e2 = N(0)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite Undefined undefined" in {
    val e1 = Undefined
    val e2 = Undefined
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Le" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "return non-intuitive resuLes from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }
  it should "(string number) and number returns numbers" in {
    val e1 = S("5")
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }


  it should "test suite S S word" in {
    val e1 = S("5")
    val e2 = S("foo")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S S number" in {
    val e1 = S("5")
    val e2 = S("5")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite S S empty" in {
    val e1 = S("5")
    val e2 = S("")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B true" in {
    val e1 = S("5")
    val e2 = B(true)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S B false" in {
    val e1 = S("5")
    val e2 = B(false)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N" in {
    val e1 = S("5")
    val e2 = N(3)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S N zero" in {
    val e1 = S("5")
    val e2 = N(0)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite S undefined" in {
    val e1 = S("5")
    val e2 = Undefined
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  //b

  it should "test suite B true S word" in {
    val e1 = B(true)
    val e2 = S("foo")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true S number" in {
    val e1 = B(true)
    val e2 = S("5")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true S empty" in {
    val e1 = B(true)
    val e2 = S("")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true B true" in {
    val e1 = B(true)
    val e2 = B(true)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true B false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true N" in {
    val e1 = B(true)
    val e2 = N(3)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite B true N zero" in {
    val e1 = B(true)
    val e2 = N(0)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite B true undefined" in {
    val e1 = B(true)
    val e2 = Undefined
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  // n

  it should "test suite N S word" in {
    val e1 = N(5)
    val e2 = S("foo")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S word" in {
    val e1 = N(0)
    val e2 = S("foo")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N S number" in {
    val e1 = N(5)
    val e2 = S("5")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N zero S number" in {
    val e1 = N(0)
    val e2 = S("5")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N S empty" in {
    val e1 = N(5)
    val e2 = S("")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero S empty" in {
    val e1 = N(0)
    val e2 = S("")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N B true" in {
    val e1 = N(5)
    val e2 = B(true)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B true" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N B false" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero B false" in {
    val e1 = N(0)
    val e2 = B(false)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N N" in {
    val e1 = N(5)
    val e2 = N(3)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N N zero" in {
    val e1 = N(5)
    val e2 = N(0)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero N zero" in {
    val e1 = N(0)
    val e2 = N(0)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "test suite N undefined" in {
    val e1 = N(5)
    val e2 = Undefined
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "test suite N zero undefined" in {
    val e1 = N(0)
    val e2 = Undefined
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

    // undefined

    it should "test suite Undefined S word" in {
      val e1 = Undefined
      val e2 = S("foo")
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined S number" in {
      val e1 = Undefined
      val e2 = S("5")
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined S empty" in {
      val e1 = Undefined
      val e2 = S("")
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined B true" in {
      val e1 = Undefined
      val e2 = B(true)
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined B false" in {
      val e1 = Undefined
      val e2 = B(false)
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined true N" in {
      val e1 = Undefined
      val e2 = N(3)
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined N zero" in {
      val e1 = Undefined
      val e2 = N(0)
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined undefined" in {
      val e1 = Undefined
      val e2 = Undefined
      val e3 = eval(Binary(Le, e1, e2))
      assert(e3 === B(false))
    }

    "Gt" should "return true if the first expression is greater than the second" in {
      val e1 = N(8)
      val e2 = N(7)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "return false if the first expression is not strictly greater than the second" in {
      val e1 = N(4)
      val e2 = N(5)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "return false if two number values are the same" in {
      val e1 = N(5)
      val e2 = N(5)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }


    it should "return false if the first expression is greater than the second" in {
      val e1 = N(7)
      val e2 = N(5)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "return non-intuitive results alphabetic strings" in {
      val e1 = S("z")
      val e2 = S("a")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 == B(true))
    }

    it should "return non-intuitive results from differing types" in {
      val e1 = N(5)
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }
    it should "(string number) and number returns numbers" in {
      val e1 = S("5")
      val e2 = N(5)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite S S word" in {
      val e1 = S("5")
      val e2 = S("foo")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite S S number" in {
      val e1 = S("5")
      val e2 = S("5")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite S S empty" in {
      val e1 = S("5")
      val e2 = S("")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S B true" in {
      val e1 = S("5")
      val e2 = B(true)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S B false" in {
      val e1 = S("5")
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S N" in {
      val e1 = S("5")
      val e2 = N(3)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S N zero" in {
      val e1 = S("5")
      val e2 = N(0)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S undefined" in {
      val e1 = S("5")
      val e2 = Undefined
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    //b

    it should "test suite B true S word" in {
      val e1 = B(true)
      val e2 = S("foo")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true S number" in {
      val e1 = B(true)
      val e2 = S("5")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true S empty" in {
      val e1 = B(true)
      val e2 = S("")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true B true" in {
      val e1 = B(true)
      val e2 = B(true)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true B false" in {
      val e1 = B(true)
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true N" in {
      val e1 = B(true)
      val e2 = N(3)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true N zero" in {
      val e1 = B(true)
      val e2 = N(0)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true undefined" in {
      val e1 = B(true)
      val e2 = Undefined
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    // n

    it should "test suite N S word" in {
      val e1 = N(5)
      val e2 = S("foo")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N zero S word" in {
      val e1 = N(0)
      val e2 = S("foo")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N S number" in {
      val e1 = N(5)
      val e2 = S("5")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N zero S number" in {
      val e1 = N(0)
      val e2 = S("5")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N S empty" in {
      val e1 = N(5)
      val e2 = S("")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero S empty" in {
      val e1 = N(0)
      val e2 = S("")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N B true" in {
      val e1 = N(5)
      val e2 = B(true)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero B true" in {
      val e1 = N(0)
      val e2 = B(true)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N B false" in {
      val e1 = N(5)
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero B false" in {
      val e1 = N(0)
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite N N" in {
      val e1 = N(5)
      val e2 = N(3)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero N" in {
      val e1 = N(0)
      val e2 = N(3)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite N N zero" in {
      val e1 = N(5)
      val e2 = N(0)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero N zero" in {
      val e1 = N(0)
      val e2 = N(0)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite N undefined" in {
      val e1 = N(5)
      val e2 = Undefined
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N zero undefined" in {
      val e1 = N(0)
      val e2 = Undefined
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    //      // undefined
    //
    it should "test suite Undefined S word" in {
      val e1 = Undefined
      val e2 = S("foo")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite Undefined S number" in {
      val e1 = Undefined
      val e2 = S("5")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined S empty" in {
      val e1 = Undefined
      val e2 = S("")
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite Undefined B true" in {
      val e1 = Undefined
      val e2 = B(true)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined B false" in {
      val e1 = Undefined
      val e2 = B(false)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined true N" in {
      val e1 = Undefined
      val e2 = N(3)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite Undefined N zero" in {
      val e1 = Undefined
      val e2 = N(0)
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined undefined" in {
      val e1 = Undefined
      val e2 = Undefined
      val e3 = eval(Binary(Gt, e1, e2))
      assert(e3 === B(false))
    }
    //
    "Ge" should "return true if the first expression is greater than the second" in {
      val e1 = N(8)
      val e2 = N(7)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "return non-intuitive resuGes from differing types" in {
      val e1 = N(5)
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "return false if the first expression is not strictly greater than the second" in {
      val e1 = N(4)
      val e2 = N(5)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "return false if two number values are the same" in {
      val e1 = N(5)
      val e2 = N(5)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    //
    it should "return false if the first expression is greater than the second" in {
      val e1 = N(7)
      val e2 = N(5)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "return true if two number values are the same" in {
      val e1 = N(5)
      val e2 = N(5)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "return non-intuitive results from differing types v1" in {
      val e1 = N(5)
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    it should "(string number) and number returns numbers" in {
      val e1 = S("5")
      val e2 = N(5)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    //
    it should "test suite S S word" in {
      val e1 = S("5")
      val e2 = S("foo")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite S S number" in {
      val e1 = S("5")
      val e2 = S("5")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S S empty" in {
      val e1 = S("5")
      val e2 = S("")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "test suite S B true" in {
      val e1 = S("5")
      val e2 = B(true)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S B false" in {
      val e1 = S("5")
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S N" in {
      val e1 = S("5")
      val e2 = N(3)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "test suite S N zero" in {
      val e1 = S("5")
      val e2 = N(0)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite S undefined" in {
      val e1 = S("5")
      val e2 = Undefined
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
    //
    //  //b
    //
    it should "test suite B true S word" in {
      val e1 = B(true)
      val e2 = S("foo")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true S number" in {
      val e1 = B(true)
      val e2 = S("5")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true S empty" in {
      val e1 = B(true)
      val e2 = S("")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true B true" in {
      val e1 = B(true)
      val e2 = B(true)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true B false" in {
      val e1 = B(true)
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "test suite B true N" in {
      val e1 = B(true)
      val e2 = N(3)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite B true N zero" in {
      val e1 = B(true)
      val e2 = N(0)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite B true undefined" in {
      val e1 = B(true)
      val e2 = Undefined
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
    //
    //  // n
    //
    it should "test suite N S word" in {
      val e1 = N(5)
      val e2 = S("foo")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N zero S word" in {
      val e1 = N(0)
      val e2 = S("foo")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N S number" in {
      val e1 = N(5)
      val e2 = S("5")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero S number" in {
      val e1 = N(0)
      val e2 = S("5")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite N S empty" in {
      val e1 = N(5)
      val e2 = S("")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero S empty" in {
      val e1 = N(0)
      val e2 = S("")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N B true" in {
      val e1 = N(5)
      val e2 = B(true)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero B true" in {
      val e1 = N(0)
      val e2 = B(true)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N B false" in {
      val e1 = N(5)
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "test suite N zero B false" in {
      val e1 = N(0)
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N N" in {
      val e1 = N(5)
      val e2 = N(3)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N zero N" in {
      val e1 = N(0)
      val e2 = N(3)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N N zero" in {
      val e1 = N(5)
      val e2 = N(0)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }
    //
    it should "test suite N zero N zero" in {
      val e1 = N(0)
      val e2 = N(0)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(true))
    }

    it should "test suite N undefined" in {
      val e1 = N(5)
      val e2 = Undefined
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite N zero undefined" in {
      val e1 = N(0)
      val e2 = Undefined
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
    //
    //    // undefined
    //
    it should "test suite Undefined S word" in {
      val e1 = Undefined
      val e2 = S("foo")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined S number" in {
      val e1 = Undefined
      val e2 = S("5")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined S empty" in {
      val e1 = Undefined
      val e2 = S("")
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined B true" in {
      val e1 = Undefined
      val e2 = B(true)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
    //
    it should "test suite Undefined B false" in {
      val e1 = Undefined
      val e2 = B(false)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined true N" in {
      val e1 = Undefined
      val e2 = N(3)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined N zero" in {
      val e1 = Undefined
      val e2 = N(0)
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }

    it should "test suite Undefined undefined" in {
      val e1 = Undefined
      val e2 = Undefined
      val e3 = eval(Binary(Ge, e1, e2))
      assert(e3 === B(false))
    }
//
  "Comparisons" should "produce non-intuitive results given the expressions given" in {
    val e1 = N(5)
    val e2 = Undefined
    assert(eval(Binary(Eq,e1,e2)) === B(false))
  }

  "ConstDecl" should "extend the environment with the first expression results bound to the identifier, and then eval the second expression" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = eval(ConstDecl("x", e1, e2))
    assert(e3 === N(4))
  }

  "If" should "eval the first expression if the conditional is true" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(true), e1, e2))
    assert(e3 === N(5))
  }

  it should "eval the second expression if the conditional is false" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(false), e1, e2))
    assert(e3 === N(2))
  }

  it should "return else if condition empty string " in {
    val e1 = S("")
    val e2 = S("x")
    val e3 = S("y")
    assert(e3 == eval(If(e1, e2, e3)))
  }

  it should "return else condition" in {
    val e1 = Binary(Gt, N(5), S("7"))
    val e2 = Binary(Times, Binary(Plus, N(3), S("2")), S("3"))
    val e3 = eval(If(B(false), e1, e2))
    assert(e3 == N(96))
  }

  "Seq" should "execute the first expression, followed by the second, and should eval to the second expression" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(Binary(Seq, e1, e2))
    assert(e3 === N(2))
  }

  "Neg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(-5))
  }

//  it should "undefined to NaN" in {
//    val e1 = Undefined
//    val e2 = eval(Unary(Neg, e1))
//    assert(e2 === N(Double.NaN))
//  }

//  it should "string to NaN" in {
//    val e1 = Undefined
//    val e2 = eval(Unary(Neg, e1))
//    assert(e2 === N(Double.NaN))
//  }

  it should "boolean true to -1" in {
    val e1 = B(true)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(-1))
  }

  it should "boolean false to 0" in {
    val e1 = B(false)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(0))
  }

  "Not" should "return the compliment of a boolean value" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Unary(Not, e1))
    val e4 = eval(Unary(Not, e2))
    assert(e3 === B(false))
    assert(e4 === B(true))
  }

}

// An adapter class to pass in your Lab2 object.
class Lab2SpecRunner extends Lab2Spec(Lab2)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab2.
// The test expects a corresponding .ans file with the expected result.
class Lab2JsyTests extends JavascriptyTester(None, "lab2", Lab2)

class Lab2Suite extends Suites(
  new Lab2SpecRunner,
  new Lab2JsyTests
)

