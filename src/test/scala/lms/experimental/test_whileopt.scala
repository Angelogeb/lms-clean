package lms.experimental

import lms.TutorialFunSuite
import lms.collection.StreamDSL
import lms.core.stub.{DslDriver, DslDriverCPP, DslGen}
import lms.core.virtualize
import lms.macros.SourceContext

class WhileOpt extends TutorialFunSuite {
  val under = ""

  abstract class DslDriverStreams[A: Manifest, B: Manifest] extends DslDriver[A,B] with StreamDSL {q =>
    override val codegen = new DslGen with StreamDSL {
      val IR: q.type = q
    }

    def sum[T:Numeric:Manifest] = (a: Rep[T]) => (e: Rep[T]) => a + e
    def mul[T:Numeric:Manifest] = (a: Rep[T]) => (e: Rep[T]) => a * e

    def arange(l: Rep[Int]): Rep[Array[Int]] = {
      val a1 = NewArray[Int](l)
      for (i <- 0 until l: Rep[Range]) {
        a1(i) = i
      }
      a1
    }
  }

  abstract class DslDriverCPPLib[A: Manifest, B: Manifest] extends DslDriverCPP[A, B] {

    @virtualize
    def initialize(len: Rep[Int]) = fun { (a: Rep[Array[Int]]) =>
      var i = 0
      while (i < len) {
        a(i) = i
        i += 1
      }
      a
    }

    @virtualize
    def l2(len: Rep[Int]) = fun { (a1: Rep[Array[Int]], a2: Rep[Array[Int]]) =>
      var i = 0
      var diff = 0
      while (i < len) {
        val tmp = a1(i) - a2(i)
        diff += tmp * tmp
        i += 1
      }
      diff
    }

    @virtualize
    def normalized(len: Rep[Int]) = fun { (a: Rep[Array[Int]]) =>
      var i = 0
      var max = a(0)
      while (i < len) {
        max = if (a(i) > max) a(i) else max
      }
      val res = NewArray[Int](len)
      i = 0
      while (i < len) {
        res(i) = a(i) / max
        i += 1
      }
      res
    }
  }

  test("dotProd") {
    val dotProd = new DslDriverStreams[Int, Int] {
      @virtualize
      def snippet(len: Rep[Int]): Rep[Int] = {
        val a1 = arange(len)
        Stream(a1).zip(mul, Stream(a1)).fold(0, sum)
      }
    }

    assert(dotProd.eval(10) == 285)
  }

  test("rw") {
    val code = new DslDriverStreams[Int, Unit] {
      @virtualize
      def snippet(len: Rep[Int]): Rep[Unit] = {
        val a1 = arange(len)
        var i = 0
        while (i < len - 1) {
          a1(i + 1) = a1(i)
          i = i + 1
        }

        println(a1(4))
      }
    }
    println(indent(code.code))
  }

  test("pureNormalized") {
    val code = new DslDriverCPPLib[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val t1 = NewArray[Int](arg)
        val t2 = NewArray[Int](arg)
        initialize(arg)(t1)
        initialize(arg)(t2)

        var iter = 0
        val maxIter = 100
        while (l2(arg)(t1, normalized(arg)(t2)) < 1 && iter < maxIter) {
          t1(iter % arg) = 0
          println(iter) // FIXME: Needed otherwise whole computation DCEd ?
          iter += 1
        }

        println(t1)
      }
    }

    println(indent(code.code))
  }

  test("impureNormalized") {
    val code = new DslDriverCPPLib[Int, Unit] {
      @virtualize
      def snippet(len: Rep[Int]) = {
        val t1 = NewArray[Int](len)
        val t2 = NewArray[Int](len)
        initialize(len)(t1)
        initialize(len)(t2)

        var iter = 0
        val maxIter = 100

        def normalized(len: Rep[Int]) = fun { (a: Rep[Array[Int]]) =>
          println("effect")
          super.normalized(len)(a)
        }

        while (l2(len)(t1, normalized(len)(t2)) < 1 && iter < maxIter) {
          t1(iter % len) = 0
          println(iter) // FIXME: Needed otherwise whole computation DCEd ?
          iter += 1
        }

        println(t1)
      }
    }

    println(indent(code.code))
  }

  test("moveMaxLen") {

    val code = new DslDriverStreams[Int, Unit] {
      @virtualize
      def length(len: Rep[Int]): Rep[Unit] = {
        val s1 = Stream(NewArray[Int](len))
        val streams = s1.map(l => NewArray[Int](l))
        val inp = streams.materialize(len)

        val maxLen: Rep[Array[Array[Int]] => Int] = fun { (a: Rep[Array[Array[Int]]]) =>
          var max = a(0).length
          for (i <- (0 until a.length) : Rep[Range]) {
            if (a(i).length > max) max = a(i).length
          }
          readVar(max)
        }
        val res = Stream(inp: Rep[Array[Array[Int]]]).filter(a =>
          Stream(a).fold(0, sum[Int]) > (maxLen(inp) / 2 /* THIS MOVES OUTSIDE*/)
        )

        val fin = res.fold(0, (a: Rep[Int]) => (e: Rep[Array[Int]]) =>
          a + Stream(e).fold(0, sum[Int])
        )
        println(fin)
      }

      @virtualize
      def snippet(x: Rep[Int]) = length(x)
    }
    println(indent(code.code))
  }
}