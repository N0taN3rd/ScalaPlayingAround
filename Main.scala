import scala.collection.immutable.HashMap
import scala.util.Random

object Main {

  //we have types A, B, C
  def compose[A,B,C](g: B => C, f: A => B) : A => C = g compose f

  def multiply(i: Int) : Int =  i * Random.nextInt(250)

  def flatmap[T,U](xs: List[T], f: T => Iterable[U]) = for(x <- xs; y <- f(x)) yield y

  def multiply(a: Int, b: Int): Int = a * b

  def nonEmptyList(l: List[_]): Boolean = l.nonEmpty

  def generateDivisibility(x: Int): List[(Int,Int)] =
    Stream range(2,x + 1) filter(xs => x % xs == 0) map {xs => (xs,x/xs)} toList

  def genDivFromRange(r: Range.Inclusive): List[List[(Int,Int)]] =
    r.map{ generateDivisibility }.toList.filter(nonEmptyList)

  def genDivFromRange2(r: Range.Inclusive): List[(Int,List[(Int,Int)])] =
    genDivFromRange(r) map { l => (l.head._1 * l.head._2 , l)}


  def main(args: Array[String]) {
    val f = (i: Int) => i.toString

    val g = (s: String) => s.length

    val h = (i: Int) => i * i

    //IndexSeq[Int,Int]

    val f2 =(x: Range.Inclusive) => x zip{Stream from 1}
    val g2 = (x: (Int,Int)) => (1 to x._2) map { i => multiply(x._1,i) } zip { Stream from 1 }
    val h2 = (x: IndexedSeq[(Int,Int)]) => x map { elem => multiply(elem._1, elem._2) }

    // (1 to 10) map { multiply } map { compose(h,compose(g,f)) } foreach  println
    println("-----------------")

    val a = (x: Range.Inclusive) => x zip { Stream from 1 } toList
    //val bb = (list: List[(Int,Int)],fun: (Int,Int) => IndexedSeq[(Int,Int)]) => list map {fun}
    val b = (x: List[(Int,Int)]) => x flatMap  { xs => (1 to xs._2) map { i => multiply(xs._1,i) } zip { Stream from 1 } }
    val c = (x: List[(Int,Int)]) => x map { elem => elem._1 + elem._2 }


    //def shit(listMap: ListMap[Int,(Int,Int)], t: (Int,Int)): ListMap[Int,(Int,Int)] = ((t._1*t._2),t) + listMap
    def sum(xs: List[Int]): Int = {
      xs match {
        case x :: tail => x + sum(tail)
        case Nil => 0
      }
    }


    val ret = genDivFromRange2(2 to 100)
    ret foreach println
    //flatmap((2 to 100).toList, generateDivisibility) foreach println
    // (ListMap[Int,(Int,Int)] /: ( 2 to 100  flatMap generateDivisibility)) { (lm:ListMap[Int,(Int,Int)],ii:(Int,Int)) => (ii._1 * ii._2,ii) + lm}
    /*
    val t =   2 to 100  flatMap generateDivisibility
    t foreach println

    val hMap =  (HashMap[Int,List[(Int,Int)]]() /: ( 2 to 100  flatMap generateDivisibility) )
    { (hm,t) => hm + ((t._1 * t._2, t::hm.getOrElse(t._1 * t._2, List[(Int,Int)]())))}

    println(hMap)
    */

    // def apply(fun: (Int,Int) => IndexedSeq[(Int,Int)], list: List[(Int,Int)]) = list map { fun }

    //val thisIsFun = compose(sum,compose(c,compose(b,a)))(1 to 10)
    //println(thisIsFun)
    //a(1 to 10) map { b }  map { c } map { sum }  foreach println
    //(1 to 10) map { multiply } map { compose(f,compose(g,h)) } foreach println

    //val zipped = (1 to 10) map { multiply } zip { Stream from 1 }


    // val composedReturn = compose(h2,compose(g2,f2))(1 to 10)

    //composedReturn foreach println
    /*
      for(i <- 1 to 10) {
        println(i)
        println(compose(h,compose(g,f))(i) == compose(compose(h,g),f)(i))
      }
      */

    //zipped.map( x => x._1 * x._2)

    //val zipped = Seq.fill(10)(Random.nextInt(256)) zip(Stream from 1)//foreach {e => println(e._1,e._2)}
    //zipped.foreach(println)
    // (1 to 10) foreach println
  }
}
