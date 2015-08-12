
import scala.math.min
import scala.Array.ofDim
import scala.collection.mutable.Map


object StringDistance {

  def levenshtein_1(s1: String, s2: String): Int = {
    def minimum(i: Int, ii: Int, iii: Int) = min(min(i, ii), iii)
    val dist = ofDim[Int](s1.length + 1, s2.length + 2)
    for (i <- 0 to s1.length)
      dist(i)(0) = i
    for (j <- 0 to s2.length)
      dist(0)(j) = j

    for (i <- 1 to s1.length; j <- 1 to s2.length)
      dist(i)(j) = minimum(dist(i - 1)(j) + 1,
        dist(i)(j - 1) + 1,
        dist(i - 1)(j - 1) + (if (s1(i - 1) == s2(j - 1)) 0 else 1))

    dist(s1.length)(s2.length)
  }


  def levenshtein_2(s1: String, s2: String): Int = {
    //helper method
    def minimum(i: Int, ii: Int, iii: Int) = min(min(i, ii), iii)

    //instead of a two 2d arrays of n1+1*n2+1 we use two 1d arrays of dim n+1
    //this is a tuple in the form of (newDistance,oldDistance)
    var dist = (new Array[Int](s1.length+1),new Array[Int](s1.length+1))

    //initialize the old distance array to the first loop values in _1
    for (i <- 0 to s1.length)
      dist._2(i) = i

    //loop through the length of the second string
    for(j <- 1 to s2.length){
      //get the tuple elements referenced by name
      val (newDist,oldDist) = dist
      //set the first position to the value of j like second loop in _1
      newDist(0) = j
      for(i <- 1 to s1.length)
        //now do the computation for the distance at s1[i] to s2[j]
        newDist(i) = minimum(
          oldDist(i) + 1,
          newDist(i-1) + 1,
          oldDist(i - 1) + (if (s1(i - 1) == s2(j - 1)) 0 else 1))

      //Tuples have a nice swap method that makes the newly found distance the old distance
      dist = dist swap
    }
    dist _2 s1.length
  }

  def levenshtein_3(s1: String, s2: String): Int = {
    //use a map/table as I implemented it the first time in Java
    //to hold the list parts with cost we have found before to
    //allow us not to have to redo computation sub-sequences
    val computedTable = Map[(List[Char],List[Char]),Int]()
    def minimum(i: Int, ii: Int, iii: Int) = min(min(i, ii), iii)
    //this method gets fun
    def compute(s1:List[Char],s2:List[Char]):Int = {
      if(!computedTable.contains((s1,s2)))
        computedTable((s1,s2)) = (s1,s2) match {
        case(_,Nil) => s1 length
        case(Nil,_) => s2 length
        case(c1::xs1,c2::xs2) =>
          minimum(compute(xs1,s2)+1, compute(s1,xs2)+1,
            compute(xs1,xs2)+(if(c1==c2) 0 else 1))
      }
      computedTable((s1,s2))

    }

    compute(s1 toList, s2 toList)
  }

}

