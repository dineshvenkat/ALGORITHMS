
package algos.string

object StringAlgos {


  //Max occurence of a word in a sentence 
  def mostFrequentWords(s: String, n: Int): List[String] =
    s.split(" ").groupBy(w => w).mapValues(_.size).toList.sortBy(-_._2).map(_._1).take(n)

}