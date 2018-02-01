package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */

  // O Set é o nome dado a uma função que por cada inteiro retorna um booleano é uma especie de aberviatura
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */

  // def contains(Int => Boolean, Int) => Boolean, ou seja, se o elemento pertence iremos
  // ter um true (s(elem) = elem => boolean)
  //def contains(s: Set, elem: Int): Boolean = s(elem)
  def contains(s: Int => Boolean, elem: Int): Boolean = s(elem)


//
  /**
   * Returns the set of the one given element.
   */

    // (Int)->(int=>boolean) = (elem == x recebido) mete no Set a condição pretendida
    def singletonSet(elem: Int): Set = (x: Int) => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */

    // Pegamos em duas regras Int para bool e fazemos a uniao das duas. p.ex: (x==2 ? || x==3) como queremos uma união
    // tem de dar true num caso ou noutro ||
    def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */


    // Neste caso apenas queremos os elementos presentes nos dois logo uma condição && falha sempre que um falhe
    def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  //elementos que estão em s (true) e (&&) não estão em t(!)
    def diff(s: Set, t: Set): Set  = (x: Int) => s(x) && !t(x)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  // filtro no grupo todos os elementos que cumprem uma regra p por exemplo todos os numeros pares (x%2)
  // não percebo porque aqui não usaram a abstração usual alterei e deu igual.....
    def filter(s: Set, p: Set): Set = x => contains(s,x) && p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */


    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a>bound) true
      else if (s(a) && !p(a)) false // se encontrar um elemento que não cumpra a regra sai fora
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */

   // temos uma função que retorna true se todos os elementos cumprem a regra.
   // queremos saber se pelo menos um elemento cumpre a regra. Podemos inverter a logica e negar ambos os lados
   // pois um elemento que cumpra a regra negado ira fazer a regra falhar.
   def exists(s: Set, p: Int => Boolean): Boolean = {

            !forall((x:Int) => s(x),(x:Int) => !p(x))

   }
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {

      (x:Int) => exists(s, y => f(y) == x) // todos os y que satisfaçam a conta f(x) ficam verdadeiros

    }

  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
