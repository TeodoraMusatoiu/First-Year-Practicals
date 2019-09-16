// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet :Node = null // or however empty set is represented

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = {
    var str = new String
    str = "{"
    var n = this
    while(n.next != null){ 
      str = str + n.next.datum
      if(n.next.next != null) str = str + ", "
      n = n.next
    }
    str = str + "}"
    str
  }

  /** Returns the last node less than the element. */
  private def find(e: Int) : Node = {
    var n = this
    //Inv : for all n1 in L(theSet, n), n1.datum < e
    while (n.next != null && n.next.next.datum < e ) n = n.next
    n
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) = {
    var n = find(e)
    if(n.next.datum != e){
      val n1 = Node(e, n.next)
      n.next = n1
    }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = {
    var n = this
    var num = 0
    //Inv : num = the number of nodes until node n
    while(n.next != null){
      num += 1
      n=n.next
    }
    num
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    val n = find(e)
    if(n.next.datum == e) true
    else false
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    assert (this.size > 0)
    val n = this
    n.next.datum
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
      var found = true 
      if (s.size != this.size) found = false
      var n = this
       while(found && n.next != null){
        if(n.next.datum != s.next.datum)
          found = false
        n=n.next
        }
      found
      }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    val n = find(e)
    if(n.next.datum == e){
      n.next = n.next.next
        true 
    }
    else false
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    var found = true
    val n = theSet
    while (n.next != null){
      if( ! that.contains(n.next.datum) )
        found = false
    }
    found
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
  	var n = this
  	var m = that
  	while(m.next != null){
  		if(! n.contains(m.next.datum))
  			this.add(m.next.datum)			
  		m = m.next
  	}
  	n
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
  	var n = this
  	var cp = this
  	while(n.next != null){
  		if(! that.contains(n.next.datum))
  			cp.remove(n.next.datum)
  		n=n.next
  	}
  	cp
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = ???

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = ???
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
