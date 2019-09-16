/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

  /* Functions below here need to be implemented */

  /** Encrypt plain using key; can also be used for decryption */

  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
  	val n = plain.size
  	val k = key.size
  	var cipher = new Array[Char] (n)
  	var i = 0 ; var j = 0

  	//Invariant : cipher[0..i) is the encrypted text and 0<i<n
  	while(i<n){
  		cipher(i)=xor(plain(i),key(j))
  		i+=1
  		//Every time j hits the last character of the key, it starts again, from 0
  		j=(j+1)%k
  	}
  	//Postcondition: cipher [0..n) is the encrypted text and i = n

    cipher
  }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) = {
    val error = "Crib is not valid."
    val n = ciphertext.size
    val cr = crib.size
    var start = 0		
    //start = the starting position for testing the crib

    //Invariant: ciphertext[0..start) are the positions which are not valid for the crib 0<start<n-cr+1
    while(start < n-cr+1){
      var keyChar = new Array[Char] (cr)
      var j = 0 

      // Invariant: keyChar [0..j) contains the first j characters of the key, computed from the crib and 0<j<cr 
      while (j<cr){
      	//It is known that xor(xor(a , b), b) = a  and that text(start+j) = xor(crib(j), keyChar(j)) =>
        keyChar (j) = xor(ciphertext(start+j), crib(j))
        j+=1
      } //while
      //Postcondition: keyChar [0..n) is the key that we get from the crib and j=n
		
	  //If the keyChar Array has a sequence of repeated characters, we try to find the actual key
      if(check (keyChar)>0){
        var k = check(keyChar)
        //k is the length of the key
        val s = k-(start%k)
        //s is the position of the first character of the key in the keyChar vector
        var i = 0 
		var key = new Array [Char] (k)

		//Invariant: key[0..i) is the array of the first i characters of the key, computed from keyChar, 0<i<k
        while(i<k){
	  		var pos=s+i
	  		//pos is the current position in the keyChar array

	  		if(pos<cr)
          	 	key(i)=keyChar(pos)
          	/*when pos reaches the end of the array, it needs to be computed %cr, so it also find the
          	remaining characters of the key */
	  		else key(i)=keyChar(pos%cr+cr-k)

	  		i+=1
        } //while
        //Postcondition: key[0..k) is the key computed from the keyChar array, i=k

		println(new String(key))
		var message = new Array [Char] (n)
		//message will contain the decrypted text
		message = encrypt(key,ciphertext)
		println(new String(message))
        start = n-cr+1 
        //if a solution is found, the loop finishes
	
      } //if
      start+=1

    } //while

  }//tryCrib

  def check (keyChar: Array[Char]) : Int = {
    val k = keyChar.size
    var p = 2
    var j = 0
    // p = the current position ; j = the length of the key = the first position that satisfies the condition

    //Invariant: the characters in keyChar[0..k-p) are not a repetitive sequence
    while(p<k-1){
      var found = true 
      var i = 0 
      while(i<k-p && found){
        if(keyChar(i) != keyChar(i+p)) found = false
        i+=1
      }
      if (found) {j= p; p = k}
        else p+=1
    }
    j
  }

  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) = {
  	var n = ciphertext.size
  	var shift = 1
  	var numShift = new Array [Int] (31)
  	//This will contain the number of matches for every shift

  	for(shift <-1 to 30)
  	{
  		var i =0

  		//Invariant: numShift(shift) = the number of matches befor the ith position and 0< i < n-shift
  		while (i<n-shift)
  		{
  			if (ciphertext(i)==ciphertext(shift+i))
  				numShift(shift)+=1
  			i+=1
  		}
  		println(shift+": " + numShift(shift))
  	}
  }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) = {
  	var n = ciphertext.size
  	var s = 1
  	//s is the multiple of klen that we are checking
  	var shift = klen

  	// We check the first 20 multiples of klen
  	while (s < 20 && shift<n){
  		var i = 0

  		//Invariant: print the guessed characters of the key until the position i and 0< i < n-shift
  		while(i< n-shift){
  			if(ciphertext(i)==ciphertext(shift+i)){
  				var c = xor(ciphertext(i), ' ')
  				if(32<= c.toInt&& c.toInt <=127)
  					println(i%klen + " " + c)
  			}
  			i+=1
  		}
  		s+=1
  		shift+=klen
  	}
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) : Unit = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      println(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
