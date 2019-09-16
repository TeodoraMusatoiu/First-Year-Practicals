

// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey
// Amended 2017 by P.G. Jeavons

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}

/** The state of an editing session 
 *  (this class is the model part of the MVC architecture) */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** The display. */
    private var display: Display = null

    /** Register a display */
    def register(display: Display) { this.display = display }
    
    // State components that are preserved by undo and redo

    /** Current editing position. */
    private var _point = 0
    private var _mark = 0
    
    /** The string that was last copied. */
    private var _copStr = ""

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Dirty flag */
    private var modified = false

    /** Mark the buffer as modified */
    private def setModified() { modified = true }

    /** Test whether the text is modified */
    def isModified = modified
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite at next update */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at current point */
    def update() { update(point) }

    /** Update display with cursor at specified position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }


    // Accessors

    def point = _point
    def mark = _mark
    

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
        
    }
    
    def mark_=(mark: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(mark) != damage_line)
            damage = EdBuffer.REWRITE
        _mark = mark
        
    }

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }

    def copStr = _copStr
    
    private def copStr_=(copStr: String){ _copStr = copStr} 

    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }


    // Mutator methods

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.deleteChar(pos)
        if(mark>pos) mark -=1
        setModified()
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        text.deleteRange(pos, len)
        if(mark>pos+len) mark-=len
        else if(mark>pos) mark = pos
        setModified()
    }
    
    /** Insert at current value of point */
    def insert(a: Any) { 
        a match {case ch: Char => insert(point, ch)
                 case s : String => insert(point, s)
        }
    }    
  
    /** Insert a character at a specified position */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.insert(pos, ch)
        if(mark>pos) mark+=1
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        text.insert(pos, s)
        val n = s.size
        if(mark>pos) mark += n
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        text.insert(pos, s)
        val n = s.length()
        if(mark>pos) mark += n
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        text.insert(pos, t)
        val n = t.length()
        if(mark>pos) mark += n
        setModified()
    }
    
    /** Transpose 2 characters. */
    def transpose(pos:Int){
        noteDamage(true)
        val ch1 = charAt(pos) 
        deleteChar(pos)
        insert(pos+1, ch1)
        setModified()
    }
    
    /** Mark the current position. */
    def mark (pos:Int) : Int = {
        noteDamage(true)
        setModified()
        return pos
    }
    
    /** Copy the text between the marker and the current position. */
    def copy (pos:Int, m:Int) = {
      noteDamage(true)
      var min = pos
      var max = m
      if(min>max){val t = min; min = max; max = t}
      val n = max-min
      copStr = getRange(min,n).toString
      setModified()
    }
    
    /** Cut the text between the marker and the current position. */
    def cut (pos:Int, m:Int) ={
       noteDamage(true)
       var mi = pos
       var ma = m
       if(mi > ma ){val t = mi; mi = ma; ma = t}
       copStr = getRange(mi, ma-mi).toString
       deleteRange(mi, ma-mi) 
       mark = mi 
       point = mi
       setModified()
    }
    
    /** Paste the last copied range. */
    def paste (pos:Int) ={
      noteDamage(true)
      insert(pos, copStr)
      setModified()
    }
    
    
     /** Load a file into the buffer. */
    def loadFile(name: String): Boolean = {
        filename = name
        
        try {
            val in = new FileReader(name)
            text.clear()
            text.insertFile(0, in)
            in.close()
            modified = false
            noteDamage(true)
            return true
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
                return false
        }       
    }
    
    /** Save buffer contents to a file */
    def saveFile(name: String): Boolean = {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            modified = false
            return true
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write file '%s'", name)
                return false
        }
    }

    /** Make a Memento that records the current state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point. */
    class Memento {
        private val pt = point
        private val mk = mark
        
        /** Restore the state when the memento was created */
        def restore() { point = pt ; mark = mk}
    }
    
    
}
   
object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}
