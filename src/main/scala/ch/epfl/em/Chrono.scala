package ch.epfl.em

class Chrono {
  
  private var currentCount = 0l
  
  private var lastStart = 0l
  
  def start { lastStart = System.currentTimeMillis() }
  def stop { currentCount += (System.currentTimeMillis() - lastStart)}
  
  def reset {currentCount = 0l}
  
  def count = currentCount
}

object EChrono extends Chrono
object MChrono extends Chrono
object GChrono extends Chrono