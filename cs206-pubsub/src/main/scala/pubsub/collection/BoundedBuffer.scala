package pubsub.collection

class BoundedBuffer[T](size: Int) extends AbstractBoundedBuffer[T](size) {


  override def put(e: T): Unit = this.synchronized {
    while(isFull) this.wait()

    buffer(tail) = e 
    count = count + 1
    notifyAll()
  }

  override def take(): T = this.synchronized {
    while(isEmpty) this.wait()
    
    val elem = buffer(head)
      
    head = (head + 1) % size
    count = count - 1
    notifyAll()
    elem
  }

  // - check whether the buffer is empty
  // - check whether the buffer is full
  // - get the index of tail

  def isEmpty: Boolean = count == 0

  def isFull: Boolean = size == count

  def tail: Int = (head + count) % size 

}
