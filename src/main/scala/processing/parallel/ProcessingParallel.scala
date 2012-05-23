package processing.parallel

import scala.collection.mutable.{Map, HashMap}
import scala.collection.mutable.ListBuffer

import scala.actors.{Actor, TIMEOUT, OutputChannel, Debug}
import Actor._

// To point out in paper: it's not a DSL for expressing/constructing/transforming graphs,
// but rather a high-level API for graph /processing/.
abstract class Vertex[Data](val label: String, initialValue: Data) {
  // TODO: subclasses should not be able to mutate the list!
  var neighbors: List[Vertex[Data]] = List()

  var value: Data = initialValue

  var graph: Graph[Data] = null

  // graph has to initialize `worker` upon partitioning the list of vertices
  // TODO: should not be accessible from subclasses!
  var worker: Actor = null

  /*
   * { ...
   *   List()
   * } then {
   *   ...
   * }
   */
  implicit def mkSubstep(block: => List[Message[Data]]) =
    new Substep(() => block, null)

  // directed graph for now
  // TODO: should not be accessible from subclasses!
  def connectTo(v: Vertex[Data]) {
    neighbors = v :: neighbors
  }
  
  def initialize() { }

  def update(superstep: Int, incoming: List[Message[Data]]): Substep[Data]

  override def toString = "Vertex(" + label + ")"
}

object Graph {
  var count = 0
  def nextId = {
    count += 1
    count
  }
}

// Message type to indicate to graph that it should start propagation
case class StartPropagation(numIterations: Int)

// TODO: maintain mapping from vertex to the worker which manages the vertex,
// do this using worker field of Vertex
class Graph[Data] extends Actor {
  var vertices: List[Vertex[Data]] = List()
  var workers: List[Actor] = List()
  var allForemen: List[Actor] = List()

  var cond: () => Boolean = () => false

  //Debug.level = 3

  def addVertex(v: Vertex[Data]): Vertex[Data] = {
    v.graph = this
    vertices = v :: vertices
    v
  }

  def createWorkers() {
    val numChildren = 4
    val numWorkers = 48

    val topLevel = (for (i <- 1 to 3) yield {
      val f = new Foreman(this, List())
      workers = f :: workers
      f
    }).toList

    val listOfMidLevels = for (foreman <- topLevel) yield {
      val midLevel: List[Foreman] =
        (for (i <- 1 to numChildren) yield new Foreman(foreman, List())).toList
      foreman.children = midLevel
      midLevel
    }

    val midLevel = listOfMidLevels.flatten
    allForemen = topLevel ::: midLevel

    // partition set of vertices into numWorkers partitions
    val sizeOfMostPartitions = (vertices.size.toDouble / numWorkers.toDouble).floor.toInt
    val tooManyPartitions = vertices.grouped(sizeOfMostPartitions).toList
    var additionalPartitions = tooManyPartitions.drop(numWorkers)
    val actualPartitions = tooManyPartitions.take(numWorkers)

    val partitions = for (part <- actualPartitions) yield {
      if (additionalPartitions.isEmpty)
        part
      else {
        val additionalPart = additionalPartitions.head
        additionalPartitions = additionalPartitions.tail
        part ::: additionalPart
      }
    }

/*
    val partitionSize = {
      val tmp = (vertices.size.toDouble / numWorkers.toDouble).ceil.toInt
      if (tmp == 0) 1 else tmp
    }
    println("partition size: " + partitionSize)
    val partitions = vertices.grouped(partitionSize).toList
*/
    //println("#partitions: " + partitions.size)

    var partition: List[Vertex[Data]] = List()
    for (i <- 0 until 12; j <- 0 until 4) {
      partition = partitions(i * 4 + j)
      val worker = new Worker(midLevel(i), partition, this)
      midLevel(i).children = worker :: midLevel(i).children
      allForemen ::= worker

      for (vertex <- partition) {
        vertex.worker = worker
      }
      worker.start()
    }

    for (f <- midLevel) { f.start() }
    for (f <- topLevel) { f.start() }
  }

  def act() {
    loop {
      receive {
        case "Stop" =>
          exit()

        case StartPropagation(numIterations) =>
          val parent = sender

          createWorkers()
          //for (w <- workers) { w ! "Init" }

          var crunchResult: Option[AbstractCrunchResult[Data]] = None

          var shouldFinish = false
          var i = 1
          while ((numIterations == 0 || i <= numIterations) && !shouldFinish) {
            i += 1
            // 0 superstep: i == 2
            // 1 superstep: i == 3
            // 2 superstep: i == 4
            // 3 superstep: i == 5

            // time if (i - 2) % 3 == 0
            
            if (!crunchResult.isEmpty)
              for (w <- workers) { // go to next superstep
                // OK println("send to workers")
                w ! crunchResult.get
              }
            else {
              //println("not crunch!")
              for (w <- workers) { // go to next superstep
                w ! "Next"
              }
            }
              
            var numDone = 0
            var numTotal: Int = workers.size
            if (numTotal < 100) numTotal = 100

            var cruncher: Option[(Data, Data) => Data] = None
            var workerResults: List[Data] = List()
            
            // UGLY !!!!
            var toOne = false

            for (w <- workers) {
              //println("Doing something")
              receive {
                case "Stop" => // stop iterating
                  //println("should stop now (received from " + sender + ")")
                  shouldFinish = true

                case CrunchToOne(fun: ((Data, Data) => Data), workerResult: Data) =>
                  // OK println("inin")
                  toOne = true
                  if(cruncher.isEmpty) {
                    cruncher = Some(fun)
                  }
                  workerResults ::= workerResult
                case Crunch(fun: ((Data, Data) => Data), workerResult: Data) =>
                  if (cruncher.isEmpty) {
                    cruncher = Some(fun)
                  }
                  workerResults ::= workerResult

                case "Done" =>
                  numDone += 1
                  //if ((numDone % (numTotal / 20)) == 0)
                  //  print("#")
              }
            }
            //println(".")

            if (!shouldFinish) {
              // are we inside a crunch step?
              if (!cruncher.isEmpty) {
                if(toOne) {
                  // OK println("in")
                  crunchResult = Some(CrunchToOneResult(workerResults.reduceLeft(cruncher.get)))
                }
                else {
                  crunchResult = Some(CrunchResult(workerResults.reduceLeft(cruncher.get)))
                }
              } else {
                crunchResult = None

                //println(this + ": sending Outgoing to " + workers)
/*
                for (w <- workers) {
                  w ! "Outgoing"
                }
                for (w <- workers) {
                  receive {
                    case "DoneOutgoing" =>
                      //println(this + ": received DoneOutgoing")
                  }
                }
*/                
              }
            }
          }

          for (foreman <- allForemen) {
            foreman ! "Stop"
          }
          parent ! "Done"
      }
    }
  }

  def iterate(numIterations: Int) {
    this !? StartPropagation(numIterations)
  }

  def iterateUntil(condition: => Boolean) {
    cond = () => condition
    this !? StartPropagation(0)
  }

  def terminate() {
    this ! "Stop"
  }
}

object Test1 {
  var count = 0
  def nextcount: Int = {
    count += 1
    count
  }
}

class Test1Vertex extends Vertex[Double]("v" + Test1.nextcount, 0.0d) {
  def update(superstep: Int, incoming: List[Message[Double]]): Substep[Double] = {
    {
      value += 1
      List()
    }
  }
}

      //, (res: Double, vertices: (Vertex, Vertex)) => {
      // (T, (Vertex, Vertex)) => List[Message[Data]]

      // (Vertex, Vertex) => (T, (Vertex, Vertex))
      //(v1.value + v2.value, (v1, v2))

class Test2Vertex extends Vertex[Double]("v" + Test1.nextcount, 0.0d) {
  def update(superstep: Int, incoming: List[Message[Double]]): Substep[Double] = {
    {
      value += 1
      List()
    } crunch((v1: Double, v2: Double) => v1 + v2) then {
      // result of crunch should be here as incoming message
      incoming match {
        case List(crunchResult) =>
          value = crunchResult.value
        case List() =>
          // do nothing
      }
      List()
    }
  }
}

class Test3Vertex extends Vertex[Double]("v" + Test1.nextcount, 0.0d) {
  def update(superstep: Int, incoming: List[Message[Double]]): Substep[Double] = 
    {
      value += 1
      List()
    } crunchToOne((v1: Double, v2: Double) => v1 + v2) then {
      if(this == graph.vertices(0)) {
        incoming match {
          case List(crunchResult) =>
            //println("curnch result: " + crunchResult)
            value = crunchResult.value
          case _ =>
        }
      }
      List()
    } then {
      List()
    }
  
}

class Test0Vertex extends Vertex[Double]("v", 0.0d) {
  def update(superstep: Int, incoming: List[Message[Double]]): Substep[Double] = {
    if(this == graph.vertices(0)) {
      List(Message(graph.vertices(0), graph.vertices(1), 2.0))
    } else {
    List()
    }
  } then {
    incoming match {
      case List(message) =>
        value = message.value
      case _ =>
    }
    List()
  }
}

object Test {

  def runTest1() {
    println("running test1...")
    val g = new Graph[Double]
    for (i <- 1 to 48) {
      g.addVertex(new Test1Vertex)
    }
    g.start()
    g.iterate(1)
    g.synchronized {
      for (v <- g.vertices) {
        if (v.value < 1) throw new Exception
      }
    }
    g.terminate()
    println("test1 OK")
  }

  def runTest2() {
    println("running test2...")
    val g = new Graph[Double]
    for (i <- 1 to 48) {
      g.addVertex(new Test2Vertex)
    }
    g.start()
    g.iterate(3)
    g.synchronized {
      for (v <- g.vertices) {
        if (v.value < 10) throw new Exception
      }
    }
    g.terminate()
    println("test2 OK")
  }
  
  def runTest3() {
    println("running test3...")
    val g = new Graph[Double]
    for(i <- 1 to 48) {
      g.addVertex(new Test3Vertex)
    }
    g.start()
    g.iterate(4)
    g.synchronized {
      for(v <- g.vertices) {
        println(v.label + ": " + v.value)
      }
    }
    g.terminate
  }
  
  def runTest0() {
    println("running test0...")
    val g = new Graph[Double]
    for(i <- 1 to 48) {
      g.addVertex(new Test0Vertex)
    }
    g.start()
    g.iterate(2)
    g.synchronized {
      for(v <- g.vertices) {
        println(v.label + "; " + v.value)
      }
    }
    g.terminate
  }

  def main(args: Array[String]) {
    //runTest1()
    //runTest2()
    runTest3()
    runTest3()
    //runTest0()
  }

}
