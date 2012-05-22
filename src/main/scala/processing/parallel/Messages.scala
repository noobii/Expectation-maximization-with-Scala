package processing.parallel


// step: the step in which the message was produced
// TODO: for distribution need to change Vertex[Data] into vertex ID
case class Message[Data](val source: Vertex[Data], val dest: Vertex[Data], val value: Data) {
  // TODO: step does not need to be visible in user code!
  var step: Int = 0
}

// @author PG

abstract class AbstractCrunch[Data]

abstract class AbstractCrunchResult[Data]

case class Crunch[Data](cruncher: (Data, Data) => Data, crunchResult: Data) extends AbstractCrunch

case class CrunchResult[Data](res: Data) extends AbstractCrunchResult[Data]

// @author PG
case class CrunchTo[Data](to: List[Vertex[Data]])(cruncher: (Data, Data) => Data, crunchResult: Data) extends AbstractCrunch

case class CrunchToResult[Data](to: List[Vertex[Data]])(res: Data) extends AbstractCrunchResult[Data]
