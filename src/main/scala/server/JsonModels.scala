package server

import io.circe.generic.auto.*
import io.circe.syntax.*

/**
 * Data structures for JSON communication between the Scala backend and TypeScript frontend.
 * Using these models avoids complex string parsing (like DOT/RegEx) on the frontend.
 */

case class NodeData(
  id: String,
  label: String,
  isEntry: Boolean = false
)

case class EdgeData(
  id: String,
  source: String,
  target: String,
  label: Option[String] = None
)

case class ProcedureGraph(
  nodes: List[NodeData],
  edges: List[EdgeData]
)

case class ProcedureTextLocation(
  name: String,
  startLine: Int
)

case class AnalysisStatus(
  status: String
)

case class DirectorySelection(
  directoryPath: String
)
