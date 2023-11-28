package analysis

import analysis.{CfgNode, CfgEdge}

import scala.collection.mutable




/*
 * Loop Identification
 *
 * 
 */


/* A loop is a subgraph <G_l, E_l> of a CFG <G, E>
 *
 */
 class Loop(var header: CfgNode):
    val reentries: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()    // Edges to loop from outside that are not to the header
    val backEdges: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()    // Edges from inside loop to the header
    val entryEdges: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()   // Edges into the header node

    val nodes: mutable.Set[CfgNode] = mutable.Set[CfgNode]()        // G_l
    val edges: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()        // G_e
    var reducible: Boolean = true                                   // Assume reducible by default

    def addEdge(edge: CfgEdge) = {
        nodes += edge.getFrom
        nodes += edge.getTo
        edges += edge
    }

    override def toString() = {
        s"Header: ${header}, Body: ${edges}"
    }

/* Loop detection and classification with respect to being reducible or irreducible. Implements the algorithm
 *  described by Wei in `A New Algorithm for Identifying Loops in Decompilation` (LNCS 4632 pp 170-183)
 */
class LoopDetector(cfg: ProgramCfg):

    // Header -> Loop
    val loops: mutable.HashMap[CfgNode, Loop] = mutable.HashMap[CfgNode, Loop]()
    val headers: mutable.Set[CfgNode] = mutable.Set[CfgNode]()

    // Algorithm helpers
    val visitedNodes: mutable.Set[CfgNode] = mutable.Set[CfgNode]()
    val nodeDFSPpos: mutable.HashMap[CfgNode, Int] = mutable.HashMap[CfgNode, Int]()
    val iloopHeaders: mutable.HashMap[CfgNode, CfgNode] = mutable.HashMap[CfgNode, CfgNode]()
    val edgeStack: mutable.Stack[CfgEdge] = mutable.Stack[CfgEdge]()

    /* 
     *
     */
    def loops_to_transform(): Set[Loop] = {
        val irreducibleLoops: Set[Loop] = loops.values.filter(l => !l.reducible).toSet;

        if (irreducibleLoops.isEmpty) {
            return Set[Loop]();
        }

        val wantedLoops: mutable.Set[Loop] = mutable.Set[Loop]();
        val irrHeaders: Set[CfgNode] = irreducibleLoops.map(l => l.header);

        irreducibleLoops;
    }

    /*
     *
     */
     def identify_loops(): Set[Loop] = {
        
        val funcEntries = cfg.nodes.filter(_.isInstanceOf[CfgFunctionEntryNode]);

        funcEntries.foreach { funcEntry =>
            traverse_loops_dfs(funcEntry, 1);
        }

        loops.values.toSet
     }


    
    /* 
     * This algorithm performs a DFS on the CFG, starting from the node `h0` (the initial one passed)
     *  and tags nodes as visited as it goes. There are 5 possible pathways on visit for each node `b`:
     *   (a) `b` has not been visited before.
     *   (b) `b` has been visited and is in the current path, i.e., it is a new loop header.
     *   (c) `b` has been visited, though is not in the current path, and is not part of a loop. It is 
     *          thus regular control flow unrelated to the headers.
     *   (d) `b` has been visited, though is not in the current path. It is part of a loop with a header
     *          in our path though, so it is another node within our loop.
     *   (e) `b` has been visited, though is not part of the current path. It is part of a loop, though
     *          not any loop that our current loop context is a part of - thus this node must be a re-entry
     *          node into the target loop, making it irreducible, so we mark it as such.
     *
     */
    def traverse_loops_dfs(b0: CfgNode, DFSPpos: Int): Option[CfgNode] = {

        visitedNodes += b0;
        nodeDFSPpos(b0) = DFSPpos;

        // Process all outgoing edges from the current node
        // b0.succEdges(true).toList.sortBy(n => n.getTo.ed).foreach{ edge =>
        //  The above makes the iteration of loops deterministic. The algorithm (and transform) should be agnostic of how loops are identified
        //      (in the sense its application should completely resolve any irreducibility), though by nature of irreducible loops they can be 
        //      characterised in different ways (i.e. )
        b0.succEdges(true).foreach{ edge =>             
            edgeStack.push(edge);
            val from = edge.getFrom;
            val b = edge.getTo;

            if (!visitedNodes.contains(b)) {
                // Case (a)
                val nh: Option[CfgNode] = traverse_loops_dfs(b, DFSPpos + 1);
                tag_lhead(b0, nh);
            } else {
                if (nodeDFSPpos(b) > 0) {
                    // Case (b)
                    // b is in DFSP(b0)
                    headers += b;

                    val newLoop = if (loops.contains(b)) loops(b) else Loop(b);
                    edgeStack.reverse.slice(nodeDFSPpos(b) - 1, nodeDFSPpos(b0)).foreach {
                        pEdge =>
                            newLoop.addEdge(pEdge);
                    }
                    newLoop.backEdges += edge

                    // Add loop entry edge
                    b.predEdges(true).foreach { predEdge =>
                        val predNode = predEdge.getFrom;
                        if (nodeDFSPpos.contains(predNode)) {
                            if (nodeDFSPpos(predNode) > 0 && predEdge != edge) {
                                newLoop.entryEdges += predEdge;
                            }
                        }
                    }

                    if (!loops.contains(b)) loops(b) = newLoop;

                    tag_lhead(b0, Some(b));
                } else if (!iloopHeaders.contains(b)) {
                    // Case (c) - do nothing
                    // b is not part of this path, and it's not a header, so we can ignore it. 
                    // (in effect we add it to our path later, but we do that when we discover a new header
                    //  by looking through the instruction call stack)
                    ;
                } else {
                    var h: CfgNode = iloopHeaders(b);
                    if (nodeDFSPpos(h) > 0) {
                        // Case (d)
                        // h is in DFSP(b0)
                        val loop = loops(h);

                        // Add current path to the existing loop (a new path in the loop is discovered)
                        // THis can happen for example in the case that there is a branch in a loop, or a `continue` stmt, etc
                        edgeStack.reverse.slice(nodeDFSPpos(h) - 1, nodeDFSPpos(b0)).foreach {
                            pEdge => loop.addEdge(pEdge);
                        }
                        b.succEdges(true).filter(e => e.getTo == h).foreach {
                            outEdge => loop.addEdge(outEdge);
                        }

                        tag_lhead(b0, Some(h));
                    } else {
                        // Case (e)
                        // reentry (irreducible!)
                        val loop = loops(h);
                        loop.reducible = false;
                        loop.reentries += edge;
                        var break = false;

                        // Make outer loops irreducible if the originating node of the re-entry edge is not within those loops
                        b.succEdges(true).foreach { 
                            nextEdge =>
                                val nextNode = nextEdge.getTo;
                                var ih = nextNode;

                                while (iloopHeaders.contains(ih)) {
                                    ih = iloopHeaders(ih);
                                    val thisLoop = loops(ih);
                                    if (iloopHeaders.contains(from)) {
                                        if (iloopHeaders(from) != ih) {
                                            thisLoop.reducible = false;
                                            thisLoop.reentries += edge;
                                        }
                                    } else {
                                        // `from` must be outside the loop
                                        thisLoop.reducible = false;
                                        thisLoop.reentries += edge;
                                    }
                                }
                        }

                        while (iloopHeaders.contains(h) && !break) {
                            h = iloopHeaders(h);
                            if (nodeDFSPpos(h) > 0) {
                                tag_lhead(b0, Some(h));
                                break = true; // Naturally, Scala doesn't have a `break`
                            }
                        }
                    }
                }
            }
            edgeStack.pop(); // Here the most recent edge will be originating from `b0`
        }
        nodeDFSPpos(b0) = 0;
        iloopHeaders.get(b0);
    }

    /** Sets the most inner loop header `h` for a given node `b`
     *
     */
    def tag_lhead(b: CfgNode, h: Option[CfgNode]): Unit = {
        var cur1: CfgNode = b;
        var cur2: CfgNode = h match {
            case Some(hh) => hh
            case _ => return
        };

        if (cur1 == cur2) {
            return; // We don't consider self-loops to be loops for our purposes
        }

        var hasLoopHeader: Boolean = iloopHeaders.contains(cur1);
        while (hasLoopHeader) {
            val ih = iloopHeaders(cur1);
            if (ih == cur2) {
                return;
            }
            if (nodeDFSPpos(ih) < nodeDFSPpos(cur2)) {
                iloopHeaders(cur1) = cur2;
                cur1 = cur2;
                cur2 = ih;
            } else {
                cur1 = ih;
            }
            hasLoopHeader = iloopHeaders.contains(cur1);
        }
        iloopHeaders(cur1) = cur2;
    }



