package translating;

import com.brunomnsilva.smartgraph.graph.Digraph;
import com.brunomnsilva.smartgraph.graph.DigraphEdgeList;
import com.brunomnsilva.smartgraph.graphview.SmartCircularSortedPlacementStrategy;
import com.brunomnsilva.smartgraph.graphview.SmartGraphPanel;
import com.brunomnsilva.smartgraph.graphview.SmartPlacementStrategy;
import astnodes.stmt.Stmt;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.IOException;
import BilParser.*;
import scala.collection.JavaConverters;
import scala.collection.mutable.ArrayBuffer;

public class FlowGraphVisualiser extends Application {

    private int edgeId = 0;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BilLexer bilLexer;
        try {
            bilLexer = new BilLexer(CharStreams.fromFileName("samples/that_compile/loops/loops_stripped.bil"));
        } catch (IOException e) {
            System.err.println("File not found.");
            throw new AssertionError();
        }
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil();
        ArrayBuffer<Stmt> facts = new ArrayBuffer<>();
        StatementLoader statementLoader = new StatementLoader(facts);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        FlowGraph flowGraph = FlowGraph.fromStmts(JavaConverters.bufferAsJavaList(facts));
        Digraph<FlowGraph.Block, String> g = new DigraphEdgeList<>();
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            for (FlowGraph.Block block : function.getBlocks()) {
                g.insertVertex(block);
            }
        }
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            for (FlowGraph.Block block : function.getBlocks()) {
                for (FlowGraph.Block child : block.getChildren()) {
                    g.insertEdge(block, child, getEdgeId());
                }
            }
        }
        SmartPlacementStrategy strategy = new SmartCircularSortedPlacementStrategy();
        SmartGraphPanel<FlowGraph.Block, String> graphView = new SmartGraphPanel<>(g, strategy);
        graphView.setAutomaticLayout(false);
        Scene scene = new Scene(graphView, 1024, 768);
        primaryStage.setTitle("Flow Graph Visualisation");
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    public String getEdgeId() {
        return String.valueOf(edgeId++);
    }
}


