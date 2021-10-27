import BilParsing.BilLexer;
import BilParsing.BilParser;
import BilTranslating.FlowGraph;
import BilTranslating.StatementLoader;
import Facts.inst.InstFact;
import com.brunomnsilva.smartgraph.graph.Digraph;
import com.brunomnsilva.smartgraph.graph.DigraphEdgeList;
import com.brunomnsilva.smartgraph.graphview.SmartCircularSortedPlacementStrategy;
import com.brunomnsilva.smartgraph.graphview.SmartGraphPanel;
import com.brunomnsilva.smartgraph.graphview.SmartPlacementStrategy;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class FlowGraphVisualiser extends Application {

    private int edgeId = 0;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BilLexer bilLexer;
        try {
            bilLexer = new BilLexer(CharStreams.fromFileName("samples/loops_stripped.bil"));
        } catch (IOException e) {
            System.err.println("File not found.");
            throw new AssertionError();
        }
        CommonTokenStream tokens = new CommonTokenStream(bilLexer);
        BilParser parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        BilParser.BilContext b = parser.bil();
        List<InstFact> facts = new ArrayList<>();
        StatementLoader statementLoader = new StatementLoader(facts);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        FlowGraph flowGraph = FlowGraph.fromFactsList(facts);
        Digraph<FlowGraph.Block, String> g = new DigraphEdgeList<>();
        for (FlowGraph.Block cluster : flowGraph.functionBlocks) {
            for (FlowGraph.Block block : cluster.blocksInCluster()) {
                g.insertVertex(block);
                System.out.println(block);
            }
        }
        for (FlowGraph.Block cluster : flowGraph.functionBlocks) {
            for (FlowGraph.Block block : cluster.blocksInCluster()) {
                for (FlowGraph.Block child : block.children) {
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


