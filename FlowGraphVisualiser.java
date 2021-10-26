import BilParsing.BilLexer;
import BilParsing.BilParser;
import BilTranslating.FlowGraph;
import BilTranslating.StatementLoader;
import Facts.inst.InstFact;
import com.brunomnsilva.smartgraph.graph.Digraph;
import com.brunomnsilva.smartgraph.graph.DigraphEdgeList;
import com.brunomnsilva.smartgraph.graph.Graph;
import com.brunomnsilva.smartgraph.graph.GraphEdgeList;
import com.brunomnsilva.smartgraph.graphview.SmartCircularSortedPlacementStrategy;
import com.brunomnsilva.smartgraph.graphview.SmartGraphPanel;
import com.brunomnsilva.smartgraph.graphview.SmartPlacementStrategy;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class FlowGraphVisualiser extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BilLexer bilLexer;
        try {
            bilLexer = new BilLexer(CharStreams.fromFileName("samples/simple_jump.bil"));
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
        Digraph<String, String> g = new DigraphEdgeList<>();
        g.insertVertex("a");
        g.insertVertex("b");
        g.insertVertex("c");
        g.insertVertex("d");
        g.insertVertex("e");
        g.insertEdge("a", "b", "x");
        g.insertEdge("b", "c", "y");
        g.insertEdge("d", "e", "z");
        g.insertEdge("c", "a", "w");
        SmartPlacementStrategy strategy = new SmartCircularSortedPlacementStrategy();
        SmartGraphPanel<String, String> graphView = new SmartGraphPanel<>(g, strategy);
        Scene scene = new Scene(graphView, 1024, 768);
        primaryStage.setTitle("Flow Graph Visualisation");
        primaryStage.setScene(scene);
        primaryStage.show();
    }
}
