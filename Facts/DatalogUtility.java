package Facts;

import Facts.exp.BopFact;
import Facts.exp.ExtractFact;
import Facts.inst.InstFact;

import java.util.*;

public class DatalogUtility {
    public int idCounter = 0;

    public static void createDatalog(List<InstFact> facts) {
        // flatten the tree: convert the tree structure into a simple node list with DFS
        Map<Fact, ExpLog> recordedFacts = new HashMap<>();
        List<Fact> factList = new ArrayList<>();
        for (InstFact fact : facts) {
            factList.addAll(fact.toFactList());
        }
        // process particular facts in particular ways
        for (Fact fact : factList) {
            if (fact instanceof BopFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            } else if (fact instanceof ExtractFact) {

            }
        }
    }

    private class Log {
        private final int id = idCounter++;
    }

    private class ExpLog extends Log {
        private final String item1;
        private final String item2;
        private final String item3;
        private final String item4;

        private ExpLog(String item1, String item2, String item3, String item4) {
            this.item1 = item1;
            this.item2 = item2;
            this.item3 = item3;
            this.item4 = item4;
        }

        @Override
        public String toString() {
            return String.format("exp%s\t%s\t%s\t%s\t%s", super.id, item1, item2, item3, item4);
        }
    }

    private class InstLog extends Log {
        private final String item1;
        private final String item2;
        private final String item3;

        private InstLog(String item1, String item2, String item3) {
            this.item1 = item1;
            this.item2 = item2;
            this.item3 = item3;
        }

        @Override
        public String toString() {
            return String.format("exp%s\t%s\t%s\t%s", super.id, item1, item2, item3);
        }
    }
}


