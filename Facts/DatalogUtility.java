package Facts;

import Facts.exp.*;
import Facts.inst.*;
import Facts.inst.assign.LoadFact;
import Facts.inst.assign.MoveFact;
import Facts.inst.assign.StoreFact;
import Facts.misc.SuccessorFact;

import java.util.*;

public class DatalogUtility {
    private int expIdCounter = 0;
    private int instIdCounter = 0;
    private int succIdCounter = 0;

    public List<Log> createDatalog(List<InstFact> facts) {
        facts = new ArrayList<>(facts);
        // append successor facts
        List<SuccessorFact> successorFacts = new ArrayList<>();
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            int factIndexOfTarget;
            // incomplete: there might be more successor facts we can extract from other fact instances
            if (fact instanceof JmpFact) {
                factIndexOfTarget = findInstWithPc(((JmpFact) fact).target, facts);
            } else if (fact instanceof CallFact) {
                // incomplete: we might also want the start of the called function as a successor to this line
                factIndexOfTarget = findInstWithPc(((CallFact) fact).returnAddr, facts);
            } else {
                continue;
            }
            if (factIndexOfTarget == -1) System.err.printf("Could not create succ fact for fact %d because no fact containing the target PC was found.", i);
            else successorFacts.add(new SuccessorFact(fact, facts.get(factIndexOfTarget)));
        }
        // flatten the tree: convert the tree structure into a simple node list with DFS
        Map<Fact, Log> recordedFacts = new HashMap<>();
        List<Fact> factList = new ArrayList<>();
        for (InstFact fact : facts) factList.addAll(fact.toFactList());
        factList.addAll(successorFacts);
        // remove duplicate expression facts
        List<Fact> noDuplicatesFactList = new ArrayList<>();
        for (Fact fact : factList) {
            if (fact instanceof ExpFact && noDuplicatesFactList.contains(fact)) continue;
            noDuplicatesFactList.add(fact);
        }

        // note: this only works because two identical instructions are not considered equal if their labels (PCs) are not equal
        // process particular facts in particular ways
        for (Fact fact : noDuplicatesFactList) {
            if (fact instanceof BopFact) {
                // exp2 bop + exp0 exp1
                BopFact bopFact = (BopFact) fact;
                recordedFacts.put(bopFact, new ExpLog(
                        "bop",
                        bopFact.op,
                        recordedFacts.get(bopFact.e1).getIDStr(),
                        recordedFacts.get(bopFact.e2).getIDStr()));
            } else if (fact instanceof ExtractFact) {
                // exp2 extract 1 2 exp1
                ExtractFact extractFact = (ExtractFact) fact;
                recordedFacts.put(extractFact, new ExpLog(
                        "extract",
                        Integer.toString(extractFact.firstInt),
                        Integer.toString(extractFact.secondInt),
                        recordedFacts.get(extractFact.variable).getIDStr()));
            } else if (fact instanceof LiteralFact) {
                // exp2 literal 1 none none
                LiteralFact literalFact = (LiteralFact) fact;
                recordedFacts.put(literalFact, new ExpLog(
                        "literal",
                        literalFact.val,
                        "none",
                        "none"));
            } else if (fact instanceof MemFact) {
                // ignore mem facts - link this mem to the log of its exp
                MemFact memFact = (MemFact) fact;
                recordedFacts.put(memFact, recordedFacts.get(memFact.exp));
            } else if (fact instanceof UopFact) {
                // exp2 uop ~ exp1 none
                UopFact uopFact = (UopFact) fact;
                recordedFacts.put(uopFact, new ExpLog(
                        "uop",
                        uopFact.op,
                        recordedFacts.get(uopFact.e1).getIDStr(),
                        "none"));
            } else if (fact instanceof VarFact) {
                // exp2 var X0 none none
                VarFact varFact = (VarFact) fact;
                recordedFacts.put(varFact, new ExpLog(
                        "var",
                        varFact.name,
                        "none",
                        "none"));
            } else if (fact instanceof LoadFact) {
                // 2 load exp1 exp0
                LoadFact loadFact = (LoadFact) fact;
                recordedFacts.put(loadFact, new InstLog(
                        "load",
                        recordedFacts.get(loadFact.lhs).getIDStr(),
                        loadFact.rhs == null ? "none" : recordedFacts.get(loadFact.rhs).getIDStr()));
            } else if (fact instanceof MoveFact) {
                // 2 move exp1 exp0
                MoveFact moveFact = (MoveFact) fact;
                recordedFacts.put(moveFact, new InstLog(
                        "move",
                        recordedFacts.get(moveFact.lhs).getIDStr(),
                        recordedFacts.get(moveFact.rhs).getIDStr()));
            } else if (fact instanceof StoreFact) {
                // 2 store exp1 exp0
                StoreFact storeFact = (StoreFact) fact;
                recordedFacts.put(storeFact, new InstLog(
                        "store",
                        recordedFacts.get(storeFact.lhs).getIDStr(),
                        recordedFacts.get(storeFact.rhs).getIDStr()));
            } else if (fact instanceof CallFact) {
                // 2 call functionName none
                CallFact callFact = (CallFact) fact;
                recordedFacts.put(callFact, new InstLog(
                        "call",
                        callFact.funcName,
                        "none"));
            } else if (fact instanceof CjmpFact) {
                // 2 cjmp exp1 target
                CjmpFact cjmpFact = (CjmpFact) fact;
                recordedFacts.put(cjmpFact, new InstLog(
                        "cjmp",
                        recordedFacts.get(cjmpFact.condition).getIDStr(),
                        cjmpFact.target));
            } else if (fact instanceof EnterSubFact) {
                // 2 enterSub function none
                EnterSubFact enterSubFact = (EnterSubFact) fact;
                recordedFacts.put(enterSubFact, new InstLog(
                        "enterSub",
                        enterSubFact.funcName,
                        "none"));
            } else if (fact instanceof ExitSubFact) {
                // 2 exitFunc function none
                ExitSubFact exitSubFact = (ExitSubFact) fact;
                recordedFacts.put(exitSubFact, new InstLog(
                        "exitSub",
                        exitSubFact.funcName,
                        "none"));
            } else if (fact instanceof JmpFact) {
                // 2 jump target none
                JmpFact jmpFact = (JmpFact) fact;
                recordedFacts.put(jmpFact, new InstLog(
                        "jump",
                        jmpFact.target,
                        "none"));
            } else if (fact instanceof NopFact) {
                // 2 nop none none
                NopFact nopFact = (NopFact) fact;
                recordedFacts.put(nopFact, new InstLog(
                        "nop",
                        "none",
                        "none"));
            } else if (fact instanceof ParamFact) {
                // 2 param name none
                ParamFact paramFact = (ParamFact) fact;
                recordedFacts.put(paramFact, new InstLog(
                        "param",
                        paramFact.name.name,
                        "none"));
            } else if (fact instanceof SuccessorFact) {
                // 0 1
                SuccessorFact successorFact = (SuccessorFact) fact;
                recordedFacts.put(successorFact, new SuccLog(
                        recordedFacts.get(successorFact.i1).getIDStr(),
                        recordedFacts.get(successorFact.i2).getIDStr()
                ));
            }
        }
        // remove unnecessary facts
        recordedFacts.entrySet().removeIf(entry -> entry.getKey() instanceof MemFact);
        // order by type, then by id
        List<ExpLog> expLogList = new ArrayList<>();
        List<InstLog> instLogList = new ArrayList<>();
        List<SuccLog> succLogList = new ArrayList<>();
        for (Log log : recordedFacts.values()) {
            if (log instanceof ExpLog) expLogList.add((ExpLog) log);
            else if (log instanceof InstLog) instLogList.add((InstLog) log);
            else if (log instanceof SuccLog) succLogList.add((SuccLog) log);
        }
        SortById sorter = new SortById();
        expLogList.sort(sorter);
        instLogList.sort(sorter);
        succLogList.sort(sorter);
        List<Log> logList = new ArrayList<>();
        logList.addAll(instLogList);
        logList.addAll(expLogList);
        logList.addAll(succLogList);
        return logList;
    }

    private int findInstWithPc(String pc, List<InstFact> facts) {
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            if (fact.label.pc.equals(pc)) {
                return i;
            }
        }
        return -1;
    }

    private class SortById implements Comparator<Log> {

        @Override
        public int compare(Log o1, Log o2) {
            return o1.id - o2.id;
        }
    }

    public abstract class Log {
        private int id;

        abstract String getIDStr();
    }

    private class ExpLog extends Log {
        private final String item1;
        private final String item2;
        private final String item3;
        private final String item4;

        private ExpLog(String item1, String item2, String item3, String item4) {
            super.id = expIdCounter++;
            this.item1 = item1;
            this.item2 = item2;
            this.item3 = item3;
            this.item4 = item4;
        }

        @Override
        String getIDStr() {
            return "exp" + super.id;
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
            super.id = instIdCounter++;
            this.item1 = item1;
            this.item2 = item2;
            this.item3 = item3;
        }

        @Override
        String getIDStr() {
            return "" + super.id;
        }

        @Override
        public String toString() {
            return String.format("%s\t%s\t%s\t%s", super.id, item1, item2, item3);
        }
    }

    private class SuccLog extends Log {
        private final String item1;
        private final String item2;

        private SuccLog(String item1, String item2) {
            super.id = succIdCounter++;
            this.item1 = item1;
            this.item2 = item2;
        }

        @Override
        String getIDStr() {
            return "" + super.id;
        }

        @Override
        public String toString() {
            return String.format("%s\t%s", item1, item2);
        }
    }
}


