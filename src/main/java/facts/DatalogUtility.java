package Facts;

import Facts.Exp.*;
import Facts.Inst.*;
import Facts.Inst.Assign.LoadFact;
import Facts.Inst.Assign.MoveFact;
import Facts.Inst.Assign.StoreFact;
import Facts.Misc.SuccessorFact;
import java.util.*;

public class DatalogUtility {

    private int expIdCounter = 0;
    private int instIdCounter = 0;
    private int succIdCounter = 0;

    public List<Log> createDatalog(List<InstFact> facts) {
        facts = new ArrayList<>(facts);
        // maps facts to their respective logs so they can be referred to by their log ID by other logs
        Map<Fact, Log> recordedFacts = new HashMap<>();
        List<Fact> factList = new ArrayList<>();
        // flatten the tree: convert the tree structure into a simple node list with DFS
        for (InstFact fact : facts) factList.addAll(fact.toFactList());
        // append successor facts
        factList.addAll(getAllSuccFacts(facts));
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
                        bopFact.getOperator(),
                        recordedFacts.get(bopFact.getFirstExp()).getIDStr(),
                        recordedFacts.get(bopFact.getSecondExp()).getIDStr()));
            } else if (fact instanceof ExtractFact) {
                // exp2 extract 1 2 exp1
                ExtractFact extractFact = (ExtractFact) fact;
                recordedFacts.put(extractFact, new ExpLog(
                        "extract",
                        Integer.toString(extractFact.getFirstInt()),
                        Integer.toString(extractFact.getSecondInt()),
                        recordedFacts.get(extractFact.getVariable()).getIDStr()));
            } else if (fact instanceof LiteralFact) {
                // exp2 literal 1 none none
                LiteralFact literalFact = (LiteralFact) fact;
                recordedFacts.put(literalFact, new ExpLog(
                        "literal",
                        literalFact.getVal(),
                        "none",
                        "none"));
            } else if (fact instanceof MemFact) {
                // ignore mem facts - link this mem to the log of its exp
                MemFact memFact = (MemFact) fact;
                recordedFacts.put(memFact, recordedFacts.get(memFact.getExp()));
            } else if (fact instanceof UopFact) {
                // exp2 uop ~ exp1 none
                UopFact uopFact = (UopFact) fact;
                recordedFacts.put(uopFact, new ExpLog(
                        "uop",
                        uopFact.getOperator(),
                        recordedFacts.get(uopFact.getExp()).getIDStr(),
                        "none"));
            } else if (fact instanceof VarFact) {
                // exp2 var X0 none none
                VarFact varFact = (VarFact) fact;
                recordedFacts.put(varFact, new ExpLog(
                        "var",
                        varFact.getName(),
                        "none",
                        "none"));
            } else if (fact instanceof LoadFact) {
                // 2 load exp1 exp0
                LoadFact loadFact = (LoadFact) fact;
                recordedFacts.put(loadFact, new InstLog(
                        "load",
                        recordedFacts.get(loadFact.getLhs()).getIDStr(),
                        loadFact.getRhs() == null ? "none" : recordedFacts.get(loadFact.getRhs()).getIDStr()));
            } else if (fact instanceof MoveFact) {
                // 2 move exp1 exp0
                MoveFact moveFact = (MoveFact) fact;
                recordedFacts.put(moveFact, new InstLog(
                        "move",
                        recordedFacts.get(moveFact.getLhs()).getIDStr(),
                        recordedFacts.get(moveFact.getRhs()).getIDStr()));
            } else if (fact instanceof StoreFact) {
                // 2 store exp1 exp0
                StoreFact storeFact = (StoreFact) fact;
                recordedFacts.put(storeFact, new InstLog(
                        "store",
                        recordedFacts.get(storeFact.getLhs()).getIDStr(),
                        recordedFacts.get(storeFact.getRhs()).getIDStr()));
            } else if (fact instanceof CallFact) {
                // 2 call functionName none
                CallFact callFact = (CallFact) fact;
                recordedFacts.put(callFact, new InstLog(
                        "call",
                        callFact.getFuncName(),
                        "none"));
            } else if (fact instanceof CjmpFact) {
                // 2 cjmp exp1 target
                CjmpFact cjmpFact = (CjmpFact) fact;
                recordedFacts.put(cjmpFact, new InstLog(
                        "cjmp",
                        recordedFacts.get(cjmpFact.getCondition()).getIDStr(),
                        cjmpFact.getTarget()));
            } else if (fact instanceof EnterSubFact) {
                // 2 enterSub function none
                EnterSubFact enterSubFact = (EnterSubFact) fact;
                recordedFacts.put(enterSubFact, new InstLog(
                        "enterSub",
                        enterSubFact.getFuncName(),
                        "none"));
            } else if (fact instanceof ExitSubFact) {
                // 2 exitFunc function none
                ExitSubFact exitSubFact = (ExitSubFact) fact;
                recordedFacts.put(exitSubFact, new InstLog(
                        "exitSub",
                        "none",
                        "none"));
            } else if (fact instanceof JmpFact) {
                // 2 jump target none
                JmpFact jmpFact = (JmpFact) fact;
                recordedFacts.put(jmpFact, new InstLog(
                        "jump",
                        jmpFact.getTarget(),
                        "none"));
            } else if (fact instanceof NopFact) {
                // 2 nop none none
                NopFact nopFact = (NopFact) fact;
                recordedFacts.put(nopFact, new InstLog(
                        "nop",
                        "none",
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

    private List<SuccessorFact> getAllSuccFacts(List<InstFact> facts) {
        // get all instfact -> targetIndex successions
        // invalid map keys or values are recorded as -1. this usually means we couldn't find a fact corresponding to a target pc
        Map<Integer, Integer> successions = new HashMap<>();
        for (int f = 0; f < facts.size(); f++) {
            InstFact fact = facts.get(f);
            if (fact instanceof JmpFact) {
                JmpFact jmpFact = (JmpFact) fact;
                int targetIndex = findInstWithPc(jmpFact.getTarget(), facts);
                successions.put(f, targetIndex);
            } else if (fact instanceof CallFact) {
                CallFact callFact = (CallFact) fact;
                // need both the call -> startFunction succession and the endFunction -> lineBelowCall succession
                int enterSubIndex = -1;
                int exitSubIndex = -1;
                // find the index of this called function
                for (int i = 0; i < facts.size(); i++) {
                    if (!(facts.get(i) instanceof EnterSubFact)) continue;
                    EnterSubFact enterSubFact = (EnterSubFact) facts.get(i);
                    if (enterSubFact.getFuncName().equals(callFact.getFuncName())) {
                        // find the index of the called function; record its index and search for its end
                        enterSubIndex = i;
                        for (int j = i; j < facts.size(); j++) {
                            if (facts.get(j) instanceof ExitSubFact) {
                                // found the end of the function; all done
                                exitSubIndex = j;
                                break;
                            }
                        }
                        break;
                    }
                }
                successions.put(f, enterSubIndex); // call -> startFunction succession
                // todo: endFunction -> lineBelowCall succession - there are many places where the function may return. this may be better done with a flow graph?
            } else if (fact instanceof CjmpFact) {
                CjmpFact cjmpFact = (CjmpFact) fact;
                // need both the ifTrue jump and ifFalse jump
                // the ifFalse jump will be a jump to the next line
                successions.put(f, f + 1);
                // the ifTrue jump will be a jump to the target
                successions.put(f, findInstWithPc(cjmpFact.getTarget(), facts));
            }
        }

        List<SuccessorFact> successorFacts = new ArrayList<>();
        for (Integer k : successions.keySet()) {
            Integer v = successions.get(k);
            if (k == -1 || v == -1) {
                System.err.println("Error recording succession fact.");
            } else {
                SuccessorFact succ = new SuccessorFact(facts.get(k), facts.get(v));
                successorFacts.add(succ);
            }
        }
        return successorFacts;
    }

    private int findInstWithPc(String pc, List<InstFact> facts) {
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            if (fact.getLabel().getPc().equals(pc)) {
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

    public class ExpLog extends Log {
        private final String item1;
        private final String item2;
        private final String item3;
        private final String item4;

        public ExpLog(String item1, String item2, String item3, String item4) {
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

    public class InstLog extends Log {
        private final String item1;
        private final String item2;
        private final String item3;

        public InstLog(String item1, String item2, String item3) {
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

    public class SuccLog extends Log {
        private final String item1;
        private final String item2;

        public SuccLog(String item1, String item2) {
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


