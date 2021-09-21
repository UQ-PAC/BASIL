import Facts.Fact;
import Facts.exp.*;
import Facts.inst.*;
import Facts.inst.assign.AssignFact;
import Facts.inst.assign.LoadFact;
import Facts.inst.assign.MoveFact;
import Facts.inst.assign.StoreFact;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * todo:
 * [ ] implement return statements in function calls
 * [ ] implement proper modifies statements
 * [ ] unary operators like ~ probably mean 'bit flip' not 'not'. figure this out and solve
 * [ ] remove lines with references to FP when FP isn't utilised throughout the program
 * [ ] implement loops properly
 * [ ] sort out whatever this is (NF <> VF | ZF <> 0) <> 0
 *
 * [ ] write out rules for wpifrg translation to boogie
 * [ ] download boogie properly
 *
 * theoretical problems:
 * [ ] registers can store function pointers?
 * [ ] what to do if we can't just ignore FP
 * [ ] how to properly deal with SP and LR
 * [ ] dereferencing of registers when their value has been extracted from mem[]
 */

public class BoogieTranslator {

    // for writing the boogie output
    BufferedWriter writer;
    // the lines in the BIL file to translate
    List<InstFact> facts;
    // the name of the function we are currently in, or "" if we are not in a function
    String funcName = "";
    int nameCount = 0;

    public BoogieTranslator(List<InstFact> facts, String outputFileName) {
        try {
            writer = new BufferedWriter(new FileWriter(outputFileName, false));
        } catch (IOException e) {
            System.err.println("Error setting up file writer.");
        }
        this.facts = facts;
    }

    /**
     * Starting point for a BIL translation.
     */
    public void translate() {
        createLabels();
        optimiseLabels();
        optimiseSkips();
        createFuncParameters();
        resolveInParams();
        // resolveOutParams();
        resolveRegisters();
        printAllFacts();
    }

    /**
     * We want to display the labels (i.e. pc's) of all instructions whose labels are referenced elsewhere (for e.g. by
     * a jump, conditional jump or call).
     */
    private void createLabels() {
        List<String> usedLabels = new ArrayList<>();
        for (InstFact fact : facts) {
            String target = extractTargetLabel(fact);
            if (target != null) usedLabels.add(target);
        }
        for (InstFact fact : facts) fact.label.hide = !usedLabels.contains(fact.label.pc);
    }

    /**
     * Remove all jumps to lines that directly follow.
     * Then remove those labels if they are not jumped to again.
     */
    private void optimiseLabels() {
        List<InstFact> toRemove = new ArrayList<>();
        for (int i = 0; i < facts.size() - 1; i++) {
            InstFact fact = facts.get(i);
            String target = extractTargetLabel(fact);
            if (target == null) continue;
            InstFact nextFact = facts.get(i + 1);
            if (nextFact.label.hide || !nextFact.label.pc.equals(target)) continue;
            // this jump goes to the next line; remove it
            if (fact instanceof CallFact) ((CallFact) fact).showJump = false;
            else toRemove.add(fact);
        }
        toRemove.forEach(fact -> facts.remove(fact));
        // finally, we need to remove redundant labels again
        createLabels();
    }

    private void optimiseSkips() {
        List<InstFact> toRemove = new ArrayList<>();
        for (InstFact fact : facts) {
            if (fact instanceof NopFact && fact.label.hide) {
                toRemove.add(fact);
            }
        }
        toRemove.forEach(fact -> facts.remove(fact));
    }

    private String extractTargetLabel(InstFact fact) {
        // target labels are used in jumps, cjumps and calls
        if (fact instanceof JmpFact) {
            return ((JmpFact) fact).target;
        } else if (fact instanceof CjmpFact) {
            return ((CjmpFact) fact).target;
        } else if (fact instanceof CallFact) {
            CallFact call = (CallFact) fact;
            if (call.showJump) {
                return call.returnAddr;
            }
        }
        return null;
    }

    /**
     * @return list of [start, end] endpoints for function blocks.
     * 'Start' is the function header and 'end' is the function return line.
     */
    private List<Integer[]> getAllFunctions() {
        List<Integer[]> functions = new ArrayList<>();
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            if (fact instanceof EnterSubFact) {
                Integer[] endPoints = new Integer[2];
                endPoints[0] = i;
                functions.add(endPoints);
            } else if (fact instanceof ExitSubFact) functions.get(functions.size() - 1)[1] = i;
        }
        return functions;
    }

    /**
     * We want to feed each EnterSubFact with a list of its parameters (i.e. ParamFacts) so that it can include these
     * in its procedure header, and use them for parameter resolving/dereferencing within the function body.
     */
    private void createFuncParameters() {
        for (Integer[] endpoints : getAllFunctions()) {
            int start = endpoints[0];
            int end = endpoints[1];
            EnterSubFact currentFunc = (EnterSubFact) facts.get(start);
            int i = start + 1;
            // get all explicitly listed function parameters. we assume they are all listed directly underneath
            while (i < end) {
                InstFact param = facts.get(i++);
                if (!(param instanceof ParamFact)) break;
                currentFunc.paramFacts.add((ParamFact) param);
            }
            // get all function parameters which are not explicitly listed (registers which are stored before they are assigned)
            List<VarFact> assignedRegisters = new ArrayList<>();
            while (i < end) {
                InstFact line = facts.get(i++);
                if (!(line instanceof AssignFact)) continue; // we are only concerned with assignments here
                if (line instanceof StoreFact) {
                    // e.g. mem[foo] := var;
                    StoreFact storeFact = (StoreFact) line;
                    if (!(storeFact.rhs instanceof VarFact)) continue; // assumption: parameter stores should contain a rhs which is a single variable
                    VarFact rhsVar = (VarFact) storeFact.rhs;
                    if (!isRegister(rhsVar) || assignedRegisters.contains(rhsVar)) continue; // variable is not a register, or has been assigned before
                    // this is a register that has been stored before assigned - it is a parameter
                    ParamFact param = new ParamFact("", new VarFact(generateUniqueName()), rhsVar, false);
                    param.alias = (MemFact) storeFact.lhs;
                    currentFunc.paramFacts.add(param);
                } else {
                    VarFact lhsVar = (VarFact) (line instanceof LoadFact ? ((LoadFact) line).lhs : ((MoveFact) line).lhs);
                    if (isRegister(lhsVar)) assignedRegisters.add(lhsVar);
                }
            }
            // once the parameters for a function have been created, create parameters for any call to this function too
            for (InstFact fact : facts) {
                if (!(fact instanceof CallFact)) continue;
                CallFact call = (CallFact) fact;
                if (!call.funcName.equals(currentFunc.funcName)) continue;
                for (ParamFact paramFact : currentFunc.paramFacts) {
                    if (!paramFact.is_result) call.params.add(paramFact.register);
                }
            }
        }
    }

    private boolean isRegister(VarFact varFact) {
        return varFact.name.charAt(0) == 'X';
    }

    /**
     * We want to replace register references and mem calls to the stack with a human-readable variable for the
     * parameter.
     * This also assumes that memory aliases for param variables are not accessed (loaded) before they are assigned (stored),
     * and that each parameter has no more than 1 MemFact alias.
     *
     * RELEVANT RULES
     * 1. All store instructions which contain both the mem (on the left-hand-side) and register (on the right-hand-side) assigned to a parameter are removed.
     * 2. All references to mems which are assigned to parameters are replaced with references to the human-readable name of that parameter.
     */
    private void resolveInParams() {
        // implement rule 1
        for (Integer[] endpoints : getAllFunctions()) {
            int start = endpoints[0];
            int end = endpoints[1];
            EnterSubFact currentFunc = (EnterSubFact) facts.get(start);
            List<Integer> forRemoval = new ArrayList<>();
            for (int i = start; i < end; i++) {
                InstFact fact = facts.get(i);
                if (!(fact instanceof StoreFact)) continue;
                StoreFact storeFact = (StoreFact) fact;
                if (!(storeFact.rhs instanceof VarFact)) continue; // assume the rhs of the stores we're looking for consist of only a variable
                MemFact lhs = (MemFact) storeFact.lhs;
                VarFact rhs = (VarFact) storeFact.rhs;
                for (ParamFact param : currentFunc.paramFacts) {
                    if (param.alias == null) continue;
                    if (param.alias.equals(lhs) && param.register.equals(rhs)) {
                        forRemoval.add(i);
                    }
                }
            }
            forRemoval.forEach(index -> facts.remove((int) index));
        }
        // implement rule 2
        for (Integer[] endpoints : getAllFunctions()) { // we have to call this function again because we just removed some lines from the facts list
            int start = endpoints[0];
            int end = endpoints[1];
            EnterSubFact currentFunc = (EnterSubFact) facts.get(start);
            for (int i = start; i < end; i++) {
                for (ParamFact param : currentFunc.paramFacts) {
                    if (param.alias != null) replaceAllInstancesOfMem(facts.get(i), param.alias, param.name);
                }
            }
        }
    }

    private void resolveOutParams() {
        for (Integer[] endpoints : getAllFunctions()) {
            int start = endpoints[0];
            int end = endpoints[1];
            EnterSubFact currentFunc = (EnterSubFact) facts.get(start);
            ParamFact outParam = null;
            for (ParamFact param : currentFunc.paramFacts) {
                if (param.is_result) outParam = param;
            }
            if (outParam == null) continue; // this function does not have an output
            int i;
            for (i = start; i < end; i++) {
                replaceAllInstancesOfVar(facts.get(i), outParam.register, outParam.name);
            }
        }
    }

    /**
     * Replace references to registers with references to the literals they represent.
     *
     * Rules:
     * 1. If we run into a bad instruction, reset all register values.
     * 2. For registers in non-assignments, change them to their mapped values.
     * 3. For registers on the rhs of assignments, change them to their mapped values.
     * 4. For registers on the lhs of assignments, update their mapping to whatever is on the rhs.
     * 5. To update, the rhs must only consist of literals, and registers which are mapped to literals.
     * 6. Redundant facts include assignments of registers which have been fully resolved up until the next assignment.
     *
     * Bad instructions include:
     * - jumps
     * - conditional jumps
     * - enter subs
     * - exit subs
     * - calls
     *
     * If we reach a register assignment:
     * - Mark the previous assignment (if not wiped) to be removed.
     * - Mark this assignment for potential removal.
     * - Replace the rhs as usual.
     * - Now if the rhs only contains literals, update the value of this register, otherwise wipe it.
     */
    private void resolveRegisters() {
        // these mapped ExpFacts are expected to only contain literals
        Map<VarFact, ExpFact> registerValues = new HashMap<>();
        // list of redundant fact objects to remove from the global facts list
        List<InstFact> redundantFacts = new ArrayList<>();
        // potentially redundant facts, to be confirmed when a reassignment to the register (map key) is found
        Map<VarFact, InstFact> potentillyRedundantFacts = new HashMap<>();

        for (InstFact fact : facts) {
            if (fact instanceof JmpFact || fact instanceof CjmpFact || fact instanceof EnterSubFact ||
                    fact instanceof ExitSubFact || fact instanceof CallFact) {
                for (VarFact register : registerValues.keySet()) {
                    replaceAllInstancesOfVar(fact, register, registerValues.get(register));
                }
                // bad instruction: clear register mappings
                registerValues = new HashMap<>();
                potentillyRedundantFacts = new HashMap<>();
            } else if (fact instanceof LoadFact || fact instanceof MoveFact) {
                // this instruction might update the value of a register
                VarFact register = fact instanceof LoadFact ? (VarFact) ((LoadFact) fact).lhs : (VarFact) ((MoveFact) fact).lhs;
                ExpFact rhs = fact instanceof LoadFact ? ((LoadFact) fact).rhs : ((MoveFact) fact).rhs;
                if (rhs == null) continue; // skip load assignments with empty rhs
                // replace the rhs with register mappings as per usual
                for (VarFact assignedRegister : registerValues.keySet()) {
                    if (!(rhs instanceof VarFact)) replaceAllInstancesOfVar(rhs, assignedRegister, registerValues.get(assignedRegister));
                    else if (rhs.equals(assignedRegister)) rhs = registerValues.get(assignedRegister);
                }
                // a register is not assigned here - we have done what we need
                if (!isRegister(register)) continue;
                // a register is assigned here: remove the previous assignment (by moving it to redundantFacts)
                if (potentillyRedundantFacts.containsKey(register)) {
                    redundantFacts.add(potentillyRedundantFacts.get(register));
                    potentillyRedundantFacts.remove(register);
                }
                // if the rhs (after being translated earlier) only contains literals, map this register to that value
                if (onlyContainsType(rhs, LiteralFact.class)) {
                    registerValues.put(register, rhs);
                    potentillyRedundantFacts.put(register, fact);
                } else {
                    // otherwise, we don't know what this value is, so clear any current mappings
                    registerValues.remove(register);
                }
            } else {
                // for any other fact type, substitute the register for its mapped literal as per usual
                for (VarFact register : registerValues.keySet()) {
                    replaceAllInstancesOfVar(fact, register, registerValues.get(register));
                }
            }
        }
        redundantFacts.forEach(fact -> facts.remove(fact));
    }

    private void printAllFacts() {
        facts.forEach(System.out::print);
    }

    /**
     * Checks if all atomic facts in the given expression are of the given type.
     * Since the only atomic facts that exist are VarFacts and Literals, it only makes sense to call this function with
     * one of these as the 'type' argument.
     * Integers such as those used in extract facts do not count as atomic facts and are ignored, as are certain strings
     * such as cjump target labels.
     */
    private boolean onlyContainsType(Fact fact, Class<? extends ExpFact> type) {
        if (fact instanceof BopFact) {
            BopFact bopFact = (BopFact) fact;
            return onlyContainsType(bopFact.e1, type) && onlyContainsType(bopFact.e2, type);
        } else if (fact instanceof ExtractFact) {
            ExtractFact extractFact = (ExtractFact) fact;
            return onlyContainsType(extractFact.variable, type);
        } else if (fact instanceof MemFact) {
            MemFact memFact = (MemFact) fact;
            return onlyContainsType(memFact.exp, type);
        } else if (fact instanceof UopFact) {
            UopFact uopFact = (UopFact) fact;
            return onlyContainsType(uopFact.e1, type);
        } else if (fact instanceof AssignFact) {
            AssignFact assignFact = (AssignFact) fact;
            return onlyContainsType(assignFact.lhs, type) && onlyContainsType(assignFact.rhs, type);
        } else if (fact instanceof  CjmpFact) {
            CjmpFact cjmpFact = (CjmpFact) fact;
            return onlyContainsType(cjmpFact.condition, type);
        } else if (fact instanceof LiteralFact) {
            return type.equals(LiteralFact.class);
        } else if (fact instanceof VarFact) {
            return type.equals(VarFact.class);
        } else {
            System.err.printf("Unhandled expression in expOnlyContains search: %s\n", fact);
            return false;
        }
    }

    /**
     * Note: doesn't work on VarFacts alone, because you need to change the var in the parent fact.
     * Doesn't apply to function headers or param instructions.
     *
     * If the newExp is not a variable, then facts that must take variables, such as cjumps and extractions, will not
     * be modified.
     */
    private void replaceAllInstancesOfVar(Fact fact, VarFact oldVar, ExpFact newExp) {
        if (fact instanceof BopFact) {
            BopFact bopFact = (BopFact) fact;
            if (!(bopFact.e1 instanceof VarFact)) replaceAllInstancesOfVar(bopFact.e1, oldVar, newExp);
            else if (bopFact.e1.equals(oldVar)) bopFact.e1 = newExp;
            if (!(bopFact.e2 instanceof VarFact)) replaceAllInstancesOfVar(bopFact.e2, oldVar, newExp);
            else if (bopFact.e2.equals(oldVar)) bopFact.e2 = newExp;
        } else if (fact instanceof ExtractFact) {
            ExtractFact extractFact = (ExtractFact) fact;
            if (extractFact.variable.equals(oldVar) && newExp instanceof VarFact) extractFact.variable = (VarFact) newExp;
        } else if (fact instanceof MemFact) {
            MemFact memFact = (MemFact) fact;
            if (!(memFact.exp instanceof VarFact)) replaceAllInstancesOfVar(memFact.exp, oldVar, newExp);
            else if (memFact.exp.equals(oldVar)) memFact.exp = newExp;
        } else if (fact instanceof UopFact) {
            UopFact uopFact = (UopFact) fact;
            if (!(uopFact.e1 instanceof VarFact)) replaceAllInstancesOfVar(uopFact.e1, oldVar, newExp);
            else if (uopFact.e1.equals(oldVar)) uopFact.e1 = newExp;
        } else if (fact instanceof AssignFact) {
            AssignFact assignFact = (AssignFact) fact;
            if (!(assignFact.lhs instanceof VarFact)) replaceAllInstancesOfVar(assignFact.lhs, oldVar, newExp);
            else if (assignFact.lhs.equals(oldVar)) assignFact.lhs = newExp;
            if (!(assignFact.rhs instanceof VarFact)) replaceAllInstancesOfVar(assignFact.rhs, oldVar, newExp);
            else if (assignFact.rhs.equals(oldVar)) assignFact.rhs = newExp;
        } else if (fact instanceof  CjmpFact) {
            CjmpFact cjmpFact = (CjmpFact) fact;
            if (cjmpFact.condition.equals(oldVar) && newExp instanceof VarFact) cjmpFact.condition = (VarFact) newExp;
        }
    }

    private void replaceAllInstancesOfMem(Fact fact, MemFact oldMem, VarFact newVar) {
        if (fact instanceof BopFact) {
            if (((BopFact) fact).e1 instanceof MemFact && ((BopFact) fact).e1.equals(oldMem)) {
                ((BopFact) fact).e1 = newVar;
            } else {
                replaceAllInstancesOfMem(((BopFact) fact).e1, oldMem, newVar);
            }
            if (((BopFact) fact).e2 instanceof MemFact && ((BopFact) fact).e2.equals(oldMem)) {
                ((BopFact) fact).e2 = newVar;
            } else {
                replaceAllInstancesOfMem(((BopFact) fact).e2, oldMem, newVar);
            }
        } else if (fact instanceof UopFact) {
            if (((UopFact) fact).e1 instanceof MemFact && ((UopFact) fact).e1.equals(oldMem)) {
                ((UopFact) fact).e1 = newVar;
            } else {
                replaceAllInstancesOfMem(((UopFact) fact).e1, oldMem, newVar);
            }
        } else if (fact instanceof AssignFact) {
            if (((AssignFact) fact).lhs instanceof MemFact && ((AssignFact) fact).lhs.equals(oldMem)) {
                ((AssignFact) fact).lhs = newVar;
            } else {
                replaceAllInstancesOfMem(((AssignFact) fact).lhs, oldMem, newVar);
            }
            if (((AssignFact) fact).rhs instanceof MemFact && ((AssignFact) fact).rhs.equals(oldMem)) {
                ((AssignFact) fact).rhs = newVar;
            } else {
                replaceAllInstancesOfMem(((AssignFact) fact).rhs, oldMem, newVar);
            }
        }
    }

    private void writeToFile(String text) {
        try {
            writer.write(text);
            writer.flush();
        } catch (IOException e) {
            System.err.println("Error writing to file.");
        }
    }

    private String generateUniqueName() {
        return "p" + nameCount++;
    }
}


