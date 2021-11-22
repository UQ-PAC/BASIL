package BilTranslating;

import Facts.Fact;
import Facts.Exp.*;
import Facts.Inst.*;
import Facts.Inst.Assign.AssignFact;
import Facts.Inst.Assign.LoadFact;
import Facts.Inst.Assign.MoveFact;
import Facts.Inst.Assign.StoreFact;
import Facts.Label;
import Facts.Parameters.InParameter;
import Facts.Parameters.OutParameter;
import Util.AssumptionViolationException;
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

/**
 * Welcome to where the magic happens!
 *
 * (how it works)
 *
 * Many layers involve operations which remove particular instructions from the global list. To do so, it is assumed
 * that no two instructions will have the same label, and hence, that the list contains no duplicate instructions. That
 * is:
 * forall i, j :: 0 <= i < list.size() && 0 <= j < list.size() && i != j ==> !facts.equals(facts.get(i), facts.get(j))
 *
 * assumption: to avoid shared-variable problems when creating the flow graph, we must assume that external lines of bil
 * are not used by multiple functions - only one function per basic block of external code. otherwise, modifying the
 * shared line will cause changes in both functions!
 *
 * NOTE: if a function adds or moves any lines within the flow graph, it is necessary to call
 * flowGraph.verifyUniqueLinesProperty() to ensure no duplicate lines were added, as this could lead to problems.
 *
 * NOTES:
 * Sometimes its ok to modify a list returned by a get method like Block.getLines(), and sometimes its not like
 * Block.getLinesInCluster(). Should this ever be ok and how do you differentiate between these cases for user
 * programmers.
 * Exps override equals and Insts don't. This is because we want identical looking lines to still be considered
 * distinct, but we do want to check if Exps look the same in methods like Fact.replace and methods in this file that
 * make use of that.
 *
 */
public class BoogieTranslator {

    // for writing the boogie output
    private BufferedWriter writer;
    // the lines in the BIL file to translate
    private final FlowGraph flowGraph;
    // for generating unique variable names and labels
    private int uniqueInt = 0;

    public BoogieTranslator(FlowGraph flowGraph, String outputFileName) {
        try {
            writer = new BufferedWriter(new FileWriter(outputFileName, false));
        } catch (IOException e) {
            System.err.println("Error setting up file writer.");
        }
        this.flowGraph = flowGraph;
    }

    /**
     * Starting point for a BIL translation.
     */
    public void translate() {
        initGlobalBlock();
        createLabels();
        optimiseSkips();
        identifyImplicitParams();
        resolveInParams();
        resolveOutParams();
        // resolveRegisters();
        addVarDeclarations();
        printAllFacts();
    }

    private void initGlobalBlock() {
        List<InstFact> globalLines = flowGraph.getGlobalBlock().getLines();
        globalLines.add(new CallFact("start", "main"));
    }

    /**
     * We want to display the labels (i.e. pc's) of all instructions whose labels are referenced elsewhere (for e.g. by
     * a jump or conditional jump).
     */
    private void createLabels() {
        List<InstFact> lines = flowGraph.getViewOfLines();
        Set<String> usedLabels = new HashSet<>();
        // get all referred labels within the flow graph
        for (InstFact line : lines) {
            String target = getJumpTarget(line);
            if (target != null) {
                usedLabels.add(target);
            }
        }
        // show all labels which are referred; hide all labels which are not
        for (InstFact line : lines) {
            Label label = line.getLabel();
            if (usedLabels.contains(label.getPc())) {
                label.show();
            } else {
                label.hide();
            }
        }
    }

    private String getJumpTarget(InstFact fact) {
        // target labels are used in jumps and cjumps
        if (fact instanceof JmpFact) {
            return ((JmpFact) fact).getTarget();
        } else if (fact instanceof CjmpFact) {
            return ((CjmpFact) fact).getTarget();
        }
        return null;
    }

    /**
     * If a skip is not jumped to, we should remove it.
     * Depends on:
     * - {@link #createLabels()}
     */
    private void optimiseSkips() {
        for (InstFact line : flowGraph.getViewOfLines()) {
            if (line instanceof NopFact && !line.getLabel().isVisible()) {
                flowGraph.removeLine(line);
            }
        }
    }

    /**
     * Many parameters are implicit in BIL, represented not as statements but as a particular pattern of loading
     * registers which have not been previously assigned within the function to memory addresses based on the SP, at the
     * beginning of the function. This method identifies this pattern and adds any parameters found to the function's
     * parameter list.
     * It is assumed that out-parameters are always explicitly stated in BIL, and therefore StatementLoader would have
     * added them already.
     * It is assumed that the first block (i.e. root block) of any function will contain all loadings of parameters.
     *
     * We assume that these store instructions contain only the register on the rhs.
     * We also assume that identical memory accesses (e.g. mem[2]) are never written differently (e.g. mem[1+1]), as
     * this is what we use for identifying and substituting implicit parameters.
     * We assume that these registers are only accessed once - i.e. by the store instruction - before they are
     * assigned.
     */
    private void identifyImplicitParams() {
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            EnterSubFact functionFact = function.getHeader();
            List<InParameter> params = functionFact.getInParams();
            FlowGraph.Block rootBlock = function.getRootBlock();
            Set<VarFact> assignedRegisters = new HashSet<>();
            for (InstFact line : rootBlock.getLines()) {
                if (line instanceof StoreFact) {
                    // store facts may represent implicit params. check conditions
                    StoreFact storeFact = (StoreFact) line;
                    if (!(storeFact.getRhs() instanceof VarFact)) {
                        continue; // rhs must be a single variable
                    }
                    VarFact rhsVar = (VarFact) storeFact.getRhs();
                    if (!isRegister(rhsVar) || assignedRegisters.contains(rhsVar)) {
                        continue; // rhs must be an unassigned register
                    }
                    InParameter param = new InParameter(new VarFact(uniqueVarName()), rhsVar);
                    param.setAlias((MemFact) storeFact.getLhs());
                    params.add(param);
                } else if (line instanceof LoadFact || line instanceof MoveFact) {
                    // if the lhs is a register, add it to the set of assigned registers
                    VarFact lhsVar = (VarFact) ((AssignFact) line).getLhs();
                    if (isRegister(lhsVar)) {
                        assignedRegisters.add(lhsVar);
                    }
                }
            }
            removeDuplicateParamsAndMerge(params);
            createCallArguments(functionFact);
        }
    }

    /**
     * Implicit params found may contain params already listed explicitly. If so, we take the var name of the explicit
     * param, and the alias of the implicit param.
     */
    private void removeDuplicateParamsAndMerge(List<InParameter> params) {
        Iterator<InParameter> iter = params.iterator();
        while (iter.hasNext()) {
            InParameter param = iter.next();
            for (InParameter otherParam : params) {
                if (param != otherParam && param.getRegister().equals(otherParam.getRegister())) {
                    // duplicate found
                    if (param.getAlias() == null) {
                        // null alias => this is the explicit param
                        otherParam.setName(param.getName());
                    } else {
                        // non-null alias => this is the implicit param
                        otherParam.setAlias(param.getAlias());
                    }
                    iter.remove();
                }
            }
        }
    }

    /**
     * Provides function calls with a list of the parameters they will need to provide arguments for.
     */
    private void createCallArguments(EnterSubFact func) {
        for (CallFact call : getCallsToFunction(func)) {
            func.getInParams().forEach(param -> call.getArgs().add(param.getRegister()));
        }
    }

    /**
     * We want to replace mem expressions which represent parameters, like mem[SP + 1], with the human-readable names
     * of those parameters.
     * We do this by first removing the initialising store fact "mem[SP + 1] := X0", then replacing all instances of
     * "mem[SP + 1]" with the variable name.
     * Depends on:
     * - {@link #identifyImplicitParams()}
     * Assumes:
     * - Registers on the RHS of the initialising store fact are reassigned before they are used again.
     * - No parameter is initialised twice (i.e. there is no more than one initialising store fact per mem exp).
     * - Equivalent mem expressions are never written differently, e.g. mem[SP + 1] is never written as mem[SP + 0 + 1].
     * - SP is equivalent at every line of code in the function, except the beginning and end.
     * - All InParameters at this point have been assigned aliases.
     * - All parameter initialisations occur in the first block of the function.
     * - ...plus many other assumptions.
     */
    private void resolveInParams() {
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            // ensure all InParameters have been assigned aliases
            for (InParameter param : function.getHeader().getInParams()) {
                if (param.getAlias() == null) {
                    throw new AssumptionViolationException(String.format(
                            "Parameter %s of function %s, with assigned register %s, was not provided with a " +
                                    "MemFact alias before resolving InParameters.",
                            param.getName(), function.getHeader().getFuncName(), param.getRegister()
                    ));
                }
            }
            // remove all parameter initialisations from the first block
            List<InstFact> forRemoval = new ArrayList<>();
            List<InstFact> firstLines = function.getRootBlock().getLines();
            for (InstFact line : firstLines) {
                if (!(line instanceof StoreFact)) continue;
                StoreFact store = (StoreFact) line;
                if (!(store.getRhs() instanceof VarFact)) continue; // assume the rhs of the stores we're looking for consist of only a variable
                MemFact lhs = (MemFact) store.getLhs();
                VarFact rhs = (VarFact) store.getRhs();
                for (InParameter param : function.getHeader().getInParams()) {
                    if (param.getAlias().equals(lhs) && param.getRegister().equals(rhs)) {
                        forRemoval.add(line);
                    }
                }
            }
            forRemoval.forEach(firstLines::remove);
            // replace all instances of the alias with the human readable parameter name
            function.getHeader().getInParams().forEach(param ->
                    function.getRootBlock().getLinesInCluster().forEach(line ->
                            replaceAllMatchingChildren(line, param.getAlias(), param.getName())));
            // ok i just did this for fun but come on, 9 function calls in a single line? :o
        }
    }

    private Set<VarFact> getLocalVarsInFunction(FlowGraph.Function function) {
        Set<VarFact> vars = new HashSet<>();
        for (InstFact line : function.getRootBlock().getLinesInCluster()) {
            if (line instanceof LoadFact || line instanceof MoveFact) {
                VarFact lhs = (VarFact) ((AssignFact) line).getLhs();
                vars.add(lhs);
            }
        }
        return vars;
    }

    /**
     * In boogie, all local variables seem to want to be initialised at the beginning of functions.
     * Do we want to make all registers local variables?
     * This should be done before memFacts are replaced by global variables, or the global variables will have var
     * initialisations.
     * Depends on:
     * - resolveRegisters()
     */
    private void addVarDeclarations() {
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            List<InstFact> firstLines = function.getRootBlock().getLines();
            for (VarFact localVar : getLocalVarsInFunction(function)) {
                firstLines.add(0, new InitFact(localVar, uniqueLabel()));
            }
        }
    }

    private List<CallFact> getCallsToFunction(EnterSubFact function) {
        List<CallFact> calls = new ArrayList<>();
        for (InstFact line : flowGraph.getViewOfLines()) {
            if (line instanceof CallFact) {
                CallFact call = (CallFact) line;
                if (call.getFuncName().equals(function.getFuncName())) {
                    calls.add(call);
                }
            }
        }
        return calls;
    }

    private boolean isRegister(VarFact varFact) {
        return varFact.getName().charAt(0) == 'X';
    }

    /**
     * Resolves outParams by crudely replacing all references to their associated register with their human-readable
     * name.
     */
    private void resolveOutParams() {
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            OutParameter outParam = function.getHeader().getOutParam();
            if (outParam == null) continue;
            function.getRootBlock().getLinesInCluster().forEach(line ->
                    replaceAllMatchingChildren(line, outParam.getRegister(), outParam.getName()));
        }
    }

    private void resolveVars() {
        for (FlowGraph.Function function : flowGraph.getFunctions()) {
            for (FlowGraph.Block block : function.getRootBlock().getBlocksInCluster()) {
                constantPropagation(block.getLines());
            }
        }
    }

    /**
     * Performs constant propagation on a list of facts. Modifies the list it is given.
     *
     * Algorithm:
     * For each line, from top to bottom:
     * If the line is an assignment with a pending-removal variable on the lhs, remove the pending-removal line.
     * Then, with the exception of the LHS of assignments, replace any instances of variables with their mapped values, if
     * such a mapping exists.
     * Then, if the result is an assignment with no variables on the RHS, compute the value of the RHS, assign it to
     * the values map and add the line for pending-removal.
     */
    private void constantPropagation(List<InstFact> lines) {
        // these mapped ExpFacts are expected to only contain literals
        Map<VarFact, LiteralFact> values = new HashMap<>();
        // assignments that will be removed if the lhs variable is re-assigned later
        Map<VarFact, AssignFact> pendingRemoval = new HashMap<>();
        // list of lines that will be removed once the loop exits
        List<AssignFact> toRemove = new ArrayList<>();
        for (InstFact line : lines) {
            // if this is an assignment, remove any assignments that pending removal, that contain this lhs
            if (line instanceof AssignFact) {
                AssignFact assignment = (AssignFact) line;
                if (assignment.getLhs() instanceof VarFact) {
                    VarFact variable = (VarFact) assignment.getLhs();
                    if (pendingRemoval.containsKey(variable)) {
                        toRemove.add(pendingRemoval.get(variable));
                        pendingRemoval.remove(variable);
                    }
                }
            }
            // with the exception of the lhs of assignments, replace any instances of variables with their mapped values
            // note that we don't make an exception for store assignments because their lhs is always a memFact, not var
            if (line instanceof LoadFact || line instanceof MoveFact) {
                AssignFact assignment = (AssignFact) line;
                for (VarFact variable : values.keySet()) {
                    // since we can't call replaceAllMatchingChildren on the whole line, we have to perform it on the rhs manually
                    replaceAllMatchingChildren(assignment.getRhs(), variable, values.get(variable));
                    if (assignment.getRhs().equals(variable)) {
                        assignment.replace(variable, values.get(variable));
                    }
                }
                /* if the result has no variables on the rhs, compute the value of the rhs, assign it to
                the values map and add the line for pending-removal */
                if (onlyContainsType(line, LiteralFact.class)) {

                }
            } else {
                values.forEach((variable, literal) -> replaceAllMatchingChildren(line, variable, literal)); // fixme: warning: this may cause some cast exceptions as some facts may expect a var, but get a literal instead
            }

        }
    }

    private <T> T computeLiteral(ExpFact expression) {
        // todo
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
        if (type == LiteralFact.class || type == VarFact.class) {
            return type.isAssignableFrom(fact.getClass());
        }
        for (ExpFact child : fact.getChildren()) {
            if (!onlyContainsType(child, type)) {
                return false;
            }
        }
        return true;
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

    // recursively replaces all children of this fact which match the given fact, with the other given fact
    // works because getChildren returns ExpFacts and ExpFacts override equals(), unlike InstFacts which are inherently
    // unique
    private void replaceAllMatchingChildren(Fact parent, ExpFact oldExp, ExpFact newExp) {
        parent.getChildren().forEach(child -> replaceAllMatchingChildren(child, oldExp, newExp));
        parent.replace(oldExp, newExp);
    }

    private void writeToFile(String text) {
        try {
            writer.write(text);
            writer.flush();
        } catch (IOException e) {
            System.err.println("Error writing to file.");
        }
    }

    private String uniqueVarName() {
        return "p" + uniqueNum();
    }

    private String uniqueLabel() {
        return "l" + uniqueNum();
    }

    private int uniqueNum() {
        return uniqueInt++;
    }
}


