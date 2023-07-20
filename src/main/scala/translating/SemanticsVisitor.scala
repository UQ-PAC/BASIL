package translating
import BilParser.BilAdtBaseVisitor
import BilParser.BilAdtParser.Basic_blkContext
import BilParser.BilAdtParser.SemanticsContext
import com.google.protobuf.ByteString
import java.util.Base64
import scala.collection.JavaConverters._
import BilParser.BilAdtParser.InstructionContext
import BilParser.BilAdtParser.StmtContext
import BilParser.BilAdtParser._
import ir._
import scala.collection.mutable._
import ir.Variable._

class SemanticsVisitor(targetuuid: ByteString, context: SemanticsContext) extends BilAdtBaseVisitor[Any]{

    def ChrisUUIDToByteString(uuid: String) : ByteString = { // This probably needs a better name, but :/
        return ByteString.copyFrom(Base64.getDecoder().decode(uuid))
    }

    def createStatements(): ArrayBuffer[Statement] = {
        val basicBlks = context.basic_blk().asScala
        var statements : ArrayBuffer[Statement] = null 

        for (BasicBlk <- basicBlks) {
            val Blkuuid = ChrisUUIDToByteString(BasicBlk.uuid().getText())
            if (Blkuuid.equals(targetuuid)) {
                statements = visitBasic_blk(BasicBlk)
            }
        }
        return statements
    }
    
    override def visitBasic_blk(ctx: Basic_blkContext): ArrayBuffer[Statement] = {
        val instructions = ctx.instruction().asScala
        val statements = ArrayBuffer[Statement]()

        for (instuction <- instructions) {
            val instructionstmts = visitInstruction(instuction)
            statements ++= instructionstmts
        }

        return statements
    }

    override def visitInstruction(ctx: InstructionContext): ArrayBuffer[Statement] = {
        val statements = ArrayBuffer[Statement]()
        val stmts = ctx.stmt_string().asScala.map(_.stmt())

        for (stmt <- stmts) {
            if (stmt.assignment_stmt() != null) { // match on type would be helpful, but i can't figure out how to gat antlr to co-operate
                val statement = visitAssignment_stmt(stmt.assignment_stmt())
                statements.addOne(statement)

            } else if (stmt.call_stmt() != null) {
                val statement = visitCall_stmt(stmt.call_stmt())
                statements.addOne(statement)

            } else if (stmt.conditional_stmt() != null) {
                visitConditional_stmt(stmt.conditional_stmt()) //  probably do nothing here, but again ask about jumps

            }
        } 
        return statements
    }

    def visitAssignment_stmt(ctx: Assignment_stmtContext): LocalAssign = {
        var stmt : LocalAssign = null
        ctx match
            case a : AssignContext => stmt = visitAssign(a)
            case c : ConstDeclContext => stmt = visitConstDecl(c)
        return stmt
    }

    override def visitCall_stmt(ctx: Call_stmtContext): MemoryAssign = {
        // TODO: probably memory store, but run into the same issue as mem load 
        val mem = Memory(???, ???, ???)
        val memstore = MemoryStore(mem, ???, ???, ???, ???)
        return MemoryAssign(mem, memstore)
    }

    override def visitConditional_stmt(ctx: Conditional_stmtContext): Any = {
        return ???
        // may be useful for discovering the condition within jumps
    }

    override def visitAssign(ctx: AssignContext): LocalAssign = {
        val lhs = visitLexpr(ctx.lexpr())
        val rhs = visitExpr(ctx.expr())
        if (lhs == null || rhs == null) {
            return null
        } else {
            return LocalAssign(lhs, rhs)
        }
    }

    override def visitConstDecl(ctx: ConstDeclContext): LocalAssign  = {
        val ty = visitType(ctx.`type`())
        val name = ctx.METHOD().getText()
        val expr = visitExpr(ctx.expr())
        if (expr != null) {
            return LocalAssign(Variable(name,ty), expr)
        } else {
            return null
        }
    }

    def visitType(ctx: TypeContext): IRType = {
        ctx match 
            case e : TypeBitsContext => 
                val size = visitExpr(e.expr()).asInstanceOf[IntLiteral].value.toInt 
                return BitVecType(size)
            case _ : Any => ??? // can be extended as more types added to grammar
    }


    def visitExpr(ctx: ExprContext): Expr = {
        var value: Expr = null
        ctx match
            case e : ExprVarContext => value = visitExprVar(e)
            case e : ExprTApplyContext => value = visitExprTApply(e)
            case e : ExprSlicesContext => value = visitExprSlices(e)
            case e : ExprFieldContext => value = visitExprField(e)
            case e : ExprArrayContext => value = visitExprArray(e)
            case e : ExprLitIntContext => value = visitExprLitInt(e)
            case e : ExprLitHexContext => ??? // not in current semantics, but still unsure how this ports to IR
            case e : ExprLitBitsContext => value = visitExprLitBits(e) 
            case e : ExprLitMaskContext => ??? // ditto above comment 
            case e : ExprLitStringContext => ??? // ditto
        return value
    }

    override def visitExprVar(ctx: ExprVarContext): Expr = ??? 
    // TODO: figure out how to turn this into expr type, since usually its just something like Expr_Var(Cse0__5)                                                           

    def getExprVarText(ctx: ExprVarContext) : String = {
        if (ctx.SSYMBOL() != null) {
            return ctx.SSYMBOL().getText()
        } else {
            return ctx.METHOD().getText()
        }
    }

    override def visitExprTApply(ctx: ExprTApplyContext): Expr = {
        val str =  ctx.METHOD.getText().substring(0, ctx.METHOD.getText().lastIndexOf(".")) 
        // removes everything up to and including the last dot

        str match 
            case "Mem.read" => 
                // TODO: all signs point to this being a load instruction, but there's no indication of the size of the memory :/
                val mem = Memory(???, ???, ???)
                return MemoryLoad(mem, ???, ???, ???)
            case "not_bool" => return UnaryExpr(BoolNOT, visitExpr(ctx.expr(0)))
            case "cvt_bool_bv" => 
                // in ocaml, takes bool and turns to bitvector -> since this is usually tied to a BinaryExpr, and 
                // BinaryExpr's don't have any "evaluate" method, i have just returned the binary expr that will evaluate to a bool
                return visitExpr(ctx.expr(0))       
            case "eq_enum" => return BinaryExpr(BVXNOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "or_bool" => return BinaryExpr(BoolOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "and_bool" => return BinaryExpr(BoolAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "replicate_bits" => return ??? // i can't understand the ocaml here, probs just duplicating bitvector though 
            case "not_bits" => return UnaryExpr(BVNOT, visitExpr(ctx.expr(0)))
            case "or_bits" => return BinaryExpr(BVOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "and_bits" => return BinaryExpr(BVAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "eor_bits" => return BinaryExpr(BVXOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "eq_bits" => return BinaryExpr(BVEQ, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "add_bits" => return BinaryExpr(BVADD, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "sub_bits" => return BinaryExpr(BVSUB, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "mul_bits" => return BinaryExpr(BVMUL, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "sdiv_bits" => return BinaryExpr(BVSDIV, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "lsl_bits" => return ??? //  can't find logical shift left binop?
            case "lsr_bits" => return BinaryExpr(BVLSHR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1))) 
            case "asr_bits" => return BinaryExpr(BVASHR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "slt_bits" => return BinaryExpr(BVSLT, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "sle_bits" => return BinaryExpr(BVSLE, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
            case "append_bits" => 
                // there is no append but there is concat, so it probably does the same thing
                return BinaryExpr(BVCONCAT, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1))) 
            case "ZeroExtend" => 
                val intexpr = visitExpr(ctx.targs().asScala.tail.asInstanceOf[ExprContext])
                val int = intexpr.asInstanceOf[IntLiteral].value
                return ZeroExtend(int.toInt, visitExpr(ctx.expr(0)))
            case "SignExtend" => 
                val intexpr = visitExpr(ctx.targs().asScala.tail.asInstanceOf[ExprContext])
                val int = intexpr.asInstanceOf[IntLiteral].value
                return SignExtend(int.toInt, visitExpr(ctx.expr(0)))
    }

    override def visitExprSlices(ctx: ExprSlicesContext): Expr = {
        val (start, end) = visitSlice_expr(ctx.slice_expr())
        return Extract(start.asInstanceOf[Int], end.asInstanceOf[Int], visitExpr(ctx.expr()))
    }

    override def visitSlice_expr(ctx: Slice_exprContext): Tuple = {
        val start = visitExpr(ctx.expr(0)).asInstanceOf[IntLiteral].value.toInt
        val end = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
        return (start, end)
    }

    override def visitExprField(ctx: ExprFieldContext): Expr = {
        var value : String = null
        ctx.expr() match
            case e : ExprVarContext => value = getExprVarText(e)
            case _ : Any => visitExpr(ctx.expr())
        val field : ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText())
        return createExprVarArray(field)
    }

    override def visitExprArray(ctx: ExprArrayContext): Expr = {
        var value : String = null
        val exprs = ctx.expr().asScala
        var i = 0
        val array = ArrayBuffer[String]()
        for (i <- 0 until exprs.size) {
            exprs(i) match
                case e : ExprVarContext => value = getExprVarText(e)
                case _ : Any => visitExpr(exprs(i))
            array(i) = value
        }
        return createExprVarArray(array)
    }

    override def visitExprLitInt(ctx: ExprLitIntContext): Expr = {
        return IntLiteral(ctx.DEC().getText().toInt)
    }

    override def visitExprLitBits(ctx: ExprLitBitsContext): Expr = {
        return BitVecLiteral(Integer.parseInt(ctx.BINARY().getText(), 2), ctx.BINARY().getText().length())
    }

    def visitLexpr(ctx: LexprContext): Variable = {
        var value : Variable = null
        ctx match 
            case l : LExprVarContext => value = visitLExprVar(l)
            case l : LExprFieldContext => value = visitLExprField(l)
            case l : LExprArrayContext => value = visitLExprArray(l)
        return value
    }

    override def visitLExprVar(ctx: LExprVarContext): Variable = {
        return createExprVarArray(ArrayBuffer(getLExprVarText(ctx), "0"))  
        // this is a super janky way to do this, but nothing will ever go wrong from that, right? 
    }

    def getLExprVarText(ctx: LExprVarContext) : String = {
        if (ctx.SSYMBOL() != null) {
            return ctx.SSYMBOL().getText()
        } else {
            return ctx.METHOD().getText()
        }
    }

    override def visitLExprField(ctx: LExprFieldContext): Variable = {
        var value : String = null
        ctx.lexpr() match
            case l : LExprVarContext => value = getLExprVarText(l)
            case _ : Any => visitLexpr(ctx.lexpr()) 
        val field : ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText())
        return createExprVarArray(field)
    }

    override def visitLExprArray(ctx: LExprArrayContext): Variable = {
        var lvalue : String = null
        var value : String = null
        val exprs = ctx.expr().asScala
        var i = 1
        val array = ArrayBuffer[String]()

        ctx.lexpr() match
            case l : LExprVarContext => value = getLExprVarText(l)
            case _ : Any => visitLexpr(ctx.lexpr)
        
        for (i <- 1 until exprs.size) {
            exprs(i - 1) match
                case e : ExprVarContext => value = getExprVarText(e)
                case _ : Any => visitExpr(exprs(i-1))
            array(i) = value
        }
        
        array(0) = lvalue
        
        return createExprVarArray(array) 
    }

    def createExprVarArray(v : ArrayBuffer[String]) : Variable = {
        v(0) match //currently only works for fields & arrays of size 2, but arrays bigger than that don't seem to appear
            case "_R" => 
                val size = getSizeofRegister(v(0)) 
                val name = "R" + v(1).asInstanceOf[Int]  
                return Variable(name, BitVecType(size))

            case "_PC" => null // also doesn't exist, although may be useful when forming jumps

            case "__BranchTaken" => null // literally doesn't exist in IR, so we pray we never have to read from it

            case "PSTATE" => 
                val Rname = v(1).asInstanceOf[String] + "F"
                return Variable(Rname, BitVecType(1))

            case "BTypeNext" => null // neither does this one

            case "SP_EL0" => return Variable("R31", BitVecType(64))
    }   

    def getSizeofRegister(name : String) : Int = {
        name match 
            case "_R" => return 64 
            case "_Z" => return 128 // or _V?  vector registers haven't come up yet 
    }

}




