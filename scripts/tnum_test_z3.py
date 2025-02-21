from z3 import *

class TNum:
    """
    Pair of bitvectors to represent the known bits of a single bitvector, such that:
     v m | r
     0 0 | 0
     1 0 | 1
     0 1 | T
    where the combination (1,1) should never occur.
    """
    def __init__(self, v, m):
        self.v = v
        self.m = m
        assert (self.v.size() == self.m.size())
        self.w = self.v.size()

    def wf(self):
        """
        A wellformed tnum should never have a bit position
        where both the mask and value are set.
        """
        return (self.v & self.m) == BitVecVal(0,self.w)

    def contains(self, bv):
        """
        A bitvector is represented by a tnum given all non-masked
        bits are equal to those of the value.
        """
        if bv.size() < self.w:
            bv = ZeroExt(self.w - bv.size(), bv)
        elif bv.size() > self.w:
            bv = Extract(self.w - 1, 0, bv)
        
        valueExtend = ZeroExt(bv.size() - self.v.size(), self.v) if bv.size() > self.v.size() else self.v
        maskExtend = ZeroExt(bv.size() - self.m.size(), self.m) if bv.size() > self.m.size() else self.m

        return (bv & ~maskExtend) == valueExtend

    def subset(self, other):
        """
        This tnum represents an equal or smaller set of bitvectors compared to another given:
        - This tnum only masks bits that are also masked by the other
        - For bits that neither tnum masks, they agree on value
        """
        mask_cmp = (self.m & ~other.m) == BitVecVal(0,self.w)
        val_cmp = ((self.v & ~other.m) == other.v)
        return mask_cmp & val_cmp

    def union(self,other):
        mask = self.m | other.m | (self.v ^ other.v)
        val = self.v & ~mask
        return TNum(val,mask)

    def __invert__(self):
        return TNum(~self.v & ~self.m, self.m)
    
    def __neg__(self):
        tnum = TNum(BitVecVal(0, self.w), BitVecVal(0, self.w))
        return tnum - self

    def __and__(self, other):
        alpha = self.v | self.m
        beta = other.v | other.m
        v = self.v & other.v
        return TNum(v, (alpha & beta & ~v))
    
    def __or__(self, other):
        v = self.v | other.v
        mu = self.m | other.m
        return TNum(v, (mu & ~v));

    def __xor__(self, other):
        v = self.v ^ other.v
        mu = self.m| other.m
        return TNum((v & ~mu), mu)
    
    def __add__(self, other):
        sm = self.m + other.m
        sv = self.v + other.v
        sigma = sm + sv
        chi = sigma ^ sv
        mu = chi | self.m | other.m
        return TNum(sv & ~mu, mu)
    
    def __mul__(self, other):
        # a = self
        # b = other
        # v = self.v * other.v
        # mu = TNum(BitVecVal(0, self.w), BitVecVal(0, self.w))

        # for _ in range(a.w):
        #     cond = (a.v != 0) | (a.m != 0)
        #     valueTest = (a.v & 1) != 0
        #     maskTest = (a.m & 1) != 0

        #     mu = If(cond,
        #             If(valueTest, mu + TNum(BitVecVal(0, self.w), b.m),
        #                 If(maskTest, mu + TNum(BitVecVal(0, self.w), b.v | b.m), mu)),
        #             mu)
        #     a = tlshr_tnum(a, 1)
        #     b = b << 1

        # return TNum(v, BitVecVal(0, self.w)) + mu

        a = self
        b = other
        a_v = self.v
        a_m = self.m
        b_v = other.v
        b_m = other.m
        v = a_v * b_v
        mu_v = BitVecVal(0, self.w)
        mu_m = BitVecVal(0, self.w)

        for _ in range(1000):
            cond = (a_v != 0) | (a_m != 0)

            if is_false(cond):
                break

            valueTest = (a_v & 1) != 0
            maskTest = (a_m & 1) != 0

            mu_v = If(cond,
                        If(valueTest, 
                            mu_v + ((mu_v + BitVecVal(0, self.w)) & ~((((mu_m + b.m) + (mu_v + BitVecVal(0, self.w))) ^ (mu_v + BitVecVal(0, self.w))) | mu_m | b.m)),
                            If(maskTest, 
                                mu_v + ((mu_v + BitVecVal(0, self.w)) & ~((((mu_m + (b.v | b.m)) + (mu_v + BitVecVal(0, self.w))) ^ (mu_v + BitVecVal(0, self.w))) | mu_m | (b.v | b.m))),
                                mu_v
                            )
                        ),
                        mu_v
                    )

            mu_m = If(cond,
                        If(valueTest, 
                            mu_m + ((((mu_m + b.m) + (mu_v + BitVecVal(0, self.w))) ^ (mu_v + BitVecVal(0, self.w))) | mu_m | b.m),
                            If(maskTest, 
                                mu_m + ((((mu_m + (b.v | b.m)) + (mu_v + BitVecVal(0, self.w))) ^ (mu_v + BitVecVal(0, self.w))) | mu_m | (b.v | b.m)),
                                mu_m
                            )
                        ),
                        mu_m
                    )
            
            a_v = LShR(a_v, 1)
            a_m = LShR(a_m, 1)

            b_v = b_v << 1
            b_m = b_m << 1

        return TNum((v + mu_v) & ~((((BitVecVal(0, self.w) + mu_m) + (v + mu_v)) ^ (v + mu_v)) | BitVecVal(0, self.w) | mu_m), 
                    ((((BitVecVal(0, self.w) + mu_m) + (v + mu_v)) ^ (v + mu_v)) | BitVecVal(0, self.w) | mu_m))

    def __sub__(self, other):
        dv = self.v - other.v
        alpha = dv + self.m
        beta = dv - other.m
        chi = alpha ^ beta
        mu = chi | self.m | other.m
        return TNum((dv & ~mu), mu)
    
    
    def __lshift__(self, other):
        thatLB = other.v & ~other.m
        thatUB = other.v | other.m

        # Shift self by the lower bound of other
        acc_v = self.v << thatLB
        acc_m = self.m << thatLB

        # Only need to consider shift values from 0 to width-1, as anything greater will always produce 0
        for i in range(self.w):
            # Determine whether i is a possible value for other
            test = (i & ~other.m) == other.v
            # If i is a possible value, perform the shift and merge it with current accumulator
            # Otherwise, don't modify accumulator
            acc_m = If (test, acc_m | (self.m << i) | ((self.v << i) ^ acc_v), acc_m)
            acc_v = If (test, acc_v & (self.v << i), acc_v)

        # If a shift value greater than or equal to width is possible, need to merge in 0
        test = UGE(thatUB, self.w)
        acc_m = If (test, acc_m | acc_v, acc_m)
        acc_v = If (test, 0, acc_v)
        return TNum(acc_v, acc_m)
    
    def __rshift__(self, other):
        thatLB = other.v & ~other.m
        thatUB = other.v | other.m

        acc_v = self.v >> thatLB
        acc_m = self.m >> thatLB

        for i in range(self.w):
            test = (i & ~other.m) == other.v
            acc_m = If(test, acc_m | (self.m >> i) | ((self.v >> i) ^ acc_v), acc_m)
            acc_v = If(test, acc_v & (self.v >> i), acc_v)

        test = UGE(thatUB, self.w)
        acc_m = If (test, acc_m | (self.m >> self.w) | ((self.v >> self.w) ^ acc_v), acc_m)
        acc_v = If (test, acc_v & (self.v >> self.w), acc_v)
        return TNum(acc_v, acc_m)
    
    def __floordiv__(self, other):
        thatLB = other.v & ~other.m
        thatUB = other.v | other.m

        acc_v = self.v // thatLB
        acc_m = self.m // thatLB

        for i in range(self.w):
            test = (i & ~other.m) == other.v
            acc_m = If(test, acc_m | (self.m // i) | ((self.v // i) ^ acc_v), acc_m)
            acc_v = If(test, acc_v & (self.v // i), acc_v)

        test = UGE(thatUB, self.w)
        acc_m = If (test, acc_m | (self.m // self.w) | ((self.v // self.w) ^ acc_v), acc_m)
        acc_v = If (test, acc_v & (self.v // self.w), acc_v)
        return TNum(acc_v, acc_m)

# NOR
def nor_tnum(a: TNum, b: TNum):
    v = a.v | b.v
    mu = a.m | b.m
    return TNum((~v & ~mu), (~v & mu))

def nor_bitvec(a: BitVecRef, b: BitVecRef):
    return ~(a | b)

# XNOR
def xnor_tnum(a: TNum, b: TNum):
    v = ~(a.v ^ b.v)
    mu = a.m | b.m
    return TNum((v & ~mu), mu)

def xnor_bitvec(a: BitVecRef, b: BitVecRef):
    return ~(a ^ b)

# NAND
def nand_tnum(a: TNum, b: TNum):
    alpha = a.v | a.m
    beta = b.v | b.m
    v = a.v & b.v
    return TNum((~v & ~(alpha & beta & ~v)), (alpha & beta & ~v))

def nand_bitvec(a: BitVecRef, b: BitVecRef):
    return ~(a & b)

# LOGICAL SHIFT RIGHT
def tlshr_tnum(a: TNum, b: TNum):
    thatLB = b.v & ~b.m
    thatUB = b.v | b.m

    acc_v = LShR(a.v, thatLB)
    acc_m = LShR(a.m, thatLB)

    for i in range(a.w):
        test = (i & ~b.m) == b.v
        acc_m = If (test, acc_m | LShR(a.m, i) | (LShR(a.v, i) ^ acc_v), acc_m)
        acc_v = If(test, acc_v & LShR(a.v, i), acc_v)

    test = UGE(thatUB, a.w)
    acc_m = If (test, acc_m | acc_v, acc_m)
    acc_v = If (test, 0, acc_v)
    return TNum(acc_v, acc_m)

def tlshr_bitvec(a: BitVecRef, b: BitVecRef):
    return LShR(a, b)

# UNSIGNED DIVISION
def tudiv_tnum(a: TNum, b: TNum):
    dividendL = a.v & ~a.m
    dividendH = a.v | a.m

    divisorL = b.v & ~b.m
    divisorH = b.v | b.m

    if (divisorL == 0 or divisorH == 0):
        return TNum(int(0), int(-1))

    q1 = dividendL / divisorL
    q2 = dividendL / divisorH
    q3 = dividendH / divisorL
    q4 = dividendH / divisorH

    newValue = q1 & q2 & q3 & q4
    newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

    return TNum(newValue, newMask)

def tudiv_bitvec(a: BitVecRef, b: BitVecRef):
    return UDiv(a, b)

# CONCAT
def concat_tnum(a: TNum, b: TNum):
    v = (a.v << b.w) | b.v
    mu = (a.m << b.w) | b.m
    return TNum(v, mu)

def concat_bitvec(a: BitVecRef, b: BitVecRef):
    return Concat(a, b)

########################
def fresh_tnum(name, w):
    return TNum(BitVec(name + '_v',w), BitVec(name + '_m',w))

def and_all(v):
    acc = BoolVal(True)
    for i in v:
        acc = acc & i
    return acc

def quant_elim(v):
    t = Then(Tactic('simplify'), With('elim-small-bv', max_bits=8))
    g = Goal()
    g.add(v)
    r = t(g)
    assert(len(r) == 1)
    return and_all(r[0])

def check(msg, cond, prop):
    """
    Check if prop is always true given cond.
    Eliminates quantified bitvectors by enumerating all possibilities.
    """
    solver = Solver()
    solver.add(quant_elim(cond & ~prop))
    if solver.check() == unsat:
        print("Passed check for " + msg)
        return True
    elif solver.check() == sat:
        print("Failed check for " + msg + " under given model:")
        print(solver)
        print(solver.model())
        return False
    else:
        print("Failed check for " + msg + " due to timeout")
        return False

def test_bop(name, tf, tf_c=None, prec=False, width=4):
    """
    Test the soundness, wellformedness and precision of a bop transfer function.
    """
    # Various symbols
    x = fresh_tnum("x", width)
    y = fresh_tnum("y", width)
    xc = BitVec("xc", width)
    yc = BitVec("yc", width)
    # Apply both the abstract and concrete transforms
    res = tf(x,y)
    res_c = tf_c(xc,yc) if tf_c else tf(xc,yc)
    # Check for soundness and wellformedness of the result
    cond = x.wf() & y.wf() & x.contains(xc) & y.contains(yc)
    check(name + " Soundness", cond, res.contains(res_c))
    check(name + " Wellformedness", cond, res.wf())
    if prec:
        # Check for precision of the operation if necessary
        t = fresh_tnum("t", width)
        quant = ForAll([xc,yc], Implies(x.contains(xc) & y.contains(yc), t.contains(res_c)))
        check(name + " Precision", x.wf() & y.wf() & t.wf() & quant, res.subset(t))

def test_uop(name, tf, tf_c=None, prec=False, width=4):
    """
    Test the soundness, wellformedness and precision of a uop transfer function.
    """
    # Various symbols
    x = fresh_tnum("x", width)
    xc = BitVec("xc", width)
    # Apply both the abstract and concrete transforms
    res = tf(x)
    res_c = tf_c(xc) if tf_c else tf(xc)
    # Check for soundness and wellformedness of the result
    cond = x.wf() & x.contains(xc)
    check(name + " Soundness", cond, res.contains(res_c))
    check(name + " Wellformedness", cond, res.wf())
    if prec:
        # Check for precision of the operation if necessary
        t = fresh_tnum("t", width)
        quant = ForAll([xc], Implies(x.contains(xc), t.contains(res_c)))
        check(name + " Precision", x.wf() & t.wf() & quant, res.subset(t))

test_bop("TAND", lambda x, y: x & y,  prec=True)
test_bop("TOR", lambda x, y: x | y,  prec=True)
test_bop("TXOR", lambda x, y: x ^ y,  prec=True)
test_bop("TNOR", lambda x, y: nor_tnum(x, y), nor_bitvec, prec=True)
test_bop("TXNOR", lambda x, y: xnor_tnum(x, y), xnor_bitvec, prec=True)
test_bop("TNAND", lambda x, y: nand_tnum(x, y), nand_bitvec, prec=True)
test_bop("TADD", lambda x, y: x + y,  prec=True)
test_bop("TSUB", lambda x, y: x - y,  prec=True)
test_bop("TSHL", lambda x, y: x << y,  prec=True)
test_bop("TASHR", lambda x, y: x >> y,  prec=True)
test_bop("TLSHR", lambda x, y: tlshr_tnum(x, y), tlshr_bitvec, prec=True)
# test_bop("TUDIV", lambda x, y: tudiv_tnum(x, y), tudiv_bitvec, prec=True)
# test_bop("TSDIV", lambda x, y: x // y,  prec=True)
test_bop("CONCAT", lambda x, y: concat_tnum(x, y), concat_bitvec, prec=True)

test_uop("INV", lambda x: ~x, prec=True)
test_uop("NEG", lambda x: -x, prec=True)
# test_bop("TMUL", lambda x, y: x * y,  prec=True)