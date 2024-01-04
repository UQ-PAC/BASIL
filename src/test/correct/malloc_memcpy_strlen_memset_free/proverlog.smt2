(set-option :print-success false)
(set-info :smt-lib-version 2.6)
(set-option :smt.mbqi false)
(set-option :model.compact false)
(set-option :model.v2 true)
(set-option :pp.bv_literals false)
; done setting options


(declare-fun tickleBool (Bool) Bool)
(assert (and (tickleBool true) (tickleBool false)))
(declare-fun gamma_store64 ((Array (_ BitVec 64) Bool) (_ BitVec 64) Bool) (Array (_ BitVec 64) Bool))
(declare-fun |lambda#0| ((_ BitVec 64) (_ BitVec 64) Bool (Array (_ BitVec 64) Bool)) (Array (_ BitVec 64) Bool))
(declare-fun |lambda#1| ((_ BitVec 64) (_ BitVec 64) (_ BitVec 64) (_ BitVec 64) (Array (_ BitVec 64) (_ BitVec 8))) (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun L ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) Bool)
(declare-fun memory_load64_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) (_ BitVec 64))
(declare-fun gamma_load64 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun gamma_load32 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun gamma_load8 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun $password_addr () (_ BitVec 64))
(declare-fun $stext_addr () (_ BitVec 64))
(declare-fun $buf_addr () (_ BitVec 64))
(declare-fun memory_load8_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) (_ BitVec 8))
(declare-fun memory_store64_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64) (_ BitVec 64)) (Array (_ BitVec 64) (_ BitVec 8)))
(assert (forall ((gammaMap (Array (_ BitVec 64) Bool)) (index (_ BitVec 64)) (value Bool) ) (! (= (gamma_store64 gammaMap index value) (|lambda#0| index #x0000000000000008 value gammaMap))
 :qid |mallocmemcpystrlenmemsetfreebpl.65:34|
 :skolemid |4|
 :pattern ( (gamma_store64 gammaMap index value))
)))
(assert (forall ((|l#0| (_ BitVec 64)) (|l#1| (_ BitVec 64)) (|l#2| (_ BitVec 64)) (|l#3| (_ BitVec 64)) (|l#4| (Array (_ BitVec 64) (_ BitVec 8))) (i (_ BitVec 64)) ) (! (= (select (|lambda#1| |l#0| |l#1| |l#2| |l#3| |l#4|) i) (ite (ite (bvule |l#0| (bvadd |l#0| |l#1|))  (and (bvule |l#0| i) (bvult i (bvadd |l#0| |l#1|)))  (or (bvule |l#0| i) (bvult i (bvadd |l#0| |l#1|)))) ((_ extract 7 0) (bvlshr |l#2| (bvmul (bvsub i |l#3|) #x0000000000000008))) (select |l#4| i)))
 :qid |mallocmemcpystrlenmemsetfreebpl.81:57|
 :skolemid |18|
 :pattern ( (select (|lambda#1| |l#0| |l#1| |l#2| |l#3| |l#4|) i))
)))
(assert (forall ((memory (Array (_ BitVec 64) (_ BitVec 8))) (index@@0 (_ BitVec 64)) ) (! (= (L memory index@@0) false)
 :qid |mallocmemcpystrlenmemsetfreebpl.37:22|
 :skolemid |0|
 :pattern ( (L memory index@@0))
)))
(assert (forall ((memory@@0 (Array (_ BitVec 64) (_ BitVec 8))) (index@@1 (_ BitVec 64)) ) (! (= (memory_load64_le memory@@0 index@@1) (concat (select memory@@0 (bvadd index@@1 #x0000000000000007)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000006)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000005)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000004)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000003)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000002)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000001)) (select memory@@0 index@@1)))))))))
 :qid |mallocmemcpystrlenmemsetfreebpl.73:37|
 :skolemid |5|
 :pattern ( (memory_load64_le memory@@0 index@@1))
)))
(assert (forall ((|l#0@@0| (_ BitVec 64)) (|l#1@@0| (_ BitVec 64)) (|l#2@@0| Bool) (|l#3@@0| (Array (_ BitVec 64) Bool)) (i@@0 (_ BitVec 64)) ) (! (= (select (|lambda#0| |l#0@@0| |l#1@@0| |l#2@@0| |l#3@@0|) i@@0) (ite (ite (bvule |l#0@@0| (bvadd |l#0@@0| |l#1@@0|))  (and (bvule |l#0@@0| i@@0) (bvult i@@0 (bvadd |l#0@@0| |l#1@@0|)))  (or (bvule |l#0@@0| i@@0) (bvult i@@0 (bvadd |l#0@@0| |l#1@@0|)))) |l#2@@0| (select |l#3@@0| i@@0)))
 :qid |mallocmemcpystrlenmemsetfreebpl.65:56|
 :skolemid |17|
 :pattern ( (select (|lambda#0| |l#0@@0| |l#1@@0| |l#2@@0| |l#3@@0|) i@@0))
)))
(assert (forall ((gammaMap@@0 (Array (_ BitVec 64) Bool)) (index@@2 (_ BitVec 64)) ) (! (= (gamma_load64 gammaMap@@0 index@@2)  (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000007)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000006)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000005)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000004)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000003)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000002)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000001)) (select gammaMap@@0 index@@2)))))))))
 :qid |mallocmemcpystrlenmemsetfreebpl.57:33|
 :skolemid |2|
 :pattern ( (gamma_load64 gammaMap@@0 index@@2))
)))
(assert (forall ((gammaMap@@1 (Array (_ BitVec 64) Bool)) (index@@3 (_ BitVec 64)) ) (! (= (gamma_load32 gammaMap@@1 index@@3)  (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000003)) (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000002)) (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000001)) (select gammaMap@@1 index@@3)))))
 :qid |mallocmemcpystrlenmemsetfreebpl.53:33|
 :skolemid |1|
 :pattern ( (gamma_load32 gammaMap@@1 index@@3))
)))
(assert (forall ((gammaMap@@2 (Array (_ BitVec 64) Bool)) (index@@4 (_ BitVec 64)) ) (! (= (gamma_load8 gammaMap@@2 index@@4) (select gammaMap@@2 index@@4))
 :qid |mallocmemcpystrlenmemsetfreebpl.61:32|
 :skolemid |3|
 :pattern ( (gamma_load8 gammaMap@@2 index@@4))
)))
(assert (= $password_addr #x0000000000020060))
(assert (= $stext_addr #x0000000000020061))
(assert (= $buf_addr #x0000000000020078))
(assert (forall ((memory@@1 (Array (_ BitVec 64) (_ BitVec 8))) (index@@5 (_ BitVec 64)) ) (! (= (memory_load8_le memory@@1 index@@5) (select memory@@1 index@@5))
 :qid |mallocmemcpystrlenmemsetfreebpl.77:36|
 :skolemid |6|
 :pattern ( (memory_load8_le memory@@1 index@@5))
)))
(assert (forall ((memory@@2 (Array (_ BitVec 64) (_ BitVec 8))) (index@@6 (_ BitVec 64)) (value@@0 (_ BitVec 64)) ) (! (= (memory_store64_le memory@@2 index@@6 value@@0) (|lambda#1| index@@6 #x0000000000000008 value@@0 index@@6 memory@@2))
 :qid |mallocmemcpystrlenmemsetfreebpl.81:38|
 :skolemid |7|
 :pattern ( (memory_store64_le memory@@2 index@@6 value@@0))
)))
(push 1)
(declare-fun ControlFlow (Int Int) Int)
(declare-fun mem@0 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun mem () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@0 () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_mem () (Array (_ BitVec 64) Bool))
(declare-fun mem@1 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@1 () (Array (_ BitVec 64) Bool))
(set-info :boogie-vc-id rely_transitive)
(set-option :timeout 0)
(set-option :rlimit 0)
(assert (not
 (=> (= (ControlFlow 0 0) 4) (let ((anon0_correct  (=> (and (and (= mem@0 mem) (= Gamma_mem@0 Gamma_mem)) (and (= (memory_load8_le mem@0 #x0000000000000990) #x01) (= (memory_load8_le mem@0 #x0000000000000991) #x00))) (=> (and (and (and (= (memory_load8_le mem@0 #x0000000000000992) #x02) (= (memory_load8_le mem@0 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@0 #x000000000001fdc9) #x08))) (and (and (= (memory_load8_le mem@0 #x000000000001fdca) #x00) (= (memory_load8_le mem@0 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdcc) #x00) (= (memory_load8_le mem@0 #x000000000001fdcd) #x00)))) (=> (and (and (and (and (and (and (= (memory_load8_le mem@0 #x000000000001fdce) #x00) (= (memory_load8_le mem@0 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdd0) #x80) (= (memory_load8_le mem@0 #x000000000001fdd1) #x08))) (and (and (= (memory_load8_le mem@0 #x000000000001fdd2) #x00) (= (memory_load8_le mem@0 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdd4) #x00) (= (memory_load8_le mem@0 #x000000000001fdd5) #x00)))) (and (and (and (= (memory_load8_le mem@0 #x000000000001fdd6) #x00) (= (memory_load8_le mem@0 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@0 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@0 #x000000000001ffd9) #x08))) (and (and (= (memory_load8_le mem@0 #x000000000001ffda) #x00) (= (memory_load8_le mem@0 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@0 #x000000000001ffdc) #x00) (= (memory_load8_le mem@0 #x000000000001ffdd) #x00))))) (and (and (and (and (= (memory_load8_le mem@0 #x000000000001ffde) #x00) (= (memory_load8_le mem@0 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@0 #x0000000000020058) #x58) (= (memory_load8_le mem@0 #x0000000000020059) #x00))) (and (and (= (memory_load8_le mem@0 #x000000000002005a) #x02) (= (memory_load8_le mem@0 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@0 #x000000000002005c) #x00) (= (memory_load8_le mem@0 #x000000000002005d) #x00)))) (and (and (and (= (memory_load8_le mem@0 #x000000000002005e) #x00) (= (memory_load8_le mem@0 #x000000000002005f) #x00)) (and (= mem@1 mem@0) (= Gamma_mem@1 Gamma_mem@0))) (and (and (= (memory_load8_le mem@1 #x0000000000000990) #x01) (= (memory_load8_le mem@1 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@1 #x0000000000000992) #x02) (= (memory_load8_le mem@1 #x0000000000000993) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@1 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@1 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@1 #x000000000001fdca) #x00) (= (memory_load8_le mem@1 #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem@1 #x000000000001fdcc) #x00) (= (memory_load8_le mem@1 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@1 #x000000000001fdce) #x00) (= (memory_load8_le mem@1 #x000000000001fdcf) #x00)))) (and (and (and (= (memory_load8_le mem@1 #x000000000001fdd0) #x80) (= (memory_load8_le mem@1 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@1 #x000000000001fdd2) #x00) (= (memory_load8_le mem@1 #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem@1 #x000000000001fdd4) #x00) (= (memory_load8_le mem@1 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@1 #x000000000001fdd6) #x00) (= (memory_load8_le mem@1 #x000000000001fdd7) #x00))))) (and (and (and (and (= (memory_load8_le mem@1 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@1 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@1 #x000000000001ffda) #x00) (= (memory_load8_le mem@1 #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem@1 #x000000000001ffdc) #x00) (= (memory_load8_le mem@1 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@1 #x000000000001ffde) #x00) (= (memory_load8_le mem@1 #x000000000001ffdf) #x00)))) (and (and (and (= (memory_load8_le mem@1 #x0000000000020058) #x58) (= (memory_load8_le mem@1 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@1 #x000000000002005a) #x02) (= (memory_load8_le mem@1 #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem@1 #x000000000002005c) #x00) (= (memory_load8_le mem@1 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@1 #x000000000002005e) #x00) (= (memory_load8_le mem@1 #x000000000002005f) #x00))))))) (and (=> (= (ControlFlow 0 2) (- 0 3)) (= mem@1 mem)) (=> (= mem@1 mem) (=> (= (ControlFlow 0 2) (- 0 1)) (= Gamma_mem@1 Gamma_mem)))))))))
(let ((PreconditionGeneratedEntry_correct  (=> (= (ControlFlow 0 4) 2) anon0_correct)))
PreconditionGeneratedEntry_correct)))
))
(check-sat)
(pop 1)
; Valid
(get-info :rlimit)
(reset)
(set-option :print-success false)
(set-info :smt-lib-version 2.6)
(set-option :smt.mbqi false)
(set-option :model.compact false)
(set-option :model.v2 true)
(set-option :pp.bv_literals false)
; done setting options


(declare-fun tickleBool (Bool) Bool)
(assert (and (tickleBool true) (tickleBool false)))
(declare-fun gamma_store64 ((Array (_ BitVec 64) Bool) (_ BitVec 64) Bool) (Array (_ BitVec 64) Bool))
(declare-fun |lambda#0| ((_ BitVec 64) (_ BitVec 64) Bool (Array (_ BitVec 64) Bool)) (Array (_ BitVec 64) Bool))
(declare-fun |lambda#1| ((_ BitVec 64) (_ BitVec 64) (_ BitVec 64) (_ BitVec 64) (Array (_ BitVec 64) (_ BitVec 8))) (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun L ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) Bool)
(declare-fun memory_load64_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) (_ BitVec 64))
(declare-fun gamma_load64 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun gamma_load32 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun gamma_load8 ((Array (_ BitVec 64) Bool) (_ BitVec 64)) Bool)
(declare-fun $password_addr () (_ BitVec 64))
(declare-fun $stext_addr () (_ BitVec 64))
(declare-fun $buf_addr () (_ BitVec 64))
(declare-fun memory_load8_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64)) (_ BitVec 8))
(declare-fun memory_store64_le ((Array (_ BitVec 64) (_ BitVec 8)) (_ BitVec 64) (_ BitVec 64)) (Array (_ BitVec 64) (_ BitVec 8)))
(assert (forall ((gammaMap (Array (_ BitVec 64) Bool)) (index (_ BitVec 64)) (value Bool) ) (! (= (gamma_store64 gammaMap index value) (|lambda#0| index #x0000000000000008 value gammaMap))
 :qid |mallocmemcpystrlenmemsetfreebpl.65:34|
 :skolemid |4|
 :pattern ( (gamma_store64 gammaMap index value))
)))
(assert (forall ((|l#0| (_ BitVec 64)) (|l#1| (_ BitVec 64)) (|l#2| (_ BitVec 64)) (|l#3| (_ BitVec 64)) (|l#4| (Array (_ BitVec 64) (_ BitVec 8))) (i (_ BitVec 64)) ) (! (= (select (|lambda#1| |l#0| |l#1| |l#2| |l#3| |l#4|) i) (ite (ite (bvule |l#0| (bvadd |l#0| |l#1|))  (and (bvule |l#0| i) (bvult i (bvadd |l#0| |l#1|)))  (or (bvule |l#0| i) (bvult i (bvadd |l#0| |l#1|)))) ((_ extract 7 0) (bvlshr |l#2| (bvmul (bvsub i |l#3|) #x0000000000000008))) (select |l#4| i)))
 :qid |mallocmemcpystrlenmemsetfreebpl.81:57|
 :skolemid |18|
 :pattern ( (select (|lambda#1| |l#0| |l#1| |l#2| |l#3| |l#4|) i))
)))
(assert (forall ((memory (Array (_ BitVec 64) (_ BitVec 8))) (index@@0 (_ BitVec 64)) ) (! (= (L memory index@@0) false)
 :qid |mallocmemcpystrlenmemsetfreebpl.37:22|
 :skolemid |0|
 :pattern ( (L memory index@@0))
)))
(assert (forall ((memory@@0 (Array (_ BitVec 64) (_ BitVec 8))) (index@@1 (_ BitVec 64)) ) (! (= (memory_load64_le memory@@0 index@@1) (concat (select memory@@0 (bvadd index@@1 #x0000000000000007)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000006)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000005)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000004)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000003)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000002)) (concat (select memory@@0 (bvadd index@@1 #x0000000000000001)) (select memory@@0 index@@1)))))))))
 :qid |mallocmemcpystrlenmemsetfreebpl.73:37|
 :skolemid |5|
 :pattern ( (memory_load64_le memory@@0 index@@1))
)))
(assert (forall ((|l#0@@0| (_ BitVec 64)) (|l#1@@0| (_ BitVec 64)) (|l#2@@0| Bool) (|l#3@@0| (Array (_ BitVec 64) Bool)) (i@@0 (_ BitVec 64)) ) (! (= (select (|lambda#0| |l#0@@0| |l#1@@0| |l#2@@0| |l#3@@0|) i@@0) (ite (ite (bvule |l#0@@0| (bvadd |l#0@@0| |l#1@@0|))  (and (bvule |l#0@@0| i@@0) (bvult i@@0 (bvadd |l#0@@0| |l#1@@0|)))  (or (bvule |l#0@@0| i@@0) (bvult i@@0 (bvadd |l#0@@0| |l#1@@0|)))) |l#2@@0| (select |l#3@@0| i@@0)))
 :qid |mallocmemcpystrlenmemsetfreebpl.65:56|
 :skolemid |17|
 :pattern ( (select (|lambda#0| |l#0@@0| |l#1@@0| |l#2@@0| |l#3@@0|) i@@0))
)))
(assert (forall ((gammaMap@@0 (Array (_ BitVec 64) Bool)) (index@@2 (_ BitVec 64)) ) (! (= (gamma_load64 gammaMap@@0 index@@2)  (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000007)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000006)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000005)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000004)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000003)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000002)) (and (select gammaMap@@0 (bvadd index@@2 #x0000000000000001)) (select gammaMap@@0 index@@2)))))))))
 :qid |mallocmemcpystrlenmemsetfreebpl.57:33|
 :skolemid |2|
 :pattern ( (gamma_load64 gammaMap@@0 index@@2))
)))
(assert (forall ((gammaMap@@1 (Array (_ BitVec 64) Bool)) (index@@3 (_ BitVec 64)) ) (! (= (gamma_load32 gammaMap@@1 index@@3)  (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000003)) (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000002)) (and (select gammaMap@@1 (bvadd index@@3 #x0000000000000001)) (select gammaMap@@1 index@@3)))))
 :qid |mallocmemcpystrlenmemsetfreebpl.53:33|
 :skolemid |1|
 :pattern ( (gamma_load32 gammaMap@@1 index@@3))
)))
(assert (forall ((gammaMap@@2 (Array (_ BitVec 64) Bool)) (index@@4 (_ BitVec 64)) ) (! (= (gamma_load8 gammaMap@@2 index@@4) (select gammaMap@@2 index@@4))
 :qid |mallocmemcpystrlenmemsetfreebpl.61:32|
 :skolemid |3|
 :pattern ( (gamma_load8 gammaMap@@2 index@@4))
)))
(assert (= $password_addr #x0000000000020060))
(assert (= $stext_addr #x0000000000020061))
(assert (= $buf_addr #x0000000000020078))
(assert (forall ((memory@@1 (Array (_ BitVec 64) (_ BitVec 8))) (index@@5 (_ BitVec 64)) ) (! (= (memory_load8_le memory@@1 index@@5) (select memory@@1 index@@5))
 :qid |mallocmemcpystrlenmemsetfreebpl.77:36|
 :skolemid |6|
 :pattern ( (memory_load8_le memory@@1 index@@5))
)))
(assert (forall ((memory@@2 (Array (_ BitVec 64) (_ BitVec 8))) (index@@6 (_ BitVec 64)) (value@@0 (_ BitVec 64)) ) (! (= (memory_store64_le memory@@2 index@@6 value@@0) (|lambda#1| index@@6 #x0000000000000008 value@@0 index@@6 memory@@2))
 :qid |mallocmemcpystrlenmemsetfreebpl.81:38|
 :skolemid |7|
 :pattern ( (memory_store64_le memory@@2 index@@6 value@@0))
)))
; Valid

(push 1)
(declare-fun ControlFlow (Int Int) Int)
(declare-fun R31@0 () (_ BitVec 64))
(declare-fun R31 () (_ BitVec 64))
(declare-fun |#4@0| () (_ BitVec 64))
(declare-fun stack@0 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun stack () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun R29 () (_ BitVec 64))
(declare-fun Gamma_stack@0 () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_stack () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_R29 () Bool)
(declare-fun stack@1 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun R30 () (_ BitVec 64))
(declare-fun Gamma_stack@1 () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_R30 () Bool)
(declare-fun R29@0 () (_ BitVec 64))
(declare-fun stack@2 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@2 () (Array (_ BitVec 64) Bool))
(declare-fun stack@3 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@3 () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_R0@0 () Bool)
(declare-fun malloc_count@0 () Int)
(declare-fun malloc_count () Int)
(declare-fun malloc_end@0 () (Array Int (_ BitVec 64)))
(declare-fun malloc_base@0 () (Array Int (_ BitVec 64)))
(declare-fun R0@0 () (_ BitVec 64))
(declare-fun malloc_base () (Array Int (_ BitVec 64)))
(declare-fun malloc_end () (Array Int (_ BitVec 64)))
(declare-fun Gamma_mem () (Array (_ BitVec 64) Bool))
(declare-fun mem () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun stack@4 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@4 () (Array (_ BitVec 64) Bool))
(declare-fun mem@0 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@0 () (Array (_ BitVec 64) Bool))
(declare-fun mem@1 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@1 () (Array (_ BitVec 64) Bool))
(declare-fun mem@2 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@2 () (Array (_ BitVec 64) Bool))
(declare-fun R8@0 () (_ BitVec 64))
(declare-fun Gamma_R8@0 () Bool)
(declare-fun stack@5 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@5 () (Array (_ BitVec 64) Bool))
(declare-fun R0@1 () (_ BitVec 64))
(declare-fun stack@6 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@6 () (Array (_ BitVec 64) Bool))
(declare-fun Gamma_R0@1 () Bool)
(declare-fun R0@2 () (_ BitVec 64))
(declare-fun R1@0 () (_ BitVec 64))
(declare-fun Gamma_R1@0 () Bool)
(declare-fun R0@3 () (_ BitVec 64))
(declare-fun Gamma_R0@2 () Bool)
(declare-fun Gamma_mem@3 () (Array (_ BitVec 64) Bool))
(declare-fun mem@3 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun R8@1 () (_ BitVec 64))
(declare-fun Gamma_R8@1 () Bool)
(declare-fun mem@4 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@4 () (Array (_ BitVec 64) Bool))
(declare-fun R0@4 () (_ BitVec 64))
(declare-fun Gamma_R0@3 () Bool)
(declare-fun R8@2 () (_ BitVec 64))
(declare-fun Gamma_R8@2 () Bool)
(declare-fun mem@5 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@5 () (Array (_ BitVec 64) Bool))
(declare-fun R9@0 () (_ BitVec 64))
(declare-fun Gamma_R9@0 () Bool)
(declare-fun R9@1 () (_ BitVec 64))
(declare-fun stack@7 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@7 () (Array (_ BitVec 64) Bool))
(declare-fun mem@6 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@6 () (Array (_ BitVec 64) Bool))
(declare-fun R9@2 () (_ BitVec 64))
(declare-fun Gamma_R9@1 () Bool)
(declare-fun stack@8 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_stack@8 () (Array (_ BitVec 64) Bool))
(declare-fun mem@7 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@7 () (Array (_ BitVec 64) Bool))
(declare-fun R0@5 () (_ BitVec 64))
(declare-fun Gamma_R0@4 () Bool)
(declare-fun Gamma_R0@5 () Bool)
(declare-fun R0@6 () (_ BitVec 64))
(declare-fun R0@7 () (_ BitVec 64))
(declare-fun Gamma_R0@6 () Bool)
(declare-fun mem@8 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@8 () (Array (_ BitVec 64) Bool))
(declare-fun R8@3 () (_ BitVec 64))
(declare-fun Gamma_R8@3 () Bool)
(declare-fun mem@9 () (Array (_ BitVec 64) (_ BitVec 8)))
(declare-fun Gamma_mem@9 () (Array (_ BitVec 64) Bool))
(declare-fun R0@8 () (_ BitVec 64))
(declare-fun Gamma_R0@7 () Bool)
(set-info :boogie-vc-id main)
(set-option :timeout 0)
(set-option :rlimit 0)
(assert (not
 (=> (= (ControlFlow 0 0) 7) (let ((lmain_correct  (=> (and (and (= R31@0 (bvadd R31 #xffffffffffffffc0)) (= |#4@0| (bvadd R31@0 #x0000000000000030))) (and (= stack@0 (memory_store64_le stack |#4@0| R29)) (= Gamma_stack@0 (gamma_store64 Gamma_stack |#4@0| Gamma_R29)))) (=> (and (and (and (= stack@1 (memory_store64_le stack@0 (bvadd |#4@0| #x0000000000000008) R30)) (= Gamma_stack@1 (gamma_store64 Gamma_stack@0 (bvadd |#4@0| #x0000000000000008) Gamma_R30))) (= R29@0 (bvadd R31@0 #x0000000000000030))) (and (and (= stack@2 (memory_store64_le stack@1 (bvadd R29@0 #xfffffffffffffff8) #x0000000000000000)) (= Gamma_stack@2 (gamma_store64 Gamma_stack@1 (bvadd R29@0 #xfffffffffffffff8) true))) (and (= stack@3 (memory_store64_le stack@2 (bvadd R29@0 #xfffffffffffffff0) #x0000000000000000)) (= Gamma_stack@3 (gamma_store64 Gamma_stack@2 (bvadd R29@0 #xfffffffffffffff0) true))))) (and (=> (= (ControlFlow 0 2) (- 0 6)) (bvugt #x000000000000000b #x0000000000000000)) (=> (bvugt #x000000000000000b #x0000000000000000) (and (=> (= (ControlFlow 0 2) (- 0 5)) (= true true)) (=> (= true true) (=> (= Gamma_R0@0 true) (=> (and (and (= malloc_count@0 (+ malloc_count 1)) (bvugt (select malloc_end@0 malloc_count@0) (select malloc_base@0 malloc_count@0))) (and (= R0@0 (select malloc_base@0 malloc_count@0)) (= (select malloc_end@0 malloc_count@0) (bvadd R0@0 #x000000000000000b)))) (=> (and (and (and (and (forall ((i@@1 Int) ) (!  (=> (not (= i@@1 malloc_count@0)) (or (bvugt (select malloc_base@0 malloc_count@0) (select malloc_end@0 i@@1)) (bvult (select malloc_end@0 malloc_count@0) (select malloc_base@0 i@@1))))
 :qid |mallocmemcpystrlenmemsetfreebpl.472:19|
 :skolemid |9|
)) (forall ((i@@2 Int) ) (!  (=> (not (= i@@2 malloc_count@0)) (and (= (select malloc_base@0 i@@2) (select malloc_base i@@2)) (= (select malloc_end@0 i@@2) (select malloc_end i@@2))))
 :qid |mallocmemcpystrlenmemsetfreebpl.473:19|
 :skolemid |10|
))) (and (bvuge R0@0 #x0000000005f5e100) (forall ((i@@3 (_ BitVec 64)) ) (!  (=> (and (bvuge i@@3 R0@0) (bvult i@@3 (bvadd R0@0 #x000000000000000b))) (and (select Gamma_mem i@@3) (gamma_load8 Gamma_mem i@@3)))
 :qid |mallocmemcpystrlenmemsetfreebpl.475:19|
 :skolemid |11|
)))) (and (and (= (memory_load8_le mem #x0000000000000990) #x01) (= (memory_load8_le mem #x0000000000000991) #x00)) (and (= (memory_load8_le mem #x0000000000000992) #x02) (= (memory_load8_le mem #x0000000000000993) #x00)))) (and (and (and (= (memory_load8_le mem #x000000000001fdc8) #xd0) (= (memory_load8_le mem #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem #x000000000001fdca) #x00) (= (memory_load8_le mem #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem #x000000000001fdcc) #x00) (= (memory_load8_le mem #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem #x000000000001fdce) #x00) (= (memory_load8_le mem #x000000000001fdcf) #x00))))) (=> (and (and (and (and (and (and (= (memory_load8_le mem #x000000000001fdd0) #x80) (= (memory_load8_le mem #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem #x000000000001fdd2) #x00) (= (memory_load8_le mem #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem #x000000000001fdd4) #x00) (= (memory_load8_le mem #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem #x000000000001fdd6) #x00) (= (memory_load8_le mem #x000000000001fdd7) #x00)))) (and (and (and (= (memory_load8_le mem #x000000000001ffd8) #xd4) (= (memory_load8_le mem #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem #x000000000001ffda) #x00) (= (memory_load8_le mem #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem #x000000000001ffdc) #x00) (= (memory_load8_le mem #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem #x000000000001ffde) #x00) (= (memory_load8_le mem #x000000000001ffdf) #x00))))) (and (and (and (and (= (memory_load8_le mem #x0000000000020058) #x58) (= (memory_load8_le mem #x0000000000020059) #x00)) (and (= (memory_load8_le mem #x000000000002005a) #x02) (= (memory_load8_le mem #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem #x000000000002005c) #x00) (= (memory_load8_le mem #x000000000002005d) #x00)) (and (= (memory_load8_le mem #x000000000002005e) #x00) (= (memory_load8_le mem #x000000000002005f) #x00)))) (and (and (and (= stack@4 (memory_store64_le stack@3 (bvadd R31@0 #x0000000000000018) #x0000000000020000)) (= Gamma_stack@4 (gamma_store64 Gamma_stack@3 (bvadd R31@0 #x0000000000000018) true))) (and (= mem@0 mem) (= Gamma_mem@0 Gamma_mem))) (and (and (= (memory_load8_le mem@0 #x0000000000000990) #x01) (= (memory_load8_le mem@0 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@0 #x0000000000000992) #x02) (= (memory_load8_le mem@0 #x0000000000000993) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@0 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@0 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@0 #x000000000001fdca) #x00) (= (memory_load8_le mem@0 #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem@0 #x000000000001fdcc) #x00) (= (memory_load8_le mem@0 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdce) #x00) (= (memory_load8_le mem@0 #x000000000001fdcf) #x00)))) (and (and (and (= (memory_load8_le mem@0 #x000000000001fdd0) #x80) (= (memory_load8_le mem@0 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@0 #x000000000001fdd2) #x00) (= (memory_load8_le mem@0 #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem@0 #x000000000001fdd4) #x00) (= (memory_load8_le mem@0 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@0 #x000000000001fdd6) #x00) (= (memory_load8_le mem@0 #x000000000001fdd7) #x00))))) (and (and (and (and (= (memory_load8_le mem@0 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@0 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@0 #x000000000001ffda) #x00) (= (memory_load8_le mem@0 #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem@0 #x000000000001ffdc) #x00) (= (memory_load8_le mem@0 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@0 #x000000000001ffde) #x00) (= (memory_load8_le mem@0 #x000000000001ffdf) #x00)))) (and (and (and (= (memory_load8_le mem@0 #x0000000000020058) #x58) (= (memory_load8_le mem@0 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@0 #x000000000002005a) #x02) (= (memory_load8_le mem@0 #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem@0 #x000000000002005c) #x00) (= (memory_load8_le mem@0 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@0 #x000000000002005e) #x00) (= (memory_load8_le mem@0 #x000000000002005f) #x00))))))) (and (=> (= (ControlFlow 0 2) (- 0 4)) (=> (L mem@0 (bvadd #x0000000000020000 #x0000000000000078)) Gamma_R0@0)) (=> (=> (L mem@0 (bvadd #x0000000000020000 #x0000000000000078)) Gamma_R0@0) (=> (and (= mem@1 (memory_store64_le mem@0 (bvadd #x0000000000020000 #x0000000000000078) R0@0)) (= Gamma_mem@1 (gamma_store64 Gamma_mem@0 (bvadd #x0000000000020000 #x0000000000000078) Gamma_R0@0))) (=> (and (and (and (= mem@2 mem@1) (= Gamma_mem@2 Gamma_mem@1)) (and (= (memory_load8_le mem@2 #x0000000000000990) #x01) (= (memory_load8_le mem@2 #x0000000000000991) #x00))) (and (and (= (memory_load8_le mem@2 #x0000000000000992) #x02) (= (memory_load8_le mem@2 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@2 #x000000000001fdc9) #x08)))) (=> (and (and (and (and (= (memory_load8_le mem@2 #x000000000001fdca) #x00) (= (memory_load8_le mem@2 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdcc) #x00) (= (memory_load8_le mem@2 #x000000000001fdcd) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001fdce) #x00) (= (memory_load8_le mem@2 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdd0) #x80) (= (memory_load8_le mem@2 #x000000000001fdd1) #x08)))) (and (and (and (= (memory_load8_le mem@2 #x000000000001fdd2) #x00) (= (memory_load8_le mem@2 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdd4) #x00) (= (memory_load8_le mem@2 #x000000000001fdd5) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001fdd6) #x00) (= (memory_load8_le mem@2 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@2 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@2 #x000000000001ffd9) #x08))))) (=> (and (and (and (and (and (= (memory_load8_le mem@2 #x000000000001ffda) #x00) (= (memory_load8_le mem@2 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@2 #x000000000001ffdc) #x00) (= (memory_load8_le mem@2 #x000000000001ffdd) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001ffde) #x00) (= (memory_load8_le mem@2 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@2 #x0000000000020058) #x58) (= (memory_load8_le mem@2 #x0000000000020059) #x00)))) (and (and (and (= (memory_load8_le mem@2 #x000000000002005a) #x02) (= (memory_load8_le mem@2 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@2 #x000000000002005c) #x00) (= (memory_load8_le mem@2 #x000000000002005d) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000002005e) #x00) (= (memory_load8_le mem@2 #x000000000002005f) #x00)) (and (= R8@0 (memory_load64_le mem@2 (bvadd #x0000000000020000 #x0000000000000078))) (= Gamma_R8@0  (or (gamma_load64 Gamma_mem@2 (bvadd #x0000000000020000 #x0000000000000078)) (L mem@2 (bvadd #x0000000000020000 #x0000000000000078)))))))) (and (and (and (and (= stack@5 (memory_store64_le stack@4 (bvadd R31@0 #x0000000000000008) R8@0)) (= Gamma_stack@5 (gamma_store64 Gamma_stack@4 (bvadd R31@0 #x0000000000000008) Gamma_R8@0))) (= R0@1 (bvadd #x0000000000020000 #x0000000000000061))) (and (= stack@6 (memory_store64_le stack@5 R31@0 R0@1)) (= Gamma_stack@6 (gamma_store64 Gamma_stack@5 R31@0 true)))) (and (and (and (= Gamma_R0@1 true) (forall ((i@@4 (_ BitVec 64)) ) (!  (=> (and (bvule R0@1 i@@4) (bvult i@@4 (bvadd R0@1 R0@2))) (not (= (select mem@2 i@@4) #x00)))
 :qid |mallocmemcpystrlenmemsetfreebpl.783:19|
 :skolemid |16|
))) (and (= (memory_load8_le mem@2 (bvadd R0@1 R0@2)) #x00) (bvult R0@1 (bvadd (bvadd R0@1 R0@2) #x0000000000000001)))) (and (and (= (memory_load8_le mem@2 #x0000000000000990) #x01) (= (memory_load8_le mem@2 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@2 #x0000000000000992) #x02) (= (memory_load8_le mem@2 #x0000000000000993) #x00)))))) (=> (and (and (and (and (and (and (= (memory_load8_le mem@2 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@2 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@2 #x000000000001fdca) #x00) (= (memory_load8_le mem@2 #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001fdcc) #x00) (= (memory_load8_le mem@2 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdce) #x00) (= (memory_load8_le mem@2 #x000000000001fdcf) #x00)))) (and (and (and (= (memory_load8_le mem@2 #x000000000001fdd0) #x80) (= (memory_load8_le mem@2 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@2 #x000000000001fdd2) #x00) (= (memory_load8_le mem@2 #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001fdd4) #x00) (= (memory_load8_le mem@2 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@2 #x000000000001fdd6) #x00) (= (memory_load8_le mem@2 #x000000000001fdd7) #x00))))) (and (and (and (and (= (memory_load8_le mem@2 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@2 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@2 #x000000000001ffda) #x00) (= (memory_load8_le mem@2 #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000001ffdc) #x00) (= (memory_load8_le mem@2 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@2 #x000000000001ffde) #x00) (= (memory_load8_le mem@2 #x000000000001ffdf) #x00)))) (and (and (and (= (memory_load8_le mem@2 #x0000000000020058) #x58) (= (memory_load8_le mem@2 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@2 #x000000000002005a) #x02) (= (memory_load8_le mem@2 #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem@2 #x000000000002005c) #x00) (= (memory_load8_le mem@2 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@2 #x000000000002005e) #x00) (= (memory_load8_le mem@2 #x000000000002005f) #x00)))))) (and (and (and (and (and (= R1@0 (memory_load64_le stack@6 R31@0)) (= Gamma_R1@0 (gamma_load64 Gamma_stack@6 R31@0))) (and (= R0@3 (memory_load64_le stack@6 (bvadd R31@0 #x0000000000000008))) (= Gamma_R0@2 (gamma_load64 Gamma_stack@6 (bvadd R31@0 #x0000000000000008))))) (and (and (forall ((i@@5 (_ BitVec 64)) ) (! (= (select Gamma_mem@3 i@@5) (ite  (and (bvule R0@3 i@@5) (bvult i@@5 (bvadd R0@3 R0@2))) (gamma_load8 Gamma_mem@3 (bvadd (bvsub i@@5 R0@3) R1@0)) (gamma_load8 Gamma_mem@2 i@@5)))
 :qid |mallocmemcpystrlenmemsetfreebpl.551:19|
 :skolemid |12|
)) (forall ((i@@6 (_ BitVec 64)) ) (! (= (select mem@3 i@@6) (ite  (and (bvule R0@3 i@@6) (bvult i@@6 (bvadd R0@3 R0@2))) (memory_load8_le mem@3 (bvadd (bvsub i@@6 R0@3) R1@0)) (memory_load8_le mem@2 i@@6)))
 :qid |mallocmemcpystrlenmemsetfreebpl.552:19|
 :skolemid |13|
))) (and (= (memory_load8_le mem@3 #x0000000000000990) #x01) (= (memory_load8_le mem@3 #x0000000000000991) #x00)))) (and (and (and (= (memory_load8_le mem@3 #x0000000000000992) #x02) (= (memory_load8_le mem@3 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@3 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@3 #x000000000001fdc9) #x08))) (and (and (= (memory_load8_le mem@3 #x000000000001fdca) #x00) (= (memory_load8_le mem@3 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@3 #x000000000001fdcc) #x00) (= (memory_load8_le mem@3 #x000000000001fdcd) #x00))))) (and (and (and (and (= (memory_load8_le mem@3 #x000000000001fdce) #x00) (= (memory_load8_le mem@3 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@3 #x000000000001fdd0) #x80) (= (memory_load8_le mem@3 #x000000000001fdd1) #x08))) (and (and (= (memory_load8_le mem@3 #x000000000001fdd2) #x00) (= (memory_load8_le mem@3 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@3 #x000000000001fdd4) #x00) (= (memory_load8_le mem@3 #x000000000001fdd5) #x00)))) (and (and (and (= (memory_load8_le mem@3 #x000000000001fdd6) #x00) (= (memory_load8_le mem@3 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@3 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@3 #x000000000001ffd9) #x08))) (and (and (= (memory_load8_le mem@3 #x000000000001ffda) #x00) (= (memory_load8_le mem@3 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@3 #x000000000001ffdc) #x00) (= (memory_load8_le mem@3 #x000000000001ffdd) #x00))))))) (=> (and (and (and (and (and (and (and (= (memory_load8_le mem@3 #x000000000001ffde) #x00) (= (memory_load8_le mem@3 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@3 #x0000000000020058) #x58) (= (memory_load8_le mem@3 #x0000000000020059) #x00))) (and (and (= (memory_load8_le mem@3 #x000000000002005a) #x02) (= (memory_load8_le mem@3 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@3 #x000000000002005c) #x00) (= (memory_load8_le mem@3 #x000000000002005d) #x00)))) (and (and (and (= (memory_load8_le mem@3 #x000000000002005e) #x00) (= (memory_load8_le mem@3 #x000000000002005f) #x00)) (and (= R8@1 (memory_load64_le stack@6 (bvadd R31@0 #x0000000000000018))) (= Gamma_R8@1 (gamma_load64 Gamma_stack@6 (bvadd R31@0 #x0000000000000018))))) (and (and (= mem@4 mem@3) (= Gamma_mem@4 Gamma_mem@3)) (and (= (memory_load8_le mem@4 #x0000000000000990) #x01) (= (memory_load8_le mem@4 #x0000000000000991) #x00))))) (and (and (and (and (= (memory_load8_le mem@4 #x0000000000000992) #x02) (= (memory_load8_le mem@4 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@4 #x000000000001fdc9) #x08))) (and (and (= (memory_load8_le mem@4 #x000000000001fdca) #x00) (= (memory_load8_le mem@4 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdcc) #x00) (= (memory_load8_le mem@4 #x000000000001fdcd) #x00)))) (and (and (and (= (memory_load8_le mem@4 #x000000000001fdce) #x00) (= (memory_load8_le mem@4 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdd0) #x80) (= (memory_load8_le mem@4 #x000000000001fdd1) #x08))) (and (and (= (memory_load8_le mem@4 #x000000000001fdd2) #x00) (= (memory_load8_le mem@4 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdd4) #x00) (= (memory_load8_le mem@4 #x000000000001fdd5) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@4 #x000000000001fdd6) #x00) (= (memory_load8_le mem@4 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@4 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@4 #x000000000001ffd9) #x08))) (and (and (= (memory_load8_le mem@4 #x000000000001ffda) #x00) (= (memory_load8_le mem@4 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@4 #x000000000001ffdc) #x00) (= (memory_load8_le mem@4 #x000000000001ffdd) #x00)))) (and (and (and (= (memory_load8_le mem@4 #x000000000001ffde) #x00) (= (memory_load8_le mem@4 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@4 #x0000000000020058) #x58) (= (memory_load8_le mem@4 #x0000000000020059) #x00))) (and (and (= (memory_load8_le mem@4 #x000000000002005a) #x02) (= (memory_load8_le mem@4 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@4 #x000000000002005c) #x00) (= (memory_load8_le mem@4 #x000000000002005d) #x00))))) (and (and (and (and (= (memory_load8_le mem@4 #x000000000002005e) #x00) (= (memory_load8_le mem@4 #x000000000002005f) #x00)) (and (= R0@4 (memory_load64_le mem@4 (bvadd R8@1 #x0000000000000078))) (= Gamma_R0@3  (or (gamma_load64 Gamma_mem@4 (bvadd R8@1 #x0000000000000078)) (L mem@4 (bvadd R8@1 #x0000000000000078)))))) (and (and (= (memory_load8_le mem@4 #x0000000000000990) #x01) (= (memory_load8_le mem@4 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@4 #x0000000000000992) #x02) (= (memory_load8_le mem@4 #x0000000000000993) #x00)))) (and (and (and (= (memory_load8_le mem@4 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@4 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@4 #x000000000001fdca) #x00) (= (memory_load8_le mem@4 #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem@4 #x000000000001fdcc) #x00) (= (memory_load8_le mem@4 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdce) #x00) (= (memory_load8_le mem@4 #x000000000001fdcf) #x00))))))) (and (and (and (and (and (and (= (memory_load8_le mem@4 #x000000000001fdd0) #x80) (= (memory_load8_le mem@4 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@4 #x000000000001fdd2) #x00) (= (memory_load8_le mem@4 #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem@4 #x000000000001fdd4) #x00) (= (memory_load8_le mem@4 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@4 #x000000000001fdd6) #x00) (= (memory_load8_le mem@4 #x000000000001fdd7) #x00)))) (and (and (and (= (memory_load8_le mem@4 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@4 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@4 #x000000000001ffda) #x00) (= (memory_load8_le mem@4 #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem@4 #x000000000001ffdc) #x00) (= (memory_load8_le mem@4 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@4 #x000000000001ffde) #x00) (= (memory_load8_le mem@4 #x000000000001ffdf) #x00))))) (and (and (and (and (= (memory_load8_le mem@4 #x0000000000020058) #x58) (= (memory_load8_le mem@4 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@4 #x000000000002005a) #x02) (= (memory_load8_le mem@4 #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem@4 #x000000000002005c) #x00) (= (memory_load8_le mem@4 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@4 #x000000000002005e) #x00) (= (memory_load8_le mem@4 #x000000000002005f) #x00)))) (and (and (and (= R8@2 (memory_load64_le stack@6 (bvadd R31@0 #x0000000000000018))) (= Gamma_R8@2 (gamma_load64 Gamma_stack@6 (bvadd R31@0 #x0000000000000018)))) (and (= mem@5 mem@4) (= Gamma_mem@5 Gamma_mem@4))) (and (and (= (memory_load8_le mem@5 #x0000000000000990) #x01) (= (memory_load8_le mem@5 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@5 #x0000000000000992) #x02) (= (memory_load8_le mem@5 #x0000000000000993) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@5 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@5 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@5 #x000000000001fdca) #x00) (= (memory_load8_le mem@5 #x000000000001fdcb) #x00))) (and (and (= (memory_load8_le mem@5 #x000000000001fdcc) #x00) (= (memory_load8_le mem@5 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@5 #x000000000001fdce) #x00) (= (memory_load8_le mem@5 #x000000000001fdcf) #x00)))) (and (and (and (= (memory_load8_le mem@5 #x000000000001fdd0) #x80) (= (memory_load8_le mem@5 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@5 #x000000000001fdd2) #x00) (= (memory_load8_le mem@5 #x000000000001fdd3) #x00))) (and (and (= (memory_load8_le mem@5 #x000000000001fdd4) #x00) (= (memory_load8_le mem@5 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@5 #x000000000001fdd6) #x00) (= (memory_load8_le mem@5 #x000000000001fdd7) #x00))))) (and (and (and (and (= (memory_load8_le mem@5 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@5 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@5 #x000000000001ffda) #x00) (= (memory_load8_le mem@5 #x000000000001ffdb) #x00))) (and (and (= (memory_load8_le mem@5 #x000000000001ffdc) #x00) (= (memory_load8_le mem@5 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@5 #x000000000001ffde) #x00) (= (memory_load8_le mem@5 #x000000000001ffdf) #x00)))) (and (and (and (= (memory_load8_le mem@5 #x0000000000020058) #x58) (= (memory_load8_le mem@5 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@5 #x000000000002005a) #x02) (= (memory_load8_le mem@5 #x000000000002005b) #x00))) (and (and (= (memory_load8_le mem@5 #x000000000002005c) #x00) (= (memory_load8_le mem@5 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@5 #x000000000002005e) #x00) (= (memory_load8_le mem@5 #x000000000002005f) #x00)))))))) (=> (and (and (and (and (and (and (and (and (= R9@0 (memory_load64_le mem@5 (bvadd R8@2 #x0000000000000078))) (= Gamma_R9@0  (or (gamma_load64 Gamma_mem@5 (bvadd R8@2 #x0000000000000078)) (L mem@5 (bvadd R8@2 #x0000000000000078))))) (= R9@1 (bvadd R9@0 #x0000000000000004))) (and (= stack@7 (memory_store64_le stack@6 (bvadd R29@0 #xfffffffffffffff8) R9@1)) (= Gamma_stack@7 (gamma_store64 Gamma_stack@6 (bvadd R29@0 #xfffffffffffffff8) Gamma_R9@0)))) (and (and (= mem@6 mem@5) (= Gamma_mem@6 Gamma_mem@5)) (and (= (memory_load8_le mem@6 #x0000000000000990) #x01) (= (memory_load8_le mem@6 #x0000000000000991) #x00)))) (and (and (and (= (memory_load8_le mem@6 #x0000000000000992) #x02) (= (memory_load8_le mem@6 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@6 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@6 #x000000000001fdc9) #x08))) (and (and (= (memory_load8_le mem@6 #x000000000001fdca) #x00) (= (memory_load8_le mem@6 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@6 #x000000000001fdcc) #x00) (= (memory_load8_le mem@6 #x000000000001fdcd) #x00))))) (and (and (and (and (= (memory_load8_le mem@6 #x000000000001fdce) #x00) (= (memory_load8_le mem@6 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@6 #x000000000001fdd0) #x80) (= (memory_load8_le mem@6 #x000000000001fdd1) #x08))) (and (and (= (memory_load8_le mem@6 #x000000000001fdd2) #x00) (= (memory_load8_le mem@6 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@6 #x000000000001fdd4) #x00) (= (memory_load8_le mem@6 #x000000000001fdd5) #x00)))) (and (and (and (= (memory_load8_le mem@6 #x000000000001fdd6) #x00) (= (memory_load8_le mem@6 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@6 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@6 #x000000000001ffd9) #x08))) (and (and (= (memory_load8_le mem@6 #x000000000001ffda) #x00) (= (memory_load8_le mem@6 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@6 #x000000000001ffdc) #x00) (= (memory_load8_le mem@6 #x000000000001ffdd) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@6 #x000000000001ffde) #x00) (= (memory_load8_le mem@6 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@6 #x0000000000020058) #x58) (= (memory_load8_le mem@6 #x0000000000020059) #x00))) (and (and (= (memory_load8_le mem@6 #x000000000002005a) #x02) (= (memory_load8_le mem@6 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@6 #x000000000002005c) #x00) (= (memory_load8_le mem@6 #x000000000002005d) #x00)))) (and (and (and (= (memory_load8_le mem@6 #x000000000002005e) #x00) (= (memory_load8_le mem@6 #x000000000002005f) #x00)) (and (= R9@2 (memory_load64_le mem@6 (bvadd R8@2 #x0000000000000078))) (= Gamma_R9@1  (or (gamma_load64 Gamma_mem@6 (bvadd R8@2 #x0000000000000078)) (L mem@6 (bvadd R8@2 #x0000000000000078)))))) (and (and (= stack@8 (memory_store64_le stack@7 (bvadd R31@0 #x0000000000000010) R9@2)) (= Gamma_stack@8 (gamma_store64 Gamma_stack@7 (bvadd R31@0 #x0000000000000010) Gamma_R9@1))) (and (= mem@7 mem@6) (= Gamma_mem@7 Gamma_mem@6))))) (and (and (and (and (= (memory_load8_le mem@7 #x0000000000000990) #x01) (= (memory_load8_le mem@7 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@7 #x0000000000000992) #x02) (= (memory_load8_le mem@7 #x0000000000000993) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@7 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@7 #x000000000001fdca) #x00) (= (memory_load8_le mem@7 #x000000000001fdcb) #x00)))) (and (and (and (= (memory_load8_le mem@7 #x000000000001fdcc) #x00) (= (memory_load8_le mem@7 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdce) #x00) (= (memory_load8_le mem@7 #x000000000001fdcf) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001fdd0) #x80) (= (memory_load8_le mem@7 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@7 #x000000000001fdd2) #x00) (= (memory_load8_le mem@7 #x000000000001fdd3) #x00))))))) (and (and (and (and (and (and (= (memory_load8_le mem@7 #x000000000001fdd4) #x00) (= (memory_load8_le mem@7 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdd6) #x00) (= (memory_load8_le mem@7 #x000000000001fdd7) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@7 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@7 #x000000000001ffda) #x00) (= (memory_load8_le mem@7 #x000000000001ffdb) #x00)))) (and (and (and (= (memory_load8_le mem@7 #x000000000001ffdc) #x00) (= (memory_load8_le mem@7 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@7 #x000000000001ffde) #x00) (= (memory_load8_le mem@7 #x000000000001ffdf) #x00))) (and (and (= (memory_load8_le mem@7 #x0000000000020058) #x58) (= (memory_load8_le mem@7 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@7 #x000000000002005a) #x02) (= (memory_load8_le mem@7 #x000000000002005b) #x00))))) (and (and (and (and (= (memory_load8_le mem@7 #x000000000002005c) #x00) (= (memory_load8_le mem@7 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@7 #x000000000002005e) #x00) (= (memory_load8_le mem@7 #x000000000002005f) #x00))) (and (and (= R0@5 (memory_load64_le mem@7 (bvadd R8@2 #x0000000000000078))) (= Gamma_R0@4  (or (gamma_load64 Gamma_mem@7 (bvadd R8@2 #x0000000000000078)) (L mem@7 (bvadd R8@2 #x0000000000000078))))) (and (= Gamma_R0@5 true) (forall ((i@@7 (_ BitVec 64)) ) (!  (=> (and (bvule R0@5 i@@7) (bvult i@@7 (bvadd R0@5 R0@6))) (not (= (select mem@7 i@@7) #x00)))
 :qid |mallocmemcpystrlenmemsetfreebpl.783:19|
 :skolemid |16|
))))) (and (and (and (= (memory_load8_le mem@7 (bvadd R0@5 R0@6)) #x00) (bvult R0@5 (bvadd (bvadd R0@5 R0@6) #x0000000000000001))) (and (= (memory_load8_le mem@7 #x0000000000000990) #x01) (= (memory_load8_le mem@7 #x0000000000000991) #x00))) (and (and (= (memory_load8_le mem@7 #x0000000000000992) #x02) (= (memory_load8_le mem@7 #x0000000000000993) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@7 #x000000000001fdc9) #x08)))))) (and (and (and (and (and (= (memory_load8_le mem@7 #x000000000001fdca) #x00) (= (memory_load8_le mem@7 #x000000000001fdcb) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdcc) #x00) (= (memory_load8_le mem@7 #x000000000001fdcd) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001fdce) #x00) (= (memory_load8_le mem@7 #x000000000001fdcf) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdd0) #x80) (= (memory_load8_le mem@7 #x000000000001fdd1) #x08)))) (and (and (and (= (memory_load8_le mem@7 #x000000000001fdd2) #x00) (= (memory_load8_le mem@7 #x000000000001fdd3) #x00)) (and (= (memory_load8_le mem@7 #x000000000001fdd4) #x00) (= (memory_load8_le mem@7 #x000000000001fdd5) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001fdd6) #x00) (= (memory_load8_le mem@7 #x000000000001fdd7) #x00)) (and (= (memory_load8_le mem@7 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@7 #x000000000001ffd9) #x08))))) (and (and (and (and (= (memory_load8_le mem@7 #x000000000001ffda) #x00) (= (memory_load8_le mem@7 #x000000000001ffdb) #x00)) (and (= (memory_load8_le mem@7 #x000000000001ffdc) #x00) (= (memory_load8_le mem@7 #x000000000001ffdd) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000001ffde) #x00) (= (memory_load8_le mem@7 #x000000000001ffdf) #x00)) (and (= (memory_load8_le mem@7 #x0000000000020058) #x58) (= (memory_load8_le mem@7 #x0000000000020059) #x00)))) (and (and (and (= (memory_load8_le mem@7 #x000000000002005a) #x02) (= (memory_load8_le mem@7 #x000000000002005b) #x00)) (and (= (memory_load8_le mem@7 #x000000000002005c) #x00) (= (memory_load8_le mem@7 #x000000000002005d) #x00))) (and (and (= (memory_load8_le mem@7 #x000000000002005e) #x00) (= (memory_load8_le mem@7 #x000000000002005f) #x00)) (and (= R0@7 (memory_load64_le stack@8 (bvadd R31@0 #x0000000000000010))) (= Gamma_R0@6 (gamma_load64 Gamma_stack@8 (bvadd R31@0 #x0000000000000010)))))))))) (and (=> (= (ControlFlow 0 2) (- 0 3)) true) (=> (and (and (= (memory_load64_le mem@8 $buf_addr) (memory_load64_le mem@7 $buf_addr)) (= (memory_load8_le mem@8 $password_addr) (memory_load8_le mem@7 $password_addr))) (and (forall ((i@@8 (_ BitVec 64)) ) (! (= (select Gamma_mem@8 i@@8) (ite  (and (bvule R0@7 i@@8) (bvult i@@8 (bvadd R0@7 R0@6))) true (gamma_load8 Gamma_mem@7 i@@8)))
 :qid |mallocmemcpystrlenmemsetfreebpl.630:19|
 :skolemid |14|
)) (forall ((i@@9 (_ BitVec 64)) ) (! (= (select mem@8 i@@9) (ite  (and (bvule R0@7 i@@9) (bvult i@@9 (bvadd R0@7 R0@6))) ((_ extract 7 0) #x0000000000000001) (memory_load8_le mem@7 i@@9)))
 :qid |mallocmemcpystrlenmemsetfreebpl.631:19|
 :skolemid |15|
)))) (=> (and (and (and (and (= (memory_load8_le mem@8 #x0000000000000990) #x01) (= (memory_load8_le mem@8 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@8 #x0000000000000992) #x02) (= (memory_load8_le mem@8 #x0000000000000993) #x00))) (and (and (= (memory_load8_le mem@8 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@8 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@8 #x000000000001fdca) #x00) (= (memory_load8_le mem@8 #x000000000001fdcb) #x00)))) (and (and (and (= (memory_load8_le mem@8 #x000000000001fdcc) #x00) (= (memory_load8_le mem@8 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@8 #x000000000001fdce) #x00) (= (memory_load8_le mem@8 #x000000000001fdcf) #x00))) (and (and (= (memory_load8_le mem@8 #x000000000001fdd0) #x80) (= (memory_load8_le mem@8 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@8 #x000000000001fdd2) #x00) (= (memory_load8_le mem@8 #x000000000001fdd3) #x00))))) (=> (and (and (and (and (and (and (= (memory_load8_le mem@8 #x000000000001fdd4) #x00) (= (memory_load8_le mem@8 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@8 #x000000000001fdd6) #x00) (= (memory_load8_le mem@8 #x000000000001fdd7) #x00))) (and (and (= (memory_load8_le mem@8 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@8 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@8 #x000000000001ffda) #x00) (= (memory_load8_le mem@8 #x000000000001ffdb) #x00)))) (and (and (and (= (memory_load8_le mem@8 #x000000000001ffdc) #x00) (= (memory_load8_le mem@8 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@8 #x000000000001ffde) #x00) (= (memory_load8_le mem@8 #x000000000001ffdf) #x00))) (and (and (= (memory_load8_le mem@8 #x0000000000020058) #x58) (= (memory_load8_le mem@8 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@8 #x000000000002005a) #x02) (= (memory_load8_le mem@8 #x000000000002005b) #x00))))) (and (and (and (and (= (memory_load8_le mem@8 #x000000000002005c) #x00) (= (memory_load8_le mem@8 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@8 #x000000000002005e) #x00) (= (memory_load8_le mem@8 #x000000000002005f) #x00))) (and (and (= R8@3 (memory_load64_le stack@8 (bvadd R31@0 #x0000000000000018))) (= Gamma_R8@3 (gamma_load64 Gamma_stack@8 (bvadd R31@0 #x0000000000000018)))) (and (= mem@9 mem@8) (= Gamma_mem@9 Gamma_mem@8)))) (and (and (and (= (memory_load8_le mem@9 #x0000000000000990) #x01) (= (memory_load8_le mem@9 #x0000000000000991) #x00)) (and (= (memory_load8_le mem@9 #x0000000000000992) #x02) (= (memory_load8_le mem@9 #x0000000000000993) #x00))) (and (and (= (memory_load8_le mem@9 #x000000000001fdc8) #xd0) (= (memory_load8_le mem@9 #x000000000001fdc9) #x08)) (and (= (memory_load8_le mem@9 #x000000000001fdca) #x00) (= (memory_load8_le mem@9 #x000000000001fdcb) #x00)))))) (and (and (and (and (and (= (memory_load8_le mem@9 #x000000000001fdcc) #x00) (= (memory_load8_le mem@9 #x000000000001fdcd) #x00)) (and (= (memory_load8_le mem@9 #x000000000001fdce) #x00) (= (memory_load8_le mem@9 #x000000000001fdcf) #x00))) (and (and (= (memory_load8_le mem@9 #x000000000001fdd0) #x80) (= (memory_load8_le mem@9 #x000000000001fdd1) #x08)) (and (= (memory_load8_le mem@9 #x000000000001fdd2) #x00) (= (memory_load8_le mem@9 #x000000000001fdd3) #x00)))) (and (and (and (= (memory_load8_le mem@9 #x000000000001fdd4) #x00) (= (memory_load8_le mem@9 #x000000000001fdd5) #x00)) (and (= (memory_load8_le mem@9 #x000000000001fdd6) #x00) (= (memory_load8_le mem@9 #x000000000001fdd7) #x00))) (and (and (= (memory_load8_le mem@9 #x000000000001ffd8) #xd4) (= (memory_load8_le mem@9 #x000000000001ffd9) #x08)) (and (= (memory_load8_le mem@9 #x000000000001ffda) #x00) (= (memory_load8_le mem@9 #x000000000001ffdb) #x00))))) (and (and (and (and (= (memory_load8_le mem@9 #x000000000001ffdc) #x00) (= (memory_load8_le mem@9 #x000000000001ffdd) #x00)) (and (= (memory_load8_le mem@9 #x000000000001ffde) #x00) (= (memory_load8_le mem@9 #x000000000001ffdf) #x00))) (and (and (= (memory_load8_le mem@9 #x0000000000020058) #x58) (= (memory_load8_le mem@9 #x0000000000020059) #x00)) (and (= (memory_load8_le mem@9 #x000000000002005a) #x02) (= (memory_load8_le mem@9 #x000000000002005b) #x00)))) (and (and (and (= (memory_load8_le mem@9 #x000000000002005c) #x00) (= (memory_load8_le mem@9 #x000000000002005d) #x00)) (and (= (memory_load8_le mem@9 #x000000000002005e) #x00) (= (memory_load8_le mem@9 #x000000000002005f) #x00))) (and (and (= R0@8 (memory_load64_le mem@9 (bvadd R8@3 #x0000000000000078))) (= Gamma_R0@7  (or (gamma_load64 Gamma_mem@9 (bvadd R8@3 #x0000000000000078)) (L mem@9 (bvadd R8@3 #x0000000000000078))))) (= (ControlFlow 0 2) (- 0 1))))))) (forall ((i@@10 Int) (j (_ BitVec 64)) ) (!  (=> (and (and (= (select malloc_base@0 i@@10) R0@8) (bvuge j R0@8)) (bvult j (select malloc_end@0 i@@10))) (select Gamma_mem@9 j))
 :qid |mallocmemcpystrlenmemsetfreebpl.142:20|
 :skolemid |8|
)))))))))))))))))))))))))))
(let ((PreconditionGeneratedEntry_correct  (=> (= (gamma_load8 Gamma_mem $password_addr) false) (=> (and (and (= malloc_count 0) (gamma_load32 Gamma_mem (memory_load64_le mem $stext_addr))) (and (= R31 #x0000000000000064) (= (memory_load8_le mem #x0000000000020050) #x00))) (=> (and (and (and (and (and (and (= (memory_load8_le mem #x0000000000020051) #x00) (= (memory_load8_le mem #x0000000000020052) #x00)) (and (= (memory_load8_le mem #x0000000000020053) #x00) (= (memory_load8_le mem #x0000000000020054) #x00))) (and (and (= (memory_load8_le mem #x0000000000020055) #x00) (= (memory_load8_le mem #x0000000000020056) #x00)) (and (= (memory_load8_le mem #x0000000000020057) #x00) (= (memory_load8_le mem #x0000000000020058) #x58)))) (and (and (and (= (memory_load8_le mem #x0000000000020059) #x00) (= (memory_load8_le mem #x000000000002005a) #x02)) (and (= (memory_load8_le mem #x000000000002005b) #x00) (= (memory_load8_le mem #x000000000002005c) #x00))) (and (and (= (memory_load8_le mem #x000000000002005d) #x00) (= (memory_load8_le mem #x000000000002005e) #x00)) (and (= (memory_load8_le mem #x000000000002005f) #x00) (= (memory_load8_le mem #x0000000000020060) #x07))))) (and (and (and (and (= (memory_load8_le mem #x0000000000020061) #x75) (= (memory_load8_le mem #x0000000000020062) #x73)) (and (= (memory_load8_le mem #x0000000000020063) #x65) (= (memory_load8_le mem #x0000000000020064) #x72))) (and (and (= (memory_load8_le mem #x0000000000020065) #x3a) (= (memory_load8_le mem #x0000000000020066) #x70)) (and (= (memory_load8_le mem #x0000000000020067) #x61) (= (memory_load8_le mem #x0000000000020068) #x73)))) (and (and (and (= (memory_load8_le mem #x0000000000020069) #x73) (= (memory_load8_le mem #x000000000002006a) #x00)) (and (= (memory_load8_le mem #x000000000002006b) #x00) (= (memory_load8_le mem #x0000000000000990) #x01))) (and (and (= (memory_load8_le mem #x0000000000000991) #x00) (= (memory_load8_le mem #x0000000000000992) #x02)) (and (= (memory_load8_le mem #x0000000000000993) #x00) (= (memory_load8_le mem #x000000000001fdc8) #xd0)))))) (and (and (and (and (and (= (memory_load8_le mem #x000000000001fdc9) #x08) (= (memory_load8_le mem #x000000000001fdca) #x00)) (and (= (memory_load8_le mem #x000000000001fdcb) #x00) (= (memory_load8_le mem #x000000000001fdcc) #x00))) (and (and (= (memory_load8_le mem #x000000000001fdcd) #x00) (= (memory_load8_le mem #x000000000001fdce) #x00)) (and (= (memory_load8_le mem #x000000000001fdcf) #x00) (= (memory_load8_le mem #x000000000001fdd0) #x80)))) (and (and (and (= (memory_load8_le mem #x000000000001fdd1) #x08) (= (memory_load8_le mem #x000000000001fdd2) #x00)) (and (= (memory_load8_le mem #x000000000001fdd3) #x00) (= (memory_load8_le mem #x000000000001fdd4) #x00))) (and (and (= (memory_load8_le mem #x000000000001fdd5) #x00) (= (memory_load8_le mem #x000000000001fdd6) #x00)) (and (= (memory_load8_le mem #x000000000001fdd7) #x00) (= (memory_load8_le mem #x000000000001ffd8) #xd4))))) (and (and (and (and (= (memory_load8_le mem #x000000000001ffd9) #x08) (= (memory_load8_le mem #x000000000001ffda) #x00)) (and (= (memory_load8_le mem #x000000000001ffdb) #x00) (= (memory_load8_le mem #x000000000001ffdc) #x00))) (and (and (= (memory_load8_le mem #x000000000001ffdd) #x00) (= (memory_load8_le mem #x000000000001ffde) #x00)) (and (= (memory_load8_le mem #x000000000001ffdf) #x00) (= (memory_load8_le mem #x0000000000020058) #x58)))) (and (and (and (= (memory_load8_le mem #x0000000000020059) #x00) (= (memory_load8_le mem #x000000000002005a) #x02)) (and (= (memory_load8_le mem #x000000000002005b) #x00) (= (memory_load8_le mem #x000000000002005c) #x00))) (and (and (= (memory_load8_le mem #x000000000002005d) #x00) (= (memory_load8_le mem #x000000000002005e) #x00)) (and (= (memory_load8_le mem #x000000000002005f) #x00) (= (ControlFlow 0 7) 2))))))) lmain_correct)))))
PreconditionGeneratedEntry_correct)))
))
(check-sat)
(get-info :reason-unknown)
