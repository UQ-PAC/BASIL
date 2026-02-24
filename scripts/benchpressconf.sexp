
(prover
  (name boogiez3)
  (cmd "timeout $timeout boogie /printVerifiedProceduresCount:0 $file")
  (unsat "^Boogie program verifier finished with 0 errors")
  (timeout "(.*timeout.*)|interrupted")
  (sat "^Boogie program verifier finished with [1-9]\\d* error")
  )

(prover
  (name boogiecvc5)
  (cmd "timeout $timeout boogie /printVerifiedProceduresCount:0 /proverOpt:SOLVER=cvc5  $file")
  (unsat "^Boogie program verifier finished with 0 errors")
  (timeout "(.*timeout.*)|interrupted")
  (sat "^Boogie program verifier finished with [1-9]\\d* error")
)


(prover
  (name boogiez3axioms)
  (cmd "timeout $timeout boogie /printVerifiedProceduresCount:0 /useArrayAxioms $file")
  (unsat "^Boogie program verifier finished with 0 errors")
  (timeout "(.*timeout.*)|interrupted")
  (sat "^Boogie program verifier finished with [1-9]\\d* error")
  )

(prover
  (name boogiecvc5axioms)
  (cmd "timeout $timeout boogie /printVerifiedProceduresCount:0 /useArrayAxioms /proverOpt:SOLVER=cvc5 $file")
  (unsat "^Boogie program verifier finished with 0 errors")
  (timeout "(.*timeout.*)|interrupted")
  (sat "^Boogie program verifier finished with [1-9]\\d* error")
)

(dir
  (path "src/test/correct")
  (pattern "^.*.bpl$")
  (expect (const unsat)))

(dir
  (path "src/test/incorrect")
  (pattern "^.*.bpl$")
  (expect (const sat)))


(task
  (name run-comparison)
  (synopsis "run boogie comparison on expected files")
  (action
    (run_provers
      (dirs (src/test/correct src/test/incorrect))
      (provers (boogiez3 boogiecvc5 boogiez3axioms boogiecvc5axioms))
      (timeout 30))))

