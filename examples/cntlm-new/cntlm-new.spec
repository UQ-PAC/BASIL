hlist_subcmp
ensures Gamma_R0

hlist_get
ensures Gamma_R0
ensures Gamma_load8(mem[R0]) for some dynamic allocation size???
ensures Gamma_load64(mem[R0]) if we say 

strlen
ensures R0 <= 4???
ensures Gamma_R0

http_parse_basic
requires Gamma_R0
requires Gamma_R2
requires Gamma_load32(mem[Gamma_R2 + 300])

strchr
ensures Gamma_R0

l001343a5:
loop invariant ugh
Gamma_R3
Gamma_R0

ntlm2_hash_password
ensures Gamma_R0