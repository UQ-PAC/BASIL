// src/lib/prism-ir.ts

import Prism from 'prismjs';

Prism.languages.ir = {
  comment: {
    pattern: /\/\*[\s\S]*?\*\//,
    greedy: true,
    alias: 'comment',
  },
  keyword:
    /\b(axiom|memory|shared|var|prog|entry|proc|le|be|load|store|call|indirect|assume|guard|assert|goto|unreachable|return|block|true|false|forall|exists|old|boolnot|intneg|booltobv1|zero_extend|sign_extend|extend|extract|bvconcat|eq|neq|bvnot|bvneg|bvand|bvor|bvadd|bvmul|bvudiv|bvurem|bvshl|bvlshr|bvnand|bvnor|bvxor|bvxnor|bvcomp|bvsub|bvsdiv|bvsrem|bvsmod|bvashr|bvule|bvugt|bvuge|bvult|bvslt|bvsle|bvsgt|bvsge|intadd|intmul|intsub|intdiv|intmod|intlt|intle|intgt|intge|booland|boolor|boolimplies|require|requires|ensure|ensures|invariant|rely|guarantee)\b/,
  number: /\b0x[\da-fA-F]+|\b\d+\b/,
  type: {
    pattern: /\b(bv\d+|bool)\b/,
    alias: 'data-type',
  },
  function: {
    pattern: /\b[a-zA-Z_][a-zA-Z0-9_]*?(?=\s*\()/,
    alias: 'function',
  },
  label: /%[a-zA-Z0-9_]+/,
  variable: {
    pattern: /\$[a-zA-Z0-9_]+|\b[a-zA-Z_][a-zA-Z0-9_]*_\d+\b|\bR\d+(_\d+)?\b/,
    alias: 'variable',
  },
  string: {
    pattern: /"[^"]*"|'[^']*'/,
    alias: 'string',
  },
  register: {
    pattern: /\bR\d+(_\d+)?\b/,
    alias: 'register',
  },
  operator: /:=|->|=>|==|!=|<=|>=|<|>|\+|-|\*|\/|=/,
  punctuation: /[()\[\]:{};,]/,
  property: /.+/,
};
