grammar Syms;

syms : relocationTable+ symbolTable+ ;

/*
    Offset is the offset where the symbol value should go
    Info tells us two things - the type (terminates the exact calculation depends on the arch) and the symbol index in the symtab
    Type - type of the symbol according to the ABI
    Sym value is the addend to be added to the symbol resolution
    Sym name and addend - a pretty printing of the symbol name + addend.
*/
relocationTable : relocationTableHeader relocationTableRow* ;
relocationTableHeader :
  'Relocation section \'' (ALPHA | '.')+ '\' at offset 0x' HEX 'contains' HEX 'entries:'
  'Offset' 'Info' 'Type' 'Sym. Value' 'Sym. Name + Addend'
  ;
relocationTableRow : HEX HEX (ALPHA|'_')+ HEX? (name ('+' HEX))?;

/* Guide from https://stackoverflow.com/questions/3065535/what-are-the-meanings-of-the-columns-of-the-symbol-table-displayed-by-readelf
    Num: = The symbol number
    Value = The address of the Symbol
    Size = The size of the symbol
    Type = symbol type: Func = Function, Object, File (source file name), Section = memory section, Notype = untyped absolute symbol or undefined
    Bind = GLOBAL binding means the symbol is visible outside the file. LOCAL binding is visible only in the file. WEAK is like global, the symbol can be overridden.
    Vis = Symbols can be default, protected, hidden or internal.
    Ndx = The section number the symbol is in. ABS means absolute: not adjusted to any section address's relocation
    Name = symbol name
*/
symbolTable : symbolTableHeader symbolTableRow* ;
symbolTableHeader :
  'Symbol table \'' ('.' | ALPHA)+ '\' contains' HEX 'entries:' // TODO hex is actually dec
  'Num:' 'Value' 'Size' 'Type' 'Bind' 'Vis' 'Ndx' 'Name'  // Mainly a sanity check for the column order
  ;
symbolTableRow : HEX ':' HEX HEX ALPHA ALPHA ALPHA (ALPHA | HEX) name? ;
// symbolTableRow : HEX ':' HEX HEX symbolType bind vis ndx name? ;

name : (ALPHA|'_'|'$') (ALPHA | HEX | '_' | '[...]' | '@' | '.' | '(' | ')' | '-' )*  ;

/*
symbolType : 'FUNC' | 'OBJECT' | 'FILE' | 'SECTION' | 'NOTYPE' ;
bind : 'GLOBAL' | 'LOCAL' | 'WEAK' ;
vis : 'DEFAULT' | 'PROTECTED' | 'HIDDEN' | 'INTERNAL' ;
ndx : 'UND' | 'ABS' | HEX;
*/

// DEC : [0-9]+ ;
HEX : ([0-9]|[a-f])+ ;
ALPHA : ([A-Z]|[a-z])+ ;
NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : ' '+ -> skip ;


