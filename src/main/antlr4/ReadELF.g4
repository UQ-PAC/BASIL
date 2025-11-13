grammar ReadELF;

syms : NEWLINE* relocationTable+ symbolTable+ ;

/*
    Offset is the offset where the symbol value should go
    Info tells us two things - the type (terminates the exact calculation depends on the arch) and the symbol index in the symtab
    Type - type of the symbol according to the ABI
    Sym value is the addend to be added to the symbol resolution
    Sym name and addend - a pretty printing of the symbol name + addend.
*/
relocationTable : (empty='There are no relocations in this file.') NEWLINE* | (relocationTableHeader NEWLINE relocationTableRow* NEWLINE*);
relocationTableHeader :
  'Relocation section' tableName 'at offset 0x' HEX 'contains' HEX 'entries:' NEWLINE
  'Offset' 'Info' 'Type' ('Sym. Value' | 'Symbol\'s Value') ('Symbol\'s Name + Addend' | 'Sym. Name + Addend')
  ;
relocationTableRow : offset=HEX HEX relocType=STRING HEX? ((name=STRING '+' HEX) | gotName=HEX) NEWLINE;

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
symbolTable : symbolTableHeader NEWLINE symbolTableRow* NEWLINE*;
symbolTableHeader :
  'Symbol table' tableName 'contains' HEX 'entries:' NEWLINE
  'Num:' 'Value' 'Size' 'Type' 'Bind' 'Vis' 'Ndx' 'Name'  // Mainly a sanity check for the column order
  ;
symbolTableRow : (num=HEX) ':' value=HEX size=HEX entrytype=STRING bind=STRING vis=STRING ndx=(HEX | STRING) name=(HEX | STRING)? STRING? NEWLINE;
// symbolTableRow : HEX ':' HEX HEX symbolType bind vis ndx name? ;

tableName : '\'' STRING '\'' ;

/*
symbolType : 'FUNC' | 'OBJECT' | 'FILE' | 'SECTION' | 'NOTYPE' ;
bind : 'GLOBAL' | 'LOCAL' | 'WEAK' ;
vis : 'DEFAULT' | 'PROTECTED' | 'HIDDEN' | 'INTERNAL' ;
ndx : 'UND' | 'ABS' | HEX;
*/

//DEC : [0-9]+ ;
HEX : '0x'? ([0-9]|[a-f])+ ;
STRING : ([0-9]|[a-z]|[A-Z]|'_'|'$'|'[...]'|'@'|'.'|'('|')'|'-'|'/')+ ;
NEWLINE : ('\r'? '\n') | EOF;
WHITESPACE : ' '+ -> skip ;
