%option caseless
%{
#include "scanner.h"

%%

"and"       return token.KW_AND;
"or"        return token.KW_OR;
"not"       return token.KW_NOT;
"equal"         return token.KW_EQUAL;
"less"      return token.KW_LESS;
"nil"       return token.KW_NIL;
"list"      return token.KW_LIST;
"append"        return token.KW_APPEND;
"concat"        return token.KW_CONCAT;
"set"       return token.KW_SET;
"deffun"        return token.KW_DEFFUN;
"for"       return token.KW_FOR;
"if"        return token.KW_IF;
"exit"      return token.KW_EXIT;
"load"      return token.KW_LOAD;
"disp"      return token.KW_DISP;
"true"      return token.KW_TRUE;
"false"         return token.KW_FALSE;


"+"             return token.OP_PLUS;
"-"             return token.OP_MINUS;
"/"             return token.OP_DIV;
"*"             return token.OP_MULT;
"("             return token.OP_OP;
")"             return token.OP_CP;
"**"            return token.OP_DBLMULT;
","             return token.OP_OC;
"\""            return token.OP_COMMA;
;;.*            return token.COMMENT;
[ \t\n\r]       { }



[0][0-9]+                 return token.UNKNOWN
[0-9]+[a-zA-Z]+.*         return token.UNKNOWN   
[a-zA-Z][a-zA-Z0-9]*      return token.      
[0]{1}                return token.
[1-9][0-9]*               return token.
	

 