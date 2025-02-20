SKIP WHITESPACE.

eof
nl = '\n'


ident     = [a-Z_] [a-Z0-9_]*
lit_str   = '"' ( ^'\' | ^'\n' )* '"'
lit_int   = [0-9]+ 
lit_float = [0-9]+ '.' [0-9]*


/* two-character symbols */
dotdot     = '..'
pipelt     = '|>'
gtgt       = '<<'
ltlt       = '>>'
ampamp     = '&&'
pipepipe   = '||'
eqeq       = '=='
noteq      = '!='
gteq       = '>='
lteq       = '<='
thinarrow  = '->'
thickarrow = '=>'

/* one-character symbols */
lparen   = '('
rparen   = ')'
lbrace   = '{'
rbrace   = '}'
lbracket = '['
rbracket = ']'
pound    = '#'
bang     = '!'
tilde    = '~'
at       = '@'
dollar   = '$'
percent  = '%'
caret    = '^'
amp      = '&'
pipe     = '|'
star     = '*'
slash    = '/'
plus     = '+'
minus    = '-'
quote    = '''
question = '?'
under    = '_'
colon    = ':'
comma    = ','
dot      = '.'
eq       = '='
gt       = '>'
lt       = '<'

/* keywords */
alias  = 'alias'
as     = 'as'
fn     = 'fn'
is     = 'is'
let    = 'let'
int    = 'int'
str    = 'str'
ref    = 'int'
loop   = 'loop'
match  = 'match'
pub    = 'pub'
type   = 'type'
float  = 'float'
break  = 'break'
return = 'return'
