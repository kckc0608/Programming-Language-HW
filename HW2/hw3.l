%{
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
//#define DEBUG 1

// typdedef 심볼테이블
extern char* type_name_array[1000];
extern int type_name_array_size;         

// #define constant 심볼테이블
extern char* define_name_array[1000];
extern int define_name_array_size;

// #define string literal 심볼테이블
extern char* str_define_name_array[1000];
extern int str_define_name_array_size;

// 현재 보고 있는 식별자의 이름
extern char* identifier_name;             
%}

D [0-9]
L [a-zA-Z_]
H [a-fA-F0-9]
E [Ee][+-]?{D}+
FS (f|F|l|L)
IS (u|U|l|L)*
INCLUDE #(include).*\n
COMMENT (\/\/).*\n|(\/\*(.|[\r\n])*?\*\/)

%%
{COMMENT} {}
#define {return DEFINE;}
"char"  {return CHAR;}
"short" {return SHORT;}
"int" {return INT;}
"long" {return LONG;}
"signed" {return SIGNED;}
"unsigned" {return UNSIGNED;}
"float" {return FLOAT;}
"double" {return DOUBLE;}
"const" {return CONST;}
"volatile" {return VOLATILE;}
"void" {return VOID;}

"auto"  {return AUTO;}

"enum" {return ENUM;}
"register" {return REGISTER;}
"static" {return STATIC;}

"break" {return BREAK;}
"case"  {return CASE;}
"continue" {return CONTINUE;}
"default" {return DEFAULT;}
"do"  {return DO;}
"else" {return ELSE;}
"extern" {return EXTERN;}
"for" {return FOR;}
"goto" {return GOTO;}
"if" {return IF;}
"return" {return RETURN;}
"sizeof" {return SIZEOF;}

"struct" {return STRUCT;}
"switch" {return SWITCH;}
"typedef" {return TYPEDEF;}
"union" {return UNION;}
"while" {return WHILE;}

{L}({L}|{D})* {
  // 식별자를 발견했을 때, 우선 토큰으로 잡은 이름을 identifier_name에 저장한다.
  // strdup() 함수는 yytext가 가리키는 문자열을 다른 별도의 메모리 공간으로 복사한다
  // yytext에는 매번 현재 읽은 토큰의 값이 들어가므로, 그 포인터 주소를 바로 저장할 수 없기 때문이다.
  identifier_name = strdup(yytext);
  
  #if DEBUG
    printf("identifier: %s\n\n", identifier_name);
  #endif

  // type name symbol table에 현재 읽은 식별자가 있는지 확인
  for (int i = 0; i < type_name_array_size; i++) {
    if (strcmp(identifier_name, type_name_array[i]) == 0) {
      #if DEBUG
        printf("type name : %s\n\n", identifier_name);
      #endif
      return TYPE_NAME;
    }
  }

  // 식별자가 현재 #define constant 심볼 테이블에 있는지 확인
  for (int i = 0; i < define_name_array_size; i++) {
    if (strcmp(identifier_name, define_name_array[i]) == 0) {
      #if DEBUG
        printf("define name : %s\n\n", identifier_name);
      #endif
      return CONSTANT;
    }
  }

  // 식별자가 현재 #define 문자열 리터럴 심볼 테이블에 있는지 확인
  for (int i = 0; i < str_define_name_array_size; i++) {
    if (strcmp(identifier_name, str_define_name_array[i]) == 0) {
      #if DEBUG
        printf("define name : %s\n\n", identifier_name);
      #endif
      return STRING_LITERAL;
    }
  }

  return IDENTIFIER;
}
0[xX]{H}+{IS}? {return CONSTANT;}
0{D}+{IS}? {return CONSTANT;}
{D}+{IS}? {return CONSTANT;}
L?'(\\.|[^\\'])+' {return CONSTANT;}
{D}+{E}{FS}? {return CONSTANT;}
{D}*"."{D}+({E})?{FS}? {return CONSTANT;} 
{D}+"."{D}*({E})?{FS}? {return CONSTANT;} 

L?\"(\\.|[^\\"])*\" {return STRING_LITERAL;}

"..." {return ELLIPSIS;}

">>=" {return RIGHT_ASSIGN;}
"<<=" {return LEFT_ASSIGN;}
"+="  {return ADD_ASSIGN;}
"-="  {return SUB_ASSIGN;}
"*="  {return MUL_ASSIGN;}
"/="  {return DIV_ASSIGN;}
"%="  {return MOD_ASSIGN;}
"&="  {return AND_ASSIGN;}
"^="  {return XOR_ASSIGN;}
"|="  {return OR_ASSIGN;}

">>" {return RIGHT_OP;}
"<<" {return LEFT_OP;}
"++" {return INC_OP;}
"--" {return DEC_OP;}
"->" {return PTR_OP;}
"&&" {return AND_OP;}
"||" {return OR_OP;}
"<=" {return LE_OP;}
">=" {return GE_OP;}
"==" {return EQ_OP;}
"!=" {return NE_OP;}

"("   {return '(';}
")"   {return ')';}
";"   {return ';';}
":"   {return ':';}
","   {return ',';}
"="   {return '=';}
"."   {return '.';}
"&"   {return '&';}
"!"   {return '!';}
"~"   {return '~';}
"-"   {return '-';}
"+"   {return '+';}
"*"   {return '*';}
"/"   {return '/';}
"%"   {return '%';}
"<"   {return '<';}
">"   {return '>';}
"^"   {return '^';}
"|"   {return '|';}
"?"   {return '?';}
("{"|"<%") {return '{';}
("}"|"%>") {return '}';}
("["|"<:") {return '[';}
("]"|":>") {return ']';}

{INCLUDE} {; }
[\t\v\n\f .]  {;}
. {}

%%

int yywrap(void) {
  return 1;
}