%{
#include <stdio.h>
// #define YYDEBUG 1
int ary[9] = {0,0,0,0,0,0,0,0,0};

// ary[] 배열에 가리킬 인덱스를 알기 쉽도록 정의
#define FUNC_CNT 0
#define OP_CNT 1
#define INT_CNT 2
#define CHAR_CNT 3
#define POINTER_CNT 4
#define ARRAY_CNT 5
#define SELECTION_CNT 6
#define LOOP_CNT 7
#define RETURN_CNT 8

// 반복문에서 else, else if 를 카운트 하지 않기 위해 사용하는 체크 변수
int isSelection = 0;

// int, char 타입 체크용도, int = 0, char = 1, 이외 타입에는 -1 을 저장
int nowType = -1;
int initVarCount = 0; // int a, b 와 같이 한번에 여러 변수를 생성할 때의 변수의 개수

int isPointer = 0; // pointer 여부 체크
int isArray = 0;   // 배열 여부 체크

int isFunctionDeclaration = 0; // 함수 전방 선언 체크
int intParameterCount = 0;     // int 파라미터 개수 체크
int charParameterCount = 0;    // char 파라미터 개수 체크
int pointerParameterCount = 0; // pointer 파라미터 개수 체크

// typedef
char* identifier_name;         // lex에서 읽은 IDENTIFIER의 실제 이름
int isTypeDef = 0;             // typedef 여부 체크

char* type_name_array[1000];   // type name 심볼 테이블 역할 배열
int type_name_array_size = 0;

char* define_name_array[1000]; // #define 상수 심볼 테이블 역할 배열
int define_name_array_size = 0;

char* str_define_name_array[1000]; // #define 문자열 리터럴 심볼 테이블 역할 배열
int str_define_name_array_size = 0;

int isPointerFunction = 0; // 함수 포인터 가능성 여부 체크

int yylex();
%}

%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF DEFINE
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%start translation_unit

%%
translation_unit
  : external_declaration
  | translation_unit external_declaration
  ; // OK

external_declaration
  : function_definition {
      #if YYDEBUG
        printf("\n함수 정의부 카운트 \n\n");
      #endif

      ary[FUNC_CNT]++;

      #if YYDEBUG
        printf("int형 파라미터 %d 개\n", intParameterCount);
      #endif
      ary[INT_CNT] += intParameterCount;
      intParameterCount = 0;

      #if YYDEBUG
        printf("char형 파라미터 %d 개\n", charParameterCount);
      #endif
      ary[CHAR_CNT] += charParameterCount;
      charParameterCount = 0;

      #if YYDEBUG
        printf("pointer형 파라미터 %d 개\n\n", pointerParameterCount);
      #endif
      ary[POINTER_CNT] += pointerParameterCount;
      pointerParameterCount = 0;
    }
  | declaration {
      if (isFunctionDeclaration == 1) { // 함수 전방 선언은 무시한다.
        #if YYDEBUG
          printf("\n함수의 전방 선언은 무시합니다.\n");
        #endif
        
        intParameterCount = 0;
        charParameterCount = 0;
        pointerParameterCount = 0;
      } else {
        if (isPointer) {
          ary[POINTER_CNT] += initVarCount;          
        }
      }

      initVarCount = 0;
      isPointer = 0;
      isFunctionDeclaration = 0;
    }
  | DEFINE IDENTIFIER CONSTANT { // "#define 식별자 상수" 처리
    #if YYDEBUG
      printf("add define : %s\n\n", identifier_name);
    #endif
    define_name_array[define_name_array_size++] = identifier_name;
  }
  | DEFINE IDENTIFIER STRING_LITERAL { // "#define 식별자 문자열" 처리
    #if YYDEBUG
      printf("add str define : %s\n\n", identifier_name);
    #endif
    str_define_name_array[str_define_name_array_size++] = identifier_name;
  }
  ; // OK

function_definition
  : declaration_specifiers declarator declaration_list compound_statement
  | declaration_specifiers declarator compound_statement
  | declarator declaration_list compound_statement
  | declarator compound_statement
  ; // OK

declaration
  : declaration_specifiers ';'
  | declaration_specifiers init_declarator_list ';' {
      if (isTypeDef) { // typedef 키워드가 등장했었다면, 새로운 타입에 대한 선언이다.
        #if YYDEBUG
          printf("add type : %s\n\n", identifier_name);
        #endif
        type_name_array[type_name_array_size] = identifier_name;
        type_name_array_size++;
        isTypeDef = 0;
      } else { // 아니라면 변수/함수에 대한 선언이다.
        if (isPointer) {
          ary[POINTER_CNT] += initVarCount;
        }
        if (nowType == 0) {
          /* int 변수 카운트 */
          #if YYDEBUG
            printf("\nint 변수 선언: %d개\n\n", initVarCount);
          #endif
          ary[INT_CNT] += initVarCount;

        } else if (nowType == 1) {
          /* char 변수 카운트 */
          #if YYDEBUG
            printf("\nchar 변수 선언: %d개\n\n", initVarCount);
          #endif
          ary[CHAR_CNT] += initVarCount;
        }
      }

      initVarCount = 0;
      isPointer = 0;
      nowType = -1;
      isTypeDef = 0;
    }
  ;

init_declarator_list
  : init_declarator                           { initVarCount++; }
  | init_declarator_list ',' init_declarator  { initVarCount++; }
  ;

init_declarator
  : declarator
  | declarator '=' initializer { ary[OP_CNT]++; }
  ;

initializer
  : assignment_expression
  | '{' initializer_list '}'
  | '{' init_declarator_list ',' '}'
  ;

initializer_list
  : initializer
  | initializer_list ',' initializer

// IDENTIFIER //
identifier_list
  : IDENTIFIER
  | identifier_list ',' IDENTIFIER
  ;

// TYPE //
type_specifier
  : VOID
  | CHAR {nowType = 1; /*char = 1*/}
  | SHORT
  | INT  {nowType = 0; /* int = 0*/}
  | LONG
  | FLOAT
  | DOUBLE
  | SIGNED
  | UNSIGNED
  | struct_or_union_specifier
  | enum_specifier
  | TYPE_NAME
  ;

type_qualifier
  : CONST
  | VOLATILE
  ;

specifier_qualifier_list
  : type_specifier specifier_qualifier_list
  | type_specifier
  | type_qualifier specifier_qualifier_list
  | type_qualifier
  ;

type_name
  : specifier_qualifier_list
  | specifier_qualifier_list abstract_delarator
  ;

pointer
  : '*' { isPointer = 1; }
  | '*' type_qualifier_list
  | '*' pointer
  | '*' type_qualifier_list pointer
  ;

type_qualifier_list
  : type_qualifier
  | type_qualifier_list type_qualifier
  ;

// STRUCT //
struct_or_union_specifier
  : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
  | struct_or_union '{' struct_declaration_list '}'
  | struct_or_union IDENTIFIER
  ;

struct_or_union
  : STRUCT { nowType = -1; } // struct 타입이 등장하였으므로 int, char 타입 체크 해제
  | UNION
  ;

struct_declaration_list
  : struct_declaration
  | struct_declaration_list struct_declaration
  ;

struct_declaration
  : specifier_qualifier_list struct_declarator_list ';' {
      if (isPointer) {
        ary[POINTER_CNT] += initVarCount;
      }
      if (nowType == 0) {
        /* int 변수 카운트 */
        #if YYDEBUG
          printf("init int: %d\n\n", initVarCount);
        #endif
        ary[INT_CNT] += initVarCount;
      } else if (nowType == 1) {
        /* char 변수 카운트 */
        #if YYDEBUG
          printf("init char: %d\n\n", initVarCount);
        #endif
        ary[CHAR_CNT] += initVarCount;
      }

      initVarCount = 0;
      isPointer = 0;
      nowType = -1;
  }
  ;

struct_declarator_list
  : struct_declarator {initVarCount++;}
  | struct_declarator_list ',' struct_declarator {initVarCount++;}
  ;

struct_declarator
  : declarator
  | ':' constant_expression
  | declarator ':' constant_expression
  ;

// OPERATOR //
unary_operator
  : '&'
  | '*'
  | '+'
  | '-'
  | '~'
  | '!'
  ;

assignment_operator
  : '='
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN
  ;


// EXPRESSION //
expression
  : assignment_expression
  | expression ',' assignment_expression
  ;

constant_expression
  : conditional_expression
  ;

assignment_expression
  : conditional_expression
  | unary_expression assignment_operator assignment_expression { ary[OP_CNT]++; }
  ;

conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' conditional_expression
  ;

cast_expression
  : unary_expression
  | '(' type_name ')' cast_expression { ary[OP_CNT]++; }
  ;

unary_expression
  : postfix_expression
  | INC_OP unary_expression { ary[OP_CNT]++; }
  | DEC_OP unary_expression { ary[OP_CNT]++; }
  | unary_operator cast_expression
  | SIZEOF unary_expression
  | SIZEOF '(' type_name ')'
  ;

postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'
  | postfix_expression '(' ')'                          { ary[FUNC_CNT]++; /*함수 호출*/ }
  | postfix_expression '(' argument_expression_list ')' { ary[FUNC_CNT]++; /*함수 호출*/ }
  | postfix_expression '.' IDENTIFIER { ary[OP_CNT]++; }
  | postfix_expression PTR_OP IDENTIFIER { ary[OP_CNT]++; }
  | postfix_expression INC_OP { ary[OP_CNT]++; }
  | postfix_expression DEC_OP { ary[OP_CNT]++; }

primary_expression
  : IDENTIFIER
  | CONSTANT
  | STRING_LITERAL
  | '(' expression ')'
  ;

argument_expression_list
  : assignment_expression
  | argument_expression_list ',' assignment_expression
  ;


// EXPRESSION WITH OPERATOR //
logical_or_expression
  : logical_and_expression
  | logical_or_expression OR_OP logical_and_expression { ary[OP_CNT]++; }
  ;

logical_and_expression
  : inclusive_or_expression
  | logical_and_expression AND_OP inclusive_or_expression { ary[OP_CNT]++; }
  ;

inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression { ary[OP_CNT]++; }
  ;

exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression { ary[OP_CNT]++; }
  ;

and_expression
  : equality_expression 
  | and_expression '&' equality_expression { ary[OP_CNT]++; }
  ;

equality_expression
  : relational_expression
  | equality_expression EQ_OP relational_expression { ary[OP_CNT]++; }
  | equality_expression NE_OP relational_expression { ary[OP_CNT]++; }
  ;

relational_expression
  : shift_expression
  | relational_expression '<' shift_expression { ary[OP_CNT]++; }
  | relational_expression '>' shift_expression { ary[OP_CNT]++; }
  | relational_expression LE_OP shift_expression { ary[OP_CNT]++; }
  | relational_expression GE_OP shift_expression { ary[OP_CNT]++; }
  ;

shift_expression
  : additive_expression
  | shift_expression LEFT_OP additive_expression { ary[OP_CNT]++; }
  | shift_expression RIGHT_OP additive_expression { ary[OP_CNT]++; }
  ;

additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression { ary[OP_CNT]++; }
  | additive_expression '-' multiplicative_expression { ary[OP_CNT]++; }
  ;

multiplicative_expression
  : cast_expression
  | multiplicative_expression '*' cast_expression { ary[OP_CNT]++; }
  | multiplicative_expression '/' cast_expression { ary[OP_CNT]++; }
  | multiplicative_expression '%' cast_expression { ary[OP_CNT]++; }
  ;


// DECLARATOR //
declarator
  : pointer direct_declarator { if (isArray) {ary[ARRAY_CNT]++; isArray = 0;} }
  | direct_declarator         { if (isArray) {ary[ARRAY_CNT]++; isArray = 0;} }
  ;

direct_declarator
  : IDENTIFIER
  | '(' declarator ')' {
    if (isPointer) {
      ary[POINTER_CNT]++;
      isPointer = 0;
      isPointerFunction = 1; // 포인터 함수 선언의 가능성이 있다.
    } /* 무조건 변수를 나타낸다. */
  }
  | direct_declarator '[' constant_expression ']' { isArray = 1; }
  | direct_declarator '[' ']' { isArray = 1; }
  | direct_declarator '(' parameter_list ')'  {
      if (isPointerFunction == 1) {
        ary[FUNC_CNT]++;
        isPointerFunction = 0;
        #if YYDEBUG
          printf("\n함수 포인터 선언\n\n");
        #endif
      } else if (isFunctionDeclaration == 0) {
        isFunctionDeclaration = 1;
      }
    }
  | direct_declarator '(' identifier_list ')' { /*isFunctionDeclaration = 1;*/ } // ANSI C 문법, 테케에 없음.
  | direct_declarator '(' ')'                 { if (isFunctionDeclaration == 0) isFunctionDeclaration = 1; }
  ;

abstract_delarator
  : pointer
  | direct_abstract_declarator          { if (isArray) {ary[ARRAY_CNT]++; isArray = 0;} }
  | pointer direct_abstract_declarator  { if (isArray) {ary[ARRAY_CNT]++; isArray = 0;} }
  ;

direct_abstract_declarator
  : '(' abstract_delarator ')'
  | '[' ']' { isArray = 1; }
  | '[' constant_expression ']' { isArray = 1; }
  | direct_abstract_declarator '[' ']'
  | direct_abstract_declarator '[' constant_expression ']'
  | '(' ')'
  | '(' parameter_type_list ')'
  | direct_abstract_declarator '(' ')'
  | direct_abstract_declarator '(' parameter_type_list ')'
  ;

// PARAMETER //
parameter_declaration
  : declaration_specifiers declarator {
      if (nowType == 0) {
        intParameterCount++;
      } else if (nowType == 1) {
        charParameterCount++;
      }

      if (isPointer == 1) {
        pointerParameterCount++;
      }

      nowType = -1; 
      isPointer = 0;
    }
  | declaration_specifiers abstract_delarator {
      #if YYDEBUG
        printf("추상");
      #endif
      if (nowType == 0) {
        intParameterCount++;
      } else if (nowType == 1) {
        charParameterCount++;
      }

      if (isPointer == 1) {
        pointerParameterCount++;
      }
      
      nowType = -1; 
      isPointer = 0;
    }   
  | declaration_specifiers  /* 파라미터 변수는 아니고 선언만 있으면 세지 않는다. */ {
    if (nowType == 0) {
      #if YYDEBUG
        printf("\nint 형 파라미터 타입만 선언\n\n");
      #endif
    } else if (nowType == 1) {
      #if YYDEBUG
        printf("\nchar 형 파라미터 타입만 선언\n\n");
      #endif
    }

    isPointer = 0;
    nowType = -1;
  }
  ;

parameter_list
  : parameter_declaration
  | parameter_list ',' parameter_declaration
  ;

parameter_type_list
  : parameter_list
  | parameter_list ',' ELLIPSIS
  ;

// DECLARATION //
declaration_list
  : declaration  
  | declaration_list declaration
  ; // OK

storage_class_specifier
  : TYPEDEF { isTypeDef = 1; }
  | EXTERN
  | STATIC
  | AUTO
  | REGISTER
  ;

declaration_specifiers
  : storage_class_specifier
  | storage_class_specifier declaration_specifiers { /* type def */}
  | type_specifier { isPointer = 0; } // 타입이 나오면 기존 포인터 체크는 변수에 쓰인 포인터가 아님
  | type_specifier declaration_specifiers
  | type_qualifier
  | type_qualifier declaration_specifiers
  ;

enum_specifier
  : ENUM '{' enumerator_list '}'
  | ENUM IDENTIFIER '{' enumerator_list '}'
  | ENUM IDENTIFIER
  ;

enumerator_list
  : enumerator
  | enumerator_list ',' enumerator
  ;

enumerator
  : IDENTIFIER
  | IDENTIFIER '=' constant_expression { ary[OP_CNT]++; }
  ;

// STATEMENT //
compound_statement
  : '{' '}'
  | '{' statement_list '}'
  /* | '{' declaration_list '}' // declaration_statement  // test
  | '{' declaration_list statement_list '}' // 여기에서 둘 사이에 순서를 없애주어야 함. */
  ; // OK

statement_list
  : statement                 { if (isSelection) {ary[SELECTION_CNT]++; isSelection = 0; } }
  | statement_list statement  { if (isSelection) {ary[SELECTION_CNT]++; isSelection = 0; } }
  ; // OK

statement
  : labeled_statement
  | compound_statement
  | declaration_or_expression_statement
  //| expression_statement { printf("\n\nend line\n\n"); }
  | selection_statement { isSelection = 1; }
  | iteration_statement { ary[LOOP_CNT]++; }
  | jump_statement
  ;

labeled_statement
  : IDENTIFIER ':' statement
  | CASE constant_expression ':' statement
  | DEFAULT ':' statement
  ; // OK

declaration_or_expression_statement
  : expression_statement
  | declaration
  ;

expression_statement
  : ';'
  | expression ';'
  ; // OK

selection_statement
  : IF '(' expression ')' statement
  | IF '(' expression ')' statement ELSE statement
  | SWITCH '(' expression ')' statement
  ;

iteration_statement
  : WHILE '(' expression ')' statement
  | DO statement WHILE '(' expression ')' ';'
  //| FOR '(' expression_statement expression_statement ')' statement
  //| FOR '(' expression_statement expression_statement expression ')' statement
  | FOR '(' declaration_or_expression_statement expression_statement ')' statement
  | FOR '(' declaration_or_expression_statement expression_statement expression ')' statement
  ;

jump_statement
  : GOTO IDENTIFIER ';'
  | CONTINUE ';'
  | BREAK ';'
  | RETURN ';'            { ary[RETURN_CNT]++; }
  | RETURN expression ';' { ary[RETURN_CNT]++; }
  ; // OK

%%

int main(void)
{
  #if YYDEBUG
    yydebug = 1;
  #endif
	yyparse();
	printf("function = %d\n", ary[0]);
	printf("operator = %d\n", ary[1]);
	printf("int = %d\n", ary[2]);
	printf("char = %d\n", ary[3]);
	printf("pointer = %d\n", ary[4]);
	printf("array = %d\n", ary[5]);
	printf("selection = %d\n", ary[6]);
	printf("loop = %d\n", ary[7]);
	printf("return = %d\n", ary[8]);
	return 0;
}

void yyerror(const char *str)
{
	fprintf(stderr, "error: %s\n", str);
}
