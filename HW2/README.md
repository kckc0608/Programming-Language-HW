# HW2 - YACC
## 과제 진행
### vscode 익스텐션 설치
vscode에 `SSH FS` 익스텐션을 추가합니다. (remote-ssh 익스텐션은 학교 서버 접속이 안됐습니다.)   

lex/yacc 문법 하이라이팅을 위해서 `Yash` 익스텐션을 추가합니다.

### 컴파일 스크립트 작성
```shell
lex hw3.l
yacc -d -v hw3.y
cc y.tab.c lex.yy.c -o yacc
./yacc < test3.c > yacc_output
```
yacc 파서 프로그램을 컴파일 용도의 스크립트를 작성합니다.   
학교 서버에서는 실행 권한이 없어 스크립트 실행은 못하지만, 작성한 스크립트는 복사해서 쉘 입력창에 그대로 붙여넣으면 4개 명령어를 한번에 실행할 수 있습니다.   


### 테스트 스크립트 사용 방법
위 스크립트대로 컴파일을 한다면, `yacc` 라는 이름의 실행파일이 만들어집니다.   
그 실행파일이 있는 위치에 레포지토리에 보이는 `test` 폴더를 붙여넣고, `yacc` 실행 파일이 있는 위치에서 `run_test.sh`를 실행하면 됩니다.   
![image](https://github.com/kckc0608/Programming-Language-HW/assets/64959010/6b5b621c-a415-4b3f-9037-1c478ae04e15)   
35개 테스트케이스가 모두 통과했다면 이렇게 보입니다.


## 디버깅 방법
`yacc -v` 를 실행하면 `y.output` 파일이 보입니다.   
```
   0  $accept : translation_unit $end

   1  translation_unit : external_declaration
   2                   | translation_unit external_declaration

   3  external_declaration : function_definition
   4                       | declaration
   5                       | DEFINE IDENTIFIER CONSTANT
   6                       | DEFINE IDENTIFIER STRING_LITERAL

   7  function_definition : declaration_specifiers declarator declaration_list compound_statement
   8                      | declaration_specifiers declarator compound_statement
   9                      | declarator declaration_list compound_statement
  10                      | declarator compound_statement

  11  declaration : declaration_specifiers ';'
  12              | declaration_specifiers init_declarator_list ';'
```
파일 상단부에는 이렇게 BNF 규칙이 적혀있습니다. 왼쪽에 있는 숫자는 rule number 입니다.   
룰 이후에는 아래와 같은 state 정보가 나타납니다.

```
state 0
	$accept : . translation_unit $end  (0)

	IDENTIFIER  shift 1
	DEFINE  shift 2
	TYPE_NAME  shift 3
	TYPEDEF  shift 4
	EXTERN  shift 5
	STATIC  shift 6
	AUTO  shift 7
```

state 0 은 시작 상태입니다. `translation_unit` 왼쪽에 있는 `.`이 현재 상태에서 체크하고 있는 위치를 의미합니다.   
이 위치에 `IDENTIFIER` 토큰이 들어오면 `state 1` 로 shift 하고 `DEFINE` 토큰이 들어오면 `state 2` 로 shift 하고... 와 같은 의미를 나타냅니다.

```
state 2
	external_declaration : DEFINE . IDENTIFIER CONSTANT  (5)
	external_declaration : DEFINE . IDENTIFIER STRING_LITERAL  (6)

	IDENTIFIER  shift 39
	.  error
```
state 2 는 위와 같이 정의 됩니다.   
만약 현재 보고 있는 위치 (`.`) 에 IDENTIFIER 가 들어오면 문법상 올바르므로 `state 39` 로 이동하고, 그 외의 모든 토큰에 대해서는 error 를 발생시킵니다. (`. error` 부분)   
이 정보는 지금 상태로는 크게 도움이 되지 않지만, 나중에 디버깅을 할 때 이 정보를 토대로 어떤 BNF 문법에서 문제가 발생했는지 확인할 수 있습니다.   

```c
int main(void)
{
  #if YYDEBUG  // #define YYDEBUG 1 으로 상단 정의부에서 정의해두면 실행된다.
    yydebug = 1;
  #endif

  ...
}
```
yacc 파일의 마지막 함수 정의 부분의 main 함수에서 yydebug 값을 1로 설정해두면 파서가 실행되었을 때, 그 중간 state의 천이 과정을 보여줍니다.   
### 예시
```c
int x;
```
다음과 같은 c코드를 넣고 yacc 프로그램을 실행하면 아래와 같이 디버그 정보가 나옵니다.
```
yydebug: state 0, reading 291 (INT)
yydebug: state 0, shifting to state 11
yydebug: state 11, reducing by rule 27 (type_specifier : INT)
yydebug: after reduction, shifting from state 0 to state 31
yydebug: state 31, reading 257 (IDENTIFIER)
yydebug: state 31, reducing by rule 174 (declaration_specifiers : type_specifier)
yydebug: after reduction, shifting from state 0 to state 29
yydebug: state 29, shifting to state 1
....
```

state 0 일 때 INT 라는 토큰을 만나서 `state 11`로 이동하고, 그 state에서 rule number (위에서 설명하였습니다.) 27 에 의해 `type_specifier` 로 `INT` 토큰을 reduce 합니다.

```
yydebug: state 1, reducing by rule 139 (direct_declarator : IDENTIFIER)
yydebug: after reduction, shifting from state 29 to state 37
yydebug: state 37, reading 0 (end-of-file)
yydebug: error recovery discarding state 37
yydebug: error recovery discarding state 29
yydebug: error recovery discarding state 0
```
만약 syntax error 가 발생했다면 위와 같이 보입니다.    
IDENTIFIER 토큰을 읽어서 `state 37` 로 이동했는데, 그 state 에서 (end-of-file) 정보를 읽었다고 합니다.

```
state 37
	declarator : direct_declarator .  (138)
	direct_declarator : direct_declarator . '[' constant_expression ']'  (141)
	direct_declarator : direct_declarator . '[' ']'  (142)
	direct_declarator : direct_declarator . '(' parameter_list ')'  (143)
	direct_declarator : direct_declarator . '(' identifier_list ')'  (144)
	direct_declarator : direct_declarator . '(' ')'  (145)

	'('  shift 61
	'['  shift 62
	TYPE_NAME  reduce 138
	TYPEDEF  reduce 138
	EXTERN  reduce 138
	STATIC  reduce 138
	AUTO  reduce 138
	REGISTER  reduce 138
	CHAR  reduce 138
	SHORT  reduce 138
	INT  reduce 138
	LONG  reduce 138
	SIGNED  reduce 138
	UNSIGNED  reduce 138
	FLOAT  reduce 138
	DOUBLE  reduce 138
	CONST  reduce 138
	VOLATILE  reduce 138
	VOID  reduce 138
	STRUCT  reduce 138
	UNION  reduce 138
	ENUM  reduce 138
	';'  reduce 138
	','  reduce 138
	'='  reduce 138
	'{'  reduce 138
	':'  reduce 138
	')'  reduce 138
```

state 37 의 규칙은 위와 같습니다.
여기에서는 '.' 위치에 (end-of-file) 이 들어갈 수 있는 규칙이 없으니 syntax error 가 발생합니다.
