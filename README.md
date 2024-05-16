# Programming-Language-HW
프로그래밍언어론 과제 레포

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

## 디버깅 방법


