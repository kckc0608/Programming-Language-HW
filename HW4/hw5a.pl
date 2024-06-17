% insertion sort
swap(Input, CheckIndex, KeyIndex, Result):-
  % CheckIndex < KeyIndex 가 전제 되어야 nth0/4 로 삽입, 추출이 꼬이지 않음.
  nth0(KeyIndex, Input, KeyValue, RestInput),           % KeyIndex 위치에 있는 값을 KeyValue에 저장하고, Input에서 그 값을 뽑아낸 나머지 배열을 RestInput에 저장
  nth0(CheckIndex, RestInput, CheckValue, RestInput2),  % RestInput에서 CheckIndex 위치에 있는 값을 CheckValue에 저장하고, 그 값을 뽑아낸 나머지 배열을 RestInput2에 저장
  nth0(CheckIndex, SwapResult1, KeyValue, RestInput2),  % RestInput2에 KeyValue를 CheckIndex 위치에 삽입하고, 그 결과를 SwapResult1 에 저장
  nth0(KeyIndex, Result, CheckValue, SwapResult1).      % SwapResult1에 CheckValue를 KeyIndex 위치에 삽입하고, 그 결과를 Result 에 저장

insert(Input, NowIndex, KeyIndex, Result):- % Input 은 KeyIndex 이전까지는 정렬된 배열, NowIndex 위치에 KeyIndex를 삽입할 수 있는지 확인한다. 초기 호출 시 NowIndex 값은 KeyIndex - 1 이다.
  (NowIndex < 0 -> ( % NowIndex 가 0보다 작다면 key index 이전의 모든 배열을 순회한 것이므로 반복(재귀)를 종료한다.
      Result = Input % 그리고 삽입한 최종 결과 배열을 Result에 저장한다.
    ) ; (  % NowIndex가 0보다 크다면
      nth0(KeyIndex, Input, KeyValue), % KeyValue = Input[KeyIndex]
      nth0(NowIndex, Input, NowValue), % NowValue = Input[NowIndex]
      (KeyValue < NowValue -> (
          swap(Input, NowIndex, KeyIndex, SwapResult), % KeyValue < NowValue 이면 두 값을 스왑하고
          NextKeyIndex is KeyIndex - 1                 % KeyValue가 앞으로 갔으니 NextKeyIndex에 KeyIndex - 1 대입
        ) ; (
          SwapResult = Input,       % 만약 스왑을 하지 않았더라도, SwapResult 변수에 Input을 그대로 넣기
          NextKeyIndex is KeyIndex  % NextKeyIndex 에는 KeyIndex 그대로 유지
        )
      ),
      NextIndex is NowIndex - 1, % 다음 반복에서 Key와 비교할 Index 값.
      insert(SwapResult, NextIndex, NextKeyIndex, Result) % 다음 반복 진행
    )
  ).

solve(Input, KeyIndex, Result):-
  length(Input, InputSize),   % Input 배열의 길이 계산, 이 길이까지만 key 인덱스를 증가시키기 위함.
  ( KeyIndex =:= InputSize -> (   % Input 배열의 길이와 key 인덱스가 같다면
      var(Result),                %   Result 변수가 할당되지 않았는지 체크하고
      Result = Input              %   할당되지 않았다면 Input 에는 정렬된 배열이 들어있으니 Result에 할당
    ) ; (                         % Input 배열의 길이 > key 라면
      InsertStartIndex is KeyIndex - 1, % key index 직전 인덱스부터, 그 값을 비교한다.
      insert(Input, InsertStartIndex, KeyIndex, SwapResult), % 현재 key값을 자신 앞에 있는 배열에 정렬되도록 끼워넣는다.

      write(KeyIndex),  % 현재 몇번째 키를 보고 있었는지 출력
      write(' '),
      writeln(SwapResult), % 현재 단계에서 정렬된 상태 출력

      NewKeyIndex is KeyIndex + 1, % 다음 키에 대해 반복하기 위해 key index 증가
      solve(SwapResult, NewKeyIndex, Result) % 다음 키에 대해서 sovle 반복 수행
    )
  ).

sorting(Input, Result):-
  solve(Input, 0, Result), % 0은 시작할 초기 key index
  nl.