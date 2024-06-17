% X 조합으로 퀸을 배치했을 때, Line에서 Pos가 0부터 Line까지에 대해 퀸을 배치할 수 있는지 확인
check_line(Line, Pos, []):- true.
check_line(Line, Pos, NowResult):-
  % 한 행에는 하나의 퀸만 배치하므로, 행은 체크할 필요가 없다.
  % 열이 겹치는지 체크할 때는 Pos가 NowResult에 들어있는지 확인한다.
  \+ member(Pos, NowResult),
  % 대각선에 다른 퀸이 있는지 체크한다.
  check_diagonals(Line, Pos, 1, NowResult).

check_diagonals(_, _, _, []).  % 빈 리스트이면 대각선에 다른 퀸이 없음
check_diagonals(Line, Pos, CheckLine, [R | Rs]) :-
    DiagonalOffset is abs(Line - CheckLine),        % 현재 배치한 퀸과, 현재 배치하려고 하는 위치의 대각선의 차이 계산
    DiagonalOffset =\= abs(Pos - R),                % 가로줄 간격의 절댓값과, 세로줄 간격의 절댓값이 달라야 배치할 수 있다.
    NextCheckLine is CheckLine + 1,
    check_diagonals(Line, Pos, NextCheckLine, Rs).  % 나머지 퀸들에 대해 재귀적으로 확인

solve(N, Line, Pos, Result, FinalResult):-
  ( N < Line -> (                                         % N < Line 이면, 모든 행에 퀸을 다 배치한 것이므로,
      var(FinalResult) ->                                 % FinalResult에 결과가 저장되어 있지 않다면 
        FinalResult = Result                              % FinalResult에 결과를 저장한다. (따라서 여러 경우의 수가 존재해도 1가지만 저장한다.)
      ;
        !                                                 % FinalResult에 이미 저장되어 있으면 무시한다.
    ) ; (
      Pos =< N -> (                                       % 만약 현재 배치하려고 하는 열이 N 이하라면 배치할 수 있으니
        check_line(Line, Pos, Result) ->                  % 이 행,열에 퀸을 새로 배치할 수 있는지 체크한다.
          NextLine is Line + 1,                           % 
          append(Result, [Pos], NewResult),               % 배치할 수 있으면, 배치 결과 리스트에 현재 행,열을 추가      
          solve(N, NextLine, 1, NewResult, FinalResult)   % 다음 행에 대해서 solve 진행
        ;
          true                                            % 이 위치에 배치할 수 없으면 무시. (재귀 종료)
        ),
        NextPos is Pos + 1,
        solve(N, Line, NextPos, Result, FinalResult)      % 현재 Pos에 대해 체크했으니 다음 Pos에 대해 체크하기
      ;                                                   % Pos > N 이면 범위를 벗어났으므로
        !                                                 % 재귀 종료
    )
  ).

% N-Queen
n_queen(N, Result):- % Result에는 실행 결과로 퀸을 배치하는 방법 리스트가 출력
  solve(N, 1, 1, [], Result),
  nl.
