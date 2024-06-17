; n queen
; 1행에서 1, 2, 3, 4 에 배치해보고
; 2행에서도 각각에 대해 1, 2, 3, 4에 배치해보고
; 3행에서도 각각에 대해 1, 2, 3, 4에 배치해보고
; 4행에서도 각각에 대해 1, 2, 3, 4에 배치해보고
; 각 배치할 때마다 상하좌우 대각선 체크 
(defun check(line pos pos_list)
  ; 가로줄에는 1개씩 배치하니까 체크할 필요가 없다.
  
  ; 세로줄에 겹치는 부분이 있는지 확인
  (loop for check_line from 1 to 4 do
    (when (eql check_line line)
      (return )
    )

    (loop for check_list in pos_list do
      (when (equal (list check_line pos) check_list) 
        (return-from check nil)
      )
    )
  )

  ; 우하 대각선에 겹치는 부분이 있는지 확인
  (loop for i from 1 to 4 do
    (setq check_line (+ i line))
    (setq check_pos  (+ i pos))
    
    (when (or (> check_line 4) (> check_pos 4))
      (return )
    )

    (loop for check_list in pos_list do
      ;; (print (equal (list check_line check_pos) check_list) )
      (when (equal (list check_line check_pos) check_list) 
        (return-from check nil)
      )
    )
  )

  ; 좌상 대각선에 겹치는 부분이 있는지 확인
  (loop for i from 1 to 4 do
    (setq check_line (- line i))
    (setq check_pos  (- pos i))
    
    (when (or (< check_line 1) (< check_pos 1))
      (return )
    )

    (loop for check_list in pos_list do
      (when (equal (list check_line check_pos) check_list) 
        (return-from check nil)
      )
    )
  )

  ; 좌하 대각선에 겹치는 부분이 있는지 확인
  (loop for i from 1 to 4 do
    (setq check_line (+ i line))
    (setq check_pos  (- pos i))
    
    (when (or (> check_line 4) (< check_pos 1))
      (return )
    )

    (loop for check_list in pos_list do
      (when (equal (list check_line check_pos) check_list) 
        (return-from check nil)
      )
    )
  )

  ; 우상 대각선에 겹치는 부분이 있는지 확인
  (loop for i from 1 to 4 do
    (setq check_line (- line i))
    (setq check_pos  (+ i pos))
    
    (when (or (< check_line 1) (> check_pos 4))
      (return )
    )

    (loop for check_list in pos_list do
      (when (equal (list check_line check_pos) check_list) 
        (return-from check nil)
      )
    )
  )

  (return-from check T)
)

(defun solve(line pos_list)
  (when (eql line 4) ; line == 4 이면
    (print pos_list)
    (return-from solve)
  )

  ; line < 4 이면,
  (loop for i from 1 to 4 do ; 다음 라인에 1, 2, 3, 4를 모두 배치해본다.
    (when (check (+ line 1) i pos_list) ; 다음 line에 i 위치가 퀸을 배치할 수 있는 위치라면
      (setq new_pos_list (append pos_list (list (list (+ line 1) i)) ) ) ; 퀸을 배치하고 재귀 호출
      (solve (+ line 1) new_pos_list)
    )
  )
)

(loop for i from 1 to 4 do
  (solve 1 (list (list 1 i)))
)
