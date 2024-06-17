(defun solve(key now_list)
  (when (>= key (length now_list))
    (princ (list "최종 결과"))
    (princ now_list)
    (print "")
    (return-from solve)
  )

  (when (eql key 1)
    (princ (list "시작 리스트"))
    (princ now_list)
    (print "")
  )

  (when (/= key 1)
    (princ (list (- key 1) " 단계"))
    (princ now_list)
    (print "")
  )
  
  (setq new_list (list ))
  (setq finish 0)
  (loop for index from 0 to key do
    (when (< (nth index now_list) (nth key now_list)) ; key 앞에 있는 원소와 key를 비교해서 key가 더 크면 앞의 원소를 그대로 넣는다.
      (setq new_list (append new_list (list (nth index now_list)) ))
    )
    (when (>= (nth index now_list) (nth key now_list)) ; key 이전에 있는 원소와 key를 비교했을 때 key가 크거나 같으면 멈춘다.
      (setq new_list (append new_list (list (nth key now_list)) )) ; 키를 먼저 넣고
      (setq finish index)
      (when (< index key)
        (setq new_list (append new_list (list (nth index now_list)) )) ; 다음에 더 큰 값을 넣는다.
        (setq finish (+ 1 index))
      )
      (return )
    )
  )

  (loop for index from finish to (- (length now_list) 1) do
    (when (/= index key)
      (setq new_list (append new_list (list (nth index now_list)) )) ; 리스트 뒤에 있는 값들을 넣는다.
    )
  )

  (solve (+ key 1) new_list)
)

(print "TC 1")
(print "")
(solve 1 (list 11 33 23 45 13 25 8 135))
(print "TC 2")
(print "")
(solve 1 (list 83 72 65 54 47 33 29 11))