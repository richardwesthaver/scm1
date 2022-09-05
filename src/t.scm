;;; t.scm --- scm1 test suite
(: t (f (a r) (? r (ok a) (er a))))
(: ok (f (a) (m a "success!")))
(: er (f (a) (m a "failed!")))
(: tr (= 0 0))
(: fa (= 0 1))
(: ln (, 1 2 3.14 -5))
(: ls (, "hello" "world"))
(t "eq" (= 0 0))
(t "no" (! 1 0))
(t "gt" (> 1 -1))
(t "lt" (< -1.5 1.32))
(t "ad" (= (+ 1 2) (+ -2 5)))
(t "su" (= (- 1) -1))
(t "mu" (= (* 2 2) 4))
(t "di" (= (/ 3 1.5) 2))
(t "po" (= (^ 2 8) 256))
(t "mo" (= (% 5 3) 2))