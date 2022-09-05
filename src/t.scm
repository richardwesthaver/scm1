;;; t.scm --- scm1 test suite
(: ok (f (a) (m a "success!")))
(: er (f (a) (m a "failed!")))
(: t (f (a r) (? r (ok a))))
(t "eq" (= 0 0))
(t "no" (! 1 0))
(t "gt" (> 1 -1))
(t "lt" (< -1.5 1.32))
(t "ad" (= (+ 1 2) (+ -2 5)))
