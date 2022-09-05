;;; 0.scm --- scm1 core lib
(: car (f (a) ([ a))) ; alias
(: cdr (f (a) (] a))) ; alias
(: cadr (f (a) ([ (] a))))
(: cdar (f (a) (] ([ a))))
(: caar (f (a) ([ ([ a))))
(: cddr (f (a) (] (] a))))
(: caadr (f (a) ([ ([ (] a)))))
(: cadar (f (a) ([ (] ([ a)))))
(: cdaar (f (a) (] ([ ([ a)))))
(: cddar (f (a) (] (] ([ a)))))
(: cdadr (f (a) (] ([ (] a)))))
(: caddr (f (a) ([ (] (] a)))))
(: caaar (f (a) ([ ([ ([ a)))))
(: cdddr (f (a) (] (] (] a)))))	    
