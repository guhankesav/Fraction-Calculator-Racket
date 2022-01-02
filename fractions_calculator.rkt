#lang racket/base
(require racket/list)
(require racket/format)
(require dyoo-while-loop)
#(require br/verbose-app)

; GCD FUNCTION TO REDUCE FRACTION TO LOWEST FORM
(define (gcd a b )
  (if ( = b 0)
      a
      (gcd b (remainder a b))))

#|========== PROPER/IMPROPER FRACTIONS ==========|#

; TO CREATE PROPER/IMPROPER FRACTION
(define ( proper_improper numer denom )
  (cons (/ numer (gcd numer denom))  ( / denom (gcd numer denom)))
)

; SELECTORS TO RETURN NUM AND DENOM SEPARATELY
(define (numer x)
  (car x )
)

(define (denom x)
  (cdr x)
)

; PRINT PROPER/IMPROPER FRACTION
(define (print-fraction x)
  ;(newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)
#|=====================================|#

#|========== MIXED FRACTIONS ==========|#

; 
(define ( make-mixed-fraction whole-part numer denom )
  (cons whole-part (cons numer denom))
)

;SELECTORS TO RETURN NUM, DENOM AND WHOLEPART SEPARATELY
(define (numer-mixed x)     
  (cadr x)
)

(define (denom-mixed x)
  (cddr x)
)

(define (wholepart-mixed x)
  (car x)
)

; PRINT MIXED FRACTION
(define (print-mixedfraction x)
  ;(newline)
  (display (wholepart-mixed x))
  (display " ")
  (display (numer-mixed x))
  (display "/")
  (display (denom-mixed x))
)
  

#|====================================|#

#|====================================|#
;COMPLEX FRACTIONS

(define ( make-complex-fraction numer denom )
  (cons numer denom)
 )

(define (numer-complex-fraction x)
  (car x)
 )

(define (denom-complex-fraction x)
  (cdr x)
 )

;DISPLAY COMPLEX FRACTION
(define ( print-complexfraction x)
  (newline)
  (display "Complex fraction: ")
  (if (is_mixed_frac (numer-complex-fraction x))
      (print-mixedfraction (numer-complex-fraction x))
      (print-fraction (numer-complex-fraction x))
  )
  (display " / ")
  (if (is_mixed_frac (denom-complex-fraction x))
      (print-mixedfraction (denom-complex-fraction x))
      (print-fraction (denom-complex-fraction x))
  )
  
)



; Check if complex fraction - returns true if complex fraction else no
(define (is_complex_frac x)
  (if(pair? (car x))
     #t
     #f
   )
)



; X - FRACTION(Proper-Improper or Mixed)
; P - power to be raised -> can be integral or fractional
(define (power-of-fraction x p)
  (newline)
  (display "Power fraction: ")

  ; if the denominator of the power is 1 i.e the power is integral
  (if( = (denom p) 1)
     ; True:  make a list containing the fraction x p times
     ;        multiply contents of the list to get the power fraction
     (print-fraction (mul-list (convert-list-pro-impro(make-list (numer p) x))))
     
     ; False:  power to be raised is a fraction of form m/n
     ;         so (a/b)^(m/n) ->  nthroot(a^m)/nthroot(b^m)
     (print-fraction
      (divide
       (proper_improper (string->number (~r (*(string->number  (~r (expt (numer (mul-list (convert-list-pro-impro(make-list (numer p) x)))) (/ 1 (denom p))) #:precision 2) )100) #:precision 0)) 100)
       (proper_improper (string->number (~r (*(string->number  (~r (expt (denom (mul-list (convert-list-pro-impro(make-list (numer p) x)))) (/ 1 (denom p))) #:precision 2) )100) #:precision 0)) 100)
      )
     )  
  )
)



;FUNCTIONS

; CHECK IF THE VALUE IS MIXED FRACTION OR NOT
(define (is_mixed_frac x)
  (if(pair? (cdr x))
     #t
     #f
   )
)

; CONVERTING MIXED TO IMPROPER FRACTION
(define ( convert-mixed-to-frac x )

  (proper_improper (+ (* (wholepart-mixed x) (denom-mixed x)) (numer-mixed x)) (denom-mixed x))
)

; CONVERTING IMPROPER TO MIXED FRACTION
(define ( convert-frac-to-mixed x)

  (make-mixed-fraction (quotient (numer x) (denom x)) (remainder (numer x) (denom x)) (denom x)) 
)

;EQUALITY BETWEEN TWO FRACTIONS
(define (equality x y)
    (and (= (numer x) (numer y) ) (= (denom x) (denom y)) )
)

;INVERT A FRACTION
(define (invert x)
  (proper_improper (denom x) (numer x))
  (print-fraction (proper_improper (denom x) (numer x)))
)



;TO ADD TWO FRACTIONS
(define (addition x y )
    ( proper_improper ( + (*(numer x)(denom y)) (*(numer y)(denom x))) (*(denom x)(denom y)) )
)

;TO MULTIPLY TWO FRACTIONS
(define (multiplication x y )
  ( proper_improper (*(numer x)(numer y)) (*(denom x)(denom y)) )
)

;TO SUBTRACT TWO FRACTIONS
(define (subtraction x y ) 
   ( proper_improper ( - (*(numer x)(denom y)) (*(numer y)(denom x))) (*(denom x)(denom y)) )
)


;TO DIVIDE TWO FRACTIONS
(define (divide x y )
   ( proper_improper (*(numer x)(denom y)) (*(denom x)(numer y)) )
)




;checking version 4

; Function to check mixed fraction - If mixed convert it into improper fraction else improper fraction
(define (check-mixed-frac-to-improper x)
  (if(is_mixed_frac x) 
     (convert-mixed-to-frac x)
     x
   )
 )


; Function to check all values in list and then change to improper fraction and then store it in new list
; li - original list containing mixed fractions, improper and proper fractions
; new_li - new list containing fractions in proper/improper format

(define (conv-list-pro-impro li new_li)
  (if(= (length li ) 0)
     new_li
     (conv-list-pro-impro (cdr li) (append new_li (list (check-mixed-frac-to-improper (car li)) ))     
     )
   )
)

(define (convert-list-pro-impro li)
  (conv-list-pro-impro li (list))
)


; newww

;convert complex into proper/improper fraction
(define (convert-complex-to-frac x)
  (div-list (convert-list-pro-impro (list (car x) (cdr x))) )
)


; Function to check Complex fraction - If Complex convert it into improper fraction else improper fraction
(define (check-complex-frac-to-improper x)
  (if(is_mixed_frac x) 
     (convert-complex-to-frac x)
     x
   )
 )

; Function to check all values in list and then change to improper fraction and then store it in new list
; li - original list containing Complex,mixed fractions, improper and proper fractions
; new_li - new list containing fractions in proper/improper format
(define (conv-complex-frac-to-pro-impro li new_list)
   (if(= (length li ) 0)
     new_list
     (let (
           (element (car li))
           (list-wo-1st-fraction (cdr li))
           )
       (if (is_complex_frac element)
           (conv-complex-frac-to-pro-impro list-wo-1st-fraction (append new_list (list (check-complex-frac-to-improper element) ))    )
           (conv-complex-frac-to-pro-impro
            list-wo-1st-fraction
            (append new_list  (
                                  (lambda (x)
                                    (if(is_mixed_frac x) 
                                       (list(convert-mixed-to-frac x))
                                       (list x)
                                       )
                                    ) element
                              )
             )
            )
           
           )
     )
   )
)


(define (convert-complex-list-pro-impro li)
  (conv-complex-frac-to-pro-impro li (list))
)

;PROCEDURAL ABSTRACTION - TERM(ADD,SUB,MUL,DIV) ---- new
(define (arithmetic-operations term n sum)
    (if(= (length n) 0)
       sum
       (arithmetic-operations term (cdr n) (term sum (car n))
       )
    )
)

;FUNCTION TO ADD A LIST OF NUMBERS - TAKES ONE PARAMETER N(LIST OF NUMBERS) ----- new
(define (add-list n)
  (arithmetic-operations addition (convert-complex-list-pro-impro n) (proper_improper  0 1))
)

;FUNCTION TO MULTIPLY A LIST OF NUMBERS - TAKES ONE PARAMETER N(LIST OF NUMBERS)---new
(define (mul-list n)
  (if (= (length n) 0)
      (proper_improper  0 1)
      (arithmetic-operations multiplication (convert-complex-list-pro-impro n) (proper_improper  1 1))
  )
)

(define (tot_div_sub func term n)
  (if(= (length n) 1)
       (car n)  
       (func term (cdr n) (car n))
  )
)


(define (sub-list n)
  (if (= (length n) 0)
      (display "Error - Enter atleast one element")
      (tot_div_sub arithmetic-operations subtraction (convert-complex-list-pro-impro n))
  )
)

(define (div-list n)
  (if (= (length n) 0)
      (display "Error - Enter atleast one element")
       (tot_div_sub arithmetic-operations divide (convert-complex-list-pro-impro n)))  
)

(define (print-arithmetic-ops x li )
  (print-fraction (x li))
 )

;sample variables
(define (start a)
  (cond
    ((= a 1)  
(display "1 :To enter proper/improper fraction \n")
(display "2 :To enter mixed fraction \n")
(display "3 :To make complex fraction\n")
(let((choice(read)))
(cond
 
((= choice 1)
   (while (not (string=? (read-line)
                      "quit"))
(begin
  (display "Enter proper/improper fractions\n")
  (define p1 (proper_improper (read) (read)))
(define p2 (proper_improper (read) (read)))


)(display "Enter what operation need to be performed\n")
(let((check(read)))
  (cond((eq? check 'addition)(display(addition p1 p2)))
       ((eq? check 'subtraction)( display (subtraction p1 p2)))
       ((eq? check 'multiplication)(display (multiplication p1 p2)))
       ((eq? check 'divide)(display (divide p1 p2)))
       ((eq? check 'frac-mixed)(display ( convert-frac-to-mixed p1) ))
       ((eq? check 'invert)(display (invert p1) ))
       ((eq? check 'equality)(display (equality p1 p2) ))
       ((eq? check 'power)(display (power-of-fraction p1 p2)))
       (else error "unknown type"))
check)
 (display "\n1-yes 0-no\n")
  (display "Do you want to continue: \n")
      (let((temp (read)))
       
      
    (if (= temp 1)(continue)(break)))
      
      (printf "quit?  ")
      )
          
 )

((= choice 2)
   (while (not (string=? (read-line)
                      "quit"))
(begin
  (display "Enter mixed fractions\n")
  (define m1 (make-mixed-fraction (read) (read) (read)))
  (define m2 (make-mixed-fraction (read) (read) (read)))

)(display "Enter what operation need to be performed\n")
(let((check(read)))
  (cond((eq? check 'mixed-improper)(display (check-mixed-frac-to-improper m1)))
       ((eq? check 'mixed-frac)( display ( convert-mixed-to-frac m2 )))
       (else error "unknown type"))
check)
  (display "\n1-yes 0-no\n")
  (display "Do you want to continue: \n")
      (let((temp (read)))
       
      
    (if (= temp 1)(continue)(break)))
      
      (printf "quit?  ")
      )
          
 )


((= choice 3)
   (while (not (string=? (read-line)
                      "quit"))
(begin
  (display "Enter proper/improper and mixed\n")
  (define p1 (proper_improper (read) (read)))
  (define p2 (proper_improper (read) (read)))
  (define m1 (make-mixed-fraction (read) (read) (read)))
  (define m2 (make-mixed-fraction (read) (read) (read)))
  


)(display "Enter what operation need to be performed\n")
(let((check(read)))
  (cond((eq? check 'make-complex)(display(make-complex-fraction m2 p2)))
       (else error "unknown type"))
check)
 (display "\n1-yes 0-no\n")
  (display "Do you want to continue: \n")
      (let((temp (read)))
       
      
    (if (= temp 1)(continue)(break)))
      
      (printf "quit?  ")
      )
          
 )

)
)
   
)) )

;sample variables
(define gm1 (make-mixed-fraction 1 3 4))
(define gm2 (make-mixed-fraction 3 1 4))

(define gp1 (proper_improper 1 2))
(define gp2 (proper_improper 3 2))

(define gc1 (make-complex-fraction gm1 gp1))
(define gc2 (make-complex-fraction gp1 gm1))


(define gl1 (list gp1 gm1))
(define gl2 (list gp1 gm1 gm2 gp2))
(define gl3 (list gp1 gm1 gc1))
(define gel (list ))

(define gep1 (proper_improper 2 1))
(define gep2 (proper_improper 2 3))



 

   

  

 
