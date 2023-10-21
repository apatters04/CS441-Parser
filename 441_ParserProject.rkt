#lang racket
(require data/maybe)   ; for maybe type 
(require data/either)  ; for either type 
(require data/monad)   ; for chain function 
(require megaparsack/parser-tools/lex)
(require megaparsack megaparsack/text)

(define rsrvd-words '("if" "while" "read" "write" "goto" "gosub" "return" "break" "end"))
(define bool-ops '("<" ">" ">="  "<=" "<>" "=="))

(define (parse filename) ; reading in file
  (cond
    [(chain iter-file(read-file filename)) (displayln "Accept")]
        (else
         (displayln "Syntax Error"))))

(define (read-file file-path); reads in file, breaks file into lines, and each line in a list of strings
  (cond
    ((file-exists? file-path)
     (success
       (map(lambda (line) (string-split line)) (file->lines file-path))))
   (else
     (failure "File not found")))) ;provide failure if file does not exist


(define (iter-file L)
  (define (remove-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst)))))
  
  (and (for ([line (remove-last L)])
    (program? line))
  (equal? (first (last L)) "$$")))

;program? -> linelist $$
(define (program? L)
  (linelist? L))

;linelist? -> line linelist | epsilon
(define (linelist? L)
  (cond
    [(stmt? L) (success #t)]
    [(line? L) (success #t)]
    [(is-end? L) (success #t)]
    (else
     (failure #f))
    ))

(define (is-end? L)
  (cond
    [(equal? (first L) "$$") #t]))

;line? -> label stmt linetail
(define (line? L)
  (label? (first L) (rest L)))

;stmt? -> id = expr; | if (boolean) stmt; | while (boolean) linelist endwhile; | read id; | write expr; | goto id; | gosub id; | return; | break; | end;
(define (stmt? L)
  (cond
  [(and (reserved-word? L rsrvd-words) (equal? (last L) ";")) (success #t)]
  [(and (id? (first L) (rest L)) (equal? (last L) ";")) (success #t)]
  (else
   (failure #f)
  )))

(define (reserved-word? L rsrvd-words) ; parse first token of a line if its a statement, if not check if its an id
  (cond
    [(member (first L) rsrvd-words)
     (success (which-reserved? (first L) (rest L)))]
    [(id? (first L) (rest L)) (etail? (first (rest L)) (rest L))]
    (else
     (failure 'no-program))))

(define (which-reserved? token L)
  (cond
    [(equal? token "if") (if? (rest L))]
    [(equal? token "while") (while? token L)]
    [(equal? token "read") (read? token (rest L))] ;if token is read, run read function, copy this format for the rest of reserved words
    [(equal? token "write") (write? token L)]
    [(equal? token "goto") (goto? token L)]
    [(equal? token "gosub") (gosub? token L)]
    [(equal? token "return") (return? token L)]
    [(equal? token "break") (break? token L)]
    [(equal? token "end") (end? token L)]
    ))

;RESERVED WORD HANDLING

(define (if? L)
  (cond
    [(l-paren? (first L)) (expr? (first (rest L)) (rest (rest L)))]
    (else #f)))

(define (while? token L)
  (cond
    (expr? (first L) (rest L))))

(define (read? token L) 
  (cond
    ((equal? token "read")
     (id? (first L) (rest L)) ;not the best solution to reading the next token as an ID
     (success 'stmt))
  (else
   (failure 'error))))

(define (write? token L)
  (cond
    [(expr? (first L) (rest L)) (success 'write)]
    (else
     (failure 'write-error))))

(define (goto? token L) 
  (cond
    ((equal? token "goto")
     (id? (rest L)) ;not the final solution to reading the next token as an ID also does not factor in semicolon, but it works for now until someone shows me how to iterate better :)
     (success 'stmt))
  (else
   (failure 'error))))

(define (gosub? token L) 
  (cond
    ((equal? token "gosub")
     (id? (rest L)) ;not the final solution to reading the next token as an ID also does not factor in semicolon, but it works for now until someone shows me how to iterate better :)
     (success 'stmt))
  (else
   (failure 'error))))

(define (return? token L)
  (cond
    [(equal? (last L)  ";") (success 'return-stmt)]
    (failure 'Missing-semi-colon)))

(define (break? token L)
    (cond
    [(equal? (last L)  ";") (success 'return-stmt)]
    (failure 'Missing-semi-colon)))

(define (end? token L)
    (cond
    [(and (> (length L) 0) (equal? (last L)  ";") (success 'return-stmt))]
    (failure #f)))



;label? -> id: | epsilon
(define (label? token L)
  (define n (string-length token))
  (let
      ([i(id?(substring token 0 (- n 1)) L)]) ;checks if string before colon is an ID
  (let
    ([m(equal? (string-ref token(sub1 (string-length token))) #\:)]) ;checking if last char of string is a colon
  (cond ((and i m) (success (stmt? L))) ;if string is an ID and the last char is a colon, then it is a label
  (else
   (failure 'error))))))

;linetail? -> stmt+ | epsilon

;boolean? -> true | false | expr bool-op expr
(define (boolean? token L)
  (cond
    [(expr? token L) #t]
    [(equal? token "true") (success (linelist? L))]
    [(equal? token "false") (success (linelist? L))]))

;bool-op? -> < | > | >= | <= | <> | ==
(define (bool-op? token L)
  (cond
    [(member token bool-ops)
     (success (expr? (first L) (rest L)))]))

;expr? -> id etail | num etail | (expr)
(define (expr? token L)
  (cond
    [(l-paren? token) (id? (first L) (rest L))]
    [(id? token (rest L)) (success (etail? (first L) (rest L)))]
    [(id? token (rest L)) (success (id-assign? (first L) (rest L)))]    
    [(id? token (rest L)) (success (bool-op? (first L) (rest L)))]
    [(is-num? token) (success (etail? (first L) (rest L)))]
    [(is-num? token) (success (bool-op? (first L) (rest L)))]
    (failure 'no-expr)))
  

;etail?-> + expr | - expr | * expr | / expr | epsilon
(define (etail? token L)
  (cond
    [(add-op? (first L)) (expr? (first L) (rest L))]
    [(mult-op? (first L)) (expr? (first L) (rest L))]
    (else
     (r-paren? token (rest L))
    )))

(define (l-paren? token)
  (cond
    [(equal? token "(") #t]))

(define (r-paren? token L)
  (cond
    ;[(equal? token ")") (id? (first L) (rest L))]
    [(equal? token ")") (stmt? L)]
    (else
     (failure 'bad-expr))))

(define (add-op? token)
  (cond
    [{equal? token "+"} #t]
    [{equal? token "-"} #t]

    (else
     nothing)))

(define (mult-op? token)
  (cond
    [{equal? token "*"} #t]
    [{equal? token "/"} #t]

    (else
     nothing)))

(define (id-assign? token L)
  (cond
    [(equal? token "=") (expr? (first L) (rest L))]
    ))
    
;id? -> [a-zA-z] [a-zA-Z0-9]*
(define (id? token L)
  (if (empty? L)
      (success 'id)
      (match token
        [(regexp #rx"^([a-zA-Z][a-zA-Z0-9]*)") (id-assign? (first L) (rest L))]
        (failure (is-num? token L))))) ; if token isnt a reserved word or an ID, finally check if its a num

;num? checking if token is a number : numsign digit digit*
(define (is-num? token L)
  (cond
    ((number? (string->number token)) ;turns token into a string, check if this conversion is a number, accounts for +/-/epsilon numsign
     #t)
  (else
   (failure #f))))