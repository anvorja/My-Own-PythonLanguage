#lang eopl

;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos y recursion

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>        ::= <expression>
;;                       <un-programa (exp)>

;; <class-decl>     ::= class <identificador> extends <identificador> {field <identificador>}* {<method-decl>}*
;;                      <a-class-decl(class-name super-name fields-ids method-decls)>

;; <method-decl>    ::= method <identificador> ( {<identificador>}*(,) ) <expresion>
;;                      <a-method-decl (method-name ids body)>

;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <expr-bool>
;;                      <boolean-expr (datum)>
;;                  ::= [{< expression>}* (;)]
;;                      <lista (list-elements)>
;;                  ::= <list-prim>
;;                      <prim-list-exp (datum)>
;;                  ::= <{ {<identificador> =<expresion>}+(;)}>
;;                      <registro (first-id first-value rest-id rest-value)>
;;                  ::= <regs-prim>
;;                      <prim-registro-exp (regs-prim)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expression> then <expression> else <expression> endif
;;                      <if-exp (test-exp true-exp false-exp)>
;;                  ::= begin {<expresion>}+(;) end
;;                     <secuencia expSec>
;;                  ::= for <identificador> = <expresion> <to-odownto> <expresion> do <expresion> done
;;                      <for-exp (id exp1 to-odwto exp2 exp3)>
;;                  ::= while <expresion-bool> do { <expresion>}done
;;                     <while-exp expBoolWhile expWhile>
;;                  ::= proc({<identificador>}*(,)) { <expression> }
;;                      <proc-exp (ids body)>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* { <expression> }
;;                      <letrec-exp proc-names idss bodies letrec-body>
;;                  ::= invocar(<expression> (<expression>)*(,))
;;                      <app-exp proc rands>

;;                  ::= new <identificador> ({<expresion}*(,))
;;                     <new-object-exp (class-name rands)>
;;                  ::= send <expresion> <identificador> ({<expresion>}*(,))
;;                     <method-app-exp (obj-exp method-name rands)>
;;                  ::= super identifier({identifier>}*(,))
;;                      <super-call-exp method-name rands>

;;                  ::= base <num> ({number}* )
;;                      <base-exp>
;;                  ::= "<cadena>"
;;                      <cadena-exp (cadena)>

;; <expr-bool>      ::= pred-prim ( expression , expression )
;;                      <comp-pred (pred-prim rand1 rand2)>
;;                  ::= oper-bin-bool ( expr-bool , expr-bool )
;;                      <comp-bool-bin (oper-bin-bool rand1 rand2)>
;;                  ::= <bool>
;;                      <booleano-lit (datum)>
;;                  ::= oper-un-bool ( expr-bool)
;;                      <comp-bool-un (oper-un-bool rand)>
;; <list-prim>      ::= vacio-lista()
;;                      <prim-make-empty-list>
;;                  ::= vacio?-lista(<expression>)
;;                      <prim-empty-list (list)>
;;                  ::= crear-lista({<expression>}* (,))
;;                      <prim-make-list (list-elem)>
;;                  ::= lista?(<expression>)
;;                      <prim-list?-list (exp)>
;;                  ::= cabeza-lista(<expression>)
;;                      <prim-head-list (exp)>
;;                  ::= cola-lista(<expression>)
;;                      <prim-tail-list (exp)>
;;                  ::= append-lista(<expression>,<expression>)
;;                      <prim-append-list (exp1 exp2)>
;;                  ::= ref-lista(<expression>,<expression>)
;;                      <prim-ref-list (list pos)>
;;                  ::= set-lista(<expression>,<expression>,<expression>)
;;                      <prim-ref-list (list pos value)>
;; <tuple-prim>     ::= vacio?-tupla(<expression>)
;;                      <prim-empty-tuple (tuple)>
;;                  ::= vacio-tuple()
;;                      <prim-make-empty-tuple>
;;                  ::= crear-tupla({<expression>}* (,))
;;                      <prim-make-tuple (elem)>
;;                  ::= tupla?(<expression>)
;;                      <tuple-prim? (exp)>
;;                  ::= cabeza-tupla(<expression>)
;;                      <prim-head-tuple (exp)>
;;                  ::= cola-tupla(<expression>)
;;                      <prim-tail-tuple (exp)>
;;                  ::= ref-tuple <expression>,<expression>)
;;                      <prim-ref-tuple (tuple pos)>
;; <regs-prim>      ::= registros?(<expression>)
;;                      <prim-regs?-registro (exp)>
;;                  ::= crear-registro({<identificador> =<expresion>}+(,))
;;                      <prim-make-registro (first-id first-value rest-id rest-value)>
;;                  ::= ref-registro(<expression>, <identifier>)
;;                      <prim-ref-registro (exp id-exp)>

;;                  ::= set-registro(<expression>, <identifier>,<expression>)
;;                      <prim-set-registro (exp id-exp val)>

;; <pred-prim>      ::= < | > | <= | >= | == | <>
;; <oper-bin-bool>  ::= and|or
;; <oper-un-bool>   ::= not
;; <primitive>      ::= + | ~ | * | add1 | sub1

;******************************************************************************************


;******************************************************************************************
;Especificación Léxica
(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?" "."))) symbol)
  (bool
   ((or "@true" "@false")) symbol)
  (txt
   ("\"" (arbno (or letter digit whitespace "," "." ":" "-")) "\"") string)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." (arbno digit)) number)
  )

   )


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    (program ((arbno class-decl) expression) a-program)

    ;;Expression
    (expression (number) numero-lit)
    (expression (identifier) var-exp)
    (expression (txt)  texto-lit)
    (expression (uni-primitive "(" expression ")") primapp-un-exp)
    (expression ("(" expression bi-primitive expression ")") primapp-bi-exp)
    (expression (expr-bool) boolean-expr)

    (expression (list-prim) prim-list-exp)

    (expression (tuple-prim) prim-tuple-exp)
    (expression ("{"identifier "=" expression (arbno ";" identifier "=" expression) "}") registro)
    (expression (regs-prim) prim-registro-exp)

    ;;if
    (expression ("if" expression "then" expression "else" expression "endif") if-exp)

    ;;procedimiento
    (expression ("proc" "(" (separated-list identifier ",") ")" "{" expression "}") proc-exp)

    ;;evaluar
    (expression ("invocar" "(" expression "(" (separated-list expression ",") ")" ")") app-exp)

    ;;letrec
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "{" expression "}") letrec-exp)

    ;;const
    (expression ("constantes" "(" (separated-list identifier "=" expression ";") ")""{" expression "}") constanteLocal-exp)

    ;;lista
    (expression ("["(separated-list expression ";") "]") lista)

    ;;tupla
    (expression ("tupla("(separated-list expression ";") ")") tupla)


    ;;begin
    ;(expression ("begin" "{" expression ";" (arbno expression ";") "}" "end") secuencia-exp)

    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)

    ;;print
    (expression ("print" "(" expression ")") print-exp)

    ;;for
    (expression ("for" identifier "=" expression to-o-downto expression "do" expression "done") for-exp)
    (to-o-downto ("to") to)
    (to-o-downto ("downto") downto)

    ;while
    (expression ("while" "("expression")" "do" expression"end" ) while-exp)

    (expression ("variables" "(" (separated-list identifier "=" expression ";") ")""{" expression "}") variableLocal-exp)


    (expression ("base" expression "(" (arbno expression) ")") base-exp)

    ;;;;;;
    (expression ("actualizar" identifier "=" expression)updateVar-exp)
    (expression ("bloque" "{" expression (arbno ";" expression) "}")
                block-exp)

    ;;;;;;
    (expr-bool (pred-prim "(" expression "," expression ")") comp-pred)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") comp-bool-bin)
    (expr-bool (bool) booleano-lit)
    (expr-bool (oper-un-bool "(" expr-bool ")") comp-bool-un)

    ;(expression (primitiva "(" (separated-list expression ",") ")")  primapp-exp)

    ;(primitiva ("print-obj") primitiva-print-obj)

    ;(primitiva ("print")   primitiva-print)

    ;------------primitivas binarias-------------
    (bi-primitive ("+") primitiva-suma)
    (bi-primitive ("~") primitiva-resta)
    (bi-primitive ("*") primitiva-multi)
    (bi-primitive ("/") primitiva-div)
    (bi-primitive ("concat") primitiva-concat)
    (bi-primitive ("mod") primitiva-elmodulo)

    ;------------primitivas unarias-------------
    (uni-primitive ("add1") primitiva-add1)
    (uni-primitive ("sub1") primitiva-sub1)
    (uni-primitive ("longitud") primitiva-longitud)

    ;;;;;;Booleanos
    (pred-prim ("<") prim-bool-menor)
    (pred-prim (">") prim-bool-mayor)
    (pred-prim ("<=") prim-bool-menor-igual)
    (pred-prim (">=") prim-bool-mayor-igual)
    (pred-prim ("==") prim-bool-equiv)
    (pred-prim ("<>") prim-bool-diff);No igual a (!=),
    (oper-bin-bool ("and") prim-bool-conj)
    (oper-bin-bool ("or") prim-bool-disy)
    (oper-un-bool ("not") prim-bool-neg)

    ;;;;;;Listas
    (list-prim ("vacio-lista" "("")") prim-make-empty-list)
    (list-prim ("vacio?-lista" "("expression")") prim-empty-list)
    (list-prim ("crear-lista" "("(separated-list expression ",") ")") prim-make-list); crear-lista(<elem1>,<elem2>,<elem3>,...)
    (list-prim ("lista?" "("expression")") prim-list?-list); lista?(<lista>)-> Bool
    (list-prim ("cabeza-lista""(" expression")") prim-head-list);cabeza-lista(<lista>)-> <elem1>
    (list-prim ("cola-lista" "(" expression")") prim-tail-list);cola-lista(<lista>)-> <elem2>,<elem3>,...
    (list-prim ("append-lista""("expression "," expression")") prim-append-list);append-lista([<elem1>,<elem2>,<elem3>,...],[<elemA>,<elemB>,<elemC>,...])-> <elem1>,<elem2>,<elem3>,...,<elemA>,<elemB>,<elemC>,...
    (list-prim ("ref-lista""("expression "," expression")") prim-ref-list);ref-lista(<lista>, pos)
    (list-prim ("set-lista""("expression "," expression "," expression ")") prim-set-list);set-lista(<lista>, pos, value)

    ;;;;;; Tuplas
    (tuple-prim ("vacio?-tupla" "("expression")") prim-empty-tuple)
    (tuple-prim ("vacio-tupla" "("")") prim-make-empty-tuple)
    (tuple-prim ("crear-tupla" "("(separated-list expression ",") ")") prim-make-tuple); crear-tupla(<elem1>,<elem2>,<elem3>,...)
    (tuple-prim ("tupla?" "("expression")") prim-tuple?-tuple); tupla?(<tupla>)-> Bool
    (tuple-prim ("cabeza-tupla""(" expression")") prim-head-tuple);cabeza-tupla(<tupla>)-> <elem1>
    (tuple-prim ("cola-tupla" "(" expression")") prim-tail-tuple);cola-tupla(<lista>)-> <elem2>,<elem3>,...
    (tuple-prim ("ref-tupla""("expression "," expression")") prim-ref-tuple);ref-tupla(<lista>, pos)

    ;;;;;;Registros
    (regs-prim ("registros?" "(" expression ")") prim-regs?-registro)
    (regs-prim ("crear-registro" "(" identifier "=" expression (arbno "," identifier "=" expression) ")") prim-make-registro)
    (regs-prim ("ref-registro" "(" expression ","expression ")") prim-ref-registro); ref-registro(<registro>,<id>) -> <value>
    (regs-prim ("set-registro" "(" expression ","expression","expression ")") prim-set-registro); set-registro(<registro>,<id>, <new-value>)

    ;;;;;;;;;;;;;OOP;;;;;;;;;;;;;

    ;;class-decl
    (class-decl ("clase" identifier "hereda" identifier (arbno "campo" identifier) (arbno method-decl)) a-class-decl)

    ;;method-decl
    (method-decl ("metodo" identifier "(" (separated-list identifier ",") ")" "{" expression "}")a-method-decl)

    ;;new-object
    (expression ("new" identifier "(" (separated-list expression ",") ")") new-object-exp)

    ;;super-call
    (expression ("super" identifier "(" (separated-list identifier ",") ")") super-call-exp)

    ;;method-app-exp
    (expression ("send" expression identifier "("  (separated-list expression ",") ")") method-app-exp)

    ;;
    (expression ("mostrar") mostrar-exp)

  )
)
;*******************************************************************************************
;Construidos automáticamente:
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x)
     (list (direct-target 1))

     (empty-env))))


;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls)
        (eval-expression exp (init-env))))))



; eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

(define eval-expression
  (lambda (exp env)
    (cases expression exp

      (numero-lit (datum) datum)

      (texto-lit (datum) (substring datum 1 (-(string-length datum) 1)))

      (var-exp (id) (apply-env  env id))

      (primapp-un-exp (prim rand)
          (let ((arg (eval-primapp-exp-rand rand env)))
               (apply-uni-primitive prim arg)))

      (primapp-bi-exp (rand1 prim rand2)
          (let((arg1 (eval-primapp-exp-rand rand1 env))
               (arg2 (eval-primapp-exp-rand rand2 env)))
               (apply-bi-primitive arg1 prim arg2)))

      (boolean-expr (datum) (eval-bool-exp datum env))

      (lista (list-elements)
             (list->vector (map (lambda (element) (eval-expression element env) ) list-elements)))

      (prim-list-exp (datum) (eval-prim-list datum env))

      (tupla (elements)
             (map (lambda (element) (eval-expression element env) ) elements))

      (prim-tuple-exp(a) (eval-prim-tuple a env))

      (base-exp (base valores)
                (eval-base-exp (eval-expression base env) (map (lambda (element) (eval-expression element env) ) valores)  env))


      (registro (first-id first-value rest-id rest-value) (list (cons first-id rest-id)
                                                                (list->vector (map (lambda (element) (eval-expression element env) ) (cons first-value rest-value)))))

      (print-exp (exp) (begin (display (eval-expression exp env)) (display "\n") 'endPrint))


       (begin-exp (exp lexps)
                 (if (null? lexps)
                     (eval-expression exp env)
                     (letrec
                         [(recorrer (lambda (L)
                                      (cond
                                        [(null? (cdr L)) (eval-expression (car L) env)]
                                        [else (begin (eval-expression (car L) env)
                                                     (recorrer (cdr L))
                                        )]
                                        )
                                      ))
                          ]
                       (begin
                         (eval-expression exp env)
                         (recorrer lexps))
                         )
                     )
                 )

      (prim-registro-exp (regs-prim) (eval-regs-prim regs-prim env))

      (if-exp (test-exp true-exp false-exp)
          (if (valor-verdad? (eval-expression test-exp env))
            (eval-expression true-exp env)
            (eval-expression false-exp env)))


      (for-exp (id exp1 tod exp2 body)
               (letrec
                   [(i (eval-expression exp1 env))
                    (parada (eval-expression exp2 env))
                    (op (cases to-o-downto tod
                          (to () +)
                          (downto () -)
                          ))
                    (proc-for (cerradura (list id) body env))
                    (for (lambda (var)
                           (if (eqv? var parada)
                               (apply-procedure proc-for (list (direct-target var)))
                               (begin (apply-procedure proc-for (list (direct-target var))) (for (op var 1)))
                               )
                               )
                           )]
                 (for i)
                   )
               )

      (while-exp (bool-exp body) (eval-while-exp bool-exp body env))

      (proc-exp (ids cuerpo)(cerradura ids cuerpo env))

      (app-exp (rator rands)
               (let((proc (eval-expression rator env))
                    (args (eval-rands rands env)))
                 (if(procVal? proc)
                    (apply-procedure proc args)
                    (eopl:error 'eval-expression
                                "Attemp to apply non-procedure ~s" proc))))

      (variableLocal-exp (ids values body)
                         (let ((args (eval-variableLocal-exp-rands values env)))
                 (eval-expression body (extend-env ids args env))))

      (constanteLocal-exp (ids values body)
                         (let ((args (eval-variableLocal-exp-rands values env)))
                           (if (searchUpdateValExp body)
                               (eopl:error 'eval-expression
                                "No puedes actualizar los valores de constantes ~s" body)
                               (eval-expression body (extend-env ids args env)))

                           ))

      (updateVar-exp (id newVal)
               (setref!
                  (apply-env-ref env id)
                  (eval-expression newVal env)))

      (block-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps)
                        acc
                        (loop (eval-expression (car exps)
                                               env)
                              (cdr exps)))))


      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      ;; Objetos
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply 'init class-name obj args)
          obj
          ))

      ;; para las expresiones de invocación de métodos (send)
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))

      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      (mostrar-exp () the-class-env)
      (else 1))))



;;funcion auxiliar que evalua expresiones booleanas

(define eval-bool-exp
  (lambda (expr-boolean env)
    (cases expr-bool expr-boolean
      (comp-pred (pred-prim rand1 rand2)
                (let ((arg1 (eval-expression rand1 env))
                      (arg2 (eval-expression rand2 env)))
                  (eval-comp-pred pred-prim arg1 arg2)))
      (comp-bool-bin (oper-bin-bool rand1 rand2)
                     (let ((arg1 (eval-bool-exp rand1 env))
                           (arg2 (eval-bool-exp rand2 env)))
                          (eval-comp-bool-bin oper-bin-bool arg1 arg2)))
      (booleano-lit (datum) (if (equal? datum '@true) #t #f))
      (comp-bool-un (oper-unario-bool rand)
                    (let ((arg1 (eval-bool-exp rand env)))
                          (cases oper-un-bool oper-unario-bool
                            (prim-bool-neg () (not arg1)))
                      )))))


;;funcion auxiliar recibe una <pred-prim>  y dos argumentos ya evaluados, les aplica el respectivo predicado.
(define eval-comp-pred
  (lambda (pred-primitive arg1 arg2)
    (cases pred-prim pred-primitive
      (prim-bool-menor () (< arg1 arg2))
      (prim-bool-mayor () (> arg1 arg2))
      (prim-bool-menor-igual () (<= arg1 arg2))
      (prim-bool-mayor-igual () (>= arg1 arg2))
      (prim-bool-equiv () (equal? arg1 arg2))
      (prim-bool-diff () (not (equal? arg1 arg2))))))

;;funcion auxiliar, recibe un <oper-bin-bool >  y dos argumentos ya evaluados, les aplica el respectivo predicado.
(define eval-comp-bool-bin
  (lambda (oper-binario arg1 arg2)
    (cases oper-bin-bool oper-binario
      (prim-bool-conj () (and arg1 arg2))
      (prim-bool-disy () (or arg1 arg2))
      )))

;;funcion auxiliar, evalua las primitivas sobre listas
(define eval-prim-list
  (lambda (primitiva env)
    (cases list-prim primitiva
      (prim-make-empty-list () (make-vector 0))
      (prim-empty-list (exp) (= (vector-length (eval-expression exp env)) 0))
      (prim-make-list (list-elem) (list->vector(map (lambda (elem) (eval-expression elem env)) list-elem)))
      (prim-list?-list (exp) (vector? (eval-expression exp env)))
      (prim-head-list (exp) (if (= (vector-length (eval-expression exp env)) 0)
                                (eopl:error 'eval-expression
                                 "cannot get the head of an empty list" )
                                (vector-ref (eval-expression exp env) 0)))
      (prim-tail-list (exp) (if (= (vector-length (eval-expression exp env)) 0)
                                (make-vector 0)
                                (list->vector(cdr (vector->list (eval-expression exp env))))))
      (prim-append-list (exp1 exp2) (list->vector(append (vector->list (eval-expression exp1 env)) (vector->list (eval-expression exp2 env)))))
      (prim-ref-list (list pos) (if (>= (eval-expression pos env) (vector-length (eval-expression list env)))
                                (eopl:error 'eval-expression
                                 "index ~s out of range [0:~s)"  (eval-expression pos env) (vector-length (eval-expression list env)))
                                (vector-ref (eval-expression list env) (eval-expression pos env))))
      (prim-set-list (list pos value) (if (>= (eval-expression pos env) (vector-length (eval-expression list env)))
                                (eopl:error 'eval-expression
                                 "index ~s out of range [0:~s)"  (eval-expression pos env) (vector-length (eval-expression list env)))
                                 (vector-set! (eval-expression list env) (eval-expression pos env) (eval-expression value env))))
    )))

;; evalua las primitivas sobre tuplas
(define eval-prim-tuple
  (lambda (primitiva env)
    (cases tuple-prim primitiva
      (prim-make-empty-tuple () (list))
      (prim-empty-tuple (exp) (= (length (eval-expression exp env)) 0))
      (prim-make-tuple (tuple-elem) (map (lambda (elem) (eval-expression elem env)) tuple-elem))
      (prim-tuple?-tuple (exp) (list? (eval-expression exp env)))
      (prim-head-tuple (exp) (if (= (length (eval-expression exp env)) 0)
                                (eopl:error 'eval-expression
                                 "cannot get the head of an empty tuple" )
                                (list-ref (eval-expression exp env) 0)))

      (prim-tail-tuple (exp) (if (= (length (eval-expression exp env)) 0)
                                (list)
                                (cdr (eval-expression exp env))))

     (prim-ref-tuple (tuple pos) (if (>= (eval-expression pos env) (length (eval-expression tuple env)))
                                (eopl:error 'eval-expression
                                 "index ~s out of range [0:~s)"  (eval-expression pos env) (length (eval-expression tuple env)))
                                (list-ref (eval-expression tuple env) (eval-expression pos env))))
    )))

;; Números en diferentes bases:
;; Una lista cuyo primer elemento representa la base en la que está escribo y su segundo elemento es la lista que lo representa

(define eval-base-exp
  (lambda (base valores env)
      (list base valores)))

;funciones auxiliar para pasar a base 10
(define base10
  (lambda (numeros exponente base)
    (cond
      ((null? numeros) 0)

      (else
       (+ (*(car numeros) (expt base exponente)) (base10 (cdr numeros) (+ exponente 1) base))))))

;; pasar numeros a decimal
(define to-decimal
  (lambda (numeros base)
    (base10 numeros 0 base)
    ))
;;sumar numeros en otras bases
(define operacion-base
  (lambda (operacion list-num1 list-num2 base)
   (cons base (list (to-base (operacion(to-decimal list-num1 base) (to-decimal list-num2 base)) base )))))

;; pasar numero a una base dada residuo(remainder 258 16) entero(quotient 258 16)
(define to-base
  (lambda (numero base)
    (cond
      ((=(quotient numero base)0) (list numero))
      (else
       (cons (remainder numero base) (to-base (quotient numero base) base))
       )
      )
    ))

;Implementación estructura while
(define eval-while-exp
        (lambda (bool-exp body env)
          (if (valor-verdad? (eval-expression bool-exp env)) (begin (eval-expression body env) (eval-while-exp bool-exp body env)) 'fin )))

;;
(define eval-regs-prim
  (lambda (regs-primitive env)
    (cases regs-prim regs-primitive

      (prim-regs?-registro (exp) (let ((regs (eval-expression exp env)))
                             (if (and (list? regs) (= (length regs) 2))
                                 (if (and (vector? (cadr regs))
                                          (= (length (car regs)) (vector-length (cadr regs))))
                                     #t
                                     #f)
                                 #f)))
      (prim-make-registro (first-id first-value rest-id rest-value) (list (cons first-id rest-id)
                                                                (list->vector (map (lambda (element) (eval-expression element env) ) (cons first-value rest-value)))))
      (prim-ref-registro (exp id-exp) (let ((regs (eval-expression exp env))
                                        (ids (car (eval-expression exp env)))
                                        (values (cadr (eval-expression exp env)))
                                        (id (cases expression id-exp
                                              (var-exp (datum) datum)
                                              (else (eopl:error 'eval-expression "expression ~s is not an identifier" id-exp)))))
                                    (if (number? (list-find-position id ids))
                                        (vector-ref values (list-find-position id ids))
                                        (eopl:error 'eval-expression "identifier ~s not found, available identifiers ~s" id ids))))
      (prim-set-registro (exp id-exp val) (let ((regs (eval-expression exp env))
                                        (ids (car (eval-expression exp env)))
                                        (values (cadr (eval-expression exp env)))
                                        (id (cases expression id-exp
                                              (var-exp (datum) datum)
                                              (else (eopl:error 'eval-expression "expression ~s is not an identifier" id-exp)))))
                                    (if (number? (list-find-position id ids))
                                        (vector-set! values (list-find-position id ids) (eval-expression val env))
                                        (eopl:error 'eval-expression "identifier ~s not found, available identifiers ~s" id ids))))
      (else 1))))


; funciones auxiliares para aplicar eval-expression a cada elemento de una
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define eval-variableLocal-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-variableLocal-exp-rand x env))
         rands)))

(define eval-variableLocal-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-bi-primitive: <expression> <primitiva-binaria> <expression> -> numero|cadena
(define apply-bi-primitive
  (lambda (arg1 prim arg2)
    (cases bi-primitive prim
       (primitiva-suma ()(if (and (list? arg1) (list? arg2))
                           (operacion-base + (car(cdr arg1)) (car(cdr arg2)) (car arg1))
                           (+ arg1 arg2 )))
      (primitiva-resta () (if (and (list? arg1) (list? arg2))
                              (operacion-base - (car(cdr arg1)) (car(cdr arg2)) (car arg1))
                              (- arg1 arg2 )))
      (primitiva-multi () (if (and (list? arg1) (list? arg2))(operacion-base * (car(cdr arg1)) (car(cdr arg2)) (car arg1))(* arg1 arg2 )))
      (primitiva-div () (if (and (list? arg1) (list? arg2)) ((eopl:error 'deref
                                                      "No se puede realizar esta operacion con bases" ))(/ arg1 arg2 )))

      (primitiva-elmodulo () (if (and (list? arg1) (list? arg2)) ((eopl:error 'deref
                                                      "No se puede realizar esta operacion con bases" ))(modulo arg1 arg2 )))
      (primitiva-concat () (string-append arg1 arg2))

    )))

;apply-uni-primitive: <primitiva-unaria> <expression> -> numero
(define apply-uni-primitive
  (lambda (prim args)
    (cases uni-primitive prim
      (primitiva-add1 () (if (list? args)
                           (operacion-base + (car(cdr args)) '(1) (car args))
                           (+ args 1 )))
      (primitiva-sub1 () (if (list? args)
                           (operacion-base - (car(cdr args)) '(1) (car args))
                           (- args 1 )))
      (primitiva-longitud (string-length args)))))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero.
(define valor-verdad?
  (lambda (x)
    (if (eqv? x #t)
        #t
        #f)))

;*******************************************************************************************
;Procedimientos ; clousure
;definir un datatype para la cerradura

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)
   ))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

; **************************************************************************************
;;Variables mutables, constantes y actualizacion de variables

;;Auxiliares para checkear los tipos de datos referencia y blanco
(define expval?
  (lambda (x)
    (or (number? x) (procVal? x) (string? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))


;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;;Metodos para manipular las referencias

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "No puede haber una referencia hacia otra referencia: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;;Metodo que busca si existe una expresion de seteo en un body

(define searchUpdateValExp
  (lambda (body)
    (cases expression body
      (numero-lit (datum) #f)
      (while-exp (bool-exp body) #f)
      (texto-lit (datum) #f)
      (var-exp (id) #f)
      (print-exp (exp) #f)
      (begin-exp (exp lexps) #f)
      (base-exp(base valores)#f)
      (primapp-un-exp (prim rand)
          #f)
      (primapp-bi-exp (rand1 prim rand2)
          #f)
      (boolean-expr (datum) #f)
      (lista (list-elements)
             #f)
      (prim-list-exp (datum) #f)
      (tupla (list-elements)
             #f)
      (prim-tuple-exp (datum) #f)
      (registro (first-id first-value rest-id rest-value) #f)
      (prim-registro-exp (regs-prim) #f)
      (if-exp (test-exp true-exp false-exp)
          #f)
      (for-exp (id exp1 tod exp2 body) #f)
      (proc-exp (ids cuerpo)
          (searchUpdateValExp cuerpo))
      (app-exp (rator rands)
               #f)
      (variableLocal-exp (ids values body)
                        (searchUpdateValExp body))
      (constanteLocal-exp (ids values body)
                        (searchUpdateValExp body) )

      (updateVar-exp (id newVal)
               #t)

      (block-exp (exp exps)
                 (if (searchUpdateValExp exp)
                     #t
                     (let loop (
                                 (exps exps)
                               )
                       (if (null? exps)
                           #f
                           (if (searchUpdateValExp (car exps))
                               #t
                               (loop (cdr exps))
                           )
                       )
                     )
                 )
        )
      (letrec-exp (proc-names idss bodies letrec-body)
                  (searchUpdateValExp letrec-body))
      (new-object-exp (id args) #f)
      (mostrar-exp () #f)
      (method-app-exp (obj-exp method-name rands) #f)
      (super-call-exp (method-name rands) #f)
    )
  )
)


;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vec vector?)
                       (env environment?))
  )

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env
  (lambda ()
    (empty-env-record)))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (cerradura ids body env))))
            (iota len) idss bodies)
          env)))))
;_____________________________________________________________________________________________________________________________________________
;;;;;;;;;;;;;;;;;;;;;;POO;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ambiente de declaracion de clases

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Class not known ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;; Ambiente de métodos

(define lookup-method-decl
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;; Auxiliares para el manejo de declaraciones de clases y métodos

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        (map (lambda (id) (string->symbol (string-append "self." (symbol->string id)))) field-ids)))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;;;;;;;;;;;;;;;;;; Selectores ;;;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

(define-datatype part part?
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'objeto)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))(direct-target 0)))))


;;;;;;;;;;;;;;;;;;;; Métodos ;;;;;;;;;;;;;;;;;;

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'objeto)
      (if (eqv? m-name 'init)
          (eopl:error 'find-method-and-apply
        "~s method was not provided" m-name)
          (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name))
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
           (apply-method m-decl host-name self args)
          (find-method-and-apply m-name
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))



;________________________________________________________________________________________________________________________
;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)

(deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env-ref "No se encontro en el ambiente a ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))



;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;*******************************PRUEBAS***********************************************************

;Expression- id-exp
(scan&parse "x")

;Expression- false-exp
(scan&parse "false")

;Expression- true-exp
(scan&parse "true")

;Expression- primapp-exp

;Primitiva Print
(scan&parse "print(x)")

;Números
(scan&parse "-8")
(scan&parse "3.5")
(scan&parse "-0.5")

;Primitivas sobre números
(scan&parse "(1 + 1)") ;suma
(scan&parse "(1~1)")   ;resta
(scan&parse "(8/2)")   ;division
(scan&parse "(4*4)")   ;producto
(scan&parse "(6 mod 4)")
(scan&parse "add1(9)")
(scan&parse "sub1(10)")

;;_____________________________________________________________________________
;Numeros en base 32, hexadecimales, octales
;16 en hexadecimal es (0 1) porque ((0*16^0)+(1*16^1))= 16

;(16 (0 1)) = 16
;(16 (0 1)) = 16
;            -----
;             32
; 32 = (16 (0 2)) porque ((0*16^0)+(2*16^1))= 32
(scan&parse "(base 16 (0 1) + base 16 (0 1))")
(scan&parse "(base 16 (1 2) + base 16 (1 2))")
(scan&parse "(base 16 (1 5) + base 16 (1 5))")
(scan&parse "(base 8 (1 2) ~ base 8 (1 2)) ")
(scan&parse "(base 32 (1 2) * base 32 (1)) ")
(scan&parse "((base 16 (1 2) + base 16 (1 2)) + base 16 (1 2))")

;(16 (5 6 3))
(scan&parse "(base 16 (4 5 3) + base 16 (1 1))")

; (16 (1 2))
(scan&parse "add1( base 16 (32))")

;(16 (5 5 3))
(scan&parse "add1( base 16(4 5 3))")
(scan&parse "add1( base 16 (15))")
(scan&parse "sub1( base 16 (1))" )
(scan&parse "add1( base 16 (1))" )

;;_____________________________________________________________________________
;;print(x)
;;para prueba en el interprete se usa begin print("hola") ; print("mundo") end
(scan&parse "begin print(\"hola\") ; print(\"mundo\") end")

;String
;;para prueba en el interprete se usa longitud("hola")
(scan&parse "longitud(\"hola\")") ;4
(scan&parse "longitud(\"longevo\")") ;7
(scan&parse "(\"hola\" concat \"mundo\")") ;"holamundo"
(scan&parse "(\"6\" concat \"4\")") ;"64"

;;_____________________________________________________________________________
;;for
(scan&parse "for x = 1 to 5 do print(x) done")
(scan&parse "for x = 5 downto 1 do print(x) done")

;;_____________________________________________________________________________
;; expr-bool
(scan&parse "true")
(scan&parse "<(8,9)")
(scan&parse "or (<(1,0),>=(10,10))")
;;_____________________________________________________________________________
;; lista
(scan&parse "[1;or (<(1,0),>=(10,10))]")
(scan&parse "vacio?-lista([1;or (<(1,0),>=(10,10))])")
(scan&parse "set-lista([1;or (<(1,0),>=(10,10))],1,9)")
;;_____________________________________________________________________________
;; tuplas
(scan&parse "ref-tupla(crear-tupla(1, 2, 4,and (<(10,5),>(1,90))),3)")
(scan&parse "vacio?-tupla(tupla(1;or (<(1,0),>=(10,10))))")
(scan&parse "tupla?(vacio-tupla())")
(scan&parse "cabeza-tupla( cola-tupla(tupla(1;2;3;4;5)))")
;;_____________________________________________________________________________
;; registros
(scan&parse "crear-registro(f=5,t=4,ff=90)")
(scan&parse "{f=5;t=4;ff=90}")
(scan&parse "registros?({f=5;t=4;ff=90})")
(scan&parse "ref-registro({f=5;t=4;ff=90}, ff)")
;;_____________________________________________________________________________
;; variables y constantes
(scan&parse "variables
(y=3){bloque{y; actualizar y = 3; y}}")
;; --> 3
(scan&parse "constantes(y=3){bloque{y; actualizar y = 3; y
}}")
;; --> error: no se pueden actualizar las constantes
;;___________________________________________


;;___________________________________________
;; procedimientos (recursivos y no recursivos)
(scan&parse "variables (x = proc(a,b) {(a+b)}; y = 4) {invocar(x(9,y))}")
;; --> 13
(scan&parse "letrec fact(n) = if ==(n,0) then 1 else (n * invocar( fact( (n~1) ) ) ) endif {invocar(fact(5))}")
;; --> 120
;;___________________________________________
(interpretador)

;;_____________________________________________
;; clases
(scan&parse "clase perro hereda objeto campo azucar metodo ladrar(y){y} mostrar")
;; --> (#(struct:a-class-decl perro objeto (azucar) (#(struct:a-method-decl ladrar (y) #(struct:var-exp y)))))
(scan&parse "clase carro hereda objeto
  campo numRuedas
  campo marca
  metodo init (marca, numRuedas){variables(self.marca=marca; self.numRuedas=numRuedas){tupla(self.marca;self.numRuedas)}}
  metodo conducir(chofer) {chofer}
clase sedan hereda carro
  campo numPuertas
  metodo derrapar(chofer){super conducir(chofer)}
mostrar")
;; --> #(struct:a-program
;;  (#(struct:a-class-decl
;;     carro
;;     objeto
;;     (numRuedas marca)
;;     (#(struct:a-method-decl
;;        init
;;        (marca numRuedas)
;;        #(struct:variableLocal-exp
;;          (self.marca self.numRuedas)
;;          (#(struct:var-exp marca) #(struct:var-exp numRuedas))
;;          #(struct:tupla (#(struct:var-exp self.marca) #(struct:var-exp self.numRuedas)))))
;;      #(struct:a-method-decl conducir (chofer) #(struct:var-exp chofer))))
;;   #(struct:a-class-decl sedan carro (numPuertas) (#(struct:a-method-decl derrapar (chofer) #(struct:super-call-exp conducir (chofer))))))
;;  #(struct:mostrar-exp)
;;________________________________________________________________________________________________
;; crear objecto
(scan&parse "clase carro hereda objeto
  campo numRuedas
  campo marca
  metodo init (marca, numRuedas){bloque{actualizar self.marca= marca ;actualizar self.numRuedas=numRuedas}}
  metodo conducir(chofer) {chofer}
clase sedan hereda carro
  campo numPuertas
  metodo derrapar(chofer){super conducir(chofer)}
new carro(\"superCar\", 4)")
;#(struct:a-program
;  (#(struct:a-class-decl
;     carro
;     objeto
;     (numRuedas marca)
;     (#(struct:a-method-decl
;        init
;        (marca numRuedas)
;        #(struct:block-exp #(struct:updateVar-exp self.marca #(struct:var-exp marca)) (#(struct:updateVar-exp self.numRuedas #(struct:var-exp numRuedas)))))
;      #(struct:a-method-decl conducir (chofer) #(struct:var-exp chofer))))
;   #(struct:a-class-decl sedan carro (numPuertas) (#(struct:a-method-decl derrapar (chofer) #(struct:super-call-exp conducir (chofer))))))
;  #(struct:new-object-exp carro (#(struct:texto-lit "\"superCar\"") #(struct:numero-lit 4))))
;___________________________________________________________________________________________________________________________________
;; herencia
 (scan&parse "clase carro hereda objeto
  campo numRuedas
  campo marca
  metodo init (marca, numRuedas){bloque{actualizar self.marca= marca ;actualizar self.numRuedas=numRuedas}}
  metodo conducir(chofer) {chofer}
clase sedan hereda carro
  campo numPuertas
  metodo init (numPuertas, numRuedas){bloque{actualizar self.numPuertas= numPuertas ;actualizar self.numRuedas=numRuedas; actualizar self.marca=\"sedan\"}}
  metodo caracteristicas(){tupla(self.numPuertas;self.numRuedas)}
  metodo derrapar(chofer){super conducir(chofer)}
new sedan(2, 4)")