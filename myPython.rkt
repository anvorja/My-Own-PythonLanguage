#lang eopl

;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos y recursion

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <un-programa (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>

;;                  ::= <expr-bool>
;;                      <boolean-expr (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expression> then <expression> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                      <letrec-exp proc-names idss bodies bodyletrec>
;;  <expr-bool>     ::= pred-prim ( expression , expression )
;;                      <comp-num (pred-prim rand1 rand2)>
;;                  ::= oper-bin-bool ( expr-bool , expr-bool )
;;                      <comp-bool-bin (oper-bin-bool rand1 rand2)>
;;                  ::= bool
;;                      <booleano-lit (datum)>
;;                  ::= oper-un-bool ( expr-bool)
;;                      <comp-bool-un (oper-un-bool rand)>
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
   ("@"letter (arbno (or letter digit "?"))) symbol)
  (bool
   ((or "true" "false")) symbol)
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
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression (txt)  texto-lit)
    (expression (identifier) var-exp)
    (expression (uni-primitive "(" expression ")") primapp-un-exp)
    (expression ("(" expression bi-primitive expression ")") primapp-bi-exp)
    (expression (expr-bool) boolean-expr)
    (expression ("Si" expression "entonces" expression "sino" expression "finSI") condicional-exp)
    (expression ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expression "finProc") procedimiento-ex)
    (expression ("declarar" "(" (separated-list identifier "=" expression ";") ")""{" expression "}") variableLocal-exp)
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) letrec-exp)
    ;;;;;;
    (expr-bool (pred-prim "(" expression "," expression ")") comp-num)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") comp-bool-bin)
    (expr-bool (bool) booleano-lit)
    (expr-bool (oper-un-bool "(" expr-bool ")") comp-bool-un)
    ;;;;;;
    (bi-primitive ("+") primitiva-suma)
    (bi-primitive ("~") primitiva-resta)
    (bi-primitive ("*") primitiva-multi)
    (bi-primitive ("/") primitiva-div)
    (bi-primitive ("concat") primitiva-concat)
    ;;;;;;
    (uni-primitive ("add1") primitiva-add1)
    (uni-primitive ("sub1") primitiva-sub1)
    (uni-primitive ("longitud") primitiva-longitud)
    (pred-prim ("<") prim-bool-menor)
    (pred-prim (">") prim-bool-mayor)
    (pred-prim ("<=") prim-bool-menor-igual)
    (pred-prim (">=") prim-bool-mayor-igual)
    (pred-prim ("==") prim-bool-equiv)
    (pred-prim ("<>") prim-bool-diff)
    (oper-bin-bool ("and") prim-bool-conj)
    (oper-bin-bool ("or") prim-bool-disy)
    (oper-un-bool ("not") prim-bool-neg)
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
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
 (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

; eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (datum) datum)
      (texto-lit (datum) (substring datum 1 (-(string-length datum) 1)))
      (var-exp (id) (apply-env  env id))
      (primapp-un-exp (prim rand)
          (let ((arg (eval-rand rand env)))
               (apply-uni-primitive prim arg)))
      (primapp-bi-exp (rator prim rand)
          (let((arg1 (eval-rator rator env))
               (arg2 (eval-rand rand env)))
               (apply-bi-primitive arg1 prim arg2)))

      (boolean-expr (datum) (eval-bool-exp datum env))
      (condicional-exp (test-exp true-exp false-exp)
          (if (valor-verdad? (eval-expression test-exp env))
            (eval-expression true-exp env)
            (eval-expression false-exp env)))
      (procedimiento-ex (ids cuerpo)
          (cerradura ids cuerpo env))
      (app-exp (rator rands)
               (let((proc (eval-expression rator env))
                    (args (eval-rands rands env)))
                 (if(procVal? proc)
                    (apply-procedure proc args)
                    (eopl:error 'eval-expression
                                "Attemp to apply non-procedure ~s" proc))))
      (variableLocal-exp (ids values body)
                         (let((args (eval-rands values env)))
                           (eval-expression body (extend-env ids args env))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (else 1))))


;;funcion auxiliar que evalua expresiones booleanas
(define eval-bool-exp
  (lambda (expr-boolean env)
    (cases expr-bool expr-boolean
      (comp-num (pred-prim rand1 rand2)
                (let ((arg1 (eval-expression rand1 env))
                      (arg2 (eval-expression rand2 env)))
                  (eval-comp-num pred-prim arg1 arg2)))
      (comp-bool-bin (oper-bin-bool rand1 rand2)
                     (let ((arg1 (eval-bool-exp rand1 env))
                           (arg2 (eval-bool-exp rand2 env)))
                          (eval-comp-bool-bin oper-bin-bool arg1 arg2)))
      (booleano-lit (datum) datum)
      (comp-bool-un (oper-unario-bool rand)
                    (let ((arg1 (eval-bool-exp rand env)))
                          (cases oper-un-bool oper-unario-bool
                            (prim-bool-neg () (not arg1)))
                      )))))


;;funcion auxiliar recibe una <pred-prim>  y dos argumentos ya evaluados, les aplica el respectivo predicado.
(define eval-comp-num
  (lambda (pred-primitive arg1 arg2)
    (cases pred-prim pred-primitive
      (prim-bool-menor () (< arg1 arg2))
      (prim-bool-mayor () (> arg1 arg2))
      (prim-bool-menor-igual () (<= arg1 arg2))
      (prim-bool-mayor-igual () (>= arg1 arg2))
      (prim-bool-equiv () (equal? arg1 arg2))
      (prim-bool-diff () (not (equal? arg1 arg2))))))

;;funcion auxiliar recibe un <oper-bin-bool >  y dos argumentos ya evaluados, les aplica el respectivo predicado.
(define eval-comp-bool-bin
  (lambda (oper-binario arg1 arg2)
    (cases oper-bin-bool oper-binario
      (prim-bool-conj () (and arg1 arg2))
      (prim-bool-disy () (or arg1 arg2))
      )))
; funciones auxiliares para aplicar eval-expression a cada elemento de una
; lista de operandos (expresiones)
(define eval-rators
  (lambda (rators env)
    (map (lambda (x) (eval-rators x env)) rators)))

(define eval-rator
  (lambda (rator env)
    (eval-expression rator env)))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-bi-primitive: <expression> <primitiva-binaria> <expression> -> numero|cadena
(define apply-bi-primitive
  (lambda (arg1 prim arg2)
    (cases bi-primitive prim
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
    )))

;apply-uni-primitive: <primitiva-unaria> <expression> -> numero
(define apply-uni-primitive
  (lambda (prim args)
    (cases uni-primitive prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-longitud (string-length args)))))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero.
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
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

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

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
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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

;******************************************************************************************
;;pruebas

;;___________________________________________
;; expr-bool
(scan&parse "true")
(scan&parse "<(8,9)")
(scan&parse "or (<(1,0),>=(10,10))")