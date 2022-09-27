; Fonction qui permet de dériver une expression mathématique
(defun deriv(expression variable)
    (cond
        ; si l'expression est un nombre, on retourne 0
        ((numberp expression)
            0
        )
        ; si l'expression est un symbole
        ((symbolp expression)
            ; si c'est la variable, on retourne 1, sinon on retourne 0
            (if (equal expression variable)
                1
                0
            )
        )
        ; si c'est une liste, on regarde l'opérateur
        
        ;si c'est un +, la dérivée de f + g est f' + g'
        ((equal (car expression) '+)
            (list '+ (deriv (cadr expression) variable) (deriv (caddr expression) variable))
        )
        ; si c'est un -, la dérivée de f - g est f' - g'
        ((equal (car expression) '-)
            (list '- (deriv (cadr expression) variable) (deriv (caddr expression) variable))
        )
        ; si c'est un *, la dérivée de f * g est f'g + g'f
        ((equal (car expression) '*)
            (list '+ 
                (list '* (deriv (cadr expression) variable) (caddr expression)) 
                (list '* (deriv (caddr expression) variable) (cadr expression))   
            )
        )
        ; si c'est un /, la dérivée de f/g est (f'g - g'f) / g²
        ((equal (car expression) '/)
            (list '/ 
                (list'- 
                    (list '* (deriv (cadr expression) variable) (caddr expression))
                    (list '** (caddr expression) 2)    
                )
            )
        )
        ; si c'est un **, la dérivée de u^n est n * u' * u^(n-1)
        ((equal (car expression) '**)
            (list '*  
                (caddr expression)
                (list '* 
                    (deriv (cadr expression) variable)
                    (list '** (cadr expression) (list '- (caddr expression) 1))    
                )
            )
        )        
    )
)

; Fonction pour simplifier une expression
(defun simplifier (expression)
    ; si l'expression est un atome, on retourne l'expression
    (if (atom expression)
        expression
        
    ; si l'expression contient 1 opérateur et 2 nombres
    (if (AND (numberp(cadr expression)) (numberp (caddr expression)))
        ; on évalue l'expression
        (eval expression)
        ; sinon, appel récursif pour évaluer les opérandes
        (list (car expression) (simplifier (cadr expression)) (simplifier (caddr expression)))
    )
    )
)