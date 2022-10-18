; IA01 - TD4 - Antoine GAJAN

; Représentation du labyrinthe sous forme de liste

(setq laby '((e 1) (1 2) (2 7) (3 6) (4 5) (5 4 12) (6 3 7) (7 6 8) (8 7 9) (9 8 10) (10 9 11 15) (11 10 12 14) (12 5 11) (13 20) (14 11) (15 10 16) (16 17) (17 18) (18 19) (19 20) (20 13 s) (s 20)))


; Fonction qui retourne les successeurs d'un état

(defun successeurs (laby etat)
     (cdr(assoc etat laby))
)

; Fonction qui retourne les successeurs valides d'un état

(defun successeurs-valides (etat laby chemin)
    ; On suppose d'abord que tous les successeurs sont valides
    ; Et on les stocke dans une variable
    (let ((valide (successeurs laby etat)))
	  ; Pour chaque successeur de valide
        (dolist (x valide valide)
            ; Si on a déjà validé l'etat
            (if (member x chemin)
                ; On le retire de la liste des successeurs valides
                (setq valide (remove x valide))
            )
        )
    )
)

; Fonction qui explore en profondeur un labyrinthe
(defun explore_prof (laby etat sortie chemin)
    ; Si etat = sortie:
    (if (equal etat sortie)
        chemin
        (progn
            (let
            ; Initialisation des variables successeurs_valides et solution
                ((valides (successeurs-valides etat laby chemin))(solution NIL))
                (setq chemin (cons etat chemin))
                ; Tant qu'il reste des successeurs validees
                (while (and valides (NOT solution))
                    ; On parcourt en profondeur le labyrinthe
                    (setq solution (explore_prof laby (pop valides) sortie chemin))
                    ; Si on a trouve la sortie du labirynthe, on ajoute l'état à solution

                    (if solution
                        (setq solution (cons etat solution))
                    )
                )
            ; Si on a trouve une solution
            (if solution
                ; Renvoie la solution trouvee
                solution
		; Sinon, on enleve l'etat du chemin et on retourne NIL
            (progn
                (setq chemin (remove etat chemin))
                ; Sinon, on retourne NIL
                NIL
            )
            ) 
              
            )
       )
    )
)

