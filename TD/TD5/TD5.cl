;; =========================== PREMIERE PARTIE DU TD5 =================================


;; Première méthode : on devra itérer sur la liste avec une boucle for
(setq base_regles_1
      (
       (F (B D E) R1)
       (A (D G) R2) 
       (A (C F) R3)
       (A (X C) R8)
       (D (C) R4)
       (E (D) R5)
       (H (A) R6)
       (X (B) R7)
      )
)

;; Deuxième méthode : utilisation de assoc
(setq base_regles_2
      (
       (F ((B D E) R1))
       (A ((D G) R2) ((C F) R3)) ((X C) R8)
       (D (C) R4)
       (E (D) R5)
       (H (A) R6)
       (X (B) R7)
      )
)

;; Avec la première méthode
(defun numRegle1 (regle)
  (caddr regle)
)

(defun premisse1 (regle)
  (cadr regle)   
)

(defun cclRegle1 (regle)
  (car regle)
)

(defun regles-candidates1 (but base_regles)
  (if  base_regles
      (if (equal (cclRegle1 (car base_regles) but))
        (cons (car base_regles) (regles-candidates but (cdr base_regles)))
        (regles-candidates1 but (cdr base_regles))
      )
   )
)

                 
;; Avec la deuxième méthode
; Attention, règle sans conclusion
(defun numRegle2 (regle)
  (cadr regle)
)

(defun premisse2 (regle)
   (cadr regle)   
)

(defun cclRegle2 (regle)
  (car regle)
)

; Attention, règle sans conclusion
(defun regles-candidates2 (but base_regles)
  (cdr (assoc but base_regles))
)

;; Fonction récursive de chainage arrière (utilisation méthode 1)
(defun chainage-arriere (bdf bdr but chemin)
    ;; Si le but est dans la base de faits, on renvoie le chemin
    (if (member but bdf)
        chemin
        ;; Sinon, initialisation des variables
        (let ((cand (regles-candidates1 but bdr))
             (solution NIL)
             (regle-actuelle NIL)
             (premisses NIL)
             )
             
             ;; Tant qu'il reste des règles candidates et 
             ;;que l'on n'a pas trouvé de solution
            (while (AND cand (NOT solution))
                ;; On prend une règle candidate
                (setq regle-actuelle (pop cand))
                ;; On suppose que l'on a une solution
                (setq solution t)
                ;; On trouve les prémisses de la règle
                (setq premisses (premisse1 regle-actuelle))
                ;; On met à jour le chemin parcouru
                (setq chemin (cons regle-actuelle chemin))
                ;; On vérifié si toutes les prémisses de la règle sont vérifiées
                (while (AND premisses solution)
                    (setq solution (chainage-arriere bdf bdr (pop premisses) chemin))
                )
                ;; Si on n'a pas trouvé de solution avec cette règle, on retire la règle du chemin
                (if (NOT solution) 
                    (setq chemin (remove regle-actuelle chemin))
                )
            )
            ;; On renvoie le chemin
            solution
        )
    )
)


;; Pour tester : (chainage-arriere '(C) base_regles_1 'A NIL) renvoie NIL
;; (chainage-arriere '( B C D) base_regles_1 'A NIL) renvoie les chemins


;; Avec un affichage
(defun chainage-arriere (bdf bdr but chemin &optional(i 0))
    ;; Si le but est dans la base de faits, on renvoie le chemin
    (if (member but bdf)
        (progn
            (format t "~V@t But vérifié : ~A~%"i but)
            chemin         
        )
        
        ;; Sinon, initialisation des variables
        (let ((cand (regles-candidates1 but bdr))
             (solution NIL)
             (regle-actuelle NIL)
             (premisses NIL)
             (premisse-actuelle NIL)
             )
             
             ;; Tant qu'il reste des règles candidates et 
             ;;que l'on n'a pas trouvé de solution
            (while (AND cand (NOT solution))
                ;; On prend une règle candidate
                (setq regle-actuelle (pop cand))
                ;; On suppose que l'on a une solution
                (setq solution t)
                ;; On trouve les prémisses de la règle
                (setq premisses (premisse1 regle-actuelle))
                ;; On met à jour le chemin parcouru
                (format t "~V@t J'essaie de prouver ~s : ~A => ~s ~%" (+ i 5) (numRegle1 regle-actuelle) (premisse1 regle-actuelle) but)
                (setq chemin (cons regle-actuelle chemin))
                ;; On vérifié si toutes les prémisses de la règle sont vérifiées
                (while (AND premisses solution)
                    (setq premisse-actuelle (pop premisses))
                    (setq solution (chainage-arriere bdf bdr premisse-actuelle chemin (+ i 5)))
                )
                ;; Si on n'a pas trouvé de solution avec cette règle, on retire la règle du chemin
                (if (NOT solution)
                    (progn 
                        (setq chemin (remove regle-actuelle chemin))
                        (format t "~V@t Impossible de prouver ~s à partir de ~s ~%" (+ i 5) premisse-actuelle (numRegle1 regle-actuelle))
                    )
                )
            )
            ;; On renvoie le chemin
            solution
        )
    )
)


;; =========================== SUITE DU TD5 =================================

;; Ordre 0+

(setq basederegles '((R1 (> d 5) (eq moyen voiture))
(R2 ((>= d 1) (< temperature 15)) (eq moyen voiture))
(R3 ((<= d 5) (>= temperature 15)) (eq moyen a-pieds))
(R4 ((eq moyen voiture) (eq cinema ville))(eq action taxi))
(R5 ((eq moyen voiture) (neq cinema ville)) (eq action voiture-perso))
(R6 ((eq moyen a-pieds)(eq temps mauvais))(eq action a-pieds-avec-impermeable))
(R7 ((eq moyen a-pieds) (eq temps beau))(eq action promenade))
))

(setq basedefaits '((temps mauvais)(temperature 5) (d 6)))

(setq but '(eq action promenage))


;; Fonction qui regarde si un but est dans la base de faits
(defun appartient (but bdf)
    ;; Définition des variables
    (let ((operateur (car but)) (variable (cadr but)) (val2 (caddr but)) val1)
        ;; On trouve val1 à partir de la base de faits
        (setq val1 (cadr(assoc variable bdf)))
        (if val1
            ;; On évalue si la condition entre les valeurs et l'opérateur est correcte
            (funcall operateur val1 val2)
            ;; Sinon, on renvoie NIL
            NIL
        )
    )
)