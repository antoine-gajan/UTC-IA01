;; =========================== PREMIERE PARTIE DU TD5 =================================


;; Premi�re m�thode : on devra it�rer sur la liste avec une boucle for
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

;; Deuxi�me m�thode : utilisation de assoc
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

;; Avec la premi�re m�thode
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

                 
;; Avec la deuxi�me m�thode
; Attention, r�gle sans conclusion
(defun numRegle2 (regle)
  (cadr regle)
)

(defun premisse2 (regle)
   (cadr regle)   
)

(defun cclRegle2 (regle)
  (car regle)
)

; Attention, r�gle sans conclusion
(defun regles-candidates2 (but base_regles)
  (cdr (assoc but base_regles))
)

;; Fonction r�cursive de chainage arri�re (utilisation m�thode 1)
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
             
             ;; Tant qu'il reste des r�gles candidates et 
             ;;que l'on n'a pas trouv� de solution
            (while (AND cand (NOT solution))
                ;; On prend une r�gle candidate
                (setq regle-actuelle (pop cand))
                ;; On suppose que l'on a une solution
                (setq solution t)
                ;; On trouve les pr�misses de la r�gle
                (setq premisses (premisse1 regle-actuelle))
                ;; On met � jour le chemin parcouru
                (setq chemin (cons regle-actuelle chemin))
                ;; On v�rifi� si toutes les pr�misses de la r�gle sont v�rifi�es
                (while (AND premisses solution)
                    (setq solution (chainage-arriere bdf bdr (pop premisses) chemin))
                )
                ;; Si on n'a pas trouv� de solution avec cette r�gle, on retire la r�gle du chemin
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
            (format t "~V@t But v�rifi� : ~A~%"i but)
            chemin         
        )
        
        ;; Sinon, initialisation des variables
        (let ((cand (regles-candidates1 but bdr))
             (solution NIL)
             (regle-actuelle NIL)
             (premisses NIL)
             (premisse-actuelle NIL)
             )
             
             ;; Tant qu'il reste des r�gles candidates et 
             ;;que l'on n'a pas trouv� de solution
            (while (AND cand (NOT solution))
                ;; On prend une r�gle candidate
                (setq regle-actuelle (pop cand))
                ;; On suppose que l'on a une solution
                (setq solution t)
                ;; On trouve les pr�misses de la r�gle
                (setq premisses (premisse1 regle-actuelle))
                ;; On met � jour le chemin parcouru
                (format t "~V@t J'essaie de prouver ~s : ~A => ~s ~%" (+ i 5) (numRegle1 regle-actuelle) (premisse1 regle-actuelle) but)
                (setq chemin (cons regle-actuelle chemin))
                ;; On v�rifi� si toutes les pr�misses de la r�gle sont v�rifi�es
                (while (AND premisses solution)
                    (setq premisse-actuelle (pop premisses))
                    (setq solution (chainage-arriere bdf bdr premisse-actuelle chemin (+ i 5)))
                )
                ;; Si on n'a pas trouv� de solution avec cette r�gle, on retire la r�gle du chemin
                (if (NOT solution)
                    (progn 
                        (setq chemin (remove regle-actuelle chemin))
                        (format t "~V@t Impossible de prouver ~s � partir de ~s ~%" (+ i 5) premisse-actuelle (numRegle1 regle-actuelle))
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
    ;; D�finition des variables
    (let ((operateur (car but)) (variable (cadr but)) (val2 (caddr but)) val1)
        ;; On trouve val1 � partir de la base de faits
        (setq val1 (cadr(assoc variable bdf)))
        (if val1
            ;; On �value si la condition entre les valeurs et l'op�rateur est correcte
            (funcall operateur val1 val2)
            ;; Sinon, on renvoie NIL
            NIL
        )
    )
)