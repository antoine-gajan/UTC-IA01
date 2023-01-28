;; TD12 - Perceptron

(setq Chiffres '((0 (1 1 1 1 1 1 1 0) 0)
                 (1 (1 0 1 1 0 0 0 0) 1)
                 (2 (1 1 1 0 1 1 0 1) 0)
                 (3 (1 1 1 1 1 0 0 1) 1)
                 (4 (1 0 1 1 0 0 1 1) 0)
                 (5 (1 1 0 1 1 0 1 1) 1)
                 (6 (1 0 0 1 1 1 1 1) 0)
                 (7 (1 1 1 1 0 0 0 0) 1)
                 (8 (1 1 1 1 1 1 1 1) 0)
                 (9 (1 1 1 1 0 0 1 1) 1)))

(setq Poids '(1 1 1 1 1 1 1 1))

;; Fonction calculant la valeur interne d'un neurone
(defun valeur-interne (entrees poids)
  ;; Vérifie que la taille des entrées et des poids est la même
  (if (eq (length entrees) (length poids))
    (let ((val_interne 0) (e NIL) (p NIL))
      ;; Calcul de la valeur interne
      (while (and entrees poids)
        (setq e (pop entrees))
        (setq p (pop poids))
        (setq val_interne (+ val_interne (* e p)))
      )
      val_interne
    )
  )
)

;; Fonction qui calcule la sortie en fonction des entrées et des poids
(defun sortie (entrees poids)
  (if (> (valeur-interne entrees poids) 0)
    1
    0
  )
)

;; Fonction de mise à jour d'un neurone
(defun mise-a-jour (entrees poids sortie_calculee sortie_attendue)
  (let ((nouv_poids NIL))
    ;; Pour chaque poids, MAJ
    (dolist (w poids)
      (push (+ w (*(- sortie_attendue sortie_calculee) (pop entrees))) nouv_poids)
    )
    ;; On retourne le nouveau poids
    nouv_poids
  )
)

;; Fonction permettant l'apprentissage par correction d'erreur
(defun apprentissage (donnees poids)
  ;; Pour chaque donnée
  (dolist (donnee donnees)
    (setq poids (mise-a-jour (cadr donnee) poids (sortie (cadr donnee) poids) (caddr donnee)))
    ;; Affichage pour vérifier suite aux itérations
    (format t "~a~%" poids)
  )
  poids
)

;; Pour tester
(apprentissage Chiffres Poids) ;; renvoie (-1 0 -2 0 1 0 1 1)
