;; RESOLUTION 1 : RECHERCHE DANS UN ESPACE D'ETATS

;; Liste des actions possibles
(setq actions '((16 3 2 1) (15 3 2 1) (14 3 2 1) (13 3 2 1) (12 3 2 1) (11 3 2 1) (10 3 2 1) (9 3 2 1) (8 3 2 1) (7 3 2 1) (6 3 2 1) (5 3 2 1) (4 3 2 1) (3 3 2 1) (2 2 1) (1 1)))

;; Fonction qui donne tous les successeurs d'un etat :
(defun successeurs (allumettes actions)
	(cdr (assoc allumettes actions))
)

;; Test de la fonction
(successeurs 16 actions) ;; renvoie (3 2 1)
(successeurs 5 actions) ;; renvoie (3 2 1)
(successeurs 2 actions) ;; renvoie (2 1)
(successeurs 1 actions) ;; renvoie (1)
(successeurs 16 NIL) ;; Aucun coup possible, renvoie NIL


;; EXPLORE 
;; Fonction qui effectue la recherche d'etat

(defun explore (allumettes actions joueur i) ;declaration de la fonction
	(cond 
		((and (eq joueur 'humain) (eq allumettes 0)) nil) ;si c'est l'humain qui joue et que le nombre d'allumettes = 0 renvoyer nil (l'IA a perdu)
   		((and (eq joueur 'IA) (eq allumettes 0)) t) ;si c'est l'IA qui joue et que le nombre d'allumettes = 0 renvoyer vrai (l'IA a gagne)
   
   		(t 
			(progn                             
				(let ((sol nil) (coups (successeurs allumettes actions))) ;on met sol a nil et coups = successeurs de l'etat actuel
					(while (and coups (not sol)) ;tant que la liste coup est non vide et que sol = nil
             				(progn
							;affichage du joueur qui est en train de jouer et du nombre d'allumettes qu'il reste
							(format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur (car coups) (- allumettes (car coups)))
							;on rappelle recursivement explore avec le nombre d'allumettes restant et l'autre joueur et on met le resultat dans sol
							(setq sol (explore (- allumettes (car coups)) actions (if (eq joueur 'IA) 'humain 'IA) (+ i 3)))
               
							(if sol           
								(setq sol (car coups))) ;si l'IA a gagne (sol = t) alors on affecte sol avec le 1er element de la liste coups (le coup gagnant)
							(format t "~%~V@t sol = ~s~%" i sol)
							(pop coups) ;on continue avec le reste de la liste coups
						)  
					)
				;; On renvoie le coup a jouer par l'IA, menant a une victoire
           			sol
				)
			)
		)
	)
) 
           
(defvar nbCoupsAJouer nil)

;on appelle la fonction avec differents nombres d'allumettes
(setq nbCoupsAJouer (explore 16 actions 'IA 0))
(setq nbCoupsAJouer (explore 8 actions 'IA 0))
(setq nbCoupsAJouer (explore 3 actions 'IA 0))


;; RESOLUTION 2 : INTELLIGENCE ARTIFICIELLE


(defun Randomsuccesseurs (actions) 
  (let ((r (random (length actions))))
    ;;(format t "~&~2t Res du random ~s~&" r)
    (nth r actions)))

;; Fonction qui renvoie le coup valide demandé par l'utilisateur
(defun JeuJoueur(allumettes actions)
    ;; Définition des variables
    (let ((coup NIL) (succ (successeurs allumettes actions)) (possible NIL))
        ;; Si la liste des successeurs est non nulle, on demande à l'utilisateur
        (if succ
            (progn
                (format t "~% Combien d'allumettes voulez-vous retirer ? ")
                (setq coup (read))
                (if (member coup succ)
                    (setq possible t)
                )
                ;; Tant que le coup n'est pas valide
                (while (not possible)
                    ;; On en redemande un et on regarde s'il est valide
                    (format t "Ce coup ne peut pas etre joue. Combien d'allumettes voulez-vous retirer ? ")
                    (setq coup (read))
                    (if (member coup succ)
                        (setq possible t)
                    )
                )
            )
        )
       
        ;; On retourne le coup
        coup
    )
)

;; Jeu de tests
(JeuJoueur 16 actions) ;; demande a l'utilisateur un nombre d'allumettes jusqu'a ce qu'il donne un nombre entre 1 et 3 (actions valides)
(JeuJoueur 2 actions) ;; demande a l'utilisateur un nombre d'allumettes jusqu'a ce qu'il donne un nombre entre 1 et 2 (seules actions valides)
(JeuJoueur 16 NIL) ;; retourne NIL sans rien demander à l'utilisateur car aucun coup possible


;; Déroulement d'une partie
(defun explore-renf(allumettes joueur actions)
    (if (AND (listp actions) actions (OR (eq joueur 'IA) (eq joueur 'humain)) (>= allumettes 0) (<= allumettes 16))
        (cond
            ;; Si l'humain gagne
            (
                (and (eq allumettes 0) (eq joueur 'humain)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%Vous avez gagne.")
                    NIL
                )
            )
            ;; Si l'IA gagne
            (
                (and (eq allumettes 0) (eq joueur 'IA)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%L'intelligence artificielle a gagne.")
                    actions
                )
            )
            ;; Sinon
            (t 
                ;; Creation des variables
                (let ((coup NIL))
                    ;; Affichage du nombre restant d'allumettes
                    (format t "~%~%Il reste ~s allumettes." allumettes)
                
                    ;; Si l'humain doit jouer
                    (if (eq joueur 'humain)
                        ;; Demande du coup a jouer et appel recursif
                        (progn
                            (setq coup (JeuJoueur allumettes actions))
                            (format t "~%Vous avez retire ~s allumettes." coup)
                            (explore-renf (- allumettes coup) 'IA actions)
                        )
                    
                        ;; Si l'IA doit jouer
                        (progn
                            ;; L'IA prend un coup random
                            (setq coup (randomSuccesseurs (successeurs allumettes actions)))
                            (format t "~%L'IA a retire ~s allumettes." coup)
                            (explore-renf (- allumettes coup) 'humain actions) 
                        )
                    )
                )
            )
        )
    )
)

;; Test de la fonction

(explore-renf 16 'IA actions) ;; L'IA commence à jouer, il reste 16 allumettes
(explore-renf 8 'Humain actions) ;; L'humain commence à jouer, il reste 8 allumettes
(explore-renf -1 'IA actions) ;; retourne NIL car -1 n'est pas un nombre d'allumettes valide
(explore-renf 13 'moi actions) ;; retourne NIL car "moi" n'est pas un joueur valide
(explore-renf 12 'IA NIL) ;; retourne NIL car actions est une liste vide


;; Fonction avec ajout du dernier coup gagnant de l'IA à la liste actions
(defun explore-renf(allumettes joueur actions dernier_coup_IA)
    (if (AND (listp actions) actions (OR (eq joueur 'IA) (eq joueur 'humain)) (>= allumettes 0) (<= allumettes 16))
        (cond
            ;; Si l'humain gagne
            (
                (and (eq allumettes 0) (eq joueur 'humain)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%Vous avez gagne.")
                    NIL
                )
            )
            ;; Si l'IA gagne
            (
                (and (eq allumettes 0) (eq joueur 'IA)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%L'intelligence artificielle a gagne.")
                    ;; Ajout du dernier coup a actions
                    (push (cadr dernier_coup_IA) (cdr(assoc (car dernier_coup_IA) actions)))
                    actions
                )
            )
            ;; Sinon
            (t 
                ;; Creation des variables
                (let ((coup NIL))
                    ;; Affichage du nombre restant d'allumettes
                    (format t "~%~%Il reste ~s allumettes." allumettes)
                
                    ;; Si l'humain doit jouer
                    (if (eq joueur 'humain)
                        ;; Demande du coup a jouer et appel recursif
                        (progn
                            (setq coup (JeuJoueur allumettes actions))
                            (format t "~%Vous avez retire ~s allumettes." coup)
                            (explore-renf (- allumettes coup) 'IA actions dernier_coup_IA)
                        )
                    
                        ;; Si l'IA doit jouer
                        (progn
                            ;; L'IA prend un coup random
                            (setq coup (randomSuccesseurs (successeurs allumettes actions)))
                            (setq dernier_coup_IA (list allumettes coup))
                            (format t "~%L'IA a retire ~s allumettes." coup)
                            (explore-renf (- allumettes coup) 'humain actions dernier_coup_IA) 
                        )
                    )
                )
            )
        )
    )
)

;; Test de la fonction

(explore-renf 16 'IA actions NIL) ;; L'IA commence à jouer, il reste 16 allumettes
(explore-renf 8 'Humain actions NIL) ;; L'humain commence à jouer, il reste 8 allumettes
(explore-renf -1 'IA actions NIL) ;; retourne NIL car -1 n'est pas un nombre d'allumettes valide
(explore-renf 13 'moi actions NIL) ;; retourne NIL car "moi" n'est pas un joueur valide
(explore-renf 12 'IA NIL NIL) ;; retourne NIL car actions est une liste vide


;; Fonction de renforcement
(defun renforcement(allumettes coup_gagnant actions)
    ;; Si les parametres sont valides
    (if (AND (listp actions) actions (>= allumettes 0) (<= allumettes 16))
        (let((liste (assoc allumettes actions)))
            ;; Si la liste est bien non vide
            (if liste
                (push coup_gagnant (cdr liste))
            )
            actions
        )
    )
)

;; Test de la fonction

(renforcement 16 2 actions) ;; Ajoute 2 dans les valeurs associees a la cle 16 de actions
(renforcement -1 2 actions) ;; renvoie NIL car -1 n'est pas un nombre d'allumettes valides
(renforcement 16 1 NIL) ;; renvoie NIL car actions = NIL

;; Fonction de jeu avec apprentissage par renforcement
(defun explore-renf-rec(allumettes joueur actions)
    (if (AND (listp actions) actions (OR (eq joueur 'IA) (eq joueur 'humain)) (>= allumettes 0) (<= allumettes 16))
        (cond
            ;; Si l'humain gagne
            (
                (and (eq allumettes 0) (eq joueur 'humain)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%Vous avez gagne.")
                    NIL
                )
            )
            ;; Si l'IA gagne
            (
                (and (eq allumettes 0) (eq joueur 'IA)) 
                (progn
                    (format t "~%~%Il reste 0 allumette.~%L'intelligence artificielle a gagne.")
                    actions
                )
            )
            ;; Sinon
            (t 
                ;; Creation des variables
                (let ((coup NIL) (sol NIL))
                    ;; Affichage du nombre restant d'allumettes
                    (format t "~%~%Il reste ~s allumettes." allumettes)
                    
                    ;; Si l'humain doit jouer
                    (if (eq joueur 'humain)
                        ;; Demande du coup a jouer et appel recursif
                        (progn
                            (setq coup (JeuJoueur allumettes actions))
                            (format t "~%Vous avez retire ~s allumettes." coup)
                            (explore-renf-rec (- allumettes coup) 'IA actions)
                        )
                        
                        ;; Si l'IA doit jouer
                        (progn
                            ;; L'IA prend un coup random
                            (setq coup (randomSuccesseurs (successeurs allumettes actions)))
                            (format t "~%L'IA a retire ~s allumettes." coup)
                            (setq sol (explore-renf-rec (- allumettes coup) 'humain actions))
                            ;; Si l'IA gagne
                            (if sol
                                ;; MAJ de actions en ajoutant le coup a actions
                                (renforcement allumettes coup actions)
                                ;; Si l'IA perd, on renvoie NIL
                                NIL
                            )
                        )
                    )
                )
            )
        )
    )
)

;; Test de la fonction

(explore-renf-rec 16 'IA actions) ;; L'IA commence à jouer, il reste 16 allumettes
(explore-renf-rec 8 'Humain actions) ;; L'humain commence à jouer, il reste 8 allumettes
(explore-renf-rec -1 'IA actions) ;; retourne NIL car -1 n'est pas un nombre d'allumettes valide
(explore-renf-rec 13 'moi actions) ;; retourne NIL car "moi" n'est pas un joueur valide
(explore-renf-rec 12 'IA NIL) ;; retourne NIL car actions est une liste vide