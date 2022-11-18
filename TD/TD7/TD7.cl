;; TD7 - Recherche informée

(setq laby '((A1 A2 B1) (A2 A3 A1) (A3 A4 A2) (A4 B4 A3) (B1 C1 A1) (B2) (B3) (B4 C4 A4) (C1 C2 D1 B1) (C2 C3 D2 C1) (C3 C4 C2) (C4 D4 C3 B4) (D1 D2 C1) (D2 D1 C2) (D3) (D4 C4)))

(defun successeurs (laby etat)
	(cdr (assoc etat laby))
)

(defun successeurs-valides (laby etat chemin)
	(let ((valides NIL) (succ (successeurs laby etat)))
		(dolist (x succ)
			(if (not(member x chemin))
				(push x valides)
			)
		)
		valides
	)
)

(defun x (etat)
	(- (char-int(char(symbol-name etat)0))64)
)

(defun y (etat)
	(- (char-int(char(symbol-name etat)1))48)
)

(defun distance_manhattan (etat etat_final)
	(+ (abs (- (x etat) (x etat_final)))
	   (abs (- (y etat) (y etat_final)))
	)
)

(defun tri (liste)
	;; On trie la liste en mettant le plus petit d'abord avec comme élément à comparer le deuxième élément de la liste
	(stable-sort liste #'< :key #'cadr)
)

(defun algo_glouton (laby entree sortie chemin)
	;; On ajoute l'entrée au chemin parcouru
	(push entree chemin)
	
	;; Si entree = sortie, on a trouvé la solution
	(if (eq entree sortie)
		;; On retourne le chemin
		(reverse chemin)
		;; Sinon, on regarde quel successeur appeler
		(progn
		(let ((succ (successeurs-valides laby entree chemin)) (liste_trie NIL) (solution NIL))
			;; Calcul des distances qu'on ajoute dans une liste
			(dolist (x succ)
				(push (list x (distance_manhattan x sortie)) liste_trie)
			)
			;; On trie les successeurs par ordre de distance
			(setq liste_trie (tri liste_trie))
			;; Tant qu'on n'a pas trouvé de solution, on continue
			(while (and liste_trie (not solution))
				(setq solution (algo_glouton laby (caar liste_trie) sortie chemin))
				;; On retire le successeur traite de la liste
				(pop liste_trie)
			)
			;; On retourne la solution trouvée
			solution
		)
		)
	)
)

;; Test de la fonction
(algo_glouton laby 'B1 'A4 NIL)
(algo_glouton laby 'D2 'B4 NIL)


(defun algo_A* (laby entree etat_actuel sortie chemin)
	;; On ajoute l'état actuel au chemin parcouru
	(push etat_actuel chemin)
	
	;; Si entree = sortie, on a trouvé la solution
	(if (eq etat_actuel sortie)
		;; On retourne le chemin
		(reverse chemin)
		;; Sinon, on regarde quel successeur appeler
		(progn
		(let ((succ (successeurs-valides laby etat_actuel chemin)) (liste_trie NIL) (solution NIL))
			;; Calcul des distances qu'on ajoute dans une liste
			(dolist (x succ)
				(push (list x (+ (distance_manhattan x sortie) (distance_manhattan entree x))) liste_trie)
			)
			;; On trie les successeurs par ordre de distance
			(setq liste_trie (tri liste_trie))
			;; Tant qu'on n'a pas trouvé de solution, on continue
			(while (and liste_trie (not solution))
				(setq solution (algo_A* laby entree (caar liste_trie) sortie chemin))
				;; On retire le successeur traite de la liste
				(pop liste_trie)
			)
			;; On retourne la solution trouvée
			solution
		)
		)
	)
)

;; Test
(algo_A* laby 'B1 'B1 'B4 NIL)
(algo_Aµ laby 'D2 'D2 'B4 NIL)


;; Solution de l'algorithme glouton par le prof en itératif
;;Init :
;;	file = (depart + dist_man debut sortie)
;;	chem = NIL
;;	succ = NIL
;;Tq sortie n'est pas dans file:
;;	Ajouter le 1er point sans le cout de file dans chemin
;;	Chercher les successeurs du 1er point de file (successeursValides)
;;	Pour chaque succ(s)
;;		etatCout = (S+ distMan)
;;		Ajouter etatCout à file
;;	Sort by file
;;Renvoyer chem

