;; Question 1


;; Un comte et un cadet de Gascogne sont des nobles.
;; De Guiche est un comte. Cyrano et Christian sont des cadets de Gascogne. 
;; Roxane est une Mondaine. 
;; Il est impossible qu'une mondaine soit une cadet de Gascogne. 
;; Roxane est prête à aider un cadet de Gascogne.
;; Roxane aime Christian. Christian, De Guiche et Cyrano aiment Roxane. ;;Cyrano et Christian appartiennent aux soldats du régiment de De Guiche. 
;; Le nez de Cyrano est de taille énorme.


;; Question 2

;; On marque M1 sur le sommet Noble et M2 sur le sommet Roxane. On propage le marqueur M1 à tous les sommets issus d'une relation is-a. On a donc les sommets Comte et Cadet de Gascogne marqués. On précède ainsi de suite sur les arcs is-a. On regarde alors quels sont les sommets qui ont la marque M1 avec une relation "aime" vers un sommet marqué M2. On s'aperçoit donc Cyrano, Christian et de Guiche qui sont aimés de Roxane.

;; Question 3


;; Fonction pour créer un noeud
(defun defnode(id nom type nodes)
	(push 
		(list id 
			(cons 'type type) 
			(cons 'nom nom) 
			(list 'arcs-in) 
			(list 'arcs-out) 
			(list 'marks)
		) 
	nodes)
)

(setq nodes NIL)
(setq nodes (defnode 'Nobles 'Nobles 'concept nodes))
(setq nodes (defnode 'Comte 'Comte 'concept nodes))
(setq nodes (defnode 'CadetDeGascogne 'CadetDeGascogne 'concept nodes))
(setq nodes (defnode 'Cyrano 'Cyrano 'instance nodes))
(setq nodes (defnode 'Christian 'Christian 'instance nodes))
(setq nodes (defnode 'Mondaine 'Mondaine 'concept nodes))
(setq nodes (defnode 'Roxane 'Roxane 'instance nodes))
(setq nodes (defnode 'DeGuiche 'DeGuiche 'instance nodes))




;; Fonction pour ajouter un arc à la liste des arcs
(defun defarc(id arc_dep type arc_fin arcs nodes)
	;; Ajout de l'arc à la liste des arcs
	(push
		(list id
			(cons 'type type)
			(cons 'from-node arc_dep)
			(cons 'to-node arc_fin)
		)
	arcs)
	;; Modification / Ajout des id des noeuds qui présents dans les arcs à la liste nodes
	(push id (cdr(assoc 'arcs-out (cdr(assoc arc_dep nodes)))))
	(push id (cdr(assoc 'arcs-in (cdr(assoc arc_fin nodes)))))
	;; Retourne la liste arcs
	arcs 
)

(setq arcs NIL)
(setq arcs (defarc 'A0 'Comte 'is-a 'Nobles arcs nodes))
(setq arcs (defarc 'A1 'CadetDeGascogne 'is-a 'Nobles arcs nodes))
(setq arcs (defarc 'A2 'Cyrano 'is-a 'CadetDeGascogne arcs nodes))
(setq arcs (defarc 'A3 'Christian 'is-a 'CadetDeGascogne arcs nodes))
(setq arcs (defarc 'A4 'DeGuiche 'is-a 'Comte arcs nodes))
(setq arcs (defarc 'A5 'Roxane 'is-a 'Mondaine arcs nodes))
(setq arcs (defarc 'A6 'Cyrano 'aime 'Roxane arcs nodes))
(setq arcs (defarc 'A7 'Christian 'aime 'Roxane arcs nodes))
(setq arcs (defarc 'A8 'DeGuiche 'aime 'Roxane arcs nodes))
(setq arcs (defarc 'A9 'Roxane 'aime 'Christian arcs nodes))


;; Questions optionnelles

;; Algo wave : progage une marque le long des arcs d’un certain type, dans le sens direct ou contraire

;;Fonction wave(noeud_depart marqueur type_arc arcs sens nodes) :
;;	Si noeud non marqué avec marqueur :
;;		Marquer le noeud noeud_depart

;;	Si sens = indirect :
;;		arcs = (predecesseurs noeud_depart nodes) ;; tous les arcs-in
;;	Sinon 
;;		arcs = (sucesseurs noeud_depart nodes) ;; tous les arcs-out

;;	Pour chaque arc dans arcs :
;;		info_arc = (cdr(assoc arc arcs))
;;		type = (cdr(assoc type info_arc))
;;		Si type = type_arc :
;;			Si sens = indirect :
;;				nouv_noeud = (cdr(assoc from-node info_arc))
;;			Sinon :
;;				nouv_noeud = (cdr(assoc to-node info_arc))
;;		wave(nouv_noeud marqueur type_arc arcs sens nodes)
	 

;;Fonction predecesseurs(noeud nodes) :
;;	(cdr(assoc arcs-in (cdr(assoc noeud nodes))))

;;Fonction successeurs(noeud nodes) :
;;	(cdr(assoc arcs-out (cdr(assoc noeud nodes))))

;; Solution prof : predecesseur renvoie tous les noeuds predecesseurs
;;Fonction predecesseur(noeud type arcs nodes) :
;;	pred = NIL
;;	arcs_in = (cdr(assoc arcs-in (cdr(assoc noeud nodes))))
;;	Pour tous les arcs de arcs-in :
;;		Si type de arc = type :
;;			(push(cdr(assoc from-node (cdr(assoc arc arcs)))) pred)	

;; Renvoie true si le noeud est marqué par le marqueur
(defun est_marque(noeud marqueur nodes)
	(if (member marqueur (cdr(assoc 'marks (cdr (assoc noeud nodes)))))
		t
		NIL
	)
)

;; Fonction pour marquer un noeud
(defun marquer_noeud (noeud marqueur nodes)
	(push marqueur (cdr (assoc 'marks (cdr (assoc noeud nodes)))))
)

;; Fonction qui renvoie les arcs ayant pour terminaison le noeud
(defun predecesseurs (noeud nodes)
	(cdr(assoc 'arcs-in (cdr(assoc noeud nodes))))
)

;; Fonction qui renvoie les arcs ayant pour origine le noeud
(defun successeurs (noeud nodes)
	(cdr(assoc 'arcs-out (cdr(assoc noeud nodes))))
)

(defun type_arc (arc arcs)
	(cdr (assoc 'type (cdr (assoc arc arcs))))
)

;; Fonction récursive pour marquer les sommets
(defun wave(noeud_depart marqueur type arcs sens nodes)
	;; Si le noeud n'est pas encore marqué, on le marque
	(if (not (est_marque noeud_depart marqueur nodes))
		;; On marque le noeud
		(marquer_noeud noeud_depart marqueur nodes)
	)
	(let ((liste_arcs NIL) (nouv_noeud NIL))
		;; On regarde le sens
		(if (eq sens 'indirect)
			;; Si sens indirect, on prend les prédecesseurs
			(setq liste_arcs (predecesseurs noeud_depart nodes))
			;; Si sens direct, on prend les successeurs
			(setq liste_arcs (successeurs noeud_depart nodes))
		)
		;; Pour chaque arc
		(dolist (arc liste_arcs)
			;; Si le type correspond à celui recherché
			(if (eq (type_arc arc arcs) type)
				;; On prend le noeud (origine ou terminaison) qui nous intéresse
				(progn
					(if (eq sens 'indirect)
						(setq nouv_noeud (cdr(assoc 'from-node  (cdr(assoc arc arcs)))))
						(setq nouv_noeud (cdr(assoc 'to-node  (cdr(assoc arc arcs)))))
					)
					;; Appel récursif sur le nouveau noeud
					(wave nouv_noeud marqueur type arcs sens nodes)
				)
			)
		)
	)
)


;; On met les marques pour répondre à la question
(wave 'Nobles 'M1 'IS-A arcs 'indirect nodes)
(wave 'Roxane 'M2 'aime arcs 'indirect nodes)

;;Fonction getResult(M1 M2 type arcs) :
;;	results = NIL
;;	Pour chaque arc dans arcs :
;;		Si type de arc = type et arcs-in marqué M1 et arc-out marqué M2 :
;;			(push toNode results)


(defun getResult (M1 M2 type arcs nodes)
	;; Variable contenant les résultats
	(let ((results NIL) (pred NIL) (succ NIL))
		;; Pour chaque arc de la liste
		(dolist (arc arcs)
			;; MAJ du pred et du succ de l'arc
			(setq pred (cdr(assoc 'from-node (cdr arc))))
			(setq succ (cdr(assoc 'to-node (cdr arc))))
			;; Si l'origine est marqué par M1 et la terminaison par M2, on ajoute à la liste results
			(if (AND (eq (cdr (assoc 'type (cdr arc))) type) (est_marque pred M1 nodes) (est_marque succ M2 nodes))
				(push pred results)
			)
		)
		;; On renvoie la liste résultat
		results
	)
)

;; On récupère les résultats de la requête
(getResult 'M1 'M2 'aime arcs nodes)