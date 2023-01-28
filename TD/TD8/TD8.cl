;; TD8 - Frames


;; Rappel du cours
;; 1 frame
;; 	-> slots
;; 		-> 1 ou des facettes
;;			-> value
;;			-> default
;;			-> démons (= fonction qu'on appelle)



;; Question 1 - Construire la représentation


;; Variable globale
(setq frames NIL)

;; Appel de make-individu
;;(make-individu 'ELEPHANT '(NAME "Clyde" COLOR "grey" AGE 5))
;;(make-individu 'Clyde 'ELEPHANT frames '(COLOR "grey" AGE 5))

;; Données initiales

(setq Etre
	'(Etre
		(type (value concept)))
)

(setq ELEPHANT
	'(ELEPHANT
		(TYPE (VALUE CONCEPT))
		(IS-A (VALUE ETRE))
		(COLOR (DEFAULT "grey"))
		(AGE (IF-NEEDED ask-user)
		     (IF-ADDED check-age))
		(POIDS (IF-NEEDED computer-weight-from-age))
		(AFFICHAGE (IF-ADDED draw-elephant)
			     (IF-REMOVED erase-elephant))))
(pushnew 'ELEPHANT frames) ;; pushnew ajoute l'élément s'il n'est pas dans la liste
(pushnew 'ETRE frames)


;; Demons nécessaires pour la suite
(defun check-age(slot value)
	(if (and (>= value 0) (<= value 80))
		value
	(progn
		(print "error age")
		NIL
	)
	)
)


(defun computer-weight-from-age(value)
	;; Le poids d'un éléphant est de 10 fois sa taille
	(* 10 value)
)


;; Ma proposition
;;Fonction make-individu (nom concept informations) :
;;	INIT :
;;		individu = NIL	
;;	INSTANCIATION DE L'INDIVIDU
;;	(push nom individu)
;;	(push (list 'type concept) individu)
;;	Tant que informations != NIL :
;;		info = (prop val)
;;		facettes = get_facettes(concept slot)
;;		Si prop n'est pas dans individu :
;;			Si if-added dans facettes :
;;				Si démon associé renvoie ok :
;;					(push (list prop val) individu)
;;		informations = (cddr informations)
		
				
;; Solution
;;Fonction make-individu (name concept frames &caract) :
;;	Si concept n'est pas dans frame :
;;		Erreur
;;	Sinon :
;;		Créer identifiant nameXX 	(frame(gentemp(symbol-name concept)))
;;		Construire début frame	frame <- (name(type(value individu)) (is-a(value concept))
;;		Tant que caract != NIL:
;;			slot <- (pop caract)
;;			value <- (pop caract)
;;			Si slot autorisé et qu'il n'existe pas déjà :
;;				demon <- (getDemon concept slot)
;;				Si demon :
;;					value <- (funcall demon slot value)
;;					Si value :
;;						Créer le slot et l'jaouter dans frame
;;				Sinon :
;;						Créer le slot et l'jaouter dans frame
;;		Ajouter frame dans frames


;; Code de make-individu
(defun make-individu (nom concept frames proprietes)
	;; Si le concept est dans frames
	(if (assoc concept frames)
		(progn
			(let ((individu NIL))
			;; Création de l'identifiant unique
			(push (gentemp (symbol-name nom)) individu)
			;; Début de liste commun à tous
			(push (list 'type (list 'value 'instance)) individu)
			(push (list 'is-a (list 'value concept)) individu)
			;; Tant qu'il y a des propriétés à ajouter
			(while proprietes
				(let* ((slot (pop proprietes)) (value (pop proprietes)))
					; Si le slot est valide
					(if (member slot (getSlotsAllowed concept frames))
						;; Si présence de démons
						(if (getDemon concept slot frames)
							;; On vérifie que la valeur est correcte
							(if (funcall (getDemon concept slot frames) slot value)
								;; Si oui, on ajoute
								(push (list slot (list 'value value)) individu)
							)
							;; Si pas de démon, on ajoute directement
							(push (list slot (list 'value value)) individu)
						)
					)
				)
			)
			;; On ajoute l'individu dans la base de données frames
			(push (reverse individu) frames)
			)
		)		
		;; Sinon, si le concept n'est pas dans frames
		(progn
			(format t "~%Erreur : le concept n'est pas dans le frame")
			NIL
		) 
	)
)


;; Fonctions de service				 

;; Obtenir le démon d'un concept
(defun getDemon (concept slot frames)
	(if concept 
		(let ((demon (cadr (assoc 'if-added (cdr (assoc slot (cdr (assoc concept frames))))))))
			(if (not demon)
				(getDemon (cadr(assoc 'value(cdr(assoc 'is-a (cdr (assoc concept frames)))))) slot frames)
			)
			demon
		)
		NIL
	)
)

;; Obtenir les slots autorisés pour un concept
(defun getSlotsAllowed(concept frames)
	(if concept
		(let ((slots NIL) (profondeur (getSlotsAllowed(cdr(assoc 'value(cdr(assoc 'is-a (cdr (assoc concept frames))))))frames)))
			;; Pour chaque slot possible
			(dolist (slot (cdr (assoc concept frames)))
				;; Ajout à la liste
				(push (car slot) slots)
			)	
			;; Appel récursif
			(if profondeur
				(cons profondeur slots)
			)
			;; Retourne la liste
			slots
		)
	)
)			
				
;; Exemple
(setq frames (make-individu 'Clyde 'ELEPHANT frames '(COLOR "grey" AGE 5)))		



;; Question 2 - Accéder aux informations


;; Solution prof
;;Si frame n'existe pas :
;;Erreur
;;Sinon :
;;	Si slot n'existe pas : 
;;		NIL
;;	Si facette value :
;;		value
;;	Si facette default :
;;		default
;;	Si facette demon "if-needed" :
;;		executer demon value
;;	Si value :
;;		value
;;	Sinon :
;;		(getSlotValue(getConcept) slot frames)



(defun getSlotValue (frame slot frames frameOriginal)
	;; Initialisation des variables nécessaires
	(let* 
		(
			(conceptOriginal (cadr(assoc 'value(cdr(assoc 'is-a (cdr (assoc frameOriginal frames)))))))
			(concept (cadr(assoc 'value(cdr(assoc 'is-a (cdr (assoc frame frames)))))))
			(slots_allowed (getSlotsAllowed conceptOriginal frames))
			(facette_value (cadr(assoc 'value(cdr(assoc slot (cdr (assoc frame frames)))))))
			(facette_default (cadr(assoc 'default(cdr(assoc slot (cdr (assoc frame frames)))))))
			(demon_if_needed (cadr (assoc 'if-needed (cdr (assoc slot (cdr (assoc concept frames))))))) 
			(age (cadr(assoc 'value(cdr(assoc 'age (cdr (assoc frameOriginal frames)))))))
		)
		
	;; Si le frame est bien dans la liste des frames
	(if (assoc frame frames)
		;; Si le slot existe
		(if (not(member slot slots_allowed))
			(progn 
				(format t "Erreur")
				NIL
			)
			;; Si la facette existe et qu'une value est associée, on la retourne
			(if facette_value 
				facette_value
			
				;; Si une facette default est associé au frame, on retourne la valeur
				(if facette_default 
					facette_default
					;; Si une facette avec demon dans if-needed, on retourne la valeur
					(if demon_if_needed 
						(funcall demon_if_needed age)
						;; Sinon, appel récursif
						(getSlotValue concept slot frames frameOriginal)
					)
				)	
			)
		)	
	)
	)
)

;; Exemple
(getslotvalue 'clyde0 'poids frames 'clyde0) ;; retoure 5
(getslotvalue 'clyde0 'poids frames 'clyde0) ;; retoure 50


