;;; SYSTEME EXPERT ORDRE 0+: MEILLEUR CADEAU DE NOEL

;;; ASSAF Nora (GI01) et GAJAN Antoine (GI01)


;; Variables globales
(setq *REGLES* NIL)
(setq *QUESTIONS* NIL)
(setq *BASE_DE_FAITS* NIL)
(setq *LISTE_CADEAUX* NIL)

;; Fonctions relatives aux r gles
;; Une r gle est de la forme (R1 (cond1 cond2 ...) conclusion)

(defun num-regle (regle)
	(car regle)
)

(defun add-regle (conditions conclusion)
	(pushnew (list (gentemp "R") conditions conclusion) *REGLES*)
)

(defun get-conditions (num-regle)
	(cadr (assoc num-regle *REGLES*))
)

(defun get-conclusion (num-regle)
	(caddr (assoc num-regle *REGLES*))
)

;; Fonction qui d termine les r gles candidates aboutissantàun but
(defun regles-candidates (but)
	;; On d finit les r gles candidatesàNIL
	(let ((liste_regles_candidates NIL))
		;; Pour chaque r gle dans la base de r gles
		(dolist (regle *REGLES*)
			;; Si elle permet la bonne conclusion
			(if (eq (get-conclusion (num-regle regle)) but)
				;; Ajoutàla liste des r gles possibles
				(pushnew (num-regle regle) liste_regles_candidates)
			)
		)
		;; On retourne la liste des r gles candidates
		liste_regles_candidates
	)
)

;; Fonction qui regarde si un but est dans la base de faits
(defun appartient (but)
    ;; D finition des variables
    (let ((operateur (car but)) (variable (cadr but)) (val2 (caddr but)) val1)
        ;; On trouve val1àpartir de la base de faits
				(setq val1 (cadr(assoc variable *BASE_DE_FAITS*)))
        (if val1
            ;; On  value si la condition entre les valeurs et l'op rateur est correcte
            (funcall operateur val1 val2)
            ;; SiNIL, on renvoie NIL
            NIL
        )
    )
)

;; Fonction qui d termine si un cadeau peut convenir en fonction de la base de faits
(defun cadeau-possible ()
	(let ((possible NIL) (cadeau NIL))
		;; On v rifie si un cadeau peut convenir
		(dolist (cad *LISTE_CADEAUX*)
			(setq possible t)
			;; On v rifie que toutes les conditions sont possibles
			(dolist (condition (car cad))
				(if (NOT(member condition *BASE_DE_FAITS* :test 'equal))
					(setq possible NIL)
				)
			)
			;; Si le cadeau est possible, on metàjour
			(if possible
				(setq cadeau (cadr cad))
			)
		)
		cadeau
	)
)

;; Fonctions relatives aux questions
;; Une question est de la forme (variable_impactee "Quel est...?" (condtion1 condition2))

(defun questions-with-priority ()
	(let ((questions-avec-prio NIL) (nombre 0) (trouve NIL) (question_possible NIL))
		;; Pour chaque question
		(dolist (question *QUESTIONS*)
			;; On suppose la question possible et on v rifie si c'est le cas
			(setq question_possible T)
			(dolist (condition (caddr question))
				;; Si la condition n'est pas vérifiée dans la base de faits
				(if (NOT(member condition *BASE_DE_FAITS* :test 'equal))
					(setq question_possible NIL)
				)
			)
			;; Si on peut poser la question
			(if question_possible
				(progn
					(setq nombre 0)
					;; On regarde le nombre de de règles qui peuvent être déclenchées
					(dolist (regle *REGLES*)
						;; Pour chaque condition dans la règle
						(dolist (condition (get-conditions (num-regle regle)))
							(if (eq (cadr condition) (car question))
								(setq trouve T)
							)
						)
						(if trouve
							;; Incrémentation de la priorité
							(progn
								(setq nombre (+ nombre 1))
								(setq trouve NIL)
							)
						)
					)
				;; Ajout à la liste
				(pushnew (list (car question) nombre) questions-avec-prio)
				)
			)
		)
		;; Retourne la liste
		questions-avec-prio
	)
)

;; Fonction qui retourne la variable associée à la meilleure question
(defun best-question ()
		(let ((questions-avec-prio (questions-with-priority)) (question-max NIL) (prio-max -1))
			;; Pour chaque question encore pr sente
			(dolist (question questions-avec-prio)
				;; Si elle est mieux que la meilleure question
				(if (> (cadr question) prio-max)
					;; Miseàjour
					(progn
						(setq question-max (car question))
						(setq prio-max (cadr question))
					)
				)
			)
			;; On retourne la meilleure question
			question-max
		)
)

(defun ask-question (theme)
	(let ((question (cadr (assoc theme *QUESTIONS*))) (fait NIL) (reponse NIL))
		;; Pose la question
		(format t "~s~%" question)
		;; Attend la réponse
		(format t ">>> ")
		(setq reponse (read))
		;; Ajout de la r ponse provisoire à la base de faits
		(setq fait (list theme reponse))
		(pushnew fait *BASE_DE_FAITS*)
		;; Activation de toutes les règles possibles
		(dolist (regle *REGLES*)
			(setq possible T)
			;; Pour chaque condition dans la règle, on regarde si elle est dans la base de faits
			(dolist (condition (get-conditions (num-regle regle)))
				(if (NOT(appartient condition))
					(setq possible NIL)
				)
			)
			;; Si la règle est activable
			(if possible
				;; Activation de la règle
				(progn
					;; Supprime la réponse provisoire
					(setf *BASE_DE_FAITS* (remove fait *BASE_DE_FAITS*))
					;; Ajout du fait d finitifàla base de faits
					(pushnew (get-conclusion (num-regle regle)) *BASE_DE_FAITS*)
					;; Supprime toutes les règles faisant intervenir ce terme en conclusion
					(supprimer-regles theme)
				)
			)
		)
		;; On retire la question
		(retirer-question theme)
	)
)

;; Supprime toutes les règles dont la conclusion fait intervenir la variable theme
(defun supprimer-regles (theme)
	(dolist (regle *REGLES*)
		(if (eq (car (get-conclusion (num-regle regle))) theme)
			(setf *REGLES*(remove regle *REGLES*))
		)
	)
)

;; Fonction pour retirer une question de la base de faits
(defun retirer-question (theme)
	;; Suppression de la question posée
	(dolist (question *QUESTIONS*)
		;; Si c'est celle qu'on vient de poser
		(if (eq (car question) theme)
			(setf *QUESTIONS* (remove question *QUESTIONS*))
		)
	)
)

;; Fonction pour ajouter une question à la base de questions
(defun add-question (theme question conditions)
		(pushnew (list theme question conditions) *QUESTIONS*)
)


;; Fonctions relatives aux cadeaux
(defun add-cadeau (conditions cadeau phrase_descri)
		;; Ajout du cadeau
		(pushnew (list conditions cadeau phrase_descri) *LISTE_CADEAUX*)
)

;; Fonction permettant de trouver la phrase d'accroche liée à un cadeau
(defun phrase-accroche (nom_cadeau)
	(dolist (cadeau *LISTE_CADEAUX*)
		(if (eq (cadr cadeau) nom_cadeau)
			(return-from phrase-accroche (caddr cadeau))
		)
	)
)


;; Fonctions relatives au moteur d'inférences

;; Chainage avant : on regarde quel est le meilleur cadeau
(defun chainage-avant ()
	;; Variables utiles
	(let ((meilleure-question NIL) (cadeau NIL))
		;; Demande des questions basiques
		(ask-question 'age)
		(ask-question 'relation)
		(ask-question 'budget)
		;; Tant que l'on n'a pas trouv  de cadeau et qu'il reste des questions
		(while (AND (eq cadeau NIL) (questions-with-priority))
			;; On pose la meilleure question
			(setq meilleure-question (best-question))
			(ask-question meilleure-question)
			;; On actualise le cadeau si un cadeau est possible
			(setq cadeau (cadeau-possible))
		)
		;; On retourne le cadeau
		cadeau
	)
)

;; Chainage arrière : on regarde si le cadeau est adapt
(defun chainage-arriere (cadeau)
  (let ((ligne_cadeau NIL) (ok NIL) (adapte NIL))
		;; Recherche de la ligne associée au cadeau
		(dolist (cad *LISTE_CADEAUX*)
			;; Si c'est bon
			(if (eq (cadr cad) cadeau)
				(setq ligne_cadeau cad)
			)
		)
		;; Pour chaque condition du cadeau
		(dolist (condition (car ligne_cadeau))
			;; On pose la question associée
			(ask-question (car condition))
		)
		;; Pur chaque cadeau de la liste
		(dolist (cad *LISTE_CADEAUX*)
			;; Si c'est bon
			(if (eq (cadr cad) cadeau)
				(progn
					(setq ligne_cadeau cad)
					(setq ok t)
					;; On vérifie si toutes les conditions du cadeau sont vérifiées
					(dolist (condition (car ligne_cadeau))
						(if (NOT(member condition *BASE_DE_FAITS* :test 'equal))
							(setq ok NIL)
						)
					)
					;; Si toutes les conditions sont ok, on approuve le cadeau
					(if ok
						(setq adapte t)
					)
				)
			)
		)
		;; On retourne si le cadeau est adapte ou NIL
		adapte
	)
)

;; Ajout des règles à la base de règles

(add-regle '((<= budget 30)) '(budget petit))
(add-regle '((> budget 30) (<= budget 100)) '(budget moyen))
(add-regle '((> budget 100) (<= budget 1500)) '(budget gros))
(add-regle '((> budget 1500)) '(budget enorme))
(add-regle '((eq relation ami)) '(relation ami))
(add-regle '((eq relation famille)) '(relation famille))
(add-regle '((eq relation compagnon)) '(relation compagnon))
(add-regle '((< age 12)) '(age enfant))
(add-regle '((>= age 12) (<= age 30)) '(age jeune))
(add-regle '((> age 30)) '(age adulte))
(add-regle '((eq musique oui)) '(musique t))
(add-regle '((eq musique non)) '(musique NIL))
(add-regle '((eq type_musique classique)) '(type_musique classique))
(add-regle '((eq type_musique rap)) '(type_musique rap))
(add-regle '((eq type_musique pop)) '(type_musique pop))
(add-regle '((eq sport oui)) '(sport t))
(add-regle '((eq sport non)) '(sport NIL))
(add-regle '((eq type_sport regarder)) '(type_sport regarder))
(add-regle '((eq type_sport pratiquer)) '(type_sport pratiquer))
(add-regle '((eq decoration oui)) '(decoration t))
(add-regle '((eq decoration non)) '(decoration NIL))
(add-regle '((eq casanier oui)) '(casanier t))
(add-regle '((eq casanier non)) '(casanier NIL))
(add-regle '((eq lire oui)) '(lire t))
(add-regle '((eq lire non)) '(lire NIL))
(add-regle '((eq cuisiner oui)) '(cuisiner t))
(add-regle '((eq cuisiner non)) '(cuisiner NIL))
(add-regle '((eq tete_en_l_air oui)) '(tete_en_l_air t))
(add-regle '((eq tete_en_l_air non)) '(tete_en_l_air NIL))
(add-regle '((eq fetard oui)) '(fetard t))
(add-regle '((eq fetard non)) '(fetard NIL))
(add-regle '((eq cadeaux_humour oui)) '(cadeaux_humour t))
(add-regle '((eq cadeaux_humour non)) '(cadeaux_humour NIL))
(add-regle '((eq souvenirs oui)) '(souvenirs t))
(add-regle '((eq souvenirs non)) '(souvenirs NIL))
(add-regle '((eq cadeaux_humour oui)) '(cadeaux_humour t))
(add-regle '((eq cadeaux_humour non)) '(cadeaux_humour NIL))
(add-regle '((eq mode oui)) '(mode t))
(add-regle '((eq mode non)) '(mode NIL))
(add-regle '((eq apprendre oui)) '(apprendre t))
(add-regle '((eq apprendre non)) '(apprendre NIL))
(add-regle '((eq fantastique oui)) '(fantastique t))
(add-regle '((eq fantastique non)) '(fantastique NIL))
(add-regle '((eq etudes oui)) '(etudes t))
(add-regle '((eq etudes non)) '(etudes NIL))
(add-regle '((eq art oui)) '(art t))
(add-regle '((eq art non)) '(art NIL))
(add-regle '((eq jeux oui)) '(jeux t))
(add-regle '((eq jeux non)) '(jeux NIL))
(add-regle '((eq type_jeux video)) '(type_jeux video))
(add-regle '((eq type_jeux societe)) '(type_jeux societe))
(add-regle '((eq twitch oui)) '(twitch t))
(add-regle '((eq twitch non)) '(twitch NIL))
(add-regle '((eq pokemon oui)) '(pokemon t))
(add-regle '((eq pokemon non)) '(pokemon NIL))
(add-regle '((eq famille oui)) '(famille t))
(add-regle '((eq famille non)) '(famille NIL))
(add-regle '((eq gastronomie oui)) '(gastronomie t))
(add-regle '((eq gastronomie non)) '(gastronomie NIL))
(add-regle '((eq gourmand oui)) '(gourmand t))
(add-regle '((eq gourmand non)) '(gourmand NIL))
(add-regle '((eq television oui)) '(television t))
(add-regle '((eq television non)) '(television NIL))
(add-regle '((eq film_serie oui)) '(film_serie t))
(add-regle '((eq film_serie NIL)) '(film_serie NIL))
(add-regle '((eq utc oui)) '(utc t))
(add-regle '((eq utc non)) '(utc NIL))
(add-regle '((eq voyage oui)) '(voyage t))
(add-regle '((eq voyage non)) '(voyage NIL))
(add-regle '((eq bijoux oui)) '(bijoux t))
(add-regle '((eq bijoux non)) '(bijoux NIL))
(add-regle '((eq construire oui)) '(construire t))
(add-regle '((eq construire non)) '(construire NIL))
(add-regle '((eq creatif oui)) '(creatif t))
(add-regle '((eq creatif non)) '(creatif NIL))



;; Ajout des questions à la base de questions

(add-question 'age "Quel âge a la personne ?" NIL)
(add-question 'budget "Quel est votre budget cadeau ?" NIL)
(add-question 'relation "Quel est votre relation avec la personne ? (ami/famille/compagnon)" NIL)
(add-question 'etudes "La personne fait-elle des études ?" '((age jeune)))
(add-question 'utc "La personne est-elle à l'UTC ?" '((etudes t)))
(add-question 'construire "La personne aime-t-elle construire des choses ?" '((age enfant)))
(add-question 'creatif "La personne est-elle créative ?" '((age enfant)))
(add-question 'bijoux "La personne aime-t-elle porter des bijoux" '((budget enorme)))
(add-question 'musique "La personne aime-t-elle la musique ?" NIL)
(add-question 'type_musique "Quel type de musique la personne préfère t-elle ? (classique/rap/pop)" '((musique t)))
(add-question 'jeux "La personne aime t'elle les jeux en général ?" NIL)
(add-question 'type_jeux "La personne préfère-t-elle les jeux vidéos ou les jeux de société ? (video/societe)" '((jeux t)))
(add-question 'pokemon "La personne aime-t-elle Pokémon ?" '((type_jeux video)))
(add-question 'twitch "La personne aime-t-elle regarder Twitch ?" '((type_jeux video)))
(add-question 'sport "La personne aime-t-elle le sport ?" NIL)
(add-question 'type_sport "La personne pratique-t-elle du sport ou regarde-t-elle uniquement ? (pratiquer/regarder)" '((sport t)))
(add-question 'mode "La personne aime-t-elle la mode ?" NIL)
(add-question 'casanier "La personne est-elle casanière ?" NIL)
(add-question 'lire "La personne aime-t-elle lire ?" NIL)
(add-question 'apprendre "La personne aime-t-elle apprendre de nouvelles choses ?" NIL)
(add-question 'television "La personne aime-t-elle regarder la télévision en général ?" NIL)
(add-question 'film_serie "La personne aime-t-elle regarder des series ?" '((television t)))
(add-question 'tete_en_l_air "La personne est-elle tête en l'air ?" NIL)
(add-question 'art "La personne est-elle sensible à l'art ?" NIL)
(add-question 'fetard "La personne est-elle fétarde ?" NIL)
(add-question 'decoration "La personne aime-t-elle la decoration ?" NIL)
(add-question 'famille "La personne aime-t-elle les moments en famille ?" NIL)
(add-question 'fantastique "La personne aime-t-elle les choses fantastiques (ex : Harry Potter) ?" NIL)
(add-question 'cadeaux_humour "La personne aime-t-elle les cadeaux humoristiques ?" NIL)
(add-question 'cuisiner "La personne aime-t-elle cuisiner ?" NIL)
(add-question 'gastronomie "La personne aime-t-elle la gastronomie ?" '((cuisiner t)))
(add-question 'souvenirs "La personne aime-t-elle les souvenirs ?" NIL)
(add-question 'gourmand "La personne est-elle gourmande ?" NIL)
(add-question 'voyage "La personne aime-t-elle voyager ?" NIL)



;; Ajout des cadeaux � la base de cadeaux

;;CADEAUX AMI JEUNE PETIT BUDGET

(add-cadeau '((budget petit) (relation ami) (age jeune) (decoration t)) 'cactus "Un jolie cactus !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (casanier NIL) (fetard t)) 'verre_a_biere "Un beau verre à bière !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation ami) (etudes t) (cuisiner t))
            'livre_cuisine_etudiant "Un livre de cuisine pour étudiant pour qu'il ou elle puisse manger plus varié et équilibré !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (fantastique t)) 'goodies_Harry_Potter "Des goodies Harry Potter !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (apprendre t))
            'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (television t))
            'place_cinema "Deux places de cinéma ! C'est l'occasion d'aller voir le dernier Avatar !")
(add-cadeau '((budget petit) (relation ami) (age jeune) (decoration NIL) (tete_en_l_air NIL) (cuisiner NIL)
              (apprendre NIL) (television NIL) (lire NIL) (fetard NIL) )
            'carte_cadeau_20 "Cette personne a l'air difficile... une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX AMI JEUNE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation ami) (age jeune) (musique t) (fetard t) (type_musique rap)) 'place_concert_rap "Une place de concert de rap !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (musique t) (fetard t) (type_musique pop)) 'place_concert_pop "Une place de concert de pop !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (musique t) (fetard t) (type_musique classique)) 'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (mode t) (etudes t) (utc t)) 'sweat_UTC "Un sweat de son école d'ingénieur préférée ! (oui oui de l'UTC)")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (mode t)) 'sweat "Un beau sweat !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (art t)) 'place_exposition_art "Une place d'exposition d'art !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (type_jeux video) (pokemon t)) 'jeu_pokemon "Le tout dernier jeu Pokémon !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (type_jeux video) (twitch t)) 'abonnement_twitch "Un abonnnement Twitch de 3 mois à son streameur préféré !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (type_jeux video) (pokemon NIL) (twitch NIL)) 'gtavi "GTAVI ! Quand il sera sortie :)")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (famille t) (jeux t)) 'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (type_sport pratiquer)) 'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (type_sport regarder)) 'place_match_foot "Une place pour un match de foot !")
(add-cadeau '((budget moyen) (relation ami) (age jeune) (sport NIL) (famille NIL) (jeux NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")

;;CADEAUX AMI JEUNE GROS BUDGET

(add-cadeau '((budget gros) (relation ami) (age jeune) (musique t) (fetard t)) 'place_festival "Une place de festival!")
(add-cadeau '((budget gros) (relation ami) (age jeune) (mode t)) 'place_defile "Une place pour un defilé de mode!")
(add-cadeau '((budget gros) (relation ami) (age jeune) (gastronomie t)) 'restaurant_chic "Un dîner dans un restaurant chic !")
(add-cadeau '((budget gros) (relation ami) (age jeune) (casanier t) (type_sport pratiquer)) 'velo_appartement "Un vélo d'appartement !")
(add-cadeau '((budget gros) (relation ami) (age jeune) (casanier t) (type_sport regarder)) 'abonnement_BeinSport "Un abonnement BeinSport à l'année")
(add-cadeau '((budget gros) (relation ami) (age jeune) (casanier t) (film_serie t) (fantastique t)) 'musee_Harry_Potter "Une visite du musée Harry Potter !")
(add-cadeau '((budget gros) (relation ami) (age jeune) (casanier t) (sport NIL) (film_serie t) (fantastique NIL))
            'abonnement_Netflix "Un abonnement Netflix à l'année pour qu'il ou elle puisse regarder tous ses films et séries préférés !")
(add-cadeau '((budget gros) (relation ami) (age jeune) (musique NIL) (mode NIL) (gastronomie NIL)) 'carte_cadeau_100
	"Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")

;;CADEAUX AMI ADULTE PETIT BUDGET

(add-cadeau '((budget petit) (relation ami) (age adulte) (decoration t)) 'cactus "Un jolie cactus !")
(add-cadeau '((budget petit) (relation ami) (age adulte) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation ami) (age adulte) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation ami) (age adulte) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation ami) (age adulte) (apprendre t)) 'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation ami) (age adulte) (television t))
            'place_cinema "Deux places de cinéma ! C'est l'occasion d'aller voir le dernier Avatar !")
(add-cadeau '((budget petit) (relation ami) (age adulte) (decoration NIL) (apprendre NIL) (television NIL) (lire NIL) (tete_en_l_air NIL) (fetard NIL)) 'carte_cadeau_20
	"Cette personne a l'air difficile... une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX AMI ADULTE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation ami) (age adulte) (musique t) (fetard t) (type_musique pop)) 'place_concert_pop "Une place de concert de pop !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (musique t) (fetard t) (type_musique classique)) 'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (mode t) (lire t) (etudes NIL)) 'echarpe "Une belle echarpe !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (art t)) 'place_exposition_art "Une place d'exposition d'art !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (famille t) (jeux t)) 'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (sport t) (type_sport pratiquer)) 'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (sport t) (type_sport regarder)) 'place_match_foot "Une place pour un match de foot !")
(add-cadeau '((budget moyen) (relation ami) (age adulte) (sport NIL) (famille NIL) (jeux NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")

;;CADEAUX AMI ADULTE GROS BUDGET

(add-cadeau '((budget gros) (relation ami) (age adulte) (cuisiner t)) 'stage_cuisine "Un stage de cuisine pour devenir un encore meilleur cuisinier !")
(add-cadeau '((budget gros) (relation ami) (age adulte) (casanier t) (type_sport pratiquer)) 'velo_appartement "Un vélo d'appartement !")
(add-cadeau '((budget gros) (relation ami) (age adulte) (casanier t) (type_sport regarder)) 'abonnement_BeinSport
	"Un abonnement BeinSport à l'année pour qu'il ou elle ne rate plus aucune de ses compétitions sportives préférées !")
(add-cadeau '((budget gros) (relation ami) (age adulte) (casanier t) (sport NIL) (film_serie t)) 'abonnement_Netflix
	"Un abonnement Netflix à l'année pour qu'il ou elle puisse regarder tous ses films et séries préférés !")
(add-cadeau '((budget gros) (relation ami) (age adulte) (cuisiner NIL) (television NIL) (sport nil))
            'carte_cadeau_100 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")

;;CADEAUX COMPAGNON JEUNE PETIT BUDGET

(add-cadeau '((budget petit) (relation compagnon) (age jeune) (casanier t) (decoration t)) 'cadre_photo_personnalise "Un cadre photo personnalisé !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (gourmand t)) 'boite_chocolat "Une boite de chocolats !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (apprendre t)) 'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (television t))
            'place_cinema "Deux places de cinéma ! C'est l'occasion d'aller voir le dernier Avatar !")
(add-cadeau '((budget petit) (relation compagnon) (age jeune) (apprendre NIL) (television NIL) (lire NIL) (tete_en_l_air NIL) (gourmand NIL))
            'carte_cadeau_20 "Cette personne a l'air difficile... Une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX COMPAGNON JEUNE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (musique t) (fetard t) (type_musique rap))
            'place_concert_rap "Une place de concert de rap !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (musique t) (fetard t) (type_musique pop))
            'place_concert_pop "Une place de concert de pop !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (musique t) (fetard t) (type_musique classique))
            'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (mode t) (etudes t) (utc t))
            'sweat_UTC "Un sweat de son école d'ingénieur preférée !! (oui oui l'UTC)")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (mode t) (etudes NIL)) 'sweat "Un beau sweat !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (art t)) 'place_exposition_art "Une place d'exposition d'art !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (type_jeux video) (pokemon t)) 'jeu_pokemon "Le tout dernier jeu Pokémon !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (type_jeux video) (twitch t))
            'abonnement_twitch "Un abonnnement Twitch à son streameur préféré !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (type_jeux video) (pokemon NIL) (twitch NIL)) 'gtavi "GTA VI ! Quand il sera sortie :)")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (famille t) (jeux t))
            'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (type_sport pratiquer))
            'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (type_sport regarder))
            'place_match_foot "Une place pour un match de foot !")
(add-cadeau '((budget moyen) (relation compagnon) (age jeune) (sport NIL) (famille NIL) (jeux NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")

;;CADEAUX COMPAGNON JEUNE GROS BUDGET

(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (voyage t)) 'voyage_a_deux "Un voyage à deux !")
(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (bijoux t)) 'bijoux "Une montre de luxe !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (musique t) (fetard t)) 'place_festival "Une place de festival!")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (mode t)) 'place_defile "Une place pour un defile de mode!")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (gastronomie t)) 'restaurant_chic "Un diner dans un restaurant chic !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (voyage t)) 'week_end_a_deux "Un week-end à deux !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (casanier t) (film_serie t) (fantastique t)) 'musee_Harry_Potter "Une visite du musée Harry Potter !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (casanier t) (sport NIL) (film_serie t) (fantastique NIL))
            'abonnement_Netflix "Un abonnement Netflix à l'année pour qu'il ou elle puisse regarder tous ses films et séries préférés !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (voyage NIL) (musique NIL) (mode NIL) (gastronomie NIL) (casanier NIL))
            'carte_cadeau_150 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")
(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (voyage NIL) (bijoux NIL) (musique NIL) (mode NIL) (gastronomie NIL))
            'carte_cadeau_150 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")

;;CADEAUX COMPAGNON ADULTE BUDGET PETIT

(add-cadeau '((budget petit) (relation compagnon) (age adulte) (casanier t) (decoration t)) 'cadre_photo_personnalise "Un cadre photo personnalisé !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (gourmand t)) 'boite_chocolat "Une boite de chocolats !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (apprendre t))
            'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (television t))
            'place_cinema "Deux places de cinéma ! C'est l'occasion d'aller voir le dernier Avatar !")
(add-cadeau '((budget petit) (relation compagnon) (age adulte) (apprendre NIL) (television NIL) (gourmand NIL) (lire NIL) (tete_en_l_air NIL))
	'carte_cadeau_20 "Cette personne a l'air difficile... Une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX COMPAGNON ADULTE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (musique t) (fetard t) (type_musique pop)) 'place_concert_pop "Une place de concert de pop!")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (musique t) (fetard t) (type_musique classique)) 'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (mode t) (lire t) (etudes NIL)) 'echarpe "Une belle echarpe !")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (art t)) 'place_exposition_art "Une place d'exposition d'art")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (famille t) (jeux t))
            'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (type_sport pratiquer))
            'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (type_sport regarder)) 'place_match_foot "Une place pour un match de foot!")
(add-cadeau '((budget moyen) (relation compagnon) (age adulte) (sport NIL) (famille NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")

;;CADEAUX COMPAGNON ADULTE GROS BUDGET

(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (voyage t)) 'voyage_a_deux "Un voyage à deux !")
(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (bijoux t)) 'bijoux "Une montre de luxe !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (musique t) (fetard t)) 'place_festival "Une place de festival!")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (mode t)) 'place_defile "Une place pour un defile de mode!")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (gastronomie t)) 'restaurant_chic "Un diner dans un restaurant chic !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (voyage t)) 'week_end_a_deux "Un week-end à deux !")
(add-cadeau '((budget gros) (relation compagnon) (age jeune) (voyage NIL) (musique NIL) (mode NIL) (gastronomie NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")
(add-cadeau '((budget enorme) (relation compagnon) (age jeune) (voyage NIL) (bijoux NIL) (musique NIL) (mode NIL))
            'carte_cadeau_150 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")


;;CADEAUX ENFANT PETIT BUDGET

(add-cadeau '((budget petit) (age enfant) (creatif t)) 'coloriage "Un kit de colofiage !")
(add-cadeau '((budget petit) (age enfant) (construire t)) 'jeu_en_bois "Des jeux en bois !")
(add-cadeau '((budget petit) (age enfant) (creatif NIL) (construire NIL)) 'peluche "Une peluche !")

;;CADEAUX ENFANT BUDGET MOYEN

(add-cadeau '((budget moyen) (age enfant) (creatif t)) 'playmobils "Des playmobils !")
(add-cadeau '((budget moyen) (age enfant) (construire t)) 'legos "Des legos !")
(add-cadeau '((budget moyen) (age enfant) (creatif NIL) (construire NIL)) 'peluche_personnalisee "Une peluche personnalisée !")

;;CADEAUX ENFANT GROS BUDGET

(add-cadeau '((budget gros) (age enfant) (creatif t)) 'kit_complet_playmobils "Un kit complet de playmobils !")
(add-cadeau '((budget gros) (age enfant) (construire t)) 'kit_complet_legos "Un kit complet de legos !")
(add-cadeau '((budget gros) (age enfant) (musique t)) 'kit_eveil_musical "Un kit d'éveil musical !")
(add-cadeau '((budget gros) (age enfant) (creatif NIL) (construire NIL) (musique NIL)) 'jeu_en_bois "Des jeux en bois !")

(add-cadeau '((budget enorme) (age enfant) (creatif t)) 'kit_complet_playmobils "Un kit complet de playmobils !")
(add-cadeau '((budget enorme) (age enfant) (construire t)) 'kit_complet_legos "Un kit complet de legos !")
(add-cadeau '((budget enorme) (age enfant) (musique t)) 'kit_eveil_musical "Un kit d'éveil musical !")
(add-cadeau '((budget enorme) (age enfant) (creatif NIL) (construire NIL) (musique NIL)) 'jeu_en_bois "Des jeux en bois !")

;;CADEAUX FAMILLE JEUNE PETIT BUDGET

(add-cadeau '((budget petit) (relation famille) (age jeune) (casanier t) (decoration t)) 'cadre_photo_personnalise "Un cadre photo personnalisé !")
(add-cadeau '((budget petit) (relation famille) (age jeune) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation famille) (age jeune) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation famille) (age jeune) (gourmand t)) 'boite_chocolat "Une boite de chocolats !")
(add-cadeau '((budget petit) (relation famille) (age jeune) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation famille) (age jeune) (apprendre t))
            'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation famille) (age jeune) (apprendre NIL) (lire NIL) (tete_en_l_air NIL) (gourmand NIL))
            'carte_cadeau_20 "Cette personne a l'air difficile... Une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX FAMILLE JEUNE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation famille) (age jeune) (musique t) (fetard t) (type_musique rap)) 'place_concert_rap "Une place de concert de rap !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (musique t) (fetard t) (type_musique pop)) 'place_concert_pop "Une place de concert de pop !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (musique t) (fetard t) (type_musique classique)) 'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (mode t) (etudes NIL)) 'sweat "Un beau sweat !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (art t)) 'place_exposition_art "Une place d'exposition d'art !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (type_jeux video) (pokemon t)) 'jeu_pokemon "Le tout dernier jeu Pokémon !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (type_jeux video) (twitch t)) 'abonnement_twitch "Un abonnnement Twitch àson streameur préféré !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (type_jeux video) (pokemon NIL) (twitch NIL)) 'gtavi "GTAVI ! (Quand il sera sortie :)")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (famille t) (jeux t))
            'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (type_sport pratiquer))
            'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (type_sport regarder)) 'place_match_foot "Une place pour un match de foot !")
(add-cadeau '((budget moyen) (relation famille) (age jeune) (sport NIL) (famille NIL) (jeux NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")

;;CADEAUX FAMILLE JEUNE GROS BUDGET

(add-cadeau '((budget enorme) (relation famille) (age jeune) (voyage t)) 'voyage_en_famille "Un voyage en famille !")
(add-cadeau '((budget enorme) (relation famille) (age jeune) (bijoux t)) 'bijoux "Une montre de luxe !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (musique t) (fetard t)) 'place_festival "Une place de festival!")
(add-cadeau '((budget gros) (relation famille) (age jeune) (mode t)) 'place_defile "Une place pour un defile de mode!")
(add-cadeau '((budget gros) (relation famille) (age jeune) (gastronomie t)) 'restaurant_chic "Un diner dans un restaurant chic !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (casanier t) (film_serie t) (fantastique t)) 'musee_Harry_Potter "Une visite du musée Harry Potter !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (casanier t) (sport NIL) (film_serie t) (fantastique NIL))
            'abonnement_Netflix "Un abonnement Netflixàl'année pour qu'il ou elle puisse regarder tous ses films et séries préférés !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (musique NIL) (mode NIL) (gastronomie NIL) (casanier NIL))
            'carte_cadeau_150 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")
(add-cadeau '((budget enorme) (relation famille) (age jeune) (voyage NIL) (bijoux NIL) (musique NIL) (mode NIL) (gastronomie NIL))
            'carte_cadeau_150 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")


;;CADEAUX FAMILLE ADULTE BUDGET PETIT

(add-cadeau '((budget petit) (relation famille) (age adulte) (casanier t) (decoration t)) 'cadre_photo_personnalise "Un cadre photo personnalisé !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (tete_en_l_air t) (cadeaux_humour t))
            'calendrier_personnalise "Un calendrier personnalisé pour qu'il ou elle n'oublie aucune date !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (tete_en_l_air t) (souvenirs t))
            'porte_cle_personnalise "Un porte clé personnalisé pour qu'il ou elle ne perde plus jamais ses clefs!")
(add-cadeau '((budget petit) (relation famille) (age adulte) (gourmand t)) 'boite_chocolat "Une boite de chocolats !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (lire t) (apprendre t))
            'livre_culture_generale "Un livre de culture générale pour s'instruire tous les soirs !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (apprendre t))
            'visite_musee "Une visite de musée pour sortir et s'instruire en même temps !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (television t))
            'place_cinema "Deux places de cinéma ! C'est l'occasion d'aller voir le dernier Avatar !")
(add-cadeau '((budget petit) (relation famille) (age adulte) (apprendre NIL) (gourmand NIL) (television NIL) (lire NIL) (tete_en_l_air NIL) (fetard NIL))
            'carte_cadeau_20 "Cette personne a l'air difficile... Une carte cadeau de 20 euros est une valeur sûre !")

;;CADEAUX FAMILLE ADULTE BUDGET MOYEN

(add-cadeau '((budget moyen) (relation famille) (age adulte) (musique t) (fetard t) (type_musique pop)) 'place_concert_pop "Une place de concert de pop!")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (musique t) (fetard t) (type_musique classique)) 'place_concert_classique "Une place de concert classique !")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (mode t) (lire t)) 'livre_mode "Un livre de mode !")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (mode t) (lire t) (etudes NIL)) 'echarpe "Une belle echarpe !")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (art t)) 'place_exposition_art "Une place d'exposition d'art")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (famille t) (jeux t))
            'jeux_de_societe "Un jeu de société pour passer plein de bons moments en famille !")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (type_sport pratiquer))
            'tenue_sport "Une tenue de sport pour continuer le sport en 2023 !")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (type_sport regarder)) 'place_match_foot "Une place pour un match de foot!")
(add-cadeau '((budget moyen) (relation famille) (age adulte) (sport NIL) (famille NIL) (jeux NIL) (art NIL) (mode NIL) (musique NIL))
            'carte_cadeau_50 "Cette personne a l'air difficile... Une carte cadeau de 50 euros est une valeur sûre !")

;;CADEAUX FAMILLE ADULTE GROS BUDGET

(add-cadeau '((budget gros) (relation famille) (age jeune) (cuisiner t)) 'stage_cuisine "Un stage de cuisine !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (casanier t) (type_sport pratiquer)) 'velo_appartement "Un vélo d'appartement !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (casanier t) (type_sport regarder))
            'abonnement_BeinSport "Un abonnement BeinSport à l'année pour qu'il ou elle ne rate plus aucune de ses compétitions sportives préférées !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (casanier t) (sport NIL) (film_serie t) (fantastique NIL))
            'abonnement_Netflix "Un abonnement Netflix à l'année pour qu'il ou elle puisse regarder tous ses films et séries préférés !")
(add-cadeau '((budget gros) (relation famille) (age jeune) (cuisiner NIL) (gastronomie NIL) (sport NIL) (television NIL))
            'carte_cadeau_100 "Cette personne a l'air difficile... Une carte cadeau de 150 euros est une valeur sûre !")


;; Fonction main
(defun main ()
	(let ((reponse NIL) (cadeau NIL) (convient NIL))
		;; Affichage du menu
		(format t "~%=== SYSTEME EXPERT : LE MEILLEUR CADEAU DE NOEL ===")
		(format t "~%~%Que voulez-vous faire ?")
		(format t "~%  1) Trouver le cadeau idéal")
		(format t "~%  2) Vérifier si le cadeau convient")
		;; Demande du choix
		(format t "~%Votre choix : ")
		(setq reponse (read))
		(if (eq reponse 1)
			;; Si trouver le cadeau idéal
			(progn
				(setq cadeau (chainage-avant))
				(format t "~%~s" (phrase-accroche cadeau))
			)
			;; Si v rifier si un cadeau convient
			(progn
				(format t "~%Quel cadeau souhaitez-vous offrir ? ")
				(setq cadeau (read))
				(setq convient (chainage-arriere cadeau))
				(if convient
					;; Si le cadeau convient
					(format t "~%Le cadeau ~s convient !" cadeau)
					;; Si le cadeau ne convient pas
					(format t "~%Il est encore temps de changer d'idée. Le cadeau ~s ne semble pas adapté..." cadeau)
				)
			)
		)
	  )
  )
