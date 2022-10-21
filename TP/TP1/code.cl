; Exercice 1

; Question 4

;on teste si les 3 premiers éléments de la liste sont des nombres
(defun nombre3 (L) 
    (if (AND (numberp (car L)) (numberp (cadr L)) (numberp (caddr L)))
        'BRAVO
        'PERDU
    )
  )

; Jeu de tests
(nombre3 '(45 23 1 a b)) ; retourne BRAVO
(nombre3 '(a 12 4 5 6)) ; retourne PERDU

;fonction recursive
(defun grouper (L1 L2)
    
    (cond
        ;si les 2 listes sont vides
        ((AND ( = (length L1) 0) (= (length L2) 0))
            ()
        )

        ;si la liste 1 est vide
        (( = (length L1) 0)
            ; on ajoute le premier élément de L2 puis on appelle la fonction
            ; grouper pour le reste de L2
            (append (list(list (car L2))) (grouper NIL (cdr L2)))
        )

        ;si la liste 2 est vide
        (( = (length L2) 0)
            ; on ajoute le premier élément de L1 puis on appelle la fonction
            ; grouper pour le reste de L1
            (append (list(list (car L1))) (grouper (cdr L1) NIL))
        )

        ;si les 2 listes sont non vides
        ((AND (car L1) (car L2))
            ; on ajoute à l'intérieur de la liste de profondeur 0 une liste qui
            ; contient le premier élément de L1 et le premier élément de L2
            ; puis on rappelle la fonction grouper pour grouper le reste des listes
            ; L1 et L2
            (append (list(list (car L1) (car L2))) (grouper (cdr L1) (cdr L2)))
    )
    )
)

; Jeu de tests
(grouper '(1 2 3) '(4 5 6)) ; retourne ((1 4) (2 5) (3 6))
(grouper '(2 4 6 8) '(1)) ; retourne ((2 1) (4) (6) (8))


(defun monReverse (L)
    ; si la liste est non vide
    (if ( > (length L) 0)
        ; on appelle de manière récursive monReverse sur le cdr de L et on 
        ; ajoute à la fin de la liste le premier élément de L
        (append (monReverse (cdr L)) (list (car L)))
    )
)

; Jeu de tests
(monReverse '(1 2 3 4)) ; retourne (4 3 2 1)
(monReverse '(a b c d)) ; retourne (d c b a)


(defun palindrome (L)
    (equal L (monReverse L))
)

; Jeu de tests
(palindrome '(k a y a k)) ; retourne true
(palindrome '(1 2 1)) ; retourne true
(palindrome '(b b c d r z)) ; retourne NIL


; Exercice 2

(defun list-triple-couple (liste)
    ; pour chaque élément x de la liste, on crée une liste qui contient 
    ; x et le triple de x, qui sera donc de la forme (x 3x)
    (mapcar #'(lambda (elt) (list elt (* 3 elt))) liste)
)

; Jeu de tests
(list-triple-couple '(0 1 2 5 9)) ; renvoie ((0 0) (1 3) (2 6) (5 15) (9 27))

; Exercice 3

(defun my-assoc (cle a-list)
    ; on vérifie que la longueur de la liste soit > 0
    (if (> (length a-list) 0)
        ; si la clé de la première paire de la liste est celle recherchée, on renvoie la paire
        (if (equal (caar a-list) cle)
             (car a-list)
             ; sinon, on recherche dans le cdr de la liste (les autres paires) placée en paramètres
             (my-assoc cle (cdr a-list))
        )
        ; si la longueur de la liste est 0, on n'a pas trouvé la clé voulue dans les paires
        NIL
    )
)

; Jeu de tests
(my-assoc 'x '((a 1) (c 21) (x 12) (z 2))) ; renvoie (x 12)
(my-assoc 'd '((a 1) (c 21) (x 12) (z 2))) ; renvoie NIL


(defun cles (a-list)
    (mapcar #'(lambda (l)  (car l)) a-list)
)

; Jeu de tests
(cles '((a 1) (c 21) (x 12) (z 2))) ; renvoie (a c x z)


(defun creation (listeCles listeValeurs) 
    (if (AND (equal (length listeCles) (length listeValeurs)))
        (grouper listeCles listeValeurs)
        (PRINT "Les listes sont de tailles differentes")
    )
  )

; Jeu de tests
(creation '(a b c) '(1 2 3)) ; renvoie ((a 1) (b 2) (c 3))
(creation '(a b c d) '(1 2 3)) ; affiche "Les listes sont de tailles differentes"

; Exercice 4

(setq BaseTest
'( 
    ("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))
    ("Conquête de la Thuringe" 531 531 (("Royaume Franc") ("Thuringes")) ("Thuringe"))
    ("Guerre des Goths" 535 553 (("Royaume ostrogoth" "Alamans" "Royaume franc" "Royaume wisigoth" "Burgondes") ("Empire byzantin")) ("Péninsule italienne"))
    ("Conquête de l'Alémanie" 536 536 (("Royaume franc") ("Alamans")) ("Alémanie"))
    ("Conquête de la Bavière" 555 555 (("Royaume franc") ("Bavarii")) ("Bavière"))
    ("Campagnes de Breatagne" 560 578 (("Royaume franc") ("Royaume de Vannetais")) ("Vannetais"))
    ("Guerre de succession mérovingienne" 584 585 (("Royaume franc") ("Royaume d'Aquitaine")) ("Comminges"))
    ("Guerre franco-frisonne" 600 793 (("Royaume franc") ("Royaume de Frise")) ("Pays-Bas" "Allemagne"))
    ("Guerre civile des Francs" 715 719 (("Neustrie") ("Austrasie")) ("Royaume franc"))
    ("Invasion omeyyade en France" 719 759 (("Royaume franc") ("Califat omeyyade")) ("Royaume d'Aquitaine" "Septimanie"))
    ("Guerre des Lombards" 755 758 (("Royaume franc") ("Lombards")) ("Lombardie"))
    ("Guerre d'Aquitaine" 761 768 (("Royaume franc") ("Aquitains")) ("Vasconie" "Aquitaine"))
    ("Guerre des Saxons" 772 804 (("Royaume Franc") ("Saxons")) ("Germanie"))
    ("Guerre des Lombards" 773 774 (("Royaume Franc") ("Lombards")) ("Lombardie"))
    ("Guerre des Avars" 791 805 (("Royaume de France") ("Avars")) ("Pannonie"))
    ("Invasions sarrasines en Provence" 798 990 (("Royaume de France" "Comté de Provence") ("Sarrasins")) ("Provence"))
    ("Guerre civile entre les fils de Louis le Pieux" 830 842 (("Francie occidentale" "Francie orientale")) ("Fontenoy"))
    ("Guerre franco-bretonne" 843 851 (("Royaume de France") ("Royaume de Bretagne" "Vikings")) ("Royaume de Bretagne"))
    ("Luttes inter-dynastiques carolingiennes" 876 946 (("Francie occidentale" "Francie orientale") ("Royaume de Bourgogne" "Francie orientale")) ("Ardennes" "Saône-et-Loire" "Rhénanie-Palatinat" "Aisne"))
    ("Invasions vikings en France" 799 1014 (("Royaume de France") ("Vikings")) ("Normandie" "Bretagne"))
    ("Première croisade" 1096 1099 (("Comté de Blois" "Comté de Toulouse" "Comté de Boulogne" "Marquisat de Provence" "Comté de Flandre" "Duché de Normandie" "Diocèse du Puy-en-Velay" "Comté de Vermandois" "République de Gênes" "Duché de Basse-Lotharingie" "Principauté de Tarente" "Empire byzantin" "Royaume de Petite-Arménie" "Croisés" "Royaume de France") ("Sultanat de Roum" "Danichmendides" "Califat fatimide")) ("Terre sainte"))
 )
 ) 


(defun dateDebut (conflit)
    (cadr conflit)
)

; Jeu de tests
(dateDebut '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) ; renvoie 523


(defun nomConflit (conflit)
    (car conflit)
)

; Jeu de tests
(nomConflit '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) ; renvoie "Guerre de Burgondie"

(defun allies (conflit)
    (car(cadddr conflit))
)

; Jeu de tests
(allies '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) ; renvoie ("Royaume Franc")

(defun ennemis (conflit)
    (cadr(cadddr conflit))
)

; Jeu de tests
(ennemis '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) ; renvoie ("Royaume des Burgondes")

(defun lieu(conflit)
    (car(cddddr conflit))
)

; Jeu de tests
(lieu '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))) ; renvoie ("Vezeronce" "Arles")


; Fonctions de service

; Affichage de chacun des conflits de la base
(defun FB1(baseTest)
    (mapcar #'(lambda (conflit) (print conflit))baseTest)
)

; Jeu de test 
(FB1 BaseTest) ; affiche tous les conflits
(FB1 NIL) ; n'affiche rien et renvoie NIL car liste vide


(defun FB2(baseTest)
    ; Itération sur les conflits de la base
    (mapcan #'(lambda (conflit)
        ; Itération sur les alliés du conflit
        (mapcan #'(lambda (allies) 
            ; Si le Royaume Franc est un des alliés, on l'ajoute à la liste à retourner
            (if (equal allies "Royaume Franc")
                (append (list conflit)))
        ) (allies conflit)
        ) 
    )baseTest
    )
)

; Jeu de test
(FB2 BaseTest) ; affiche tous les conflits du Royaume Franc


; Même principe, en rajoutant un allie en parametre
(defun FB3(baseTest allie)
    (mapcan #'(lambda (conflit)
        (mapcan #'(lambda (allies) 
            (if (equal allies allie)
                (append (list conflit)))
        ) (allies conflit)
        ) 
    )baseTest
    )
)

; Jeu de test
(FB3 BaseTest 'Neustrie) ; renvoie ("Guerre civile des Francs" 715 719 (("Neustrie") ("Austrasie")) ("Royaume Franc"))

(defun FB4(baseTest)
    ; Itération sur les conflits
    (mapcan #'(lambda (conflit)
        ; Si le conflit a débuté en 523, on retourne le conflit
        (if (equal (dateDebut conflit) 523)
            conflit
        )
    )baseTest
    )
)

; Jeu de test
(FB4 BaseTest) ; renvoie ("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))

(defun FB5(baseTest)
    (mapcan #'(lambda (conflit)
        (if (AND(> (dateDebut conflit) 523)(< (dateDebut conflit) 715))
            conflit
        )
    )baseTest
    )
)

; Jeu de test
(FB5 BaseTest) ; renvoie tous les conflits ayant débuté entre 523 et 715

(defun FB6(baseTest)
    ; Création de la variable taille
    (let ((taille 0))
    ; Parcours des conflits de la base
    (mapcar #'(lambda (conflit)
        ; Parcours des ennemis
        (mapcar #'(lambda (ennemi)
            ; Si les lombards sont ennemis
            (if (equal ennemi "Lombards")
                ; on rajoute 1 à la variable taille
                (setq taille (+ taille 1))
        ))(ennemis conflit))
    )baseTest
    )
    taille
    )
)

; Jeu de test
(FB6 BaseTest) ; retourne 2

