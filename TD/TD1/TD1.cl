; Fonction qui transforme une expression infixe en expression prefixe

(defun transformation(liste)
	(if (> (length liste) 0)
		(list (cadr liste) (transformation (car liste))(transformation (cddr liste)))
	)
)