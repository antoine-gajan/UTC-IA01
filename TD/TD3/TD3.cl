(setq l '
    (html 
        (header 
            (title
                "Ma page"
            )   
        )
        (body
            (h1
                "Un titre"
            )
            (p
                "Soror et aemula Romae"
            )
        )
    )
)

(defun make-html (liste i file)
    ; Si la liste envoyée est bien une liste
    (if (listp liste)
        (progn
            ; Affichage de la balise ouvrante et de car de liste
            (format file "~V@t<~s>~%" i (car liste))
                ; Pour tous les éléments du cdr liste
                (mapcar #'(lambda (l)
                    ; Appel en profondeur
                    (make-html l (+ i 4))
                    )(cdr liste)
                ) 
            ; Affichage de la balise fermante avec car liste
            (format file "~V@t</~s>~%" i (car liste))
        )
        ; Sinon, affichage de la liste
        (format file "~V@t~a~%" (+ i 4) liste)
    )
)

; Question 3 : création du fichier html
(defun make-html-file (liste)
    (with-open-file
        (file "td3.html"
            :if-does-not-exist :create
            :if-exists :overwrite
            :direction :output)
        (make-html liste 0 file)
    )
)