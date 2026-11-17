;;; ============================================================
;;; UNIRPONTAS.lsp (v2)  -  sem dependencia de COM/ActiveX
;;; Para DraftSight 2020 (AutoLISP puro)
;;;
;;; Comandos disponiveis:
;;;
;;;   UNIRPONTAS - modo interativo (fica ativo ate ESC). Pergunta
;;;       tolerancia, seleciona objetos, e um quadrado do tamanho
;;;       da tolerancia acompanha o cursor. Clique perto de 2+
;;;       pontas dentro do quadrado para uni-las.
;;;
;;;   UNIRVARIASPONTAS - operacao unica (sem interacao de mouse).
;;;       Pergunta tolerancia, pede para selecionar uma area/
;;;       objetos, e une AUTOMATICAMENTE todos os grupos de pontas
;;;       proximas encontrados dentro da selecao (agrupamento em
;;;       cadeia: se A esta perto de B e B perto de C, os tres
;;;       entram no mesmo grupo mesmo que A e C estejam longe).
;;;
;;;   MOSTRARABERTOS - modo de diagnostico (fica ativo ate ESC).
;;;       Seleciona objetos e desenha um circulo vermelho em cada
;;;       ponta que NAO esta REALMENTE coincidente com outra (nao
;;;       usa a tolerancia digitada nos outros comandos - aqui o
;;;       que importa e uniao de verdade, ponta encostada em ponta).
;;;       Os circulos somem ao sair (ESC).
;;;
;;; Regra de uniao (comum aos tres comandos):
;;;   - Se todas forem pontas de LINE/LWPOLYLINE: movidas para a
;;;     posicao MEDIA entre elas.
;;;   - Se alguma for ponta de ARC: as outras sao movidas
;;;     EXATAMENTE para a ponta do arco (o arco fica parado,
;;;     ja que mover a ponta de um arco livremente mudaria seu
;;;     raio/centro - a linha/polilinha e que se ajusta a ele).
;;;
;;; Nao usa vl-load-com nem vlax-curve-*/vla-* (dependentes de
;;; automacao ActiveX/COM).
;;; ============================================================

(setq *UP-VERSION* "v2")
(setq *UP-TOL-PADRAO* 0.5)  ; tolerancia usada se o usuario so der ENTER
(setq *UP-EPS-COINCIDENCIA* 1e-4)  ; margem minima (so erro de ponto
                                     ; flutuante) usada pelo MOSTRARABERTOS
                                     ; para considerar duas pontas "de fato
                                     ; unidas" - independe da tolerancia
                                     ; digitada pelo usuario.
(setq *UP-DIAMETRO-ABERTO* 5.0)  ; diametro (fixo, em unidades de desenho)
                                   ; do circulo vermelho do MOSTRARABERTOS.
                                   ; SEMPRE o mesmo tamanho, mesmo que se
                                   ; sobreponha com outro circulo proximo.

;; ------------------------------------------------------------
;; Ponto real (X Y) de uma extremidade de ARC, a partir do
;; angulo inicial (isStart=T) ou final (isStart=nil).
;; ------------------------------------------------------------
(defun UP:arc-endpoint (edata isStart / center cx cy radius ang)
  (setq center (cdr (assoc 10 edata)))
  (setq cx (car center) cy (cadr center))
  (setq radius (cdr (assoc 40 edata)))
  (setq ang (if isStart (cdr (assoc 50 edata)) (cdr (assoc 51 edata))))
  (list (+ cx (* radius (cos ang))) (+ cy (* radius (sin ang))))
)

;; ------------------------------------------------------------
;; Devolve as "pontas livres" de uma entidade:
;;   LINE       -> (list (list p1 10) (list p2 11))
;;   ARC        -> (list (list pStart "SA") (list pEnd "EA"))
;;   LWPOLYLINE -> (list (list primeiro "FIRSTVTX")
;;                        (list ultimo  "LASTVTX"))
;;                 (nil se a polilinha for FECHADA - nao tem
;;                 pontas livres)
;; ------------------------------------------------------------
(defun UP:get-endpoints (ename / edata etype closedFlag pts first last)
  (setq edata (entget ename))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "LINE")
     (list (list (cdr (assoc 10 edata)) 10)
           (list (cdr (assoc 11 edata)) 11))
    )
    ((= etype "ARC")
     (list (list (UP:arc-endpoint edata T) "SA")
           (list (UP:arc-endpoint edata nil) "EA"))
    )
    ((= etype "LWPOLYLINE")
     (setq closedFlag (and (assoc 70 edata)
                            (= 1 (logand 1 (cdr (assoc 70 edata))))))
     (if closedFlag
       nil
       (progn
         (setq pts '())
         (foreach pair edata
           (if (= (car pair) 10) (setq pts (cons (cdr pair) pts)))
         )
         (setq pts (reverse pts))
         (setq first (car pts) last (car (reverse pts)))
         (if (and first last)
           (list (list first "FIRSTVTX") (list last "LASTVTX"))
           nil
         )
       )
     )
    )
    (T nil)
  )
)

;; ------------------------------------------------------------
;; Monta o "pool" (lista) de todas as pontas de todos os objetos
;; selecionados. Cada item: (list ponto entName whichCode entType)
;; ------------------------------------------------------------
(defun UP:build-pool (ss / n i ename eps e pool etype)
  (setq pool '() n (sslength ss) i 0)
  (repeat n
    (setq ename (ssname ss i))
    (setq etype (cdr (assoc 0 (entget ename))))
    (setq eps (UP:get-endpoints ename))
    (foreach e eps
      (setq pool (cons (list (car e) ename (cadr e) etype) pool))
    )
    (setq i (1+ i))
  )
  (reverse pool)
)

;; ------------------------------------------------------------
;; Ajusta o angulo inicial/final de um ARC para que a ponta
;; aponte na direcao do ponto "pt" (o raio/centro NAO mudam).
;; ------------------------------------------------------------
(defun UP:set-arc-angle (edata pt isStart / center cx cy ang)
  (setq center (cdr (assoc 10 edata)))
  (setq cx (car center) cy (cadr center))
  (setq ang (atan (- (cadr pt) cy) (- (car pt) cx)))
  (if isStart
    (subst (cons 50 ang) (assoc 50 edata) edata)
    (subst (cons 51 ang) (assoc 51 edata) edata)
  )
)

;; ------------------------------------------------------------
;; Troca o primeiro ou o ultimo vertice (codigo 10) de uma
;; LWPOLYLINE pelo ponto "pt".
;; ------------------------------------------------------------
(defun UP:set-poly-vertex (edata pt isFirst / total idx newlist targetIdx pt2d pair)
  (setq pt2d (list (car pt) (cadr pt)))
  (setq total 0)
  (foreach pair edata (if (= (car pair) 10) (setq total (1+ total))))
  (setq targetIdx (if isFirst 0 (1- total)))
  (setq idx 0 newlist '())
  (foreach pair edata
    (if (= (car pair) 10)
      (progn
        (if (= idx targetIdx)
          (setq newlist (append newlist (list (cons 10 pt2d))))
          (setq newlist (append newlist (list pair)))
        )
        (setq idx (1+ idx))
      )
      (setq newlist (append newlist (list pair)))
    )
  )
  newlist
)

;; ------------------------------------------------------------
;; Move a ponta descrita pelo registro "rec" (do pool) para o
;; ponto "pt", editando a entidade de verdade (entmod + entupd).
;; ------------------------------------------------------------
(defun UP:update-point (rec pt / ename whichCode edata)
  (setq ename (nth 1 rec) whichCode (nth 2 rec))
  (setq edata (entget ename))
  (cond
    ((numberp whichCode)
     (setq edata (subst (cons whichCode pt) (assoc whichCode edata) edata))
    )
    ((= whichCode "SA") (setq edata (UP:set-arc-angle edata pt T)))
    ((= whichCode "EA") (setq edata (UP:set-arc-angle edata pt nil)))
    ((= whichCode "FIRSTVTX") (setq edata (UP:set-poly-vertex edata pt T)))
    ((= whichCode "LASTVTX") (setq edata (UP:set-poly-vertex edata pt nil)))
  )
  (entmod edata)
  (entupd ename)
)

;; ------------------------------------------------------------
;; Recebe a lista de registros do pool que cairam dentro do
;; quadrado clicado, e os une:
;;   - se houver algum ARC entre eles, todos os outros vao
;;     exatamente para a ponta desse ARC (o primeiro encontrado);
;;   - senao, todos vao para o ponto MEDIO entre eles.
;; Devolve o ponto final usado (para a mensagem de confirmacao).
;; ------------------------------------------------------------
(defun UP:unir (matched / temArco refRec rec sx sy cnt target)
  (setq temArco nil refRec nil)
  (foreach rec matched
    (if (and (= (nth 3 rec) "ARC") (not temArco))
      (setq temArco T refRec rec)
    )
  )
  (if temArco
    (setq target (car refRec))
    (progn
      (setq sx 0.0 sy 0.0 cnt 0)
      (foreach rec matched
        (setq sx (+ sx (car (car rec))) sy (+ sy (cadr (car rec))) cnt (1+ cnt))
      )
      (setq target (list (/ sx cnt) (/ sy cnt)))
    )
  )
  (foreach rec matched
    (if (not (equal rec refRec))
      (UP:update-point rec target)
    )
  )
  target
)

;; ------------------------------------------------------------
;; Garante que a camada do quadrado-cursor exista.
;; ------------------------------------------------------------
(defun UP:ensure-layer (layerName colorIdx)
  (if (not (tblsearch "LAYER" layerName))
    (entmake (list (cons 0 "LAYER") (cons 2 layerName) (cons 70 0)
                   (cons 62 colorIdx) (cons 6 "CONTINUOUS")))
  )
)

;; ------------------------------------------------------------
;; Cria uma LWPOLYLINE FECHADA real (entidade de verdade, nao
;; desenho de pixel) representando o quadrado de meio-lado "tol"
;; centralizado em "pt", na camada indicada. Devolve o nome da
;; entidade criada (entmakex, para podermos apagar depois).
;;
;; Por ser uma entidade real, o proprio DraftSight cuida do
;; redesenho dela automaticamente - evita o rastro que o
;; grdraw/redraw manual estava deixando em algumas instalacoes.
;; ------------------------------------------------------------
(defun UP:make-square-ent (pt tol layerName / p1 p2 p3 p4)
  (setq p1 (list (- (car pt) tol) (- (cadr pt) tol)))
  (setq p2 (list (+ (car pt) tol) (- (cadr pt) tol)))
  (setq p3 (list (+ (car pt) tol) (+ (cadr pt) tol)))
  (setq p4 (list (- (car pt) tol) (+ (cadr pt) tol)))
  (entmakex (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbPolyline")
                  (cons 8 layerName) (cons 90 4) (cons 70 1)
                  (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)))
)

(defun c:UNIRPONTAS ( / tol ss pool cursorEnt gr codigo valor exitLoop clickPt
                         matched rec target *UP-OLDERR* ultimoAtt agora)

  (setq *UP-OLDERR* *error*)
  (defun *error* (msg / )
    (if cursorEnt (progn (entdel cursorEnt) (setq cursorEnt nil)))
    (setq *error* *UP-OLDERR*)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort")
             (/= msg "console break"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (UP:ensure-layer "UNIRPONTAS_CURSOR" 3)

  (setq tol (getdist (strcat "\nInforme a tolerancia (distancia maxima entre pontas) <"
                              (rtos *UP-TOL-PADRAO* 2 3) ">: ")))
  (if (not tol) (setq tol *UP-TOL-PADRAO*))

  (if (<= tol 0.0)
    (princ "\nTolerancia invalida. Comando cancelado.")
    (progn
      (setq ss (ssget (list (cons 0 "LINE,ARC,LWPOLYLINE")
                             (cons 8 "~MEDLIN_MARCADOR,MEDLIN_COTAS,UNIRPONTAS_CURSOR,MOSTRARABERTOS_MARCA"))))
      (if (not ss)
        (princ "\nNenhum objeto valido selecionado (use LINE, ARC ou LWPOLYLINE).")
        (progn
          (setq pool (UP:build-pool ss))
          (princ "\nModo UNIRPONTAS ativo.")
          (princ "\nClique perto de duas (ou mais) pontas dentro da tolerancia para uni-las.")
          (princ "\nPressione ESC para sair.")

          (setq cursorEnt nil exitLoop nil)
          (setq ultimoAtt (getvar "DATE"))
          (while (not exitLoop)
            (setq gr (grread T 15 0))
            (if (not gr)
              (setq exitLoop T)
              (progn
                (setq codigo (car gr) valor (cadr gr))
                (setq agora (getvar "DATE"))

                (cond
                  ((= codigo 5)
                   ;; move o quadrado: apaga a entidade anterior e cria
                   ;; uma nova na posicao atual do cursor
                   (if cursorEnt (entdel cursorEnt))
                   (setq cursorEnt (UP:make-square-ent valor tol "UNIRPONTAS_CURSOR"))
                   (setq ultimoAtt agora)
                  )
                  ((= codigo 3)
                   (if cursorEnt (progn (entdel cursorEnt) (setq cursorEnt nil)))
                   (setq clickPt valor)
                   (setq matched '())
                   (foreach rec pool
                     (if (and (<= (abs (- (car (car rec)) (car clickPt))) tol)
                              (<= (abs (- (cadr (car rec)) (cadr clickPt))) tol))
                       (setq matched (cons rec matched))
                     )
                   )
                   (cond
                     ((>= (length matched) 2)
                      (setq target (UP:unir matched))
                      (setq pool (UP:build-pool ss))
                      (princ (strcat "\n" (itoa (length matched))
                                     " ponta(s) unida(s) em X=" (rtos (car target) 2 3)
                                     " Y=" (rtos (cadr target) 2 3) "."))
                     )
                     (T
                      (princ "\nNenhum par de pontas encontrado nessa area."))
                   )
                  )
                  ((and (= codigo 2) (= valor 27))
                   (if cursorEnt (progn (entdel cursorEnt) (setq cursorEnt nil)))
                   (setq exitLoop T)
                  )
                )
              )
            )
          )
          (princ "\nModo UNIRPONTAS encerrado.")
        )
      )
    )
  )
  (setq *error* *UP-OLDERR*)
  (princ)
)


;; ============================================================
;; FUNCOES AUXILIARES COMPARTILHADAS POR UNIRVARIASPONTAS E
;; MOSTRARABERTOS
;; ============================================================

;; ------------------------------------------------------------
;; Apaga TODAS as entidades de uma camada (usado para limpar os
;; circulos do MOSTRARABERTOS ao sair, inclusive em caso de erro).
;; ------------------------------------------------------------
(defun UP:limpar-camada (layerName / ss2)
  (setq ss2 (ssget "_X" (list (cons 8 layerName))))
  (if ss2 (command "_.ERASE" ss2 ""))
  (princ)
)

;; ------------------------------------------------------------
;; Verdadeiro se o ponto do registro (nth idx pool) tiver algum
;; OUTRO ponto do pool dentro da tolerancia (ou seja, tem "par").
;; ------------------------------------------------------------
(defun UP:tem-par (idx pool tol / n j p q achou)
  (setq p (nth idx pool))
  (setq n (length pool) j 0 achou nil)
  (repeat n
    (if (/= j idx)
      (progn
        (setq q (nth j pool))
        (if (and (<= (abs (- (car (car p)) (car (car q)))) tol)
                 (<= (abs (- (cadr (car p)) (cadr (car q)))) tol))
          (setq achou T)
        )
      )
    )
    (setq j (1+ j))
  )
  achou
)

;; ------------------------------------------------------------
;; Verdadeiro se o registro "p" estiver dentro da tolerancia de
;; QUALQUER membro do grupo "grupo".
;; ------------------------------------------------------------
(defun UP:perto-de-grupo (p grupo tol / g achou)
  (setq achou nil)
  (foreach g grupo
    (if (and (<= (abs (- (car (car p)) (car (car g)))) tol)
             (<= (abs (- (cadr (car p)) (cadr (car g)))) tol))
      (setq achou T)
    )
  )
  achou
)

;; ------------------------------------------------------------
;; Agrupa o pool inteiro em "clusters" de pontas proximas, em
;; CADEIA: se A esta perto de B e B perto de C, os tres caem no
;; mesmo grupo, mesmo que A e C estejam longe um do outro.
;; Devolve uma lista de grupos (cada grupo e uma lista de
;; registros do pool).
;; ------------------------------------------------------------
(defun UP:cluster-pool (pool tol / restantes grupos grupoAtual mudou p novoRestantes)
  (setq restantes pool grupos '())
  (while restantes
    (setq grupoAtual (list (car restantes)))
    (setq restantes (cdr restantes))
    (setq mudou T)
    (while mudou
      (setq mudou nil novoRestantes '())
      (foreach p restantes
        (if (UP:perto-de-grupo p grupoAtual tol)
          (progn (setq grupoAtual (cons p grupoAtual)) (setq mudou T))
          (setq novoRestantes (cons p novoRestantes))
        )
      )
      (setq restantes novoRestantes)
    )
    (setq grupos (cons grupoAtual grupos))
  )
  grupos
)


;; ============================================================
;; UNIRVARIASPONTAS - operacao unica, sem interacao de mouse
;; ============================================================
(defun c:UNIRVARIASPONTAS ( / tol ss pool grupos grp target totalGrupos totalPontas)

  (setq tol (getdist (strcat "\nInforme a tolerancia (distancia maxima entre pontas) <"
                              (rtos *UP-TOL-PADRAO* 2 3) ">: ")))
  (if (not tol) (setq tol *UP-TOL-PADRAO*))

  (if (<= tol 0.0)
    (princ "\nTolerancia invalida. Comando cancelado.")
    (progn
      (princ "\nSelecione a area/objetos onde deseja unir as pontas proximas: ")
      (setq ss (ssget (list (cons 0 "LINE,ARC,LWPOLYLINE")
                             (cons 8 "~MEDLIN_MARCADOR,MEDLIN_COTAS,UNIRPONTAS_CURSOR,MOSTRARABERTOS_MARCA"))))
      (if (not ss)
        (princ "\nNenhum objeto valido selecionado (use LINE, ARC ou LWPOLYLINE).")
        (progn
          (setq pool (UP:build-pool ss))
          (setq grupos (UP:cluster-pool pool tol))
          (setq totalGrupos 0 totalPontas 0)
          (foreach grp grupos
            (if (>= (length grp) 2)
              (progn
                (setq target (UP:unir grp))
                (setq totalGrupos (1+ totalGrupos))
                (setq totalPontas (+ totalPontas (length grp)))
              )
            )
          )
          (if (> totalGrupos 0)
            (princ (strcat "\n" (itoa totalGrupos) " grupo(s) unido(s), totalizando "
                           (itoa totalPontas) " ponta(s)."))
            (princ "\nNenhum grupo de pontas proximas encontrado dentro da tolerancia.")
          )
        )
      )
    )
  )
  (princ)
)


;; ============================================================
;; MOSTRARABERTOS - modo de diagnostico (fica ativo ate ESC)
;; ============================================================
(defun c:MOSTRARABERTOS ( / ss pool n i raio abertos gr exitLoop *UP-OLDERR2* pt dprox)

  (setq *UP-OLDERR2* *error*)
  (defun *error* (msg / )
    (UP:limpar-camada "MOSTRARABERTOS_MARCA")
    (setq *error* *UP-OLDERR2*)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort")
             (/= msg "console break"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (UP:ensure-layer "MOSTRARABERTOS_MARCA" 1)
  ;; limpa qualquer sobra de uma execucao anterior interrompida
  (UP:limpar-camada "MOSTRARABERTOS_MARCA")

  (setq ss (ssget (list (cons 0 "LINE,ARC,LWPOLYLINE")
                         (cons 8 "~MEDLIN_MARCADOR,MEDLIN_COTAS,UNIRPONTAS_CURSOR,MOSTRARABERTOS_MARCA"))))
  (if (not ss)
    (princ "\nNenhum objeto valido selecionado (use LINE, ARC ou LWPOLYLINE).")
    (progn
      (setq pool (UP:build-pool ss))
      (setq raio (/ *UP-DIAMETRO-ABERTO* 2.0))
      (setq n (length pool) i 0 abertos 0)
      (repeat n
        ;; usa *UP-EPS-COINCIDENCIA* (fixo) - NAO a tolerancia do
        ;; usuario - porque aqui o que importa e coincidencia real
        (if (not (UP:tem-par i pool *UP-EPS-COINCIDENCIA*))
          (progn
            (setq pt (car (nth i pool)))
            (entmake (list (cons 0 "CIRCLE")
                           (cons 10 (list (car pt) (cadr pt) 0.0))
                           (cons 40 raio)
                           (cons 8 "MOSTRARABERTOS_MARCA")))
            (setq abertos (1+ abertos))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa abertos) " ponta(s) NAO unida(s) encontrada(s) (sem coincidencia real)."))
      (if (> abertos 0)
        (progn
          (princ "\nPressione ESC para fechar e limpar os circulos.")
          (setq exitLoop nil)
          (while (not exitLoop)
            (setq gr (grread T 15 0))
            (if (or (not gr) (and (= (car gr) 2) (= (cadr gr) 27)))
              (setq exitLoop T)
            )
          )
          (UP:limpar-camada "MOSTRARABERTOS_MARCA")
          (princ "\nMOSTRARABERTOS encerrado, circulos removidos.")
        )
      )
    )
  )
  (setq *error* *UP-OLDERR2*)
  (princ)
)


(princ "\n UNIRPONTAS.LSP (v2) - Encontrado Comandos")
(princ "\n Finalidade..: Unir pontas de LINE, ARC e LWPOLYLINE que estejam proximas dentro de uma tolerancia, fechando pequenas folgas entre objetos quase encostados; e localizar visualmente pontas que ficaram sem par.")
(princ "\n Comandos....: UNIRPONTAS, UNIRVARIASPONTAS, MOSTRARABERTOS")
(princ "\n Autoria.....: Rafael.K e Claudio*")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ)
