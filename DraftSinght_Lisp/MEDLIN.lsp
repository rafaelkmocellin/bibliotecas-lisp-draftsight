;;; ============================================================
;;; MEDLIN.lsp  -  sem dependencia de COM/ActiveX
;;; Para DraftSight 2020 (AutoLISP puro)
;;;
;;; Comandos disponiveis:
;;;   MEDLIN        - percorre o comprimento informado. Valor
;;;                   NEGATIVO = sentido inverso. A soma das
;;;                   cotas sempre bate com o valor digitado.
;;;   MEDLININVERSO - mesma logica do MEDLIN (sinal define o
;;;                   sentido), porem sempre no sentido CONTRARIO
;;;                   ao que o MEDLIN faria com o mesmo valor.
;;;                   ALEM DISSO, cota tambem o RESTANTE do
;;;                   percurso (o complemento) - ou seja, o trecho
;;;                   informado fica cotado normalmente e todo o
;;;                   resto da peca tambem recebe cotas em cascata,
;;;                   cobrindo o percurso inteiro. Nao pergunta
;;;                   mais Direto/Inverso.
;;;   DELLIN        - apaga TODAS as cotas e marcadores criados
;;;                   por essas rotinas em qualquer execucao
;;;                   anterior (camadas MEDLIN_COTAS e
;;;                   MEDLIN_MARCADOR).
;;;
;;; Selecione VARIOS objetos (LINE, ARC e/ou LWPOLYLINE), mesmo
;;; que NAO estejam soldados/unidos em uma unica entidade - a
;;; rotina conecta pelos pontos de extremidade coincidentes.
;;;
;;; No final:
;;;  - marca o ponto com uma linha perpendicular que "corta" o
;;;    segmento no local exato (camada MEDLIN_MARCADOR, travada
;;;    = nao editavel).
;;;  - cria uma cota alinhada PARA CADA segmento percorrido
;;;    (estilo cascata), na cor ciano (camada MEDLIN_COTAS).
;;;
;;; Nao usa vl-load-com nem vlax-curve-* (dependentes de COM).
;;; ============================================================

(setq *ML-TOL* 1e-4)
(setq *ML-DARKRED* 9109504)  ; cor verdadeira RGB(139,0,0) - vermelho escuro
(setq *ML-SETA-TAMANHO* 20.0)  ; comprimento FIXO da seta amarela de referencia

;;; ============================================================
;;; CONFIGURACOES DE SENTIDO (direcao) - todas juntas aqui.
;;; Cada uma das 3 variaveis por comando agora tem uma versao
;;; "Positivo" (usada quando o comprimento digitado e >= 0) e
;;; uma versao "Negativo" (usada quando o comprimento digitado
;;; e < 0) - totalmente independentes, sem multiplicacao.
;;;
;;;  ...PercursoMarca(Positivo/Negativo) -> decide para que lado
;;;      o PONTO/MARCADOR final caminha ao longo do percurso.
;;;      1.0 = sentido da seta amarela. -1.0 = sentido oposto.
;;;
;;;  ...PercursoCota(Positivo/Negativo) -> decide de que LADO da
;;;      linha as COTAS ficam, em relacao ao sentido do percurso
;;;      (mantem o lado que voce clicou consistente).
;;;      1.0 = normal. -1.0 = invertido.
;;;
;;;  ...Cota(Positivo/Negativo) -> decide qual ponta de cada
;;;      trecho e a "primeira" linha de extensao da cota (efeito
;;;      so cosmetico, nao muda posicao nem valor da cota).
;;;      1.0 = normal. -1.0 = invertido.
;;; ============================================================

(setq MEDLINsentidoPercursoMarcaPositivo 1.0)
(setq MEDLINsentidoPercursoMarcaNegativo -1.0)

(setq MEDLINsentidoPercursoCotaPositivo 1.0)
(setq MEDLINsentidoPercursoCotaNegativo -1.0)

(setq MEDLINsentidoCotaPositivo 1.0)
(setq MEDLINsentidoCotaNegativo -1.0)

(setq MEDLININVERSOsentidoPercursoMarcaPositivo 1.0)
(setq MEDLININVERSOsentidoPercursoMarcaNegativo -1.0)

(setq MEDLININVERSOsentidoPercursoCotaPositivo 1.0)
(setq MEDLININVERSOsentidoPercursoCotaNegativo -1.0)

(setq MEDLININVERSOsentidoCotaPositivo 1.0)
(setq MEDLININVERSOsentidoCotaNegativo 1.0)

(defun ML:pi () (* 4.0 (atan 1.0)))
(defun ML:max2 (a b) (if (> a b) a b))
(defun ML:min2 (a b) (if (< a b) a b))

(defun ML:norm-ang (ang / twopi)
  (setq twopi (* 2.0 (ML:pi)))
  (while (< ang 0.0) (setq ang (+ ang twopi)))
  (while (>= ang twopi) (setq ang (- ang twopi)))
  ang
)

(defun ML:to3d (p)
  (if (>= (length p) 3)
    (list (car p) (cadr p) (caddr p))
    (list (car p) (cadr p) 0.0))
)

(defun ML:pt-equal (p1 p2 tol)
  (and (< (abs (- (car p1) (car p2))) tol)
       (< (abs (- (cadr p1) (cadr p2))) tol))
)

(defun ML:set-assoc (lst code val)
  (if (assoc code lst)
    (subst (cons code val) (assoc code lst) lst)
    (append lst (list (cons code val)))
  )
)

;; garante que a camada exista, com a cor indicada
(defun ML:ensure-layer (layerName colorIdx)
  (if (not (tblsearch "LAYER" layerName))
    (entmake (list (cons 0 "LAYER") (cons 2 layerName) (cons 70 0)
                   (cons 62 colorIdx) (cons 6 "CONTINUOUS")))
  )
)

;; trava (lockState = T) ou destrava (lockState = nil) uma camada
;; trava (lockState = T) ou destrava (lockState = nil) uma camada,
;; usando o comando nativo -LAYER (mesmo mecanismo do painel de
;; Camadas) - mais confiavel para realmente impedir selecao do
;; que editar a flag da tabela diretamente.
(defun ML:set-layer-lock (layerName lockState)
  (command "_.-LAYER" (if lockState "_LOck" "_UNLock") layerName "")
  (princ)
)

(defun ML:closest-t (P p1 p2 / dx dy d2 tParam)
  (setq dx (- (car p2) (car p1)) dy (- (cadr p2) (cadr p1)))
  (setq d2 (+ (* dx dx) (* dy dy)))
  (if (< d2 1e-9)
    0.0
    (progn
      (setq tParam (/ (+ (* (- (car P) (car p1)) dx)
                          (* (- (cadr P) (cadr p1)) dy))
                       d2))
      (cond ((< tParam 0.0) 0.0) ((> tParam 1.0) 1.0) (T tParam))
    )
  )
)

(defun ML:pt-on-seg (p1 p2 tParam)
  (list (+ (car p1) (* tParam (- (car p2) (car p1))))
        (+ (cadr p1) (* tParam (- (cadr p2) (cadr p1))))
        0.0)
)

(defun ML:arc-endpoints (edata / center radius sa ea sweep cx cy p1 p2 bulge)
  (setq center (cdr (assoc 10 edata)))
  (setq cx (car center) cy (cadr center))
  (setq radius (cdr (assoc 40 edata)))
  (setq sa (cdr (assoc 50 edata)))
  (setq ea (cdr (assoc 51 edata)))
  (setq sweep (- ea sa))
  (if (<= sweep 1e-9) (setq sweep (+ sweep (* 2.0 (ML:pi)))))
  (setq p1 (list (+ cx (* radius (cos sa))) (+ cy (* radius (sin sa))) 0.0))
  (setq p2 (list (+ cx (* radius (cos ea))) (+ cy (* radius (sin ea))) 0.0))
  (setq bulge (/ (sin (/ sweep 4.0)) (cos (/ sweep 4.0))))
  (list p1 p2 bulge)
)

(defun ML:seg-length (p1 p2 b / d theta radius)
  (if (equal b 0.0 1e-9)
    (distance p1 p2)
    (progn
      (setq d (distance p1 p2))
      (setq theta (* 4.0 (atan b)))
      (setq radius (/ d (* 2.0 (abs (sin (/ theta 2.0))))))
      (* radius (abs theta))
    )
  )
)

(defun ML:seg-arc-info (p1 p2 b / dx dy d theta radius a sgn e2x e2y mx my)
  (setq dx (- (car p2) (car p1)) dy (- (cadr p2) (cadr p1)))
  (setq d (distance p1 p2))
  (setq theta (* 4.0 (atan b)))
  (setq radius (/ d (* 2.0 (abs (sin (/ theta 2.0))))))
  (setq a (* radius (cos (/ theta 2.0))))
  (setq sgn (if (< b 0.0) -1.0 1.0))
  (setq e2x (/ dy d) e2y (/ (- dx) d))
  (setq mx (/ (+ (car p1) (car p2)) 2.0) my (/ (+ (cadr p1) (cadr p2)) 2.0))
  (list (- mx (* sgn a e2x)) (- my (* sgn a e2y)) radius theta)
)

(defun ML:seg-point-at-dist (p1 p2 b s / dd info cx cy radius angP1 dirsgn angPt)
  (if (equal b 0.0 1e-9)
    (progn
      (setq dd (distance p1 p2))
      (if (< dd 1e-9) (setq dd 1e-9))
      (list (+ (car p1) (* (/ s dd) (- (car p2) (car p1))))
            (+ (cadr p1) (* (/ s dd) (- (cadr p2) (cadr p1))))
            0.0)
    )
    (progn
      (setq info (ML:seg-arc-info p1 p2 b))
      (setq cx (nth 0 info) cy (nth 1 info) radius (nth 2 info))
      (setq angP1 (atan (- (cadr p1) cy) (- (car p1) cx)))
      (setq dirsgn (if (< b 0.0) -1.0 1.0))
      (setq angPt (+ angP1 (* dirsgn (/ s radius))))
      (list (+ cx (* radius (cos angPt))) (+ cy (* radius (sin angPt))) 0.0)
    )
  )
)

(defun ML:seg-closest (P p1 p2 b / tt cp d info cx cy radius theta angP1 angQ sweepTotal rel seglen)
  (if (equal b 0.0 1e-9)
    (progn
      (setq tt (ML:closest-t P p1 p2))
      (setq cp (ML:pt-on-seg p1 p2 tt))
      (setq d (distance p1 p2))
      (list cp (* tt d) (distance P cp))
    )
    (progn
      (setq info (ML:seg-arc-info p1 p2 b))
      (setq cx (nth 0 info) cy (nth 1 info) radius (nth 2 info) theta (nth 3 info))
      (setq angP1 (ML:norm-ang (atan (- (cadr p1) cy) (- (car p1) cx))))
      (setq angQ  (ML:norm-ang (atan (- (cadr P) cy) (- (car P) cx))))
      (setq sweepTotal (abs theta))
      (setq rel (if (>= b 0.0)
                  (ML:norm-ang (- angQ angP1))
                  (ML:norm-ang (- angP1 angQ))))
      (setq seglen (* radius sweepTotal))
      (if (<= rel sweepTotal)
        (progn
          (setq cp (list (+ cx (* radius (cos angQ))) (+ cy (* radius (sin angQ))) 0.0))
          (list cp (* rel radius) (distance P cp))
        )
        (if (< (distance P p1) (distance P p2))
          (list p1 0.0 (distance P p1))
          (list p2 seglen (distance P p2))
        )
      )
    )
  )
)

(defun ML:chain-point (fP1 fP2 fBulge fCum fLen nc dist / idx cum found ld)
  (if (< dist 0.0) (setq dist 0.0))
  (setq idx 0 found nil)
  (while (and (< idx nc) (not found))
    (if (<= dist (+ (nth idx fCum) (nth idx fLen) 1e-9))
      (setq found T)
      (setq idx (1+ idx))
    )
  )
  (if (not found) (setq idx (1- nc)))
  (setq ld (- dist (nth idx fCum)))
  (if (< ld 0.0) (setq ld 0.0))
  (if (> ld (nth idx fLen)) (setq ld (nth idx fLen)))
  (ML:seg-point-at-dist (nth idx fP1) (nth idx fP2) (nth idx fBulge) ld)
)

(defun ML:run ( sentPercursoMarcaPos sentPercursoMarcaNeg
                sentPercursoCotaPos sentPercursoCotaNeg
                sentCotaPos sentCotaNeg
                modoComplemento
                / ss n2 i2 ent edata etype pts bulges closedFlag k2 pA pB bB
                     nvp nsegp arcInfo segP1 segP2 segBulge npool
                     pt0 bestDist bestSeg bestLocal bestCp jp p1 p2 b res cp ld dd
                     chainIdx chainOrient usedSet frontPt backPt extending kk
                     foundIdx foundOrient prependCount chainClosed nc
                     fP1 fP2 fBulge ci idxS ori fLen fCum facc fTotal
                     startDist startPt len dir dirCota sentCotaEfetivo newDist idx cum found
                     localDist finalPt
                     distA distB pIdx segStart segEnd ov1 ov2 localA localB
                     subP1 subP2 subLen contribList refPt signMul dx dy
                     segMag perpx perpy mid dotv dimPt entry sumSub avgSub
                     offsetDist dimEnt dimData avgLen markerRadius mcx mcy
                     segLenIdx epsD daBack daFwd pTangA pTangB tdx tdy tmag
                     tperpx tperpy tickP1 tickP2
                     oldClayer oldDimClrD oldDimClrE oldDimClrT
                     previewLen previewDist previewPt tempEnt
                     dirMag unitX unitY fixedPt
                     wrapCase intervals ivl textPt
                     needSwap fFront fBack nfP1 nfP2 nfBulge ri
                     middleInterval outerIntervals complementIntervals
                     *ML-OLDERR*)

  (setq *ML-OLDERR* *error*)
  (defun *error* (msg / )
    (if tempEnt (progn (entdel tempEnt) (setq tempEnt nil)))
    (if oldClayer (setvar "CLAYER" oldClayer))
    (if oldDimClrD (setvar "DIMCLRD" oldDimClrD))
    (if oldDimClrE (setvar "DIMCLRE" oldDimClrE))
    (if oldDimClrT (setvar "DIMCLRT" oldDimClrT))
    (setq *error* *ML-OLDERR*)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort")
             (/= msg "console break"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (ML:ensure-layer "MEDLIN_MARCADOR" 1)
  (ML:ensure-layer "MEDLIN_COTAS" 4)
  (ML:set-layer-lock "MEDLIN_MARCADOR" nil)

  (setq ss (ssget (list (cons 0 "LINE,ARC,LWPOLYLINE")
                         (cons 8 "~MEDLIN_MARCADOR,MEDLIN_COTAS"))))

  (if (not ss)
    (princ "\nNenhum objeto valido selecionado (use LINE, ARC ou LWPOLYLINE).")
    (progn
      (setq segP1 '() segP2 '() segBulge '())
      (setq n2 (sslength ss) i2 0)
      (repeat n2
        (setq ent (ssname ss i2))
        (setq edata (entget ent))
        (setq etype (cdr (assoc 0 edata)))
        (cond
          ((= etype "LINE")
           (setq segP1 (append segP1 (list (ML:to3d (cdr (assoc 10 edata))))))
           (setq segP2 (append segP2 (list (ML:to3d (cdr (assoc 11 edata))))))
           (setq segBulge (append segBulge (list 0.0)))
          )
          ((= etype "ARC")
           (setq arcInfo (ML:arc-endpoints edata))
           (setq segP1 (append segP1 (list (nth 0 arcInfo))))
           (setq segP2 (append segP2 (list (nth 1 arcInfo))))
           (setq segBulge (append segBulge (list (nth 2 arcInfo))))
          )
          ((= etype "LWPOLYLINE")
           (setq pts '() bulges '())
           (foreach pair edata
             (cond
               ((= (car pair) 10)
                (setq pts (cons (ML:to3d (cdr pair)) pts))
                (setq bulges (cons 0.0 bulges)))
               ((= (car pair) 42)
                (setq bulges (cons (cdr pair) (cdr bulges))))
             )
           )
           (setq pts (reverse pts) bulges (reverse bulges))
           (setq closedFlag (and (assoc 70 edata)
                                  (= 1 (logand 1 (cdr (assoc 70 edata))))))
           (setq nvp (length pts))
           (setq nsegp (if closedFlag nvp (1- nvp)))
           (setq k2 0)
           (repeat nsegp
             (setq pA (nth k2 pts))
             (setq pB (nth (rem (1+ k2) nvp) pts))
             (setq bB (nth k2 bulges))
             (setq segP1 (append segP1 (list pA)))
             (setq segP2 (append segP2 (list pB)))
             (setq segBulge (append segBulge (list bB)))
             (setq k2 (1+ k2))
           )
          )
        )
        (setq i2 (1+ i2))
      )

      (setq npool (length segP1))

      (if (< npool 1)
        (princ "\nNenhum segmento valido encontrado na selecao.")
        (progn
          (setq pt0 (getpoint "\nIndique o ponto de partida sobre o percurso: "))

          (if (not pt0)
            (princ "\nPonto nao informado. Comando cancelado.")
            (progn
              (setq bestDist 1e99 bestSeg 0 bestLocal 0.0 bestCp pt0 jp 0)
              (repeat npool
                (setq p1 (nth jp segP1) p2 (nth jp segP2) b (nth jp segBulge))
                (setq res (ML:seg-closest pt0 p1 p2 b))
                (setq cp (nth 0 res) ld (nth 1 res) dd (nth 2 res))
                (if (< dd bestDist)
                  (progn (setq bestDist dd bestSeg jp bestLocal ld bestCp cp))
                )
                (setq jp (1+ jp))
              )

              (setq chainIdx (list bestSeg) chainOrient (list 1) usedSet (list bestSeg))
              (setq frontPt (nth bestSeg segP1) backPt (nth bestSeg segP2))

              (setq extending T)
              (while extending
                (setq extending nil kk 0 foundIdx nil foundOrient nil)
                (repeat npool
                  (if (not (member kk usedSet))
                    (cond
                      ((ML:pt-equal (nth kk segP1) backPt *ML-TOL*)
                       (setq foundIdx kk foundOrient 1))
                      ((ML:pt-equal (nth kk segP2) backPt *ML-TOL*)
                       (setq foundIdx kk foundOrient -1))
                    )
                  )
                  (setq kk (1+ kk))
                )
                (if foundIdx
                  (progn
                    (setq chainIdx (append chainIdx (list foundIdx)))
                    (setq chainOrient (append chainOrient (list foundOrient)))
                    (setq usedSet (cons foundIdx usedSet))
                    (setq backPt (if (= foundOrient 1) (nth foundIdx segP2) (nth foundIdx segP1)))
                    (setq extending T)
                  )
                )
              )

              (setq prependCount 0 extending T)
              (while extending
                (setq extending nil kk 0 foundIdx nil foundOrient nil)
                (repeat npool
                  (if (not (member kk usedSet))
                    (cond
                      ((ML:pt-equal (nth kk segP2) frontPt *ML-TOL*)
                       (setq foundIdx kk foundOrient 1))
                      ((ML:pt-equal (nth kk segP1) frontPt *ML-TOL*)
                       (setq foundIdx kk foundOrient -1))
                    )
                  )
                  (setq kk (1+ kk))
                )
                (if foundIdx
                  (progn
                    (setq chainIdx (cons foundIdx chainIdx))
                    (setq chainOrient (cons foundOrient chainOrient))
                    (setq usedSet (cons foundIdx usedSet))
                    (setq frontPt (if (= foundOrient 1) (nth foundIdx segP1) (nth foundIdx segP2)))
                    (setq prependCount (1+ prependCount))
                    (setq extending T)
                  )
                )
              )

              (setq chainClosed (ML:pt-equal frontPt backPt *ML-TOL*))

              (setq nc (length chainIdx))
              (setq fP1 '() fP2 '() fBulge '() ci 0)
              (repeat nc
                (setq idxS (nth ci chainIdx) ori (nth ci chainOrient))
                (setq pA (nth idxS segP1) pB (nth idxS segP2) bB (nth idxS segBulge))
                (if (= ori 1)
                  (progn
                    (setq fP1 (append fP1 (list pA)))
                    (setq fP2 (append fP2 (list pB)))
                    (setq fBulge (append fBulge (list bB)))
                  )
                  (progn
                    (setq fP1 (append fP1 (list pB)))
                    (setq fP2 (append fP2 (list pA)))
                    (setq fBulge (append fBulge (list (- bB))))
                  )
                )
                (setq ci (1+ ci))
              )

              (setq fLen '())
              (foreach z2 (mapcar 'list fP1 fP2 fBulge)
                (setq fLen (append fLen (list (ML:seg-length (nth 0 z2) (nth 1 z2) (nth 2 z2)))))
              )

              (setq fCum '() facc 0.0)
              (foreach L fLen (setq fCum (append fCum (list facc))) (setq facc (+ facc L)))
              (setq fTotal facc)

              (setq startDist (+ (nth prependCount fCum) bestLocal))
              (setq startPt bestCp)

              ;; --- torna a orientacao da cadeia deterministica (independe de qual
              ;;     entidade foi usada como ancora / ordem de selecao): a ponta
              ;;     "canonica" (menor X, e em empate menor Y) sempre vira a
              ;;     posicao 0 (inicio). Assim, clicando no mesmo ponto, o sentido
              ;;     "Direto" e sempre o mesmo, independente de qual objeto foi
              ;;     escolhido como referencia inicial.
              (setq fFront (nth 0 fP1) fBack (nth (1- nc) fP2))
              (setq needSwap
                (or (> (car fFront) (car fBack))
                    (and (equal (car fFront) (car fBack) 1e-6) (> (cadr fFront) (cadr fBack))))
              )
              (if needSwap
                (progn
                  (setq nfP1 '() nfP2 '() nfBulge '() ri (1- nc))
                  (repeat nc
                    (setq nfP1 (append nfP1 (list (nth ri fP2))))
                    (setq nfP2 (append nfP2 (list (nth ri fP1))))
                    (setq nfBulge (append nfBulge (list (- (nth ri fBulge)))))
                    (setq ri (1- ri))
                  )
                  (setq fP1 nfP1 fP2 nfP2 fBulge nfBulge)
                  (setq fLen (reverse fLen))
                  (setq fCum '() facc 0.0)
                  (foreach L fLen (setq fCum (append fCum (list facc))) (setq facc (+ facc L)))
                  (setq startDist (- fTotal startDist))
                )
              )

              (if (> nc 1)
                (princ (strcat "\nPercurso conectado a partir de " (itoa nc) " segmento(s)."))
              )

              ;; --- seta temporaria amarela: sempre aponta no mesmo sentido fisico
              ;;     (o sentido "positivo" de referencia), igual em MEDLIN e MEDLININVERSO.
              ;;     A DIRECAO usa a mesma logica de sempre (ponto na cadeia um pouco
              ;;     a frente de startDist); o TAMANHO desenhado agora e sempre fixo
              ;;     (*ML-SETA-TAMANHO*), independente do tamanho dos segmentos. ---
              (setq avgLen (/ fTotal nc))
              (setq tempEnt nil)
              (setq previewLen (ML:min2 (* avgLen 1.5) (- fTotal startDist)))
              (if (> previewLen 1e-6)
                (progn
                  (setq previewPt (ML:chain-point fP1 fP2 fBulge fCum fLen nc (+ startDist previewLen)))
                  (setq dirMag (distance startPt previewPt))
                  (if (> dirMag 1e-9)
                    (progn
                      (setq unitX (/ (- (car previewPt) (car startPt)) dirMag))
                      (setq unitY (/ (- (cadr previewPt) (cadr startPt)) dirMag))
                      (setq fixedPt (list (+ (car startPt) (* unitX *ML-SETA-TAMANHO*))
                                          (+ (cadr startPt) (* unitY *ML-SETA-TAMANHO*))
                                          0.0))
                      (setq tempEnt (entmakex (list (cons 0 "LINE") (cons 10 startPt) (cons 11 fixedPt) (cons 62 2))))
                    )
                  )
                )
              )
              (if tempEnt
                (princ (strcat "\nSeta/linha amarela temporaria = sentido de referencia."
                               " Comprimento POSITIVO usa a config. '...Positivo';"
                               " NEGATIVO usa '...Negativo' (ver topo do arquivo)."))
              )

              (setq len (getdist pt0 "\nInforme o comprimento a percorrer (negativo = sentido inverso): "))

              ;; remove a seta temporaria assim que o comprimento for informado
              (if tempEnt (entdel tempEnt))

              (if (or (not len) (= len 0.0))
                (princ "\nComprimento invalido. Comando cancelado.")
                (progn
                  (if (< len 0.0)
                    (progn
                      (setq dir sentPercursoMarcaNeg)
                      (setq dirCota sentPercursoCotaNeg)
                      (setq sentCotaEfetivo sentCotaNeg)
                    )
                    (progn
                      (setq dir sentPercursoMarcaPos)
                      (setq dirCota sentPercursoCotaPos)
                      (setq sentCotaEfetivo sentCotaPos)
                    )
                  )
                  (setq len (abs len))

                  (setq newDist (+ startDist (* dir len)))

                  (setq wrapCase 0)
                  (cond
                    ((and chainClosed (> newDist fTotal))
                     (setq newDist (- newDist fTotal))
                     (setq wrapCase 1))
                    ((and chainClosed (< newDist 0.0))
                     (setq newDist (+ newDist fTotal))
                     (setq wrapCase -1))
                    ((> newDist fTotal)
                     (progn
                       (princ "\nAviso: o comprimento ultrapassa o final do percurso. Ajustado para o ponto final.")
                       (setq newDist fTotal)))
                    ((< newDist 0.0)
                     (progn
                       (princ "\nAviso: o comprimento ultrapassa o inicio do percurso. Ajustado para o ponto inicial.")
                       (setq newDist 0.0)))
                  )

                  (setq idx 0 found nil)
                  (while (and (< idx nc) (not found))
                    (if (<= newDist (+ (nth idx fCum) (nth idx fLen) 1e-9))
                      (setq found T)
                      (setq idx (1+ idx))
                    )
                  )
                  (if (not found) (setq idx (1- nc)))

                  (setq localDist (- newDist (nth idx fCum)))
                  (if (< localDist 0.0) (setq localDist 0.0))
                  (if (> localDist (nth idx fLen)) (setq localDist (nth idx fLen)))

                  (setq finalPt (ML:seg-point-at-dist (nth idx fP1) (nth idx fP2)
                                                       (nth idx fBulge) localDist))

                  (entmake (list (cons 0 "POINT") (cons 10 finalPt) (cons 8 "MEDLIN_MARCADOR")
                                 (cons 420 *ML-DARKRED*)))

                  ;; --- marcador: linha perpendicular que corta o segmento no ponto final ---
                  (setq markerRadius (* avgLen 0.15))
                  (if (< markerRadius 1e-6) (setq markerRadius 1.0))
                  (setq mcx (car finalPt) mcy (cadr finalPt))

                  (setq segLenIdx (nth idx fLen))
                  (setq epsD (ML:min2 (* segLenIdx 0.01) (* markerRadius 0.5)))
                  (if (< epsD 1e-6) (setq epsD 1e-6))
                  (setq daBack (- localDist epsD))
                  (if (< daBack 0.0) (setq daBack 0.0))
                  (setq daFwd (+ localDist epsD))
                  (if (> daFwd segLenIdx) (setq daFwd segLenIdx))
                  (setq pTangA (ML:seg-point-at-dist (nth idx fP1) (nth idx fP2) (nth idx fBulge) daBack))
                  (setq pTangB (ML:seg-point-at-dist (nth idx fP1) (nth idx fP2) (nth idx fBulge) daFwd))
                  (setq tdx (- (car pTangB) (car pTangA)) tdy (- (cadr pTangB) (cadr pTangA)))
                  (setq tmag (distance pTangA pTangB))
                  (if (< tmag 1e-9) (setq tmag 1e-9))
                  (setq tperpx (/ tdy tmag) tperpy (/ (- tdx) tmag))
                  (setq tickP1 (list (- mcx (* markerRadius tperpx)) (- mcy (* markerRadius tperpy)) 0.0))
                  (setq tickP2 (list (+ mcx (* markerRadius tperpx)) (+ mcy (* markerRadius tperpy)) 0.0))
                  (entmake (list (cons 0 "LINE") (cons 10 tickP1) (cons 11 tickP2)
                                 (cons 8 "MEDLIN_MARCADOR") (cons 420 *ML-DARKRED*)))

                  ;; --- anotacao no marcador ---
                  (setq textPt (list (+ mcx (* tperpx markerRadius 3.0))
                                     (+ mcy (* tperpy markerRadius 3.0)) 0.0))
                  (entmake (list (cons 0 "LINE") (cons 10 finalPt) (cons 11 textPt)
                                 (cons 8 "MEDLIN_MARCADOR") (cons 420 *ML-DARKRED*)))
                  (entmake (list (cons 0 "TEXT") (cons 10 textPt) (cons 40 8.0)
                                 (cons 1 "Use DELLIN p/ apagar") (cons 8 "MEDLIN_MARCADOR")
                                 (cons 420 *ML-DARKRED*)))

                  (ML:set-layer-lock "MEDLIN_MARCADOR" T)

                  (princ (strcat "\nPonto marcado a " (rtos len 2 3) " unidades, sentido "
                                 (if (= dir 1.0) "Direto" "Inverso") "."))
                  (princ (strcat "\nCoordenadas: X=" (rtos (car finalPt) 2 3)
                                 "  Y=" (rtos (cadr finalPt) 2 3)))

                  ;; --- sub-trechos cobertos pela medida (trata "volta" em percurso fechado) ---
                  (setq middleInterval (list (ML:min2 startDist newDist) (ML:max2 startDist newDist)))
                  (setq outerIntervals (list (list 0.0 (nth 0 middleInterval))
                                              (list (nth 1 middleInterval) fTotal)))
                  (if (= wrapCase 0)
                    (progn
                      (setq intervals (list middleInterval))
                      (setq complementIntervals outerIntervals)
                    )
                    (progn
                      (setq intervals outerIntervals)
                      (setq complementIntervals (list middleInterval))
                    )
                  )
                  ;; MEDLININVERSO: cota APENAS o restante do percurso (complemento),
                  ;; sem cotar o trecho medido em si.
                  (if modoComplemento
                    (progn
                      (setq intervals complementIntervals)
                      (princ "\nModo complemento (MEDLININVERSO): cotando o restante do percurso.")
                    )
                  )
                  (setq contribList '())
                  (foreach ivl intervals
                    (setq distA (nth 0 ivl) distB (nth 1 ivl))
                    (setq pIdx 0)
                    (repeat nc
                      (setq segStart (nth pIdx fCum) segEnd (+ (nth pIdx fCum) (nth pIdx fLen)))
                      (setq ov1 (ML:max2 segStart distA) ov2 (ML:min2 segEnd distB))
                      (if (> (- ov2 ov1) 1e-6)
                        (progn
                          (setq localA (- ov1 segStart) localB (- ov2 segStart))
                          (setq subP1 (ML:seg-point-at-dist (nth pIdx fP1) (nth pIdx fP2) (nth pIdx fBulge) localA))
                          (setq subP2 (ML:seg-point-at-dist (nth pIdx fP1) (nth pIdx fP2) (nth pIdx fBulge) localB))
                          (setq subLen (- localB localA))
                          (setq contribList (append contribList (list (list subP1 subP2 subLen))))
                        )
                      )
                      (setq pIdx (1+ pIdx))
                    )
                  )

                  (if (and contribList (> (length contribList) 0))
                    (progn
                      (setq refPt (getpoint "\nClique de um lado da linha para posicionar todas as cotas (ESC = nao criar cotas): "))
                      (if refPt
                        (progn
                          (setq entry (nth 0 contribList))
                          (setq subP1 (nth 0 entry) subP2 (nth 1 entry))
                          ;; direcao efetiva = relativa ao sentido de percurso das COTAS
                          ;; (dirCota), assim o "lado" escolhido pelo usuario nao inverte
                          (setq dx (* dirCota (- (car subP2) (car subP1)))
                                dy (* dirCota (- (cadr subP2) (cadr subP1))))
                          (setq segMag (distance subP1 subP2))
                          (if (< segMag 1e-9) (setq segMag 1e-9))
                          (setq perpx (/ dy segMag) perpy (/ (- dx) segMag))
                          (setq mid (list (/ (+ (car subP1) (car subP2)) 2.0)
                                          (/ (+ (cadr subP1) (cadr subP2)) 2.0)))
                          (setq dotv (+ (* perpx (- (car refPt) (car mid)))
                                        (* perpy (- (cadr refPt) (cadr mid)))))
                          (setq signMul (if (>= dotv 0.0) 1.0 -1.0))

                          (setq sumSub 0.0)
                          (foreach e contribList (setq sumSub (+ sumSub (nth 2 e))))
                          (setq avgSub (/ sumSub (length contribList)))
                          (setq offsetDist (* avgSub 0.35))
                          (if (< offsetDist 1e-6) (setq offsetDist markerRadius))

                          ;; ativa camada e cores ciano temporariamente
                          (setq oldClayer (getvar "CLAYER"))
                          (setq oldDimClrD (getvar "DIMCLRD"))
                          (setq oldDimClrE (getvar "DIMCLRE"))
                          (setq oldDimClrT (getvar "DIMCLRT"))
                          (setvar "CLAYER" "MEDLIN_COTAS")
                          (setvar "DIMCLRD" 4)
                          (setvar "DIMCLRE" 4)
                          (setvar "DIMCLRT" 4)

                          (foreach entry contribList
                            (setq subP1 (nth 0 entry) subP2 (nth 1 entry) subLen (nth 2 entry))
                            (setq dx (* dirCota (- (car subP2) (car subP1)))
                                  dy (* dirCota (- (cadr subP2) (cadr subP1))))
                            (setq segMag (distance subP1 subP2))
                            (if (< segMag 1e-9) (setq segMag 1e-9))
                            (setq perpx (/ dy segMag) perpy (/ (- dx) segMag))
                            (setq mid (list (/ (+ (car subP1) (car subP2)) 2.0)
                                            (/ (+ (cadr subP1) (cadr subP2)) 2.0)))
                            (setq dimPt (list (+ (car mid) (* signMul offsetDist perpx))
                                               (+ (cadr mid) (* signMul offsetDist perpy))
                                               0.0))
                            ;; sentCotaEfetivo define qual ponta e a "primeira" linha de extensao
                            (if (< sentCotaEfetivo 0.0)
                              (command "_.DIMALIGNED" subP2 subP1 dimPt)
                              (command "_.DIMALIGNED" subP1 subP2 dimPt)
                            )
                            (setq dimEnt (entlast))
                            (if (and dimEnt (= (cdr (assoc 0 (entget dimEnt))) "DIMENSION"))
                              (progn
                                (setq dimData (entget dimEnt))
                                (setq dimData (ML:set-assoc dimData 1 (rtos subLen 2 3)))
                                (setq dimData (ML:set-assoc dimData 62 4))
                                (entmod dimData)
                                (entupd dimEnt)
                              )
                            )
                          )

                          (setvar "CLAYER" oldClayer)
                          (setvar "DIMCLRD" oldDimClrD)
                          (setvar "DIMCLRE" oldDimClrE)
                          (setvar "DIMCLRT" oldDimClrT)

                          (princ (strcat "\n" (itoa (length contribList)) " cota(s) criada(s) em cascata (ciano)."))
                        )
                        (princ "\nCriacao de cotas cancelada pelo usuario.")
                      )
                    )
                  )
                  (princ)
                )
              )
            )
          )
        )
      )
    )
  )
  (setq *error* *ML-OLDERR*)
  (princ)
)

(defun c:MEDLIN ()
  (ML:run MEDLINsentidoPercursoMarcaPositivo MEDLINsentidoPercursoMarcaNegativo
          MEDLINsentidoPercursoCotaPositivo MEDLINsentidoPercursoCotaNegativo
          MEDLINsentidoCotaPositivo MEDLINsentidoCotaNegativo
          nil)
  (princ)
)

(defun c:MEDLININVERSO ()
  (ML:run MEDLININVERSOsentidoPercursoMarcaPositivo MEDLININVERSOsentidoPercursoMarcaNegativo
          MEDLININVERSOsentidoPercursoCotaPositivo MEDLININVERSOsentidoPercursoCotaNegativo
          MEDLININVERSOsentidoCotaPositivo MEDLININVERSOsentidoCotaNegativo
          T)
  (princ)
)

(defun c:DELLIN ( / ss1 ss2 total)
  (ML:set-layer-lock "MEDLIN_MARCADOR" nil)
  (setq total 0)
  (setq ss1 (ssget "_X" (list (cons 8 "MEDLIN_MARCADOR"))))
  (setq ss2 (ssget "_X" (list (cons 8 "MEDLIN_COTAS"))))
  (if ss1 (progn (setq total (+ total (sslength ss1))) (command "_.ERASE" ss1 "")))
  (if ss2 (progn (setq total (+ total (sslength ss2))) (command "_.ERASE" ss2 "")))
  (if (= total 0)
    (princ "\nNenhuma cota ou marcador do MEDLIN encontrado no desenho.")
    (princ (strcat "\n" (itoa total) " objeto(s) removido(s) (cotas + marcador do MEDLIN)."))
  )
  (princ)
)

;; cabesalho do comando inicializamento ...
(princ "\n MEDLIN.LSP (v15)- Encontrado Comandos")
(princ "\n Finalidade..: Medir comprimentos e gerar cotas em cascata ao longo de percursos formados por objetos soltos ou unidos (LINE, ARC, LWPOLYLINE).")
(princ "\n Comandos....: MEDLIN, MEDLININVERSO, DELLIN")
(princ "\n Autoria.....: Rafael.K e Claudio*")
(princ "\n  ")
(princ)