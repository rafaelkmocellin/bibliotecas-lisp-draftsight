;;; ============================================================
;;; GRAVIDADE.LSP  (v10)
;;; Ferramenta de posicionamento manual "por gravidade" para
;;; DraftSight 2020 Professional (AutoLISP puro, SEM ActiveX/COM)
;;;
;;; NOVIDADE NESSA VERSAO -- CAIXA COMO FOLGA EXTRA, NAO SUBSTITUTA:
;;;  Antes, editar a caixa no GRAVBOX fazia ela SUBSTITUIR a forma
;;;  real da peca (o teste de arestas parava de rodar pra aquela
;;;  peca). Agora, a diferenca entre a caixa editada e o contorno
;;;  real vira uma FOLGA EXTRA por lado (esquerda/direita/baixo/
;;;  cima), somada ao gap normal. O teste de arestas continua
;;;  SEMPRE rodando na forma real -- a caixa so adiciona distancia
;;;  extra de seguranca no lado que voce esticou.
;;;  A caixa mostrada na tela (GRAVBOX / tecla B) reflete o
;;;  contorno automatico (bbox real, com bulge) + qualquer folga
;;;  que voce tenha configurado.
;;;
;;; COMO USAR:
;;;  1. (load "caminho/gravidade.lsp")
;;;  2. Comando GRAVSET  -> selecione as pecas "obstaculo",
;;;     informe o espacamento (gap) e, se quiser (opcional),
;;;     selecione o retangulo que representa a chapa
;;;  3. Comando GRAVIDADE -> selecione a peca, aperte W/A/S/D
;;;     (sem precisar de Enter) para mandar ela na direcao, ou
;;;     aperte B para mostrar/esconder as caixas de colisao.
;;;     Fica em loop continuo -- aperte ESC para sair.
;;;  4. Comando GRAVBOX (a qualquer momento) -> mostra as caixas
;;;     e permite editar (arrastar grips) para dar folga extra
;;;     nalgum lado especifico. Rode GRAVBOX de novo para SALVAR
;;;     (vira folga extra, sem substituir a forma real) e esconder.
;;;  5. Se voce mover, criar ou apagar pecas fora do fluxo normal,
;;;     ou quiser mudar o gap, rode GRAVSET de novo.
;;;
;;; LIMITACOES:
;;;  - POLYLINE antiga (nao LWPOLYLINE) ainda trata vertices com
;;;    bulge como reta.
;;;  - Se DUAS pecas ja estao sobrepostas antes de comecar um
;;;    movimento, o algoritmo pode deixar de reconhecer essa peca
;;;    como obstaculo naquela direcao especifica. Separe
;;;    manualmente antes de continuar.
;;;  - A busca fina (creep) vai ate 200mm alem do contato por
;;;    bbox; se a forma real nao tocar em ninguem dentro desse
;;;    limite, a rotina usa a aproximacao por bbox (sem aplicar
;;;    a folga extra da peca nesse caso raro de fallback).
;;;  - Ao decidir se o obstaculo mais proximo e uma peca ou a
;;;    chapa, a folga extra AINDA NAO entra nessa comparacao
;;;    inicial (so entra depois, no calculo fino) -- em folgas
;;;    grandes (muitos mm) isso podia, em teoria, escolher o
;;;    "vencedor" errado num caso bem particular. Para folgas
;;;    tipicas (poucos mm) isso nao deve ser perceptivel.
;;;  - Se NENHUM obstaculo for encontrado na direcao, a peca e
;;;    movida 10000mm nessa direcao.
;;;  - O limite da chapa continua exato via bounding box.
;;; ============================================================

;; ============================================================
;; BULGE (arcos dentro de LWPOLYLINE) -- subdivisao recursiva
;; ============================================================

(defun GRAV-BULGE-MID (p1 p2 b / dx dy chordLen sag perpx perpy mx my)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq chordLen (sqrt (+ (* dx dx) (* dy dy))))
  (if (< chordLen 1e-9)
    p1
    (progn
      (setq sag (/ (* b chordLen) 2.0))
      (setq perpx (/ dy chordLen))
      (setq perpy (/ (- dx) chordLen))
      (setq mx (+ (/ (+ (car p1) (car p2)) 2.0) (* perpx sag)))
      (setq my (+ (/ (+ (cadr p1) (cadr p2)) 2.0) (* perpy sag)))
      (list mx my)
    )
  )
)

(defun GRAV-HALF-BULGE (b)
  (/ b (+ (sqrt (+ 1.0 (* b b))) 1.0))
)

(defun GRAV-BULGE-PTS (p1 p2 b depth / mid hb pts1 pts2)
  (if (or (equal b 0.0 1e-9) (< depth 1))
    (list p1)
    (progn
      (setq mid (GRAV-BULGE-MID p1 p2 b))
      (setq hb (GRAV-HALF-BULGE b))
      (setq pts1 (GRAV-BULGE-PTS p1 mid hb (1- depth)))
      (setq pts2 (GRAV-BULGE-PTS mid p2 hb (1- depth)))
      (append pts1 pts2)
    )
  )
)

(defun GRAV-LWPOLY-VERTS (edata / pts pt bl item)
  (setq pts nil)
  (setq pt nil) (setq bl 0.0)
  (foreach item edata
    (cond
      ((= (car item) 10)
       (if pt (setq pts (cons (cons pt bl) pts)))
       (setq pt (cdr item))
       (setq bl 0.0)
      )
      ((= (car item) 42)
       (setq bl (cdr item))
      )
    )
  )
  (if pt (setq pts (cons (cons pt bl) pts)))
  (reverse pts)
)

(defun GRAV-LWPOLY-EXPAND (ent / edata verts n i v1 v2 pts closed flags)
  (setq edata (entget ent))
  (setq verts (GRAV-LWPOLY-VERTS edata))
  (setq n (length verts))
  (setq flags (cdr (assoc 70 edata)))
  (if (not flags) (setq flags 0))
  (setq closed (= 1 (logand flags 1)))
  (setq pts nil)
  (setq i 0)
  (repeat (if closed n (max 0 (1- n)))
    (setq v1 (nth i verts))
    (setq v2 (nth (rem (1+ i) n) verts))
    (setq pts (append pts (GRAV-BULGE-PTS (car v1) (car v2) (cdr v1) 3)))
    (setq i (1+ i))
  )
  (if (and (not closed) (> n 0))
    (setq pts (append pts (list (car (nth (1- n) verts)))))
  )
  pts
)

;; ============================================================
;; BOUNDING BOX (retangulo envolvente REAL, sem folga) -- usado
;; para o descarte rapido e como base pra caixa visual
;; ============================================================

(defun GRAV-BBOX-PTS (pts / minx miny maxx maxy p)
  (setq minx (car (car pts)))  (setq maxx minx)
  (setq miny (cadr (car pts))) (setq maxy miny)
  (foreach p pts
    (if (< (car p) minx) (setq minx (car p)))
    (if (> (car p) maxx) (setq maxx (car p)))
    (if (< (cadr p) miny) (setq miny (cadr p)))
    (if (> (cadr p) maxy) (setq maxy (cadr p)))
  )
  (list (list minx miny) (list maxx maxy))
)

(defun GRAV-ANG-NORM (a / twopi)
  (setq twopi (* 2.0 pi))
  (while (< a 0.0) (setq a (+ a twopi)))
  (while (>= a twopi) (setq a (- a twopi)))
  a
)

(defun GRAV-ANG-IN-RANGE (a a1 a2)
  (setq a  (GRAV-ANG-NORM a))
  (setq a1 (GRAV-ANG-NORM a1))
  (setq a2 (GRAV-ANG-NORM a2))
  (if (<= a1 a2)
    (and (>= a a1) (<= a a2))
    (or (>= a a1) (<= a a2))
  )
)

(defun GRAV-ARC-BBOX (c r a1 a2 / pts angs a)
  (setq pts (list (polar c a1 r) (polar c a2 r)))
  (setq angs (list 0.0 (/ pi 2.0) pi (* 1.5 pi)))
  (foreach a angs
    (if (GRAV-ANG-IN-RANGE a a1 a2)
      (setq pts (cons (polar c a r) pts))
    )
  )
  (GRAV-BBOX-PTS pts)
)

(defun GRAV-BBOX-GEOM (ent / edata etype p1 p2 c r a1 a2 pts)
  (setq edata (entget ent))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "LINE")
     (setq p1 (cdr (assoc 10 edata)))
     (setq p2 (cdr (assoc 11 edata)))
     (GRAV-BBOX-PTS (list p1 p2))
    )
    ((= etype "CIRCLE")
     (setq c (cdr (assoc 10 edata)))
     (setq r (cdr (assoc 40 edata)))
     (list (list (- (car c) r) (- (cadr c) r)) (list (+ (car c) r) (+ (cadr c) r)))
    )
    ((= etype "ARC")
     (setq c  (cdr (assoc 10 edata)))
     (setq r  (cdr (assoc 40 edata)))
     (setq a1 (cdr (assoc 50 edata)))
     (setq a2 (cdr (assoc 51 edata)))
     (GRAV-ARC-BBOX c r a1 a2)
    )
    ((= etype "LWPOLYLINE")
     (setq pts (GRAV-LWPOLY-EXPAND ent))
     (if pts (GRAV-BBOX-PTS pts) nil)
    )
    ((= etype "POLYLINE")
     (setq pts nil)
     (setq p1 (entnext ent))
     (while (and p1 (= (cdr (assoc 0 (entget p1))) "VERTEX"))
       (setq pts (cons (cdr (assoc 10 (entget p1))) pts))
       (setq p1 (entnext p1))
     )
     (if pts (GRAV-BBOX-PTS pts) nil)
    )
    (T nil)
  )
)

(defun GRAV-BBOX-ZOOM (ent / vc vs scr asp w h minpt maxpt)
  (setq *GRAV-ZOOMS* (1+ *GRAV-ZOOMS*))
  (command "._ZOOM" "_Object" ent "")
  (setq vc  (getvar "VIEWCTR"))
  (setq vs  (getvar "VIEWSIZE"))
  (setq scr (getvar "SCREENSIZE"))
  (setq asp (/ (float (car scr)) (float (cadr scr))))
  (setq w (* vs asp))
  (setq h vs)
  (setq minpt (list (- (car vc) (/ w 2.0)) (- (cadr vc) (/ h 2.0))))
  (setq maxpt (list (+ (car vc) (/ w 2.0)) (+ (cadr vc) (/ h 2.0))))
  (list minpt maxpt)
)

;; Bbox REAL (sem folga do GRAVBOX) -- usado no escaneamento rapido
(defun GRAV-BBOX (ent / g)
  (setq g (GRAV-BBOX-GEOM ent))
  (if g
    g
    (progn
      (princ "\n(Aviso: entidade nao suportada pelo calculo exato -- usando ZOOM, menos preciso.)")
      (GRAV-BBOX-ZOOM ent)
    )
  )
)

;; Folga extra por lado (padLeft padRight padBottom padTop), do
;; GRAVBOX. Sem override, e (0 0 0 0).
(defun GRAV-PIECE-PAD (ent / pair)
  (setq pair (assoc ent *GRAV-OVERRIDES*))
  (if pair (cdr pair) (list 0.0 0.0 0.0 0.0))
)

;; Bbox real INFLADO pela folga -- so pra visualizacao (GRAVBOX)
(defun GRAV-PADDED-BBOX (ent / bbox pad minpt maxpt)
  (setq bbox (GRAV-BBOX ent))
  (setq pad (GRAV-PIECE-PAD ent))
  (setq minpt (car bbox)) (setq maxpt (cadr bbox))
  (list
    (list (- (car minpt) (nth 0 pad)) (- (cadr minpt) (nth 2 pad)))
    (list (+ (car maxpt) (nth 1 pad)) (+ (cadr maxpt) (nth 3 pad)))
  )
)

(defun GRAV-RESTORE-VIEW ( / k)
  (if (> *GRAV-ZOOMS* 0)
    (progn
      (setq k (min *GRAV-ZOOMS* 10))
      (repeat k (command "._ZOOM" "_Previous"))
    )
  )
  (setq *GRAV-ZOOMS* 0)
)

;; ============================================================
;; FORMA REAL -- pontos do contorno, arestas, intersecao,
;; ponto-dentro-do-poligono (adaptado do VERIFICA_COLISAO.LSP)
;; ============================================================

(defun GRAV-RECT-CORNERS (bbox / minpt maxpt)
  (setq minpt (car bbox)) (setq maxpt (cadr bbox))
  (list minpt (list (car maxpt) (cadr minpt)) maxpt (list (car minpt) (cadr maxpt)))
)

;; SEMPRE devolve a forma real (bulge incluso) -- a folga do
;; GRAVBOX NAO substitui mais isso, so soma depois no calculo.
(defun GRAV-ENT-POLY-PTS (ent / edata etype pts c r i ang n)
  (setq edata (entget ent))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "LWPOLYLINE")
     (setq pts (GRAV-LWPOLY-EXPAND ent))
     (if pts pts (GRAV-RECT-CORNERS (GRAV-BBOX ent)))
    )
    ((= etype "POLYLINE")
     (setq pts nil)
     (setq c (entnext ent))
     (while (and c (= (cdr (assoc 0 (entget c))) "VERTEX"))
       (setq pts (cons (cdr (assoc 10 (entget c))) pts))
       (setq c (entnext c))
     )
     (setq pts (reverse pts))
     (if pts pts (GRAV-RECT-CORNERS (GRAV-BBOX ent)))
    )
    ((= etype "CIRCLE")
     (setq c (cdr (assoc 10 edata)))
     (setq r (cdr (assoc 40 edata)))
     (setq pts nil)
     (setq n 36)
     (setq i 0)
     (repeat n
       (setq ang (* 2.0 pi (/ (float i) (float n))))
       (setq pts (cons (polar c ang r) pts))
       (setq i (1+ i))
     )
     pts
    )
    (T (GRAV-RECT-CORNERS (GRAV-BBOX ent)))
  )
)

(defun GRAV-TRANSLATE-PTS (pts dx dy / result p)
  (setq result nil)
  (foreach p pts
    (setq result (cons (list (+ (car p) dx) (+ (cadr p) dy)) result))
  )
  (reverse result)
)

(defun GRAV-POLY-EDGES (pts / edges n i p1 p2)
  (setq n (length pts))
  (setq edges nil)
  (setq i 0)
  (repeat n
    (setq p1 (nth i pts))
    (setq p2 (nth (rem (1+ i) n) pts))
    (setq edges (cons (list p1 p2) edges))
    (setq i (1+ i))
  )
  edges
)

(defun GRAV-EDGES-CROSS-P (edges1 edges2 / touch e1 e2 ip)
  (setq touch nil)
  (foreach e1 edges1
    (if (not touch)
      (foreach e2 edges2
        (if (not touch)
          (progn
            (setq ip (inters (car e1) (cadr e1) (car e2) (cadr e2) T))
            (if ip (setq touch T))
          )
        )
      )
    )
  )
  touch
)

(defun GRAV-PT-IN-POLY-P (pt pts / n c i j xi yi xj yj px py)
  (setq px (car pt) py (cadr pt))
  (setq n (length pts))
  (setq c nil)
  (setq i 0)
  (setq j (1- n))
  (while (< i n)
    (setq xi (car (nth i pts)) yi (cadr (nth i pts)))
    (setq xj (car (nth j pts)) yj (cadr (nth j pts)))
    (if (and (not (eq (> yi py) (> yj py)))
             (< px (+ xi (* (/ (- py yi) (- yj yi)) (- xj xi))))
        )
      (setq c (not c))
    )
    (setq j i)
    (setq i (1+ i))
  )
  c
)

(defun GRAV-POLY-TOUCH-PTS (pts1 pts2 / edges1 edges2 touch)
  (setq edges1 (GRAV-POLY-EDGES pts1))
  (setq edges2 (GRAV-POLY-EDGES pts2))
  (setq touch (GRAV-EDGES-CROSS-P edges1 edges2))
  (if (not touch)
    (if (or (GRAV-PT-IN-POLY-P (car pts1) pts2)
            (GRAV-PT-IN-POLY-P (car pts2) pts1))
      (setq touch T)
    )
  )
  touch
)

;; Avanca 0.5mm por vez (depende qdo valor q foi colocado)a partir do contato por bbox, testando a
;; forma real. Retorna (contactPos . obstaculoAtingido) ou nil.
(defun GRAV-CREEP-CONTACT (ent dir rawContact corridorObs minX minY maxX maxY
                            / step maxSteps k off myPts testPts obsCache pair
                              collided contactPos hitEnt o)
  (if (not *GRAV-STEP*) (setq *GRAV-STEP* 0.5))
  (setq step *GRAV-STEP*)
  (setq maxSteps (fix (/ 200.0 step))) ;; sempre busca ate ~200mm, seja qual for o passo
  (setq myPts (GRAV-ENT-POLY-PTS ent))
  (setq obsCache nil)
  (foreach o corridorObs
    (setq obsCache (cons (cons o (GRAV-ENT-POLY-PTS o)) obsCache))
  )

  (setq k 0)
  (setq collided nil)
  (setq contactPos nil)
  (setq hitEnt nil)

  (while (and (not collided) (< k maxSteps))
    (cond
      ((= dir "Cima")
       (setq off (- (+ rawContact (* k step)) maxY))
       (setq testPts (GRAV-TRANSLATE-PTS myPts 0.0 off)))
      ((= dir "Baixo")
       (setq off (- (- rawContact (* k step)) minY))
       (setq testPts (GRAV-TRANSLATE-PTS myPts 0.0 off)))
      ((= dir "Direita")
       (setq off (- (+ rawContact (* k step)) maxX))
       (setq testPts (GRAV-TRANSLATE-PTS myPts off 0.0)))
      ((= dir "Esquerda")
       (setq off (- (- rawContact (* k step)) minX))
       (setq testPts (GRAV-TRANSLATE-PTS myPts off 0.0)))
    )

    (foreach pair obsCache
      (if (not collided)
        (if (GRAV-POLY-TOUCH-PTS testPts (cdr pair))
          (progn (setq collided T) (setq hitEnt (car pair)))
        )
      )
    )

    (if collided
      (cond
        ((= dir "Cima")     (setq contactPos (+ rawContact (* k step))))
        ((= dir "Baixo")    (setq contactPos (- rawContact (* k step))))
        ((= dir "Direita")  (setq contactPos (+ rawContact (* k step))))
        ((= dir "Esquerda") (setq contactPos (- rawContact (* k step))))
      )
    )
    (setq k (1+ k))
  )

  (if contactPos (cons contactPos hitEnt) nil)
)

;; ============================================================
;; SELECAO, TECLAS, COR/LINETYPE, CAIXAS VISUAIS
;; ============================================================

(defun GRAV-IS-BOX-ENT (ent / found pair)
  (setq found nil)
  (foreach pair *GRAV-BOX-LINKS*
    (if (equal (car pair) ent) (setq found T))
  )
  found
)

(defun GRAV-PICK-ENT ( / ss ent pick)
  (setq ent nil)
  (setq pick T)
  (while pick
    (setq ss (ssget "_I"))
    (if ss
      (setq ent (ssname ss 0))
      (setq ent (car (entsel "\nSelecione a peca a mover: ")))
    )
    (cond
      ((not ent) (setq pick nil))
      ((GRAV-IS-BOX-ENT ent)
       (princ "\n(Isso e uma caixa de colisao, nao uma peca -- selecione a peca real.)")
       (setq ent nil)
      )
      (T (setq pick nil))
    )
  )
  ent
)

(defun GRAV-READ-DIR (ent / code val dir)
  (setq dir nil)
  (while (not dir)
    (setq code (grread))
    (if (= (car code) 2)
      (progn
        (setq val (cadr code))
        (cond
          ((member val '(87 119)) (setq dir "Cima"))
          ((member val '(83 115)) (setq dir "Baixo"))
          ((member val '(65 97))  (setq dir "Esquerda"))
          ((member val '(68 100)) (setq dir "Direita"))
          ((member val '(66 98))
           (setq *GRAV-SHOW-BOXES* (not *GRAV-SHOW-BOXES*))
           (if *GRAV-SHOW-BOXES*
             (progn (GRAV-REFRESH-BBOXES) (princ "\nCaixas de colisao: LIGADAS."))
             (progn (GRAV-CLEAR-BBOXES) (princ "\nCaixas de colisao: DESLIGADAS."))
           )
           (princ "\nDirecao [Cima(w)/Baixo(s)/Esquerda(a)/Direita(d)] ou B p/ caixas, M p/ mover manual: ")
          )
          ((member val '(77 109))
           (command "._MOVE" ent "" PAUSE PAUSE)
           (if *GRAV-SHOW-BOXES* (GRAV-REFRESH-BBOXES))
           (princ "\nPeca movida manualmente.")
           (princ "\nDirecao [Cima(w)/Baixo(s)/Esquerda(a)/Direita(d)] ou B p/ caixas, M p/ mover manual: ")
          )
        )
      )
    )
  )
  dir
)

(defun GRAV-SET-COLOR (ent colorNum / edata pair)
  (if (and ent (entget ent))
    (progn
      (setq edata (entget ent))
      (setq pair (assoc 62 edata))
      (if pair
        (setq edata (subst (cons 62 colorNum) pair edata))
        (setq edata (append edata (list (cons 62 colorNum))))
      )
      (entmod edata)
    )
  )
)

(defun GRAV-SET-LINETYPE (ent ltName / edata pair)
  (if (and ent (entget ent))
    (progn
      (setq edata (entget ent))
      (setq pair (assoc 6 edata))
      (if pair
        (setq edata (subst (cons 6 ltName) pair edata))
        (setq edata (append edata (list (cons 6 ltName))))
      )
      (entmod edata)
    )
  )
)

(defun GRAV-SET-LTSCALE (ent scaleVal / edata pair)
  (if (and ent (entget ent))
    (progn
      (setq edata (entget ent))
      (setq pair (assoc 48 edata))
      (if pair
        (setq edata (subst (cons 48 scaleVal) pair edata))
        (setq edata (append edata (list (cons 48 scaleVal))))
      )
      (entmod edata)
    )
  )
)

(defun GRAV-DASHED-OR-CONTINUOUS ()
  (if (tblsearch "LTYPE" "DASHED") "DASHED" "CONTINUOUS")
)

(defun GRAV-SEND-BACK (ent)
  (if (and ent (entget ent))
    (command "._DRAWORDER" ent "" "_Back")
  )
)

(defun GRAV-DRAW-BBOX (bbox colorNum / minpt maxpt p1 p2 p3 p4 ent)
  (setq minpt (car bbox)) (setq maxpt (cadr bbox))
  (setq p1 (list (car minpt) (cadr minpt) 0.0))
  (setq p2 (list (car maxpt) (cadr minpt) 0.0))
  (setq p3 (list (car maxpt) (cadr maxpt) 0.0))
  (setq p4 (list (car minpt) (cadr maxpt) 0.0))
  (command "._PLINE" p1 p2 p3 p4 "_C")
  (setq ent (entlast))
  (GRAV-SET-COLOR ent colorNum)
  (GRAV-SET-LINETYPE ent (GRAV-DASHED-OR-CONTINUOUS))
  (GRAV-SET-LTSCALE ent 10.0)
  (GRAV-SEND-BACK ent)
  ent
)

(defun GRAV-CLEAR-BBOXES ( / pair)
  (foreach pair *GRAV-BOX-LINKS*
    (if (and (car pair) (entget (car pair))) (entdel (car pair)))
  )
  (setq *GRAV-BOX-LINKS* nil)
)

;; Desenha a caixa (real + folga) de cada obstaculo
(defun GRAV-REFRESH-BBOXES ( / n i ent bbox boxEnt)
  (GRAV-CLEAR-BBOXES)
  (setq n (sslength *GRAV-SS*))
  (setq i 0)
  (while (< i n)
    (setq ent (ssname *GRAV-SS* i))
    (setq bbox (GRAV-PADDED-BBOX ent))
    (setq boxEnt (GRAV-DRAW-BBOX bbox 8))
    (setq *GRAV-BOX-LINKS* (cons (cons boxEnt ent) *GRAV-BOX-LINKS*))
    (setq i (1+ i))
  )
  (GRAV-RESTORE-VIEW)
)

;; Le a caixa editada, compara com o contorno REAL da peca, e
;; grava a diferenca (por lado) como folga extra
(defun GRAV-APPLY-BOX-OVERRIDES ( / pair boxEnt pieceEnt trueBbox editedBbox
                                    padL padR padB padT oldPair)
  (foreach pair *GRAV-BOX-LINKS*
    (setq boxEnt (car pair))
    (setq pieceEnt (cdr pair))
    (if (and boxEnt (entget boxEnt) pieceEnt (entget pieceEnt))
      (progn
        (setq trueBbox (GRAV-BBOX pieceEnt))
        (setq editedBbox (GRAV-BBOX-GEOM boxEnt))
        (if (and trueBbox editedBbox)
          (progn
            (setq padL (- (car (car trueBbox))  (car (car editedBbox))))
            (setq padR (- (car (cadr editedBbox)) (car (cadr trueBbox))))
            (setq padB (- (cadr (car trueBbox))  (cadr (car editedBbox))))
            (setq padT (- (cadr (cadr editedBbox)) (cadr (cadr trueBbox))))
            (setq oldPair (assoc pieceEnt *GRAV-OVERRIDES*))
            (if oldPair
              (setq *GRAV-OVERRIDES*
                (subst (cons pieceEnt (list padL padR padB padT)) oldPair *GRAV-OVERRIDES*))
              (setq *GRAV-OVERRIDES*
                (cons (cons pieceEnt (list padL padR padB padT)) *GRAV-OVERRIDES*))
            )
          )
        )
      )
    )
  )
)

;; Liga o destaque de cor: pecas do GRAVSET ficam amarelas, a
;; chapa fica ciano. So faz efeito uma vez (se ja estiver ligado,
;; nao faz nada, pra nao "perder" a cor original salva).
(defun GRAV-HIGHLIGHT-ON ( / n i ent col)
  (if (not *GRAV-HIGHLIGHTED*)
    (progn
      (setq *GRAV-ORIG-COLORS* nil)
      (setq n (sslength *GRAV-SS*))
      (setq i 0)
      (while (< i n)
        (setq ent (ssname *GRAV-SS* i))
        (setq col (cdr (assoc 62 (entget ent))))
        (if (not col) (setq col 256))
        (setq *GRAV-ORIG-COLORS* (cons (cons ent col) *GRAV-ORIG-COLORS*))
        (GRAV-SET-COLOR ent 2) ;; amarelo
        (setq i (1+ i))
      )
      (setq *GRAV-SHEET-ORIG-COLOR* nil)
      (if *GRAV-SHEET-ENT*
        (progn
          (setq *GRAV-SHEET-ORIG-COLOR* (cdr (assoc 62 (entget *GRAV-SHEET-ENT*))))
          (if (not *GRAV-SHEET-ORIG-COLOR*) (setq *GRAV-SHEET-ORIG-COLOR* 256))
          (GRAV-SET-COLOR *GRAV-SHEET-ENT* 4) ;; ciano
        )
      )
      (setq *GRAV-HIGHLIGHTED* T)
    )
  )
)

;; Desliga o destaque: devolve a cor original de cada peca e da chapa
(defun GRAV-HIGHLIGHT-OFF ( / pair)
  (if *GRAV-HIGHLIGHTED*
    (progn
      (foreach pair *GRAV-ORIG-COLORS*
        (GRAV-SET-COLOR (car pair) (cdr pair))
      )
      (setq *GRAV-ORIG-COLORS* nil)
      (if *GRAV-SHEET-ENT*
        (GRAV-SET-COLOR *GRAV-SHEET-ENT* *GRAV-SHEET-ORIG-COLOR*)
      )
      (setq *GRAV-HIGHLIGHTED* nil)
    )
  )
)

;; A entidade e uma area fechada (pode virar limite de chapa)?
(defun GRAV-IS-CLOSED-SHAPE (ent / edata etype flags)
  (setq edata (entget ent))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "CIRCLE") T)
    ((= etype "LWPOLYLINE")
     (setq flags (cdr (assoc 70 edata)))
     (if (not flags) (setq flags 0))
     (= 1 (logand flags 1))
    )
    ((= etype "POLYLINE")
     (setq flags (cdr (assoc 70 edata)))
     (if (not flags) (setq flags 0))
     (= 1 (logand flags 1))
    )
    (T nil)
  )
)

;; ============================================================
;; COMANDOS
;; ============================================================

(defun c:GRAVSET ( / ss oldCmdEcho ans ent bbox bbox2 gap step keepAsking)
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq *GRAV-ZOOMS* 0)
  (setq *GRAV-OVERRIDES* nil)

  (GRAV-HIGHLIGHT-OFF)
  (GRAV-CLEAR-BBOXES)
  (setq *GRAV-BOX-MODE* nil)

  (princ "\n=== CONFIGURACAO DA GRAVIDADE ===")
  (princ "\nSelecione TODAS as pecas que podem servir de obstaculo: ")
  (setq ss (ssget))

  (if (not ss)
    (progn
      (setvar "CMDECHO" oldCmdEcho)
      (princ "\nNenhuma peca selecionada. GRAVSET cancelado.")
      (princ)
    )
    (progn
      (setq *GRAV-SS* ss)

  (if (not *GRAV-GAP*) (setq *GRAV-GAP* 0.0))
  (initget 6)
  (setq gap (getreal (strcat "\nEspacamento / gap (mm) <"
                              (rtos *GRAV-GAP* 2 2) ">: ")))
  (if gap (setq *GRAV-GAP* gap))

  (if (not *GRAV-STEP*) (setq *GRAV-STEP* 0.5))
  (initget 6)
  (setq step (getreal (strcat "\nSensibilidade da busca fina (mm) -- menor = mais preciso porem mais lento <"
                               (rtos *GRAV-STEP* 2 3) ">: ")))
  (if step (setq *GRAV-STEP* step))

  (initget "Sim Nao")
  (setq ans (getkword "\nExiste um desenho (retangulo) definindo os limites da chapa? (OPCIONAL) [Sim/Nao] <Nao>: "))
  (if (not ans) (setq ans "Nao"))

  (if (= ans "Sim")
    (progn
      (setq ent nil)
      (setq keepAsking T)
      (while keepAsking
        (setq ent (car (entsel "\nSelecione o contorno da chapa (deve ser uma area fechada): ")))
        (cond
          ((not ent) (setq keepAsking nil)) ;; usuario cancelou -- sai com ent=nil
          ((not (GRAV-IS-CLOSED-SHAPE ent))
           (princ "\n(Essa entidade nao e uma area fechada -- selecione um retangulo, polilinha fechada ou circulo.)")
           (setq ent nil)
          )
          (T (setq keepAsking nil)) ;; entidade valida -- sai
        )
      )
      (if ent
        (progn
          (setq bbox (GRAV-BBOX-GEOM ent))
          (if (not bbox) (setq bbox (GRAV-BBOX-ZOOM ent)))
          (setq *GRAV-SHEET*
            (list (car (car bbox)) (cadr (car bbox))
                  (- (car (cadr bbox)) (car (car bbox)))
                  (- (cadr (cadr bbox)) (cadr (car bbox)))))
          (setq *GRAV-SHEET-ENT* ent)
          (setq bbox2 (ssdel ent *GRAV-SS*))
          (if bbox2 (setq *GRAV-SS* bbox2))
          (princ "\nLimites da chapa definidos a partir do desenho selecionado.")
        )
        (progn
          (princ "\nNada selecionado. Limite da chapa nao definido.")
          (setq *GRAV-SHEET* nil)
          (setq *GRAV-SHEET-ENT* nil)
        )
      )
    )
    (progn
      (setq *GRAV-SHEET* nil)
      (setq *GRAV-SHEET-ENT* nil)
    )
  )

  (GRAV-RESTORE-VIEW)
  (setvar "CMDECHO" oldCmdEcho)
  (princ (strcat "\nGRAVSET concluido. Gap = " (rtos *GRAV-GAP* 2 2) "mm."))
  (princ "\nAbrindo GRAVIDADE...")
  (princ)
  (c:GRAVIDADE)
    )
  )
  (princ)
)

(defun GRAV-MOVE-ONE (ent dir gap / bbox minX maxX minY maxY
                       n i obsEnt obsBbox oMinX oMaxX oMinY oMaxY
                       overlap candidate best bestSource bestPieceRaw
                       shX shY shW shH dx dy ovr corridorObs
                       creepResult contactPos hitEnt movingPad obsPad)

  (setq *GRAV-ZOOMS* 0)
  (setq bbox (GRAV-BBOX ent))
  (setq minX (car (car bbox)))   (setq minY (cadr (car bbox)))
  (setq maxX (car (cadr bbox)))  (setq maxY (cadr (cadr bbox)))

  (setq best nil)
  (setq bestSource nil)
  (setq bestPieceRaw nil)
  (setq corridorObs nil)

  ;; ---- limite da chapa (retangular, sempre exato) ----
  (if *GRAV-SHEET*
    (progn
      (setq shX (nth 0 *GRAV-SHEET*))
      (setq shY (nth 1 *GRAV-SHEET*))
      (setq shW (nth 2 *GRAV-SHEET*))
      (setq shH (nth 3 *GRAV-SHEET*))
      (cond
        ((= dir "Cima")
         (setq candidate (- (+ shY shH) gap))
         (if (or (not best) (< candidate best))
           (progn (setq best candidate) (setq bestSource 'sheet))))
        ((= dir "Baixo")
         (setq candidate (+ shY gap))
         (if (or (not best) (> candidate best))
           (progn (setq best candidate) (setq bestSource 'sheet))))
        ((= dir "Direita")
         (setq candidate (- (+ shX shW) gap))
         (if (or (not best) (< candidate best))
           (progn (setq best candidate) (setq bestSource 'sheet))))
        ((= dir "Esquerda")
         (setq candidate (+ shX gap))
         (if (or (not best) (> candidate best))
           (progn (setq best candidate) (setq bestSource 'sheet))))
      )
    )
  )

  ;; ---- varre obstaculos (pecas) por bbox real (sem folga ainda) ----
  (setq n (sslength *GRAV-SS*))
  (setq i 0)
  (while (< i n)
    (setq obsEnt (ssname *GRAV-SS* i))
    (if (not (equal obsEnt ent))
      (progn
        (setq obsBbox (GRAV-BBOX obsEnt))
        (setq oMinX (car (car obsBbox)))   (setq oMinY (cadr (car obsBbox)))
        (setq oMaxX (car (cadr obsBbox)))  (setq oMaxY (cadr (cadr obsBbox)))

        (cond
          ((= dir "Cima")
           (setq overlap (and (< minX oMaxX) (> maxX oMinX)))
           (if (and overlap (>= oMinY maxY))
             (progn
               (setq corridorObs (cons obsEnt corridorObs))
               (setq candidate (- oMinY gap))
               (if (or (not best) (< candidate best))
                 (progn (setq best candidate) (setq bestSource 'piece) (setq bestPieceRaw oMinY))
               )
             )
           )
          )
          ((= dir "Baixo")
           (setq overlap (and (< minX oMaxX) (> maxX oMinX)))
           (if (and overlap (<= oMaxY minY))
             (progn
               (setq corridorObs (cons obsEnt corridorObs))
               (setq candidate (+ oMaxY gap))
               (if (or (not best) (> candidate best))
                 (progn (setq best candidate) (setq bestSource 'piece) (setq bestPieceRaw oMaxY))
               )
             )
           )
          )
          ((= dir "Direita")
           (setq overlap (and (< minY oMaxY) (> maxY oMinY)))
           (if (and overlap (>= oMinX maxX))
             (progn
               (setq corridorObs (cons obsEnt corridorObs))
               (setq candidate (- oMinX gap))
               (if (or (not best) (< candidate best))
                 (progn (setq best candidate) (setq bestSource 'piece) (setq bestPieceRaw oMinX))
               )
             )
           )
          )
          ((= dir "Esquerda")
           (setq overlap (and (< minY oMaxY) (> maxY oMinY)))
           (if (and overlap (<= oMaxX minX))
             (progn
               (setq corridorObs (cons obsEnt corridorObs))
               (setq candidate (+ oMaxX gap))
               (if (or (not best) (> candidate best))
                 (progn (setq best candidate) (setq bestSource 'piece) (setq bestPieceRaw oMaxX))
               )
             )
           )
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (GRAV-RESTORE-VIEW)

  ;; ---- se uma PECA venceu (nao a chapa), refina com forma real + folga ----
  (if (equal bestSource 'piece)
    (progn
      (setq creepResult
        (GRAV-CREEP-CONTACT ent dir bestPieceRaw corridorObs minX minY maxX maxY))
      (if creepResult
        (progn
          (setq contactPos (car creepResult))
          (setq hitEnt (cdr creepResult))
          (setq movingPad (GRAV-PIECE-PAD ent))
          (setq obsPad (GRAV-PIECE-PAD hitEnt))
          (cond
            ((= dir "Cima")
             (setq best (- contactPos (+ gap (nth 3 movingPad) (nth 2 obsPad)))))
            ((= dir "Baixo")
             (setq best (+ contactPos (+ gap (nth 2 movingPad) (nth 3 obsPad)))))
            ((= dir "Direita")
             (setq best (- contactPos (+ gap (nth 1 movingPad) (nth 0 obsPad)))))
            ((= dir "Esquerda")
             (setq best (+ contactPos (+ gap (nth 0 movingPad) (nth 1 obsPad)))))
          )
        )
        (princ "\n(Aviso: forma real nao encontrou contato dentro do limite de busca -- usando aproximacao por bounding box.)")
      )
    )
  )

  ;; ---- se nao achou obstaculo nenhum, usa fallback de 10000mm ----
  (if (not best)
    (progn
      (cond
        ((= dir "Cima")     (setq best (+ maxY 10000.0)))
        ((= dir "Baixo")    (setq best (- minY 10000.0)))
        ((= dir "Direita")  (setq best (+ maxX 10000.0)))
        ((= dir "Esquerda") (setq best (- minX 10000.0)))
      )
      (princ "\nNenhum obstaculo encontrado -- movendo 10000mm nessa direcao.")
    )
  )

  (setq dx 0.0) (setq dy 0.0)
  (cond
    ((= dir "Cima")     (setq dy (- best maxY)))
    ((= dir "Baixo")    (setq dy (- best minY)))
    ((= dir "Direita")  (setq dx (- best maxX)))
    ((= dir "Esquerda") (setq dx (- best minX)))
  )
  (command "._MOVE" ent "" (list 0 0 0) (list dx dy 0))

  (princ (strcat "\nPeca movida (" (rtos dx 2 2) ", " (rtos dy 2 2) ")."))
  (princ)
)

(defun c:GRAVIDADE ( / oldCmdEcho ent dir again oldError)

  (if (not *GRAV-SS*)
    (progn (princ "\nRode o comando GRAVSET primeiro.") (princ))
    (progn
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (if (not *GRAV-GAP*) (setq *GRAV-GAP* 0.0))

      (if *GRAV-BOX-MODE*
        (progn
          (GRAV-APPLY-BOX-OVERRIDES)
          (setq *GRAV-BOX-MODE* nil)
        )
      )

      (setq oldError *error*)
      (defun *error* (msg)
        (GRAV-CLEAR-BBOXES)
        (GRAV-HIGHLIGHT-OFF)
        (setvar "CMDECHO" oldCmdEcho)
        (setq *error* oldError)
        (princ "\nGRAVIDADE encerrado.")
        (princ)
      )

      (GRAV-HIGHLIGHT-ON)
      (if *GRAV-SHOW-BOXES* (GRAV-REFRESH-BBOXES))

      (setq again T)
      (while again
        (setq ent (GRAV-PICK-ENT))
        (if (not ent)
          (princ "\nNada selecionado -- selecione uma peca, ou aperte ESC para sair.")
          (progn
            (princ "\nDirecao [Cima(w)/Baixo(s)/Esquerda(a)/Direita(d)] ou B p/ caixas, M p/ mover manual: ")
            (setq dir (GRAV-READ-DIR ent))
            (GRAV-MOVE-ONE ent dir *GRAV-GAP*)
            (if *GRAV-SHOW-BOXES* (GRAV-REFRESH-BBOXES))
          )
        )
      )

      (GRAV-CLEAR-BBOXES)
      (GRAV-HIGHLIGHT-OFF)
      (setvar "CMDECHO" oldCmdEcho)
      (setq *error* oldError)
    )
  )
  (princ)
)

(defun c:GRAVBOX ( / )
  (if (not *GRAV-SS*)
    (progn (princ "\nRode o comando GRAVSET primeiro.") (princ))
    (progn
      (if *GRAV-BOX-MODE*
        (progn
          (GRAV-APPLY-BOX-OVERRIDES)
          (GRAV-CLEAR-BBOXES)
          (GRAV-HIGHLIGHT-OFF)
          (setq *GRAV-BOX-MODE* nil)
          (princ "\nFolgas salvas. Caixas de colisao ocultadas.")
        )
        (progn
          (GRAV-HIGHLIGHT-ON)
          (GRAV-REFRESH-BBOXES)
          (setq *GRAV-BOX-MODE* T)
          (princ "\nCaixas de colisao visiveis. Arraste os grips para dar folga extra nalgum lado.")
          (princ "\n(Para salvar, rode novamente o comando GRAVBOX.)")
        )
      )
    )
  )
  (princ)
)

(setq *GRAV-ZOOMS* 0)
(setq *GRAV-BOX-LINKS* nil)
(setq *GRAV-BOX-MODE* nil)
(setq *GRAV-ORIG-COLORS* nil)
(setq *GRAV-HIGHLIGHTED* nil)
(if (not (boundp '*GRAV-OVERRIDES*)) (setq *GRAV-OVERRIDES* nil))
(if (not (boundp '*GRAV-SHOW-BOXES*)) (setq *GRAV-SHOW-BOXES* nil))
(if (not (boundp '*GRAV-STEP*)) (setq *GRAV-STEP* 0.1))
(if (not (boundp '*GRAV-SHEET-ENT*)) (setq *GRAV-SHEET-ENT* nil))

;; cabesalho do comando inicializamento ...
(princ "\n GRAVIDADE.LSP (v11) - Encontrado Comandos")
(princ "\n Finalidade..: Posicionamento manual de pecas (nesting) por colisao real de contorno, para chapas de corte.")
(princ "\n Comandos....: GRAVSET, GRAVIDADE (W/A/S/D, B = caixas), GRAVBOX (folga extra)")
(princ "\n Autoria.....: Rafael.K e Claudio*")
(princ "\n  ")

(princ)