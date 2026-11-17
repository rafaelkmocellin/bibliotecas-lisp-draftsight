;;; ============================================================
;;; PIQUE.LSP
;;; Comandos: PIQUE / -PIQUE / PK / -PK (tamanho fixo padrao)
;;;           ADAPTPIQUE / -ADAPTPIQUE (pede abertura e profundidade)
;;; Desenha o simbolo do piquete em LINHAS RETAS PURAS (sem bloco),
;;; alinhado automaticamente (tangente) a linha, polilinha, circulo
;;; ou arco clicado, e corta a entidade original no mesmo ponto.
;;; ============================================================

(setq *PIQUE-ABERTURA*      4.0)  ; largura padrao do "V" no topo, sobre a linha (mm)
(setq *PIQUE-PROFUNDIDADE*  4.0)  ; profundidade padrao do "V", da linha ate a ponta (mm)

;; ---- Erro padrao: avisa onde parou, sem travar o DraftSight ----
(defun pique-error (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\nPIQUE interrompido: " msg))
  )
  (princ)
)

;; ---- Distancia de um ponto a um segmento (2D) ----
(defun ptSegDist (p a b / ab ap tt proj denom)
  (setq ab (list (- (car b) (car a)) (- (cadr b) (cadr a))))
  (setq ap (list (- (car p) (car a)) (- (cadr p) (cadr a))))
  (setq denom (+ (* (car ab) (car ab)) (* (cadr ab) (cadr ab)) 1e-9))
  (setq tt (/ (+ (* (car ap) (car ab)) (* (cadr ap) (cadr ab))) denom))
  (if (< tt 0.0) (setq tt 0.0))
  (if (> tt 1.0) (setq tt 1.0))
  (setq proj (list (+ (car a) (* tt (car ab))) (+ (cadr a) (* tt (cadr ab)))))
  (distance p proj)
)

;; ---- Projeta um ponto sobre o segmento a-b, travado entre as extremidades ----
(defun pique-clamp (p a b / ab ap tt denom)
  (setq ab (list (- (car b) (car a)) (- (cadr b) (cadr a))))
  (setq ap (list (- (car p) (car a)) (- (cadr p) (cadr a))))
  (setq denom (+ (* (car ab) (car ab)) (* (cadr ab) (cadr ab)) 1e-9))
  (setq tt (/ (+ (* (car ap) (car ab)) (* (cadr ap) (cadr ab))) denom))
  (if (< tt 0.0) (setq tt 0.0))
  (if (> tt 1.0) (setq tt 1.0))
  (list (+ (car a) (* tt (car ab))) (+ (cadr a) (* tt (cadr ab))))
)

;; ---- Detecta angulo tangente + limites do segmento (para travar o corte) ----
;; Retorna (list angulo ptA ptB) -- ptA/ptB = nil quando nao ha segmento reto
;; para travar (caso de circulo/arco, onde a curva e reencaixada por osnap)
(defun pique-info (ent pt / edata etype p1 p2 verts distmin best bestA bestB i a b d)
  (setq edata (entget ent))
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((= etype "LINE")
     (setq p1 (cdr (assoc 10 edata)))
     (setq p2 (cdr (assoc 11 edata)))
     (list (angle p1 p2) p1 p2)
    )
    ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
     (setq verts nil)
     (foreach pair edata
       (if (= (car pair) 10) (setq verts (append verts (list (cdr pair)))))
     )
     (setq distmin 1e9 best nil bestA nil bestB nil i 0)
     (repeat (max 0 (1- (length verts)))
       (setq a (nth i verts))
       (setq b (nth (1+ i) verts))
       (setq d (ptSegDist pt a b))
       (if (< d distmin)
         (progn (setq distmin d) (setq best (angle a b)) (setq bestA a) (setq bestB b))
       )
       (setq i (1+ i))
     )
     (list best bestA bestB)
    )
    ((or (= etype "CIRCLE") (= etype "ARC"))
     (setq p1 (cdr (assoc 10 edata)))   ; centro
     (list (+ (angle p1 pt) (/ pi 2.0)) nil nil)
    )
    (t (list nil nil nil))
  )
)

;; ---- Funcao central: desenha o "V" (2 linhas) e corta a entidade original ----
;; inverter:     T para o "V" apontar para o lado oposto
;; abertura:     largura do topo do "V", sobre a linha (mm)
;; profundidade: profundidade do "V", da linha ate a ponta (mm)
(defun pique-executar (inverter abertura profundidade
                        / *error* olderr sel ent pt ptLinha angRad
                        info segmA segmB meiaAbert dirT dirP sinal
                        pEsq pDir pApice)
  (setq olderr *error*)
  (defun *error* (msg) (pique-error msg) (setq *error* olderr))

  ;; 0. Limpa qualquer selecao/grip ativo antes de comecar
  (sssetfirst nil nil)
  (setvar "PICKFIRST" 1)

  ;; 1. Um unico clique: seleciona a linha/polilinha/circulo/arco E define o ponto
  (setq sel (entsel "\nClique na linha/polilinha/circulo/arco, no ponto onde deseja inserir o piquete: "))
  (if (not sel)
    (progn (princ "\nNenhuma entidade selecionada. Comando cancelado.") (exit))
  )
  (setq ent (car sel))
  (setq pt  (cadr sel))

  ;; 2. Ajusta o ponto clicado para ficar exatamente em cima da entidade
  (setq ptLinha (osnap pt "_nea"))
  (if (not ptLinha) (setq ptLinha pt))

  ;; 3. Detecta o angulo tangente + limites do segmento (para travar o corte)
  (setq info (pique-info ent ptLinha))
  (setq angRad (nth 0 info))
  (setq segmA  (nth 1 info))
  (setq segmB  (nth 2 info))
  (if (not angRad)
    (progn
      (princ "\nAviso: nao foi possivel detectar a tangente automaticamente para este tipo de entidade.")
      (setq angRad (* (getreal "\nInforme o angulo manualmente [0]: ") (/ pi 180.0)))
      (if (not angRad) (setq angRad 0))
      (setq segmA nil segmB nil)
    )
  )

  ;; 3b. Sinal do lado (para onde o "V" aponta) -- T inverte para o outro lado
  (setq sinal (if inverter -1.0 1.0))

  ;; 4. Vetores unitarios: tangente (ao longo da entidade) e perpendicular
  (setq dirT (list (cos angRad) (sin angRad)))
  (setq dirP (list (* sinal (- (sin angRad))) (* sinal (cos angRad))))

  ;; 5. Pontos do topo do "V" (sobre a entidade, +- metade da abertura)
  (setq meiaAbert (/ abertura 2.0))
  (setq pEsq (list (- (car ptLinha) (* meiaAbert (car dirT)))
                    (- (cadr ptLinha) (* meiaAbert (cadr dirT)))))
  (setq pDir (list (+ (car ptLinha) (* meiaAbert (car dirT)))
                    (+ (cadr ptLinha) (* meiaAbert (cadr dirT)))))

  ;; 5b. Trava os pontos do topo dentro do segmento real (polilinha reta),
  ;;     OU, em entidades curvas (circulo/arco), reencaixa exatamente sobre a curva
  (if (and segmA segmB)
    (progn
      (setq pEsq (pique-clamp pEsq segmA segmB))
      (setq pDir (pique-clamp pDir segmA segmB))
    )
    (progn
      (setq pEsq (cond ((osnap pEsq "_nea")) (T pEsq)))
      (setq pDir (cond ((osnap pDir "_nea")) (T pDir)))
    )
  )

  ;; 6. Ponta do "V" (profundidade)
  (setq pApice (list (- (car ptLinha) (* profundidade (car dirP)))
                      (- (cadr ptLinha) (* profundidade (cadr dirP)))))

  ;; 7. Desenha as 2 linhas retas do "V": perna esquerda e perna direita
  (command "_.LINE" pEsq pApice "")
  (command "_.LINE" pApice pDir "")

  ;; 8. Corta a entidade original nos mesmos pontos do topo do "V"
  (command "_.BREAK" ent "_F" pEsq pDir)

  (princ "\nPiquete inserido e entidade cortada.")
  (princ)
)

;; ---- Comandos disponiveis ----

;; Tamanho padrao (4mm / 4mm), tangente automatico
(defun c:PIQUE ()  (pique-executar nil *PIQUE-ABERTURA* *PIQUE-PROFUNDIDADE*))
(defun c:-PIQUE () (pique-executar T   *PIQUE-ABERTURA* *PIQUE-PROFUNDIDADE*))
(defun c:PK ()     (pique-executar nil *PIQUE-ABERTURA* *PIQUE-PROFUNDIDADE*))
(defun c:-PK ()    (pique-executar T   *PIQUE-ABERTURA* *PIQUE-PROFUNDIDADE*))

;; Tamanho customizavel: pede abertura e profundidade antes de inserir
(defun pique-adapt-perguntar ( / ab prof)
  (setq ab (getreal
    (strcat "\nAbertura no topo, sobre a linha, em mm ["
            (rtos *PIQUE-ABERTURA* 2 2) "]: ")))
  (if (not ab) (setq ab *PIQUE-ABERTURA*))

  (setq prof (getreal
    (strcat "\nProfundidade do V, ate a ponta, em mm ["
            (rtos *PIQUE-PROFUNDIDADE* 2 2) "]: ")))
  (if (not prof) (setq prof *PIQUE-PROFUNDIDADE*))

  (list ab prof)
)

(defun c:ADAPTPIQUE ( / vals)
  (setq vals (pique-adapt-perguntar))
  (pique-executar nil (nth 0 vals) (nth 1 vals))
)

(defun c:-ADAPTPIQUE ( / vals)
  (setq vals (pique-adapt-perguntar))
  (pique-executar T (nth 0 vals) (nth 1 vals))
)

;; cabecalho do comando inicializamento ...
(princ "\n PIQUE.LSP (v21) - Encontrado Comandos")
(princ "\n Finalidade..: Inserir automaticamente o simbolo de piquete de corte, alinhado a entidade, cortando-a no ponto.")
(princ "\n Comandos....: PIQUE / -PIQUE / PK / -PK (tamanho padrao 4mm/4mm), ADAPTPIQUE / -ADAPTPIQUE (abertura e profundidade customizaveis).")
(princ "\n Autoria.....: Rafael.K e Claudio*")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ "\n  ")
(princ)