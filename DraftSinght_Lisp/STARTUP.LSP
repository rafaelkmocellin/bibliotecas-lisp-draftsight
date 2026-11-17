;;; ============================================================
;;; STARTUP.LSP
;;; Arquivo "mestre" que carrega automaticamente os programas
;;; personalizados do DraftSight (GRAVIDADE, MEDLIN, PIQUE,
;;; UNIRPONTAS e qualquer outro .lsp colocado na mesma pasta).
;;;
;;; VOCE NAO PRECISA ENTENDER TUDO ABAIXO PARA USAR ISSO.
;;; Se so quer instalar, va direto para a secao "COMO ATIVAR ESTE
;;; ARQUIVO" no final deste cabecalho. As explicacoes de LISP
;;; abaixo sao so para quem tiver curiosidade ou precisar
;;; ativar/desativar algum dos programas no futuro.
;;; ============================================================


;;; ------------------------------------------------------------
;;; O BASICO: COMO O DRAFTSIGHT "LE" UM ARQUIVO .LSP
;;; ------------------------------------------------------------
;;;
;;; Um arquivo .lsp e um arquivo de texto simples, escrito na
;;; linguagem AutoLISP. Quando o DraftSight "carrega" um arquivo
;;; desses, ele le o conteudo de CIMA PARA BAIXO, uma instrucao
;;; de cada vez, e vai executando cada uma na hora que encontra --
;;; nao existe um "botao de play" separado: ler = executar.
;;;
;;; A instrucao mais importante deste arquivo e (load "caminho").
;;; Ela funciona como um "copia e cola": quando o DraftSight
;;; encontra um (load ".../GRAVIDADE.lsp"), e como se o conteudo
;;; INTEIRO do arquivo GRAVIDADE.lsp fosse colado naquele exato
;;; ponto e executado ali mesmo, na hora.
;;;
;;; COMENTARIOS: qualquer coisa depois de dois pontos-e-virgula
;;; (;;) numa linha e IGNORADA pelo DraftSight -- serve so para
;;; nos, humanos, lermos explicacoes (como este paragrafo). Mas
;;; isso tambem da um truque muito util: se voce colocar ;; NO
;;; COMECO de uma linha que executaria alguma coisa, essa linha
;;; vira "invisivel" para o programa, sem precisar apagar nada.
;;; E assim que a gente liga/desliga cada programa abaixo.
;;;
;;;
;;; ------------------------------------------------------------
;;; COMO ATIVAR/DESATIVAR CADA PROGRAMA
;;; ------------------------------------------------------------
;;;
;;; No MODO PADRAO (automatico, mais abaixo), todos os .lsp da
;;; pasta sao carregados. Para desativar um programa, tire o
;;; arquivo da pasta ou renomeie a extensao (ex: MEDLIN.lsp.OFF).
;;;
;;; No MODO ALTERNATIVO (manual), cada programa tem uma linha
;;; parecida com esta:
;;;
;;;     (STARTUP:carregar "MEDLIN" "MEDLIN.lsp")
;;;
;;; PARA DESATIVAR um programa (por exemplo, se voce nao vai usar
;;; o GRAVIDADE por um tempo): coloque ;; bem no inicio dessa
;;; linha, assim:
;;;
;;;     ;;(STARTUP:carregar "GRAVIDADE" "GRAVIDADE.lsp")
;;;
;;; PARA REATIVAR: e so tirar o ;; do inicio da linha de novo.
;;;
;;; Isso NAO apaga nem desinstala nada -- so faz o DraftSight
;;; pular aquele programa na proxima vez que abrir, sem precisar
;;; mexer nos outros.
;;;
;;;
;;; ------------------------------------------------------------
;;; COMO ATIVAR ESTE ARQUIVO NO DRAFTSIGHT (RESUMO)
;;; ------------------------------------------------------------
;;;
;;; 1. Salve este arquivo, com o nome exato STARTUP.LSP, numa
;;;    pasta fixa, junto com os outros .lsp. Por exemplo:
;;;        C:\Users\<seu usuario>\Documents\DraftSight_Lisp\
;;;    (pode ser qualquer pasta -- o importante e que os outros
;;;    .lsp fiquem JUNTO com este arquivo, na mesma pasta)
;;;
;;; 2. No DraftSight, digite o comando OPTIONS (ou Ferramentas >
;;;    Opcoes), va em "Localizacao de Arquivos > Sistema >
;;;    Caminho de Pesquisa de Arquivos de Suporte", clique em
;;;    "Novo..." e selecione essa pasta. Clique OK.
;;;
;;; 3. Feche e abra o DraftSight de novo.
;;;
;;; A partir dai, toda vez que o DraftSight abrir, este arquivo
;;; (e os programas que ele carrega) devem aparecer sozinhos,
;;; sem precisar digitar nada. O passo a passo completo esta no
;;; manual de instalacao de qualquer um dos programas (secao 2).
;;;
;;;
;;; ------------------------------------------------------------
;;; SE VOCE TROCAR DE COMPUTADOR OU DE USUARIO DO WINDOWS
;;; ------------------------------------------------------------
;;;
;;; Nao precisa editar nada neste arquivo. Ele NAO tem nenhum
;;; caminho fixo (nome de usuario, nome de pasta): ao carregar,
;;; ele pergunta ao DraftSight onde o proprio STARTUP.LSP esta
;;; (findfile) e carrega os outros .lsp DESSA MESMA pasta. Basta
;;; refazer o passo 2 acima (caminho de pesquisa) no computador
;;; novo.
;;; ============================================================


;; ------------------------------------------------------------
;; Funcao auxiliar: carrega um arquivo .lsp de forma "segura".
;; Em vez de so chamar (load "caminho") direto -- o que, se o
;; arquivo nao existir ali, pode gerar um erro confuso e ATE
;; impedir que os programas seguintes desta lista sejam
;; carregados -- esta funcao primeiro CONFERE se o arquivo existe
;; (findfile) e avisa com uma mensagem clara, em qualquer um dos
;; dois casos, sem travar o carregamento dos outros programas.
;; ------------------------------------------------------------
(defun STARTUP:carregar (nome caminho / achou)
  (setq achou (findfile caminho))
  (if achou
    (load achou)
    (princ (strcat "\n[STARTUP] AVISO: " nome
                    " NAO encontrado em:\n           " caminho
                    "\n           Verifique se o arquivo existe e se a pasta"
                    " esta no Caminho de Pesquisa de Arquivos de Suporte."))
  )
  (princ)
)

;; ------------------------------------------------------------
;; Funcao auxiliar: carrega TODOS os arquivos .lsp encontrados
;; dentro de uma pasta, automaticamente -- sem precisar listar
;; cada arquivo um por um.
;;
;; Usa (vl-directory-files pasta "*.lsp" 1) -- essa e uma funcao
;; de SISTEMA DE ARQUIVOS (so le nomes de arquivo no HD), bem
;; diferente das funcoes vlax-curve-* que dependem de automacao
;; ActiveX/COM. Ainda assim, se por algum motivo esta funcao nao
;; existir na sua instalacao, o AVISO abaixo aparece na tela
;; (em vez de travar o carregamento) e voce pode voltar a usar a
;; lista manual (secao alternativa, mais abaixo).
;;
;; O ultimo parametro da vl-directory-files (1) significa "so
;; arquivos, sem sub-pastas". O proprio STARTUP.LSP e ignorado de
;; proposito, para nao tentar se carregar de novo dentro do loop.
;; ------------------------------------------------------------
(defun STARTUP:carregar-pasta (pasta / arquivos arq caminho ok)
  (setq ok (not (vl-catch-all-error-p
                  (vl-catch-all-apply 'vl-directory-files (list pasta "*.lsp" 1)))))
  (if ok
    (progn
      (setq arquivos (vl-directory-files pasta "*.lsp" 1))
      (if arquivos
        (foreach arq arquivos
          (if (/= (strcase arq) "STARTUP.LSP")
            (progn
              (setq caminho (strcat pasta "/" arq))
              (STARTUP:carregar arq caminho)
            )
          )
        )
        (princ (strcat "\n[STARTUP] AVISO: nenhum arquivo .lsp encontrado em: " pasta))
      )
    )
    (princ "\n[STARTUP] AVISO: vl-directory-files nao disponivel nesta instalacao -- use a lista manual (veja comentarios mais abaixo neste arquivo).")
  )
  (princ)
)

;; ------------------------------------------------------------
;; Funcao auxiliar: devolve so a PASTA de um caminho completo
;; (tudo antes da ultima barra \ ou /). Ex:
;;   "C:\\Pasta\\STARTUP.LSP"  ->  "C:\\Pasta"
;; ------------------------------------------------------------
(defun STARTUP:pasta-de (caminho / i pos c)
  (setq i 1 pos nil)
  (while (<= i (strlen caminho))
    (setq c (substr caminho i 1))
    (if (or (= c "\\") (= c "/")) (setq pos i))
    (setq i (1+ i))
  )
  (if pos (substr caminho 1 (1- pos)))
)

;; ------------------------------------------------------------
;; Descobre em qual pasta este STARTUP.LSP esta. O findfile
;; procura nas pastas do "Caminho de Pesquisa de Arquivos de
;; Suporte" -- a mesma busca que o DraftSight usou para achar e
;; carregar este arquivo. Assim nenhum caminho (nome de usuario,
;; nome de pasta) precisa ficar escrito a mao aqui dentro.
;; ------------------------------------------------------------
(setq STARTUP:pasta
  (if (findfile "STARTUP.LSP")
    (STARTUP:pasta-de (findfile "STARTUP.LSP"))
  )
)


;; =============================================================
;; PROGRAMAS CARREGADOS
;; =============================================================
;;
;; --------------------MODO PADRAO------------------(automatico)
;;
;; Carrega automaticamente TODOS os .lsp encontrados na mesma
;; pasta deste STARTUP.LSP. Se voce adicionar um programa novo
;; nessa mesma pasta no futuro, ele passa a carregar sozinho, sem
;; precisar editar este STARTUP.LSP.
;;
;; Para DESATIVAR algum programa especifico nesse modo automatico,
;; a forma mais simples e mover o arquivo .lsp dele para fora
;; dessa pasta (ou renomear a extensao, tipo MEDLIN.lsp.OFF).
;; ------------------------------------------------------------

(if STARTUP:pasta
  (STARTUP:carregar-pasta STARTUP:pasta)
  (princ "\n[STARTUP] AVISO: nao foi possivel descobrir a pasta do STARTUP.LSP -- confira o Caminho de Pesquisa de Arquivos de Suporte.")
)



;; --------------------MODO ALTERNATIVO------------------(manual)
;;
;; Use se a busca automatica acima nao funcionar na sua
;; instalacao, ou caso voce prefira controlar exatamente quais
;; arquivos carregam e em que ordem.
;;
;; Para usar este modo em vez do automatico: comente o bloco
;; (if STARTUP:pasta ...) logo acima (coloque ;; no inicio de
;; cada uma das 4 linhas dele) e descomente as linhas abaixo
;; (tire o ;; do inicio de cada uma).
;;
;; Aqui basta o NOME do arquivo: o findfile procura sozinho nas
;; pastas do Caminho de Pesquisa de Arquivos de Suporte.
;; ------------------------------------------------------------

;;(STARTUP:carregar "GRAVIDADE"  "GRAVIDADE.lsp")
;;(STARTUP:carregar "MEDLIN"     "MEDLIN.lsp")
;;(STARTUP:carregar "PIQUE"      "PIQUE.lsp")
;;(STARTUP:carregar "UNIRPONTAS" "UNIRPONTAS.lsp")


;; ============================================================
;; FIM DO STARTUP.LSP
;; (cada programa acima, se carregado com sucesso, mostra seu
;; proprio aviso/banner na tela -- se algum nao aparecer, procure
;; a mensagem "[STARTUP] AVISO" acima com o motivo)
;; ============================================================
(princ)
