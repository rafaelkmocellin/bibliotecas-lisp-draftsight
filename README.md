# Bibliotecas LISP para DraftSight

Coleção de rotinas **AutoLISP** para o **DraftSight 2020 Professional**, voltadas para desenho técnico de peças e chapas de corte.

Todas as rotinas foram escritas em **AutoLISP puro**: nenhuma usa `vl-load-com`, `vla-*` ou `vlax-*` (ActiveX/COM). Por isso funcionam mesmo em instalações em que a extensão COM do DraftSight não carrega, como acontece na licença Professional.

| Arquivo | Para que serve | Comandos |
|---|---|---|
| [STARTUP.LSP](DraftSinght_Lisp/STARTUP.LSP) | Carrega automaticamente os outros `.lsp` ao abrir o DraftSight | *(nenhum, só carrega)* |
| [GRAVIDADE.lsp](DraftSinght_Lisp/GRAVIDADE.lsp) | Encaixe manual de peças na chapa ("nesting" por gravidade) | `GRAVSET`, `GRAVIDADE`, `GRAVBOX` |
| [MEDLIN.lsp](DraftSinght_Lisp/MEDLIN.lsp) | Mede um comprimento ao longo de um percurso e cota em cascata | `MEDLIN`, `MEDLININVERSO`, `DELLIN` |
| [PIQUE.LSP](DraftSinght_Lisp/PIQUE.LSP) | Desenha o piquete (entalhe em "V") e corta a linha no ponto | `PIQUE`, `-PIQUE`, `PK`, `-PK`, `ADAPTPIQUE`, `-ADAPTPIQUE` |
| [UNIRPONTAS.lsp](DraftSinght_Lisp/UNIRPONTAS.lsp) | Fecha pequenas folgas entre pontas de linhas, arcos e polilinhas | `UNIRPONTAS`, `UNIRVARIASPONTAS`, `MOSTRARABERTOS` |

Manuais detalhados de cada rotina (instalação, uso e solução de problemas) estão nos arquivos `MANUAL_*.txt` na raiz do repositório.

---

## STARTUP.LSP: o carregador automático

### O que ele faz

O `STARTUP.LSP` **não tem nenhum comando de desenho**. Ele só **carrega os outros arquivos `.lsp`** toda vez que o DraftSight abre, para que os comandos (`PIQUE`, `MEDLIN` etc.) já estejam disponíveis sem precisar digitar nada.

Ele tem dois modos:

- **Modo padrão (automático):** carrega **todos** os `.lsp` que estiverem na mesma pasta (menos ele mesmo). Se você colocar uma rotina nova na pasta, ela passa a carregar sozinha. Para desativar uma rotina, basta tirá-la da pasta ou renomear a extensão (por exemplo, `MEDLIN.lsp.OFF`).
- **Modo alternativo (manual):** uma lista fixa de linhas `(STARTUP:carregar "NOME" "caminho")`, uma por rotina. Serve para escolher exatamente quais arquivos carregam e em que ordem, ou para o caso de a função `vl-directory-files` (usada no modo automático) não existir na sua instalação. Para usar, comente a linha `(STARTUP:carregar-pasta ...)` e descomente as linhas da lista.

Nos dois modos, antes de carregar cada arquivo ele confere se o arquivo existe. Se não encontrar, mostra um aviso `[STARTUP] AVISO: ...` na linha de comando e continua carregando os demais, sem travar.

### Ele é necessário?

**Não para as rotinas funcionarem.** Cada `.lsp` é independente e pode ser usado sozinho, carregando manualmente na linha de comando do DraftSight:

```lisp
(load "C:/Users/<seu usuario>/Documents/DraftSight_Lisp/MEDLIN.lsp")
```

O problema é que o carregamento manual **vale só até fechar o DraftSight**: na próxima vez, é preciso repetir. O `STARTUP.LSP` existe para automatizar isso. Ele é recomendado se você usa as rotinas no dia a dia e dispensável se só quer testar uma delas.

### Como ativar

1. Coloque o `STARTUP.LSP` e as rotinas que quiser **na mesma pasta**, por exemplo `C:\Users\<seu usuario>\Documents\DraftSight_Lisp\`. O nome da pasta pode ser qualquer um.
2. No DraftSight, rode `OPTIONS` (Ferramentas > Opções) e vá em **Localização de Arquivos > Sistema > Caminho de Pesquisa de Arquivos de Suporte**. Clique em **Novo...**, selecione a pasta e confirme com OK.
3. Feche e abra o DraftSight. Cada rotina carregada mostra a sua mensagem na linha de comando.

Não é preciso editar nada dentro do `STARTUP.LSP`. Ele não tem nenhum caminho fixo: ao carregar, descobre sozinho em que pasta está (pelo `findfile`) e carrega os `.lsp` dessa mesma pasta. Por isso funciona em qualquer computador ou usuário do Windows.

> **Dica:** os arquivos devem ficar em texto puro **ANSI** (sem acentos, não UTF-8) e com extensão `.lsp` de verdade, não `.lsp.txt`. Ative "Mostrar extensões de arquivo" no Windows para conferir.

---

## GRAVIDADE.lsp: encaixe de peças por "gravidade"

**Objetivo:** ajudar no posicionamento manual de peças numa chapa de corte (*nesting* manual), aproveitando melhor o material.

**Como funciona:** você escolhe uma peça e uma direção, e ela "cai" nessa direção até encostar na peça mais próxima (ou na borda da chapa), respeitando um espaçamento mínimo. A colisão é calculada pelo **contorno real** da peça, incluindo arcos, e não só pelo retângulo que a envolve.

- **`GRAVSET`**: configuração. Seleciona as peças que servem de obstáculo, define o espaçamento mínimo entre peças (gap, em mm), a precisão da busca (padrão 0,1 mm) e, opcionalmente, um retângulo que representa os limites da chapa. Rode de novo sempre que mudar as peças ou o gap.
- **`GRAVIDADE`**: comando principal, em loop até apertar **ESC**. Selecione a peça e aperte, sem Enter:
  - `W` cima, `S` baixo, `A` esquerda, `D` direita
  - `B` mostra ou esconde as caixas de colisão
- **`GRAVBOX`**: mostra as caixas de colisão para conferência. Você pode arrastar os grips de uma caixa para criar uma **folga extra** em um lado específico da peça. Rode `GRAVBOX` de novo para salvar e esconder. A folga é somada ao gap; a forma real continua sendo usada no cálculo.

**Limitações:** em `POLYLINE` antiga (não `LWPOLYLINE`), arcos são tratados como retas. Peças que já começam sobrepostas podem não ser reconhecidas como obstáculo; separe-as antes.

---

## MEDLIN.lsp: medidor de linhas e percursos

**Objetivo:** marcar um ponto a uma distância exata **ao longo** de um contorno (por exemplo, "a 350 mm do canto, seguindo a borda da peça") e cotar o trecho percorrido.

**Como funciona:** você seleciona as linhas, arcos e polilinhas que formam o percurso. **Eles não precisam estar unidos**: a rotina conecta os objetos pelas pontas coincidentes. Depois você clica o ponto de partida e digita o comprimento.

- **`MEDLIN`**: percorre o comprimento informado. Uma seta amarela temporária mostra o sentido positivo; um valor negativo percorre o sentido contrário. No final:
  - marca o ponto com um `POINT`, um pequeno risco perpendicular e a nota "Use DELLIN p/ apagar", na camada `MEDLIN_MARCADOR` (travada);
  - pergunta de que lado ficam as cotas e cria **uma cota alinhada por segmento** (em cascata), na camada `MEDLIN_COTAS` (ciano). A soma das cotas sempre bate com o valor digitado.
- **`MEDLININVERSO`**: mesmo fluxo, mas percorre o sentido contrário ao do `MEDLIN` e cota **o restante do percurso** (o complemento), em vez do trecho digitado.
- **`DELLIN`**: apaga **todas** as cotas e marcadores criados pelo `MEDLIN` e `MEDLININVERSO` no desenho, de qualquer execução anterior.

**Entidades suportadas:** `LINE`, `ARC` e `LWPOLYLINE` (inclusive fechada e com arcos).

---

## PIQUE.LSP: piquetes em "V"

**Objetivo:** inserir o **piquete** (pequeno entalhe em "V" usado como marca de encaixe ou referência em moldes e peças cortadas) com um único clique.

**Como funciona:** você clica sobre uma linha, polilinha, círculo ou arco. A rotina detecta a direção da linha naquele ponto (tangente), desenha o "V" com linhas retas (**sem usar bloco**, sem depender de nenhum `.dwg` externo) e **corta a entidade original** exatamente na abertura do "V".

- **`PIQUE`** (atalho **`PK`**): piquete com tamanho padrão de 4 mm de abertura por 4 mm de profundidade.
- **`-PIQUE`** (atalho **`-PK`**): igual, mas com o "V" virado para o outro lado. Use quando o "V" sair para o lado errado.
- **`ADAPTPIQUE`**: pergunta a abertura e a profundidade antes de desenhar (Enter mantém o padrão).
- **`-ADAPTPIQUE`**: tamanho personalizado com o "V" invertido.

Para mudar o tamanho padrão, edite as variáveis `*PIQUE-ABERTURA*` e `*PIQUE-PROFUNDIDADE*` no início do arquivo.

**Entidades suportadas:** linha, polilinha de segmentos retos, círculo e arco. Em outras (elipse, spline), a rotina pede o ângulo manualmente.

---

## UNIRPONTAS.lsp: união de pontas próximas

**Objetivo:** fechar folgas pequenas entre objetos que deveriam estar encostados mas ficaram com uma "pontinha solta", o que é comum em desenhos importados de outros programas. Contornos abertos costumam dar problema na hora de gerar o corte.

- **`UNIRPONTAS`**: modo interativo (até **ESC**). Pergunta a tolerância (padrão 0,5), você seleciona os objetos e um quadrado do tamanho da tolerância acompanha o cursor. Clique onde houver duas ou mais pontas dentro do quadrado para uni-las.
- **`UNIRVARIASPONTAS`**: operação única. Une **automaticamente** todos os grupos de pontas próximas dentro da seleção. O agrupamento é em cadeia: se A está perto de B e B perto de C, os três são unidos juntos. No final, informa quantos grupos e pontas foram unidos.
- **`MOSTRARABERTOS`**: diagnóstico, não altera nada. Desenha um **círculo vermelho** em cada ponta que não está realmente encostada em outra. Útil para conferir o desenho antes e depois da união. Os círculos somem ao sair com ESC.

**Regra de união:** entre linhas e polilinhas, as pontas vão para a posição média entre elas. Se uma das pontas for de um **arco**, o arco fica parado e as outras pontas vão até ele, para não alterar o raio do arco.

---

## Requisitos

- DraftSight 2020 Professional (pode funcionar em outras versões com suporte a AutoLISP, mas não foi testado).
- Nenhuma dependência externa: cada rotina é um único arquivo `.lsp`.

## Autoria

Criado por **Rafael K.** O `UNIRPONTAS` foi feito em parceria com **Claudio\***.

## Licença

Distribuído sob a licença MIT. Veja o arquivo [LICENSE](LICENSE).
