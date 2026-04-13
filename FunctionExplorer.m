(* ::Package:: *)

(* :Title:            FunctionExplorer                                       *)
(* :Context:          FunctionExplorer`                                      *)
(* :Author:           [Nome Gruppo]                                          *)
(* :Summary:          Simulatore e quiz interattivo su funzioni matematiche. *)
(*                    L'utente visualizza il grafico di una funzione          *)
(*                    generata casualmente e risponde a domande su zeri,      *)
(*                    massimi/minimi e derivate. Implementa le 5             *)
(*                    funzionalita' obbligatorie dell'Esercizio Universale.  *)
(* :Copyright:        [Nome Gruppo] 2026                                     *)
(* :Package Version:  1.0                                                    *)
(* :Mathematica Version: 14                                                  *)
(* :History:          Creato per il corso MC 2025/26                         *)
(* :Keywords:         funzioni, grafico, derivata, zeri, quiz, interfaccia   *)
(* :Warning:          DOCUMENTATE TUTTO il codice                            *)

(* =========================================================================*)
(* SEZIONE PUBBLICA                                                          *)
(* Dichiarazione dei simboli visibili all'esterno del pacchetto.             *)
(* =========================================================================*)

BeginPackage["FunctionExplorer`"]

esercizioUniversale::usage =
  "esercizioUniversale[] lancia l'interfaccia interattiva FunctionExplorer.";

(* Messaggi di errore personalizzati: definiti PRIMA di Begin[Private]      *)
(* cosi' sono visibili anche dentro i Button del DynamicModule              *)
FunctionExplorer::badseed =
  "Il Seed deve essere un numero intero. Verra' usato il valore 1 come default.";
FunctionExplorer::badinput =
  "Inserisci un numero valido (es. 3.14).";

(* =========================================================================*)
(* SEZIONE PRIVATA                                                           *)
(* Tutte le funzioni ausiliarie: non accessibili dall'esterno.               *)
(* =========================================================================*)

Begin["`Private`"]

(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feGeneraFunzione                                     *)
(*                                                                           *)
(* Scopo:   Dato un seed intero, restituisce una funzione pura f             *)
(*          scelta casualmente da un catalogo di 8 funzioni.                 *)
(*                                                                           *)
(* Input:   seed_Integer  - intero per inizializzare SeedRandom              *)
(* Lavoro:  catalogo      - lista di 8 funzioni pure Function[x, ...]        *)
(*          scelta        - indice estratto casualmente in [1, 8]            *)
(* Output:  funzione pura f, usare come f[valore]                            *)
(* -------------------------------------------------------------------------*)
feGeneraFunzione[seed_Integer] :=
  Module[{catalogo, scelta},
    SeedRandom[seed];
    catalogo = {
      (* 1. Polinomio grado 2: a x^2 + b x + c *)
      Function[x, RandomInteger[{1,2}]*x^2 + RandomInteger[{-3,3}]*x
                  + RandomInteger[{-3,3}]],
      (* 2. Polinomio grado 3: a x^3 + b x *)
      Function[x, RandomChoice[{-1,1}]*x^3 + RandomInteger[{-4,4}]*x],
      (* 3. Seno scalato: a Sin[b x] *)
      Function[x, RandomInteger[{1,2}]*Sin[RandomInteger[{1,2}]*x]],
      (* 4. Coseno scalato: a Cos[b x] *)
      Function[x, RandomInteger[{1,2}]*Cos[RandomInteger[{1,2}]*x]],
      (* 5. Esponenziale: Exp[a x] *)
      Function[x, Exp[RandomChoice[{-1,1}]*x]],
      (* 6. Combinazione: Sin[x] + Cos[x] *)
      Function[x, Sin[x] + Cos[x]],
      (* 7. Polinomio con radici esplicite: (x-a)(x-b) *)
      Function[x, (x - RandomInteger[{-2,2}])*(x - RandomInteger[{-2,2}])],
      (* 8. Logaritmo regolarizzato: Log[|x|+1], definito per tutti x *)
      Function[x, Log[Abs[x] + 1]]
    };
    scelta = RandomInteger[{1, Length[catalogo]}];
    catalogo[[scelta]]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feGeneraDomanda                                      *)
(*                                                                           *)
(* Scopo:   Dato un seed, sceglie casualmente il tipo di domanda             *)
(*          e i parametri. Usa seed+1 per differenziarsi da feGeneraFunzione.*)
(*                                                                           *)
(* Input:   seed_Integer  - intero                                           *)
(* Lavoro:  tipi          - lista {"zero", "minmax", "derivata"}             *)
(*          tipo          - tipo estratto con RandomChoice                   *)
(*          x0            - punto in {-2,-1,0,1,2} (solo per "derivata")    *)
(*          intervallo    - {-3, 3} (per "zero" e "minmax")                  *)
(* Output:  Association con chiavi "Tipo", "Parametri", "Testo"              *)
(* -------------------------------------------------------------------------*)
feGeneraDomanda[seed_Integer] :=
  Module[{tipi, tipo, x0, intervallo},
    SeedRandom[seed + 1];
    tipi = {"zero", "minmax", "derivata"};
    tipo = RandomChoice[tipi];
    Which[
      tipo === "zero",
        intervallo = {-3, 3};
        <| "Tipo"      -> "zero",
           "Parametri" -> intervallo,
           "Testo"     -> "Trova uno zero di f(x) nell'intervallo [-3, 3].\
\nInserisci x tale che f(x) \[TildeEqual] 0." |>,
      tipo === "minmax",
        intervallo = {-3, 3};
        <| "Tipo"      -> "minmax",
           "Parametri" -> intervallo,
           "Testo"     -> "Trova x in [-3, 3] dove f(x) ha un massimo o \
minimo locale.\nInserisci il valore di x." |>,
      True,
        x0 = RandomChoice[{-2, -1, 0, 1, 2}];
        <| "Tipo"      -> "derivata",
           "Parametri" -> x0,
           "Testo"     -> "Calcola f'(x) nel punto x\[Sub]0 = " <>
             ToString[x0] <> ".\nInserisci il valore numerico di f'(" <>
             ToString[x0] <> ")." |>
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feCalcolaRisposta                                    *)
(*                                                                           *)
(* Scopo:   Calcola numericamente la risposta corretta per la domanda.       *)
(*                                                                           *)
(* Input:   f_                 - funzione pura (da feGeneraFunzione)         *)
(*          domanda_Association - Association (da feGeneraDomanda)           *)
(* Lavoro:  tipo     - tipo di domanda estratto dall'Association             *)
(*          par      - parametri (intervallo o punto x0)                     *)
(*          fp       - derivata simbolica D[f[x],x] (solo "derivata")        *)
(*          rootList - lista zeri NSolve (solo "zero")                       *)
(* Output:  numero reale (risposta corretta), oppure $Failed se errore       *)
(* -------------------------------------------------------------------------*)
feCalcolaRisposta[f_, domanda_Association] :=
  Module[{tipo, par, sol, fp, rootList},
    tipo = domanda["Tipo"];
    par  = domanda["Parametri"];
    (* Quiet sopprime messaggi del Kernel; Check restituisce $Failed se errore *)
    sol = Check[
      Quiet[
        Which[
          (* CASO zero: cerca radici con NSolve; fallback FindRoot al centro *)
          tipo === "zero",
            rootList = x /. NSolve[
              f[x] == 0 && par[[1]] <= x <= par[[2]], x, Reals];
            If[Length[rootList] == 0,
               x /. FindRoot[f[x], {x, 0}],
               First[rootList]
            ],
          (* CASO minmax: NMinimize e NMaximize; restituisce l'estremo     *)
          (* con valore assoluto maggiore                                  *)
          tipo === "minmax",
            Module[{xMin, xMax, fMin, fMax},
              {fMin, {x -> xMin}} =
                NMinimize[{f[x], par[[1]] <= x <= par[[2]]}, x];
              {fMax, {x -> xMax}} =
                NMaximize[{f[x], par[[1]] <= x <= par[[2]]}, x];
              If[Abs[fMin] >= Abs[fMax], xMin, xMax]
            ],
          (* CASO derivata: calcola D[f[x],x] simbolicamente poi valuta   *)
          True,
            fp = D[f[x], x];
            N[fp /. x -> par]
        ]
      ],
      $Failed
    ];
    sol
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feVerificaRisposta                                   *)
(*                                                                           *)
(* Scopo:   Confronta la risposta dell'utente con la soluzione corretta      *)
(*          usando una tolleranza numerica assoluta configurabile.            *)
(*                                                                           *)
(* Input:   rispostaUtente_?NumericQ - numero inserito dall'utente           *)
(*          soluzione_?NumericQ      - numero da feCalcolaRisposta           *)
(*          opts                     - opzioni (tolleranza modificabile)     *)
(* Lavoro:  tol - valore tolleranza da OptionValue["Tolleranza"]             *)
(* Output:  True se |risposta - soluzione| <= tol, False altrimenti          *)
(*          Il secondo pattern cattura input non numerici -> sempre False    *)
(* -------------------------------------------------------------------------*)
Options[feVerificaRisposta] = {"Tolleranza" -> 0.05};

feVerificaRisposta[rispostaUtente_?NumericQ, soluzione_?NumericQ,
                   opts : OptionsPattern[]] :=
  Module[{tol},
    (* Stringa "Tolleranza" per stabilita' rispetto al contesto Global     *)
    tol = OptionValue["Tolleranza"];
    Abs[N[rispostaUtente] - N[soluzione]] <= tol
  ]

(* Pattern di sicurezza: input non numerico -> False, nessun errore        *)
feVerificaRisposta[_, _, OptionsPattern[]] := False


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feGraficoAnnotato                                    *)
(*                                                                           *)
(* Scopo:   Genera il grafico della funzione su [-4,4] con il punto          *)
(*          soluzione evidenziato in rosso e una linea tratteggiata.         *)
(*          Chiamata SOLO nel pop-up Mostra Soluzione.                       *)
(*                                                                           *)
(* Input:   f_                 - funzione pura                               *)
(*          xSol_?NumericQ     - ascissa del punto soluzione                 *)
(*          domanda_Association - Association (per tipo)                     *)
(* Lavoro:  ySol    - f[xSol] numerico; fallback 0 se non valutabile        *)
(*          pGrafico - Plot della funzione in blu                            *)
(*          pPunto   - punto rosso con etichetta coordinate                  *)
(*          pLabel   - linea tratteggiata verticale                          *)
(* Output:  oggetto grafico combinato con Show                               *)
(* -------------------------------------------------------------------------*)
feGraficoAnnotato[f_, xSol_?NumericQ, domanda_Association] :=
  Module[{ySol, pGrafico, pPunto, pLabel},
    ySol = Quiet[Check[N[f[xSol]], 0]];
    pGrafico = Plot[
      f[x], {x, -4, 4},
      PlotStyle      -> {Blue, Thickness[0.003]},
      AxesLabel      -> {Style["x", FontFamily -> "Arial"],
                         Style["f(x)", FontFamily -> "Arial"]},
      PlotLabel      -> Style["Grafico di f(x)", Bold, 13,
                               FontFamily -> "Arial"],
      GridLines      -> Automatic,
      GridLinesStyle -> LightGray,
      ImageSize      -> 420
    ];
    pPunto = Graphics[{
      Red, PointSize[0.022], Point[{xSol, ySol}],
      Text[
        Style["(" <> ToString[NumberForm[xSol,{4,2}]] <> ", " <>
               ToString[NumberForm[ySol,{4,2}]] <> ")",
               Red, 10, FontFamily -> "Arial"],
        {xSol, ySol}, {-1.1, 1.5}
      ]
    }];
    pLabel = Graphics[{Dashed, LightRed,
      Line[{{xSol, 0}, {xSol, ySol}}]
    }];
    Show[pGrafico, pPunto, pLabel]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feValidaSeed                                         *)
(*                                                                           *)
(* Scopo:   Valida il seed inserito dall'utente. Se non e' un intero         *)
(*          valido emette FunctionExplorer::badseed e restituisce 1.         *)
(*                                                                           *)
(* Input:   seedInput_ - qualsiasi valore (numero o stringa)                 *)
(* Lavoro:  parsed - Round[ToExpression[...]] oppure $Failed                 *)
(* Output:  intero seed validato (>= 1)                                      *)
(* -------------------------------------------------------------------------*)
feValidaSeed[seedInput_] :=
  Module[{parsed},
    parsed = Quiet[Check[Round[ToExpression[ToString[seedInput]]], $Failed]];
    If[IntegerQ[parsed],
       Abs[parsed] + 1,
       Message[FunctionExplorer::badseed];
       1
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: feBottoneAvvia                                       *)
(*                                                                           *)
(* Scopo:   Genera il Button di avvio visibile all'utente nel Tutorial.nb.   *)
(*          Al click carica il pacchetto e lancia esercizioUniversale[].     *)
(*          Cio' rende il caricamento TRASPARENTE per l'utente               *)
(*          (nessun bisogno di eseguire celle di codice manualmente).        *)
(*                                                                           *)
(* Input:   nessuno                                                          *)
(* Output:  oggetto Button                                                   *)
(* -------------------------------------------------------------------------*)
feBottoneAvvia[] :=
  Button[
    Style[" \[FilledRightTriangle]  Avvia FunctionExplorer ", White, Bold, 14,
          FontFamily -> "Arial"],
    (* Il Button esegue SetDirectory + Get + lancia l'interfaccia          *)
    (* NotebookEvaluate non e' disponibile, usiamo ToExpression            *)
    (
      SetDirectory[NotebookDirectory[]];
      Get["FunctionExplorer.m"];
      CreateDocument[{
        ExpressionCell[esercizioUniversale[], "Output"]
      }]
    ),
    Background   -> RGBColor[0.1, 0.5, 0.1],
    FrameMargins -> 10
  ]


(* =========================================================================*)
(* FUNZIONE PRINCIPALE (PUBBLICA): esercizioUniversale                      *)
(*                                                                           *)
(* Scopo:   Genera e visualizza l'interfaccia interattiva tramite            *)
(*          DynamicModule. Implementa le 5 funzionalita' obbligatorie:       *)
(*            1. Seed        - InputField per scegliere il seed              *)
(*            2. Genera Esercizio  - Button verde                            *)
(*            3. Verifica Risultato - Button blu                             *)
(*            4. Mostra Soluzione  - Button viola -> MessageDialog pop-up   *)
(*            5. Pulisci Campi    - Button grigio                            *)
(*                                                                           *)
(*          Tutto il testo usa FontFamily -> "Arial" per accessibilita'      *)
(*          e leggibilita', come richiesto (anche per utenti con dislessia). *)
(*                                                                           *)
(* Input:   nessuno                                                          *)
(* Output:  oggetto DynamicModule (interfaccia interattiva nel notebook)     *)
(* =========================================================================*)
esercizioUniversale[] :=
  DynamicModule[
    (* Variabili di stato locali al DynamicModule (non Global)             *)
    {
      feStatoSeed        = 42,   (* seed corrente                          *)
      feStatoFunzione    = None, (* funzione pura generata                 *)
      feStatoDomanda     = None, (* Association tipo/parametri/testo       *)
      feStatoSoluzione   = None, (* risposta corretta calcolata            *)
      feStatoRisposta    = "",   (* stringa inserita dall'utente           *)
      feStatoFeedback    = "",   (* messaggio esito verifica               *)
      feStatoGrafico     = None, (* grafico iniziale senza annotazioni     *)
      feStatoOn          = False (* True dopo il primo Genera Esercizio    *)
    },

    Column[{

      (* TITOLO \[LongDash] font Arial, come richiesto dalla professoressa           *)
      Style["FunctionExplorer", Bold, 20, FontFamily -> "Arial",
            RGBColor[0.1, 0.3, 0.7]],
      Style["Simulatore e Quiz su Funzioni Matematiche",
            Italic, 12, FontFamily -> "Arial", Gray],
      Spacer[12],

      (* RIGA SEED                                                          *)
      Row[{
        Style["Seed:  ", Bold, 12, FontFamily -> "Arial"],
        InputField[Dynamic[feStatoSeed], Number,
                   FieldSize -> 6, FieldHint -> "es. 42"]
      }],
      Spacer[8],

      (* BOTTONE 1: Genera Esercizio (verde)                               *)
      (* Chiama feValidaSeed, feGeneraFunzione, feGeneraDomanda,           *)
      (* feCalcolaRisposta \[LongDash] tutte funzioni ausiliarie separate            *)
      Button[
        Style[" \[FilledRightTriangle]  Genera Esercizio ", White, Bold, 12,
              FontFamily -> "Arial"],
        Module[{sv, f, dom, sol},
          sv  = feValidaSeed[feStatoSeed];
          f   = feGeneraFunzione[sv];
          dom = feGeneraDomanda[sv];
          sol = feCalcolaRisposta[f, dom];
          feStatoFunzione  = f;
          feStatoDomanda   = dom;
          feStatoSoluzione = sol;
          feStatoRisposta  = "";
          feStatoFeedback  = "";
          feStatoGrafico   = Plot[
            f[x], {x, -4, 4},
            PlotStyle      -> {Blue, Thickness[0.003]},
            AxesLabel      -> {Style["x", FontFamily -> "Arial"],
                               Style["f(x)", FontFamily -> "Arial"]},
            PlotLabel      -> Style["Grafico di f(x)", Bold, 12,
                                     FontFamily -> "Arial"],
            GridLines      -> Automatic,
            GridLinesStyle -> LightGray,
            ImageSize      -> 420
          ];
          feStatoOn = True
        ],
        Background -> RGBColor[0.1, 0.5, 0.1], FrameMargins -> 7
      ],
      Spacer[10],

      (* GRAFICO: placeholder prima di Genera Esercizio                    *)
      Dynamic[
        If[feStatoOn,
           feStatoGrafico,
           Framed[
             Style["Premi Genera Esercizio per iniziare.",
                   Gray, Italic, 11, FontFamily -> "Arial"],
             FrameStyle -> LightGray, RoundingRadius -> 4,
             FrameMargins -> 12, ImageSize -> {420, 50}, Alignment -> Center
           ]
        ]
      ],
      Spacer[6],

      (* TESTO DOMANDA: riquadro giallo                                    *)
      Dynamic[
        If[feStatoOn,
           Framed[
             Style[feStatoDomanda["Testo"], 12, FontFamily -> "Arial"],
             Background -> LightYellow, FrameStyle -> Orange,
             RoundingRadius -> 4, FrameMargins -> 10
           ],
           ""
        ]
      ],
      Spacer[8],

      (* CAMPO RISPOSTA                                                    *)
      Dynamic[
        If[feStatoOn,
           Row[{
             Style["La tua risposta:  ", Bold, 12, FontFamily -> "Arial"],
             InputField[Dynamic[feStatoRisposta], String,
                        FieldSize -> 10, FieldHint -> "es. 1.57"]
           }],
           ""
        ]
      ],
      Spacer[10],

      (* RIGA BOTTONI AZIONE                                               *)
      Dynamic[
        If[feStatoOn,
           Row[{

             (* BOTTONE 2: Verifica Risultato (blu)                        *)
             (* Chiama feVerificaRisposta (funzione ausiliaria separata)   *)
             Button[
               Style[" \[Checkmark]  Verifica Risultato ", White, Bold, 12,
                     FontFamily -> "Arial"],
               Module[{valIn, esito},
                 valIn = Quiet[Check[ToExpression[feStatoRisposta], $Failed]];
                 If[!NumericQ[valIn],
                    Message[FunctionExplorer::badinput];
                    feStatoFeedback =
                      "\[WarningSign]  Inserisci un numero valido (es. 3.14).",
                    esito = feVerificaRisposta[valIn, feStatoSoluzione];
                    feStatoFeedback = If[esito,
                      "\[Checkmark]  Corretto! Ottimo lavoro.",
                      "\[Cross]  Non corretto. Riprova o usa Mostra Soluzione."
                    ]
                 ]
               ],
               Background -> RGBColor[0.1, 0.35, 0.7], FrameMargins -> 6
             ],
             Spacer[10],

             (* BOTTONE 3: Mostra Soluzione (viola) -> pop-up              *)
             (* Chiama feGraficoAnnotato (funzione ausiliaria separata)    *)
             Button[
               Style[" \[FilledSmallSquare]  Mostra Soluzione ", White, Bold, 12,
                     FontFamily -> "Arial"],
               Module[{gSol},
                 gSol = feGraficoAnnotato[
                   feStatoFunzione, feStatoSoluzione, feStatoDomanda];
                 MessageDialog[Column[{
                   Style["Soluzione", Bold, 15, Blue, FontFamily -> "Arial"],
                   Spacer[4],
                   Style[
                     "Risposta corretta:  x \[TildeEqual] " <>
                     ToString[NumberForm[N[feStatoSoluzione], {5,3}]],
                     12, FontFamily -> "Arial"
                   ],
                   Spacer[6],
                   gSol
                 }]]
               ],
               Background -> RGBColor[0.5, 0.1, 0.6], FrameMargins -> 6
             ],
             Spacer[10],

             (* BOTTONE 4: Pulisci Campi (grigio)                          *)
             (* Reimposta tutte le variabili di stato allo stato iniziale  *)
             Button[
               Style[" \[FilledSquare]  Pulisci Campi ", White, Bold, 12,
                     FontFamily -> "Arial"],
               feStatoFunzione  = None;
               feStatoDomanda   = None;
               feStatoSoluzione = None;
               feStatoRisposta  = "";
               feStatoFeedback  = "";
               feStatoGrafico   = None;
               feStatoOn        = False,
               Background -> RGBColor[0.45, 0.45, 0.45], FrameMargins -> 6
             ]
           }],
           ""
        ]
      ],
      Spacer[8],

      (* FEEDBACK: riquadro verde (corretto) o rosso (errato)              *)
      Dynamic[
        If[feStatoFeedback =!= "",
           Framed[
             Style[feStatoFeedback, Bold, 12, FontFamily -> "Arial",
               If[StringContainsQ[feStatoFeedback, "\[Checkmark]"],
                  Darker[Green], Darker[Red]]
             ],
             Background ->
               If[StringContainsQ[feStatoFeedback, "\[Checkmark]"],
                  LightGreen, LightRed],
             FrameStyle ->
               If[StringContainsQ[feStatoFeedback, "\[Checkmark]"],
                  Green, Red],
             RoundingRadius -> 4, FrameMargins -> 10
           ],
           ""
        ]
      ]

    }, Alignment -> Left, Spacings -> 0]

  ] (* fine DynamicModule *)


End[]        (* chiude FunctionExplorer`Private` *)

EndPackage[] (* chiude FunctionExplorer` e ripristina $ContextPath *)



