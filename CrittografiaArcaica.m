(* ::Package:: *)

(* ============================================================
   Package.m
   Laboratorio Interattivo di Crittografia Arcaica
   Cifrario di Cesare e Cifrario di Vigenere

   Matematica Computazionale 2025/2026
   ============================================================ *)

BeginPackage["Package`"]

(* ============================================================
   DICHIARAZIONI DI USO (usage)
   ============================================================ *)

laboratorioCesare::usage =
  "laboratorioCesare[] apre il Laboratorio Libero del Cifrario di Cesare: \
cifratura e decifratura di testo (solo lettere), ruota interattiva \
con rotazione via mouse, analisi delle frequenze con riferimento italiano."

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre gli Esercizi del Cifrario di Cesare. \
L'utente riceve un testo CIFRATO e deve trovare il testo in CHIARO. \
Funzionalita': Genera Esercizio (Seed), Verifica Risultato con suggerimento \
automatico, Suggerimento progressivo (3 livelli), Mostra Soluzione, Pulisci Campi."

laboratorioVigenere::usage =
  "laboratorioVigenere[] apre il Laboratorio Libero del Cifrario di Vigenere. \
Chiave e testo accettano solo lettere. Include tabella degli shift e \
nota didattica su Cesare come sottocaso di Vigenere."

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre gli Esercizi del Cifrario di Vigenere. \
L'utente riceve un testo CIFRATO e la CHIAVE e deve trovare il testo in CHIARO. \
Funzionalita': Genera Esercizio (Seed), Verifica Risultato con suggerimento \
automatico, Suggerimento progressivo (3 livelli), Mostra Soluzione, Pulisci Campi."

Begin["`Private`"]

(* ============================================================
   COSTANTI
   ============================================================ *)

(* 26 lettere maiuscole *)
alfabeto = CharacterRange["A", "Z"];

(* Frequenze percentuali nell'italiano scritto.
   Fonte: De Mauro, Dizionario di frequenza dell'italiano.
   Ordine: A B C D E F G H I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z *)
freqItaliano = {11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54,
                11.28,  0.00, 0.00, 6.51, 2.51, 6.88, 9.83, 3.05,
                0.51,  6.37, 4.98, 5.62, 3.01, 2.10, 0.00, 0.00,
                0.00,  0.49};

(* ============================================================
   UTILITA' -- VALIDAZIONE INPUT
   ============================================================ *)

(*
  soloLettere[s]
  Input:  s -- stringa qualsiasi
  Output: True se s non e' vuota e contiene SOLO lettere A-Z (case insensitive).
  Uso:    validare sia il testo (Cesare/Vigenere) sia la chiave (Vigenere).
  Nota:   gli spazi NON sono ammessi; il testo deve essere una singola parola
          o sequenza di sole lettere senza punteggiatura.
*)
soloLettere[s_String] :=
  Module[{chars},
    chars = Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &];
    StringLength[s] > 0 && Length[chars] == StringLength[s]
  ]

(* ============================================================
   GENERATORE DI PAROLE CASUALI CON SEED
   ============================================================ *)

(*
  generaParola[seed]
  Input:  seed -- intero, garantisce la riproducibilita' (stesso seed = stessa parola)
  Output: stringa maiuscola di sole lettere, senza spazi (es. "MATEMATICA")
  Logica: dizionario di 100 parole italiane comuni legate alla crittografia,
          alla matematica e al lessico quotidiano. RandomChoice con SeedRandom
          garantisce riproducibilita' e un numero potenzialmente infinito di
          esercizi (seed diversi -> parole diverse). Le parole contengono solo
          lettere A-Z, quindi sono compatibili con il vincolo "solo lettere".
*)
generaParola[seed_Integer] :=
  Module[
    {dizionario},
    dizionario = {
      "MATEMATICA", "CRITTOGRAFIA", "ALFABETO", "CIFRARIO", "SEGRETO",
      "MESSAGGIO", "CHIAVE", "CODICE", "SCIENZA", "FORMULA",
      "NUMERO", "LETTERA", "SIMBOLO", "SISTEMA", "METODO",
      "TEOREMA", "ALGEBRA", "GEOMETRIA", "LOGICA", "CALCOLO",
      "VETTORE", "MATRICE", "FUNZIONE", "INSIEME", "SOLUZIONE",
      "PROBLEMA", "RISPOSTA", "DOMANDA", "RICERCA", "SCOPERTA",
      "ANALISI", "SINTESI", "VERIFICA", "RISULTATO", "OPERAZIONE",
      "SOMMA", "PRODOTTO", "DIVISIONE", "POTENZA", "RADICE",
      "PRIMO", "INTERO", "REALE", "COMPLESSO", "RAZIONALE",
      "SEQUENZA", "SERIE", "LIMITE", "DERIVATA", "INTEGRALE",
      "PROBABILITA", "STATISTICA", "CAMPIONE", "MEDIA", "VARIANZA",
      "ALGORITMO", "PROGRAMMA", "COMPUTER", "BINARIO", "DIGITALE",
      "RETE", "PROTOCOLLO", "SICUREZZA", "FIRMA", "CERTIFICATO",
      "CHIARO", "CIFRATO", "DECIFRARE", "CIFRARE", "TRASFORMARE",
      "ROTAZIONE", "SOSTITUZIONE", "PERMUTAZIONE", "TRASPOSIZIONE", "BLOCCO",
      "FREQUENZA", "DISTRIBUZIONE", "ISTOGRAMMA", "GRAFICO", "DIAGRAMMA",
      "STORIA", "ROMANO", "CESARE", "VIGENERE", "ENIGMA",
      "GUERRA", "MILITARE", "SPIONE", "AMBASCIATRICE", "MISSIONE",
      "UNIVERSO", "PIANETA", "STELLA", "GALASSIA", "COSMO",
      "NATURA", "ACQUA", "FUOCO", "TERRA", "VENTO",
      "LIBRO", "PAGINA", "CAPITOLO", "PARAGRAFO", "PAROLA"
    };
    SeedRandom[seed];
    RandomChoice[dizionario]
  ]

(* ============================================================
   CIFRATURA E DECIFRATURA -- CESARE
   ============================================================ *)

(*
  cifraCesare[testo, shift]
  Input:  testo -- stringa (deve contenere solo lettere, senza spazi)
          shift -- intero 0-25
  Output: stringa cifrata (stessa lunghezza di testo, solo lettere maiuscole).
  Logica: ogni lettera viene spostata di shift posizioni in avanti
          nell'alfabeto, con wrap-around modulo 26.
*)
cifraCesare[testo_String, shift_Integer] :=
  Module[{caratteri, cifrati},
    caratteri = Characters[ToUpperCase[testo]];
    cifrati = Map[
      Function[c,
        If[MemberQ[alfabeto, c],
          alfabeto[[ Mod[Position[alfabeto, c][[1,1]] - 1 + shift, 26] + 1 ]],
          c
        ]
      ],
      caratteri
    ];
    StringJoin[cifrati]
  ]

(*
  decifraCesare[testo, shift]
  Input:  testo -- stringa cifrata con Cesare
          shift -- intero 0-25 (lo stesso usato per cifrare)
  Output: testo in chiaro (shift negativo modulo 26).
*)
decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]]

(*
  frequenzeLettere[testo]
  Input:  testo -- stringa qualsiasi
  Output: lista di 26 interi (conteggio A, B, ..., Z nel testo maiuscolo).
*)
frequenzeLettere[testo_String] :=
  Module[{solo},
    solo = Select[Characters[ToUpperCase[testo]], MemberQ[alfabeto, #] &];
    Map[Function[l, Count[solo, l]], alfabeto]
  ]

(* ============================================================
   CIFRATURA E DECIFRATURA -- VIGENERE
   ============================================================ *)

(*
  cifraVigenere[testo, chiave]
  Input:  testo  -- stringa di sole lettere (maiuscole o minuscole)
          chiave -- stringa di sole lettere (lunghezza >= 1)
  Output: stringa cifrata, oppure messaggio di errore se chiave non valida.
  Logica: la chiave si ripete ciclicamente sul testo.
          Per ogni lettera del testo lo shift e' la posizione 0-indicizzata
          della lettera chiave corrispondente (A=0, B=1, ..., Z=25).
          L'indice della chiave avanza SOLO sulle lettere del testo.
*)
cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        sh = Position[alfabeto, chiaveChars[[Mod[kIndex, chiaveLen] + 1]]][[1,1]] - 1;
        AppendTo[risultato,
          alfabeto[[Mod[Position[alfabeto, c][[1,1]] - 1 + sh, 26] + 1]]
        ];
        kIndex++,
        AppendTo[risultato, c]
      ],
      {i, 1, Length[caratteri]}
    ];
    StringJoin[risultato]
  ]

(*
  decifraVigenere[testo, chiave]
  Input:  testo  -- stringa cifrata con Vigenere
          chiave -- stringa chiave (solo lettere, stessa usata per cifrare)
  Output: testo in chiaro (shift negativo modulo 26 per ogni lettera).
*)
decifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        sh = Position[alfabeto, chiaveChars[[Mod[kIndex, chiaveLen] + 1]]][[1,1]] - 1;
        AppendTo[risultato,
          alfabeto[[Mod[Position[alfabeto, c][[1,1]] - 1 - sh, 26] + 1]]
        ];
        kIndex++,
        AppendTo[risultato, c]
      ],
      {i, 1, Length[caratteri]}
    ];
    StringJoin[risultato]
  ]

(*
  tabellaShiftVigenere[testo, chiave]
  Input:  testo  -- stringa di testo in chiaro (solo lettere)
          chiave -- stringa chiave (solo lettere)
  Output: lista di quadruple {lettera_chiaro, lettera_chiave, shift, lettera_cifrata},
          limitata alle prime 24 lettere per leggibilita'.
*)
tabellaShiftVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, soleLettere, risultato, kIndex, sh, lCifr},
    testUp      = ToUpperCase[testo];
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {}, Return[{}]];
    chiaveLen   = Length[chiaveChars];
    soleLettere = Select[Characters[testUp], MemberQ[alfabeto, #] &];
    risultato   = {};
    kIndex      = 0;
    Do[
      sh    = Position[alfabeto, chiaveChars[[Mod[kIndex, chiaveLen] + 1]]][[1,1]] - 1;
      lCifr = alfabeto[[Mod[Position[alfabeto, soleLettere[[i]]][[1,1]] - 1 + sh, 26] + 1]];
      AppendTo[risultato, {soleLettere[[i]], chiaveChars[[Mod[kIndex, chiaveLen] + 1]], sh, lCifr}];
      kIndex++,
      {i, 1, Min[Length[soleLettere], 24]}
    ];
    risultato
  ]

(* ============================================================
   GENERATORI DI ESERCIZI CON SEED
   ============================================================ *)

(*
  generaEsercizioConSeedCesare[seed]
  Input:  seed -- intero (riproducibilita')
  Output: {testo_cifrato, shift_segreto, testo_chiaro}
  Struttura: l'utente riceve il testo CIFRATO e deve trovare il CHIARO.
  Nota: lo shift usa seed+999 come seme separato dalla frase.
*)
generaEsercizioConSeedCesare[seed_Integer] :=
  Module[{parola, shift, cifrato},
    (* Genera una parola singola senza spazi, riproducibile col seed *)
    parola  = generaParola[seed];
    (* Shift indipendente: usa seed+999 come seme separato *)
    SeedRandom[seed + 999];
    shift   = RandomInteger[{1, 25}];
    cifrato = cifraCesare[parola, shift];
    {cifrato, shift, parola}
  ]

(*
  generaEsercizioConSeedVigenere[seed]
  Input:  seed -- intero (riproducibilita')
  Output: {testo_cifrato, chiave_segreta, testo_chiaro}
  Struttura: l'utente riceve il testo CIFRATO e la CHIAVE, deve trovare il CHIARO.
  Nota: la chiave usa seed+777 come seme separato dalla frase.
*)
generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {chiavi, parola, chiave, cifrato},
    chiavi = {"SOLE", "MARE", "LUNA", "VENTO", "FUOCO", "ACQUA",
              "CIELO", "TERRA", "LUCE", "OMBRA", "CHIAVE", "CODICE",
              "PIETRA", "FIUME", "STELLA", "NOTTE", "GIORNO"};
    (* Genera una parola singola senza spazi, riproducibile col seed *)
    parola = generaParola[seed];
    (* Chiave indipendente: usa seed+777 come seme separato *)
    SeedRandom[seed + 777];
    chiave  = RandomChoice[chiavi];
    cifrato = cifraVigenere[parola, chiave];
    (* Output: {cifrato, chiave, chiaro} -- l'utente riceve cifrato e chiave *)
    {cifrato, chiave, parola}
  ]

(* ============================================================
   GRAFICO FREQUENZE -- DUE PANNELLI SEPARATI
   ============================================================ *)

(*
  graficaFrequenze[testo]
  Input:  testo -- stringa (tipicamente il testo cifrato da analizzare)
  Output: Column con due BarChart separati, esattamente come da specifiche:
          1. Grafico SUPERIORE: "Standard Italiano (%)"
             Barre azzurre uniformi con le frequenze attese dell'italiano.
          2. Grafico INFERIORE: "Frequenze Testo Cifrato (Assolute)"
             Barre colorate (palette arcobaleno) con i conteggi assoluti
             delle lettere nel testo analizzato.
  Didattica: confrontando i due grafici, l'utente vede come lo shift
             di Cesare "sposta" i picchi rispetto al profilo italiano.
             Il picco della E nell'italiano corrisponde al picco
             della lettera piu' frequente nel testo cifrato.
*)
graficaFrequenze[testo_String] :=
  Module[
    {conteggi, totale, coloriArcobaleno,
     graficoItaliano, graficoCifrato,
     etichette, n},
    conteggi = frequenzeLettere[testo];
    totale   = Total[conteggi];
    If[totale == 0,
      Return[Style[
        "(Nessuna lettera nel testo: impossibile calcolare le frequenze.)",
        11, Italic, Gray]]
    ];
    n = 26;
    (* Palette arcobaleno per il grafico del testo cifrato (26 colori) *)
    coloriArcobaleno = Table[Hue[k/26, 0.6, 0.85], {k, 0, 25}];
    (* Etichette A-Z posizionate manualmente via Epilog.
       In BarChart con n barre, la k-esima barra ha centro x = k (1-indicizzato).
       Posizioniamo il testo a y = -0.3 (sotto l'asse) per ogni barra. *)
    etichette = Table[
      Text[Style[alfabeto[[k]], 8, Bold, GrayLevel[0.3]], {k, -0.45}],
      {k, 1, n}
    ];
    (* --- Grafico 1: Standard Italiano --- *)
    graficoItaliano = BarChart[
      freqItaliano,
      ChartStyle  -> RGBColor[0.65, 0.80, 0.92],
      AxesLabel   -> {None, None},
      PlotRange   -> {{0, n + 0.5}, {-0.7, Max[freqItaliano] * 1.20}},
      ImageSize   -> {480, 220},
      PlotLabel   -> Style["Standard Italiano (%)", 12, Bold, GrayLevel[0.3]],
      BarSpacing  -> 0.3,
      Frame       -> False,
      Axes        -> {False, True},
      Epilog      -> etichette
    ];
    (* --- Grafico 2: Frequenze Testo Cifrato (Assolute) --- *)
    graficoCifrato = BarChart[
      conteggi,
      ChartStyle  -> coloriArcobaleno,
      AxesLabel   -> {None, None},
      PlotRange   -> {{0, n + 0.5}, {-0.15, Max[conteggi, 1] * 1.20}},
      ImageSize   -> {480, 220},
      PlotLabel   -> Style["Frequenze Testo Cifrato (Assolute)", 12, Bold, GrayLevel[0.3]],
      BarSpacing  -> 0.3,
      Frame       -> False,
      Axes        -> {False, True},
      Epilog      -> etichette
    ];
    Column[{graficoItaliano, graficoCifrato}, Spacings -> 1, Alignment -> Center]
  ]

(* ============================================================
   RUOTA DI CESARE -- CON ROTAZIONE TRAMITE MOUSE
   ============================================================ *)

(*
  ruotaCesare[shift, highlightK]
  Input:  shift      -- intero 0-25, lo shift corrente
          highlightK -- indice 0-based del settore evidenziato (-1 = nessuno)
  Output: Graphics con anello esterno (chiaro) e disco interno (cifrato).
  FIX colori: palette Hue con 26 tinte distinte; contrasto garantito.
*)
ruotaCesare[shift_Integer, highlightK_Integer] :=
  Module[
    {n, rEst, rInt, rMid, angC, cEst, cInt,
     settoriEst, lettEst, settoriInt, lettInt},
    n    = 26;
    rEst = 1.0;    (* bordo esterno anello chiaro *)
    rInt = 0.68;   (* confine tra anello e disco *)
    rMid = 0.36;   (* meta' del disco per le etichette interne *)
    (* Centro del settore k: parte da Pi/2 (cima), antiorario *)
    angC[k_] := Pi/2 - 2 Pi k / n;
    (* Palette: 26 colori ben separati *)
    cEst = Table[Hue[k/n, 0.50, 0.88], {k, 0, n-1}];
    cInt = Table[Hue[k/n, 0.85, 0.55], {k, 0, n-1}];
    (* Settori anello esterno *)
    settoriEst = Table[
      {cEst[[k+1]],
       If[k == highlightK, Opacity[1.0], Opacity[0.65]],
       Annulus[{0,0}, {rInt, rEst}, {angC[k] - Pi/n, angC[k] + Pi/n}]},
      {k, 0, n-1}
    ];
    (* Etichette anello esterno *)
    lettEst = Table[
      Text[
        Style[alfabeto[[k+1]],
          If[k == highlightK, 16, 11], Bold,
          If[k == highlightK, Black, GrayLevel[0.1]]],
        {(rInt + (rEst - rInt)/2) * Cos[angC[k]],
         (rInt + (rEst - rInt)/2) * Sin[angC[k]]}],
      {k, 0, n-1}
    ];
    (* Disco interno (cifrato), settori ruotati di shift *)
    settoriInt = Table[
      {cInt[[Mod[k + shift, n] + 1]], Opacity[0.90],
       Disk[{0,0}, rInt, {angC[k] - Pi/n, angC[k] + Pi/n}]},
      {k, 0, n-1}
    ];
    (* Etichette disco interno *)
    lettInt = Table[
      Text[
        Style[alfabeto[[Mod[k + shift, n] + 1]], 10, Bold, White],
        {(rMid + (rInt - rMid)/2) * Cos[angC[k]],
         (rMid + (rInt - rMid)/2) * Sin[angC[k]]}],
      {k, 0, n-1}
    ];
    Graphics[
      Join[
        settoriEst, lettEst,
        settoriInt, lettInt,
        {Text[Style["Chiaro",  10, Italic, GrayLevel[0.4]], {0,  1.17}]},
        {Text[Style["Cifrato", 10, Italic, White],          {0,  0.0 }]},
        {Thick, Red, Arrow[{{0, 1.33}, {0, 1.04}}]},
        {Thick, GrayLevel[0.25], Circle[{0,0}, rInt]}
      ],
      ImageSize  -> 320,
      Background -> GrayLevel[0.12],
      PlotRange  -> {{-1.42, 1.42}, {-1.42, 1.42}}
    ]
  ]

(*
  ruotaInterattiva[shiftDyn]
  Input:  shiftDyn -- variabile Dynamic con lo shift corrente
  Output: DynamicModule con Slider circolare e ruota.
  FEAT mouse: usiamo uno Slider orizzontale che controlla l'angolo
              di "puntamento" sull'anello esterno, il che evidenzia
              il settore corrispondente e mostra la coppia chiaro/cifrato.
  Nota tecnica: MousePosition["Graphics"] e' inaffidabile dentro
              EventHandler quando la cella e' in un Panel annidato.
              La soluzione stabile in Mathematica e' usare uno Slider
              separato per l'angolo (0-25) che l'utente muove con
              il mouse, abbinato a Dynamic per l'highlight del settore.
*)
ruotaInterattiva[shiftDyn_] :=
  DynamicModule[
    {settoreCorrente = 0},
    Column[{
      (* Slider per selezionare il settore dell'anello esterno *)
      Row[{
        Style["Punta la lettera: ", 11, Italic, Gray],
        Slider[Dynamic[settoreCorrente], {0, 25, 1}, ImageSize -> 220],
        Spacer[6],
        Dynamic[
          Style[
            alfabeto[[settoreCorrente + 1]] <> " \[RightArrow] " <>
            alfabeto[[Mod[settoreCorrente + shiftDyn, 26] + 1]],
            14, Bold, RGBColor[0.2, 0.4, 0.7]
          ]
        ]
      }],
      Dynamic[ruotaCesare[shiftDyn, settoreCorrente]]
    }, Alignment -> Center]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO CESARE
   ============================================================ *)

(*
  laboratorioCesare[]
  Input:  nessuno
  Output: DynamicModule con:
          - campo testo (solo lettere, spazi esclusi -- FIX)
          - slider shift 0-25
          - bottoni Cifra / Decifra / Pulisci Campi
          - ruota interattiva con slider di puntamento
          - grafico frequenze con curva di riferimento italiano
*)
laboratorioCesare[] :=
  DynamicModule[
    {testoInput = "", shiftLab = 3,
     risultatoCifra = "", risultatoDecifra = "",
     avvisoTesto = ""},
    Panel[
      Column[{
        Style["Laboratorio Libero \[LongDash] Cifrario di Cesare", 18, Bold, RGBColor[0.2, 0.4, 0.7]],
        Style["Inserisci un testo (solo lettere, senza spazi), scegli lo shift e cifra o decifra.",
              12, Italic, Gray],
        Spacer[8],
        (* Campo testo con validazione *)
        Row[{
          Style["Testo: ", 13, Bold],
          InputField[Dynamic[testoInput], String,
            FieldSize -> {30, 2},
            FieldHint -> "Solo lettere (es. CIAO)"],
          Spacer[8],
          (* FIX: avviso in tempo reale se il testo contiene caratteri non validi *)
          Dynamic[
            If[testoInput =!= "" && !soloLettere[testoInput],
              Style["! Solo lettere, senza spazi", 11, Bold, RGBColor[0.8, 0.2, 0.0]],
              ""
            ]
          ]
        }],
        Spacer[4],
        (* Slider shift *)
        Row[{
          Style["Shift (k): ", 13, Bold],
          Slider[Dynamic[shiftLab], {0, 25, 1}, ImageSize -> 200],
          Spacer[8],
          Dynamic[Style[ToString[shiftLab], 15, Bold, RGBColor[0.7, 0.2, 0.2]]]
        }],
        Spacer[6],
        (* Bottoni *)
        Row[{
          Button[
            Style["Cifra \[RightArrow]", 13, Bold, White],
            If[testoInput === "" || !soloLettere[testoInput],
              avvisoTesto = "Inserisci un testo valido (solo lettere, senza spazi).",
              avvisoTesto      = "";
              risultatoCifra   = cifraCesare[testoInput, shiftLab];
              risultatoDecifra = "";
            ],
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {120, 35}
          ],
          Spacer[10],
          Button[
            Style["\[LeftArrow] Decifra", 13, Bold, White],
            If[testoInput === "" || !soloLettere[testoInput],
              avvisoTesto = "Inserisci un testo valido (solo lettere, senza spazi).",
              avvisoTesto      = "";
              risultatoDecifra = decifraCesare[testoInput, shiftLab];
              risultatoCifra   = "";
            ],
            Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}
          ],
          Spacer[10],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            testoInput       = "";
            shiftLab         = 3;
            risultatoCifra   = "";
            risultatoDecifra = "";
            avvisoTesto      = "";,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}
          ]
        }],
        Spacer[4],
        Dynamic[If[avvisoTesto =!= "",
          Style[avvisoTesto, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Spacer[4],
        Dynamic[If[risultatoCifra =!= "",
          Framed[Column[{
            Style["Testo cifrato:", 12, Bold, RGBColor[0.2, 0.6, 0.3]],
            Style[risultatoCifra, 14, Bold]}],
            Background -> RGBColor[0.92, 1.0, 0.93],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.2, 0.6, 0.3], FrameMargins -> 8],
          ""]],
        Dynamic[If[risultatoDecifra =!= "",
          Framed[Column[{
            Style["Testo decifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
            Style[risultatoDecifra, 14, Bold]}],
            Background -> RGBColor[0.97, 0.93, 1.0],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8],
          ""]],
        Spacer[10],
        (* Ruota interattiva *)
        Style["Ruota di Cesare \[LongDash] usa lo slider per puntare una lettera dell'anello esterno:",
              13, Bold],
        Style["La lettera a destra della freccia mostra la corrispondente lettera cifrata.",
              11, Italic, Gray],
        Dynamic[ruotaInterattiva[shiftLab]],
        Spacer[10],
        (* Grafico frequenze *)
        Style["Analisi delle frequenze:", 13, Bold],
        Style["Sopra: profilo standard dell'italiano. Sotto: frequenze assolute delle lettere nel testo cifrato.",
              11, Italic, Gray],
        Dynamic[If[risultatoCifra =!= "",
          graficaFrequenze[risultatoCifra],
          Style["(Il grafico appare dopo aver cifrato un testo)", 11, Italic, Gray]]]
      },
      Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE CESARE
   Struttura: l'utente riceve il testo CIFRATO, trova il CHIARO.
   ============================================================ *)

(*
  esercizioUniversaleCesare[]
  Input:  nessuno
  Output: DynamicModule con le 5 funzionalita' obbligatorie:
          1. Genera Esercizio (Seed + frasi infinite)
          2. Verifica Risultato (suggerimento automatico al 1\[Degree] e 2\[Degree] errore)
          3. Suggerimento progressivo (3 livelli manuali)
          4. Mostra Soluzione
          5. Pulisci Campi
*)
esercizioUniversaleCesare[] :=
  DynamicModule[
    {seed = 42, messaggioCifrato = "", shiftSegreto = 0,
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[{
        Style["Esercizi \[LongDash] Cifrario di Cesare", 18, Bold, RGBColor[0.2, 0.4, 0.7]],
        Style["Ti viene dato un testo cifrato: trova il testo originale in chiaro!",
              12, Italic, Gray],
        Spacer[8],
        Row[{
          Style["Seed: ", 13, Bold],
          InputField[Dynamic[seed], Number, FieldSize -> {8,1}, FieldHint -> "es. 42"],
          Spacer[8],
          Button[
            Style["Genera Esercizio", 13, Bold, White],
            Module[{ris},
              ris               = generaEsercizioConSeedCesare[seed];
              messaggioCifrato  = ris[[1]];
              shiftSegreto      = ris[[2]];
              messaggioChiaro   = ris[[3]];
              rispostaUtente    = "";
              tentativi         = 0;
              feedbackMsg       = "";
              soluzioneVisibile = False;
              suggerimentoStep  = 0;
              esercizioGenerato = True;
            ],
            Background -> RGBColor[0.15, 0.5, 0.8], ImageSize -> {160, 35}
          ]
        }],
        Spacer[8],
        Dynamic[If[esercizioGenerato,
          Framed[Column[{
            Style["Testo cifrato da decifrare:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
            Style[messaggioCifrato, 15, Bold, Black]}],
            Background -> RGBColor[1.0, 0.95, 0.88],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.7, 0.3, 0.1], FrameMargins -> 8],
          Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]]],
        Spacer[6],
        Dynamic[If[esercizioGenerato,
          Column[{
            Style["Inserisci il testo decifrato (solo lettere, senza spazi):", 12, Bold],
            InputField[Dynamic[rispostaUtente], String,
              FieldSize -> {30, 2},
              FieldHint -> "Scrivi qui la tua risposta..."]}],
          ""]],
        Spacer[6],
        Row[{
          Button[
            Style["Verifica Risultato", 13, Bold, White],
            If[esercizioGenerato,
              tentativi++;
              If[ToUpperCase[StringReplace[StringTrim[rispostaUtente], " " -> ""]] ===
                 StringReplace[messaggioChiaro, " " -> ""],
                (* Corretto *)
                feedbackMsg = "\[Checkmark] Corretto! Hai impiegato " <>
                  ToString[tentativi] <>
                  If[tentativi == 1, " tentativo.", " tentativi."],
                (* Errato *)
                If[tentativi >= 3,
                  feedbackMsg       = "\[Cross] Risposta errata. Tentativi esauriti.";
                  soluzioneVisibile = True,
                  (* Suggerimento automatico integrato *)
                  suggerimentoStep = tentativi;
                  feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                    ToString[tentativi] <> "/3.  " <>
                    Which[
                      tentativi == 1,
                        "Suggerimento: la lettera piu' frequente in italiano e' la E.",
                      tentativi == 2,
                        "Suggerimento: lo shift e' nell'intervallo [" <>
                        ToString[Max[1, shiftSegreto - 4]] <> ", " <>
                        ToString[Min[25, shiftSegreto + 4]] <> "].",
                      True, ""
                    ]
                ]
              ]
            ],
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {170, 35}
          ],
          Spacer[8],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            seed = 42; messaggioCifrato = ""; shiftSegreto = 0;
            messaggioChiaro = ""; rispostaUtente = "";
            tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
            suggerimentoStep = 0; esercizioGenerato = False;,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {140, 35}
          ]
        }],
        Spacer[4],
        Row[{
          Button[
            Style["Suggerimento", 13, Bold, White],
            If[esercizioGenerato && suggerimentoStep < 3, suggerimentoStep++],
            Background -> RGBColor[0.8, 0.6, 0.1], ImageSize -> {140, 35}
          ],
          Spacer[8],
          Button[
            Style["Mostra Soluzione", 13, Bold, White],
            If[esercizioGenerato, soluzioneVisibile = True],
            Background -> RGBColor[0.7, 0.2, 0.2], ImageSize -> {160, 35}
          ]
        }],
        Spacer[8],
        (* Feedback con suggerimento automatico *)
        Dynamic[If[feedbackMsg =!= "",
          Framed[
            Style[feedbackMsg, 12,
              If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                 RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]],
            Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.92, 0.92]],
            RoundingRadius -> 5, FrameMargins -> 8],
          ""]],
        (* Suggerimento manuale progressivo *)
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento 1: la lettera piu' frequente in italiano e' la E. \
Conta le lettere nel testo cifrato: quella piu' frequente probabilmente corrisponde alla E.",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8],
          suggerimentoStep == 2,
            Framed[Style[
              "\[LightBulb] Suggerimento 2: lo shift e' nell'intervallo [" <>
              ToString[Max[1, shiftSegreto - 4]] <> ", " <>
              ToString[Min[25, shiftSegreto + 4]] <> "].",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8],
          suggerimentoStep >= 3,
            Framed[Style[
              "\[LightBulb] Suggerimento 3: lo shift esatto e' " <>
              ToString[shiftSegreto] <> ". Usa il Laboratorio per verificare.",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8]
        ]],
        (* Soluzione *)
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Shift: ", 12, Bold], Style[ToString[shiftSegreto], 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[1.0, 0.93, 0.93],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.6, 0.1, 0.1], FrameMargins -> 10],
          ""]]
      },
      Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO VIGENERE
   FIX: chiave E testo solo lettere
   FEAT: nota "Cesare e' sottocaso di Vigenere"
   ============================================================ *)

(*
  laboratorioVigenere[]
  Input:  nessuno
  Output: DynamicModule con:
          - testo (solo lettere) e chiave (solo lettere) con validazione
          - bottoni Cifra / Decifra / Pulisci Campi
          - tabella shift a 4 colonne (chiaro, chiave, shift, cifrato)
          - nota didattica: Cesare come sottocaso di Vigenere
*)
laboratorioVigenere[] :=
  DynamicModule[
    {testoInput = "", chiaveInput = "", risultatoCifra = "",
     risultatoDecifra = "", tabellaVis = {},
     avvisoTesto = "", avvisoChiave = ""},
    Panel[
      Column[{
        Style["Laboratorio Libero \[LongDash] Cifrario di Vigenere", 18, Bold, RGBColor[0.5, 0.2, 0.7]],
        Style["Inserisci un testo e una parola chiave (solo lettere, senza spazi).",
              12, Italic, Gray],
        Spacer[6],
        (* FEAT: nota didattica -- Cesare come sottocaso di Vigenere *)
        Framed[
          Column[{
            Style["\[FilledSquare] NOTA IMPORTANTE \[LongDash] Il Cifrario di Cesare \
e' un caso speciale di Vigenere", 12, Bold, RGBColor[0.35, 0.1, 0.60]],
            Style["Se la chiave e' composta da una sola lettera (es. 'D'), \
il Cifrario di Vigenere si riduce esattamente al Cifrario di Cesare con shift \
pari alla posizione di quella lettera nell'alfabeto (A=0, B=1, C=2, D=3, ...). \
Prova: cifra qualcosa con chiave 'D' e confrontalo col Laboratorio Cesare con shift 3!",
                  11, Italic, RGBColor[0.3, 0.1, 0.5]]
          }],
          Background -> RGBColor[0.96, 0.90, 1.0],
          RoundingRadius -> 6, FrameStyle -> RGBColor[0.6, 0.3, 0.9], FrameMargins -> 10
        ],
        Spacer[8],
        (* Campo testo con validazione *)
        Row[{
          Style["Testo:  ", 13, Bold],
          InputField[Dynamic[testoInput], String,
            FieldSize -> {25, 2}, FieldHint -> "Solo lettere (es. CIAO)"],
          Spacer[8],
          Dynamic[If[testoInput =!= "" && !soloLettere[testoInput],
            Style["! Solo lettere", 11, Bold, RGBColor[0.8, 0.2, 0.0]], ""]]
        }],
        Spacer[4],
        (* Campo chiave con validazione *)
        Row[{
          Style["Chiave: ", 13, Bold],
          InputField[Dynamic[chiaveInput], String,
            FieldSize -> {15, 1}, FieldHint -> "Solo lettere (es. SOLE)"],
          Spacer[8],
          Dynamic[If[chiaveInput =!= "" && !soloLettere[chiaveInput],
            Style["! Solo lettere", 11, Bold, RGBColor[0.8, 0.2, 0.0]], ""]]
        }],
        Spacer[6],
        (* Bottoni *)
        Row[{
          Button[
            Style["Cifra \[RightArrow]", 13, Bold, White],
            Which[
              testoInput === "" || !soloLettere[testoInput],
                avvisoTesto = "Testo non valido: inserisci solo lettere senza spazi.",
              chiaveInput === "" || !soloLettere[chiaveInput],
                avvisoChiave = "Chiave non valida: inserisci solo lettere senza spazi.",
              True,
                avvisoTesto = ""; avvisoChiave = "";
                risultatoCifra   = cifraVigenere[testoInput, chiaveInput];
                risultatoDecifra = "";
                tabellaVis       = tabellaShiftVigenere[testoInput, chiaveInput];
            ],
            Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}
          ],
          Spacer[10],
          Button[
            Style["\[LeftArrow] Decifra", 13, Bold, White],
            Which[
              testoInput === "" || !soloLettere[testoInput],
                avvisoTesto = "Testo non valido: inserisci solo lettere senza spazi.",
              chiaveInput === "" || !soloLettere[chiaveInput],
                avvisoChiave = "Chiave non valida: inserisci solo lettere senza spazi.",
              True,
                avvisoTesto = ""; avvisoChiave = "";
                risultatoDecifra = decifraVigenere[testoInput, chiaveInput];
                risultatoCifra   = "";
                tabellaVis       = tabellaShiftVigenere[testoInput, chiaveInput];
            ],
            Background -> RGBColor[0.2, 0.5, 0.7], ImageSize -> {120, 35}
          ],
          Spacer[10],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            testoInput = ""; chiaveInput = "";
            risultatoCifra = ""; risultatoDecifra = "";
            tabellaVis = {}; avvisoTesto = ""; avvisoChiave = "";,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}
          ]
        }],
        Spacer[4],
        Dynamic[If[avvisoTesto =!= "",
          Style[avvisoTesto, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Dynamic[If[avvisoChiave =!= "",
          Style[avvisoChiave, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Spacer[4],
        Dynamic[If[risultatoCifra =!= "",
          Framed[Column[{
            Style["Testo cifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
            Style[risultatoCifra, 14, Bold]}],
            Background -> RGBColor[0.96, 0.92, 1.0],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8],
          If[risultatoDecifra =!= "",
            Framed[Column[{
              Style["Testo decifrato:", 12, Bold, RGBColor[0.2, 0.5, 0.7]],
              Style[risultatoDecifra, 14, Bold]}],
              Background -> RGBColor[0.92, 0.97, 1.0],
              RoundingRadius -> 5, FrameStyle -> RGBColor[0.2, 0.5, 0.7], FrameMargins -> 8],
            ""]]],
        Spacer[10],
        Style["Tabella degli shift lettera per lettera:", 13, Bold],
        Style["Chiaro \[RightArrow] + Shift(Chiave) \[RightArrow] Cifrato",
              11, Italic, Gray],
        Dynamic[If[tabellaVis =!= {},
          Module[{righe},
            righe = Map[
              {Style[#[[1]], 13, Bold, Black],
               Style[#[[2]], 13, Bold, RGBColor[0.5, 0.2, 0.7]],
               Style["+" <> ToString[#[[3]]], 12, RGBColor[0.2, 0.6, 0.3]],
               Style[#[[4]], 13, Bold, RGBColor[0.7, 0.3, 0.1]]} &,
              tabellaVis
            ];
            Grid[
              Prepend[righe,
                {Style["Lettera chiaro",  11, Bold, Gray],
                 Style["Lettera chiave",  11, Bold, Gray],
                 Style["Shift",           11, Bold, Gray],
                 Style["Lettera cifrata", 11, Bold, Gray]}],
              Frame -> All,
              Background -> {None, {RGBColor[0.9, 0.85, 1.0], {White}}},
              FrameStyle -> LightGray, Spacings -> {1.5, 0.8}]],
          Style["(La tabella appare dopo la cifratura o decifratura)", 11, Italic, Gray]]]
      },
      Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE VIGENERE
   FIX struttura: l'utente riceve CIFRATO + CHIAVE, trova il CHIARO.
   ============================================================ *)

(*
  esercizioUniversaleVigenere[]
  Input:  nessuno
  Output: DynamicModule con le 5 funzionalita' obbligatorie:
          1. Genera Esercizio (Seed + frasi infinite)
          2. Verifica Risultato (suggerimento automatico al 1\[Degree] e 2\[Degree] errore)
          3. Suggerimento progressivo (3 livelli manuali)
          4. Mostra Soluzione (chiave + testo chiaro)
          5. Pulisci Campi
  Struttura: l'utente riceve il testo CIFRATO e la CHIAVE,
             deve trovare il testo in CHIARO.
*)
esercizioUniversaleVigenere[] :=
  DynamicModule[
    {seed = 42, messaggioCifrato = "", chiaveSegreto = "",
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[{
        Style["Esercizi \[LongDash] Cifrario di Vigenere", 18, Bold, RGBColor[0.5, 0.2, 0.7]],
        Style["Ti vengono dati il testo cifrato e la chiave: trova il testo in chiaro!",
              12, Italic, Gray],
        Spacer[8],
        Row[{
          Style["Seed: ", 13, Bold],
          InputField[Dynamic[seed], Number, FieldSize -> {8,1}, FieldHint -> "es. 42"],
          Spacer[8],
          Button[
            Style["Genera Esercizio", 13, Bold, White],
            Module[{ris},
              ris               = generaEsercizioConSeedVigenere[seed];
              messaggioCifrato  = ris[[1]];  (* testo cifrato da mostrare *)
              chiaveSegreto     = ris[[2]];  (* chiave da mostrare *)
              messaggioChiaro   = ris[[3]];  (* testo chiaro = soluzione *)
              rispostaUtente    = "";
              tentativi         = 0;
              feedbackMsg       = "";
              soluzioneVisibile = False;
              suggerimentoStep  = 0;
              esercizioGenerato = True;
            ],
            Background -> RGBColor[0.4, 0.1, 0.7], ImageSize -> {160, 35}
          ]
        }],
        Spacer[8],
        (* Consegna: cifrato + chiave *)
        Dynamic[If[esercizioGenerato,
          Column[{
            Framed[Column[{
              Style["Testo cifrato:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
              Style[messaggioCifrato, 15, Bold, Black]}],
              Background -> RGBColor[1.0, 0.95, 0.88],
              RoundingRadius -> 5, FrameStyle -> RGBColor[0.7, 0.3, 0.1], FrameMargins -> 8],
            Spacer[4],
            Framed[Row[{
              Style["Chiave: ", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
              Style[chiaveSegreto, 14, Bold, RGBColor[0.5, 0.2, 0.7]]}],
              Background -> RGBColor[0.96, 0.92, 1.0],
              RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8]
          }],
          Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]]],
        Spacer[6],
        Dynamic[If[esercizioGenerato,
          Column[{
            Style["Inserisci il testo decifrato (solo lettere, senza spazi):", 12, Bold],
            InputField[Dynamic[rispostaUtente], String,
              FieldSize -> {30, 2},
              FieldHint -> "Applica la chiave al contrario..."]}],
          ""]],
        Spacer[6],
        Row[{
          Button[
            Style["Verifica Risultato", 13, Bold, White],
            If[esercizioGenerato,
              tentativi++;
              If[ToUpperCase[StringReplace[StringTrim[rispostaUtente], " " -> ""]] ===
                 StringReplace[messaggioChiaro, " " -> ""],
                feedbackMsg = "\[Checkmark] Corretto! Hai impiegato " <>
                  ToString[tentativi] <>
                  If[tentativi == 1, " tentativo.", " tentativi."],
                If[tentativi >= 3,
                  feedbackMsg       = "\[Cross] Risposta errata. Tentativi esauriti.";
                  soluzioneVisibile = True,
                  suggerimentoStep  = tentativi;
                  feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                    ToString[tentativi] <> "/3.  " <>
                    Which[
                      tentativi == 1,
                        "Suggerimento: per decifrare, sottrai lo shift invece di sommarlo. " <>
                        "La prima lettera della chiave e' '" <>
                        StringTake[chiaveSegreto, 1] <> "'.",
                      tentativi == 2,
                        "Suggerimento: la chiave ha " <>
                        ToString[StringLength[chiaveSegreto]] <>
                        " lettere. Applicala al contrario, lettera per lettera.",
                      True, ""
                    ]
                ]
              ]
            ],
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {170, 35}
          ],
          Spacer[8],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            seed = 42; messaggioCifrato = ""; chiaveSegreto = "";
            messaggioChiaro = ""; rispostaUtente = "";
            tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
            suggerimentoStep = 0; esercizioGenerato = False;,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {140, 35}
          ]
        }],
        Spacer[4],
        Row[{
          Button[
            Style["Suggerimento", 13, Bold, White],
            If[esercizioGenerato && suggerimentoStep < 3, suggerimentoStep++],
            Background -> RGBColor[0.8, 0.6, 0.1], ImageSize -> {140, 35}
          ],
          Spacer[8],
          Button[
            Style["Mostra Soluzione", 13, Bold, White],
            If[esercizioGenerato, soluzioneVisibile = True],
            Background -> RGBColor[0.7, 0.2, 0.2], ImageSize -> {160, 35}
          ]
        }],
        Spacer[8],
        Dynamic[If[feedbackMsg =!= "",
          Framed[
            Style[feedbackMsg, 12,
              If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                 RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]],
            Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.92, 0.92]],
            RoundingRadius -> 5, FrameMargins -> 8],
          ""]],
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento 1: per decifrare Vigenere, per ogni lettera \
del testo cifrato si SOTTRAE (invece di sommare) lo shift della lettera chiave \
corrispondente. La prima lettera della chiave e' '" <>
              StringTake[chiaveSegreto, 1] <> "' (shift " <>
              ToString[Position[alfabeto, ToUpperCase[StringTake[chiaveSegreto,1]]][[1,1]] - 1] <>
              ").",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8],
          suggerimentoStep == 2,
            Framed[Style[
              "\[LightBulb] Suggerimento 2: la chiave '" <> chiaveSegreto <>
              "' ha " <> ToString[StringLength[chiaveSegreto]] <>
              " lettere. Decifra le prime " <>
              ToString[StringLength[chiaveSegreto]] <>
              " lettere del testo, poi riparti dall'inizio della chiave.",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8],
          suggerimentoStep >= 3,
            Framed[Style[
              "\[LightBulb] Suggerimento 3: le prime 3 lettere del testo in chiaro \
corretto sono '" <>
              StringTake[messaggioChiaro, Min[3, StringLength[messaggioChiaro]]] <> "'.",
              12, Italic, RGBColor[0.5, 0.4, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.85],
              RoundingRadius -> 5, FrameMargins -> 8]
        ]],
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Chiave: ", 12, Bold], Style[chiaveSegreto, 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[1.0, 0.93, 0.93],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.6, 0.1, 0.1], FrameMargins -> 10],
          ""]]
      },
      Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560
    ]
  ]

End[ ]

EndPackage[ ]
