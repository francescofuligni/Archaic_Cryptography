(* ::Package:: *)

(* ::Package:: *)

(* ============================================================
   CrittografiaArcaica.m
   Laboratorio Interattivo di Crittografia Arcaica
   Cifrario di Cesare e Cifrario di Vigenere

   Matematica Computazionale 2025/2026
   ============================================================ *)

BeginPackage["CrittografiaArcaica`"]

(* ============================================================
   DICHIARAZIONI DI USO (usage)
   ============================================================ *)

avviaLaboratorio::usage =
  "avviaLaboratorio[] avvia l'interfaccia principale con TabView."

bottoneLaboratorioCesare::usage =
  "bottoneLaboratorioCesare[] restituisce un bottone che apre il Laboratorio Libero del Cifrario di Cesare. Da usare nella sezione II.2 del Tutorial.nb."

bottoneEserciziCesare::usage =
  "bottoneEserciziCesare[] restituisce un bottone che apre gli Esercizi del Cifrario di Cesare. Da usare nella sezione II.3 del Tutorial.nb."

bottoneLaboratorioVigenere::usage =
  "bottoneLaboratorioVigenere[] restituisce un bottone che apre il Laboratorio Libero del Cifrario di Vigenere. Da usare nella sezione III.2 del Tutorial.nb."

bottoneEserciziVigenere::usage =
  "bottoneEserciziVigenere[] restituisce un bottone che apre gli Esercizi del Cifrario di Vigenere. Da usare nella sezione III.3 del Tutorial.nb."

laboratorioCesare::usage =
  "laboratorioCesare[] apre il Laboratorio Libero del Cifrario di Cesare."

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre gli Esercizi del Cifrario di Cesare."

laboratorioVigenere::usage =
  "laboratorioVigenere[] apre il Laboratorio Libero del Cifrario di Vigenere."

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre gli Esercizi del Cifrario di Vigenere."

Begin["`Private`"]

(* ============================================================
   COSTANTI
   ============================================================ *)

alfabeto = CharacterRange["A", "Z"];

(* Frequenze percentuali nell'italiano scritto.
   Fonte: De Mauro, Dizionario di frequenza dell'italiano.
   Ordine: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *)
freqItaliano = {11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54,
                11.28, 0.00, 0.00, 6.51, 2.51, 6.88, 9.83, 3.05,
                0.51, 6.37, 4.98, 5.62, 3.01, 2.10, 0.00, 0.00,
                0.00, 0.49};

(* ============================================================
   FUNZIONI HELPER PRIVATE -- RIUSO DEL CODICE
   Queste funzioni incapsulano operazioni ripetute piu' volte
   nel pacchetto, evitando duplicazione del codice.
   ============================================================ *)

(*
  lettereIn[s]
  Input:  s -- stringa qualsiasi
  Output: lista dei caratteri maiuscoli A-Z contenuti in s,
          nell'ordine in cui compaiono.
  Uso:    estrarre solo le lettere da testo o chiave prima
          di cifrare/decifrare. Usata in tutte le funzioni
          crittografiche al posto di:
          Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &]
*)
lettereIn[s_String] :=
  Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &]

(*
  indiceLettera[c]
  Input:  c -- carattere maiuscolo A-Z
  Output: indice 0-indicizzato nell'alfabeto (A=0, B=1, ..., Z=25).
  Uso:    calcolare lo shift nelle funzioni di cifratura/decifratura.
          Usata al posto di: Position[alfabeto, c][[1,1]] - 1
*)
indiceLettera[c_String] :=
  Position[alfabeto, c][[1, 1]] - 1

(* ============================================================
   UTILITA' -- VALIDAZIONE INPUT
   ============================================================ *)

(*
  soloLettere[s]
  Input:  s -- stringa qualsiasi
  Output: True se s non e' vuota e contiene SOLO lettere A-Z.
  Uso:    validare testo e chiave prima di cifrare/decifrare.
*)
soloLettere[s_String] :=
  Module[{chars},
    chars = lettereIn[s];
    StringLength[s] > 0 && Length[chars] == StringLength[s]
  ]

(* ============================================================
   GENERATORE DI PAROLE CON SEED (AGGIORNATO)
   ============================================================ *)

(* 
1. Creiamo il dizionario usando la Memoization ( := definisce e salva ).
In questo modo Mathematica carichera' e filtrera' la lista di decine 
di migliaia di parole *solo la prima volta* che viene chiamata, 
evitando fastidiosi lag dell'interfaccia a ogni generazione di esercizio.
*)
dizionarioItaliano := dizionarioItaliano = Module[{tutteLeParole},
  (* Estrae tutte le parole italiane disponibili nel dizionario di sistema *)
  tutteLeParole = DictionaryLookup[{"Italian", "*"}];
  
  (* Filtra le parole: 
     - Solo caratteri A-Z (esclude lettere accentate come 'a','e', spazi o apostrofi)
       poiche' la logica crittografica si basa su CharacterRange["A", "Z"].
     - Lunghezza minima di 4 caratteri per avere parole con un senso compiuto. *)
  tutteLeParole = Select[tutteLeParole, StringMatchQ[#, RegularExpression["[a-zA-Z]{4,}"]] &];
  
  (* Converte tutto in maiuscolo, pronto per la cifratura *)
  ToUpperCase[tutteLeParole]
];

(*
  generaParola[seed]
  Input:  seed -- intero, garantisce riproducibilita'
  Output: stringa maiuscola di sole lettere standard
  Logica: Estrae casualmente da un dizionario reale di oltre 60.000 parole.
*)
generaParola[seed_Integer] := Module[{},
  SeedRandom[seed];
  RandomChoice[dizionarioItaliano]
]

(* ============================================================
   CIFRATURA E DECIFRATURA -- CESARE
   ============================================================ *)

(*
  cifraCesare[testo, shift]
  Input:  testo -- stringa di sole lettere
          shift -- intero 0-25
  Output: stringa cifrata (ogni lettera spostata di shift posizioni
          in avanti nell'alfabeto, modulo 26).
*)
cifraCesare[testo_String, shift_Integer] :=
  Module[{caratteri, cifrati},
    caratteri = Characters[ToUpperCase[testo]];
    cifrati = Map[
      Function[c,
        If[MemberQ[alfabeto, c],
          alfabeto[[Mod[indiceLettera[c] + shift, 26] + 1]],
          c]],
      caratteri];
    StringJoin[cifrati]
  ]

(*
  decifraCesare[testo, shift]
  Input:  testo -- stringa cifrata con Cesare
          shift -- lo shift usato per cifrare (0-25)
  Output: testo in chiaro (shift negativo modulo 26).
*)
decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]]

(*
  frequenzeLettere[testo]
  Input:  testo -- stringa qualsiasi
  Output: lista di 26 interi (conteggio A, B, ..., Z).
*)
frequenzeLettere[testo_String] :=
  Module[{solo},
    solo = lettereIn[testo];
    Map[Function[l, Count[solo, l]], alfabeto]
  ]

(* ============================================================
   CIFRATURA E DECIFRATURA -- VIGENERE
   ============================================================ *)

(*
  cifraVigenere[testo, chiave]
  Input:  testo  -- stringa di sole lettere
          chiave -- stringa di sole lettere (lunghezza >= 1)
  Output: stringa cifrata. La chiave si ripete ciclicamente.
          Per ogni lettera del testo lo shift e' la posizione
          0-indicizzata della lettera chiave (A=0, B=1, ..., Z=25).
          L'indice della chiave avanza solo sulle lettere del testo.
*)
cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        sh = indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]];
        AppendTo[risultato,
          alfabeto[[Mod[indiceLettera[c] + sh, 26] + 1]]];
        kIndex++,
        AppendTo[risultato, c]],
      {i, 1, Length[caratteri]}];
    StringJoin[risultato]
  ]

(*
  decifraVigenere[testo, chiave]
  Input:  testo  -- stringa cifrata con Vigenere
          chiave -- stringa chiave (sole lettere, stessa usata per cifrare)
  Output: testo in chiaro (shift negativo modulo 26 per ogni lettera).
*)
decifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        sh = indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]];
        AppendTo[risultato,
          alfabeto[[Mod[indiceLettera[c] - sh, 26] + 1]]];
        kIndex++,
        AppendTo[risultato, c]],
      {i, 1, Length[caratteri]}];
    StringJoin[risultato]
  ]

(*
  tabellaShiftVigenere[testo, chiave, cifra]
  Input:  testo  -- stringa di sole lettere
          chiave -- stringa chiave (sole lettere)
          cifra  -- True se si sta cifrando, False se si sta decifrando
  Output: lista di quadruple {lettera_input, lettera_chiave, shift, lettera_output}
          limitata alle prime 24 lettere per leggibilita'.
  Nota:   se cifra=True, la lettera_output e' quella cifrata (+shift).
          se cifra=False, la lettera_output e' quella decifrata (-shift).
*)
tabellaShiftVigenere[testo_String, chiave_String, cifra_] :=
  Module[
    {testUp, chiaveChars, chiaveLen, soleLettere, risultato, kIndex, sh, lOut, segno},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {}, Return[{}]];
    chiaveLen   = Length[chiaveChars];
    soleLettere = lettereIn[testo];
    risultato   = {};
    kIndex      = 0;
    (* segno: +1 per cifrare, -1 per decifrare *)
    segno = If[cifra, 1, -1];
    Do[
      sh   = indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]];
      lOut = alfabeto[[Mod[indiceLettera[soleLettere[[i]]] + segno * sh, 26] + 1]];
      AppendTo[risultato,
        {soleLettere[[i]], chiaveChars[[Mod[kIndex, chiaveLen] + 1]], sh, lOut}];
      kIndex++,
      {i, 1, Min[Length[soleLettere], 24]}];
    risultato
  ]

(* ============================================================
   GENERATORI DI ESERCIZI CON SEED
   ============================================================ *)

(*
  generaEsercizioConSeedCesare[seed]
  Input:  seed -- intero
  Output: {testo_cifrato, shift_segreto, testo_chiaro}
  Struttura: l'utente riceve il testo CIFRATO e deve trovare il CHIARO.
*)
generaEsercizioConSeedCesare[seed_Integer] :=
  Module[{parola, shift, cifrato},
    parola  = generaParola[seed];
    SeedRandom[seed + 999];
    shift   = RandomInteger[{1, 25}];
    cifrato = cifraCesare[parola, shift];
    {cifrato, shift, parola}
  ]

(*
  generaEsercizioConSeedVigenere[seed]
  Input:  seed -- intero
  Output: {testo_cifrato, chiave_segreta, testo_chiaro}
  Struttura: l'utente riceve il testo CIFRATO e la CHIAVE, trova il CHIARO.
*)
generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {chiavi, parola, chiave, cifrato},
    chiavi = {"SOLE", "MARE", "LUNA", "VENTO", "FUOCO", "ACQUA",
              "CIELO", "TERRA", "LUCE", "OMBRA", "CHIAVE", "CODICE",
              "PIETRA", "FIUME", "STELLA", "NOTTE", "GIORNO"};
    parola = generaParola[seed];
    SeedRandom[seed + 777];
    chiave  = RandomChoice[chiavi];
    cifrato = cifraVigenere[parola, chiave];
    {cifrato, chiave, parola}
  ]

(* ============================================================
   GRAFICO FREQUENZE -- DUE PANNELLI SEPARATI
   ============================================================ *)

(*
  graficaFrequenze[testo]
  Input:  testo -- stringa da analizzare (tipicamente il testo cifrato)
  Output: Column con due BarChart separati:
          1. Standard Italiano (%): barre azzurre con etichette A-Z
          2. Frequenze Testo Cifrato (Assolute): barre colorate con etichette A-Z
  Nota: ChartLabels -> CharacterRange["A","Z"] e' il metodo che funziona
        in Mathematica per mostrare le etichette sotto ogni barra.
*)
graficaFrequenze[testo_String] :=
  Module[
    {conteggi, totale, coloriArcobaleno, graficoItaliano, graficoCifrato},
    conteggi = frequenzeLettere[testo];
    totale   = Total[conteggi];
    If[totale == 0,
      Return[Style[
        "(Nessuna lettera nel testo: impossibile calcolare le frequenze.)",
        11, Italic, Gray]]];
    coloriArcobaleno = Table[Hue[k/26, 0.6, 0.85], {k, 0, 25}];
    graficoItaliano = BarChart[
      freqItaliano,
      ChartLabels -> CharacterRange["A", "Z"],
      ChartStyle  -> RGBColor[0.65, 0.80, 0.92],
      PlotRange   -> {0, Max[freqItaliano] * 1.20},
      ImageSize   -> {500, 280},
      PlotLabel   -> Style["Standard Italiano (%)", 12, Bold, GrayLevel[0.3]],
      BarSpacing  -> 0.3,
      Frame       -> False,
      ImagePadding -> {{30, 10}, {35, 20}}
    ];
    graficoCifrato = BarChart[
      conteggi,
      ChartLabels -> CharacterRange["A", "Z"],
      ChartStyle  -> coloriArcobaleno,
      PlotRange   -> {0, Max[conteggi, 1] * 1.20},
      ImageSize   -> {500, 280},
      PlotLabel   -> Style["Frequenze Testo Cifrato (Assolute)", 12, Bold, GrayLevel[0.3]],
      BarSpacing  -> 0.3,
      Frame       -> False,
      ImagePadding -> {{30, 10}, {35, 20}}
    ];
    Column[{graficoItaliano, graficoCifrato}, Spacings -> 1, Alignment -> Center]
  ]

(* ============================================================
   RUOTA DI CESARE
   ============================================================ *)

(*
  ruotaCesare[shift, highlightK]
  Input:  shift      -- intero 0-25
          highlightK -- indice 0-based del settore evidenziato (-1 = nessuno)
  Output: Graphics con anello esterno (testo chiaro) e disco interno (testo cifrato).
*)
ruotaCesare[shift_Integer, highlightK_Integer] :=
  Module[
    {n, rEst, rInt, rMid, angC, cEst, cInt,
     settoriEst, lettEst, settoriInt, lettInt,
     angFreccia, puntaFreccia, codaFreccia},
    n    = 26;
    rEst = 1.0;
    rInt = 0.62;
    rMid = 0.31;
    angC[k_] := Pi/2 - 2 Pi k / n;
    (* Colori originali con luminosita' ridotta per garantire contrasto *)
    cEst = Table[Hue[k/n, 0.55, 0.75], {k, 0, n-1}];
    cInt = Table[Hue[k/n, 0.85, 0.55], {k, 0, n-1}];
    settoriEst = Table[
      {cEst[[k+1]],
       If[k == highlightK, Opacity[1.0], Opacity[0.75]],
       Annulus[{0,0}, {rInt, rEst}, {angC[k] - Pi/n, angC[k] + Pi/n}]},
      {k, 0, n-1}];
    lettEst = Table[
      Text[
        Style[alfabeto[[k+1]],
          If[k == highlightK, 15, 10], Bold, White],
        {(rInt + (rEst - rInt)/2) * Cos[angC[k]],
         (rInt + (rEst - rInt)/2) * Sin[angC[k]]}],
      {k, 0, n-1}];
    settoriInt = Table[
      {cInt[[Mod[k + shift, n] + 1]], Opacity[0.90],
       Disk[{0,0}, rInt, {angC[k] - Pi/n, angC[k] + Pi/n}]},
      {k, 0, n-1}];
    lettInt = Table[
      Text[
        Style[alfabeto[[Mod[k + shift, n] + 1]], 11, Bold, White],
        {(rMid + (rInt - rMid)/2) * Cos[angC[k]],
         (rMid + (rInt - rMid)/2) * Sin[angC[k]]}],
      {k, 0, n-1}];
    (* La freccia punta al settore evidenziato dallo slider.
       Se nessun settore e' selezionato (highlightK = 0 di default),
       punta in cima alla lettera A. *)
    angFreccia  = angC[highlightK];
    codaFreccia = 1.30 * {Cos[angFreccia], Sin[angFreccia]};
    puntaFreccia = 1.03 * {Cos[angFreccia], Sin[angFreccia]};
    Graphics[
      Join[
        settoriEst, lettEst, settoriInt, lettInt,
        {Text[Style["Cifrato", 10, Italic, White], {0, 0.0}]},
        (* Freccia mobile che segue la lettera selezionata *)
        {Thickness[0.008], RGBColor[0.7, 0.2, 0.2],
         Arrow[{codaFreccia, puntaFreccia}]},
        {Thickness[0.005], GrayLevel[0.5], Circle[{0,0}, rInt]}],
      ImageSize  -> 320,
      Background -> GrayLevel[0.97],
      PlotRange  -> {{-1.38, 1.38}, {-1.38, 1.38}}]
  ]

(*
  ruotaInterattiva[shiftDyn]
  Input:  shiftDyn -- variabile Dynamic con lo shift corrente
  Output: DynamicModule con Slider di selezione lettera e ruota grafica.
          Lo Slider seleziona il settore dell'anello esterno da evidenziare
          e mostra in tempo reale la coppia lettera_chiara -> lettera_cifrata.
*)
ruotaInterattiva[shiftDyn_] :=
  DynamicModule[
    {settoreCorrente = 0},
    Column[{
      Row[{
        Style["Punta la lettera: ", 11, Italic, Gray],
        Slider[Dynamic[settoreCorrente], {0, 25, 1}, ImageSize -> 220],
        Spacer[6],
        Dynamic[Style[
          alfabeto[[settoreCorrente + 1]] <> " \[RightArrow] " <>
          alfabeto[[Mod[settoreCorrente + shiftDyn, 26] + 1]],
          14, Bold, RGBColor[0.2, 0.4, 0.7]]]
      }],
      Dynamic[ruotaCesare[shiftDyn, settoreCorrente]]
    }, Alignment -> Center]
  ]

(* ============================================================
   LABORATORIO LIBERO -- CESARE
   ============================================================ *)

(*
  laboratorioCesare[]
  Input:  nessuno
  Output: DynamicModule con:
          - campo testo con validazione (solo lettere, senza spazi)
          - slider shift 0-25
          - bottoni Cifra / Decifra / Pulisci Campi
          - ruota interattiva
          - grafico frequenze a due pannelli
*)
laboratorioCesare[] :=
  DynamicModule[
    {testoInput = "", shiftLab = 3,
     risultatoCifra = "", risultatoDecifra = "", avvisoTesto = ""},
    Panel[
      Column[{
        Style["Laboratorio Libero \[LongDash] Cifrario di Cesare",
              18, Bold, RGBColor[0.2, 0.4, 0.7]],
        Style["Inserisci un testo (solo lettere, senza spazi), scegli lo shift e cifra o decifra.",
              12, Italic, Gray],
        Spacer[8],
        Row[{
          Style["Testo: ", 13, Bold],
          InputField[Dynamic[testoInput], String,
            FieldSize -> {30, 2}, FieldHint -> "Solo lettere (es. CIAO)"],
          Spacer[8],
          Dynamic[If[testoInput =!= "" && !soloLettere[testoInput],
            Style["! Solo lettere, senza spazi", 11, Bold, RGBColor[0.8, 0.2, 0.0]], ""]]
        }],
        Spacer[4],
        Row[{
          Style["Shift (k): ", 13, Bold],
          Slider[Dynamic[shiftLab], {0, 25, 1}, ImageSize -> 200],
          Spacer[8],
          Dynamic[Style[ToString[shiftLab], 15, Bold, RGBColor[0.7, 0.2, 0.2]]]
        }],
        Spacer[6],
        Row[{
          Button[
            Style["Cifra \[RightArrow]", 13, Bold, White],
            If[testoInput === "" || !soloLettere[testoInput],
              avvisoTesto = "Inserisci un testo valido (solo lettere, senza spazi).",
              avvisoTesto = ""; risultatoCifra = cifraCesare[testoInput, shiftLab];
              risultatoDecifra = ""];,
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {120, 35}],
          Spacer[10],
          Button[
            Style["\[LeftArrow] Decifra", 13, Bold, White],
            If[testoInput === "" || !soloLettere[testoInput],
              avvisoTesto = "Inserisci un testo valido (solo lettere, senza spazi).",
              avvisoTesto = ""; risultatoDecifra = decifraCesare[testoInput, shiftLab];
              risultatoCifra = ""];,
            Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}],
          Spacer[10],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            testoInput = ""; shiftLab = 3;
            risultatoCifra = ""; risultatoDecifra = ""; avvisoTesto = "";,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}]
        }],
        Spacer[4],
        Dynamic[If[avvisoTesto =!= "",
          Style[avvisoTesto, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Spacer[4],
        Dynamic[If[risultatoCifra =!= "",
          Framed[Column[{Style["Testo cifrato:", 12, Bold, RGBColor[0.2, 0.6, 0.3]],
            Style[risultatoCifra, 14, Bold]}],
            Background -> RGBColor[0.92, 1.0, 0.93],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.2, 0.6, 0.3], FrameMargins -> 8], ""]],
        Dynamic[If[risultatoDecifra =!= "",
          Framed[Column[{Style["Testo decifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
            Style[risultatoDecifra, 14, Bold]}],
            Background -> RGBColor[0.97, 0.93, 1.0],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8], ""]],
        Spacer[10],
        Style["Ruota di Cesare \[LongDash] usa lo slider per puntare una lettera:", 13, Bold],
        Style["La lettera a destra della freccia mostra la corrispondente lettera cifrata.",
              11, Italic, Gray],
        Dynamic[ruotaInterattiva[shiftLab]],
        Spacer[10],
        Style["Analisi delle frequenze:", 13, Bold],
        Style["Sopra: profilo standard dell'italiano. Sotto: frequenze del testo cifrato.",
              11, Italic, Gray],
        Dynamic[If[risultatoCifra =!= "",
          graficaFrequenze[risultatoCifra],
          Style["(Il grafico appare dopo aver cifrato un testo)", 11, Italic, Gray]]]
      }, Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560]
  ]

(* ============================================================
   ESERCIZI -- CESARE
   Struttura: l'utente riceve il testo CIFRATO, trova il CHIARO.
   Suggerimento automatico al 1\[Degree] e 2\[Degree] errore (nessun bottone).
   ============================================================ *)

(*
  esercizioUniversaleCesare[]
  Input:  nessuno
  Output: DynamicModule con le funzionalita' obbligatorie:
          - Genera Esercizio (Seed)
          - Verifica Risultato (suggerimento automatico all'errore)
          - Mostra Soluzione
          - Pulisci Campi
*)
esercizioUniversaleCesare[] :=
  DynamicModule[
    {seed = 42, messaggioCifrato = "", shiftSegreto = 0,
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False, shiftEsplorazione = 0},
    Panel[
      Column[{
        Style["Esercizi \[LongDash] Cifrario di Cesare",
              18, Bold, RGBColor[0.2, 0.4, 0.7]],
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
              ris = generaEsercizioConSeedCesare[seed];
              messaggioCifrato = ris[[1]]; shiftSegreto = ris[[2]];
              messaggioChiaro = ris[[3]]; rispostaUtente = "";
              tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
              suggerimentoStep = 0; esercizioGenerato = True];,
            Background -> RGBColor[0.15, 0.5, 0.8], ImageSize -> {160, 35}]
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
              FieldSize -> {30, 2}, FieldHint -> "Scrivi qui la tua risposta..."]}],
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
                  feedbackMsg = "\[Cross] Risposta errata. Tentativi esauriti.";
                  soluzioneVisibile = True,
                  suggerimentoStep = tentativi;
                  feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                    ToString[tentativi] <> "/3."]]];,
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {170, 35}],
          Spacer[8],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            seed = 42; messaggioCifrato = ""; shiftSegreto = 0;
            messaggioChiaro = ""; rispostaUtente = "";
            tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
            suggerimentoStep = 0; esercizioGenerato = False; shiftEsplorazione = 0;,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {140, 35}],
          Spacer[8],
          Button[
            Style["Mostra Soluzione", 13, Bold, White],
            If[esercizioGenerato, soluzioneVisibile = True];,
            Background -> RGBColor[0.7, 0.2, 0.2], ImageSize -> {160, 35}]
        }],
        Spacer[8],
        Dynamic[If[feedbackMsg =!= "",
          Framed[Style[feedbackMsg, 12,
            If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
               RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]],
            Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.92, 1.0, 0.93], RGBColor[1.0, 0.93, 0.93]],
            FrameStyle -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.2, 0.6, 0.3], RGBColor[0.7, 0.2, 0.2]],
            RoundingRadius -> 5, FrameMargins -> 10], ""]],
        (* Suggerimento automatico: appare progressivamente ad ogni errore *)
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento: la lettera piu' frequente in italiano e' la E. " <>
              "La lettera piu' comune nel testo cifrato probabilmente corrisponde alla E.",
              12, Italic, RGBColor[0.45, 0.35, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.87],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep >= 2,
            Framed[Style[
              "\[LightBulb] Suggerimento: lo shift e' nell'intervallo [" <>
              ToString[Max[1, shiftSegreto - 4]] <> ", " <>
              ToString[Min[25, shiftSegreto + 4]] <> "].",
              12, Italic, RGBColor[0.45, 0.35, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.87],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10]]],
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Shift: ", 12, Bold], Style[ToString[shiftSegreto], 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[1.0, 0.93, 0.93],
            FrameStyle -> RGBColor[0.6, 0.1, 0.1],
            RoundingRadius -> 5, FrameMargins -> 10],
          ""]],
        (* Ruota di Cesare interattiva per esplorare le rotazioni *)
        Spacer[10],
        Dynamic[If[esercizioGenerato,
          Column[{
            Style["Ruota di Cesare \[LongDash] esplora le rotazioni per trovare lo shift giusto:", 13, Bold],
            Style["Usa lo slider per provare diversi shift e puntare le lettere.",
                  11, Italic, Gray],
            Row[{
              Style["Shift di prova: ", 12, Bold],
              Slider[Dynamic[shiftEsplorazione], {0, 25, 1}, ImageSize -> 200],
              Spacer[8],
              Dynamic[Style[ToString[shiftEsplorazione], 15, Bold, RGBColor[0.7, 0.2, 0.2]]]
            }],
            Dynamic[ruotaInterattiva[shiftEsplorazione]]
          }, Alignment -> Center],
          ""]],
        (* Grafico frequenze standard italiano come riferimento *)
        Spacer[10],
        Dynamic[If[esercizioGenerato,
          Column[{
            Style["Frequenze standard dell'alfabeto italiano (%)", 13, Bold],
            Style["Usa questo grafico come riferimento per l'analisi delle frequenze.",
                  11, Italic, Gray],
            BarChart[
              freqItaliano,
              ChartLabels  -> CharacterRange["A", "Z"],
              ChartStyle   -> RGBColor[0.65, 0.80, 0.92],
              PlotRange    -> {0, Max[freqItaliano] * 1.20},
              ImageSize    -> {500, 250},
              BarSpacing   -> 0.3,
              Frame        -> False,
              ImagePadding -> {{30, 10}, {35, 20}}]
          }, Alignment -> Center],
          ""]]
      }, Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560]
  ]

(* ============================================================
   LABORATORIO LIBERO -- VIGENERE
   ============================================================ *)

(*
  laboratorioVigenere[]
  Input:  nessuno
  Output: DynamicModule con:
          - testo e chiave con validazione (solo lettere, senza spazi)
          - bottoni Cifra / Decifra / Pulisci Campi
          - tabella shift a 4 colonne
          - nota didattica: Cesare e' sottocaso di Vigenere
*)
laboratorioVigenere[] :=
  DynamicModule[
    {testoInput = "", chiaveInput = "", risultatoCifra = "",
     risultatoDecifra = "", tabellaVis = {},
     avvisoTesto = "", avvisoChiave = ""},
    Panel[
      Column[{
        Style["Laboratorio Libero \[LongDash] Cifrario di Vigenere",
              18, Bold, RGBColor[0.5, 0.2, 0.7]],
        Style["Inserisci un testo e una parola chiave (solo lettere, senza spazi).",
              12, Italic, Gray],
        Spacer[6],
        Row[{
          Style["Testo:  ", 13, Bold],
          InputField[Dynamic[testoInput], String,
            FieldSize -> {25, 2}, FieldHint -> "Solo lettere (es. CIAO)"],
          Spacer[8],
          Dynamic[If[testoInput =!= "" && !soloLettere[testoInput],
            Style["! Solo lettere", 11, Bold, RGBColor[0.8, 0.2, 0.0]], ""]]
        }],
        Spacer[4],
        Row[{
          Style["Chiave: ", 13, Bold],
          InputField[Dynamic[chiaveInput], String,
            FieldSize -> {15, 1}, FieldHint -> "Solo lettere (es. SOLE)"],
          Spacer[8],
          Dynamic[If[chiaveInput =!= "" && !soloLettere[chiaveInput],
            Style["! Solo lettere", 11, Bold, RGBColor[0.8, 0.2, 0.0]], ""]]
        }],
        Spacer[6],
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
                risultatoCifra = cifraVigenere[testoInput, chiaveInput];
                risultatoDecifra = "";
                tabellaVis = tabellaShiftVigenere[testoInput, chiaveInput, True]];,
            Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}],
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
                risultatoCifra = "";
                tabellaVis = tabellaShiftVigenere[testoInput, chiaveInput, False]];,
            Background -> RGBColor[0.2, 0.5, 0.7], ImageSize -> {120, 35}],
          Spacer[10],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            testoInput = ""; chiaveInput = "";
            risultatoCifra = ""; risultatoDecifra = "";
            tabellaVis = {}; avvisoTesto = ""; avvisoChiave = "";,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}]
        }],
        Spacer[4],
        Dynamic[If[avvisoTesto =!= "",
          Style[avvisoTesto, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Dynamic[If[avvisoChiave =!= "",
          Style[avvisoChiave, 12, Bold, RGBColor[0.75, 0.1, 0.0]], ""]],
        Spacer[4],
        Dynamic[If[risultatoCifra =!= "",
          Framed[Column[{Style["Testo cifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
            Style[risultatoCifra, 14, Bold]}],
            Background -> RGBColor[0.96, 0.92, 1.0],
            RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8],
          If[risultatoDecifra =!= "",
            Framed[Column[{Style["Testo decifrato:", 12, Bold, RGBColor[0.2, 0.5, 0.7]],
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
              tabellaVis];
            Grid[Prepend[righe,
              {Style["Lettera chiaro",  11, Bold, Gray],
               Style["Lettera chiave",  11, Bold, Gray],
               Style["Shift",           11, Bold, Gray],
               Style["Lettera cifrata", 11, Bold, Gray]}],
              Frame -> All,
              Background -> {None, {RGBColor[0.9, 0.85, 1.0], {White}}},
              FrameStyle -> LightGray, Spacings -> {1.5, 0.8}]],
          Style["(La tabella appare dopo la cifratura o decifratura)", 11, Italic, Gray]]]
      }, Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560]
  ]

(* ============================================================
   ESERCIZI -- VIGENERE
   Struttura: l'utente riceve CIFRATO + CHIAVE, trova il CHIARO.
   Suggerimento automatico al 1\[Degree] e 2\[Degree] errore (nessun bottone).
   ============================================================ *)

(*
  esercizioUniversaleVigenere[]
  Input:  nessuno
  Output: DynamicModule con le funzionalita' obbligatorie:
          - Genera Esercizio (Seed)
          - Verifica Risultato (suggerimento automatico all'errore)
          - Mostra Soluzione
          - Pulisci Campi
*)
esercizioUniversaleVigenere[] :=
  DynamicModule[
    {seed = 42, messaggioCifrato = "", chiaveSegreto = "",
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[{
        Style["Esercizi \[LongDash] Cifrario di Vigenere",
              18, Bold, RGBColor[0.5, 0.2, 0.7]],
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
              ris = generaEsercizioConSeedVigenere[seed];
              messaggioCifrato = ris[[1]]; chiaveSegreto = ris[[2]];
              messaggioChiaro = ris[[3]]; rispostaUtente = "";
              tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
              suggerimentoStep = 0; esercizioGenerato = True];,
            Background -> RGBColor[0.4, 0.1, 0.7], ImageSize -> {160, 35}]
        }],
        Spacer[8],
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
              RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8]}],
          Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]]],
        Spacer[6],
        Dynamic[If[esercizioGenerato,
          Column[{
            Style["Inserisci il testo decifrato (solo lettere, senza spazi):", 12, Bold],
            InputField[Dynamic[rispostaUtente], String,
              FieldSize -> {30, 2}, FieldHint -> "Applica la chiave al contrario..."]}],
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
                  feedbackMsg = "\[Cross] Risposta errata. Tentativi esauriti.";
                  soluzioneVisibile = True,
                  suggerimentoStep = tentativi;
                  feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                    ToString[tentativi] <> "/3."]]];,
            Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {170, 35}],
          Spacer[8],
          Button[
            Style["Pulisci Campi", 13, Bold, White],
            seed = 42; messaggioCifrato = ""; chiaveSegreto = "";
            messaggioChiaro = ""; rispostaUtente = "";
            tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
            suggerimentoStep = 0; esercizioGenerato = False;,
            Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {140, 35}],
          Spacer[8],
          Button[
            Style["Mostra Soluzione", 13, Bold, White],
            If[esercizioGenerato, soluzioneVisibile = True];,
            Background -> RGBColor[0.7, 0.2, 0.2], ImageSize -> {160, 35}]
        }],
        Spacer[8],
        Dynamic[If[feedbackMsg =!= "",
          Framed[Style[feedbackMsg, 12,
            If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
               RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]],
            Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.92, 1.0, 0.93], RGBColor[1.0, 0.93, 0.93]],
            FrameStyle -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
              RGBColor[0.2, 0.6, 0.3], RGBColor[0.7, 0.2, 0.2]],
            RoundingRadius -> 5, FrameMargins -> 10], ""]],
        (* Suggerimento automatico progressivo *)
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento: per decifrare Vigenere si SOTTRAE lo shift " <>
              "invece di sommarlo. La prima lettera della chiave e' '" <>
              StringTake[chiaveSegreto, 1] <> "' (shift " <>
              ToString[indiceLettera[ToUpperCase[StringTake[chiaveSegreto,1]]]] <> ").",
              12, Italic, RGBColor[0.45, 0.35, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.87],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep >= 2,
            Framed[Style[
              "\[LightBulb] Suggerimento: la chiave '" <> chiaveSegreto <>
              "' ha " <> ToString[StringLength[chiaveSegreto]] <>
              " lettere. Decifra le prime " <> ToString[StringLength[chiaveSegreto]] <>
              " lettere, poi riparti dall'inizio della chiave.",
              12, Italic, RGBColor[0.45, 0.35, 0.0]],
              Background -> RGBColor[1.0, 0.97, 0.87],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10]]],
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Chiave: ", 12, Bold], Style[chiaveSegreto, 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[1.0, 0.93, 0.93],
            FrameStyle -> RGBColor[0.6, 0.1, 0.1],
            RoundingRadius -> 5, FrameMargins -> 10],
          ""]]
      }, Alignment -> Left, Spacings -> 1],
      Background -> GrayLevel[0.97], ImageSize -> 560]
  ]

(* ============================================================
   INTERFACCIA PRINCIPALE -- TabView con tutte le sezioni
   ============================================================ *)

(*
  avviaLaboratorio[]
  Input:  nessuno
  Output: TabView con 4 schede:
          1. Laboratorio Cesare
          2. Esercizi Cesare
          3. Laboratorio Vigenere
          4. Esercizi Vigenere
  Uso nel Tutorial.nb (unica cella di codice visibile):
          avviaLaboratorio[]
*)
avviaLaboratorio[] :=
  TabView[
    {
      "Laboratorio Cesare"   -> laboratorioCesare[],
      "Esercizi Cesare"      -> esercizioUniversaleCesare[],
      "Laboratorio Vigenere" -> laboratorioVigenere[],
      "Esercizi Vigenere"    -> esercizioUniversaleVigenere[]
    },
    ImageSize -> Full
  ]

(* ============================================================
   BOTTONI DI LANCIO -- uno per sezione del Tutorial.nb
   Ogni funzione restituisce un Button che, quando premuto,
   apre l'interfaccia corrispondente in una nuova cella di output.
   Uso nel Tutorial.nb: mettere una cella Input (nascosta) per
   ciascuna sezione con la chiamata al bottone corrispondente.
   ============================================================ *)

(*
  bottoneLaboratorioCesare[]
  Output: Button blu che avvia laboratorioCesare[] al click.
*)
bottoneLaboratorioCesare[] :=
  Button[
    Style["\[RightTriangle]  Apri il Laboratorio Libero \[LongDash] Cifrario di Cesare",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Pane[laboratorioCesare[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}],
        "Output",
        Editable -> False, Deletable -> False, ShowCellBracket -> False]},
      Editable -> False, Deletable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Laboratorio Cesare"
    ],
    Background -> RGBColor[0.2, 0.4, 0.7],
    ImageSize  -> {460, 50}
  ]

(*
  bottoneEserciziCesare[]
  Output: Button verde che avvia esercizioUniversaleCesare[] al click.
*)
bottoneEserciziCesare[] :=
  Button[
    Style["\[RightTriangle]  Apri gli Esercizi \[LongDash] Cifrario di Cesare",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Pane[esercizioUniversaleCesare[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}],
        "Output",
        Editable -> False, Deletable -> False, ShowCellBracket -> False]},
      Editable -> False, Deletable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Esercizi Cesare"
    ],
    Background -> RGBColor[0.15, 0.55, 0.25],
    ImageSize  -> {460, 50}
  ]

(*
  bottoneLaboratorioVigenere[]
  Output: Button viola che avvia laboratorioVigenere[] al click.
*)
bottoneLaboratorioVigenere[] :=
  Button[
    Style["\[RightTriangle]  Apri il Laboratorio Libero \[LongDash] Cifrario di Vigenere",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Pane[laboratorioVigenere[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}],
        "Output",
        Editable -> False, Deletable -> False, ShowCellBracket -> False]},
      Editable -> False, Deletable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Laboratorio Vigenere"
    ],
    Background -> RGBColor[0.45, 0.15, 0.65],
    ImageSize  -> {460, 50}
  ]

(*
  bottoneEserciziVigenere[]
  Output: Button viola scuro che avvia esercizioUniversaleVigenere[] al click.
*)
bottoneEserciziVigenere[] :=
  Button[
    Style["\[RightTriangle]  Apri gli Esercizi \[LongDash] Cifrario di Vigenere",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Pane[esercizioUniversaleVigenere[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}],
        "Output",
        Editable -> False, Deletable -> False, ShowCellBracket -> False]},
      Editable -> False, Deletable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Esercizi Vigenere"
    ],
    Background -> RGBColor[0.55, 0.10, 0.40],
    ImageSize  -> {460, 50}
  ]

(* Precaricamento del dizionario al momento del caricamento del pacchetto.
   In questo modo il primo esercizio generato non subisce lag. *)
dizionarioItaliano;

End[ ]

EndPackage[ ]
