(* ::Package:: *)

(* :Title: CrittografiaArcaica *)
(* :Context: CrittografiaArcaica` *)
(* :Authors: Matteo Boscherini, Alessandro Campedelli, Francesco Maria Fuligni, Mattia Furini, Mohamed Samir Haffoudhi *)
(* :Summary: Laboratorio interattivo di crittografia arcaica. Implementa il Cifrario di Cesare e il Cifrario di Vigenere con interfacce didattiche e esercizi. *)
(* :Copyright: Gruppo "I Cesaroni" *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 14 *)
(* :History: Ultima modifica il 30/04/2026 *)
(* :Keywords: crittografia, Cesare, Vigenere, cifrario *)
(* :Sources: De Mauro - Dizionario di frequenza dell'italiano *)
(* :Limitations: solo lettere A-Z, nessun carattere accentato *)
(* :Discussion: Progetto del corso di Matematica Computazionale *)
(* :Requirements: Mathematica 14, connessione per DictionaryLookup *)

BeginPackage["CrittografiaArcaica`"];

Unprotect[
  avviaLaboratorio,
  bottoneEserciziCesare,
  bottoneEserciziVigenere,
  esercizioUniversaleCesare,
  esercizioUniversaleVigenere
];

avviaLaboratorio::usage =
  "avviaLaboratorio[] avvia l'interfaccia principale con TabView.";

bottoneEserciziCesare::usage =
  "bottoneEserciziCesare[] restituisce un bottone che apre gli Esercizi del Cifrario di Cesare. Da usare nella sezione II.3 del Tutorial.nb.";

bottoneEserciziVigenere::usage =
  "bottoneEserciziVigenere[] restituisce un bottone che apre gli Esercizi del Cifrario di Vigenere. Da usare nella sezione III.3 del Tutorial.nb.";

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre gli Esercizi del Cifrario di Cesare.";

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre gli Esercizi del Cifrario di Vigenere.";

Begin["`Private`"];

alfabeto = CharacterRange["A", "Z"]; (* le 26 lettere maiuscole dell'alfabeto latino *)

(* Frequenze percentuali delle lettere nell'italiano scritto - fonte: De Mauro.
   Ordine: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *)
freqItaliano = {11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54,
                11.28, 0.00, 0.00, 6.51, 2.51, 6.88, 9.83, 3.05,
                0.51, 6.37, 4.98, 5.62, 3.01, 2.10, 0.00, 0.00,
                0.00, 0.49};

lettereIn[s_String] :=
  Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &] (* filtra solo i caratteri alfabetici A-Z *)

indiceLettera[c_String] :=
  Position[alfabeto, c][[1, 1]] - 1 (* restituisce la posizione 0-indicizzata: A=0, B=1, ..., Z=25 *)

soloLettere[s_String] :=  (* True se la stringa contiene solo lettere A-Z e non e' vuota *)
  Module[{chars},
    chars = lettereIn[s];
    StringLength[s] > 0 && Length[chars] == StringLength[s]
  ]

dizionarioItaliano := dizionarioItaliano = Module[{tutteLeParole}, (* := con memoization: viene costruito una sola volta, poi riutilizzato *)
  (* Estrae tutte le parole italiane disponibili nel dizionario di sistema *)
  tutteLeParole = DictionaryLookup[{"Italian", "*"}];
  
  (* Filtra: solo parole con lettere A-Z pure (no accenti), lunghezza minima 4 caratteri *)
    tutteLeParole = Select[tutteLeParole, StringMatchQ[#, RegularExpression["[a-zA-Z]{4,}"]] &];
  
  (* Normalizza in maiuscolo per uniformita' con l'alfabeto di riferimento *)
  ToUpperCase[tutteLeParole]
];

(* Estrae una parola casuale dal dizionario italiano in modo riproducibile dato un seed *)
generaParola[seed_Integer] := Module[{},
  SeedRandom[seed];
  RandomChoice[dizionarioItaliano]
]

(* Funzione ausiliaria: cifra un singolo carattere con il Cifrario di Cesare.
   Se il carattere e' una lettera lo sposta di shift posizioni, altrimenti lo lascia invariato. *)
cifraCarattereCesare[c_String, shift_Integer] :=
  If[MemberQ[alfabeto, c],
    alfabeto[[Mod[indiceLettera[c] + shift, 26] + 1]],
    c]

cifraCesare[testo_String, shift_Integer] :=
  Module[{caratteri},
    caratteri = Characters[ToUpperCase[testo]]; (* converte in maiuscolo e separa in lista di caratteri *)
    StringJoin[Map[cifraCarattereCesare[#, shift] &, caratteri]] (* applica la cifratura a ogni carattere *)
  ]

decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]] (* decifrare equivale a cifrare con lo shift opposto *)

frequenzeLettere[testo_String] :=
  Module[{solo},
    solo = lettereIn[testo];
    Map[Function[l, Count[solo, l]], alfabeto] (* conta le occorrenze di ciascuna lettera A-Z *)
  ]

(* Funzione ausiliaria: applica un singolo passo di cifratura Vigenere a un carattere.
   Restituisce {carattereRisultante, nuovoIndiceChiave}.
   Se c non e' una lettera, lo restituisce invariato senza avanzare l'indice della chiave. *)
applicaPassoCifraVigenere[c_String, kIndex_Integer, chiaveChars_List, chiaveLen_Integer] :=
  If[MemberQ[alfabeto, c],
    {alfabeto[[Mod[indiceLettera[c] + indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]], 26] + 1]], kIndex + 1},
    {c, kIndex}
  ]

(* Cifrario di Vigenere: applica uno shift variabile lettera per lettera,
   ciclando sulla chiave. I caratteri non alfabetici vengono preservati. *)
cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, passo},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0; (* indice nella chiave: avanza solo quando incontriamo una lettera *)
    risultato = Map[
      Function[c,
        passo = applicaPassoCifraVigenere[c, kIndex, chiaveChars, chiaveLen];
        kIndex = passo[[2]];
        passo[[1]]
      ],
      caratteri
    ];
    StringJoin[risultato]
  ]

(* Funzione ausiliaria: applica un singolo passo di decifratura Vigenere a un carattere.
   Identica ad applicaPassoCifraVigenere ma sottrae lo shift invece di sommarlo. *)
applicaPassoDecifraVigenere[c_String, kIndex_Integer, chiaveChars_List, chiaveLen_Integer] :=
  If[MemberQ[alfabeto, c],
    {alfabeto[[Mod[indiceLettera[c] - indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]], 26] + 1]], kIndex + 1},
    {c, kIndex}
  ]

(* Decifratura Vigenere: identica alla cifratura ma sottrae lo shift invece di sommarlo *)
decifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, passo},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    risultato = Map[
      Function[c,
        passo = applicaPassoDecifraVigenere[c, kIndex, chiaveChars, chiaveLen];
        kIndex = passo[[2]];
        passo[[1]]
      ],
      caratteri
    ];
    StringJoin[risultato]
  ]

(* Genera una tabella didattica con il dettaglio passo-passo della cifratura/decifratura Vigenere.
   Ogni riga mostra: {lettera originale, lettera chiave, shift applicato, lettera risultante} *)
tabellaShiftVigenere[testo_String, chiave_String, cifra_] :=
  Module[
    {testUp, chiaveChars, chiaveLen, soleLettere, risultato, kIndex, sh, lOut, segno},
    testUp      = ToUpperCase[testo];
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {}, Return[{}]];
    chiaveLen   = Length[chiaveChars];
    soleLettere = lettereIn[testo];
    (* segno: +1 per cifrare, -1 per decifrare *)
    segno = If[cifra, 1, -1];
    risultato = Table[
      Module[{sh, lOut},
        sh   = indiceLettera[chiaveChars[[Mod[i - 1, chiaveLen] + 1]]];
        lOut = alfabeto[[Mod[indiceLettera[soleLettere[[i]]] + segno * sh, 26] + 1]];
        {soleLettere[[i]], chiaveChars[[Mod[i - 1, chiaveLen] + 1]], sh, lOut}
      ],
      {i, 1, Min[Length[soleLettere], 24]}
    ]; (* limitata a 24 righe per non appesantire la visualizzazione *)
    risultato
  ]

generaEsercizioConSeedCesare[seed_Integer] :=
  Module[{parola, shift, cifrato},
    (* Inizializza il generatore col seed per ottenere un esercizio riproducibile *)
    parola  = generaParola[seed];
        SeedRandom[seed + 999]; (* offset +999 per rendere lo shift indipendente dalla parola generata *)
    shift   = RandomInteger[{1, 25}]; (* shift tra 1 e 25: escludiamo 0 = nessuna cifratura *)
    cifrato = cifraCesare[parola, shift];
    (* Restituisce {cifrato, shift, chiaro}: l'utente vede il cifrato, la soluzione e' il chiaro *)
    {cifrato, shift, parola}
  ]

generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {chiavi, parola, chiave, cifrato},
    (* Pool di chiavi tematiche italiane, scelte per lunghezza e riconoscibilita' *)
        chiavi = {
          "SOLE", "MARE", "LUNA", "LUCE", "ARIA", "LAGO",
          "MANO", "CANE", "ROSA", "PANE", "VINO", "SALE",
          "ANNO", "RIVA", "ERBA", "ORSO", "RANA", "PACE",
          "MELA", "PERA", "NASO", "DITO", "NEVE", "SERA",
          "ALBA", "GELO", "ONDE", "RAMO", "TOPO", "VELA",
          "RETE", "SETA", "MODO", "MOTO", "NOTA", "ONDA",
          "PALO", "POLO", "RAME", "RENA", "RISO", "ROTA",
          "FUOCO", "ACQUA", "TERRA", "VENTO", "PORTA",
          "PONTE", "CAMPO", "BOSCO", "MONTE", "PIANO",
          "FIORE", "FONTE", "LINEA", "LIBRO", "PRATO",
          "TRENO", "VETRO", "VOLTA", "ZUCCA", "BURRO",
          "CORSA", "DONNA", "FIUME", "GEMMA", "ISOLA"
        };
    parola = generaParola[seed];
    (* Usa seed+777 per la chiave, indipendente da seed usato per la parola *)
    SeedRandom[seed + 777]; (* offset +777 per rendere la scelta della chiave indipendente dalla parola *)
    chiave  = RandomChoice[chiavi];
    cifrato = cifraVigenere[parola, chiave];
        {cifrato, chiave, parola}
  ]

(* Confronto visivo tra le frequenze dell'italiano standard e quelle del testo fornito.
   Utile per individuare lo shift nel cifrario di Cesare tramite analisi delle frequenze. *)
graficaFrequenze[testo_String] :=
  Module[
    {conteggi, totale, coloriArcobaleno, graficoItaliano, graficoCifrato},
    (* Conto le occorrenze di ciascuna lettera A-Z nel testo *)
    conteggi = frequenzeLettere[testo];
    totale   = Total[conteggi];
    (* Se il testo non contiene lettere, restituisco un messaggio descrittivo *)
    If[totale == 0,
      Return[Style[
        "(Nessuna lettera nel testo: impossibile calcolare le frequenze.)",
        11, Italic, Gray]]];
    (* Palette di 26 tonalita' distinte per differenziare visivamente le barre *)
    coloriArcobaleno = Table[Hue[k/26, 0.6, 0.85], {k, 0, 25}];
    (* Grafico di riferimento: distribuzione attesa nell'italiano standard *)
    graficoItaliano = BarChart[
      freqItaliano,
      ChartLabels -> CharacterRange["A", "Z"],
      ChartStyle  -> RGBColor[0.65, 0.80, 0.92], (* azzurro uniforme *)
      PlotRange   -> {0, Max[freqItaliano] * 1.20},
      ImageSize   -> {500, 280},
      PlotLabel   -> Style["Standard Italiano (%)", 12, Bold, GrayLevel[0.3]],
      BarSpacing  -> 0.3,
      Frame       -> False,
      ImagePadding -> {{30, 10}, {35, 20}} (* spazio extra sotto per le etichette A-Z *)
    ];
    (* Grafico del testo analizzato: frequenze assolute delle lettere nel testo cifrato *)
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

(* Disegna la ruota di Cesare: due anelli concentrici (chiaro esterno, cifrato interno)
   con il disco interno ruotato di 'shift' posizioni. 'highlightK' evidenzia un settore. *)
ruotaCesare[shift_Integer, highlightK_Integer] :=
  Module[
    {n, rEst, rInt, rMid, angC, cEst, cInt,
     settoriEst, lettEst, settoriInt, lettInt,
     angFreccia, puntaFreccia, codaFreccia},
    n    = 26;  (* numero di lettere dell'alfabeto *)
    rEst = 1.0; (* raggio esterno: bordo dell'anello chiaro *)
    rInt = 0.62; (* raggio interno: confine tra anello chiaro e disco cifrato *)
    rMid = 0.31; (* meta' del disco cifrato: posizione delle etichette interne *)
        angC[k_] := Pi/2 - 2 Pi k / n; (* angolo del settore k: parte da Pi/2 (ore 12) e procede in senso orario *)
        cEst = Table[Hue[k/n, 0.55, 0.75], {k, 0, n-1}]; (* palette anello esterno (chiaro) *)
    cInt = Table[Hue[k/n, 0.85, 0.55], {k, 0, n-1}]; (* palette disco interno (cifrato) *)
        settoriEst = Table[
      {cEst[[k+1]],
       If[k == highlightK, Opacity[1.0], Opacity[0.75]],
       Annulus[{0,0}, {rInt, rEst}, {angC[k] - Pi/n, angC[k] + Pi/n}]},
      {k, 0, n-1}];
    (* Etichette lettera chiaro: font piu' grande per il settore selezionato *)
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
    (* Etichette lettera cifrata: posizionate a meta' del disco interno *)
    lettInt = Table[
      Text[
        Style[alfabeto[[Mod[k + shift, n] + 1]], 11, Bold, White],
        {(rMid + (rInt - rMid)/2) * Cos[angC[k]],
         (rMid + (rInt - rMid)/2) * Sin[angC[k]]}],
      {k, 0, n-1}];
        angFreccia   = angC[highlightK]; (* la freccia punta sempre al settore selezionato dallo slider *)
    codaFreccia  = 1.30 * {Cos[angFreccia], Sin[angFreccia]};
    puntaFreccia = 1.03 * {Cos[angFreccia], Sin[angFreccia]};
    (* Assemblaggio finale: settori + etichette + label centrale + freccia indicatrice + bordo *)
    Graphics[
      Join[
        settoriEst, lettEst, settoriInt, lettInt,
        {Text[Style["Cifrato", 10, Italic, White], {0, 0.0}]},
        {Thickness[0.008], RGBColor[0.7, 0.2, 0.2],
         Arrow[{codaFreccia, puntaFreccia}]},
        {Thickness[0.005], GrayLevel[0.5], Circle[{0,0}, rInt]}],
      ImageSize  -> 320,
      PlotRange  -> {{-1.38, 1.38}, {-1.38, 1.38}}]
  ]

ruotaInterattiva[shiftDyn_] :=
  DynamicModule[
    {settoreCorrente = 0}, (* indice 0-25 della lettera puntata dallo slider *)
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
      Dynamic[ruotaCesare[shiftDyn, settoreCorrente]] (* ridisegna la ruota ad ogni movimento dello slider *)
    }, Alignment -> Center]
  ]

esercizioUniversaleCesare[] :=
  DynamicModule[
        {seed = 42,              (* seed iniziale di default *)
         messaggioCifrato = "",  (* testo cifrato mostrato come consegna *)
         shiftSegreto = 0,       (* shift usato per cifrare, nascosto all'utente *)
         messaggioChiaro = "",   (* testo in chiaro originale, e' la soluzione *)
         rispostaUtente = "",    (* risposta inserita dall'utente *)
         tentativi = 0,          (* numero di tentativi gia' usati *)
         feedbackMsg = "",       (* messaggio mostrato dopo Verifica Risultato *)
         soluzioneVisibile = False, (* True quando si mostra la soluzione *)
         suggerimentoStep = 0,   (* livello del suggerimento attivo: 0=nessuno, 1=primo, 2=secondo *)
         esercizioGenerato = False, (* True dopo aver premuto Genera Esercizio *)
         shiftEsplorazione = 0}, (* shift separato per la ruota di esplorazione *)
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
            If[!IntegerQ[seed],
              feedbackMsg = "\[WarningSign] Errore: il seed deve essere un numero intero."; esercizioGenerato = False,
              Module[{ris},
                ris = generaEsercizioConSeedCesare[seed]; (* genera {cifrato, shift, chiaro} *)
                messaggioCifrato = ris[[1]]; shiftSegreto = ris[[2]];
                messaggioChiaro = ris[[3]]; rispostaUtente = "";
                tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
                suggerimentoStep = 0; esercizioGenerato = True]];,
            Background -> RGBColor[0.15, 0.5, 0.8], ImageSize -> {160, 35}]
}],
        Spacer[8],
        Dynamic[If[esercizioGenerato,
          Framed[Column[{
            Style["Testo cifrato da decifrare:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
            Style[messaggioCifrato, 15, Bold]}],
            Background -> RGBColor[0.7, 0.3, 0.1, 0.1],
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
  Button
      [Style["Verifica Risultato", 12, Bold, White],
       If[esercizioGenerato, tentativi++];
       Which[!esercizioGenerato, Null,
             ToUpperCase[StringReplace[StringTrim[rispostaUtente], " "->""]] ===
             StringReplace[messaggioChiaro, " "->""],
             feedbackMsg = "\[Checkmark] Corretto! Hai impiegato " <>
                           ToString[tentativi] <>
                               If[tentativi == 1, " tentativo.", " tentativi."],
             True,
             feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                           ToString[tentativi] <> "."];
       , Background->RGBColor[0.2, 0.6, 0.3], ImageSize->{150, 30}],
      Spacer[6],
      Button[Dynamic[Style["Suggerimento", 12, Bold, White, FontVariations -> {"StrikeThrough" -> !(esercizioGenerato && suggerimentoStep < 3)}]],
             If[esercizioGenerato && suggerimentoStep < 3, suggerimentoStep++],
             Background->Dynamic[If[esercizioGenerato && suggerimentoStep < 3, RGBColor[0.85, 0.65, 0.05], RGBColor[0.6, 0.6, 0.6]]],
             Enabled->Dynamic[esercizioGenerato && suggerimentoStep < 3],
             ImageSize->{120, 30}],
      Spacer[6],
      Button[Style["Pulisci Campi", 12, Bold, White], seed = 42;
             messaggioCifrato = ""; shiftSegreto = 0; messaggioChiaro = "";
             rispostaUtente = ""; tentativi = 0; feedbackMsg = "";
             soluzioneVisibile = False; suggerimentoStep = 0;
             esercizioGenerato = False; shiftEsplorazione = 0;
             , Background->RGBColor[0.2, 0.4, 0.7], ImageSize->{110, 30}],
      Spacer[6],
      Button[Style["Mostra Soluzione", 12, Bold, White],
             If[esercizioGenerato, soluzioneVisibile = True];
             , Background->RGBColor[0.7, 0.2, 0.2], ImageSize->{130, 30}]
        }],
        Spacer[8],
        Dynamic[Which[
          feedbackMsg === "", "",
          StringStartsQ[feedbackMsg, "\[Checkmark]"],
            Framed[Style[feedbackMsg, 12, RGBColor[0.2, 0.6, 0.3]],
              Background -> RGBColor[0.2, 0.6, 0.3, 0.1],
              FrameStyle -> RGBColor[0.2, 0.6, 0.3],
              RoundingRadius -> 5, FrameMargins -> 10],
          True,
            Framed[Style[feedbackMsg, 12, RGBColor[0.7, 0.2, 0.2]],
              Background -> RGBColor[0.7, 0.2, 0.2, 0.1],
              FrameStyle -> RGBColor[0.7, 0.2, 0.2],
              RoundingRadius -> 5, FrameMargins -> 10]
        ]],
        (* Suggerimento progressivo a 3 livelli, attivato manualmente dal bottone *)
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento 1: la lettera piu' frequente in italiano e' la E. " <>
              "La lettera piu' comune nel testo cifrato probabilmente corrisponde alla E.",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep == 2,
            Framed[Style[
              "\[LightBulb] Suggerimento 2: lo shift e' nell'intervallo [" <>
              ToString[Max[1, shiftSegreto - 4]] <> ", " <>
              ToString[Min[25, shiftSegreto + 4]] <> "].",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep >= 3,
            Framed[Style[
              "\[LightBulb] Suggerimento 3: lo shift esatto e' " <>
              ToString[shiftSegreto] <> ". Usa la ruota per verificare.",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10]]],
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Shift: ", 12, Bold], Style[ToString[shiftSegreto], 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[0.7, 0.2, 0.2, 0.1],
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
      ImageSize -> 560]
  ]

esercizioUniversaleVigenere[] :=
  DynamicModule[
        {seed = 42,              (* seed iniziale di default *)
         messaggioCifrato = "",  (* testo cifrato mostrato come consegna *)
         chiaveSegreto = "",     (* chiave mostrata all'utente: nel Vigenere la chiave e' nota *)
         messaggioChiaro = "",   (* testo in chiaro originale, e' la soluzione *)
         rispostaUtente = "",    (* risposta inserita dall'utente *)
         tentativi = 0,          (* numero di tentativi gia' usati *)
         feedbackMsg = "",       (* messaggio mostrato dopo Verifica Risultato *)
         soluzioneVisibile = False, (* True quando si mostra la soluzione *)
         suggerimentoStep = 0,   (* livello del suggerimento attivo: 0=nessuno, 1=primo, 2=secondo *)
         esercizioGenerato = False}, (* True dopo aver premuto Genera Esercizio *)
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
            If[!IntegerQ[seed],
              feedbackMsg = "\[WarningSign] Errore: il seed deve essere un numero intero."; esercizioGenerato = False,
              Module[{ris},
                ris = generaEsercizioConSeedVigenere[seed]; (* genera {cifrato, chiave, chiaro} *)
                messaggioCifrato = ris[[1]]; chiaveSegreto = ris[[2]];
                messaggioChiaro = ris[[3]]; rispostaUtente = "";
                tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
                suggerimentoStep = 0; esercizioGenerato = True]];,
            Background -> RGBColor[0.4, 0.1, 0.7], ImageSize -> {160, 35}]
}],
        Spacer[8],
        Dynamic[If[esercizioGenerato,
          Column[{
            Framed[Column[{
              Style["Testo cifrato:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
              Style[messaggioCifrato, 15, Bold]}],
              Background -> RGBColor[0.7, 0.3, 0.1, 0.1],
              RoundingRadius -> 5, FrameStyle -> RGBColor[0.7, 0.3, 0.1], FrameMargins -> 8],
            Spacer[4],
            Framed[Row[{
              Style["Chiave: ", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
              Style[chiaveSegreto, 14, Bold, RGBColor[0.5, 0.2, 0.7]]}],
              Background -> RGBColor[0.5, 0.2, 0.7, 0.1],
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
  Button
      [Style["Verifica Risultato", 12, Bold, White],
       If[esercizioGenerato, tentativi++];
       Which[!esercizioGenerato, Null,
             ToUpperCase[StringReplace[StringTrim[rispostaUtente], " "->""]] ===
             StringReplace[messaggioChiaro, " "->""],
             feedbackMsg = "\[Checkmark] Corretto! Hai impiegato " <>
                           ToString[tentativi] <>
                               If[tentativi == 1, " tentativo.", " tentativi."],
             True,
             feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                           ToString[tentativi] <> "."];
       , Background->RGBColor[0.2, 0.6, 0.3], ImageSize->{150, 30}],
      Spacer[6],
      Button[Dynamic[Style["Suggerimento", 12, Bold, White, FontVariations -> {"StrikeThrough" -> !(esercizioGenerato && suggerimentoStep < 3)}]],
             If[esercizioGenerato && suggerimentoStep < 3, suggerimentoStep++],
             Background->Dynamic[If[esercizioGenerato && suggerimentoStep < 3, RGBColor[0.85, 0.65, 0.05], RGBColor[0.6, 0.6, 0.6]]],
             Enabled->Dynamic[esercizioGenerato && suggerimentoStep < 3],
             ImageSize->{120, 30}],
      Spacer[6],
      Button[Style["Pulisci Campi", 12, Bold, White], seed = 42;
             messaggioCifrato = ""; chiaveSegreto = ""; messaggioChiaro = "";
             rispostaUtente = ""; tentativi = 0; feedbackMsg = "";
             soluzioneVisibile = False; suggerimentoStep = 0;
             esercizioGenerato = False;
             , Background->RGBColor[0.2, 0.4, 0.7], ImageSize->{110, 30}],
      Spacer[6],
      Button[Style["Mostra Soluzione", 12, Bold, White],
             If[esercizioGenerato, soluzioneVisibile = True];
             , Background->RGBColor[0.7, 0.2, 0.2], ImageSize->{130, 30}]
        }],
        Spacer[8],
        Dynamic[Which[
          feedbackMsg === "", "",
          StringStartsQ[feedbackMsg, "\[Checkmark]"],
            Framed[Style[feedbackMsg, 12, RGBColor[0.2, 0.6, 0.3]],
              Background -> RGBColor[0.2, 0.6, 0.3, 0.1],
              FrameStyle -> RGBColor[0.2, 0.6, 0.3],
              RoundingRadius -> 5, FrameMargins -> 10],
          True,
            Framed[Style[feedbackMsg, 12, RGBColor[0.7, 0.2, 0.2]],
              Background -> RGBColor[0.7, 0.2, 0.2, 0.1],
              FrameStyle -> RGBColor[0.7, 0.2, 0.2],
              RoundingRadius -> 5, FrameMargins -> 10]
        ]],
        (* Suggerimento progressivo a 3 livelli, attivato manualmente dal bottone *)
        Dynamic[Which[
          !esercizioGenerato || suggerimentoStep == 0, "",
          suggerimentoStep == 1,
            Framed[Style[
              "\[LightBulb] Suggerimento 1: per decifrare Vigenere si SOTTRAE lo shift " <>
              "invece di sommarlo. La prima lettera della chiave e' '" <>
              StringTake[chiaveSegreto, 1] <> "' (shift " <>
              ToString[indiceLettera[ToUpperCase[StringTake[chiaveSegreto,1]]]] <> ").",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep == 2,
            Framed[Style[
              "\[LightBulb] Suggerimento 2: la chiave '" <> chiaveSegreto <>
              "' ha " <> ToString[StringLength[chiaveSegreto]] <>
              " lettere. Decifra le prime " <> ToString[StringLength[chiaveSegreto]] <>
              " lettere, poi riparti dall'inizio della chiave.",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10],
          suggerimentoStep >= 3,
            Framed[Style[
              "\[LightBulb] Suggerimento 3: le prime 3 lettere del testo in chiaro sono '" <>
              StringTake[messaggioChiaro, Min[3, StringLength[messaggioChiaro]]] <> "'.",
              12, Italic, RGBColor[0.65, 0.5, 0.1]],
              Background -> RGBColor[0.7, 0.6, 0.1, 0.1],
              FrameStyle -> RGBColor[0.75, 0.6, 0.1],
              RoundingRadius -> 5, FrameMargins -> 10]]],
        Dynamic[If[soluzioneVisibile && esercizioGenerato,
          Framed[Column[{
            Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
            Row[{Style["Chiave: ", 12, Bold], Style[chiaveSegreto, 13, Bold]}],
            Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]}],
            Background -> RGBColor[0.7, 0.2, 0.2, 0.1],
            FrameStyle -> RGBColor[0.6, 0.1, 0.1],
            RoundingRadius -> 5, FrameMargins -> 10],
          ""]]
}, Alignment -> Left, Spacings -> 1],
      ImageSize -> 560]
  ]

avviaLaboratorio[] :=
  TabView[ (* crea un pannello a schede con le due sezioni esercizi *)
    {
      "Esercizi Cesare"      -> esercizioUniversaleCesare[],
      "Esercizi Vigenere"    -> esercizioUniversaleVigenere[]
    },
    ImageSize -> Full
  ]

bottoneEserciziCesare[] :=
  Button[
    Style["\[RightTriangle]  Apri gli Esercizi \[LongDash] Cifrario di Cesare",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Deploy[Pane[CrittografiaArcaica`esercizioUniversaleCesare[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}]], (* Deploy blocca la modifica, Pane aggiunge la scrollbar *)
        "Output",
        Deployed -> True, Editable -> False, Deletable -> False, Selectable -> False, Copyable -> False, ShowCellBracket -> False]},
      Deployed -> True, Editable -> False, Deletable -> False, Saveable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Esercizi Cesare"
    ],
    Background -> RGBColor[0.15, 0.55, 0.25],
    ImageSize  -> {460, 50}
  ]

bottoneEserciziVigenere[] :=
  Button[
    Style["\[RightTriangle]  Apri gli Esercizi \[LongDash] Cifrario di Vigenere",
          15, Bold, White],
    CreateDocument[
      {ExpressionCell[
        Deploy[Pane[CrittografiaArcaica`esercizioUniversaleVigenere[], {620, 700},
          Scrollbars -> {False, True}, AppearanceElements -> {}]], (* Deploy blocca la modifica, Pane aggiunge la scrollbar *)
        "Output",
        Deployed -> True, Editable -> False, Deletable -> False, Selectable -> False, Copyable -> False, ShowCellBracket -> False]},
      Deployed -> True, Editable -> False, Deletable -> False, Saveable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Esercizi Vigenere"
    ],
    Background -> RGBColor[0.55, 0.10, 0.40],
    ImageSize  -> {460, 50}
  ]

dizionarioItaliano;
(*forza il precaricamento del dizionario al caricamento del pacchetto,
 evitando lag al primo esercizio *)

    End[];

Protect[avviaLaboratorio, bottoneEserciziCesare, bottoneEserciziVigenere,
        esercizioUniversaleCesare, esercizioUniversaleVigenere];

EndPackage[];
