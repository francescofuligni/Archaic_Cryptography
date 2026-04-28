(* ::Package:: *)

(* ::Package:: *)

(* ::Package:: *)

BeginPackage["CrittografiaArcaica`"]

avviaLaboratorio::usage =
  "avviaLaboratorio[] avvia l'interfaccia principale con TabView."

bottoneEserciziCesare::usage =
  "bottoneEserciziCesare[] restituisce un bottone che apre gli Esercizi del Cifrario di Cesare. Da usare nella sezione II.3 del Tutorial.nb."

bottoneEserciziVigenere::usage =
  "bottoneEserciziVigenere[] restituisce un bottone che apre gli Esercizi del Cifrario di Vigenere. Da usare nella sezione III.3 del Tutorial.nb."

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre gli Esercizi del Cifrario di Cesare."

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre gli Esercizi del Cifrario di Vigenere."

Begin["`Private`"]

alfabeto = CharacterRange["A", "Z"]; (* lista delle 26 lettere maiuscole, usata come riferimento in tutto il pacchetto *)

(* Frequenze percentuali delle lettere nell'italiano scritto - fonte: De Mauro.
   Ordine: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *)
freqItaliano = {11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54,
                11.28, 0.00, 0.00, 6.51, 2.51, 6.88, 9.83, 3.05,
                0.51, 6.37, 4.98, 5.62, 3.01, 2.10, 0.00, 0.00,
                0.00, 0.49};

lettereIn[s_String] :=
  Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &] (* estrae solo le lettere A-Z da una stringa, ignorando spazi e punteggiatura *)

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
  
  (* Converte tutto in maiuscolo, pronto per la cifratura *)
  ToUpperCase[tutteLeParole]
];

generaParola[seed_Integer] := Module[{},
  SeedRandom[seed];
  RandomChoice[dizionarioItaliano]
]

cifraCesare[testo_String, shift_Integer] :=
  Module[{caratteri, cifrati},
    caratteri = Characters[ToUpperCase[testo]]; (* converto in maiuscolo e divido in lista di caratteri *)
    cifrati = Map[
      Function[c,
        If[MemberQ[alfabeto, c],
          alfabeto[[Mod[indiceLettera[c] + shift, 26] + 1]], (* sposto la lettera di shift posizioni con wrap-around *)
          c]], (* caratteri non alfabetici (spazi, punteggiatura) rimangono invariati *)
      caratteri];
    StringJoin[cifrati] (* riassemblo la lista di caratteri in una stringa *)
  ]

decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]] (* decifrare = cifrare con shift negativo, Mod gestisce il wrap-around *)

frequenzeLettere[testo_String] :=
  Module[{solo},
    solo = lettereIn[testo]; (* estraggo solo le lettere, ignorando tutto il resto *)
    Map[Function[l, Count[solo, l]], alfabeto] (* conto quante volte appare ogni lettera A-Z *)
  ]

cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    (* Estraggo solo le lettere dalla chiave, scartando eventuali caratteri non validi *)
    chiaveChars = lettereIn[chiave];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0; (* indice nella chiave: avanza solo quando incontriamo una lettera *)
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
                sh = indiceLettera[chiaveChars[[Mod[kIndex, chiaveLen] + 1]]];
        (* Sposto la lettera in avanti di sh posizioni, con wrap-around modulo 26 *)
        AppendTo[risultato,
          alfabeto[[Mod[indiceLettera[c] + sh, 26] + 1]]];
        kIndex++,
                AppendTo[risultato, c]],
      {i, 1, Length[caratteri]}];
    StringJoin[risultato]
  ]

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
          alfabeto[[Mod[indiceLettera[c] - sh, 26] + 1]]]; (* sottraggo sh: operazione inversa rispetto alla cifratura *)
        kIndex++,
        AppendTo[risultato, c]],
      {i, 1, Length[caratteri]}];
    StringJoin[risultato]
  ]

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
      {i, 1, Min[Length[soleLettere], 24]}]; (* limitata a 24 righe per non appesantire la visualizzazione *)
    risultato
  ]

generaEsercizioConSeedCesare[seed_Integer] :=
  Module[{parola, shift, cifrato},
    (* Genera la parola dal dizionario italiano usando il seed come indice riproducibile *)
    parola  = generaParola[seed];
        SeedRandom[seed + 999];
    shift   = RandomInteger[{1, 25}]; (* shift tra 1 e 25: escludiamo 0 = nessuna cifratura *)
    cifrato = cifraCesare[parola, shift];
    (* Restituisce {cifrato, shift, chiaro}: l'utente vede il cifrato, la soluzione e' il chiaro *)
    {cifrato, shift, parola}
  ]

generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {chiavi, parola, chiave, cifrato},
        chiavi = {"SOLE", "MARE", "LUNA", "VENTO", "FUOCO", "ACQUA",
              "CIELO", "TERRA", "LUCE", "OMBRA", "CHIAVE", "CODICE",
              "PIETRA", "FIUME", "STELLA", "NOTTE", "GIORNO"};
    parola = generaParola[seed];
    (* Usa seed+777 per la chiave, indipendente da seed usato per la parola *)
    SeedRandom[seed + 777];
    chiave  = RandomChoice[chiavi];
    cifrato = cifraVigenere[parola, chiave];
        {cifrato, chiave, parola}
  ]

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
    (* 26 colori arcobaleno distinti, uno per ogni lettera del grafico cifrato *)
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

ruotaCesare[shift_Integer, highlightK_Integer] :=
  Module[
    {n, rEst, rInt, rMid, angC, cEst, cInt,
     settoriEst, lettEst, settoriInt, lettInt,
     angFreccia, puntaFreccia, codaFreccia},
    n    = 26;  (* numero di lettere dell'alfabeto *)
    rEst = 1.0; (* raggio esterno: bordo dell'anello chiaro *)
    rInt = 0.62; (* raggio interno: confine tra anello chiaro e disco cifrato *)
    rMid = 0.31; (* meta' del disco cifrato: posizione delle etichette interne *)
        angC[k_] := Pi/2 - 2 Pi k / n; (* angolo del settore k: parte da Pi/2 (cima) e ruota in senso antiorario *)
        cEst = Table[Hue[k/n, 0.55, 0.75], {k, 0, n-1}]; (* anello esterno: piu' chiaro *)
    cInt = Table[Hue[k/n, 0.85, 0.55], {k, 0, n-1}]; (* disco interno: piu' scuro e saturo *)
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
    Graphics[
      Join[
        settoriEst, lettEst, settoriInt, lettInt,
        {Text[Style["Cifrato", 10, Italic, White], {0, 0.0}]},
        {Thickness[0.008], RGBColor[0.7, 0.2, 0.2],
         Arrow[{codaFreccia, puntaFreccia}]},
        {Thickness[0.005], GrayLevel[0.5], Circle[{0,0}, rInt]}],
      ImageSize  -> 320,
      Background -> GrayLevel[0.97],
      PlotRange  -> {{-1.38, 1.38}, {-1.38, 1.38}}]
  ]

ruotaInterattiva[shiftDyn_] :=
  DynamicModule[
    {settoreCorrente = 0}, (* indice 0-25 della lettera puntata dallo slider *)
    Column[{
      Row[{
        Style["Punta la lettera: ", 11, Italic, Gray],
        Slider[Dynamic[settoreCorrente], {0, 25, 1}, ImageSize -> 220], (* slider per selezionare la lettera dell'anello esterno *)
        Spacer[6],
        Dynamic[Style[
          alfabeto[[settoreCorrente + 1]] <> " \[RightArrow] " <>
          alfabeto[[Mod[settoreCorrente + shiftDyn, 26] + 1]], (* mostra la coppia: lettera chiaro -> lettera cifrata *)
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
            Module[{ris},
              ris = generaEsercizioConSeedCesare[seed]; (* genera {cifrato, shift, chiaro} *)
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
                If[tentativi >= 3, (* al terzo errore mostra automaticamente la soluzione *)
                  feedbackMsg = "\[Cross] Risposta errata. Tentativi esauriti.";
                  soluzioneVisibile = True,
                  suggerimentoStep = tentativi; (* avanza il livello di suggerimento ad ogni errore *)
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
            Module[{ris},
              ris = generaEsercizioConSeedVigenere[seed]; (* genera {cifrato, chiave, chiaro} *)
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
          Scrollbars -> {False, True}, AppearanceElements -> {}]],
        "Output",
        Deployed -> True, Editable -> False, Deletable -> False, Selectable -> False, Copyable -> False, ShowCellBracket -> False]},
      Deployed -> True, Editable -> False, Deletable -> False, Saveable -> False, ShowCellBracket -> False,
      WindowSize -> {660, 750}, WindowTitle -> "Esercizi Vigenere"
    ],
    Background -> RGBColor[0.55, 0.10, 0.40],
    ImageSize  -> {460, 50}
  ]

dizionarioItaliano; (* forza il precaricamento del dizionario al caricamento del pacchetto, evitando lag al primo esercizio *)

End[ ]

EndPackage[ ]
