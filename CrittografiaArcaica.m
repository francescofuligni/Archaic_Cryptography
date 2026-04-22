(* ::Package:: *)

(* ============================================================
   CrittografiaArcaica.m
   Laboratorio Interattivo di Crittografia Arcaica
   Cifrario di Cesare e Cifrario di Vigenere

   Matematica Computazionale 2025/2026

   Changelog:
   - FIX: colori ruota di Cesare (palette distinta, contrasto migliorato)
   - FEAT: ruota interattiva con hover del mouse sull'anello esterno
   - FEAT: suggerimento automatico integrato nella Verifica Risultato
   - FEAT: generatore di frasi infinito (composizione soggetto+verbo+complemento)
   - FEAT: grafico frequenze con riferimento italiano sovrapposto
   - FIX: Vigenere accetta solo parole (chiave filtrata + avviso utente in tempo reale)
   - FIX: shift Vigenere corretto e verificato (A=0 ... Z=25)
   - FIX: struttura esercizi (Cesare -> decifra, Vigenere -> cifra)
   - FEAT: nota didattica "Cesare e' sottocaso di Vigenere" nel laboratorio
   ============================================================ *)

BeginPackage["CrittografiaArcaica`"]

(* ============================================================
   DICHIARAZIONI DI USO (usage) -- visibili fuori dal pacchetto
   ============================================================ *)

laboratorioCesare::usage =
  "laboratorioCesare[] apre il Laboratorio Libero del Cifrario di Cesare: \
cifratura, decifratura, ruota interattiva con hover del mouse e analisi \
delle frequenze con riferimento all'italiano standard."

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre gli Esercizi del Cifrario di Cesare. \
L'utente riceve un testo CIFRATO e deve trovare il testo in CHIARO. \
Funzionalita': Genera Esercizio (Seed, frasi infinite), Verifica Risultato \
con suggerimento automatico, Suggerimento progressivo, Mostra Soluzione, Pulisci Campi."

laboratorioVigenere::usage =
  "laboratorioVigenere[] apre il Laboratorio Libero del Cifrario di Vigenere. \
Include validazione della chiave (solo lettere), tabella degli shift, \
e nota didattica su Cesare come sottocaso di Vigenere."

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre gli Esercizi del Cifrario di Vigenere. \
L'utente riceve il testo in CHIARO e la CHIAVE e deve produrre il testo CIFRATO. \
Funzionalita': Genera Esercizio (Seed, frasi infinite), Verifica Risultato \
con suggerimento automatico, Suggerimento progressivo, Mostra Soluzione, Pulisci Campi."

(* ============================================================
   INIZIO SEZIONE PRIVATA
   Tutto il codice implementativo e le variabili ausiliarie
   sono nascosti nel contesto Private, non accessibili dall'utente.
   ============================================================ *)

Begin["`Private`"]

(* ============================================================
   COSTANTI
   ============================================================ *)

(* 26 lettere maiuscole dell'alfabeto internazionale *)
alfabeto = CharacterRange["A", "Z"];

(* Frequenze percentuali delle lettere nell'italiano standard.
   Fonte: Bortolini, Tagliavini, Zampolli (1972).
   Ordine: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *)
freqItaliano = {11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54,
                11.28, 0.00, 0.00, 6.51, 2.51, 6.88, 9.83, 3.05,
                0.51, 6.37, 4.98, 5.62, 3.01, 2.10, 0.00, 0.00,
                0.00, 0.49};

(* ============================================================
   UTILITA' -- VALIDAZIONE INPUT
   ============================================================ *)

(*
  soloLettere[s]
  Input:  s -- stringa qualsiasi
  Output: True se s e' non vuota e contiene SOLO lettere dell'alfabeto,
          False altrimenti.
  Scopo:  validare la chiave del Cifrario di Vigenere.
*)
soloLettere[s_String] :=
  Module[
    {chars},
    chars = Select[Characters[ToUpperCase[s]], MemberQ[alfabeto, #] &];
    StringLength[s] > 0 && Length[chars] == StringLength[s]
  ]

(* ============================================================
   GENERATORE DI FRASI INFINITO
   ============================================================ *)

(*
  generaFrase[seed]
  Input:  seed -- intero, controlla SeedRandom per riproducibilita'
  Output: stringa maiuscola con una frase generata combinando
          soggetti, verbi e complementi da liste predefinite.
  Logica: ogni lista ha ~10 elementi -> circa 1000 combinazioni distinte,
          riproducibili con lo stesso seed. Questo supera il limite
          della lista fissa della versione precedente del pacchetto.
*)
generaFrase[seed_Integer] :=
  Module[
    {soggetti, verbi, complementi, s, v, c},
    soggetti = {
      "IL MATEMATICO", "LA CRITTOGRAFA", "LO STUDENTE", "IL CODICE",
      "LA CHIAVE", "IL MESSAGGIO", "IL CIFRARIO", "LA FORMULA",
      "IL RICERCATORE", "LA SCIENZA"
    };
    verbi = {
      "NASCONDE", "RIVELA", "PROTEGGE", "TRASFORMA", "ANALIZZA",
      "SCOPRE", "STUDIA", "CIFRA", "DECIFRA", "CUSTODISCE"
    };
    complementi = {
      "UN SEGRETO ANTICO", "IL MESSAGGIO CIFRATO", "LA VERITA NASCOSTA",
      "UN CODICE MISTERIOSO", "LA SOLUZIONE GIUSTA", "UN TESORO PREZIOSO",
      "IL TESTO IN CHIARO", "UNA FORMULA SEGRETA", "IL METODO CORRETTO",
      "UN ALFABETO SEGRETO"
    };
    SeedRandom[seed];
    s = RandomChoice[soggetti];
    v = RandomChoice[verbi];
    c = RandomChoice[complementi];
    s <> " " <> v <> " " <> c
  ]

(* ============================================================
   CIFRATURA E DECIFRATURA -- CESARE
   ============================================================ *)

(*
  cifraCesare[testo, shift]
  Input:  testo -- stringa di qualsiasi carattere
          shift -- intero 0-25 (numero di posizioni di spostamento)
  Output: stringa cifrata con il Cifrario di Cesare.
  Logica: il testo viene portato in maiuscolo; ogni lettera viene
          spostata di shift posizioni in avanti nell'alfabeto
          (con wrap-around modulo 26).
          I caratteri non alfabetici restano invariati.
*)
cifraCesare[testo_String, shift_Integer] :=
  Module[
    {caratteri, cifrati},
    (* Porto tutto in maiuscolo prima della cifratura *)
    caratteri = Characters[ToUpperCase[testo]];
    (* Mappa su ogni carattere: shift se lettera, invariato altrimenti *)
    cifrati = Map[
      Function[c,
        If[MemberQ[alfabeto, c],
          (* Posizione 0-indicizzata + shift, modulo 26 -> riconverto in lettera *)
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
  Input:  testo -- stringa cifrata con il Cifrario di Cesare
          shift -- lo shift usato per cifrare (0-25)
  Output: testo in chiaro.
  Logica: la decifratura equivale a cifrare con shift negativo modulo 26.
*)
decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]]

(*
  frequenzeLettere[testo]
  Input:  testo -- stringa qualsiasi
  Output: lista di 26 interi: numero di occorrenze di ciascuna
          lettera dell'alfabeto (A, B, ..., Z) nel testo.
*)
frequenzeLettere[testo_String] :=
  Module[
    {solo},
    (* Estraggo solo le lettere maiuscole *)
    solo = Select[Characters[ToUpperCase[testo]], MemberQ[alfabeto, #] &];
    (* Conto le occorrenze di ciascuna delle 26 lettere *)
    Map[Function[l, Count[solo, l]], alfabeto]
  ]

(* ============================================================
   CIFRATURA E DECIFRATURA -- VIGENERE
   ============================================================ *)

(*
  cifraVigenere[testo, chiave]
  Input:  testo  -- stringa di testo in chiaro
          chiave -- stringa (deve contenere solo lettere, lunghezza >= 1)
  Output: stringa cifrata con il Cifrario di Vigenere,
          oppure stringa di errore se la chiave non e' valida.
  Logica: la chiave viene ripetuta ciclicamente sul testo.
          Per ogni lettera del testo, lo shift e' la posizione
          0-indicizzata della lettera chiave corrispondente (A=0, Z=25).
          I caratteri non alfabetici del testo restano invariati
          e NON avanzano l'indice della chiave.
  FIX: la chiave viene filtrata estraendo solo le lettere;
       se il risultato e' vuoto, si restituisce un messaggio di errore.
*)
cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    (* FIX: estraggo solo le lettere dalla chiave *)
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera dell'alfabeto."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0; (* indice nella chiave, parte da 0 *)
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        (* sh = posizione 0-indicizzata della lettera chiave: A->0, B->1, ..., Z->25 *)
        sh = Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1,1]] - 1;
        AppendTo[risultato,
          alfabeto[[ Mod[Position[alfabeto, c][[1,1]] - 1 + sh, 26] + 1 ]]
        ];
        kIndex++, (* l'indice chiave avanza SOLO sulle lettere del testo *)
        AppendTo[risultato, c] (* carattere non-lettera: resta invariato *)
      ],
      {i, 1, Length[caratteri]}
    ];
    StringJoin[risultato]
  ]

(*
  decifraVigenere[testo, chiave]
  Input:  testo  -- stringa cifrata con il Cifrario di Vigenere
          chiave -- la stessa parola chiave usata per cifrare
  Output: testo in chiaro.
  Logica: identica a cifraVigenere, ma lo shift viene sottratto
          invece di essere sommato (shift negativo modulo 26).
*)
decifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, caratteri, risultato, kIndex, c, sh},
    testUp      = ToUpperCase[testo];
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {},
      Return["ERRORE: la chiave deve contenere almeno una lettera dell'alfabeto."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex    = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        sh = Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1,1]] - 1;
        (* FIX: shift NEGATIVO per decifrare, modulo 26 per wrap-around corretto *)
        AppendTo[risultato,
          alfabeto[[ Mod[Position[alfabeto, c][[1,1]] - 1 - sh, 26] + 1 ]]
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
  Input:  testo  -- stringa di testo in chiaro
          chiave -- stringa chiave (solo lettere)
  Output: lista di quadruple {lettera_chiaro, lettera_chiave, shift, lettera_cifrata},
          limitata alle prime 24 lettere per leggibilita' nella tabella.
*)
tabellaShiftVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveChars, chiaveLen, soleLettere, risultato, kIndex, sh, lCifrata},
    testUp      = ToUpperCase[testo];
    chiaveChars = Select[Characters[ToUpperCase[chiave]], MemberQ[alfabeto, #] &];
    If[chiaveChars === {}, Return[{}]];
    chiaveLen   = Length[chiaveChars];
    (* Considero solo le lettere del testo *)
    soleLettere = Select[Characters[testUp], MemberQ[alfabeto, #] &];
    risultato   = {};
    kIndex      = 0;
    (* Limito a 24 righe per non appesantire la tabella *)
    Do[
      sh = Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1,1]] - 1;
      lCifrata = alfabeto[[ Mod[Position[alfabeto, soleLettere[[i]]][[1,1]] - 1 + sh, 26] + 1 ]];
      AppendTo[risultato,
        {soleLettere[[i]],
         chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]],
         sh,
         lCifrata}
      ];
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
  Input:  seed -- intero fornito dall'utente
  Output: {testo_cifrato, shift_segreto, testo_chiaro}
  Logica: usa generaFrase[] per ottenere frasi potenzialmente infinite.
          Lo shift viene generato in modo indipendente dalla frase
          usando seed+999 come seme separato.
  FIX struttura: l'utente deve DECIFRARE (riceve il cifrato).
*)
generaEsercizioConSeedCesare[seed_Integer] :=
  Module[
    {frase, shift, cifrato},
    (* La frase dipende da seed *)
    frase = generaFrase[seed];
    (* Lo shift dipende da seed+999 per essere indipendente dalla frase *)
    SeedRandom[seed + 999];
    shift  = RandomInteger[{1, 25}];
    cifrato = cifraCesare[frase, shift];
    (* Output: {cifrato, shift, chiaro} -- l'utente riceve il cifrato *)
    {cifrato, shift, frase}
  ]

(*
  generaEsercizioConSeedVigenere[seed]
  Input:  seed -- intero fornito dall'utente
  Output: {testo_chiaro, chiave_segreta, testo_cifrato}
  Logica: usa generaFrase[] per frasi infinite e una lista di chiavi predefinite.
  FIX struttura: l'utente deve CIFRARE (riceve il chiaro e la chiave).
*)
generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {chiavi, frase, chiave, cifrato},
    chiavi = {"SOLE", "MARE", "LUNA", "VENTO", "FUOCO", "ACQUA",
              "CIELO", "TERRA", "LUCE", "OMBRA", "CHIAVE", "CODICE",
              "PIETRA", "FIUME", "STELLA", "NOTTE", "GIORNO"};
    frase = generaFrase[seed];
    SeedRandom[seed + 777];
    chiave  = RandomChoice[chiavi];
    cifrato = cifraVigenere[frase, chiave];
    (* Output: {chiaro, chiave, cifrato} -- l'utente riceve chiaro e chiave *)
    {frase, chiave, cifrato}
  ]

(* ============================================================
   GRAFICO FREQUENZE CON RIFERIMENTO ITALIANO
   ============================================================ *)

(*
  graficaFrequenze[testo]
  Input:  testo -- stringa da analizzare (tipicamente il testo cifrato)
  Output: grafico BarChart con:
          - barre blu: frequenze relative (%) delle lettere nel testo
          - barre arancioni: frequenze attese nell'italiano standard
  Didattica: la differenza tra le due distribuzioni evidenzia come
             lo shift di Cesare "sposti" il picco della E nell'alfabeto.
             Se i picchi coincidono, lo shift e' probabilmente 0.
*)
graficaFrequenze[testo_String] :=
  Module[
    {conteggi, totale, freqRel},
    conteggi = frequenzeLettere[testo];
    totale   = Total[conteggi];
    If[totale == 0,
      Return[Style[
        "(Nessuna lettera nel testo: impossibile calcolare le frequenze.)",
        11, Italic, Gray]]
    ];
    (* Frequenze relative in percentuale *)
    freqRel = N[100 * conteggi / totale];
    BarChart[
      {freqRel, freqItaliano},
      ChartLabels  -> {Placed[alfabeto, Below], None},
      ChartStyle   -> {RGBColor[0.2, 0.5, 0.85], RGBColor[0.95, 0.6, 0.1]},
      ChartLegends -> {"Testo analizzato (%)", "Italiano standard (%)"},
      AxesLabel    -> {None, "Frequenza (%)"},
      PlotLabel    -> Style["Analisi delle frequenze", 13, Bold],
      ImageSize    -> 480,
      PlotRange    -> {0, Max[freqRel, freqItaliano] * 1.2},
      BarSpacing   -> {0.1, 0.5},
      LabelStyle   -> {FontSize -> 9}
    ]
  ]

(* ============================================================
   RUOTA DI CESARE -- FIX COLORI + INTERATTIVITA' MOUSE
   ============================================================ *)

(*
  ruotaCesare[shift, hoverAngle]
  Input:  shift      -- intero 0-25
          hoverAngle -- angolo (in radianti) della lettera evidenziata,
                        oppure Null se nessuna lettera e' sotto il mouse.
  Output: Graphics con:
          - anello esterno: lettere del testo chiaro (A-Z fisso)
          - disco interno: lettere del testo cifrato (ruotate di shift)
  FIX colori: palette Hue distinta per i 26 settori; lettere chiare
              in nero su sfondo chiaro, lettere cifrate in bianco su scuro.
  FEAT hover: il settore piu' vicino all'angolo hoverAngle viene ingrandito.
*)
ruotaCesare[shift_Integer, hoverAngle_] :=
  Module[
    {n, rEst, rInt, rTesto, angoloCentro, coloriEst, coloriInt,
     evidenzia, settoriEst, lettEst, settoriInt, lettInt, k},
    n      = 26;
    rEst   = 1.0;   (* bordo esterno dell'anello chiaro *)
    rInt   = 0.68;  (* confine tra anello chiaro e disco cifrato *)
    rTesto = 0.35;  (* centro del disco cifrato per le etichette *)
    (* Angolo del centro del settore k: parte dalla cima (Pi/2), senso antiorario *)
    angoloCentro[k_] := Pi/2 - 2 Pi k / n;
    (* Palette colori: 26 tinte ben distinte per i settori *)
    coloriEst = Table[Hue[k/n, 0.50, 0.88], {k, 0, n-1}];
    coloriInt = Table[Hue[k/n, 0.85, 0.55], {k, 0, n-1}];
    (* Individuo quale settore e' sotto il mouse *)
    evidenzia = If[NumericQ[hoverAngle],
      Module[{diffs},
        diffs = Table[
          Abs[Mod[angoloCentro[k] - hoverAngle + Pi, 2 Pi] - Pi],
          {k, 0, n-1}
        ];
        (* Indice 0-based del settore piu' vicino all'angolo hover *)
        First[Ordering[diffs]] - 1
      ],
      -1 (* nessun settore evidenziato *)
    ];
    (* Settori anello esterno (chiaro) *)
    settoriEst = Table[
      {coloriEst[[k+1]],
       If[k == evidenzia, Opacity[1.0], Opacity[0.70]],
       Annulus[{0,0}, {rInt, rEst},
         {angoloCentro[k] - Pi/n, angoloCentro[k] + Pi/n}]
      },
      {k, 0, n-1}
    ];
    (* Etichette anello esterno *)
    lettEst = Table[
      Text[
        Style[alfabeto[[k+1]],
          If[k == evidenzia, 15, 11], Bold,
          If[k == evidenzia, Black, GrayLevel[0.15]]
        ],
        {(rInt + (rEst - rInt)/2) * Cos[angoloCentro[k]],
         (rInt + (rEst - rInt)/2) * Sin[angoloCentro[k]]}
      ],
      {k, 0, n-1}
    ];
    (* Settori disco interno (cifrato, ruotati di shift) *)
    settoriInt = Table[
      {coloriInt[[ Mod[k + shift, n] + 1 ]],
       Opacity[0.90],
       Disk[{0,0}, rInt,
         {angoloCentro[k] - Pi/n, angoloCentro[k] + Pi/n}]
      },
      {k, 0, n-1}
    ];
    (* Etichette disco interno *)
    lettInt = Table[
      Text[
        Style[alfabeto[[ Mod[k + shift, n] + 1 ]], 10, Bold, White],
        {(rTesto + (rInt - rTesto)/2) * Cos[angoloCentro[k]],
         (rTesto + (rInt - rTesto)/2) * Sin[angoloCentro[k]]}
      ],
      {k, 0, n-1}
    ];
    Graphics[
      Join[
        settoriEst, lettEst,
        settoriInt, lettInt,
        (* Etichette descrittive centrali *)
        {Text[Style["Chiaro",  10, Italic, GrayLevel[0.35]], {0,  1.16}]},
        {Text[Style["Cifrato", 10, Italic, White],           {0,  0.0 }]},
        (* Freccia rossa che indica il settore A in cima *)
        {Thick, Red, Arrow[{{0, 1.32}, {0, 1.03}}]},
        (* Cerchio separatore tra anello esterno e disco interno *)
        {Thick, GrayLevel[0.25], Circle[{0,0}, rInt]}
      ],
      ImageSize    -> 320,
      Background   -> GrayLevel[0.12],
      PlotRange    -> {{-1.40, 1.40}, {-1.40, 1.40}}
    ]
  ]

(*
  ruotaInterattiva[shiftDyn]
  Input:  shiftDyn -- variabile Dynamic che contiene lo shift corrente
  Output: DynamicModule con EventHandler che traccia il mouse sull'immagine
          e aggiorna l'angolo di hover per evidenziare il settore corrispondente.
  FEAT: quando il cursore passa sull'anello esterno (raggio 0.68-1.05),
        la lettera sotto il cursore viene evidenziata con dimensione maggiore.
*)
ruotaInterattiva[shiftDyn_] :=
  DynamicModule[
    {angHover = Null},
    EventHandler[
      Dynamic[ruotaCesare[shiftDyn, angHover]],
      {
        "MouseMoved" :> Module[
          {pos, mx, my, r},
          pos = MousePosition["Graphics"];
          (* Verifico che la posizione sia disponibile *)
          If[pos =!= None && Length[pos] == 2,
            (* Converto coordinate normalizzate [0,1] in coordinate della ruota [-1.4, 1.4] *)
            mx = pos[[1]] * 2.80 - 1.40;
            my = pos[[2]] * 2.80 - 1.40;
            r  = Sqrt[mx^2 + my^2];
            If[r >= 0.68 && r <= 1.05,
              angHover = ArcTan[mx, my],
              angHover = Null
            ]
          ]
        ],
        "MouseExited" :> (angHover = Null)
      }
    ]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO CESARE
   ============================================================ *)

(*
  laboratorioCesare[]
  Input:  nessuno
  Output: DynamicModule con:
          - campo testo libero e slider shift 0-25
          - bottoni Cifra / Decifra / Pulisci Campi
          - ruota di Cesare interattiva (hover del mouse)
          - grafico delle frequenze con riferimento italiano
*)
laboratorioCesare[] :=
  DynamicModule[
    {testoInput = "", shiftLab = 3, risultatoCifra = "", risultatoDecifra = ""},
    Panel[
      Column[
        {
          Style["Laboratorio Libero \[LongDash] Cifrario di Cesare", 18, Bold, RGBColor[0.2, 0.4, 0.7]],
          Style["Inserisci un testo, scegli lo shift e cifra o decifra.", 12, Italic, Gray],
          Spacer[8],
          (* Campo testo *)
          Row[{
            Style["Testo: ", 13, Bold],
            InputField[Dynamic[testoInput], String,
              FieldSize -> {30, 2},
              FieldHint -> "Scrivi qui il tuo messaggio..."]
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
          (* Bottoni azione *)
          Row[{
            Button[
              Style["Cifra \[RightArrow]", 13, Bold, White],
              risultatoCifra   = cifraCesare[testoInput, shiftLab];
              risultatoDecifra = "";,
              Background -> RGBColor[0.2, 0.6, 0.3], ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["\[LeftArrow] Decifra", 13, Bold, White],
              risultatoDecifra = decifraCesare[testoInput, shiftLab];
              risultatoCifra   = "";,
              Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              testoInput       = "";
              shiftLab         = 3;
              risultatoCifra   = "";
              risultatoDecifra = "";,
              Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}
            ]
          }],
          Spacer[8],
          (* Risultato cifratura *)
          Dynamic[
            If[risultatoCifra =!= "",
              Framed[
                Column[{
                  Style["Testo cifrato:", 12, Bold, RGBColor[0.2, 0.6, 0.3]],
                  Style[risultatoCifra, 14, Bold]
                }],
                Background   -> RGBColor[0.92, 1.0, 0.93],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.2, 0.6, 0.3],
                FrameMargins -> 8
              ],
              ""
            ]
          ],
          (* Risultato decifratura *)
          Dynamic[
            If[risultatoDecifra =!= "",
              Framed[
                Column[{
                  Style["Testo decifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
                  Style[risultatoDecifra, 14, Bold]
                }],
                Background   -> RGBColor[0.97, 0.93, 1.0],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.5, 0.2, 0.7],
                FrameMargins -> 8
              ],
              ""
            ]
          ],
          Spacer[10],
          (* Ruota interattiva *)
          Style["Ruota di Cesare \[LongDash] passa il mouse sull'anello esterno per evidenziare una lettera:",
                13, Bold],
          Dynamic[ruotaInterattiva[shiftLab]],
          Spacer[10],
          (* Grafico frequenze *)
          Style["Analisi delle frequenze:", 13, Bold],
          Style["Blu = frequenze nel testo cifrato; Arancione = riferimento italiano standard.",
                11, Italic, Gray],
          Dynamic[
            If[risultatoCifra =!= "",
              graficaFrequenze[risultatoCifra],
              Style["(Il grafico appare dopo aver premuto Cifra)", 11, Italic, Gray]
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize  -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE CESARE
   FIX struttura: l'utente riceve il testo CIFRATO e trova il CHIARO.
   FEAT: suggerimento automatico alla Verifica Risultato.
   ============================================================ *)

(*
  esercizioUniversaleCesare[]
  Input:  nessuno
  Output: DynamicModule con le 5 funzionalita' obbligatorie:
          1. Genera Esercizio: usa Seed + generaFrase (frasi infinite)
          2. Verifica Risultato: con suggerimento automatico all'errore
          3. Suggerimento: progressivo a 3 livelli (manuale)
          4. Mostra Soluzione: mostra shift e testo chiaro
          5. Pulisci Campi: azzera tutta l'interfaccia
  Logica dell'esercizio: l'utente riceve un testo CIFRATO con Cesare
                         e deve inserire il corrispondente testo in CHIARO.
*)
esercizioUniversaleCesare[] :=
  DynamicModule[
    {seed = 42, messaggioCifrato = "", shiftSegreto = 0,
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[
        {
          Style["Esercizi \[LongDash] Cifrario di Cesare", 18, Bold, RGBColor[0.2, 0.4, 0.7]],
          Style["Ti viene dato un testo cifrato: trova il testo originale in chiaro!",
                12, Italic, Gray],
          Spacer[8],
          (* Input Seed + Genera Esercizio *)
          Row[{
            Style["Seed: ", 13, Bold],
            InputField[Dynamic[seed], Number,
              FieldSize -> {8, 1}, FieldHint -> "es. 42"],
            Spacer[8],
            Button[
              Style["Genera Esercizio", 13, Bold, White],
              Module[{ris},
                ris               = generaEsercizioConSeedCesare[seed];
                messaggioCifrato  = ris[[1]];  (* testo cifrato da mostrare *)
                shiftSegreto      = ris[[2]];  (* shift segreto (soluzione) *)
                messaggioChiaro   = ris[[3]];  (* testo chiaro (soluzione) *)
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
          (* Testo cifrato da decifrare *)
          Dynamic[
            If[esercizioGenerato,
              Framed[
                Column[{
                  Style["Testo cifrato da decifrare:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
                  Style[messaggioCifrato, 15, Bold, Black]
                }],
                Background   -> RGBColor[1.0, 0.95, 0.88],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.7, 0.3, 0.1],
                FrameMargins -> 8
              ],
              Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]
            ]
          ],
          Spacer[6],
          (* Campo risposta dell'utente *)
          Dynamic[
            If[esercizioGenerato,
              Column[{
                Style["Inserisci il testo decifrato:", 12, Bold],
                InputField[Dynamic[rispostaUtente], String,
                  FieldSize -> {30, 2},
                  FieldHint -> "Scrivi qui la tua risposta in chiaro..."]
              }],
              ""
            ]
          ],
          Spacer[6],
          (* Bottoni principali *)
          Row[{
            Button[
              Style["Verifica Risultato", 13, Bold, White],
              If[esercizioGenerato,
                tentativi++;
                If[ToUpperCase[StringTrim[rispostaUtente]] === messaggioChiaro,
                  (* Risposta corretta *)
                  feedbackMsg = "\[Checkmark] Corretto! Hai impiegato " <>
                    ToString[tentativi] <>
                    If[tentativi == 1, " tentativo.", " tentativi."],
                  (* Risposta errata *)
                  If[tentativi >= 3,
                    feedbackMsg       = "\[Cross] Risposta errata. Hai esaurito i 3 tentativi.";
                    soluzioneVisibile = True,
                    (* FEAT: suggerimento automatico progressivo *)
                    suggerimentoStep  = tentativi;
                    feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                      ToString[tentativi] <> "/3. " <>
                      Which[
                        tentativi == 1,
                          "Suggerimento automatico: la lettera piu' frequente in italiano e' la E.",
                        tentativi == 2,
                          "Suggerimento automatico: lo shift e' nell'intervallo [" <>
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
              seed              = 42;
              messaggioCifrato  = "";
              shiftSegreto      = 0;
              messaggioChiaro   = "";
              rispostaUtente    = "";
              tentativi         = 0;
              feedbackMsg       = "";
              soluzioneVisibile = False;
              suggerimentoStep  = 0;
              esercizioGenerato = False;,
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
          (* Feedback verifica (contiene il suggerimento automatico) *)
          Dynamic[
            If[feedbackMsg =!= "",
              Framed[
                Style[feedbackMsg, 12,
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]
                ],
                Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                  RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.92, 0.92]],
                RoundingRadius -> 5, FrameMargins -> 8
              ],
              ""
            ]
          ],
          (* Suggerimento manuale progressivo (tre livelli) *)
          Dynamic[
            Which[
              !esercizioGenerato || suggerimentoStep == 0, "",
              suggerimentoStep == 1,
                Framed[
                  Style["\[LightBulb] Suggerimento 1: la lettera piu' frequente in italiano \
e' la E. Analizza il grafico delle frequenze nel Laboratorio Libero: \
la lettera piu' comune nel testo cifrato probabilmente corrisponde alla E.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep == 2,
                Framed[
                  Style["\[LightBulb] Suggerimento 2: lo shift e' nell'intervallo [" <>
                    ToString[Max[1, shiftSegreto - 4]] <> ", " <>
                    ToString[Min[25, shiftSegreto + 4]] <> "].",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep >= 3,
                Framed[
                  Style["\[LightBulb] Suggerimento 3: lo shift esatto e' " <>
                    ToString[shiftSegreto] <>
                    ". Usa il Laboratorio Libero per verificare.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ]
            ]
          ],
          (* Soluzione completa *)
          Dynamic[
            If[soluzioneVisibile && esercizioGenerato,
              Framed[
                Column[{
                  Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
                  Row[{Style["Shift usato: ", 12, Bold],
                       Style[ToString[shiftSegreto], 13, Bold]}],
                  Row[{Style["Testo in chiaro: ", 12, Bold],
                       Style[messaggioChiaro, 13, Bold]}]
                }],
                Background   -> RGBColor[1.0, 0.93, 0.93],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.6, 0.1, 0.1],
                FrameMargins -> 10
              ],
              ""
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize  -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO VIGENERE
   FIX: chiave filtrata con avviso in tempo reale
   FEAT: nota didattica "Cesare e' sottocaso di Vigenere"
   ============================================================ *)

(*
  laboratorioVigenere[]
  Input:  nessuno
  Output: DynamicModule con:
          - campo testo e campo chiave (validato: solo lettere)
          - avviso in tempo reale se la chiave contiene caratteri non validi
          - bottoni Cifra / Decifra / Pulisci Campi
          - tabella shift lettera per lettera (con colonna lettera cifrata)
          - nota didattica: il Cifrario di Cesare e' un sottocaso di Vigenere
*)
laboratorioVigenere[] :=
  DynamicModule[
    {testoInput = "", chiaveInput = "", risultatoCifra = "",
     risultatoDecifra = "", tabellaVis = {}, avvisoChiave = ""},
    Panel[
      Column[
        {
          Style["Laboratorio Libero \[LongDash] Cifrario di Vigenere", 18, Bold, RGBColor[0.5, 0.2, 0.7]],
          Style["Inserisci un testo e una parola chiave (solo lettere) per cifrare o decifrare.",
                12, Italic, Gray],
          Spacer[6],
          (* FEAT: nota didattica -- Cesare come sottocaso di Vigenere *)
          Framed[
            Column[{
              Style["\[FilledSquare] NOTA IMPORTANTE \[LongDash] Il Cifrario di Cesare e' un caso speciale di Vigenere",
                    12, Bold, RGBColor[0.35, 0.1, 0.60]],
              Style["Se la chiave e' composta da una singola lettera (es. 'D'), il Cifrario \
di Vigenere si riduce esattamente al Cifrario di Cesare con uno shift pari alla posizione \
di quella lettera nell'alfabeto (A=0, B=1, C=2, D=3, ...). \
Prova tu stesso: cifra un testo con chiave 'D' qui, e confrontalo con il Laboratorio \
del Cesare usando shift = 3!",
                    11, Italic, RGBColor[0.3, 0.1, 0.5]]
            }],
            Background   -> RGBColor[0.96, 0.90, 1.0],
            RoundingRadius -> 6,
            FrameStyle   -> RGBColor[0.6, 0.3, 0.9],
            FrameMargins -> 10
          ],
          Spacer[8],
          (* Campo testo *)
          Row[{
            Style["Testo:  ", 13, Bold],
            InputField[Dynamic[testoInput], String,
              FieldSize -> {28, 2},
              FieldHint -> "Messaggio da cifrare..."]
          }],
          Spacer[4],
          (* Campo chiave con avviso in tempo reale *)
          Row[{
            Style["Chiave: ", 13, Bold],
            InputField[Dynamic[chiaveInput], String,
              FieldSize -> {15, 1},
              FieldHint -> "Solo lettere (es. SOLE)..."],
            Spacer[8],
            (* FIX: avviso immediato se la chiave non e' valida *)
            Dynamic[
              If[chiaveInput =!= "" && !soloLettere[chiaveInput],
                Style["! Solo lettere dell'alfabeto", 11, Bold, RGBColor[0.8, 0.2, 0.0]],
                ""
              ]
            ]
          }],
          Spacer[6],
          (* Bottoni azione *)
          Row[{
            Button[
              Style["Cifra \[RightArrow]", 13, Bold, White],
              (* FIX: blocco se la chiave non e' valida *)
              If[chiaveInput === "" || !soloLettere[chiaveInput],
                avvisoChiave = "Errore: inserisci una chiave valida (solo lettere, nessuno spazio).",
                avvisoChiave     = "";
                risultatoCifra   = cifraVigenere[testoInput, chiaveInput];
                risultatoDecifra = "";
                tabellaVis       = tabellaShiftVigenere[testoInput, chiaveInput];
              ],
              Background -> RGBColor[0.5, 0.2, 0.7], ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["\[LeftArrow] Decifra", 13, Bold, White],
              If[chiaveInput === "" || !soloLettere[chiaveInput],
                avvisoChiave = "Errore: inserisci una chiave valida (solo lettere, nessuno spazio).",
                avvisoChiave     = "";
                risultatoDecifra = decifraVigenere[testoInput, chiaveInput];
                risultatoCifra   = "";
                tabellaVis       = tabellaShiftVigenere[testoInput, chiaveInput];
              ],
              Background -> RGBColor[0.2, 0.5, 0.7], ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              testoInput       = "";
              chiaveInput      = "";
              risultatoCifra   = "";
              risultatoDecifra = "";
              tabellaVis       = {};
              avvisoChiave     = "";,
              Background -> RGBColor[0.5, 0.5, 0.5], ImageSize -> {130, 35}
            ]
          }],
          Spacer[4],
          (* Avviso chiave non valida *)
          Dynamic[
            If[avvisoChiave =!= "",
              Style[avvisoChiave, 12, Bold, RGBColor[0.75, 0.1, 0.0]],
              ""
            ]
          ],
          Spacer[4],
          (* Risultato cifra o decifra *)
          Dynamic[
            If[risultatoCifra =!= "",
              Framed[
                Column[{
                  Style["Testo cifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
                  Style[risultatoCifra, 14, Bold]
                }],
                Background   -> RGBColor[0.96, 0.92, 1.0],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.5, 0.2, 0.7],
                FrameMargins -> 8
              ],
              If[risultatoDecifra =!= "",
                Framed[
                  Column[{
                    Style["Testo decifrato:", 12, Bold, RGBColor[0.2, 0.5, 0.7]],
                    Style[risultatoDecifra, 14, Bold]
                  }],
                  Background   -> RGBColor[0.92, 0.97, 1.0],
                  RoundingRadius -> 5,
                  FrameStyle   -> RGBColor[0.2, 0.5, 0.7],
                  FrameMargins -> 8
                ],
                ""
              ]
            ]
          ],
          Spacer[10],
          (* Tabella shift lettera per lettera -- FIX: aggiunta colonna lettera cifrata *)
          Style["Tabella degli shift lettera per lettera:", 13, Bold],
          Style["Mostra come ogni lettera del testo chiaro viene trasformata dalla lettera della chiave.",
                11, Italic, Gray],
          Dynamic[
            If[tabellaVis =!= {},
              Module[{righe},
                righe = Map[
                  {Style[#[[1]], 13, Bold, Black],
                   Style[#[[2]], 13, Bold, RGBColor[0.5, 0.2, 0.7]],
                   Style["+" <> ToString[#[[3]]], 12, RGBColor[0.2, 0.6, 0.3]],
                   (* FIX: colonna lettera cifrata risultante *)
                   Style[#[[4]], 13, Bold, RGBColor[0.7, 0.3, 0.1]]} &,
                  tabellaVis
                ];
                Grid[
                  Prepend[righe,
                    {Style["Lettera chiaro",  11, Bold, Gray],
                     Style["Lettera chiave",  11, Bold, Gray],
                     Style["Shift",           11, Bold, Gray],
                     Style["Lettera cifrata", 11, Bold, Gray]}
                  ],
                  Frame      -> All,
                  Background -> {None, {RGBColor[0.9, 0.85, 1.0], {White}}},
                  FrameStyle -> LightGray,
                  Spacings   -> {1.5, 0.8}
                ]
              ],
              Style["(La tabella appare dopo la cifratura o decifratura)", 11, Italic, Gray]
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize  -> 560
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE VIGENERE
   FIX struttura: l'utente riceve il testo CHIARO e la CHIAVE,
                  deve produrre il testo CIFRATO.
   FEAT: suggerimento automatico alla Verifica Risultato.
   ============================================================ *)

(*
  esercizioUniversaleVigenere[]
  Input:  nessuno
  Output: DynamicModule con le 5 funzionalita' obbligatorie:
          1. Genera Esercizio: Seed + frasi infinite + chiave casuale
          2. Verifica Risultato: con suggerimento automatico all'errore
          3. Suggerimento: progressivo a 3 livelli (manuale)
          4. Mostra Soluzione: chiave e testo cifrato corretto
          5. Pulisci Campi: azzera tutta l'interfaccia
  Logica dell'esercizio: l'utente riceve il testo in CHIARO e la CHIAVE,
                         e deve produrre il corrispondente testo CIFRATO.
*)
esercizioUniversaleVigenere[] :=
  DynamicModule[
    {seed = 42, messaggioChiaro = "", chiaveSegreto = "",
     messaggioCifrato = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[
        {
          Style["Esercizi \[LongDash] Cifrario di Vigenere", 18, Bold, RGBColor[0.5, 0.2, 0.7]],
          Style["Ti vengono dati il testo in chiaro e la chiave: cifra il messaggio con Vigenere!",
                12, Italic, Gray],
          Spacer[8],
          (* Seed e Genera *)
          Row[{
            Style["Seed: ", 13, Bold],
            InputField[Dynamic[seed], Number,
              FieldSize -> {8, 1}, FieldHint -> "es. 42"],
            Spacer[8],
            Button[
              Style["Genera Esercizio", 13, Bold, White],
              Module[{ris},
                ris               = generaEsercizioConSeedVigenere[seed];
                messaggioChiaro   = ris[[1]];  (* testo in chiaro da mostrare *)
                chiaveSegreto     = ris[[2]];  (* chiave da mostrare *)
                messaggioCifrato  = ris[[3]];  (* testo cifrato (la soluzione) *)
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
          (* Consegna: testo chiaro + chiave *)
          Dynamic[
            If[esercizioGenerato,
              Column[{
                Framed[
                  Column[{
                    Style["Testo in chiaro:", 12, Bold, RGBColor[0.2, 0.5, 0.2]],
                    Style[messaggioChiaro, 14, Bold, Black]
                  }],
                  Background   -> RGBColor[0.92, 1.0, 0.92],
                  RoundingRadius -> 5,
                  FrameStyle   -> RGBColor[0.3, 0.6, 0.3],
                  FrameMargins -> 8
                ],
                Spacer[4],
                Framed[
                  Row[{
                    Style["Chiave: ", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
                    Style[chiaveSegreto, 14, Bold, RGBColor[0.5, 0.2, 0.7]]
                  }],
                  Background   -> RGBColor[0.96, 0.92, 1.0],
                  RoundingRadius -> 5,
                  FrameStyle   -> RGBColor[0.5, 0.2, 0.7],
                  FrameMargins -> 8
                ]
              }],
              Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]
            ]
          ],
          Spacer[6],
          (* Campo risposta *)
          Dynamic[
            If[esercizioGenerato,
              Column[{
                Style["Inserisci il testo cifrato con Vigenere:", 12, Bold],
                InputField[Dynamic[rispostaUtente], String,
                  FieldSize -> {30, 2},
                  FieldHint -> "Applica la chiave lettera per lettera..."]
              }],
              ""
            ]
          ],
          Spacer[6],
          (* Bottoni principali *)
          Row[{
            Button[
              Style["Verifica Risultato", 13, Bold, White],
              If[esercizioGenerato,
                tentativi++;
                If[ToUpperCase[StringTrim[rispostaUtente]] === messaggioCifrato,
                  feedbackMsg = "\[Checkmark] Corretto! Hai cifrato il messaggio in " <>
                    ToString[tentativi] <>
                    If[tentativi == 1, " tentativo.", " tentativi."],
                  If[tentativi >= 3,
                    feedbackMsg       = "\[Cross] Risposta errata. Hai esaurito i 3 tentativi.";
                    soluzioneVisibile = True,
                    (* FEAT: suggerimento automatico progressivo *)
                    suggerimentoStep  = tentativi;
                    feedbackMsg = "\[Cross] Non corretto \[LongDash] Tentativo " <>
                      ToString[tentativi] <> "/3. " <>
                      Which[
                        tentativi == 1,
                          "Suggerimento automatico: ricorda che A=0, B=1, ..., Z=25. " <>
                          "La prima lettera della chiave e' '" <>
                          StringTake[chiaveSegreto, 1] <> "'.",
                        tentativi == 2,
                          "Suggerimento automatico: la chiave ha " <>
                          ToString[StringLength[chiaveSegreto]] <>
                          " lettere e si ripete ciclicamente sul testo.",
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
              seed              = 42;
              messaggioChiaro   = "";
              chiaveSegreto     = "";
              messaggioCifrato  = "";
              rispostaUtente    = "";
              tentativi         = 0;
              feedbackMsg       = "";
              soluzioneVisibile = False;
              suggerimentoStep  = 0;
              esercizioGenerato = False;,
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
          (* Feedback verifica (contiene suggerimento automatico) *)
          Dynamic[
            If[feedbackMsg =!= "",
              Framed[
                Style[feedbackMsg, 12,
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.1, 0.5, 0.1], RGBColor[0.5, 0.1, 0.1]]
                ],
                Background -> If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                  RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.92, 0.92]],
                RoundingRadius -> 5, FrameMargins -> 8
              ],
              ""
            ]
          ],
          (* Suggerimento manuale progressivo *)
          Dynamic[
            Which[
              !esercizioGenerato || suggerimentoStep == 0, "",
              suggerimentoStep == 1,
                Framed[
                  Style["\[LightBulb] Suggerimento 1: nel Cifrario di Vigenere, ogni lettera \
del testo viene spostata in avanti di un numero di posizioni pari alla posizione della \
lettera chiave corrispondente (A=0, B=1, ..., Z=25). \
La prima lettera della chiave e' '" <> StringTake[chiaveSegreto, 1] <> "'.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep == 2,
                Framed[
                  Style["\[LightBulb] Suggerimento 2: la chiave '" <> chiaveSegreto <>
                    "' ha " <> ToString[StringLength[chiaveSegreto]] <>
                    " lettere e si ripete ciclicamente. Cifra le prime " <>
                    ToString[StringLength[chiaveSegreto]] <>
                    " lettere del testo, poi riprendi la chiave dall'inizio.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep >= 3,
                Framed[
                  Style["\[LightBulb] Suggerimento 3: le prime 3 lettere del testo cifrato \
corretto sono '" <>
                    StringTake[messaggioCifrato, Min[3, StringLength[messaggioCifrato]]] <> "'.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ]
            ]
          ],
          (* Soluzione completa *)
          Dynamic[
            If[soluzioneVisibile && esercizioGenerato,
              Framed[
                Column[{
                  Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
                  Row[{Style["Chiave: ", 12, Bold],
                       Style[chiaveSegreto, 13, Bold]}],
                  Row[{Style["Testo cifrato corretto: ", 12, Bold],
                       Style[messaggioCifrato, 13, Bold]}]
                }],
                Background   -> RGBColor[1.0, 0.93, 0.93],
                RoundingRadius -> 5,
                FrameStyle   -> RGBColor[0.6, 0.1, 0.1],
                FrameMargins -> 10
              ],
              ""
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize  -> 560
    ]
  ]

(* ============================================================
   FINE SEZIONE PRIVATA
   ============================================================ *)

End[ ]

EndPackage[ ]
