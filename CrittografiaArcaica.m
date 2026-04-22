(* ============================================================
   CrittografiaArcaica.m
   Laboratorio Interattivo di Crittografia Arcaica
   Cifrario di Cesare e Cifrario di Vigenere
   
   Matematica Computazionale 2025/2026
   ============================================================ *)

BeginPackage["CrittografiaArcaica`"]

(* ============================================================
   DICHIARAZIONI DI USO (usage) -- visibili fuori dal pacchetto
   Queste stringhe appaiono quando l'utente digita ?NomeFunzione
   ============================================================ *)

(* --- Funzioni principali dell'interfaccia --- *)
laboratorioCesare::usage =
  "laboratorioCesare[] apre il Laboratorio Libero del Cifrario di Cesare: \
l'utente puo' cifrare e decifrare messaggi, visualizzare la ruota di Cesare \
e analizzare le frequenze delle lettere."

esercizioUniversaleCesare::usage =
  "esercizioUniversaleCesare[] apre la sezione Esercizi del Cifrario di Cesare. \
L'utente deve decifrare un messaggio cifrato generato casualmente tramite Seed. \
Funzionalita': Genera Esercizio, Verifica Risultato, Suggerimento, \
Mostra Soluzione, Pulisci Campi."

laboratorioVigenere::usage =
  "laboratorioVigenere[] apre il Laboratorio Libero del Cifrario di Vigenere: \
l'utente puo' cifrare e decifrare messaggi con una parola chiave e \
visualizzare la tabella degli shift lettera per lettera."

esercizioUniversaleVigenere::usage =
  "esercizioUniversaleVigenere[] apre la sezione Esercizi del Cifrario di Vigenere. \
L'utente deve trovare la chiave o decifrare un messaggio cifrato con Vigenere. \
Funzionalita': Genera Esercizio, Verifica Risultato, Suggerimento, \
Mostra Soluzione, Pulisci Campi."

(* ============================================================
   INIZIO SEZIONE PRIVATA
   Tutto il codice implementativo e le variabili ausiliarie
   sono nascosti nel contesto Private, non accessibili dall'utente.
   ============================================================ *)

Begin["`Private`"]

(* ============================================================
   COSTANTI E UTILITA' GENERALI
   ============================================================ *)

(* alfabeto italiano/internazionale: 26 lettere maiuscole *)
alfabeto = CharacterRange["A", "Z"];

(* ============================================================
   FUNZIONI DI CIFRATURA E DECIFRATURA -- CESARE
   ============================================================ *)

(*
  cifraCesare[testo, shift]
  Input:  testo  -- stringa di qualsiasi carattere
          shift  -- numero intero (lo shift dell'alfabeto, 0-25)
  Output: stringa cifrata con il Cifrario di Cesare
  Logica: ogni lettera maiuscola viene spostata di shift posizioni
          in avanti nell'alfabeto (con wrap-around modulo 26).
          I caratteri non alfabetici (spazi, punteggiatura) restano invariati.
*)
cifraCesare[testo_String, shift_Integer] :=
  Module[
    {caratteri, cifrati},
    (* Converto la stringa in lista di caratteri e la porto in maiuscolo *)
    caratteri = Characters[ToUpperCase[testo]];
    (* Per ogni carattere: se e' una lettera, shifto; altrimenti lascio invariato *)
    cifrati = Map[
      Function[c,
        If[MemberQ[alfabeto, c],
          (* posizione 0-indicizzata, shift modulo 26, poi riconverto *)
          alfabeto[[ Mod[Position[alfabeto, c][[1, 1]] - 1 + shift, 26] + 1 ]],
          c (* carattere non alfabetico: invariato *)
        ]
      ],
      caratteri
    ];
    StringJoin[cifrati]
  ]

(*
  decifraCesare[testo, shift]
  Input:  testo  -- stringa cifrata con Cesare
          shift  -- lo shift usato per cifrare (0-25)
  Output: testo in chiaro (decifratura = cifratura con shift negativo mod 26)
*)
decifraCesare[testo_String, shift_Integer] :=
  cifraCesare[testo, Mod[-shift, 26]]

(*
  frequenzeLettere[testo]
  Input:  testo -- stringa qualsiasi
  Output: lista di regole {lettera -> conteggio} per le 26 lettere maiuscole,
          utile per costruire il grafico a barre delle frequenze.
*)
frequenzeLettere[testo_String] :=
  Module[
    {solo, conteggi},
    (* Tengo solo le lettere alfabetiche maiuscole *)
    solo = Select[Characters[ToUpperCase[testo]], MemberQ[alfabeto, #] &];
    (* Conto le occorrenze di ogni lettera nell'alfabeto *)
    conteggi = Map[Function[l, Count[solo, l]], alfabeto];
    Thread[alfabeto -> conteggi]
  ]

(* ============================================================
   FUNZIONI DI CIFRATURA E DECIFRATURA -- VIGENERE
   ============================================================ *)

(*
  cifraVigenere[testo, chiave]
  Input:  testo  -- stringa di testo in chiaro
          chiave -- stringa (parola chiave, solo lettere)
  Output: stringa cifrata con il Cifrario di Vigenere
  Logica: la chiave viene ripetuta ciclicamente sul testo.
          Per ogni lettera del testo si applica uno shift pari alla
          posizione (0-indicizzata) della corrispondente lettera della chiave.
          I caratteri non alfabetici vengono mantenuti senza avanzare l'indice chiave.
*)
cifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveUp, chiaveChars, chiaveLen,
     caratteri, risultato, kIndex, c, shift},
    testUp   = ToUpperCase[testo];
    chiaveUp = ToUpperCase[chiave];
    (* Verifico che la chiave contenga solo lettere *)
    chiaveChars = Select[Characters[chiaveUp], MemberQ[alfabeto, #] &];
    If[chiaveChars == {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex = 0; (* indice nella chiave, parte da 0 *)
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        (* Shift dato dalla posizione della lettera chiave corrispondente *)
        shift = Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1, 1]] - 1;
        AppendTo[risultato,
          alfabeto[[ Mod[Position[alfabeto, c][[1, 1]] - 1 + shift, 26] + 1 ]]
        ];
        kIndex++, (* avanzo l'indice chiave solo sulle lettere *)
        AppendTo[risultato, c] (* non-lettera: invariata, chiave non avanza *)
      ],
      {i, 1, Length[caratteri]}
    ];
    StringJoin[risultato]
  ]

(*
  decifraVigenere[testo, chiave]
  Input:  testo  -- stringa cifrata con Vigenere
          chiave -- stringa (parola chiave usata per cifrare)
  Output: testo in chiaro (shift negativo per ogni lettera)
*)
decifraVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveUp, chiaveChars, chiaveLen,
     caratteri, risultato, kIndex, c, shift},
    testUp   = ToUpperCase[testo];
    chiaveUp = ToUpperCase[chiave];
    chiaveChars = Select[Characters[chiaveUp], MemberQ[alfabeto, #] &];
    If[chiaveChars == {},
      Return["ERRORE: la chiave deve contenere almeno una lettera."]
    ];
    chiaveLen = Length[chiaveChars];
    caratteri = Characters[testUp];
    risultato = {};
    kIndex = 0;
    Do[
      c = caratteri[[i]];
      If[MemberQ[alfabeto, c],
        shift = Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1, 1]] - 1;
        AppendTo[risultato,
          alfabeto[[ Mod[Position[alfabeto, c][[1, 1]] - 1 - shift, 26] + 1 ]]
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
  Input:  testo  -- stringa di testo in chiaro (solo lettere, gia' maiuscolo)
          chiave -- stringa chiave (gia' maiuscolo, gia' filtrata)
  Output: lista di triple {lettera_testo, lettera_chiave, shift}
          utile per visualizzare la tabella didattica dello shift lettera per lettera.
*)
tabellaShiftVigenere[testo_String, chiave_String] :=
  Module[
    {testUp, chiaveUp, chiaveChars, chiaveLen,
     caratteri, soleLettere, risultato, kIndex},
    testUp   = ToUpperCase[testo];
    chiaveUp = ToUpperCase[chiave];
    chiaveChars = Select[Characters[chiaveUp], MemberQ[alfabeto, #] &];
    If[chiaveChars == {}, Return[{}]];
    chiaveLen = Length[chiaveChars];
    (* Considero solo le lettere del testo *)
    soleLettere = Select[Characters[testUp], MemberQ[alfabeto, #] &];
    risultato = {};
    kIndex = 0;
    Do[
      AppendTo[risultato,
        {soleLettere[[i]],
         chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]],
         Position[alfabeto, chiaveChars[[ Mod[kIndex, chiaveLen] + 1 ]]][[1,1]] - 1}
      ];
      kIndex++,
      {i, 1, Length[soleLettere]}
    ];
    risultato
  ]

(* ============================================================
   GENERATORI DI ESERCIZI
   ============================================================ *)

(*
  generaEsercizioConSeedCesare[seed]
  Input:  seed -- intero non negativo fornito dall'utente
  Output: {messaggio_cifrato, shift_segreto, messaggio_chiaro}
          Genera un messaggio cifrato riproducibile dato lo stesso seed.
  Logica: il seed controlla SeedRandom, garantendo riproducibilita'.
          I messaggi sono scelti da una lista di frasi predefinite,
          lo shift e' un intero casuale tra 1 e 25.
*)
generaEsercizioConSeedCesare[seed_Integer] :=
  Module[
    {frasi, frase, shift, cifrato},
    (* Lista di frasi didattiche sull'informatica e la matematica *)
    frasi = {
      "LA MATEMATICA E LA REGINA DELLE SCIENZE",
      "LA CRITTOGRAFIA PROTEGGE I NOSTRI SEGRETI",
      "OGNI MESSAGGIO NASCONDE UN SEGRETO",
      "IL CIFRARIO DI CESARE E MOLTO ANTICO",
      "LA CONOSCENZA E UN TESORO PREZIOSO",
      "STUDIARE E IL SEGRETO DEL SUCCESSO",
      "I NUMERI SONO IL LINGUAGGIO DELLA NATURA",
      "LA LOGICA E LA BASE DEL PENSIERO SCIENTIFICO",
      "INFORMATICA E MATEMATICA VANNO A BRACCETTO",
      "CHI CERCA TROVA LA SOLUZIONE GIUSTA"
    };
    SeedRandom[seed];
    frase  = RandomChoice[frasi];
    shift  = RandomInteger[{1, 25}];
    cifrato = cifraCesare[frase, shift];
    {cifrato, shift, frase}
  ]

(*
  generaEsercizioConSeedVigenere[seed]
  Input:  seed -- intero non negativo fornito dall'utente
  Output: {messaggio_cifrato, chiave_segreta, messaggio_chiaro}
          Genera un messaggio cifrato con Vigenere, riproducibile col seed.
*)
generaEsercizioConSeedVigenere[seed_Integer] :=
  Module[
    {frasi, chiavi, frase, chiave, cifrato},
    frasi = {
      "LA MATEMATICA E LA REGINA DELLE SCIENZE",
      "LA CRITTOGRAFIA PROTEGGE I NOSTRI SEGRETI",
      "OGNI MESSAGGIO NASCONDE UN SEGRETO",
      "IL CIFRARIO DI VIGENERE E PIU SICURO",
      "LA CONOSCENZA E UN TESORO PREZIOSO",
      "STUDIARE E IL SEGRETO DEL SUCCESSO",
      "I NUMERI SONO IL LINGUAGGIO DELLA NATURA",
      "LA CHIAVE RENDE IL MESSAGGIO INVISIBILE",
      "CRITTOGRAFIA POLIALFABETICA E AFFASCINANTE",
      "SCOPRI IL MESSAGGIO NASCOSTO NELLA SEQUENZA"
    };
    (* Chiavi di lunghezza variabile tra 3 e 6 lettere *)
    chiavi = {"SOLE", "MARE", "LUNA", "VENTO", "FUOCO", "ACQUA",
              "CIELO", "TERRA", "LUCE", "OMBRA", "CHIAVE", "CODICE"};
    SeedRandom[seed];
    frase  = RandomChoice[frasi];
    chiave = RandomChoice[chiavi];
    cifrato = cifraVigenere[frase, chiave];
    {cifrato, chiave, frase}
  ]

(* ============================================================
   COMPONENTI GRAFICI -- RUOTA DI CESARE
   ============================================================ *)

(*
  ruotaCesare[shift]
  Input:  shift -- intero 0-25, lo shift corrente
  Output: oggetto grafico (Graphics) che mostra due anelli concentrici:
          anello esterno = alfabeto chiaro, anello interno = alfabeto cifrato.
  Nota:   la ruota e' usata nel Laboratorio Libero per visualizzare lo shift.
*)
ruotaCesare[shift_Integer] :=
  Module[
    {n, r1, r2, angoli, colori, etichette},
    n = 26; (* numero di lettere *)
    r1 = 1.0; (* raggio esterno (chiaro) *)
    r2 = 0.65; (* raggio interno (cifrato) *)
    angoli = Table[2 Pi k / n - Pi/2, {k, 0, n - 1}];
    colori = Table[
      Hue[k/n, 0.6, 0.95],
      {k, 0, n - 1}
    ];
    Graphics[
      Join[
        (* Settori colorati anello esterno *)
        Table[
          {colori[[k+1]], Opacity[0.5],
           Annulus[{0,0}, {r2+0.02, r1+0.06},
             {angoli[[k+1]] - Pi/n, angoli[[k+1]] + Pi/n}]},
          {k, 0, n-1}
        ],
        (* Lettere anello esterno (chiaro) *)
        Table[
          Text[
            Style[alfabeto[[k+1]], 13, Bold, Black],
            {(r1 + 0.15) * Cos[angoli[[k+1]]],
             (r1 + 0.15) * Sin[angoli[[k+1]]]}
          ],
          {k, 0, n-1}
        ],
        (* Settori colorati anello interno (cifrato, ruotato di shift) *)
        Table[
          {colori[[k+1]], Opacity[0.8],
           Disk[{0,0}, r2-0.02,
             {angoli[[k+1]] - Pi/n, angoli[[k+1]] + Pi/n}]},
          {k, 0, n-1}
        ],
        (* Lettere anello interno (cifrato) *)
        Table[
          Text[
            Style[alfabeto[[ Mod[k + shift, 26] + 1 ]], 11, Bold, White],
            {(r2 - 0.15) * Cos[angoli[[k+1]]],
             (r2 - 0.15) * Sin[angoli[[k+1]]]}
          ],
          {k, 0, n-1}
        ],
        (* Etichette centrali *)
        {Text[Style["Testo\nchiaro", 10, Italic, Gray], {0, 1.32}]},
        {Text[Style["Testo\ncifrato", 9, Italic, White], {0, 0}]},
        (* Freccia indicatore in alto *)
        {Thick, Red, Arrow[{{0, 1.55}, {0, 1.12}}]}
      ],
      ImageSize -> 300,
      Background -> GrayLevel[0.15]
    ]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO CESARE
   ============================================================ *)

(*
  laboratorioCesare[]
  Input:  nessuno
  Output: interfaccia interattiva (DynamicModule) per:
          - cifrare/decifrare un testo con shift scelto dall'utente,
          - visualizzare la ruota di Cesare animata,
          - mostrare il grafico a barre delle frequenze.
  Nota:   usa DynamicModule per localizzare le variabili dell'interfaccia.
          Non vengono usate variabili globali.
*)
laboratorioCesare[] :=
  DynamicModule[
    (* Variabili locali dell'interfaccia *)
    {testoInput = "", shift = 3, risultatoCifra = "", risultatoDecifra = ""},
    Panel[
      Column[
        {
          (* Titolo sezione *)
          Style["Laboratorio Libero — Cifrario di Cesare", 18, Bold,
                RGBColor[0.2, 0.4, 0.7]],
          Style["Inserisci un testo, scegli lo shift e cifra o decifra.", 12, Italic, Gray],
          Spacer[8],
          (* Campo di input testo *)
          Row[{
            Style["Testo: ", 13, Bold],
            InputField[Dynamic[testoInput], String,
              FieldSize -> {30, 2},
              FieldHint -> "Scrivi qui il tuo messaggio..."]
          }],
          Spacer[4],
          (* Slider per lo shift *)
          Row[{
            Style["Shift: ", 13, Bold],
            Slider[Dynamic[shift], {0, 25, 1}, ImageSize -> 200],
            Spacer[6],
            Dynamic[Style[ToString[shift], 14, Bold, RGBColor[0.7, 0.2, 0.2]]]
          }],
          Spacer[6],
          (* Bottoni Cifra / Decifra / Pulisci *)
          Row[{
            Button[
              Style["Cifra \[RightArrow]", 13, Bold, White],
              risultatoCifra = cifraCesare[testoInput, shift];
              risultatoDecifra = "";,
              Background -> RGBColor[0.2, 0.6, 0.3],
              ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["\[LeftArrow] Decifra", 13, Bold, White],
              risultatoDecifra = decifraCesare[testoInput, shift];
              risultatoCifra = "";,
              Background -> RGBColor[0.5, 0.2, 0.7],
              ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              testoInput = "";
              shift = 3;
              risultatoCifra = "";
              risultatoDecifra = "";,
              Background -> RGBColor[0.65, 0.65, 0.65],
              ImageSize -> {130, 35}
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
                Background -> RGBColor[0.92, 1.0, 0.93],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.2, 0.6, 0.3]
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
                Background -> RGBColor[0.97, 0.93, 1.0],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7]
              ],
              ""
            ]
          ],
          Spacer[10],
          (* Ruota di Cesare animata *)
          Style["Ruota di Cesare:", 14, Bold],
          Style["L'anello esterno mostra le lettere chiare, l'interno le lettere cifrate.", 11, Italic, Gray],
          Dynamic[ruotaCesare[shift]],
          Spacer[8],
          (* Grafico frequenze *)
          Style["Analisi delle frequenze del testo cifrato:", 14, Bold],
          Dynamic[
            If[risultatoCifra =!= "",
              Module[
                {freq},
                freq = frequenzeLettere[risultatoCifra];
                BarChart[
                  freq[[All, 2]],
                  ChartLabels -> Placed[alfabeto, Below],
                  ChartStyle -> "SandyTerrain",
                  AxesLabel -> {"Lettera", "Frequenza"},
                  PlotLabel -> Style["Frequenze nel testo cifrato", 12, Bold],
                  ImageSize -> 450
                ]
              ],
              Style["(Il grafico appare dopo la cifratura)", 11, Italic, Gray]
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize -> 520
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE CESARE
   ============================================================ *)

(*
  esercizioUniversaleCesare[]
  Input:  nessuno
  Output: interfaccia interattiva con le 5 funzionalita' obbligatorie:
          1. Genera Esercizio (tramite Seed)
          2. Verifica Risultato
          3. Suggerimento
          4. Mostra Soluzione
          5. Pulisci Campi
  Logica: l'utente inserisce un Seed, genera un messaggio cifrato
          e deve trovare lo shift corretto o inserire il testo in chiaro.
          Massimo 3 tentativi; al quarto tentativo viene mostrata la soluzione.
*)
esercizioUniversaleCesare[] :=
  DynamicModule[
    {seed = 1, messaggioCifrato = "", shiftSegreto = 0,
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[
        {
          Style["Esercizi — Cifrario di Cesare", 18, Bold, RGBColor[0.2, 0.4, 0.7]],
          Style["Decifra il messaggio! Inserisci un Seed e genera il tuo esercizio.", 12, Italic, Gray],
          Spacer[8],
          (* Input Seed *)
          Row[{
            Style["Seed: ", 13, Bold],
            InputField[Dynamic[seed], Number,
              FieldSize -> {8, 1},
              FieldHint -> "es. 42"],
            Spacer[8],
            Button[
              Style["Genera Esercizio", 13, Bold, White],
              (* Azzero lo stato per il nuovo esercizio *)
              Module[
                {ris},
                ris = generaEsercizioConSeedCesare[seed];
                messaggioCifrato   = ris[[1]];
                shiftSegreto       = ris[[2]];
                messaggioChiaro    = ris[[3]];
                rispostaUtente     = "";
                tentativi          = 0;
                feedbackMsg        = "";
                soluzioneVisibile  = False;
                suggerimentoStep   = 0;
                esercizioGenerato  = True;
              ],
              Background -> RGBColor[0.15, 0.5, 0.8],
              ImageSize -> {160, 35}
            ]
          }],
          Spacer[8],
          (* Messaggio cifrato *)
          Dynamic[
            If[esercizioGenerato,
              Framed[
                Column[{
                  Style["Messaggio cifrato:", 12, Bold, RGBColor[0.7, 0.3, 0.1]],
                  Style[messaggioCifrato, 15, Bold, Black]
                }],
                Background -> RGBColor[1.0, 0.95, 0.88],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.7, 0.3, 0.1],
                FrameMargins -> 8
              ],
              Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]
            ]
          ],
          Spacer[6],
          (* Input risposta utente *)
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
          (* Riga bottoni principali *)
          Row[{
            (* Verifica Risultato *)
            Button[
              Style["Verifica Risultato", 13, Bold, White],
              If[esercizioGenerato,
                tentativi++;
                If[ToUpperCase[rispostaUtente] === messaggioChiaro,
                  feedbackMsg = "\[Checkmark] Corretto! Bravo! Hai impiegato " <>
                                ToString[tentativi] <> " tentativo/i.",
                  If[tentativi >= 3,
                    feedbackMsg = "\[Cross] Risposta errata. Hai esaurito i tentativi. Premi 'Mostra Soluzione'.";
                    soluzioneVisibile = True,
                    feedbackMsg = "\[Cross] Non corretto. Tentativo " <>
                                  ToString[tentativi] <> "/3. Prova ancora o chiedi un Suggerimento."
                  ]
                ]
              ],
              Background -> RGBColor[0.2, 0.6, 0.3],
              ImageSize -> {170, 35}
            ],
            Spacer[8],
            (* Pulisci Campi *)
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              seed = 1;
              messaggioCifrato  = "";
              shiftSegreto      = 0;
              messaggioChiaro   = "";
              rispostaUtente    = "";
              tentativi         = 0;
              feedbackMsg       = "";
              soluzioneVisibile = False;
              suggerimentoStep  = 0;
              esercizioGenerato = False;,
              Background -> RGBColor[0.65, 0.65, 0.65],
              ImageSize -> {140, 35}
            ]
          }],
          Spacer[4],
          Row[{
            (* Suggerimento *)
            Button[
              Style["Suggerimento", 13, Bold, White],
              If[esercizioGenerato,
                suggerimentoStep++
              ],
              Background -> RGBColor[0.8, 0.6, 0.1],
              ImageSize -> {140, 35}
            ],
            Spacer[8],
            (* Mostra Soluzione *)
            Button[
              Style["Mostra Soluzione", 13, Bold, White],
              If[esercizioGenerato,
                soluzioneVisibile = True
              ],
              Background -> RGBColor[0.7, 0.2, 0.2],
              ImageSize -> {160, 35}
            ]
          }],
          Spacer[8],
          (* Feedback Verifica *)
          Dynamic[
            If[feedbackMsg =!= "",
              Framed[
                Style[feedbackMsg, 13, Bold,
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.1, 0.5, 0.1], RGBColor[0.6, 0.1, 0.1]]
                ],
                Background ->
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.9, 0.9]],
                RoundingRadius -> 5, FrameMargins -> 8
              ],
              ""
            ]
          ],
          (* Suggerimento progressivo *)
          Dynamic[
            Which[
              !esercizioGenerato || suggerimentoStep == 0, "",
              suggerimentoStep == 1,
                Framed[
                  Style["\[LightBulb] Suggerimento 1: prova ad analizzare le frequenze delle lettere. \
La lettera piu' frequente in italiano e' spesso la 'E'.", 12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep == 2,
                Framed[
                  Style["\[LightBulb] Suggerimento 2: lo shift e' un numero tra 1 e 25. \
Prova shift diversi fino a trovare parole di senso compiuto.", 12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep >= 3,
                Framed[
                  Style["\[LightBulb] Suggerimento 3: lo shift usato e' compreso tra " <>
                    ToString[Max[1, shiftSegreto - 5]] <> " e " <>
                    ToString[Min[25, shiftSegreto + 5]] <> ".",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ]
            ]
          ],
          (* Soluzione *)
          Dynamic[
            If[soluzioneVisibile && esercizioGenerato,
              Framed[
                Column[{
                  Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
                  Row[{Style["Shift: ", 12, Bold], Style[ToString[shiftSegreto], 13, Bold]}],
                  Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]
                }],
                Background -> RGBColor[1.0, 0.93, 0.93],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.6, 0.1, 0.1],
                FrameMargins -> 10
              ],
              ""
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize -> 540
    ]
  ]

(* ============================================================
   INTERFACCIA -- LABORATORIO LIBERO VIGENERE
   ============================================================ *)

(*
  laboratorioVigenere[]
  Input:  nessuno
  Output: interfaccia interattiva per cifrare/decifrare con Vigenere.
          Mostra la tabella degli shift lettera per lettera (didattica).
*)
laboratorioVigenere[] :=
  DynamicModule[
    {testoInput = "", chiaveInput = "", risultatoCifra = "",
     risultatoDecifra = "", tabellaVis = {}},
    Panel[
      Column[
        {
          Style["Laboratorio Libero — Cifrario di Vigenere", 18, Bold,
                RGBColor[0.5, 0.2, 0.7]],
          Style["Inserisci un testo e una parola chiave per cifrare o decifrare.", 12, Italic, Gray],
          Spacer[8],
          (* Testo *)
          Row[{
            Style["Testo:  ", 13, Bold],
            InputField[Dynamic[testoInput], String,
              FieldSize -> {28, 2},
              FieldHint -> "Messaggio da cifrare..."]
          }],
          Spacer[4],
          (* Chiave *)
          Row[{
            Style["Chiave: ", 13, Bold],
            InputField[Dynamic[chiaveInput], String,
              FieldSize -> {15, 1},
              FieldHint -> "Parola chiave (solo lettere)..."]
          }],
          Spacer[6],
          (* Bottoni *)
          Row[{
            Button[
              Style["Cifra \[RightArrow]", 13, Bold, White],
              risultatoCifra  = cifraVigenere[testoInput, chiaveInput];
              risultatoDecifra = "";
              tabellaVis = tabellaShiftVigenere[testoInput, chiaveInput];,
              Background -> RGBColor[0.5, 0.2, 0.7],
              ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["\[LeftArrow] Decifra", 13, Bold, White],
              risultatoDecifra = decifraVigenere[testoInput, chiaveInput];
              risultatoCifra = "";
              tabellaVis = tabellaShiftVigenere[testoInput, chiaveInput];,
              Background -> RGBColor[0.2, 0.5, 0.7],
              ImageSize -> {120, 35}
            ],
            Spacer[10],
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              testoInput = ""; chiaveInput = "";
              risultatoCifra = ""; risultatoDecifra = "";
              tabellaVis = {};,
              Background -> RGBColor[0.65, 0.65, 0.65],
              ImageSize -> {130, 35}
            ]
          }],
          Spacer[8],
          (* Risultato *)
          Dynamic[
            If[risultatoCifra =!= "",
              Framed[
                Column[{
                  Style["Testo cifrato:", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
                  Style[risultatoCifra, 14, Bold]
                }],
                Background -> RGBColor[0.96, 0.92, 1.0],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8
              ],
              If[risultatoDecifra =!= "",
                Framed[
                  Column[{
                    Style["Testo decifrato:", 12, Bold, RGBColor[0.2, 0.5, 0.7]],
                    Style[risultatoDecifra, 14, Bold]
                  }],
                  Background -> RGBColor[0.92, 0.97, 1.0],
                  RoundingRadius -> 5, FrameStyle -> RGBColor[0.2, 0.5, 0.7], FrameMargins -> 8
                ],
                ""
              ]
            ]
          ],
          Spacer[10],
          (* Tabella shift lettera per lettera *)
          Style["Tabella degli shift lettera per lettera:", 14, Bold],
          Style["Mostra come ogni lettera del testo viene spostata dalla lettera della chiave.", 11, Italic, Gray],
          Dynamic[
            If[tabellaVis =!= {},
              Module[
                {righe},
                righe = Map[
                  {Style[#[[1]], 13, Bold, Black],
                   Style[#[[2]], 13, Bold, RGBColor[0.5, 0.2, 0.7]],
                   Style["+" <> ToString[#[[3]]], 12, RGBColor[0.2, 0.6, 0.3]]} &,
                  Take[tabellaVis, Min[20, Length[tabellaVis]]]
                ];
                Grid[
                  Prepend[righe,
                    {Style["Lettera testo", 11, Bold, Gray],
                     Style["Lettera chiave", 11, Bold, Gray],
                     Style["Shift", 11, Bold, Gray]}
                  ],
                  Frame -> All,
                  Background -> {None, {RGBColor[0.9, 0.85, 1.0], {White}}},
                  FrameStyle -> LightGray,
                  Spacings -> {1.5, 0.8}
                ]
              ],
              Style["(La tabella appare dopo la cifratura o decifratura)", 11, Italic, Gray]
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize -> 540
    ]
  ]

(* ============================================================
   INTERFACCIA -- ESERCIZIO UNIVERSALE VIGENERE
   ============================================================ *)

(*
  esercizioUniversaleVigenere[]
  Input:  nessuno
  Output: interfaccia interattiva con le 5 funzionalita' obbligatorie
          adattate al Cifrario di Vigenere.
  Logica: l'utente riceve un messaggio cifrato con Vigenere e deve
          trovare il testo in chiaro originale.
          Massimo 3 tentativi; al terzo mostra la soluzione.
*)
esercizioUniversaleVigenere[] :=
  DynamicModule[
    {seed = 1, messaggioCifrato = "", chiaveSegreto = "",
     messaggioChiaro = "", rispostaUtente = "",
     tentativi = 0, feedbackMsg = "", soluzioneVisibile = False,
     suggerimentoStep = 0, esercizioGenerato = False},
    Panel[
      Column[
        {
          Style["Esercizi — Cifrario di Vigenere", 18, Bold, RGBColor[0.5, 0.2, 0.7]],
          Style["Decifra il messaggio! Inserisci un Seed e genera il tuo esercizio.", 12, Italic, Gray],
          Spacer[8],
          (* Input Seed *)
          Row[{
            Style["Seed: ", 13, Bold],
            InputField[Dynamic[seed], Number,
              FieldSize -> {8, 1},
              FieldHint -> "es. 42"],
            Spacer[8],
            Button[
              Style["Genera Esercizio", 13, Bold, White],
              Module[
                {ris},
                ris = generaEsercizioConSeedVigenere[seed];
                messaggioCifrato  = ris[[1]];
                chiaveSegreto     = ris[[2]];
                messaggioChiaro   = ris[[3]];
                rispostaUtente    = "";
                tentativi         = 0;
                feedbackMsg       = "";
                soluzioneVisibile = False;
                suggerimentoStep  = 0;
                esercizioGenerato = True;
              ],
              Background -> RGBColor[0.4, 0.1, 0.7],
              ImageSize -> {160, 35}
            ]
          }],
          Spacer[8],
          (* Messaggio cifrato *)
          Dynamic[
            If[esercizioGenerato,
              Framed[
                Column[{
                  Style["Messaggio cifrato (Vigenere):", 12, Bold, RGBColor[0.5, 0.2, 0.7]],
                  Style[messaggioCifrato, 15, Bold, Black]
                }],
                Background -> RGBColor[0.97, 0.93, 1.0],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.5, 0.2, 0.7], FrameMargins -> 8
              ],
              Style["(Genera un esercizio per iniziare)", 11, Italic, Gray]
            ]
          ],
          Spacer[6],
          (* Input risposta *)
          Dynamic[
            If[esercizioGenerato,
              Column[{
                Style["Inserisci il testo decifrato:", 12, Bold],
                InputField[Dynamic[rispostaUtente], String,
                  FieldSize -> {30, 2},
                  FieldHint -> "Scrivi il messaggio in chiaro..."]
              }],
              ""
            ]
          ],
          Spacer[6],
          (* Bottoni *)
          Row[{
            Button[
              Style["Verifica Risultato", 13, Bold, White],
              If[esercizioGenerato,
                tentativi++;
                If[ToUpperCase[rispostaUtente] === messaggioChiaro,
                  feedbackMsg = "\[Checkmark] Corretto! Ottimo lavoro! Tentativi usati: " <>
                                ToString[tentativi] <> ".",
                  If[tentativi >= 3,
                    feedbackMsg = "\[Cross] Risposta errata. Tentativi esauriti. Premi 'Mostra Soluzione'.";
                    soluzioneVisibile = True,
                    feedbackMsg = "\[Cross] Non corretto. Tentativo " <>
                                  ToString[tentativi] <> "/3."
                  ]
                ]
              ],
              Background -> RGBColor[0.2, 0.6, 0.3],
              ImageSize -> {170, 35}
            ],
            Spacer[8],
            Button[
              Style["Pulisci Campi", 13, Bold, White],
              seed = 1; messaggioCifrato = ""; chiaveSegreto = "";
              messaggioChiaro = ""; rispostaUtente = "";
              tentativi = 0; feedbackMsg = ""; soluzioneVisibile = False;
              suggerimentoStep = 0; esercizioGenerato = False;,
              Background -> RGBColor[0.65, 0.65, 0.65],
              ImageSize -> {140, 35}
            ]
          }],
          Spacer[4],
          Row[{
            Button[
              Style["Suggerimento", 13, Bold, White],
              If[esercizioGenerato, suggerimentoStep++],
              Background -> RGBColor[0.8, 0.6, 0.1],
              ImageSize -> {140, 35}
            ],
            Spacer[8],
            Button[
              Style["Mostra Soluzione", 13, Bold, White],
              If[esercizioGenerato, soluzioneVisibile = True],
              Background -> RGBColor[0.7, 0.2, 0.2],
              ImageSize -> {160, 35}
            ]
          }],
          Spacer[8],
          (* Feedback *)
          Dynamic[
            If[feedbackMsg =!= "",
              Framed[
                Style[feedbackMsg, 13, Bold,
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.1, 0.5, 0.1], RGBColor[0.6, 0.1, 0.1]]
                ],
                Background ->
                  If[StringStartsQ[feedbackMsg, "\[Checkmark]"],
                     RGBColor[0.9, 1.0, 0.9], RGBColor[1.0, 0.9, 0.9]],
                RoundingRadius -> 5, FrameMargins -> 8
              ],
              ""
            ]
          ],
          (* Suggerimenti progressivi *)
          Dynamic[
            Which[
              !esercizioGenerato || suggerimentoStep == 0, "",
              suggerimentoStep == 1,
                Framed[
                  Style["\[LightBulb] Suggerimento 1: il Cifrario di Vigenere usa una parola chiave \
che viene ripetuta ciclicamente. Prova a identificare pattern ripetuti nel testo cifrato.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep == 2,
                Framed[
                  Style["\[LightBulb] Suggerimento 2: la lunghezza della chiave e' di " <>
                    ToString[StringLength[chiaveSegreto]] <> " lettere.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ],
              suggerimentoStep >= 3,
                Framed[
                  Style["\[LightBulb] Suggerimento 3: la prima lettera della chiave e' '" <>
                    StringTake[chiaveSegreto, 1] <> "'.",
                    12, Italic, RGBColor[0.5, 0.4, 0.0]],
                  Background -> RGBColor[1.0, 0.97, 0.85],
                  RoundingRadius -> 5, FrameMargins -> 8
                ]
            ]
          ],
          (* Soluzione *)
          Dynamic[
            If[soluzioneVisibile && esercizioGenerato,
              Framed[
                Column[{
                  Style["Soluzione:", 13, Bold, RGBColor[0.6, 0.1, 0.1]],
                  Row[{Style["Chiave: ", 12, Bold], Style[chiaveSegreto, 13, Bold]}],
                  Row[{Style["Testo in chiaro: ", 12, Bold], Style[messaggioChiaro, 13, Bold]}]
                }],
                Background -> RGBColor[1.0, 0.93, 0.93],
                RoundingRadius -> 5, FrameStyle -> RGBColor[0.6, 0.1, 0.1], FrameMargins -> 10
              ],
              ""
            ]
          ]
        },
        Alignment -> Left, Spacings -> 1
      ],
      Background -> GrayLevel[0.97],
      ImageSize -> 540
    ]
  ]

(* ============================================================
   FINE SEZIONE PRIVATA
   ============================================================ *)

End[ ]

EndPackage[ ]
