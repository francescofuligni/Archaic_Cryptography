(* ::Package:: *)

(* :Title:            MathInfo                                               *)
(* :Context:          MathInfo`                                              *)
(* :Author:           [Nome Gruppo]                                          *)
(* :Summary:          Quiz interattivo di matematica applicata               *)
(*                    all'informatica. Tre livelli: conversioni numeriche,    *)
(*                    colori esadecimali, crittografia. Ogni livello ha      *)
(*                    3 tentativi, suggerimenti e mostra la soluzione.       *)
(* :Copyright:        [Nome Gruppo] 2026                                     *)
(* :Package Version:  1.0                                                    *)
(* :Mathematica Version: 14                                                  *)
(* :History:          Creato per il corso MC 2025/26                         *)
(* :Keywords:         binario, esadecimale, colori, crittografia, quiz       *)
(* :Warning:          DOCUMENTATE TUTTO il codice                            *)

(* =========================================================================*)
(* SEZIONE PUBBLICA                                                          *)
(* =========================================================================*)

BeginPackage["MathInfo`"]

esercizioUniversale::usage =
  "esercizioUniversale[] lancia l'interfaccia interattiva MathInfo. \
L'utente sceglie il livello (Base, Intermedio, Avanzato) e risponde \
a quiz di matematica applicata all'informatica.";

MathInfo::badinput =
  "Input non valido. Inserisci una risposta nel formato corretto.";

(* =========================================================================*)
(* SEZIONE PRIVATA                                                           *)
(* =========================================================================*)

Begin["`Private`"]

(* =========================================================================*)
(* --- LIVELLO BASE: CONVERSIONI NUMERICHE ---                              *)
(* =========================================================================*)

(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miGeneraEsercizioBase                               *)
(*                                                                           *)
(* Scopo:   Genera un esercizio di conversione numerica. In base al tipo    *)
(*          scelto, produce una domanda su binario<->decimale oppure        *)
(*          su conversioni di unita' di memoria (KB, MB, GB, TB).           *)
(*                                                                           *)
(* Input:   seed_Integer   - intero per SeedRandom                          *)
(*          tipo_String    - "binario" oppure "memoria"                     *)
(*                                                                           *)
(* Lavoro:  n       - numero intero casuale da convertire                   *)
(*          binStr  - rappresentazione binaria come stringa                  *)
(*          unita   - lista delle unita' di memoria disponibili             *)
(*          da, a   - unita' di partenza e arrivo                           *)
(*          valore  - valore numerico da convertire                         *)
(*          fattore - rapporto di conversione (potenza di 1024)             *)
(*                                                                           *)
(* Output:  Association con chiavi:                                         *)
(*          "Testo"     - testo della domanda mostrata all'utente           *)
(*          "Risposta"  - risposta corretta (stringa o numero)              *)
(*          "Soluzione" - spiegazione testuale della soluzione              *)
(*          "Tipo"      - tipo di esercizio per il suggerimento             *)
(* -------------------------------------------------------------------------*)
miGeneraEsercizioBase[seed_Integer, tipo_String] :=
  Module[{n, binStr, unita, da, a, valore, fattore, indDA, indA,
          direzione, risposta, testo, soluzione},
    SeedRandom[seed];
    Which[

      (* TIPO BINARIO: genera un intero in [1,255] e chiede la conversione *)
      tipo === "binario",
        direzione = RandomChoice[{"dec2bin", "bin2dec"}];
        n = RandomInteger[{1, 255}];
        binStr = IntegerString[n, 2];  (* converte in stringa binaria      *)
        If[direzione === "dec2bin",
           testo    = "Converti il numero decimale  " <> ToString[n] <>
                      "  in binario.",
           risposta = binStr;
           soluzione = ToString[n] <> " in binario e' " <> binStr,
        (* else: bin2dec *)
           testo    = "Converti il numero binario  " <> binStr <>
                      "  in decimale.",
           risposta = ToString[n];
           soluzione = binStr <> " in decimale e' " <> ToString[n]
        ];
        <| "Testo"     -> testo,
           "Risposta"  -> risposta,
           "Soluzione" -> soluzione,
           "Tipo"      -> "binario",
           "Dir"       -> direzione,
           "Valore"    -> n |>,

      (* TIPO MEMORIA: sceglie due unita' diverse e un valore casuale      *)
      tipo === "memoria",
        unita  = {"KB", "MB", "GB", "TB"};
        indDA  = RandomInteger[{1, 3}];     (* indice unita' di partenza  *)
        indA   = indDA + 1;                 (* unita' sempre superiore    *)
        da     = unita[[indDA]];
        a      = unita[[indA]];
        valore = RandomChoice[{128, 256, 512, 1024, 2048, 4096}];
        (* ogni passaggio di unita' divide per 1024                       *)
        fattore = 1024^(indA - indDA);
        risposta = ToString[N[valore / fattore]];
        testo    = "Converti  " <> ToString[valore] <> " " <> da <>
                   "  in " <> a <> ".";
        soluzione = ToString[valore] <> " " <> da <> " = " <>
                    ToString[N[valore / fattore]] <> " " <> a;
        <| "Testo"     -> testo,
           "Risposta"  -> risposta,
           "Soluzione" -> soluzione,
           "Tipo"      -> "memoria",
           "Valore"    -> valore,
           "Da"        -> da,
           "A"         -> a |>
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miSuggerimentoBase                                  *)
(*                                                                           *)
(* Scopo:   Restituisce un suggerimento testuale adatto al tipo di          *)
(*          esercizio base e alla direzione della conversione.              *)
(*                                                                           *)
(* Input:   esercizio_Association - Association da miGeneraEsercizioBase    *)
(*          tentativo_Integer     - numero del tentativo corrente (1,2,3)   *)
(*                                                                           *)
(* Output:  stringa con il suggerimento                                     *)
(* -------------------------------------------------------------------------*)
miSuggerimentoBase[esercizio_Association, tentativo_Integer] :=
  Module[{tipo, dir, v},
    tipo = esercizio["Tipo"];
    Which[
      tipo === "binario",
        dir = esercizio["Dir"];
        v   = esercizio["Valore"];
        Which[
          tentativo === 1 && dir === "dec2bin",
            "Suggerimento: dividi ripetutamente per 2 e leggi i resti \
dal basso verso l'alto.",
          tentativo === 2 && dir === "dec2bin",
            "Suggerimento: " <> ToString[v] <> " = " <>
            StringRiffle[
              ToString /@ Reverse[IntegerDigits[v, 2]], " * 2^? + ..."],
          tentativo === 1 && dir === "bin2dec",
            "Suggerimento: moltiplica ogni cifra per 2 elevato alla \
sua posizione (da destra, partendo da 0).",
          True,
            "Suggerimento: la risposta corretta e' " <>
            esercizio["Risposta"]
        ],
      tipo === "memoria",
        Which[
          tentativo === 1,
            "Suggerimento: ogni unita' e' 1024 volte quella precedente \
(1 GB = 1024 MB).",
          tentativo === 2,
            "Suggerimento: dividi " <> ToString[esercizio["Valore"]] <>
            " per 1024 per passare da " <> esercizio["Da"] <>
            " a " <> esercizio["A"] <> ".",
          True,
            "La risposta corretta e' " <> esercizio["Risposta"]
        ]
    ]
  ]


(* =========================================================================*)
(* --- LIVELLO INTERMEDIO: COLORI ESADECIMALI ---                           *)
(* =========================================================================*)

(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miGeneraColore                                      *)
(*                                                                           *)
(* Scopo:   Genera un colore RGB casuale e la sua rappresentazione          *)
(*          esadecimale nel formato #RRGGBB.                                *)
(*                                                                           *)
(* Input:   seed_Integer - intero per SeedRandom                            *)
(*                                                                           *)
(* Lavoro:  r, g, b   - valori RGB interi in [0, 255]                      *)
(*          toHex2     - funzione pura che converte intero in 2 cifre hex   *)
(*          hexStr     - stringa "#RRGGBB"                                  *)
(*                                                                           *)
(* Output:  Association con "R","G","B","HEX","Colore" (oggetto RGBColor)  *)
(* -------------------------------------------------------------------------*)
miGeneraColore[seed_Integer] :=
  Module[{r, g, b, toHex2, hexStr},
    SeedRandom[seed];
    r = RandomInteger[{0, 255}];
    g = RandomInteger[{0, 255}];
    b = RandomInteger[{0, 255}];
    (* converte intero in stringa esadecimale di esattamente 2 cifre       *)
    toHex2 = IntegerString[#, 16, 2] &;
    hexStr = "#" <> ToUpperCase[toHex2[r]] <>
                    ToUpperCase[toHex2[g]] <>
                    ToUpperCase[toHex2[b]];
    <| "R"      -> r,
       "G"      -> g,
       "B"      -> b,
       "HEX"    -> hexStr,
       "Colore" -> RGBColor[r/255., g/255., b/255.] |>
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miGeneraEsercizioColore                             *)
(*                                                                           *)
(* Scopo:   Genera un esercizio sul colore esadecimale. In base alla        *)
(*          modalita', chiede all'utente di:                                *)
(*          - "hex2col": scrivere il codice HEX dato il colore mostrato    *)
(*          - "col2hex": scegliere il colore tra 4 opzioni dato il codice  *)
(*                                                                           *)
(* Input:   seed_Integer    - intero per SeedRandom                         *)
(*          modalita_String - "hex2col" oppure "col2hex"                    *)
(*                                                                           *)
(* Lavoro:  coloreGiusto - Association del colore corretto                  *)
(*          distrattori  - lista di 3 colori sbagliati                      *)
(*          opzioni      - lista mista ordinata casualmente (per col2hex)   *)
(*                                                                           *)
(* Output:  Association con "Modalita'","Colore","Opzioni","Risposta",      *)
(*          "Testo","Soluzione"                                              *)
(* -------------------------------------------------------------------------*)
miGeneraEsercizioColore[seed_Integer, modalita_String] :=
  Module[{coloreGiusto, distrattori, opzioni, testo, risposta, soluzione},
    SeedRandom[seed];
    coloreGiusto = miGeneraColore[seed];
    Which[

      (* MODALITA' hex2col: mostra il colore, l'utente scrive il codice HEX*)
      modalita === "hex2col",
        testo    = "Osserva il colore qui sotto e scrivi il suo codice \
esadecimale nel formato #RRGGBB.";
        risposta = coloreGiusto["HEX"];
        soluzione = "Il codice HEX del colore e' " <> coloreGiusto["HEX"] <>
                    "  (R=" <> ToString[coloreGiusto["R"]] <>
                    ", G=" <> ToString[coloreGiusto["G"]] <>
                    ", B=" <> ToString[coloreGiusto["B"]] <> ")";
        <| "Modalita'" -> "hex2col",
           "Colore"    -> coloreGiusto,
           "Opzioni"   -> {},
           "Risposta"  -> risposta,
           "Testo"     -> testo,
           "Soluzione" -> soluzione |>,

      (* MODALITA' col2hex: mostra il codice HEX, l'utente sceglie il colore*)
      modalita === "col2hex",
        (* genera 3 colori distrattori con seed diversi                    *)
        distrattori = Table[miGeneraColore[seed + k], {k, 1, 3}];
        (* mescola il colore giusto con i distrattori                      *)
        opzioni = RandomSample[Prepend[distrattori, coloreGiusto]];
        testo   = "Quale dei 4 colori corrisponde al codice  " <>
                  coloreGiusto["HEX"] <> " ?";
        risposta = coloreGiusto["HEX"];
        soluzione = "Il colore corretto e' quello con codice " <>
                    coloreGiusto["HEX"] <>
                    "  (R=" <> ToString[coloreGiusto["R"]] <>
                    ", G=" <> ToString[coloreGiusto["G"]] <>
                    ", B=" <> ToString[coloreGiusto["B"]] <> ")";
        <| "Modalita'" -> "col2hex",
           "Colore"    -> coloreGiusto,
           "Opzioni"   -> opzioni,
           "Risposta"  -> risposta,
           "Testo"     -> testo,
           "Soluzione" -> soluzione |>
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miSuggerimentoColore                                *)
(*                                                                           *)
(* Scopo:   Restituisce un suggerimento per l'esercizio sul colore          *)
(*          esadecimale, crescente in dettaglio al crescere del tentativo.  *)
(*                                                                           *)
(* Input:   esercizio_Association - Association da miGeneraEsercizioColore  *)
(*          tentativo_Integer     - numero del tentativo corrente           *)
(*                                                                           *)
(* Output:  stringa con il suggerimento                                     *)
(* -------------------------------------------------------------------------*)
miSuggerimentoColore[esercizio_Association, tentativo_Integer] :=
  Module[{col, mod},
    col = esercizio["Colore"];
    mod = esercizio["Modalita'"];
    Which[
      tentativo === 1,
        "Suggerimento: un codice HEX ha la forma #RRGGBB dove ogni \
coppia e' un numero esadecimale da 00 a FF (0-255 in decimale).",
      tentativo === 2 && mod === "hex2col",
        "Suggerimento: il rosso vale " <> ToString[col["R"]] <>
        ", il verde vale " <> ToString[col["G"]] <>
        ", il blu vale " <> ToString[col["B"]] <> ".",
      tentativo === 2 && mod === "col2hex",
        "Suggerimento: cerca il colore con componente rossa ~" <>
        ToString[col["R"]] <> ".",
      True,
        "La risposta corretta e' " <> esercizio["Risposta"]
    ]
  ]


(* =========================================================================*)
(* --- LIVELLO AVANZATO: CRITTOGRAFIA ---                                   *)
(* =========================================================================*)

(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miCifraCesare                                       *)
(*                                                                           *)
(* Scopo:   Cifra o decifra una stringa con il Cifrario di Cesare.          *)
(*          Solo lettere maiuscole A-Z vengono spostate; gli altri          *)
(*          caratteri restano invariati.                                    *)
(*                                                                           *)
(* Input:   testo_String  - stringa da cifrare/decifrare                    *)
(*          chiave_Integer - spostamento (0-25)                             *)
(*          cifra_        - True per cifrare, False per decifrare           *)
(*                                                                           *)
(* Lavoro:  shift  - spostamento effettivo (negato se si decifra)          *)
(*          trasforma - funzione pura applicata a ogni carattere            *)
(*                                                                           *)
(* Output:  stringa cifrata o decifrata                                     *)
(* -------------------------------------------------------------------------*)
miCifraCesare[testo_String, chiave_Integer, cifra_] :=
  Module[{shift, trasforma},
    shift = If[cifra, chiave, -chiave];
    (* applica lo shift solo alle lettere A-Z (ASCII 65-90)               *)
    trasforma = Function[c,
      If[65 <= ToCharacterCode[c][[1]] <= 90,
         FromCharacterCode[Mod[ToCharacterCode[c][[1]] - 65 + shift, 26] + 65],
         c
      ]
    ];
    StringJoin[trasforma /@ Characters[ToUpperCase[testo]]]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miCifraVigenere                                     *)
(*                                                                           *)
(* Scopo:   Cifra o decifra una stringa con il Cifrario di Vigenere.        *)
(*          La chiave e' una parola; ogni lettera del testo viene spostata  *)
(*          del valore della lettera corrispondente della chiave (ciclica). *)
(*                                                                           *)
(* Input:   testo_String  - stringa di sole lettere maiuscole               *)
(*          chiave_String - parola chiave (es. "CHIAVE")                   *)
(*          cifra_        - True per cifrare, False per decifrare           *)
(*                                                                           *)
(* Lavoro:  chars     - lista di caratteri del testo                        *)
(*          keyChars  - lista ciclica dei codici ASCII della chiave        *)
(*          shifts    - lista degli spostamenti per ogni carattere          *)
(*                                                                           *)
(* Output:  stringa cifrata o decifrata                                     *)
(* -------------------------------------------------------------------------*)
miCifraVigenere[testo_String, chiave_String, cifra_] :=
  Module[{chars, keyChars, n, shifts, risultato},
    chars   = Characters[ToUpperCase[testo]];
    keyChars = ToCharacterCode[ToUpperCase[chiave]];
    n       = Length[keyChars];
    (* costruisce la lista degli shift ciclici sulla chiave               *)
    shifts = Table[
      If[cifra,
         keyChars[[Mod[i-1, n] + 1]] - 65,
        -(keyChars[[Mod[i-1, n] + 1]] - 65)
      ],
      {i, 1, Length[chars]}
    ];
    (* applica ogni shift alla lettera corrispondente                     *)
    risultato = MapThread[
      Function[{c, s},
        If[65 <= ToCharacterCode[c][[1]] <= 90,
           FromCharacterCode[Mod[ToCharacterCode[c][[1]] - 65 + s, 26] + 65],
           c
        ]
      ],
      {chars, shifts}
    ];
    StringJoin[risultato]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miGeneraEsercizioAvanzato                           *)
(*                                                                           *)
(* Scopo:   Genera un esercizio di decrittografia. Mostra all'utente un    *)
(*          messaggio cifrato e chiede di trovare la chiave.                *)
(*                                                                           *)
(* Input:   seed_Integer    - intero per SeedRandom                         *)
(*          cifrario_String - "cesare" oppure "vigenere"                    *)
(*                                                                           *)
(* Lavoro:  frasi    - lista di frasi brevi in chiaro da cifrare            *)
(*          frase    - frase scelta casualmente                              *)
(*          chiave   - chiave scelta casualmente                            *)
(*          cifrato  - testo dopo la cifratura                              *)
(*                                                                           *)
(* Output:  Association con "Cifrario","Testo","Cifrato","Chiave",          *)
(*          "Risposta","Soluzione"                                           *)
(* -------------------------------------------------------------------------*)
miGeneraEsercizioAvanzato[seed_Integer, cifrario_String] :=
  Module[{frasi, frase, chiave, chiaveStr, cifrato, testo, soluzione},
    SeedRandom[seed];
    frasi = {"MATEMATICA", "INFORMATICA", "ALGORITMO", "BINARIO",
             "CRITTOGRAFIA", "SICUREZZA", "CODIFICA", "PROTOCOLLO"};
    frase = RandomChoice[frasi];
    Which[

      (* CESARE: chiave intera in [1, 25]                                 *)
      cifrario === "cesare",
        chiave    = RandomInteger[{1, 25}];
        chiaveStr = ToString[chiave];
        cifrato   = miCifraCesare[frase, chiave, True];
        testo     = "Il seguente messaggio e' stato cifrato con il \
Cifrario di Cesare:\n\n  " <> cifrato <>
                    "\n\nTrova la chiave (un numero intero da 1 a 25).";
        soluzione = "La chiave e' " <> chiaveStr <>
                    ". Il messaggio in chiaro e' \"" <> frase <> "\".";
        <| "Cifrario"  -> "cesare",
           "Testo"     -> testo,
           "Frase"     -> frase,
           "Cifrato"   -> cifrato,
           "Chiave"    -> chiave,
           "Risposta"  -> chiaveStr,
           "Soluzione" -> soluzione |>,

      (* VIGENERE: chiave parola scelta da lista breve                    *)
      cifrario === "vigenere",
        chiave    = RandomChoice[{"ALPHA", "BETA", "GAMMA", "DELTA",
                                  "OMEGA", "SIGMA"}];
        chiaveStr = chiave;
        cifrato   = miCifraVigenere[frase, chiave, True];
        testo     = "Il seguente messaggio e' stato cifrato con il \
Cifrario di Vigenere:\n\n  " <> cifrato <>
                    "\n\nTrova la chiave (una parola tra: ALPHA, BETA, \
GAMMA, DELTA, OMEGA, SIGMA).";
        soluzione = "La chiave e' " <> chiaveStr <>
                    ". Il messaggio in chiaro e' \"" <> frase <> "\".";
        <| "Cifrario"  -> "vigenere",
           "Testo"     -> testo,
           "Frase"     -> frase,
           "Cifrato"   -> cifrato,
           "Chiave"    -> chiave,
           "Risposta"  -> chiaveStr,
           "Soluzione" -> soluzione |>
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miSuggerimentoAvanzato                              *)
(*                                                                           *)
(* Scopo:   Restituisce un suggerimento per l'esercizio di crittografia,   *)
(*          sempre piu' dettagliato al crescere del tentativo.              *)
(*                                                                           *)
(* Input:   esercizio_Association - Association da miGeneraEsercizioAvanzato*)
(*          tentativo_Integer     - numero del tentativo corrente           *)
(*                                                                           *)
(* Output:  stringa con il suggerimento                                     *)
(* -------------------------------------------------------------------------*)
miSuggerimentoAvanzato[esercizio_Association, tentativo_Integer] :=
  Module[{cif, chiave, cifrato},
    cif    = esercizio["Cifrario"];
    chiave = esercizio["Chiave"];
    cifrato = esercizio["Cifrato"];
    Which[
      tentativo === 1 && cif === "cesare",
        "Suggerimento: prova a decifrare la prima lettera \"" <>
        StringTake[cifrato, 1] <> "\" con diversi shift da 1 a 25.",
      tentativo === 2 && cif === "cesare",
        "Suggerimento: la chiave e' un numero tra 1 e 25. " <>
        "Prova con valori vicino a " <> ToString[Round[chiave/2]] <> ".",
      tentativo === 1 && cif === "vigenere",
        "Suggerimento: il Cifrario di Vigenere usa una parola chiave. \
Prova ogni parola della lista e decifra le prime lettere.",
      tentativo === 2 && cif === "vigenere",
        "Suggerimento: la chiave inizia con la lettera \"" <>
        StringTake[ToString[chiave], 1] <> "\".",
      True,
        "La risposta corretta e' " <> ToString[esercizio["Risposta"]]
    ]
  ]


(* -------------------------------------------------------------------------*)
(* FUNZIONE AUSILIARIA: miVerificaRisposta                                  *)
(*                                                                           *)
(* Scopo:   Confronta la risposta dell'utente con quella corretta.          *)
(*          Per il livello base/avanzato: confronto stringa case-insensitive*)
(*          Per il colore hex2col: confronto con tolleranza su HEX          *)
(*                                                                           *)
(* Input:   rispostaUtente_String - testo inserito dall'utente              *)
(*          rispostaCorretta_     - stringa della risposta corretta         *)
(*                                                                           *)
(* Output:  True se corretta, False altrimenti                              *)
(* -------------------------------------------------------------------------*)
miVerificaRisposta[rispostaUtente_String, rispostaCorretta_] :=
  ToUpperCase[StringTrim[rispostaUtente]] ===
  ToUpperCase[StringTrim[ToString[rispostaCorretta]]]

miVerificaRisposta[_, _] := False


(* =========================================================================*)
(* FUNZIONE PRINCIPALE (PUBBLICA): esercizioUniversale                     *)
(*                                                                           *)
(* Scopo:   Genera e visualizza l'interfaccia interattiva con DynamicModule.*)
(*          L'utente sceglie il livello e la modalita', inserisce il Seed  *)
(*          e risponde alle domande. Implementa le 5 funzionalita':         *)
(*            1. Seed           - InputField                                *)
(*            2. Genera Esercizio - Button                                  *)
(*            3. Verifica Risultato - Button (3 tentativi)                  *)
(*            4. Mostra Soluzione - Button -> MessageDialog                 *)
(*            5. Pulisci Campi  - Button                                    *)
(*                                                                           *)
(*          Font Arial ovunque per accessibilita' e leggibilita'.           *)
(*                                                                           *)
(* Input:   nessuno                                                          *)
(* Output:  oggetto DynamicModule                                           *)
(* =========================================================================*)
esercizioUniversale[] :=
  DynamicModule[
    (* Variabili di stato \[LongDash] tutte locali al DynamicModule                 *)
    {
      stSeed       = 42,       (* seed corrente                           *)
      stLivello    = "Base",   (* "Base", "Intermedio", "Avanzato"        *)
      stModalita   = "binario",(* modalita' interna al livello            *)
      stEsercizio  = None,     (* Association dell'esercizio corrente     *)
      stRisposta   = "",       (* stringa inserita dall'utente            *)
      stFeedback   = "",       (* messaggio di esito verifica             *)
      stTentativi  = 0,        (* numero di tentativi usati               *)
      stOn         = False,    (* True dopo Genera Esercizio              *)
      stColoreScelta = None    (* colore selezionato in modalita' col2hex *)
    },

    Column[{

      (* TITOLO *)
      Style["MathInfo", Bold, 22, FontFamily -> "Arial",
            RGBColor[0.1, 0.2, 0.6]],
      Style["Quiz di Matematica Applicata all'Informatica",
            Italic, 12, FontFamily -> "Arial", Gray],
      Spacer[14],

      (* SELEZIONE LIVELLO con RadioButtonBar                              *)
      Row[{
        Style["Livello:  ", Bold, 12, FontFamily -> "Arial"],
        RadioButtonBar[
          Dynamic[stLivello,
            (* al cambio livello imposta la modalita' di default          *)
            (stLivello = #;
             stModalita = Switch[#,
               "Base",       "binario",
               "Intermedio", "hex2col",
               "Avanzato",   "cesare"
             ];
             stOn = False; stFeedback = ""; stRisposta = "";
             stTentativi = 0; stEsercizio = None) &
          ],
          {"Base" -> Style["Base", FontFamily -> "Arial", 11],
           "Intermedio" -> Style["Intermedio", FontFamily -> "Arial", 11],
           "Avanzato"   -> Style["Avanzato",   FontFamily -> "Arial", 11]}
        ]
      }],
      Spacer[6],

      (* SELEZIONE MODALITA' (dipende dal livello scelto)                 *)
      Dynamic[
        Row[{
          Style["Modalita':  ", Bold, 12, FontFamily -> "Arial"],
          Which[
            stLivello === "Base",
              RadioButtonBar[
                Dynamic[stModalita],
                {"binario" -> Style["Binario \[LeftRightArrow] Decimale",
                               FontFamily -> "Arial", 11],
                 "memoria" -> Style["Conversioni di memoria",
                               FontFamily -> "Arial", 11]}
              ],
            stLivello === "Intermedio",
              RadioButtonBar[
                Dynamic[stModalita],
                {"hex2col" -> Style["Colore \[RightArrow] Codice HEX",
                               FontFamily -> "Arial", 11],
                 "col2hex" -> Style["Codice HEX \[RightArrow] Colore",
                               FontFamily -> "Arial", 11]}
              ],
            stLivello === "Avanzato",
              RadioButtonBar[
                Dynamic[stModalita],
                {"cesare"   -> Style["Cifrario di Cesare",
                                FontFamily -> "Arial", 11],
                 "vigenere" -> Style["Cifrario di Vigenere",
                                FontFamily -> "Arial", 11]}
              ]
          ]
        }]
      ],
      Spacer[8],

      (* RIGA SEED *)
      Row[{
        Style["Seed:  ", Bold, 12, FontFamily -> "Arial"],
        InputField[Dynamic[stSeed], Number,
                   FieldSize -> 6, FieldHint -> "es. 42"]
      }],
      Spacer[8],

      (* BOTTONE 1: Genera Esercizio                                       *)
      Button[
        Style[" \[FilledRightTriangle]  Genera Esercizio ",
              White, Bold, 12, FontFamily -> "Arial"],
        Module[{sv},
          sv = Quiet[Check[Round[stSeed], 42]];
          stEsercizio = Which[
            stLivello === "Base",
              miGeneraEsercizioBase[sv, stModalita],
            stLivello === "Intermedio",
              miGeneraEsercizioColore[sv, stModalita],
            stLivello === "Avanzato",
              miGeneraEsercizioAvanzato[sv, stModalita]
          ];
          stRisposta      = "";
          stFeedback      = "";
          stTentativi     = 0;
          stColoreScelta  = None;
          stOn            = True
        ],
        Background -> RGBColor[0.1, 0.5, 0.1], FrameMargins -> 8
      ],
      Spacer[12],

      (* TESTO DELLA DOMANDA + ELEMENTO VISIVO                            *)
      Dynamic[
        If[stOn && stEsercizio =!= None,
           Column[{
             (* testo domanda su sfondo giallo                            *)
             Framed[
               Style[stEsercizio["Testo"], 12, FontFamily -> "Arial"],
               Background -> LightYellow, FrameStyle -> Orange,
               RoundingRadius -> 5, FrameMargins -> 12,
               ImageSize -> {500, Automatic}
             ],
             Spacer[10],
             (* elemento visivo specifico per livello                     *)
             Which[
               (* BASE: nessun elemento visivo aggiuntivo                *)
               stLivello === "Base",
                 Spacer[0],

               (* INTERMEDIO hex2col: mostra il quadrato di colore        *)
               stLivello === "Intermedio" && stEsercizio["Modalita'"] === "hex2col",
                 Graphics[{stEsercizio["Colore"]["Colore"],
                   Rectangle[{0,0},{4,4}]}, ImageSize -> 120,
                   Frame -> True, FrameStyle -> Gray],

               (* INTERMEDIO col2hex: mostra 4 quadrati selezionabili    *)
               stLivello === "Intermedio" && stEsercizio["Modalita'"] === "col2hex",
                 Row[
                   Table[
                     Module[{opt = stEsercizio["Opzioni"][[k]]},
                       EventHandler[
                         Framed[
                           Graphics[{opt["Colore"], Rectangle[{0,0},{3,3}]},
                             ImageSize -> 90, Frame -> True,
                             FrameStyle ->
                               Dynamic[If[stColoreScelta === opt["HEX"],
                                 Orange, Gray]]
                           ],
                           FrameStyle ->
                             Dynamic[If[stColoreScelta === opt["HEX"],
                               {Thick, Orange}, LightGray]],
                           RoundingRadius -> 4
                         ],
                         {"MouseClicked" :>
                           (stColoreScelta = opt["HEX"];
                            stRisposta = opt["HEX"])}
                       ]
                     ],
                     {k, 1, 4}
                   ],
                   Spacer[8]
                 ],

               (* AVANZATO: nessun elemento visivo aggiuntivo            *)
               True, Spacer[0]
             ]
           }],
           Framed[
             Style["Scegli livello, modalita' e premi Genera Esercizio.",
                   Gray, Italic, 11, FontFamily -> "Arial"],
             FrameStyle -> LightGray, RoundingRadius -> 4,
             FrameMargins -> 12, ImageSize -> {500, 50}, Alignment -> Center
           ]
        ]
      ],
      Spacer[8],

      (* CAMPO RISPOSTA (non mostrato per col2hex: si clicca sul colore)  *)
      Dynamic[
        If[stOn && stEsercizio =!= None &&
           !(stLivello === "Intermedio" &&
             stEsercizio["Modalita'"] === "col2hex"),
           Row[{
             Style["La tua risposta:  ", Bold, 12, FontFamily -> "Arial"],
             InputField[Dynamic[stRisposta], String,
                        FieldSize -> 14,
                        FieldHint -> Switch[stLivello,
                          "Base",       "es. 42  oppure  101010",
                          "Intermedio", "es. #FF8C00",
                          "Avanzato",   "es. 13  oppure  ALPHA"
                        ]]
           }],
           ""
        ]
      ],
      Spacer[6],

      (* INDICATORE TENTATIVI RIMASTI                                     *)
      Dynamic[
        If[stOn,
           Style["Tentativi rimasti: " <> ToString[3 - stTentativi] <> " / 3",
                 Bold, 11, FontFamily -> "Arial",
                 If[stTentativi < 2, Darker[Green], Darker[Red]]],
           ""
        ]
      ],
      Spacer[10],

      (* RIGA BOTTONI AZIONE                                              *)
      Dynamic[
        If[stOn,
           Row[{

             (* BOTTONE 2: Verifica Risultato                              *)
             Button[
               Style[" \[Checkmark]  Verifica Risultato ",
                     White, Bold, 12, FontFamily -> "Arial"],
               Module[{esito},
                 If[stTentativi >= 3,
                    stFeedback = "\[WarningSign]  Tentativi esauriti. \
Usa Mostra Soluzione.",
                    If[StringTrim[stRisposta] === "",
                       stFeedback = "\[WarningSign]  Inserisci una risposta.",
                       esito = miVerificaRisposta[stRisposta,
                                                   stEsercizio["Risposta"]];
                       stTentativi++;
                       If[esito,
                          stFeedback = "\[Checkmark]  Corretto! Ottimo lavoro.",
                          (* risposta errata: mostra suggerimento          *)
                          stFeedback = "\[Cross]  Non corretto.  " <>
                            Which[
                              stLivello === "Base",
                                miSuggerimentoBase[stEsercizio, stTentativi],
                              stLivello === "Intermedio",
                                miSuggerimentoColore[stEsercizio, stTentativi],
                              True,
                                miSuggerimentoAvanzato[stEsercizio, stTentativi]
                            ]
                       ]
                    ]
                 ]
               ],
               Background -> RGBColor[0.1, 0.35, 0.7], FrameMargins -> 6
             ],
             Spacer[10],

             (* BOTTONE 3: Mostra Soluzione -> MessageDialog              *)
             Button[
               Style[" \[FilledSmallSquare]  Mostra Soluzione ",
                     White, Bold, 12, FontFamily -> "Arial"],
               MessageDialog[
                 Column[{
                   Style["Soluzione", Bold, 15, Blue, FontFamily -> "Arial"],
                   Spacer[6],
                   Style[stEsercizio["Soluzione"], 12, FontFamily -> "Arial"],
                   (* per il livello intermedio mostra anche il colore    *)
                   If[stLivello === "Intermedio",
                      Column[{
                        Spacer[8],
                        Graphics[{stEsercizio["Colore"]["Colore"],
                          Rectangle[{0,0},{3,3}]}, ImageSize -> 80,
                          Frame -> True]
                      }],
                      Spacer[0]
                   ]
                 }]
               ],
               Background -> RGBColor[0.5, 0.1, 0.6], FrameMargins -> 6
             ],
             Spacer[10],

             (* BOTTONE 4: Pulisci Campi                                  *)
             Button[
               Style[" \[FilledSquare]  Pulisci Campi ",
                     White, Bold, 12, FontFamily -> "Arial"],
               stEsercizio     = None;
               stRisposta      = "";
               stFeedback      = "";
               stTentativi     = 0;
               stOn            = False;
               stColoreScelta  = None,
               Background -> RGBColor[0.45, 0.45, 0.45], FrameMargins -> 6
             ]
           }],
           ""
        ]
      ],
      Spacer[8],

      (* FEEDBACK: verde se corretto, rosso se errato                     *)
      Dynamic[
        If[stFeedback =!= "",
           Framed[
             Style[stFeedback, Bold, 12, FontFamily -> "Arial",
               If[StringContainsQ[stFeedback, "\[Checkmark]"],
                  Darker[Green], Darker[Red]]
             ],
             Background ->
               If[StringContainsQ[stFeedback, "\[Checkmark]"],
                  LightGreen, LightRed],
             FrameStyle ->
               If[StringContainsQ[stFeedback, "\[Checkmark]"],
                  Green, Red],
             RoundingRadius -> 4, FrameMargins -> 10,
             ImageSize -> {500, Automatic}
           ],
           ""
        ]
      ]

    }, Alignment -> Left, Spacings -> 0]

  ] (* fine DynamicModule *)


End[]        (* chiude MathInfo`Private` *)

EndPackage[] (* chiude MathInfo` e ripristina $ContextPath *)
