(* ::Package:: *)

(* :Title:            CryptoLab (Package)                                   *)
(* :Context:          Package`                                              *)
(* :Author:           [Nome Gruppo]                                          *)
(* :Summary:          Laboratorio Storico di Crittografia e Quiz Sfida.      *)
(* :Copyright:        [Nome Gruppo] 2026                                     *)
(* :Package Version:  5.0                                                    *)
(* :Mathematica Version: 14                                                  *)

BeginPackage["Package`"]
esercizioUniversale::usage = "Lancia il laboratorio interattivo CryptoLab."
Begin["`Private`"]

(* =========================================================================*)
(* ENGINE CRITTOGRAFICO E STORICO                                           *)
(* =========================================================================*)

pulisciTesto[txt_String] := StringJoin[Select[Characters[ToUpperCase[txt]], LetterQ]]

miCifraCesare[testo_String, chiave_Integer, cifra_] := Module[
  {shift, trasforma, cleanTxt},
  cleanTxt = pulisciTesto[testo];
  shift = If[cifra, chiave, -chiave];
  trasforma = Function[c, FromCharacterCode[Mod[ToCharacterCode[c][[1]] - 65 + shift, 26] + 65]];
  StringJoin[trasforma /@ Characters[cleanTxt]]
]

frequenzeLettere[txt_String] := Module[{counts},
  counts = Counts[Characters[pulisciTesto[txt]]];
  Table[Lookup[counts, FromCharacterCode[65 + i], 0], {i, 0, 25}]
]

frequenzeItaliano = {
  11.74, 0.92, 4.50, 3.73, 11.79, 0.95, 1.64, 1.54, 11.28, 0.00, 
  0.00, 6.51, 2.51, 6.88, 9.83, 3.05, 0.51, 6.37, 4.98, 5.62, 
  3.01, 2.10, 0.00, 0.00, 0.00, 0.49
};

(* =========================================================================*)
(* VISUALIZZAZIONI GRAFICHE                                                 *)
(* =========================================================================*)

disegnaRuotaCesare[chiave_Integer] := Graphics[{
  EdgeForm[Directive[Thick, RGBColor[0.2, 0.2, 0.2]]], FaceForm[RGBColor[0.9, 0.9, 0.95]], Disk[{0, 0}, 2],
  FaceForm[White], Disk[{0, 0}, 1.4],
  FaceForm[RGBColor[0.8, 0.8, 0.8]], Disk[{0, 0}, 0.5],
  
  (* Raggi ruota esterna *)
  Table[Line[{1.4 * {Cos[a], Sin[a]}, 2 * {Cos[a], Sin[a]}}], {a, 0, 2 Pi, 2 Pi / 26}],
  (* Raggi ruota interna *)
  Table[Line[{0.5 * {Cos[a], Sin[a]}, 1.4 * {Cos[a], Sin[a]}}], {a, 0, 2 Pi, 2 Pi / 26}],

  (* Lettere Testo Chiaro (Esterne, fisse) *)
  Table[
    Text[Style[FromCharacterCode[65 + i], 14, Bold, Black], 1.7 * {Cos[a + Pi/26], Sin[a + Pi/26]}],
    {i, 0, 25}, {a, {-(i-6) * 2 Pi / 26}}
  ],
  
  (* Lettere Testo Cifrato (Interne, ruotate in base a K) *)
  Table[
    Text[Style[FromCharacterCode[65 + Mod[i - chiave, 26]], 14, Bold, RGBColor[0.8, 0.2, 0.2]], 
         0.95 * {Cos[a + Pi/26], Sin[a + Pi/26]}],
    {i, 0, 25}, {a, {-(i-6) * 2 Pi / 26}}
  ]
}, ImageSize -> 250, PlotRangePadding -> 0.1]


(* =========================================================================*)
(* INTERFACCIA MAIN GRAPHIC USER INTERFACE                                  *)
(* =========================================================================*)

esercizioUniversale[] := DynamicModule[
  {
    (* Variabili Laboratorio Libero *)
    labTesto = "LE TRUPPE ROMANE ATTACHERANNO LA GALLIA ALLA PRIMA LUCE DELL ALBA",
    labChiaveCesare = 3,
    
    (* Variabili Quiz *)
    quizTesti = {"INFORMATICA", "MATEMATICA", "CRITTOGRAFIA", "ALGORITMO", "UNIVERSITA", "COMPLESSITA"},
    quizRound = 1,
    quizTestoInChiaro = "",
    quizTestoCifrato = "",
    quizChiaveSegreta = 1,
    quizTentativi = 0,
    quizTesterK = 0,
    quizTop3 = {},
    quizRispostaUtente = "",
    quizStato = "Premi INIZIA per generare un'intercettazione nemica.",
    
    generaNuovoQuiz, verificaQuiz, chiediSuggerimento, penalita
  },

  generaNuovoQuiz = Function[{},
    quizTestoInChiaro = RandomChoice[quizTesti];
    quizChiaveSegreta = RandomInteger[{1, 25}];
    quizTestoCifrato = miCifraCesare[quizTestoInChiaro, quizChiaveSegreta, True];
    
    (* Calcola la top 3 delle lettere per gli Hint *)
    quizTop3 = Take[Reverse[SortBy[Tally[Characters[quizTestoInChiaro]], Last]], UpTo[3]];
    
    quizTentativi = 0;
    quizStato = "Messaggio intercettato! Usa la ruota per testare le chiavi.";
    quizRispostaUtente = "";
  ];

  penalita = Function[{messaggioBase},
    quizTentativi++;
    If[quizTentativi >= 4,
      quizStato = "GAME OVER. " <> messaggioBase <> " Troppi tentativi/aiuti usati.\nLa decodifica esatta era: " <> quizTestoInChiaro <> " (K=" <> ToString[quizChiaveSegreta] <> ").";
      quizTestoCifrato = ""; (* blocca game *)
      ,
      quizStato = messaggioBase <> "\nHai ancora " <> ToString[4 - quizTentativi] <> " tentativi a disposizione o aiuti richiedibili.";
    ]
  ];

  verificaQuiz = Function[{},
    If[quizTestoCifrato != "",
      If[StringTrim[ToUpperCase[ToString[quizRispostaUtente]]] === quizTestoInChiaro,
         quizStato = "VITTORIA! Hai decriptato la parola consumando " <> ToString[quizTentativi] <> " aiuti/vite.\nLa chiave era K=" <> ToString[quizChiaveSegreta] <> "!";
         quizTestoCifrato = ""; (* blocca game *)
         ,
         penalita["SBAGLIATO! La parola non compare nei registri."]
      ]
    ]
  ];

  chiediSuggerimento = Function[{},
    If[quizTestoCifrato != "",
      quizTentativi++;
      If[quizTentativi >= 4,
        quizStato = "GAME OVER. Hai superato il limite di aiuti/vite!\nLa decodifica esatta era: " <> quizTestoInChiaro <> " (K=" <> ToString[quizChiaveSegreta] <> ").";
        quizTestoCifrato = ""; (* blocca game *)
        ,
        Module[{hintChar},
          If[quizTentativi == 1,
             hintChar = If[Length[quizTop3] >= 3, quizTop3[[3, 1]], quizTop3[[-1, 1]]];
             quizStato = "HINT 1: Tra le lettere in chiaro, cerca una '" <> hintChar <> "'\n(Ti restano " <> ToString[4 - quizTentativi] <> " vite)";
          ];
          If[quizTentativi == 2,
             hintChar = If[Length[quizTop3] >= 2, quizTop3[[2, 1]], quizTop3[[1, 1]]];
             quizStato = "HINT 2: Una lettera molto presente \[EGrave] la '" <> hintChar <> "'\n(Ti resta " <> ToString[4 - quizTentativi] <> " vita)";
          ];
          If[quizTentativi == 3,
             hintChar = quizTop3[[1, 1]];
             quizStato = "HINT SUPREMO! La lettera pi\[UGrave] frequente in assoluto \[EGrave] la '" <> hintChar <> "'\n(Ultimo appello!)";
          ]
        ]
      ]
    ]
  ];

  Panel[
    TabView[{
      
      (* TAB 1: LABORATORIO LIBERO *)
      "Laboratorio Libero" -> Column[{
        Style["CryptoLab: Laboratorio di Cifratura", Bold, 20, RGBColor[0.2, 0.4, 0.8]],
        Spacer[10],
        Row[{
          Style["Testo in Chiaro: ", Bold],
          InputField[Dynamic[labTesto], String, FieldSize -> {50, 3}]
        }],
        Spacer[15],
        
        Style["Cifrario di Cesare (Sostituzione Monoalfabetica)", Bold, 16],
        Row[{
          Column[{
            Row[{
              Style["Spiazzamento Chiave (K): "],
              InputField[Dynamic[labChiaveCesare], Number, FieldSize -> 5]
            }],
            Spacer[10],
            Row[{
              Style["Testo Cifrato: ", Bold],
              InputField[Dynamic[miCifraCesare[labTesto, labChiaveCesare, True]], String, FieldSize -> {35, 3}, Enabled -> False]
            }]
          }],
          Spacer[30],
          Dynamic@disegnaRuotaCesare[labChiaveCesare]
        }],
        Spacer[15],
        
        Style["Analisi delle Frequenze", Bold, 14],
        Row[{
          BarChart[frequenzeItaliano, ChartLabels -> CharacterRange["A", "Z"], ImageSize -> 350, ChartStyle -> LightBlue, PlotLabel -> "Standard Italiano (%)"],
          Spacer[20],
          Dynamic@BarChart[
            frequenzeLettere[miCifraCesare[labTesto, labChiaveCesare, True]], 
            ChartLabels -> CharacterRange["A", "Z"], 
            ImageSize -> 350, ChartStyle -> "Pastel", 
            PlotLabel -> "Frequenze Testo Cifrato (Assolute)", AxesLabel -> {"", ""}
          ]
        }]
      }],

      (* TAB 2: ROMPI IL CODICE (QUIZ) *)
      "Quiz: Rompi il Codice" -> Column[{
        Style["CryptoSfida: Rompi il Codice di Cesare", Bold, 20, RGBColor[0.7, 0.2, 0.2]],
        Spacer[10],
        Button[" INTERCETTA NUOVO MESSAGGIO ", generaNuovoQuiz[], Background -> LightBlue, BaseStyle -> {Bold, 14}],
        Spacer[15],
        
        Dynamic@Framed[Style[quizStato, Bold, 14, If[StringStartsQ[quizStato, "VITTORIA"], Darker[Green], Darker[Red]]], 
                       Background -> LightYellow, FrameMargins -> 10, RoundingRadius -> 5],
        Spacer[15],
        
        Dynamic@If[quizTestoCifrato != "",
          Column[{
            Style["TESTO CIFRATO:", Bold, 16],
            Style[quizTestoCifrato, Bold, 24, FontFamily -> "Courier New", Blue],
            Spacer[15],
            
            Row[{
              Column[{
                Style["Ruota di Supporto:", Bold, 14],
                Row[{
                  Style["Testa una Chiave K = "], 
                  InputField[Dynamic[quizTesterK], Number, FieldSize -> 5]
                }]
              }, Alignment -> Center],
              Spacer[20],
              Dynamic@disegnaRuotaCesare[quizTesterK]
            }],
            Spacer[15],
            
            Row[{
               Style["Qual \[EGrave] la parola nascosta? "],
               InputField[Dynamic[quizRispostaUtente], String, FieldSize -> {20, 1}],
               Spacer[10],
               Button["Verifica", verificaQuiz[], Background -> RGBColor[0.2, 0.8, 0.4]],
               Spacer[10],
               Button["CHIEDI HINT", chiediSuggerimento[], Background -> RGBColor[0.8, 0.8, 0.2]]
            }]
          }],
          Spacer[0]
        ]
      }]
      
    }],
    Background -> GrayLevel[0.98]
  ]
]

End[]
EndPackage[]
