# Crittografia Arcaica

Laboratorio interattivo di crittografia classica realizzato in Wolfram Language per il corso di Matematica Computazionale, A.A. 2025/2026.

Il progetto guida l'utente nello studio del Cifrario di Cesare e del Cifrario di Vigenere attraverso spiegazioni teoriche, esempi visuali ed esercizi interattivi con feedback immediato.

## Contenuti

- Tutorial guidato in notebook Mathematica.
- Introduzione ai concetti base della crittografia classica.
- Cifratura e decifratura con il Cifrario di Cesare.
- Analisi delle frequenze per comprendere la debolezza del Cifrario di Cesare.
- Ruota di Cesare interattiva per esplorare gli shift.
- Cifratura e decifratura con il Cifrario di Vigenere.
- Esercizi generati tramite seed, quindi riproducibili.
- Suggerimenti progressivi, verifica della risposta e visualizzazione della soluzione.

## Struttura della repository

```text
.
|-- CrittografiaArcaica.m
|-- Laboratorio_Crittografia_Arcaica.nb
`-- README.md
```

### `Laboratorio_Crittografia_Arcaica.nb`

Notebook principale del laboratorio. Contiene il percorso didattico completo:

1. introduzione alla crittografia;
2. Cifrario di Cesare;
3. Cifrario di Vigenere;
4. approfondimenti;
5. bibliografia;
6. commenti e lavoro futuro.

### `CrittografiaArcaica.m`

Package Wolfram Language che implementa la logica del laboratorio:

- cifratura e decifratura dei messaggi;
- generazione degli esercizi;
- calcolo e visualizzazione delle frequenze;
- ruota di Cesare;
- interfacce dinamiche per gli esercizi;
- bottoni per aprire gli esercizi in finestre separate.

Le funzioni pensate per l'uso diretto nel notebook sono:

```wolfram
bottoneEserciziCesare[]
bottoneEserciziVigenere[]
```

## Requisiti

- Wolfram Mathematica 14 o superiore.
- I file `Laboratorio_Crittografia_Arcaica.nb` e `CrittografiaArcaica.m` devono trovarsi nella stessa cartella.
- Disponibilita' del dizionario italiano usato da `DictionaryLookup`.

## Come avviare il laboratorio

1. Clonare la repository:

   ```bash
   git clone https://github.com/francescofuligni/MC-Project.git
   cd MC-Project
   ```

2. Aprire `Laboratorio_Crittografia_Arcaica.nb` con Wolfram Mathematica.

3. Premere il bottone **Avvia il Laboratorio** nel notebook.

   In alternativa, valutare l'intero notebook manualmente da Mathematica.

Il notebook carica il package con:

```wolfram
<< CrittografiaArcaica.m
```

## Uso degli esercizi

Gli esercizi richiedono un seed numerico. A parita' di seed viene generato lo stesso esercizio, rendendo il risultato riproducibile.

### Cifrario di Cesare

L'utente riceve un testo cifrato e deve ricostruire il testo originale. L'interfaccia mette a disposizione:

- verifica della risposta;
- conteggio dei tentativi;
- tre suggerimenti progressivi;
- soluzione esplicita;
- ruota di Cesare interattiva;
- grafico delle frequenze della lingua italiana.

### Cifrario di Vigenere

L'utente riceve un testo cifrato e la chiave usata per cifrarlo. Deve quindi applicare la decifratura sottraendo gli shift determinati dalle lettere della chiave.

Anche questo esercizio include verifica, suggerimenti progressivi e soluzione.

## Scelte implementative

- Il testo viene normalizzato in maiuscolo.
- L'alfabeto gestito e' quello latino `A-Z`.
- Le parole degli esercizi vengono ricavate dal dizionario italiano di Mathematica e filtrate per mantenere solo parole alfabetiche di almeno quattro caratteri.
- Le chiavi di Vigenere sono scelte da una lista fissa di parole italiane semplici, cosi' da mantenere l'esercizio leggibile.
- Le interfacce sono costruite con `DynamicModule`, `Button`, `CreateDocument`, `Deploy`, `Pane`, `TabView` e primitive grafiche Wolfram.

## Limitazioni

- Sono supportate solo lettere non accentate `A-Z`.
- Accenti, lettere estese e alfabeti diversi non sono gestiti come caratteri cifrabili.
- I caratteri non alfabetici vengono preservati nella cifratura, ma non contribuiscono all'avanzamento della chiave di Vigenere.
- Il progetto ha finalita' didattiche e non implementa cifrari sicuri per uso reale.

## Lavoro futuro

Possibili estensioni indicate nel notebook:

- aggiungere una sezione sugli attacchi brute force;
- approfondire l'evoluzione storica dei cifrari a sostituzione;
- introdurre un sistema di punteggio persistente basato su tentativi e suggerimenti usati.

## Autori

Gruppo "I Cesaroni":

- Matteo Boscherini
- Alessandro Campedelli
- Francesco Maria Fuligni
- Mattia Furini
- Mohamed Samir Haffoudhi

## Bibliografia

Il notebook include riferimenti a:

- materiale del corso di crittografia;
- *The Code Book* di Simon Singh;
- introduzione al Wolfram Language;
- documentazione Wolfram per lo sviluppo di package.
