# Archaic Cryptography

Interactive classical cryptography laboratory built with Wolfram Language for the Computational Mathematics course, academic year 2025/2026.

The project guides users through Caesar cipher and Vigenere cipher concepts with theoretical explanations, visual examples, and interactive exercises with immediate feedback.

## Contents

- Guided Mathematica notebook tutorial.
- Introduction to basic classical cryptography concepts.
- Encryption and decryption with the Caesar cipher.
- Frequency analysis to understand the weakness of the Caesar cipher.
- Interactive Caesar wheel for exploring shifts.
- Encryption and decryption with the Vigenere cipher.
- Seed-based exercises, making each exercise reproducible.
- Progressive hints, answer checking, and solution reveal.

## Repository Structure

```text
.
|-- CrittografiaArcaica.m
|-- Laboratorio_Crittografia_Arcaica.nb
`-- README.md
```

### `Laboratorio_Crittografia_Arcaica.nb`

Main laboratory notebook. It contains the complete learning path:

1. introduction to cryptography;
2. Caesar cipher;
3. Vigenere cipher;
4. further reading;
5. bibliography;
6. comments and future work.

### `CrittografiaArcaica.m`

Wolfram Language package implementing the laboratory logic:

- message encryption and decryption;
- exercise generation;
- frequency calculation and visualization;
- Caesar wheel;
- dynamic exercise interfaces;
- buttons for opening exercises in separate windows.

The functions intended for direct use inside the notebook are:

```wolfram
bottoneEserciziCesare[]
bottoneEserciziVigenere[]
```

## Requirements

- Wolfram Mathematica 14 or later.
- `Laboratorio_Crittografia_Arcaica.nb` and `CrittografiaArcaica.m` must be in the same folder.
- Availability of the Italian dictionary used by `DictionaryLookup`.

## How to Run

1. Clone the repository:

   ```bash
   git clone https://github.com/francescofuligni/MC-Project.git
   cd MC-Project
   ```

2. Open `Laboratorio_Crittografia_Arcaica.nb` with Wolfram Mathematica.

3. Click the **Avvia il Laboratorio** button inside the notebook.

   Alternatively, evaluate the entire notebook manually from Mathematica.

The notebook loads the package with:

```wolfram
<< CrittografiaArcaica.m
```

## Exercise Workflow

Exercises require a numeric seed. The same seed always generates the same exercise, making the result reproducible.

### Caesar Cipher

The user receives an encrypted text and must reconstruct the original plaintext. The interface provides:

- answer checking;
- attempt counter;
- three progressive hints;
- explicit solution reveal;
- interactive Caesar wheel;
- Italian letter-frequency chart.

### Vigenere Cipher

The user receives an encrypted text and the key used to encrypt it. To decrypt the text, the user subtracts the shifts determined by the letters of the key.

This exercise also includes answer checking, progressive hints, and solution reveal.

## Implementation Notes

- Text is normalized to uppercase.
- The supported alphabet is the Latin `A-Z` alphabet.
- Exercise words are taken from Mathematica's Italian dictionary and filtered to keep only alphabetic words with at least four characters.
- Vigenere keys are selected from a fixed list of simple Italian words, keeping the exercise readable.
- Interfaces are built with `DynamicModule`, `Button`, `CreateDocument`, `Deploy`, `Pane`, `TabView`, and Wolfram graphics primitives.

## Limitations

- Only unaccented `A-Z` letters are supported.
- Accents, extended letters, and other alphabets are not handled as encryptable characters.
- Non-alphabetic characters are preserved during encryption, but they do not advance the Vigenere key.
- The project is educational and does not implement cryptographic methods suitable for real-world security.

## Future Work

Possible extensions listed in the notebook:

- add a section about brute-force attacks;
- expand the historical evolution of substitution ciphers;
- introduce a persistent scoring system based on attempts and hints used.

## Authors

Group "I Cesaroni":

- Matteo Boscherini
- Alessandro Campedelli
- Francesco Maria Fuligni
- Mattia Furini
- Mohamed Samir Haffoudhi

## Bibliography

The notebook includes references to:

- course material about cryptography;
- *The Code Book* by Simon Singh;
- an introduction to Wolfram Language;
- Wolfram documentation for package development.
