# SandScout

SandScout is a framework to extract, decompile, formally model, and analyze iOS sandbox profiles as logic-based programs. The authors used Prolog-based queries to evaluate file-based security properties of the container sandbox profile for iOS 9.0.2 and discovered seven classes of exploitable vulnerabilities.

The [SandScout paper](https://dl.acm.org/doi/10.1145/2976749.2978336) (`SandScout: Automatic Detection of Flaws in iOS Sandbox Profiles`), presented at ACM CCS 2016, details the architecture and implementation of SandScout and their findings.

SandScout is open source software released under the 3-clause BSD license.

Authors:
  * Luke Deshotels, North Carolina State University
  * Răzvan Deaconescu, University POLITEHNICA of Bucharest
  * Mihai Chiroiu, University POLITEHNICA of Bucharest
  * Lucas Davi, Technische Universitat Darmstadt
  * William Enck, North Carolina State University
  * Ahmad-Reza Sadeghi, Technische Universitat Darmstadt

# Haskell Rewrite

This is a Haskell rewrite of the original Python/Prolog project. It parses SBPL profiles, flattens the rules into Prolog facts (for testing), and runs the security queries natively.

Rewrite authors:
  * Ahmad Arnaoute, University POLITEHNICA of Bucharest
  * Petre-Luca Mirea-Bulubașa, University POLITEHNICA of Bucharest

## Build

To run from source:

```sh
cabal run -- <flags> # see usage for flags
```

To build a binary locally:

```sh
cabal install --install-method=copy --installdir=./dist
```

To put the binary in another directory change `--installdir` to your desired path.

## Usage

```sh
sandscout [MODE] <profile.sb>
```

| Flag | Description |
|---|---|
| *(none)* | Run all queries (default) |
| `--query [1-4]` | Run analysis queries (or a specific one) |
| `--json [1-4]` | Same, but output as JSON |
| `--prolog-facts` | Emit flattened Prolog facts |
| `--ast` | Print the parsed rule AST |
| `--help` | Show help |

## Queries

1. `file-write*` rules without container sandbox, where all capabilities are third-party
2. `file-read*` rules without container sandbox or any capabilities
3. Overlapping read/write rules without container sandbox or capabilities
4. `file-read*` rules accessing `/private/var/mobile/` without container sandbox or capabilities

## Tests

```sh
cabal test
```

Compares parser + emitter output against `.pl` files in `test/test-cases/`, and query output against reference files in `test/outputFromQueries/`.

## Project Structure

```
app/Main.hs          CLI entry point
src/
  Types.hs           AST types (Rule, Filter, FlatRule, FlatFilter)
  Parser.hs          Megaparsec parser for SBPL
  PrologEmitter.hs   Flattens rules (require-all/any/not)
  Render.hs          Text rendering for flat filters (quoted/unquoted modes)
  Analyze.hs         Security queries 1-4
  JsonEmitter.hs     JSON output
test/
  Spec.hs            Test suite
  test-cases/        .sb input + .pl output pairs
  outputFromQueries/ Expected query output for containerBetterGraphProcess.sb
```