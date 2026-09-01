# Sandscout (Haskell Port)

Sandscout is a static analysis tool for iOS Sandbox Profile Language (SBPL) profiles, originally ported from Python/Prolog to a unified, standalone Haskell architecture.

## Overview

The original implementation relied on a Python PLY parser, shell transformation scripts, and an external SWI-Prolog runtime (`rules.pl`) to evaluate security queries. This project replaces the entire pipeline with a single standalone Haskell binary using Megaparsec for parsing, an intermediate AST representation, and a native rule analysis engine.

## Architecture

- **`src/Types.hs`**: Abstract Syntax Tree (AST) definitions for SBPL profiles, rules, filters, and flattened evaluation facts.
- **`src/Parser.hs`**: Megaparsec-based SBPL parser handling Lisp-style combinators, regular expressions, and nested filters.
- **`src/Render.hs`**: AST formatting engine supporting both Prolog fact serialization (quoted) and query reporting (unquoted).
- **`src/PrologEmitter.hs`**: Flattens nested AST structures into canonical `allow`/`deny` fact representations.
- **`src/Analyze.hs`**: Pure Haskell security rule evaluation engine replacing the legacy Prolog/SWI-Prolog runtime.
- **`app/Main.hs`**: CLI entrypoint with modes for AST inspection, Prolog fact emission, and native query analysis.

## Security Analysis Queries

1. **Query 1**: Identifies `file-write*` permissions requiring third-party extensions without container restrictions.
2. **Query 2**: Detects unrestricted `file-read*` operations lacking container or capability constraints.
3. **Query 3**: Evaluates overlapping read and write access rules via path prefix matching and regex heuristics.
4. **Query 4**: Flags read rules granting access to sensitive mobile paths (`/private/var/mobile/`) or generic `vnode-type` filters.

## Building & Testing

```bash
# Build the executable and libraries
cabal build

# Run full test suite (golden .pl parity + query outputs)
cabal test
```

## CLI Usage

```bash
# Run all native security analysis queries (Queries 1-4)
cabal run sandscout -- --query test-cases/containerBetterGraphProcess.sb

# Run a specific query (e.g. Query 1)
cabal run sandscout -- --query 1 test-cases/containerBetterGraphProcess.sb

# Emit flattened Prolog facts
cabal run sandscout -- --prolog-facts test-cases/requireAnyTest.sb

# Inspect parsed AST
cabal run sandscout -- --ast test-cases/requireAnyTest.sb
```