## Summary

Replaces the legacy external Python (`ply`) and Prolog (`swipl`) pipeline with a native, single-binary Haskell architecture for parsing SBPL profiles and executing security analysis queries.

## Key Changes

- **AST & Parsing (`src/Types.hs`, `src/Parser.hs`)**:
  - Implemented Megaparsec-based recursive descent parser for Lisp-style SBPL syntax.
  - Normalized wildcard actions (`*` $\rightarrow$ `STAR`), subpath trailing slashes, and compound filter combinators (`require-all`, `require-any`, `require-not`, `require-entitlement`).

- **Unified Rendering Engine (`src/Render.hs`)**:
  - Extracted AST formatting pipeline with parameterized `RenderMode` (`Quoted` / `Unquoted`).
  - Decoupled serialization logic from `PrologEmitter.hs` and removed formatting duplication.

- **Native Rule Analyzer (`src/Analyze.hs`)**:
  - Implemented pure Haskell evaluation for Queries 1–4, eliminating external dependencies on `rules.pl` and SWI-Prolog.
  - Added prefix matching and sound conservative over-approximation in `pathAccess` / `overlap` for Query 3.

- **Docs & CI**:
  - Updated `README.md` with architectural overview, build instructions, and CLI usage.
  - Added GitHub Actions workflow (`.github/workflows/ci.yml`) for automated builds and testing.

## Verification & Testing

- [x] **Prolog Emission Parity**: 100% match against golden `.pl` fixtures in `test-cases/` (`containerBetterGraphProcess`, `containerManualPruning`, `profile`, `requireAnyTest`).
- [x] **Query Parity**: Exact match on Query 1, Query 2, and Query 4 outputs against reference data in `outputFromQueries/`.
- [x] **Automated Test Suite**: All 7 Tasty test cases passing (`cabal test`).
- [x] **CLI Validation**: Verified `--ast`, `--prolog-facts`, `--query`, and `--query [1-4]` flags.