# Changelog

All notable changes to this project are documented here.
This project follows [Semantic Versioning](https://semver.org/).

## [2.2.0] — 2026-09-02

### Added
- **Academic Research Manuscript Deployment**:
  - Authored full research paper (*"Six Decades of Human Spaceflight: A Reproducible Analysis of Global Participation, Career Duration, and the Commercial Inflection Point (1961–2026)"*) in `docs/MANUSCRIPT.md` and `docs/SUMMARY_REPORT.md`.
  - Built `scripts/generate_manuscript.py` to compile the manuscript into both standalone **PDF** (`docs/Astronaut_Trends_Manuscript.pdf` / `Astronaut_Trends_Manuscript.pdf`) and **Word Document (.docx)** (`docs/Astronaut_Trends_Manuscript.docx` / `Astronaut_Trends_Manuscript.docx`) formats with embedded high-resolution figures, styled tables, and formatted references.
- **GitHub Pages Narrative & Showcase Upgrade**:
  - Enhanced `docs/index.html` with an integrated Research Narrative & Executive Summary showcase, Key Findings cards, and prominent manuscript download buttons.
  - Corrected external stylesheet/script dependency URLs in `docs/index.html` to point to locally vendored `cose-assets/` for complete offline and archival reliability.

## [2.1.0] — 2026-07-11

### Added
- **Orbital vs suborbital dimension.** Each astronaut is now classified as *Orbital*
  (reached orbit on any mission) or *Suborbital-only*, derived from the Planet4589
  mission `OrbID` field (`ORB*` vs `SO*`).
- A **flight-class filter** (All / Orbital / Suborbital) on the dashboard that
  re-renders **every** chart for the selected population, plus a featured
  **orbital-vs-suborbital timeline**, two new KPIs, a static figure
  (`figures/fig6_orbital_suborbital.png`), and a `flightclass_by_year.csv` output.
- Result: 637 astronauts (82.4%) reached orbit; 136 (17.6%) are suborbital-only, almost
  all post-2021 commercial passengers.

### Changed
- Dashboard aggregation refactored to compute all chart data client-side from the
  per-astronaut table, so the country and flight-class filters apply uniformly.

## [2.0.0] — 2026-07-11

A complete rebuild: from a single script + static image to a reproducible,
documented, and interactively visualised project ready for archival.

### Added
- **Reproducible pipeline** `scripts/build_dataset.py` — fetches the live
  Planet4589 astronaut & mission catalogues, reprocesses them, and regenerates
  all datasets and the dashboard bundle. Supports `--offline` reprocessing.
- **Tidy datasets** in `data/processed/` (per-astronaut table + five aggregates),
  fully described in `data/DATA_DICTIONARY.md`.
- **Gender** as a new analysis dimension.
- **Interactive dashboard** (`docs/index.html`) with 13 linked, filterable,
  theme-aware visualisations (timeline, small multiples, box-and-whisker, bar,
  100% stacked, sankey, chord, force network, radar, heatmap, gender, table),
  deployed via GitHub Pages. ECharts + D3 vendored locally.
- **Static figures** and auto-generated summary tables via `scripts/make_figures.py`.
- **Zenodo/citation packaging**: `CITATION.cff`, `.zenodo.json`, `CONTRIBUTING.md`,
  `requirements.txt`, `.gitignore`, and a GitHub Pages deploy workflow.
- **Data coverage extended** from 1961–2024 to **1961–2026**.

### Changed
- Repository reorganised into `data/`, `scripts/`, `figures/`, `docs/`.
- Mission-tag parsing corrected (tag-before-slash, comma-separated) and analysis
  restricted to the *Flown Astronauts* section of the catalogue.
- README rewritten as a full project overview + roadmap.

### Preserved
- Original R analysis (`scripts/mission_analysis_original.R`), the original study
  figure (`figures/original_study_figure.png`), and the 2024 summary CSV
  (`data/astronaut_summary_v1_2024.csv`).

## [1.0.0] — original release
- Single R script analysing Planet4589 data through 2024, one static faceted line
  chart, and a descriptive README.
