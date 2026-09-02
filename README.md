# 🚀 Astronaut Trends

**Six decades of human spaceflight — who has flown, from where, for how long, and how the mix has shifted from the Space Race to the commercial era.**

[![Live dashboard](https://img.shields.io/badge/Live-Interactive_Dashboard-2a78d6)](https://dr-richard-barker.github.io/Astronaut_trends/)
[![Manuscript PDF](https://img.shields.io/badge/Manuscript-PDF_Download-e34948)](docs/Astronaut_Trends_Manuscript.pdf)
[![Manuscript Word](https://img.shields.io/badge/Manuscript-Word_DOCX-2a78d6)](docs/Astronaut_Trends_Manuscript.docx)
[![Data: CC-BY-4.0](https://img.shields.io/badge/Data-CC--BY--4.0-1baf7a)](https://creativecommons.org/licenses/by/4.0/)
[![Code: MIT](https://img.shields.io/badge/Code-MIT-eda100)](LICENSE)
[![Data source: planet4589](https://img.shields.io/badge/Source-planet4589.org-lightgrey)](https://planet4589.org/space/astro/lists/)

> 👉 **[Open the live interactive dashboard & research showcase →](https://dr-richard-barker.github.io/Astronaut_trends/)**  
> (13 linked, filterable, theme-aware visualisations + academic research narrative — updated through **2026**.)  
> 📄 **[Download Full Manuscript PDF](docs/Astronaut_Trends_Manuscript.pdf)** · 📝 **[Download Word Document (DOCX)](docs/Astronaut_Trends_Manuscript.docx)**

---

## What this repository is

This project takes the [Planet4589 General Catalogue of Astronauts](https://planet4589.org/space/astro/lists/) (curated by Jonathan C. McDowell) and turns it into a **reproducible, updateable, and richly visualised census** of global human-spaceflight trends. It started life as a single R script and a static line chart; it is now a full automated data pipeline, a published dataset, an academic research manuscript, and an interactive web dashboard.

Every person who has flown is counted in the **year of their maiden spaceflight**, grouped by **citizenship** (USA · Russia/USSR · China · Other), by **flight class** (Orbital vs. Suborbital), and by the **cumulative time they have spent in space** across their career, binned into five operational bands:

| Band | Meaning | Typical Programme Profiles |
|---|---|---|
| **< 1 hour** | Suborbital hops | Early test flights (Mercury/X-15), modern commercial suborbital (Blue Origin, Virgin Galactic) |
| **1 hour – 1 week** | Short orbital missions | Vostok, Mercury orbital, Gemini, early Soyuz, short Shenzhou demos |
| **1 week – 50 days** | Medium orbital stays | Typical Space Shuttle missions, short ISS taxi flights, Apollo lunar missions |
| **50 days – 1 year** | Long-duration station increments | Standard Salyut, Mir, ISS, and Tiangong expedition crews |
| **> 1 year** | Ultra-long cumulative career time | Multi-mission career veterans exceeding 365 cumulative days aloft |

---

## 🎯 Goals & Roadmap

Items marked ✅ are complete in this release.

**Data & reproducibility**
- ✅ Rebuild the analysis from the **primary source** (`scripts/build_dataset.py` fetches and reprocesses the live Planet4589 catalogues).
- ✅ Extend coverage from the original 1961–2024 snapshot to **1961–2026**.
- ✅ Publish tidy, documented, machine-readable datasets (`data/processed/`, with a [data dictionary](data/DATA_DICTIONARY.md)).
- ✅ Preserve the original R script and figure for provenance.
- ⬜ Add automated monthly refresh (GitHub Action) so the site tracks the source.
- ⬜ Unit tests / data-validation checks on the parser.

**Analysis — "better ways to look at it"**
- ✅ Distinguish **orbital vs suborbital** astronauts — reveals that the recent boom is almost entirely suborbital.
- ✅ Add **gender** as an analysis dimension (women's share over time).
- ✅ Analyse **career-duration distributions** (box-and-whisker) rather than just counts.
- ✅ Compare national **"duration fingerprints"** (radar) — each country has a distinct operational philosophy.
- ✅ Show **flows** (country → duration band) and **relationships** (decade ↔ country) explicitly.
- ⬜ Age-at-first-flight and career-span analyses (the pipeline already extracts birth year).
- ⬜ Vehicle / programme breakdown (Shuttle vs Soyuz vs Shenzhou vs commercial).
- ⬜ Cumulative person-days-in-space by nation over time.

**Presentation & dissemination**
- ✅ Deploy an **interactive dashboard** as a GitHub Pages site.
- ✅ Author and deploy a complete **academic manuscript** as both a **PDF** (`docs/Astronaut_Trends_Manuscript.pdf`) and **Word document** (`docs/Astronaut_Trends_Manuscript.docx`).
- ✅ Provide **static figures** and summary reports for papers/slides (`docs/SUMMARY_REPORT.md`, `docs/SUMMARY_TABLES.md`).
- ✅ Package the repository for a **Zenodo** archival release with a DOI (`CITATION.cff`, `.zenodo.json`).

---

## 📊 Headline Findings (1961–2026 Snapshot)

*773 flown astronauts analysed, 1961–2026. Source snapshot generated automatically from Planet4589.*

- **The USA dominates by headcount** (460, ~60% of all flyers) but flies **short**: a median career of just **~20 days** (19.8 days) in space.
- **Russia/USSR flies long**: 131 astronauts with a median career of **~195 days** (194.8 days) and the single longest career on record (**> 1,110 days** cumulative, Oleg Kononenko).
- **China** (27 astronauts, all since **2003**) clusters in the **50 days – 1 year** band — a focused, station-oriented programme (median: **192.2 days**).
- **The 2020s are already the busiest debut decade ever** (199 new astronauts, vs 168 in the 1990s ISS build-up) — driven overwhelmingly by **suborbital commercial spaceflight**.
- **Orbital vs suborbital:** **637 (82.4%)** of all astronauts have reached orbit; **136 (17.6%)** are suborbital-only — and 128 of those are post-2021 Blue Origin / Virgin Galactic passengers. Russia and China have flown **zero** suborbital astronauts; the split is a US-and-partners phenomenon.
- **Women remain a minority**: just **14.7% (114/773)** of all astronauts, a share that rises over time (19.1% in the 2020s) but has never approached parity.

![New astronauts per year by country](figures/fig1_timeline.png)

![Orbital vs suborbital over time](figures/fig6_orbital_suborbital.png)

<p align="center">
  <img src="figures/fig2_box_country.png" width="48%" alt="Career duration by country">
  <img src="figures/fig4_radar.png" width="48%" alt="Duration profile radar">
</p>

See [`docs/SUMMARY_TABLES.md`](docs/SUMMARY_TABLES.md) for the full numeric tables and [`docs/MANUSCRIPT.md`](docs/MANUSCRIPT.md) for the complete paper.

---

## 🖥️ The Interactive Dashboard & Research Showcase

The [live site](https://dr-richard-barker.github.io/Astronaut_trends/) includes:
- **Research Narrative & Executive Summary**: High-level key findings and insights.
- **Manuscript Downloads**: Direct access to the PDF and Word document versions.
- **Filterable Interactive Visualizations** (by country and orbital flight class):
  0. **★ Orbital vs suborbital over time** — always shows both classes.
  1. **Stacked timeline** — new astronauts per year by country (zoomable).
  2. **Small multiples** — duration mix over time, per country.
  3. **Box & whisker** — career duration by country.
  4. **Box & whisker** — career duration by decade.
  5. **Bar** — total astronauts by country.
  6. **100% stacked bar** — duration-band composition.
  7. **Sankey** — country → duration-band flows.
  8. **Chord** — decade ↔ country relationships.
  9. **Force-directed network** — country–decade bipartite graph.
  10. **Radar** — national duration "fingerprints".
  11. **Heatmap** — decade × duration band.
  12. **Gender trend** — women in spaceflight over time.
  13. **Searchable summary table** — full aggregated dataset.

Charts are built with [ECharts](https://echarts.apache.org/) and [D3](https://d3js.org/), **vendored locally** (`docs/libs/` and `docs/cose-assets/`) so the site renders with no external network calls — essential for long-term reproducibility and Zenodo archival.

---

## 🗂️ Repository Structure

```
Astronaut_trends/
├── README.md                          ← you are here
├── Astronaut_Trends_Manuscript.pdf    ← full academic manuscript (PDF)
├── Astronaut_Trends_Manuscript.docx   ← full academic manuscript (Word DOCX)
├── LICENSE                            ← MIT (code)
├── CITATION.cff                       ← citation metadata
├── .zenodo.json                       ← Zenodo archival metadata
├── CHANGELOG.md                       ← version history
├── CONTRIBUTING.md                    ← contribution & update guidelines
├── requirements.txt                   ← Python dependencies
├── data/
│   ├── raw/                           ← Planet4589 source snapshots (astro.html, missions.html)
│   ├── processed/                     ← tidy, analysis-ready CSVs (regenerated by the pipeline)
│   ├── astronaut_summary_v1_2024.csv  ← original 2024 summary (archived for provenance)
│   └── DATA_DICTIONARY.md             ← field definitions & schema
├── scripts/
│   ├── build_dataset.py               ← fetch + parse + bin + summarise (the pipeline)
│   ├── make_figures.py                ← static PNG figures + summary tables
│   ├── generate_manuscript.py         ← compiles PDF & Word manuscripts
│   └── mission_analysis_original.R    ← original R analysis (preserved)
├── figures/                           ← generated PNGs + original study figure
└── docs/                              ← GitHub Pages deployment
    ├── index.html                     ← dashboard & research showcase
    ├── Astronaut_Trends_Manuscript.pdf
    ├── Astronaut_Trends_Manuscript.docx
    ├── MANUSCRIPT.md                  ← full manuscript in markdown
    ├── SUMMARY_REPORT.md              ← executive research report
    ├── SUMMARY_TABLES.md              ← auto-generated numeric tables
    ├── data/astronaut_data.json       ← dashboard data bundle
    ├── libs/                          ← vendored ECharts + D3
    └── cose-assets/                   ← vendored theme assets
```

---

## 🔄 Reproduce / Update the Data

```bash
# 1. Install dependencies (Python 3.9+)
pip install -r requirements.txt

# 2. Fetch the latest catalogues and rebuild all datasets + dashboard JSON
python scripts/build_dataset.py           # add --offline to use cached data/raw snapshot

# 3. Regenerate static figures and summary tables
python scripts/make_figures.py

# 4. Generate the PDF and Word document manuscripts
python scripts/generate_manuscript.py

# 5. Preview the dashboard locally
cd docs && python3 -m http.server 8097     # open http://localhost:8097
```

---

## 📚 Data Source & Citation

All underlying data is from **Jonathan C. McDowell's General Catalogue of Astronauts**:
- Astronauts: <https://planet4589.org/space/astro/lists/astro.html>
- Missions: <https://planet4589.org/space/astro/lists/missions.html>
- Project index: <https://planet4589.org/jcm/index.html>

Please credit Planet4589 for the source data. To cite this analysis:

```bibtex
@misc{barker2026astronauttrends,
  author = {Barker, Richard},
  title = {Six Decades of Human Spaceflight: A Reproducible Analysis of Global Participation, Career Duration, and the Commercial Inflection Point (1961--2026)},
  year = {2026},
  publisher = {GitHub / Zenodo},
  url = {https://dr-richard-barker.github.io/Astronaut_trends/}
}
```

---

## ⚖️ Licensing

- **Code** (scripts, dashboard) — [MIT](LICENSE).
- **Derived data, manuscript & figures** — [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/).
- **Underlying source data** — © Jonathan McDowell / Planet4589; used with attribution.

*Part of the MadWest Rocketry education & outreach programme. Contributions welcome — see [CONTRIBUTING.md](CONTRIBUTING.md).*
