# Astronaut Trends — Summary Report

*A reproducible analysis of global human-spaceflight participation, 1961–2026.*
Source data: Jonathan McDowell, [Planet4589 General Catalogue of Astronauts](https://planet4589.org/space/astro/lists/).
Generated from `data/processed/` — see the [live dashboard](https://dr-richard-barker.github.io/Astronaut_trends/) for the interactive version.

---

## Overview

This report analyses **773 flown astronauts** whose first spaceflight fell between
**1961 and 2026**. Each person is placed in the year of their first flight, grouped
by citizenship (USA, Russia/USSR, China, Other), and classified by the cumulative
time they have spent in space across their career.

The story that emerges is one of **three distinct national styles** layered over a
**structural shift in what "being an astronaut" means** — from a handful of
government test pilots to, in the 2020s, a wave of short-hop commercial flyers.

![New astronauts per year, by country](../figures/fig1_timeline.png)

---

## 1. Who flies, and how the total has grown

Human spaceflight has come in **waves**, each tied to a programme:

- the **Apollo/early-Soyuz** era of the 1960s,
- the **Space Shuttle** surge that peaks in **1985**,
- the **ISS build-up** of the 1990s (the previous record decade),
- and an unprecedented **2020s spike** that has *already* made it the busiest debut
  decade on record — **199 new astronauts and counting**.

| Decade | New astronauts | USA | Russia | China | Other |
|---|--:|--:|--:|--:|--:|
| 1960s | 53 | 32 | 21 | 0 | 0 |
| 1970s | 47 | 19 | 22 | 0 | 6 |
| 1980s | 129 | 89 | 22 | 0 | 18 |
| 1990s | 168 | 111 | 21 | 0 | 36 |
| 2000s | 120 | 86 | 13 | 6 | 15 |
| 2010s | 57 | 19 | 17 | 5 | 16 |
| 2020s | 199 | 104 | 15 | 16 | 64 |

The 2010s dip and 2020s explosion is the Shuttle-retirement gap (2011) followed by
the arrival of **commercial crew and suborbital tourism**.

---

## 2. Three national styles

| Country | Astronauts | Share | Median career (days) | Max career (days) |
|---|--:|--:|--:|--:|
| USA | 460 | 59.5% | 19.8 | 695.3 |
| Russia | 131 | 16.9% | 194.8 | 1110.6 |
| China | 27 | 3.5% | 192.2 | 418.6 |
| Other | 155 | 20.1% | 9.1 | 545.1 |

- **USA — breadth.** Nearly 60% of all flyers, but a **median career under three
  weeks**. The American programme has always flown *many people for short trips*
  (Shuttle crews of seven; now suborbital hops).
- **Russia — endurance.** Far fewer people, but a **median career near 200 days** and
  the longest career on record (**> 1,100 cumulative days**). A station culture from
  Salyut and Mir through the ISS.
- **China — focused.** A young programme (all 27 astronauts since **2003**) that sits
  almost entirely in the **50 days – 1 year** band — deliberate, station-oriented increments.
- **Other — collaborative & short.** International partners flying as guests on
  Shuttle/Soyuz/ISS, plus a recent surge of commercial tourists, give this group the
  *shortest* median of all.

<p align="center">
  <img src="../figures/fig2_box_country.png" width="49%" alt="Career duration by country">
  <img src="../figures/fig4_radar.png" width="49%" alt="Duration fingerprints">
</p>

The radar "fingerprints" make the styles visible at a glance: the USA and Other groups
bulge toward the short bands, Russia and China toward the long ones.

---

## 3. The commercial-spaceflight inflection

| Duration band | Astronauts | Share |
|---|--:|--:|
| less than 1 hour | 134 | 17.3% |
| 1 hour to 1 week | 52 | 6.7% |
| 1 week to 50 days | 333 | 43.1% |
| 50 days to 1 year | 191 | 24.7% |
| more than 1 year | 63 | 8.2% |

The **"less than 1 hour"** band — 17% of *all* astronauts ever — is almost entirely a
post-2021 phenomenon: suborbital passengers on Blue Origin and Virgin Galactic. This is
the single biggest change in the character of human spaceflight in the dataset, and it is
why the 2020s "Other" and "USA" counts balloon while long-duration numbers stay flat.

---

## 4. Women in spaceflight

Women make up just **14.7%** (114 of 773) of all flown astronauts. The share rises over
the decades but has never approached parity, and remains sensitive to which programmes are
flying in a given year.

![Women in human spaceflight](../figures/fig5_gender.png)

---

## Methodology & caveats

- **Counting rule:** each astronaut is counted once, in the year of their first flight;
  duration is their *cumulative* career total, not a single mission.
- **Binning:** < 1 hour, 1 hour–1 week, 1 week–50 days, 50 days–1 year, > 1 year.
- **Groups:** USA, Russia (incl. USSR), China, Other.
- **Scope:** the *Flown Astronauts* section of the catalogue only; chimpanzee and
  aborted-launch entries are excluded. A small number of records lacking a resolvable
  first-flight year or duration are dropped from the aggregates.
- **Provenance:** figures regenerate from `scripts/build_dataset.py` + `make_figures.py`;
  the exact source snapshot is archived in `data/raw/`.

*Full field definitions: [`data/DATA_DICTIONARY.md`](../data/DATA_DICTIONARY.md).*
