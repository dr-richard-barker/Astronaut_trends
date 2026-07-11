#!/usr/bin/env python3
"""
make_figures.py — Generate static PNG figures and Markdown summary tables
from the processed Astronaut Trends datasets.

Outputs
-------
figures/fig1_timeline.png       stacked area: new astronauts/yr by country
figures/fig2_box_country.png    box & whisker: career days by country
figures/fig3_bar_country.png    bar: total astronauts by country
figures/fig4_radar.png          radar: duration profile by country
figures/fig5_gender.png         women's share over time
docs/SUMMARY_TABLES.md          Markdown tables used in the report / README

Run after scripts/build_dataset.py. Requires matplotlib, numpy, pandas.
"""
from __future__ import annotations
import os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROC = os.path.join(ROOT, "data", "processed")
FIG = os.path.join(ROOT, "figures")
DOCS = os.path.join(ROOT, "docs")
os.makedirs(FIG, exist_ok=True)

COUNTRIES = ["USA", "Russia", "China", "Other"]
CCOL = {"USA": "#2a78d6", "Russia": "#e34948", "China": "#eda100", "Other": "#1baf7a"}
BINS = ["less than 1 hour", "1 hour to 1 week", "1 week to 50 days",
        "50 days to 1 year", "more than 1 year"]
BCOL = ["#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"]

plt.rcParams.update({
    "font.family": "DejaVu Sans", "font.size": 11,
    "axes.edgecolor": "#c3c2b7", "axes.linewidth": 0.8,
    "axes.grid": True, "grid.color": "#e1e0d9", "grid.linewidth": 0.6,
    "axes.spines.top": False, "axes.spines.right": False,
    "figure.facecolor": "white", "savefig.facecolor": "white", "savefig.dpi": 150,
})

summary = pd.read_csv(os.path.join(PROC, "astronaut_summary.csv"))
astro = pd.read_csv(os.path.join(PROC, "astronaut_level.csv"))
usable = astro.dropna(subset=["FirstMissionYear", "DurationBin"]).copy()
usable["FirstMissionYear"] = usable["FirstMissionYear"].astype(int)


def save(fig, name):
    p = os.path.join(FIG, name)
    fig.tight_layout()
    fig.savefig(p, bbox_inches="tight")
    plt.close(fig)
    print("  wrote", p)


# --- Fig 1: stacked area timeline ---
def fig_timeline():
    piv = (summary.groupby(["FirstMissionYear", "CitizenCleaned"])["Count"].sum()
           .unstack(fill_value=0).reindex(columns=COUNTRIES, fill_value=0))
    years = piv.index.values
    fig, ax = plt.subplots(figsize=(10, 4.2))
    ax.stackplot(years, [piv[c].values for c in COUNTRIES],
                 labels=COUNTRIES, colors=[CCOL[c] for c in COUNTRIES], alpha=0.92)
    ax.set_xlim(years.min(), years.max())
    ax.set_xlabel("Year of first spaceflight")
    ax.set_ylabel("New astronauts")
    ax.set_title("New astronauts entering service each year, by country", fontweight="bold")
    ax.legend(loc="upper left", frameon=False, ncol=4)
    save(fig, "fig1_timeline.png")


# --- Fig 2: box & whisker career days by country ---
def fig_box_country():
    data = [usable.loc[usable.CitizenGroup == c, "DurationDays"].clip(lower=0.01).values
            for c in COUNTRIES]
    fig, ax = plt.subplots(figsize=(7, 4.2))
    bp = ax.boxplot(data, tick_labels=COUNTRIES, patch_artist=True, widths=0.6,
                    medianprops=dict(color="#0b0b0b", linewidth=1.6),
                    flierprops=dict(marker="o", markersize=3, markerfacecolor="#898781",
                                    markeredgecolor="none", alpha=0.5))
    for patch, c in zip(bp["boxes"], COUNTRIES):
        patch.set_facecolor(CCOL[c]); patch.set_alpha(0.85); patch.set_edgecolor(CCOL[c])
    ax.set_yscale("log")
    ax.set_ylabel("Career time in space (days, log scale)")
    ax.set_title("Distribution of career duration by country", fontweight="bold")
    save(fig, "fig2_box_country.png")


# --- Fig 3: bar total by country ---
def fig_bar_country():
    counts = [int((usable.CitizenGroup == c).sum()) for c in COUNTRIES]
    fig, ax = plt.subplots(figsize=(7, 3.6))
    bars = ax.barh(COUNTRIES[::-1], counts[::-1],
                   color=[CCOL[c] for c in COUNTRIES[::-1]])
    for b, v in zip(bars, counts[::-1]):
        ax.text(v + max(counts) * 0.01, b.get_y() + b.get_height() / 2, str(v),
                va="center", fontsize=10, color="#52514e")
    ax.set_xlabel("Number of astronauts")
    ax.set_title("Total flown astronauts by country (1961–present)", fontweight="bold")
    ax.grid(axis="y", visible=False)
    save(fig, "fig3_bar_country.png")


# --- Fig 4: radar duration profile ---
def fig_radar():
    def share(c):
        sub = usable[usable.CitizenGroup == c]
        return [(sub.DurationBin == b).sum() / max(1, len(sub)) for b in BINS]
    angles = np.linspace(0, 2 * np.pi, len(BINS), endpoint=False).tolist()
    angles += angles[:1]
    fig, ax = plt.subplots(figsize=(6.4, 6), subplot_kw=dict(polar=True))
    for c in COUNTRIES:
        vals = share(c); vals += vals[:1]
        ax.plot(angles, vals, color=CCOL[c], linewidth=2, label=c)
        ax.fill(angles, vals, color=CCOL[c], alpha=0.07)
    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(["< 1 hr", "1 hr–1 wk", "1 wk–50 d", "50 d–1 yr", "> 1 yr"])
    ax.set_title("National duration 'fingerprint'\n(share of each country's astronauts per band)",
                 fontweight="bold", pad=24)
    ax.legend(loc="upper right", bbox_to_anchor=(1.22, 1.1), frameon=False)
    save(fig, "fig4_radar.png")


# --- Fig 5: gender over time ---
def fig_gender():
    gy = pd.read_csv(os.path.join(PROC, "gender_by_year.csv"))
    piv = gy.pivot_table(index="FirstMissionYear", columns="Gender",
                         values="Count", aggfunc="sum", fill_value=0)
    for col in ("Male", "Female"):
        if col not in piv:
            piv[col] = 0
    piv = piv.sort_index()
    fig, ax = plt.subplots(figsize=(10, 3.8))
    ax.bar(piv.index, piv["Male"], color="#898781", label="Male")
    ax.bar(piv.index, piv["Female"], bottom=piv["Male"], color="#eda100", label="Female")
    ax.set_ylabel("New astronauts")
    ax.set_xlabel("Year of first spaceflight")
    ax.set_title("Women in human spaceflight: new astronauts per year", fontweight="bold")
    ax.legend(frameon=False, loc="upper left")
    save(fig, "fig5_gender.png")


def markdown_tables():
    lines = ["# Astronaut Trends — Summary Tables",
             "",
             "_Auto-generated by `scripts/make_figures.py` from the processed dataset._",
             ""]
    n = len(usable)
    lines += ["## Headline figures", "",
              f"- **Total flown astronauts analysed:** {n}",
              f"- **Year range:** {int(usable.FirstMissionYear.min())}–{int(usable.FirstMissionYear.max())}",
              f"- **Female astronauts:** {int((usable.Gender=='Female').sum())} "
              f"({100*(usable.Gender=='Female').mean():.1f}%)",
              f"- **Astronauts with > 1 year cumulative in space:** {int((usable.DurationBin=='more than 1 year').sum())}",
              ""]

    # Table 1: by country
    lines += ["## Astronauts by country", "",
              "| Country | Astronauts | Share | Median career (days) | Max career (days) |",
              "|---|--:|--:|--:|--:|"]
    for c in COUNTRIES:
        sub = usable[usable.CitizenGroup == c]
        lines.append(f"| {c} | {len(sub)} | {100*len(sub)/n:.1f}% | "
                     f"{sub.DurationDays.median():.1f} | {sub.DurationDays.max():.1f} |")
    lines.append("")

    # Table 2: by decade
    usable["Decade"] = (usable.FirstMissionYear // 10 * 10).astype(int).astype(str) + "s"
    lines += ["## New astronauts by decade", "",
              "| Decade | Astronauts | USA | Russia | China | Other |",
              "|---|--:|--:|--:|--:|--:|"]
    for d, sub in usable.groupby("Decade"):
        row = [f"| {d} | {len(sub)} "]
        for c in COUNTRIES:
            row.append(f"| {int((sub.CitizenGroup==c).sum())} ")
        lines.append("".join(row) + "|")
    lines.append("")

    # Table 3: duration bin counts
    lines += ["## Astronauts by duration band", "",
              "| Duration band | Astronauts | Share |", "|---|--:|--:|"]
    for b in BINS:
        k = int((usable.DurationBin == b).sum())
        lines.append(f"| {b} | {k} | {100*k/n:.1f}% |")
    lines.append("")

    out = os.path.join(DOCS, "SUMMARY_TABLES.md")
    with open(out, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    print("  wrote", out)


if __name__ == "__main__":
    print("Rendering figures")
    fig_timeline(); fig_box_country(); fig_bar_country(); fig_radar(); fig_gender()
    print("Writing summary tables")
    markdown_tables()
    print("Done.")
