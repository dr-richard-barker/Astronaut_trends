#!/usr/bin/env python3
"""
build_dataset.py — Fetch and reprocess the Planet4589 human-spaceflight
catalogues into tidy, analysis-ready datasets for the Astronaut Trends project.

Pipeline
--------
1. Download (or read cached) fixed-width catalogues:
     - astro.html    : one row per astronaut (career totals)
     - missions.html : one row per crewed mission (launch/land dates)
2. Parse the fixed-width columns into records.
3. For each *flown* astronaut, derive:
     - first / last mission year (join Missions -> missions catalogue)
     - total career duration in space (seconds / days)
     - a coarse duration bin (5 categories)
     - a cleaned citizenship group (USA / Russia / China / Other)
4. Write tidy outputs to data/processed/ and JSON bundles to docs/data/
   that drive the interactive dashboard.

The dataset can be refreshed at any time by re-running this script; pass
--offline to reprocess the cached snapshot in data/raw/ without a network call.

Data source: Jonathan McDowell, planet4589.org/space/astro/lists/
Author: Dr Richard Barker (MadWest Rocketry / project team)
License: CC-BY-4.0 (data), MIT (code)
"""
from __future__ import annotations

import argparse
import csv
import json
import os
import sys
import urllib.request
from collections import Counter, defaultdict
from datetime import datetime, timezone

# --------------------------------------------------------------------------
# Paths
# --------------------------------------------------------------------------
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW_DIR = os.path.join(ROOT, "data", "raw")
PROC_DIR = os.path.join(ROOT, "data", "processed")
SITE_DATA_DIR = os.path.join(ROOT, "docs", "data")

ASTRO_URL = "https://planet4589.org/space/astro/lists/astro.html"
MISSIONS_URL = "https://planet4589.org/space/astro/lists/missions.html"
ASTRO_RAW = os.path.join(RAW_DIR, "astro.html")
MISSIONS_RAW = os.path.join(RAW_DIR, "missions.html")

# Fixed-width column start positions (0-indexed), derived from the header rows.
ASTRO_COLS = {
    "No": (0, 9), "Name": (9, 50), "Rank": (50, 69), "Born": (69, 82),
    "Died": (82, 95), "Citizen": (95, 104), "Native": (104, 113),
    "S": (113, 115), "NFL": (115, 120), "NRI": (120, 125),
    "Duration": (125, 142), "G": (142, 144), "M": (144, 146),
    "ETH": (146, 152), "FLAG": (152, 158), "ShortName": (158, 189),
    "Missions": (189, 279), "UName": (279, None),
}
MISSION_COLS = {
    "HSFID": (0, 9), "HSFTAG": (9, 20), "Desig": (20, 33), "JTAG": (33, 46),
    "Ship": (46, 77), "LDate": (77, 98), "EDate": (98, 119), "Dur": (119, 136),
    "Crew": (136, 141), "Callsign": (141, 162), "Station": (162, 175),
    "Progra": (175, 182), "Project": (182, 197), "OrbID": (197, 206),
    "DS": (206, 211), "SSF": (211, 216), "MType": (216, None),
}

DURATION_BINS = [
    "less than 1 hour",
    "1 hour to 1 week",
    "1 week to 50 days",
    "50 days to 1 year",
    "more than 1 year",
]


# --------------------------------------------------------------------------
# Fetch
# --------------------------------------------------------------------------
def fetch(url: str, dest: str) -> None:
    print(f"  downloading {url}")
    req = urllib.request.Request(url, headers={"User-Agent": "AstronautTrends/1.0"})
    with urllib.request.urlopen(req, timeout=90) as r:
        data = r.read()
    with open(dest, "wb") as f:
        f.write(data)
    print(f"    -> {dest} ({len(data):,} bytes)")


def slice_row(line: str, cols: dict) -> dict:
    out = {}
    for name, (a, b) in cols.items():
        out[name] = line[a:b].strip() if b else line[a:].strip()
    return out


# --------------------------------------------------------------------------
# Parse
# --------------------------------------------------------------------------
def parse_astro(path: str) -> list[dict]:
    """Return records only from the 'Flown Astronauts' section (humans)."""
    rows = []
    in_flown = False
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in f:
            line = line.rstrip("\n")
            if "<H3>" in line:
                in_flown = "Flown Astronauts" in line
                continue
            if not in_flown:
                continue
            if not line.startswith("AS-"):
                continue
            rows.append(slice_row(line, ASTRO_COLS))
    return rows


def parse_missions(path: str) -> dict:
    """HSFTAG -> launch year (int) for every crewed mission."""
    tag_year = {}
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line[0] != "H":
                continue
            if line.startswith("HSFID"):
                continue
            rec = slice_row(line, MISSION_COLS)
            tag = rec["HSFTAG"].strip()
            ldate = rec["LDate"].strip()
            if not tag or not ldate:
                continue
            year = ldate.split()[0]
            if year.isdigit():
                tag_year[tag] = int(year)
    return tag_year


# --------------------------------------------------------------------------
# Transform
# --------------------------------------------------------------------------
def duration_to_seconds(dur: str):
    """Parse D:H:M:S (or H:M:S / M:S) career duration into seconds."""
    dur = (dur or "").strip()
    if not dur:
        return None
    parts = dur.split(":")
    try:
        nums = [int(p) for p in parts]
    except ValueError:
        return None
    # Pad from the right so the last field is always seconds.
    while len(nums) < 4:
        nums.insert(0, 0)
    d, h, m, s = nums[-4], nums[-3], nums[-2], nums[-1]
    return d * 86400 + h * 3600 + m * 60 + s


def bin_duration(seconds):
    if seconds is None:
        return None
    if seconds < 3600:
        return "less than 1 hour"
    if seconds <= 604800:
        return "1 hour to 1 week"
    if seconds <= 4320000:
        return "1 week to 50 days"
    if seconds <= 31536000:
        return "50 days to 1 year"
    return "more than 1 year"


def clean_citizen(code: str) -> str:
    code = (code or "").strip().upper().rstrip("?")
    if code in ("US", "USA"):
        return "USA"
    if code == "RU":
        return "Russia"
    if code == "CN":
        return "China"
    return "Other"


def clean_gender(code: str) -> str:
    code = (code or "").strip().upper()
    if code == "M":
        return "Male"
    if code == "F":
        return "Female"
    return "Unknown"


def mission_tags(field: str) -> list[str]:
    """'MR3/PLT,A14/CDR' -> ['MR3', 'A14'] (tag before first slash, comma-split)."""
    tags = []
    for chunk in (field or "").split(","):
        chunk = chunk.strip()
        if not chunk:
            continue
        tags.append(chunk.split("/")[0].strip())
    return [t for t in tags if t]


def build_records(astro_rows: list[dict], tag_year: dict) -> list[dict]:
    records = []
    for r in astro_rows:
        tags = mission_tags(r["Missions"])
        years = [tag_year[t] for t in tags if t in tag_year]
        first_year = min(years) if years else None
        last_year = max(years) if years else None
        secs = duration_to_seconds(r["Duration"])
        born = r["Born"].split()[0] if r["Born"].strip() else ""
        records.append({
            "Name": r["Name"],
            "Citizen": r["Citizen"],
            "CitizenGroup": clean_citizen(r["Citizen"]),
            "Gender": clean_gender(r["G"]),
            "BornYear": int(born) if born.isdigit() else None,
            "FirstMissionYear": first_year,
            "LastMissionYear": last_year,
            "CareerSpanYears": (last_year - first_year) if (first_year and last_year) else None,
            "NumMissions": len(tags),
            "DurationSeconds": secs,
            "DurationDays": round(secs / 86400, 3) if secs is not None else None,
            "DurationBin": bin_duration(secs),
        })
    return records


# --------------------------------------------------------------------------
# Aggregate + write
# --------------------------------------------------------------------------
def write_csv(path: str, fieldnames: list[str], rows: list[dict]) -> None:
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        for row in rows:
            w.writerow(row)
    print(f"    wrote {path} ({len(rows)} rows)")


def main() -> int:
    ap = argparse.ArgumentParser(description="Build Astronaut Trends datasets.")
    ap.add_argument("--offline", action="store_true",
                    help="Reprocess cached data/raw snapshot; skip download.")
    args = ap.parse_args()

    os.makedirs(RAW_DIR, exist_ok=True)
    os.makedirs(PROC_DIR, exist_ok=True)
    os.makedirs(SITE_DATA_DIR, exist_ok=True)

    print("1. Acquiring raw catalogues")
    if not args.offline:
        try:
            fetch(ASTRO_URL, ASTRO_RAW)
            fetch(MISSIONS_URL, MISSIONS_RAW)
        except Exception as e:  # noqa: BLE001
            print(f"  ! download failed ({e}); falling back to cached snapshot")
    if not (os.path.exists(ASTRO_RAW) and os.path.exists(MISSIONS_RAW)):
        print("  ! no raw data available", file=sys.stderr)
        return 1

    print("2. Parsing")
    astro_rows = parse_astro(ASTRO_RAW)
    tag_year = parse_missions(MISSIONS_RAW)
    print(f"    {len(astro_rows)} flown astronauts, {len(tag_year)} missions")

    print("3. Building per-astronaut records")
    records = build_records(astro_rows, tag_year)
    usable = [r for r in records if r["FirstMissionYear"] and r["DurationBin"]]
    print(f"    {len(usable)} astronauts with usable year + duration")

    # --- per-astronaut tidy table ---
    astro_fields = ["Name", "Citizen", "CitizenGroup", "Gender", "BornYear",
                    "FirstMissionYear", "LastMissionYear", "CareerSpanYears",
                    "NumMissions", "DurationSeconds", "DurationDays", "DurationBin"]
    write_csv(os.path.join(PROC_DIR, "astronaut_level.csv"), astro_fields, records)

    # --- summary: year x group x bin (compatible with the original CSV) ---
    counter = Counter()
    for r in usable:
        counter[(r["FirstMissionYear"], r["CitizenGroup"], r["DurationBin"])] += 1
    summary = [{"FirstMissionYear": y, "CitizenCleaned": g,
                "DurationBinned": b, "Count": c}
               for (y, g, b), c in sorted(counter.items())]
    write_csv(os.path.join(PROC_DIR, "astronaut_summary.csv"),
              ["FirstMissionYear", "CitizenCleaned", "DurationBinned", "Count"], summary)

    # --- country x bin matrix (for sankey / radar / heatmap) ---
    country_bin = Counter()
    for r in usable:
        country_bin[(r["CitizenGroup"], r["DurationBin"])] += 1
    cb_rows = [{"CitizenGroup": g, "DurationBin": b, "Count": c}
               for (g, b), c in sorted(country_bin.items())]
    write_csv(os.path.join(PROC_DIR, "country_by_bin.csv"),
              ["CitizenGroup", "DurationBin", "Count"], cb_rows)

    # --- decade x country (for chord / network) ---
    def decade(y):
        return (y // 10) * 10
    dec_country = Counter()
    for r in usable:
        dec_country[(f"{decade(r['FirstMissionYear'])}s", r["CitizenGroup"])] += 1
    dc_rows = [{"Decade": d, "CitizenGroup": g, "Count": c}
               for (d, g), c in sorted(dec_country.items())]
    write_csv(os.path.join(PROC_DIR, "decade_by_country.csv"),
              ["Decade", "CitizenGroup", "Count"], dc_rows)

    # --- gender x year ---
    gender_year = Counter()
    for r in usable:
        gender_year[(r["FirstMissionYear"], r["Gender"])] += 1
    gy_rows = [{"FirstMissionYear": y, "Gender": g, "Count": c}
               for (y, g), c in sorted(gender_year.items())]
    write_csv(os.path.join(PROC_DIR, "gender_by_year.csv"),
              ["FirstMissionYear", "Gender", "Count"], gy_rows)

    print("4. Writing JSON bundle for the dashboard")
    groups = ["USA", "Russia", "China", "Other"]
    years = sorted({r["FirstMissionYear"] for r in usable})

    # box/whisker source: duration-days per astronaut by group and by decade
    dur_by_group = defaultdict(list)
    dur_by_decade = defaultdict(list)
    for r in usable:
        if r["DurationDays"] is not None:
            dur_by_group[r["CitizenGroup"]].append(r["DurationDays"])
            dur_by_decade[f"{decade(r['FirstMissionYear'])}s"].append(r["DurationDays"])

    bundle = {
        "meta": {
            "generated": datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC"),
            "source": "planet4589.org/space/astro/lists (J. McDowell)",
            "n_astronauts": len(usable),
            "n_missions": len(tag_year),
            "year_min": min(years),
            "year_max": max(years),
            "groups": groups,
            "bins": DURATION_BINS,
        },
        "summary": summary,
        "astronaut_level": [
            {k: r[k] for k in ("Name", "CitizenGroup", "Gender", "FirstMissionYear",
                               "NumMissions", "DurationDays", "DurationBin")}
            for r in usable
        ],
        "country_by_bin": cb_rows,
        "decade_by_country": dc_rows,
        "gender_by_year": gy_rows,
        "duration_by_group": {g: sorted(dur_by_group[g]) for g in groups},
        "duration_by_decade": {d: sorted(v) for d, v in sorted(dur_by_decade.items())},
    }
    out = os.path.join(SITE_DATA_DIR, "astronaut_data.json")
    with open(out, "w", encoding="utf-8") as f:
        json.dump(bundle, f, indent=1)
    print(f"    wrote {out} ({os.path.getsize(out):,} bytes)")

    # headline numbers for the report
    print("\nHeadline figures")
    print(f"  Total flown astronauts analysed : {len(usable)}")
    by_group = Counter(r["CitizenGroup"] for r in usable)
    for g in groups:
        print(f"    {g:8s}: {by_group[g]}")
    fem = sum(1 for r in usable if r["Gender"] == "Female")
    print(f"  Female astronauts               : {fem} ({100*fem/len(usable):.1f}%)")
    print(f"  Year range                      : {min(years)}-{max(years)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
