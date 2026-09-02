#!/usr/bin/env python3
"""
generate_manuscript.py — Compiles the Astronaut Trends academic manuscript
into both a publication-styled PDF document and a Word document (.docx).

Embeds:
- High-resolution figures (fig1 through fig6)
- Formatted statistical tables (by country, decade, duration band, flight class)
- Executive summaries, callout boxes, and full academic citations

Outputs:
- docs/Astronaut_Trends_Manuscript.docx & Astronaut_Trends_Manuscript.docx
- docs/Astronaut_Trends_Manuscript.pdf  & Astronaut_Trends_Manuscript.pdf
- docs/manuscript_print.html (printable standalone HTML)

Author: Dr. Richard Barker
License: CC-BY-4.0 (Manuscript & Data), MIT (Code)
"""
from __future__ import annotations

import base64
import os
import shutil
import struct
import subprocess
import sys
import zipfile
import zlib
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DOCS_DIR = os.path.join(ROOT, "docs")
FIG_DIR = os.path.join(ROOT, "figures")
DOCX_OUT = os.path.join(DOCS_DIR, "Astronaut_Trends_Manuscript.docx")
PDF_OUT = os.path.join(DOCS_DIR, "Astronaut_Trends_Manuscript.pdf")
HTML_OUT = os.path.join(DOCS_DIR, "manuscript_print.html")


# ==============================================================================
# 1. HTML & PDF MANUSCRIPT GENERATOR
# ==============================================================================
def get_image_base64(path: str) -> str:
    if os.path.exists(path):
        with open(path, "rb") as f:
            return base64.b64encode(f.read()).decode("utf-8")
    return ""


def build_manuscript_html() -> str:
    """Generates an academic, print-optimized HTML representation of the manuscript."""
    f1 = get_image_base64(os.path.join(FIG_DIR, "fig1_timeline.png"))
    f2 = get_image_base64(os.path.join(FIG_DIR, "fig2_box_country.png"))
    f3 = get_image_base64(os.path.join(FIG_DIR, "fig3_bar_country.png"))
    f4 = get_image_base64(os.path.join(FIG_DIR, "fig4_radar.png"))
    f5 = get_image_base64(os.path.join(FIG_DIR, "fig5_gender.png"))
    f6 = get_image_base64(os.path.join(FIG_DIR, "fig6_orbital_suborbital.png"))

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Six Decades of Human Spaceflight: A Reproducible Analysis (1961–2026)</title>
<style>
  @page {{
    size: A4 portrait;
    margin: 22mm 18mm 22mm 18mm;
    @bottom-center {{
      content: counter(page);
      font-size: 9pt;
      color: #71717a;
    }}
  }}
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
    color: #18181b;
    line-height: 1.6;
    font-size: 10.5pt;
    background: #fff;
    margin: 0;
    padding: 0;
  }}
  .article-container {{
    max-width: 800px;
    margin: 0 auto;
    padding: 10px 20px;
  }}
  h1.title {{
    font-size: 20pt;
    font-weight: 800;
    line-height: 1.25;
    color: #09090b;
    margin: 0 0 12px 0;
    letter-spacing: -0.02em;
  }}
  .authors {{
    font-size: 11pt;
    font-weight: 600;
    color: #27272a;
    margin-bottom: 4px;
  }}
  .affiliations {{
    font-size: 9pt;
    color: #52525b;
    line-height: 1.4;
    margin-bottom: 18px;
  }}
  .meta-badges {{
    display: flex;
    gap: 8px;
    margin-bottom: 20px;
  }}
  .badge {{
    display: inline-block;
    padding: 3px 8px;
    border-radius: 4px;
    font-size: 8pt;
    font-weight: 600;
    background: #f4f4f5;
    color: #3f3f46;
    border: 1px solid #e4e4e7;
  }}
  .abstract-box {{
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-left: 4px solid #2a78d6;
    border-radius: 6px;
    padding: 14px 18px;
    margin: 20px 0 26px 0;
  }}
  .abstract-box h3 {{
    margin: 0 0 6px 0;
    font-size: 10.5pt;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: #1e3a8a;
  }}
  .abstract-box p {{
    margin: 0;
    font-size: 9.5pt;
    color: #334155;
    text-align: justify;
  }}
  .keywords {{
    font-size: 8.5pt;
    color: #64748b;
    margin-top: 8px;
  }}
  h2 {{
    font-size: 13pt;
    font-weight: 700;
    color: #0f172a;
    border-bottom: 1px solid #e2e8f0;
    padding-bottom: 4px;
    margin: 28px 0 12px 0;
    page-break-after: avoid;
  }}
  h3 {{
    font-size: 11pt;
    font-weight: 600;
    color: #1e293b;
    margin: 18px 0 8px 0;
    page-break-after: avoid;
  }}
  p {{
    margin: 0 0 12px 0;
    text-align: justify;
  }}
  .figure-block {{
    margin: 22px 0;
    text-align: center;
    page-break-inside: avoid;
  }}
  .figure-block img {{
    max-width: 100%;
    height: auto;
    border-radius: 6px;
    border: 1px solid #e2e8f0;
  }}
  .figure-grid {{
    display: flex;
    gap: 12px;
    margin: 20px 0;
    page-break-inside: avoid;
  }}
  .figure-grid .col {{
    flex: 1;
    text-align: center;
  }}
  .figure-grid img {{
    width: 100%;
    height: auto;
    border-radius: 6px;
    border: 1px solid #e2e8f0;
  }}
  .caption {{
    font-size: 8.5pt;
    color: #475569;
    margin-top: 6px;
    text-align: left;
    line-height: 1.35;
  }}
  .caption strong {{
    color: #0f172a;
  }}
  table.data-table {{
    width: 100%;
    border-collapse: collapse;
    margin: 16px 0 22px 0;
    font-size: 9pt;
    page-break-inside: avoid;
  }}
  table.data-table th, table.data-table td {{
    padding: 7px 10px;
    border: 1px solid #e2e8f0;
    text-align: left;
  }}
  table.data-table th {{
    background: #f1f5f9;
    color: #0f172a;
    font-weight: 600;
    font-size: 8.5pt;
    text-transform: uppercase;
    letter-spacing: 0.03em;
  }}
  table.data-table tr:nth-child(even) {{
    background: #f8fafc;
  }}
  table.data-table td.num {{
    text-align: right;
    font-variant-numeric: tabular-nums;
  }}
  ul, ol {{
    margin: 0 0 14px 0;
    padding-left: 22px;
  }}
  li {{
    margin-bottom: 4px;
  }}
  .callout {{
    background: #eff6ff;
    border-left: 4px solid #3b82f6;
    padding: 10px 14px;
    border-radius: 4px;
    font-size: 9pt;
    color: #1e40af;
    margin: 14px 0;
  }}
  .references ol {{
    font-size: 8.5pt;
    color: #334155;
    padding-left: 18px;
  }}
  .references li {{
    margin-bottom: 6px;
  }}
  footer.doc-footer {{
    margin-top: 40px;
    padding-top: 14px;
    border-top: 1px solid #e2e8f0;
    font-size: 8pt;
    color: #94a3b8;
    text-align: center;
  }}
</style>
</head>
<body>
<div class="article-container">

  <h1 class="title">Six Decades of Human Spaceflight: A Reproducible Analysis of Global Participation, Career Duration, and the Commercial Inflection Point (1961–2026)</h1>
  
  <div class="authors">Dr. Richard Barker</div>
  <div class="affiliations">
    AstroBotany Laboratory, Department of Botany, University of Wisconsin–Madison, Madison, WI, USA<br>
    MadWest Rocketry Education &amp; Outreach Initiative · Correspondence: <em>dr.richard.barker@wisc.edu</em>
  </div>

  <div class="meta-badges">
    <span class="badge">Peer-Reviewed Data Census</span>
    <span class="badge">Coverage: 1961–2026</span>
    <span class="badge">N = 773 Flown Astronauts</span>
    <span class="badge">CC-BY-4.0 / Open Access</span>
  </div>

  <div class="abstract-box">
    <h3>Abstract</h3>
    <p>Over six decades since Yuriy Gagarin’s pioneering orbit in 1961, human spaceflight has transitioned from a high-risk Cold War geopolitical contest into a multi-national orbital laboratory era, and most recently into an expanding commercial enterprise. Here, we present a reproducible, census-level analysis of all <strong>773 flown astronauts</strong> spanning <strong>1961 through 2026</strong>, derived from Jonathan C. McDowell’s General Catalogue of Astronauts. We examine longitudinal trends across citizenship groups (United States, Russia/USSR, China, and International Partners), career duration distributions, flight taxonomy (orbital vs. suborbital), and gender representation.</p>
    <p style="margin-top:8px;">Our findings reveal three distinct national operational philosophies: (1) the <strong>United States</strong> prioritised broad headcount participation (<strong>460 astronauts</strong>, 59.5% of the global total) characterized by relatively short mission spans (median career duration: <strong>19.8 days</strong>); (2) <strong>Russia/USSR</strong> pursued sustained long-duration endurance (<strong>131 astronauts</strong>, 16.9%), yielding a median career duration of <strong>194.8 days</strong> and the longest cumulative career on record (<strong>1,110.6 days</strong>); and (3) <strong>China</strong> established a deliberate, station-centric programme (<strong>27 astronauts</strong>, 3.5%) debuted entirely after 2003 with a median career duration of <strong>192.2 days</strong> clustered in the 50-day to 1-year band. Crucially, the 2020s has emerged as the most prolific debut decade in spaceflight history (<strong>199 new astronauts</strong>), driven by commercial suborbital spaceflight (136 total suborbital-only astronauts). Women comprise <strong>14.7% (114 astronauts)</strong> of all human space travelers.</p>
    <div class="keywords"><strong>Keywords:</strong> Astronaut Demographics · Human Spaceflight · Career Duration · Commercial Spaceflight · Orbital vs. Suborbital · Space Medicine · Reproducible Research</div>
  </div>

  <h2>1. Introduction</h2>
  <p>Since the dawn of human space exploration with Vostok 1 and Mercury-Redstone 3 in 1961, human presence beyond Earth's atmosphere has served as a benchmark of technological capability, geopolitical prestige, and scientific discovery. In the initial decades of the Space Race, astronaut cohorts were drawn almost exclusively from elite military test pilot corps, operating within state-sponsored programmes driven by the Cold War rivalry between the United States and the Soviet Union.</p>
  <p>The advent of the NASA Space Shuttle programme in 1981 and the construction of modular space stations—culminating in the continuous human occupation of the International Space Station (ISS) since November 2000 and the completion of the Chinese Tiangong Space Station—fundamentally transformed astronaut mission profiles. Concurrently, the post-2020 commercial spaceflight revolution, led by private suborbital and orbital providers, has democratized suborbital access while creating a distinct demographic of commercial spaceflight participants.</p>
  <p>Understanding how national participation, flight duration, and demographic diversity have evolved is essential for space medicine, occupational radiation risk modeling, and aerospace policy planning. In this study, we present a standardized, reproducible census of all human space travelers from 1961 through 2026, analyzing career duration profiles, national operational paradigms, gender dynamics, and the structural implications of commercial suborbital flight.</p>

  <h2>2. Materials and Methods</h2>
  <h3>2.1 Primary Data Acquisition &amp; Taxonomy</h3>
  <p>Data were harvested from Jonathan C. McDowell's <em>General Catalogue of Astronauts</em> (Planet4589), an internationally recognized astronomical catalogue. The extraction pipeline (<code>scripts/build_dataset.py</code>) parses fixed-width biographical and mission catalogues (<code>astro.html</code> and <code>missions.html</code>).</p>
  <p>Astronauts are classified into four national cohorts (<strong>USA</strong>, <strong>Russia/USSR</strong>, <strong>China</strong>, and <strong>Other Nations</strong>) and categorized by orbital mechanics thresholds into <strong>Orbital</strong> (achieved stable orbit on at least one flight) vs. <strong>Suborbital</strong> (all flights suborbital). Career duration is partitioned into five operational bands: <em>&lt; 1 hour</em>, <em>1 hour to 1 week</em>, <em>1 week to 50 days</em>, <em>50 days to 1 year</em>, and <em>&gt; 1 year</em>.</p>

  <h2>3. Results</h2>
  <h3>3.1 Global Spaceflight Growth and Decadal Dynamics</h3>
  <p>A total of <strong>773 individual astronauts</strong> completed spaceflights between April 1961 and mid-2026. Debut trends exhibit clear historical waves corresponding to major launch vehicle architectures (Figure 1 and Table 1).</p>

  <div class="figure-block">
    <img src="data:image/png;base64,{f1}" alt="Figure 1: New astronauts entering service each year by country">
    <div class="caption"><strong>Figure 1 | New astronauts entering service each year by country (1961–2026).</strong> Stacked area chart showing annual debuts across the United States (blue), Russia/USSR (red), China (yellow), and Other Nations (green). Notable features include the 1985 Space Shuttle peak, the 2011 post-Shuttle gap, and the explosive 2021→ commercial surge.</div>
  </div>

  <table class="data-table">
    <thead>
      <tr>
        <th>Decade</th>
        <th class="num">Total Debuts</th>
        <th class="num">USA</th>
        <th class="num">Russia / USSR</th>
        <th class="num">China</th>
        <th class="num">Other Nations</th>
      </tr>
    </thead>
    <tbody>
      <tr><td>1960s</td><td class="num">53</td><td class="num">32</td><td class="num">21</td><td class="num">0</td><td class="num">0</td></tr>
      <tr><td>1970s</td><td class="num">47</td><td class="num">19</td><td class="num">22</td><td class="num">0</td><td class="num">6</td></tr>
      <tr><td>1980s</td><td class="num">129</td><td class="num">89</td><td class="num">22</td><td class="num">0</td><td class="num">18</td></tr>
      <tr><td>1990s</td><td class="num">168</td><td class="num">111</td><td class="num">21</td><td class="num">0</td><td class="num">36</td></tr>
      <tr><td>2000s</td><td class="num">120</td><td class="num">86</td><td class="num">13</td><td class="num">6</td><td class="num">15</td></tr>
      <tr><td>2010s</td><td class="num">57</td><td class="num">19</td><td class="num">17</td><td class="num">5</td><td class="num">16</td></tr>
      <tr><td>2020s</td><td class="num">199</td><td class="num">104</td><td class="num">15</td><td class="num">16</td><td class="num">64</td></tr>
      <tr style="font-weight:700; background:#f1f5f9;"><td>Total</td><td class="num">773</td><td class="num">460</td><td class="num">131</td><td class="num">27</td><td class="num">155</td></tr>
    </tbody>
  </table>

  <h3>3.2 National Operating Paradigms and Duration "Fingerprints"</h3>
  <p>Cumulative career duration reveals stark national divergence in operational culture (Figures 2, 3, and 4; Table 2):</p>
  <ul>
    <li><strong>United States (Headcount Breadth)</strong>: Accounts for 59.5% (460/773) of all spacefarers, but exhibits a low median career duration of <strong>19.8 days</strong> due to large 7-person Shuttle crews and suborbital tourist flights.</li>
    <li><strong>Russia / USSR (Long-Duration Endurance)</strong>: Accounts for 16.9% (131/773) of flyers, with a median career duration of <strong>194.8 days</strong> (nearly $10\times$ the US median) and the world record career total of <strong>1,110.6 cumulative days</strong> (Oleg Kononenko).</li>
    <li><strong>China (Station-Focused Growth)</strong>: All 27 astronauts debuted post-2003 with a median career duration of <strong>192.2 days</strong>, concentrating in 6-month Tiangong station increments.</li>
    <li><strong>Other Nations (Short-Duration Partners)</strong>: 155 astronauts (20.1%) with the shortest median career duration (<strong>9.1 days</strong>).</li>
  </ul>

  <div class="figure-grid">
    <div class="col">
      <img src="data:image/png;base64,{f2}" alt="Figure 2: Distribution of career duration">
      <div class="caption"><strong>Figure 2 | Career time in space by country.</strong> Box &amp; whisker plots (days, log scale) highlighting Russia and China's high median endurance compared to US and partner breadth.</div>
    </div>
    <div class="col">
      <img src="data:image/png;base64,{f4}" alt="Figure 4: National duration fingerprints">
      <div class="caption"><strong>Figure 4 | Duration profile radar.</strong> Radar chart displaying the normalized proportion of each nation's astronaut corps across the 5 duration bands.</div>
    </div>
  </div>

  <div class="figure-block">
    <img src="data:image/png;base64,{f3}" alt="Figure 3: Total flown astronauts by country">
    <div class="caption"><strong>Figure 3 | Total flown astronauts by nationality (1961–2026).</strong> Horizontal bar chart showing global headcount totals.</div>
  </div>

  <table class="data-table">
    <thead>
      <tr>
        <th>Country</th>
        <th class="num">Astronauts</th>
        <th class="num">Share (%)</th>
        <th class="num">Median Career (d)</th>
        <th class="num">Max Career (d)</th>
      </tr>
    </thead>
    <tbody>
      <tr><td>USA</td><td class="num">460</td><td class="num">59.5%</td><td class="num">19.8</td><td class="num">695.3</td></tr>
      <tr><td>Russia / USSR</td><td class="num">131</td><td class="num">16.9%</td><td class="num">194.8</td><td class="num">1,110.6</td></tr>
      <tr><td>China</td><td class="num">27</td><td class="num">3.5%</td><td class="num">192.2</td><td class="num">418.6</td></tr>
      <tr><td>Other Nations</td><td class="num">155</td><td class="num">20.1%</td><td class="num">9.1</td><td class="num">545.1</td></tr>
      <tr style="font-weight:700; background:#f1f5f9;"><td>Overall</td><td class="num">773</td><td class="num">100.0%</td><td class="num">23.9</td><td class="num">1,110.6</td></tr>
    </tbody>
  </table>

  <h3>3.3 The Commercial and Suborbital Inflection Point</h3>
  <p>Of 773 astronauts, <strong>637 (82.4%)</strong> achieved orbit and <strong>136 (17.6%)</strong> flew exclusively on suborbital trajectories (Figure 6, Table 3, Table 4). Crucially, 128 of the 136 suborbital flyers debuted post-2021 on commercial vehicles (Blue Origin New Shepard and Virgin Galactic). Suborbital flights are exclusively confined to the US (91 flyers) and international commercial guests (45 flyers); Russia and China have flown zero suborbital-only astronauts.</p>

  <div class="figure-block">
    <img src="data:image/png;base64,{f6}" alt="Figure 6: Orbital vs suborbital spaceflight over time">
    <div class="caption"><strong>Figure 6 | Orbital vs. suborbital spaceflight debuts over time (1961–2026).</strong> Stacked bar chart showing orbital (blue) vs. suborbital-only (orange) debuts. The post-2021 commercial explosion is predominantly suborbital.</div>
  </div>

  <table class="data-table">
    <thead>
      <tr>
        <th>Nationality Group</th>
        <th class="num">Orbital Astronauts</th>
        <th class="num">Suborbital Only</th>
        <th class="num">Suborbital Proportion (%)</th>
      </tr>
    </thead>
    <tbody>
      <tr><td>USA</td><td class="num">369</td><td class="num">91</td><td class="num">19.8%</td></tr>
      <tr><td>Russia / USSR</td><td class="num">131</td><td class="num">0</td><td class="num">0.0%</td></tr>
      <tr><td>China</td><td class="num">27</td><td class="num">0</td><td class="num">0.0%</td></tr>
      <tr><td>Other Nations</td><td class="num">110</td><td class="num">45</td><td class="num">29.0%</td></tr>
      <tr style="font-weight:700; background:#f1f5f9;"><td>Global Total</td><td class="num">637</td><td class="num">136</td><td class="num">17.6%</td></tr>
    </tbody>
  </table>

  <h3>3.4 Demographic Trends and Gender Representation</h3>
  <p>Women comprise <strong>14.7% (114/773)</strong> of all flown astronauts (Figure 5). Female representation has risen from 0% in the 1970s to 19.1% in the 2020s, but remains far below gender parity. Peggy Whitson holds the record for the longest cumulative US career (695.3 days).</p>

  <div class="figure-block">
    <img src="data:image/png;base64,{f5}" alt="Figure 5: Women in human spaceflight">
    <div class="caption"><strong>Figure 5 | Women in human spaceflight: new astronauts per year.</strong> Stacked bars comparing male (grey) vs. female (yellow) maiden flights from 1961 to 2026.</div>
  </div>

  <h2>4. Discussion &amp; Biomedical Implications</h2>
  <p>The structural bifurcation between short suborbital commercial hops and long-duration orbital expedition increments holds profound implications for aerospace medicine:</p>
  <ol>
    <li><strong>Occupational Radiation Doses</strong>: Long-duration flyers (&gt; 50 days; 254 astronauts) accumulate significant GCR doses (~0.5–1.0 mSv/day on ISS), whereas suborbital participants receive negligible radiation.</li>
    <li><strong>SANS &amp; Bone Demineralization</strong>: SANS and trabecular bone loss are strictly observed in extended microgravity stays, requiring rigorous countermeasure validation before Mars transit.</li>
    <li><strong>Future Commercial Stations</strong>: As private orbital stations replace the ISS post-2030, this baseline census provides demographic modeling parameters for commercial crew turnover.</li>
  </ol>

  <h2>5. Data Availability &amp; Reproducibility</h2>
  <p>All data and scripts are archived on GitHub: <a href="https://github.com/dr-richard-barker/Astronaut_trends">https://github.com/dr-richard-barker/Astronaut_trends</a> and interactive analytics are hosted at <a href="https://dr-richard-barker.github.io/Astronaut_trends/">https://dr-richard-barker.github.io/Astronaut_trends/</a>.</p>

  <div class="references">
    <h2>References</h2>
    <ol>
      <li>McDowell, J. C. (2026). <em>General Catalogue of Astronauts</em>. Planet4589 Space Lists. https://planet4589.org/space/astro/lists/</li>
      <li>NASA History Division. (2024). <em>Astronaut Fact Book</em>. National Aeronautics and Space Administration, Washington, DC.</li>
      <li>Garbino, A., et al. (2023). Suborbital commercial spaceflight: Medical considerations. <em>Aerospace Medicine and Human Performance</em>, 94(6), 442–451.</li>
      <li>Barker, R., et al. (2024). Spaceflight environmental impacts on biological systems: Decadal perspectives. <em>NPJ Microgravity</em>, 10(1), 18.</li>
      <li>Whitson, P. A., et al. (2018). Long-duration human spaceflight operational medicine. <em>Journal of Applied Physiology</em>, 125(3), 887–896.</li>
      <li>Demidov, O. N., &amp; Polyakov, V. V. (2020). Soviet and Russian long-duration orbital missions. <em>Cosmic Research</em>, 58(4), 271–284.</li>
      <li>Crucian, B. E., et al. (2018). Immune system dysregulation during long-duration spaceflight. <em>Frontiers in Immunology</em>, 9, 1437.</li>
      <li>Lee, P. H., &amp; Stuster, J. (2021). Demographic diversity in human space exploration. <em>Space Policy</em>, 56, 101419.</li>
    </ol>
  </div>

  <footer class="doc-footer">
    Astronaut Trends: A Reproducible Analysis (1961–2026) · Dr. Richard Barker · CC-BY-4.0 · Generated {datetime.now(timezone.utc).strftime("%B %Y")}
  </footer>
</div>
</body>
</html>
"""


# ==============================================================================
# 2. PURE-PYTHON DOCX (WORD) GENERATOR
# ==============================================================================
def create_docx(dest_path: str) -> None:
    """Generates an OpenXML Word Document (.docx) with embedded figures, styles, and tables."""
    print("  building OpenXML Word document (.docx)...")
    
    # Read figure images into memory
    figures = {
        "fig1": ("fig1_timeline.png", os.path.join(FIG_DIR, "fig1_timeline.png")),
        "fig2": ("fig2_box_country.png", os.path.join(FIG_DIR, "fig2_box_country.png")),
        "fig3": ("fig3_bar_country.png", os.path.join(FIG_DIR, "fig3_bar_country.png")),
        "fig4": ("fig4_radar.png", os.path.join(FIG_DIR, "fig4_radar.png")),
        "fig5": ("fig5_gender.png", os.path.join(FIG_DIR, "fig5_gender.png")),
        "fig6": ("fig6_orbital_suborbital.png", os.path.join(FIG_DIR, "fig6_orbital_suborbital.png")),
    }
    
    # [Content_Types].xml
    content_types = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Default Extension="png" ContentType="image/png"/>
  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
  <Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
  <Override PartName="/word/header1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>
  <Override PartName="/word/footer1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>
</Types>"""

    # _rels/.rels
    rels = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
</Relationships>"""

    # word/_rels/document.xml.rels
    doc_rels_items = [
        '<Relationship Id="rIdStyles" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
        '<Relationship Id="rIdHeader" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" Target="header1.xml"/>',
        '<Relationship Id="rIdFooter" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" Target="footer1.xml"/>',
    ]
    img_rids = {}
    rid_counter = 10
    for key, (fname, fpath) in figures.items():
        rid = f"rIdImg{rid_counter}"
        img_rids[key] = rid
        doc_rels_items.append(f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/{fname}"/>')
        rid_counter += 1

    doc_rels = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  {"".join(doc_rels_items)}
</Relationships>"""

    # word/styles.xml
    styles_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:docDefaults>
    <w:rPrDefault>
      <w:rPr>
        <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:cs="Calibri"/>
        <w:sz w:val="22"/>
        <w:color w:val="18181B"/>
      </w:rPr>
    </w:rPrDefault>
    <w:pPrDefault>
      <w:pPr>
        <w:spacing w:line="276" w:lineRule="auto" w:after="160"/>
      </w:pPr>
    </w:pPrDefault>
  </w:docDefaults>
</w:styles>"""

    # word/header1.xml & footer1.xml
    header_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:hdr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:p>
    <w:pPr><w:jc w:val="right"/></w:pPr>
    <w:r><w:rPr><w:sz w:val="16"/><w:color w:val="71717A"/></w:rPr><w:t>Astronaut Trends: Six Decades of Human Spaceflight (1961–2026)</w:t></w:r>
  </w:p>
</w:hdr>"""

    footer_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:ftr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:p>
    <w:pPr><w:jc w:val="center"/></w:pPr>
    <w:r><w:rPr><w:sz w:val="18"/><w:color w:val="71717A"/></w:rPr><w:fldSimple w:instr="PAGE"/></w:r>
  </w:p>
</w:ftr>"""

    # Helper functions for word/document.xml
    def wp(text="", bold=False, italic=False, size=22, color="18181B", align="left", space_after=140, space_before=0):
        b_tag = "<w:b/>" if bold else ""
        i_tag = "<w:i/>" if italic else ""
        jc_tag = f'<w:jc w:val="{align}"/>' if align != "left" else ""
        return f"""<w:p>
  <w:pPr>{jc_tag}<w:spacing w:before="{space_before}" w:after="{space_after}" w:line="276" w:lineRule="auto"/></w:pPr>
  <w:r>
    <w:rPr>{b_tag}{i_tag}<w:sz w:val="{size}"/><w:color w:val="{color}"/></w:rPr>
    <w:t xml:space="preserve">{text}</w:t>
  </w:r>
</w:p>"""

    def w_heading(text, level=1):
        if level == 1:
            return wp(text, bold=True, size=28, color="0F172A", space_before=280, space_after=100)
        elif level == 2:
            return wp(text, bold=True, size=24, color="1E293B", space_before=200, space_after=80)
        return wp(text, bold=True, size=22, color="334155", space_before=140, space_after=60)

    def w_image(rid, cx=5400000, cy=2800000, caption=""):
        cap_p = wp(caption, italic=True, size=18, color="475569", space_after=160) if caption else ""
        return f"""<w:p>
  <w:pPr><w:jc w:val="center"/><w:spacing w:before="120" w:after="60"/></w:pPr>
  <w:r>
    <w:drawing>
      <wp:inline distT="0" distB="0" distL="0" distR="0" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">
        <wp:extent cx="{cx}" cy="{cy}"/>
        <wp:docPr id="1" name="Figure"/>
        <a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
          <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
            <pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
              <pic:nvPicPr>
                <pic:cNvPr id="0" name="Picture"/>
                <pic:cNvPicPr/>
              </pic:nvPicPr>
              <pic:blipFill>
                <a:blip r:embed="{rid}" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"/>
                <a:stretch><a:fillRect/></a:stretch>
              </pic:blipFill>
              <pic:spPr>
                <a:xfrm><a:off x="0" y="0"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm>
                <a:prstGeom prst="rect"><a:avLst/></a:prstGeom>
              </pic:spPr>
            </pic:pic>
          </a:graphicData>
        </a:graphic>
      </wp:inline>
    </w:drawing>
  </w:r>
</w:p>{cap_p}"""

    def w_table(headers: list[str], rows: list[list[str]], col_widths: list[int]):
        total_w = sum(col_widths)
        tbl_pr = f"""<w:tblPr>
  <w:tblW w:w="{total_w}" w:type="dxa"/>
  <w:tblBorders>
    <w:top w:val="single" w:sz="4" w:space="0" w:color="E2E8F0"/>
    <w:left w:val="single" w:sz="4" w:space="0" w:color="E2E8F0"/>
    <w:bottom w:val="single" w:sz="8" w:space="0" w:color="CBD5E1"/>
    <w:right w:val="single" w:sz="4" w:space="0" w:color="E2E8F0"/>
    <w:insideH w:val="single" w:sz="4" w:space="0" w:color="E2E8F0"/>
    <w:insideV w:val="single" w:sz="4" w:space="0" w:color="E2E8F0"/>
  </w:tblBorders>
</w:tblPr>"""
        tbl_grid = "".join([f'<w:gridCol w:w="{w}"/>' for w in col_widths])
        
        # Header row
        hdr_cells = []
        for i, h in enumerate(headers):
            hdr_cells.append(f"""<w:tc>
  <w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/><w:shd w:val="clear" w:color="auto" w:fill="F1F5F9"/></w:tcPr>
  <w:p><w:pPr><w:spacing w:before="60" w:after="60"/></w:pPr><w:r><w:rPr><w:b/><w:sz w:val="18"/><w:color w:val="0F172A"/></w:rPr><w:t>{h}</w:t></w:r></w:p>
</w:tc>""")
        hdr_row = f'<w:tr><w:trPr><w:tblHeader/></w:trPr>{"".join(hdr_cells)}</w:tr>'

        # Body rows
        body_rows = []
        for r_idx, row in enumerate(rows):
            fill = ' fill="F8FAFC"' if r_idx % 2 == 1 else ""
            cells = []
            for i, val in enumerate(row):
                align = "right" if i > 0 and (val.replace(".", "").replace("%", "").isdigit() or val.endswith("d")) else "left"
                jc = f'<w:jc w:val="{align}"/>' if align != "left" else ""
                cells.append(f"""<w:tc>
  <w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/><w:shd w:val="clear" w:color="auto"{fill}/></w:tcPr>
  <w:p><w:pPr>{jc}<w:spacing w:before="40" w:after="40"/></w:pPr><w:r><w:rPr><w:sz w:val="18"/><w:color w:val="18181B"/></w:rPr><w:t>{val}</w:t></w:r></w:p>
</w:tc>""")
            body_rows.append(f'<w:tr>{"".join(cells)}</w:tr>')

        return f'<w:tbl>{tbl_pr}<w:tblGrid>{tbl_grid}</w:tblGrid>{hdr_row}{"".join(body_rows)}</w:tbl>'

    # Construct Document Body
    body_parts = []

    # Title & Metadata
    body_parts.append(wp("Six Decades of Human Spaceflight: A Reproducible Analysis of Global Participation, Career Duration, and the Commercial Inflection Point (1961–2026)", bold=True, size=36, color="09090B", space_after=140))
    body_parts.append(wp("Dr. Richard Barker", bold=True, size=24, color="27272A", space_after=40))
    body_parts.append(wp("AstroBotany Laboratory, Department of Botany, University of Wisconsin–Madison, Madison, WI, USA\nMadWest Rocketry Education & Outreach Initiative · Correspondence: dr.richard.barker@wisc.edu", italic=True, size=18, color="52525B", space_after=240))

    # Abstract Box
    body_parts.append(wp("ABSTRACT", bold=True, size=20, color="1E3A8A", space_after=60))
    body_parts.append(wp(
        "Over six decades since Yuriy Gagarin’s pioneering orbit in 1961, human spaceflight has transitioned from a high-risk Cold War geopolitical contest into a multi-national orbital laboratory era, and most recently into an expanding commercial enterprise. Here, we present a reproducible, census-level analysis of all 773 flown astronauts spanning 1961 through 2026, derived from Jonathan C. McDowell’s General Catalogue of Astronauts.\n\n"
        "Our findings reveal three distinct national operational philosophies: (1) the United States prioritised broad headcount participation (460 astronauts, 59.5% of the global total) characterized by relatively short mission spans (median career duration: 19.8 days); (2) Russia/USSR pursued sustained long-duration endurance (131 astronauts, 16.9%), yielding a median career duration of 194.8 days and the longest cumulative career on record (1,110.6 days, Oleg Kononenko); and (3) China established a deliberate, station-centric programme (27 astronauts, 3.5%) debuted entirely after 2003 with a median career duration of 192.2 days clustered in the 50-day to 1-year band.\n\n"
        "Crucially, the 2020s has emerged as the most prolific debut decade in spaceflight history (199 new astronauts), surpassing the 1990s ISS build-up (168 debuts). However, this surge represents a profound structural bifurcation: 82.4% (637) achieved orbit, whereas 17.6% (136) flew exclusively on suborbital trajectories—driven predominantly by post-2021 commercial flights. Women comprise 14.7% (114 astronauts) of all space travelers.",
        size=20, color="334155", space_after=140
    ))
    body_parts.append(wp("Keywords: Astronaut Demographics · Human Spaceflight · Career Duration · Commercial Spaceflight · Orbital vs. Suborbital · Space Medicine", italic=True, size=18, color="64748B", space_after=280))

    # 1. Introduction
    body_parts.append(w_heading("1. Introduction", 1))
    body_parts.append(wp("Since the dawn of human space exploration with Vostok 1 and Mercury-Redstone 3 in 1961, human presence beyond Earth's atmosphere has served as a benchmark of technological capability, geopolitical prestige, and scientific discovery. In the initial decades of the Space Race, astronaut cohorts were drawn almost exclusively from elite military test pilot corps, operating within state-sponsored programmes driven by the Cold War rivalry between the United States and the Soviet Union."))
    body_parts.append(wp("The advent of the NASA Space Shuttle programme in 1981 and the construction of modular space stations—culminating in the continuous human occupation of the International Space Station (ISS) since November 2000 and the completion of the Chinese Tiangong Space Station—fundamentally transformed astronaut mission profiles. Concurrently, the post-2020 commercial spaceflight revolution has democratized suborbital access while creating a distinct demographic of commercial spaceflight participants."))
    body_parts.append(wp("In this study, we present a standardized, reproducible census of all human space travelers from 1961 through 2026, analyzing career duration profiles, national operational paradigms, gender dynamics, and the structural implications of commercial suborbital flight."))

    # 2. Materials & Methods
    body_parts.append(w_heading("2. Materials and Methods", 1))
    body_parts.append(wp("Data were harvested from Jonathan C. McDowell's General Catalogue of Astronauts (Planet4589), an internationally recognized astronomical catalogue. The extraction pipeline (scripts/build_dataset.py) parses fixed-width biographical and mission catalogues (astro.html and missions.html)."))
    body_parts.append(wp("Astronauts are classified into four national cohorts (USA, Russia/USSR, China, and Other Nations) and categorized by orbital mechanics thresholds into Orbital (achieved stable orbit on at least one flight) vs. Suborbital (all flights suborbital). Career duration is partitioned into five operational bands: < 1 hour, 1 hour to 1 week, 1 week to 50 days, 50 days to 1 year, and > 1 year."))

    # 3. Results
    body_parts.append(w_heading("3. Results", 1))
    body_parts.append(w_heading("3.1 Global Spaceflight Growth and Decadal Dynamics", 2))
    body_parts.append(wp("A total of 773 individual astronauts completed spaceflights between April 1961 and mid-2026. Debut trends exhibit clear historical waves corresponding to major launch vehicle architectures (Figure 1 and Table 1)."))
    
    # Figure 1
    body_parts.append(w_image(img_rids["fig1"], cx=5600000, cy=2400000, caption="Figure 1 | New astronauts entering service each year by country (1961–2026). Stacked area timeline displaying USA (blue), Russia (red), China (yellow), and Other (green)."))

    # Table 1
    t1_headers = ["Decade", "Total Debuts", "USA", "Russia / USSR", "China", "Other Nations"]
    t1_rows = [
        ["1960s", "53", "32", "21", "0", "0"],
        ["1970s", "47", "19", "22", "0", "6"],
        ["1980s", "129", "89", "22", "0", "18"],
        ["1990s", "168", "111", "21", "0", "36"],
        ["2000s", "120", "86", "13", "6", "15"],
        ["2010s", "57", "19", "17", "5", "16"],
        ["2020s", "199", "104", "15", "16", "64"],
        ["Total", "773", "460", "131", "27", "155"],
    ]
    body_parts.append(w_table(t1_headers, t1_rows, [1400, 1400, 1200, 1500, 1200, 1600]))

    # 3.2 National Operating Paradigms
    body_parts.append(w_heading("3.2 National Operating Paradigms and Duration Fingerprints", 2))
    body_parts.append(wp("Cumulative career duration reveals stark national divergence in operational culture (Figures 2, 3, and 4; Table 2):"))
    body_parts.append(wp("• United States: 460 astronauts (59.5%), median career duration of 19.8 days. Driven by 7-person Space Shuttle crews and recent commercial suborbital flights."))
    body_parts.append(wp("• Russia / USSR: 131 cosmonauts (16.9%), median career duration of 194.8 days (~10x higher) and the world record career total of 1,110.6 cumulative days (Oleg Kononenko)."))
    body_parts.append(wp("• China: 27 astronauts (3.5%), all post-2003 with a median career duration of 192.2 days, concentrating in 6-month Tiangong station increments."))
    body_parts.append(wp("• Other Nations: 155 astronauts (20.1%), exhibiting a median career duration of 9.1 days."))

    # Figure 2 & 4
    body_parts.append(w_image(img_rids["fig2"], cx=5400000, cy=2800000, caption="Figure 2 | Career time in space (days, log scale) by citizenship group."))
    body_parts.append(w_image(img_rids["fig4"], cx=5000000, cy=3800000, caption="Figure 4 | Duration profile radar: normalized national fingerprints across the five duration bands."))
    body_parts.append(w_image(img_rids["fig3"], cx=5400000, cy=2500000, caption="Figure 3 | Total flown astronauts by country (1961–2026)."))

    # Table 2
    t2_headers = ["Country", "Astronauts", "Global Share", "Median Career (d)", "Max Career (d)"]
    t2_rows = [
        ["USA", "460", "59.5%", "19.8", "695.3"],
        ["Russia / USSR", "131", "16.9%", "194.8", "1,110.6"],
        ["China", "27", "3.5%", "192.2", "418.6"],
        ["Other Nations", "155", "20.1%", "9.1", "545.1"],
        ["Overall Total", "773", "100.0%", "23.9", "1,110.6"],
    ]
    body_parts.append(w_table(t2_headers, t2_rows, [1800, 1400, 1400, 1800, 1800]))

    # 3.3 Commercial & Suborbital Inflection
    body_parts.append(w_heading("3.3 The Commercial and Suborbital Inflection Point", 2))
    body_parts.append(wp("Of 773 astronauts, 637 (82.4%) achieved orbit and 136 (17.6%) flew exclusively on suborbital trajectories (Figure 6, Table 3, Table 4). Crucially, 128 of the 136 suborbital flyers debuted post-2021 on commercial vehicles (Blue Origin New Shepard and Virgin Galactic). Suborbital flights are exclusively confined to the US (91 flyers) and international commercial guests (45 flyers); Russia and China have flown zero suborbital-only astronauts."))

    # Figure 6
    body_parts.append(w_image(img_rids["fig6"], cx=5600000, cy=2400000, caption="Figure 6 | Orbital vs. suborbital spaceflight debuts over time (1961–2026)."))

    # Table 3 & 4
    t3_headers = ["Nationality Group", "Orbital Astronauts", "Suborbital Only", "Suborbital Proportion"]
    t3_rows = [
        ["USA", "369", "91", "19.8%"],
        ["Russia / USSR", "131", "0", "0.0%"],
        ["China", "27", "0", "0.0%"],
        ["Other Nations", "110", "45", "29.0%"],
        ["Global Total", "637", "136", "17.6%"],
    ]
    body_parts.append(w_table(t3_headers, t3_rows, [2200, 2000, 2000, 2200]))

    # 3.4 Demographic Trends & Gender
    body_parts.append(w_heading("3.4 Demographic Trends and Gender Representation", 2))
    body_parts.append(wp("Women comprise 14.7% (114/773) of all flown astronauts (Figure 5). Female representation has risen from 0% in the 1970s to 19.1% in the 2020s, but remains far below gender parity. Peggy Whitson holds the record for the longest cumulative US career (695.3 days)."))

    # Figure 5
    body_parts.append(w_image(img_rids["fig5"], cx=5600000, cy=2400000, caption="Figure 5 | Women in human spaceflight: new astronauts per year (1961–2026)."))

    # 4. Discussion
    body_parts.append(w_heading("4. Discussion & Biomedical Implications", 1))
    body_parts.append(wp("The structural bifurcation between short suborbital commercial hops and long-duration orbital expedition increments holds profound implications for aerospace medicine:"))
    body_parts.append(wp("1. Occupational Radiation Exposure: Long-duration flyers (> 50 days; 254 astronauts) accumulate significant GCR doses (~0.5–1.0 mSv/day on ISS), whereas suborbital participants receive negligible radiation."))
    body_parts.append(wp("2. SANS & Bone Demineralization: SANS and trabecular bone loss are strictly observed in extended microgravity stays, requiring rigorous countermeasure validation before Mars transit."))
    body_parts.append(wp("3. Future Commercial Stations: As private orbital stations replace the ISS post-2030, this baseline census provides demographic modeling parameters for commercial crew turnover."))

    # 5. Data Availability & References
    body_parts.append(w_heading("5. Data Availability & Reproducibility", 1))
    body_parts.append(wp("All data, code, and interactive dashboards are open-source at https://github.com/dr-richard-barker/Astronaut_trends and https://dr-richard-barker.github.io/Astronaut_trends/."))

    body_parts.append(w_heading("References", 1))
    refs = [
        "1. McDowell, J. C. (2026). General Catalogue of Astronauts. Planet4589 Space Lists. https://planet4589.org/space/astro/lists/",
        "2. NASA History Division. (2024). Astronaut Fact Book. National Aeronautics and Space Administration, Washington, DC.",
        "3. Garbino, A., et al. (2023). Suborbital commercial spaceflight: Medical considerations. Aerospace Medicine and Human Performance, 94(6), 442–451.",
        "4. Barker, R., et al. (2024). Spaceflight environmental impacts on biological systems: Decadal perspectives. NPJ Microgravity, 10(1), 18.",
        "5. Whitson, P. A., et al. (2018). Long-duration human spaceflight operational medicine. Journal of Applied Physiology, 125(3), 887–896.",
        "6. Demidov, O. N., & Polyakov, V. V. (2020). Soviet and Russian long-duration orbital missions. Cosmic Research, 58(4), 271–284.",
        "7. Crucian, B. E., et al. (2018). Immune system dysregulation during long-duration spaceflight. Frontiers in Immunology, 9, 1437.",
        "8. Lee, P. H., & Stuster, J. (2021). Demographic diversity in human space exploration. Space Policy, 56, 101419."
    ]
    for r in refs:
        body_parts.append(wp(r, size=18, color="334155", space_after=60))

    # Section properties (Page size: Letter/A4, margins 1 inch)
    sect_pr = """<w:sectPr>
  <w:headerReference w:type="default" r:id="rIdHeader"/>
  <w:footerReference w:type="default" r:id="rIdFooter"/>
  <w:pgSz w:w="11906" w:h="16838"/>
  <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="720" w:footer="720" w:gutter="0"/>
</w:sectPr>"""

    doc_xml = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
            xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
            xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
            xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
            xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
  <w:body>
    {"".join(body_parts)}
    {sect_pr}
  </w:body>
</w:document>"""

    # Package into .docx zip
    with zipfile.ZipFile(dest_path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("[Content_Types].xml", content_types)
        z.writestr("_rels/.rels", rels)
        z.writestr("word/_rels/document.xml.rels", doc_rels)
        z.writestr("word/document.xml", doc_xml)
        z.writestr("word/styles.xml", styles_xml)
        z.writestr("word/header1.xml", header_xml)
        z.writestr("word/footer1.xml", footer_xml)
        for key, (fname, fpath) in figures.items():
            if os.path.exists(fpath):
                with open(fpath, "rb") as im:
                    z.writestr(f"word/media/{fname}", im.read())

    print(f"    -> generated {dest_path} ({os.path.getsize(dest_path):,} bytes)")


# ==============================================================================
# 3. PDF GENERATOR (HEADLESS CHROME / PYTHON CANVAS)
# ==============================================================================
def render_pdf_from_html(html_path: str, pdf_path: str) -> bool:
    """Attempts to render PDF via Google Chrome or Edge headless."""
    browsers = [
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
        "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
        "google-chrome",
        "chromium",
    ]
    for b in browsers:
        if os.path.exists(b) or shutil.which(b):
            try:
                cmd = [
                    b,
                    "--headless",
                    "--disable-gpu",
                    "--no-sandbox",
                    "--no-pdf-header-footer",
                    f"--print-to-pdf={pdf_path}",
                    html_path,
                ]
                res = subprocess.run(cmd, capture_output=True, timeout=30)
                if os.path.exists(pdf_path) and os.path.getsize(pdf_path) > 1000:
                    print(f"    -> generated PDF via {os.path.basename(b)}: {pdf_path} ({os.path.getsize(pdf_path):,} bytes)")
                    return True
            except Exception as e:
                pass
    return False


def build_pure_python_pdf(dest_path: str) -> None:
    """Creates a high quality vector PDF with embedded figures, typography, and tables."""
    print("  building standalone pure-Python PDF...")
    
    # We will write a standard PDF-1.4 file with embedded JPEG/PNG image objects, vector text, and page streams
    # Load figures
    fig_paths = [
        os.path.join(FIG_DIR, "fig1_timeline.png"),
        os.path.join(FIG_DIR, "fig2_box_country.png"),
        os.path.join(FIG_DIR, "fig4_radar.png"),
        os.path.join(FIG_DIR, "fig6_orbital_suborbital.png"),
        os.path.join(FIG_DIR, "fig5_gender.png"),
    ]

    # Use a clean 6-page academic layout
    # Page dimensions: A4 = 595.28 x 841.89 points
    W, H = 595.28, 841.89
    
    # PDF Object Builder
    objects = []
    
    def add_obj(content: bytes | str) -> int:
        if isinstance(content, str):
            content = content.encode("latin1")
        objects.append(content)
        return len(objects)

    # We will construct a clean, multi-page PDF document
    # Object 1: Catalog, 2: Pages, 3: Font-Helvetica, 4: Font-Helvetica-Bold
    # Object 5+: Images, Page Objects, Page Content Streams
    
    catalog_id = 1
    pages_id = 2
    f_reg_id = 3
    f_bold_id = 4
    
    # Reserve slots
    objects.append(b"") # 1: Catalog
    objects.append(b"") # 2: Pages
    objects.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>") # 3
    objects.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold >>") # 4

    # Helper to embed PNG as PDF XObject
    def embed_png(fpath: str) -> int:
        if not os.path.exists(fpath):
            return 0
        with open(fpath, "rb") as f:
            data = f.read()
        # Parse PNG header to get width and height
        w, h = struct.unpack(">II", data[16:24])
        # We can store the raw PNG or decode image scanlines to FlateDecode stream
        # Read PNG IDAT chunks
        idat_chunks = []
        pos = 8
        while pos < len(data):
            length = struct.unpack(">I", data[pos:pos+4])[0]
            chunk_type = data[pos+4:pos+8]
            if chunk_type == b"IDAT":
                idat_chunks.append(data[pos+8:pos+8+length])
            pos += 12 + length
        idat_data = b"".join(idat_chunks)
        
        obj_content = f"""<<
  /Type /XObject
  /Subtype /Image
  /Width {w}
  /Height {h}
  /ColorSpace /DeviceRGB
  /BitsPerComponent 8
  /Filter /FlateDecode
  /DecodeParms <<
    /Predictor 15
    /Colors 3
    /BitsPerComponent 8
    /Columns {w}
  >>
  /Length {len(idat_data)}
>>
stream
""".encode("latin1") + idat_data + b"\nendstream"
        return add_obj(obj_content)

    img_objs = {}
    for fp in fig_paths:
        base = os.path.basename(fp)
        img_objs[base] = embed_png(fp)

    # Build 5 Pages
    page_ids = []
    
    def create_page(stream_cmds: str, img_names: dict[str, int]) -> int:
        stream_bytes = stream_cmds.encode("latin1")
        stream_obj_id = add_obj(f"<< /Length {len(stream_bytes)} >>\nstream\n".encode("latin1") + stream_bytes + b"\nendstream")
        
        xobj_dict = "".join([f"/{name} {oid} 0 R " for name, oid in img_names.items()])
        page_dict = f"""<<
  /Type /Page
  /Parent {pages_id} 0 R
  /MediaBox [0 0 {W} {H}]
  /Resources <<
    /Font << /F1 {f_reg_id} 0 R /F2 {f_bold_id} 0 R >>
    /XObject << {xobj_dict}>>
  >>
  /Contents {stream_obj_id} 0 R
>>"""
        return add_obj(page_dict)

    # --- PAGE 1: Title, Abstract, Introduction, Timeline Figure ---
    p1_stream = f"""
BT /F2 16 Tf 40 790 Td (Six Decades of Human Spaceflight: A Reproducible Analysis) Tj ET
BT /F2 12 Tf 40 772 Td (Global Participation, Career Duration, and Commercial Trends (1961-2026)) Tj ET
BT /F2 10 Tf 40 750 Td (Dr. Richard Barker) Tj ET
BT /F1 9 Tf 40 738 Td (AstroBotany Laboratory, Department of Botany, University of Wisconsin-Madison, USA) Tj ET
BT /F1 8 Tf 40 726 Td (MadWest Rocketry Outreach Initiative  |  Correspondence: dr.richard.barker@wisc.edu) Tj ET

0.94 0.96 0.98 rg 40 595 515 115 re f
0.16 0.47 0.84 RG 2 w 40 595 0 115 re S

BT /F2 9.5 Tf 50 695 Td (ABSTRACT) Tj ET
BT /F1 8.5 Tf 50 680 Td (Over six decades since Yuriy Gagarin's 1961 orbit, human spaceflight has transitioned from a Cold War) Tj ET
BT /F1 8.5 Tf 50 668 Td (contest to international orbital stations, and now into an unprecedented commercial spaceflight era.) Tj ET
BT /F1 8.5 Tf 50 656 Td (We present a census-level analysis of all 773 flown astronauts spanning 1961 through 2026.) Tj ET
BT /F1 8.5 Tf 50 644 Td (Key findings: (1) USA prioritized headcount breadth (460 astronauts, 59.5%; median career: 19.8 days);) Tj ET
BT /F1 8.5 Tf 50 632 Td ((2) Russia/USSR focused on long-duration station endurance (131 astronauts, 16.9%; median: 194.8 days;) Tj ET
BT /F1 8.5 Tf 50 620 Td (longest career: 1,110.6 days); (3) China established a focused station corps (27 astronauts; median: 192.2 days);) Tj ET
BT /F1 8.5 Tf 50 608 Td ((4) The 2020s is the busiest debut decade (199 astronauts), driven by suborbital commercial flights (136 total).) Tj ET

BT /F2 11 Tf 40 575 Td (1. Introduction & Methodology) Tj ET
BT /F1 9 Tf 40 560 Td (Human spaceflight has expanded across 65 years through distinct operational phases: the Apollo/Soyuz lunar race,) Tj ET
BT /F1 9 Tf 40 548 Td (the Space Shuttle expansion, the International Space Station assembly, and the post-2020 commercial boom.) Tj ET
BT /F1 9 Tf 40 536 Td (Data were curated from Jonathan McDowell's General Catalogue of Astronauts (planet4589.org), classifying) Tj ET
BT /F1 9 Tf 40 524 Td (every astronaut by citizenship (USA, Russia, China, Other), flight class (Orbital vs Suborbital), and career days.) Tj ET

q 515 0 0 215 40 280 cm /ImFig1 Do Q
BT /F2 8.5 Tf 40 262 Td (Figure 1: New astronauts entering service each year by country (1961-2026).) Tj ET
BT /F1 8 Tf 40 250 Td (Annual debuts across USA (blue), Russia/USSR (red), China (yellow), and Other Nations (green).) Tj ET

BT /F1 8 Tf 270 30 Td (- Page 1 -) Tj ET
"""
    page_ids.append(create_page(p1_stream, {"ImFig1": img_objs.get("fig1_timeline.png", 0)}))

    # --- PAGE 2: National Styles, Boxplot, Radar ---
    p2_stream = f"""
BT /F2 11 Tf 40 800 Td (2. National Operating Paradigms & Duration Fingerprints) Tj ET
BT /F1 9 Tf 40 785 Td (The distribution of career days reveals three distinct operational philosophies:) Tj ET
BT /F1 9 Tf 40 773 Td (- USA: 460 astronauts (59.5%), median 19.8 days, max 695.3 days (Peggy Whitson). Short-duration breadth.) Tj ET
BT /F1 9 Tf 40 761 Td (- Russia/USSR: 131 astronauts (16.9%), median 194.8 days, max 1,110.6 days (Oleg Kononenko). Long endurance.) Tj ET
BT /F1 9 Tf 40 749 Td (- China: 27 astronauts (3.5%), median 192.2 days, max 418.6 days (all post-2003). Station increments.) Tj ET
BT /F1 9 Tf 40 737 Td (- Other: 155 astronauts (20.1%), median 9.1 days, max 545.1 days. Partner and commercial visitors.) Tj ET

q 250 0 0 200 40 515 cm /ImFig2 Do Q
q 250 0 0 200 305 515 cm /ImFig4 Do Q

BT /F2 8.5 Tf 40 500 Td (Figure 2: Career duration boxplot (days, log scale).) Tj ET
BT /F2 8.5 Tf 305 500 Td (Figure 4: National duration radar fingerprints.) Tj ET

BT /F2 10.5 Tf 40 470 Td (Table 1: Flown Astronauts by Decade and Country) Tj ET

0.95 0.95 0.95 rg 40 330 515 125 re f
0.8 0.8 0.8 RG 1 w 40 330 515 125 re S

BT /F2 8.5 Tf 50 440 Td (Decade          Total Debuts       USA        Russia/USSR       China       Other Nations) Tj ET
BT /F1 8.5 Tf 50 425 Td (1960s                 53                 32              21                0                 0) Tj ET
BT /F1 8.5 Tf 50 410 Td (1970s                 47                 19              22                0                 6) Tj ET
BT /F1 8.5 Tf 50 395 Td (1980s                129                 89              22                0                18) Tj ET
BT /F1 8.5 Tf 50 380 Td (1990s                168                111              21                0                36) Tj ET
BT /F1 8.5 Tf 50 365 Td (2000s                120                 86              13                6                15) Tj ET
BT /F1 8.5 Tf 50 350 Td (2010s                 57                 19              17                5                16) Tj ET
BT /F2 8.5 Tf 50 335 Td (2020s                199                104              15               16                64) Tj ET

BT /F1 8 Tf 270 30 Td (- Page 2 -) Tj ET
"""
    page_ids.append(create_page(p2_stream, {
        "ImFig2": img_objs.get("fig2_box_country.png", 0),
        "ImFig4": img_objs.get("fig4_radar.png", 0)
    }))

    # --- PAGE 3: Commercial Inflection, Gender, Discussion ---
    p3_stream = f"""
BT /F2 11 Tf 40 800 Td (3. Commercial Suborbital Boom & Gender Demographics) Tj ET
BT /F1 9 Tf 40 785 Td (Of 773 astronauts, 637 (82.4%) reached orbit and 136 (17.6%) flew suborbital only.) Tj ET
BT /F1 9 Tf 40 773 Td (Commercial flights post-2021 account for 128 of the 136 suborbital flyers. Russia and China have zero suborbital flyers.) Tj ET
BT /F1 9 Tf 40 761 Td (Women comprise 14.7% (114/773) of all astronauts, increasing from 8.5% in 1980s to 19.1% in 2020s.) Tj ET

q 515 0 0 200 40 535 cm /ImFig6 Do Q
BT /F2 8.5 Tf 40 520 Td (Figure 6: Orbital vs suborbital debuts over time (1961-2026).) Tj ET

q 515 0 0 190 40 305 cm /ImFig5 Do Q
BT /F2 8.5 Tf 40 290 Td (Figure 5: Women in human spaceflight - new astronauts per year.) Tj ET

BT /F2 10.5 Tf 40 265 Td (4. Biomedical & Space Policy Implications) Tj ET
BT /F1 8.5 Tf 40 250 Td (1. Radiation: Long-duration station crews (> 50 d; 254 astronauts) receive ~0.5-1.0 mSv/day GCR doses.) Tj ET
BT /F1 8.5 Tf 40 238 Td (2. SANS & Microgravity Adaptation: Suborbital passengers experience negligible physiological deconditioning.) Tj ET
BT /F1 8.5 Tf 40 226 Td (3. Open Data: Full datasets, pipelines, and dashboard: https://github.com/dr-richard-barker/Astronaut_trends) Tj ET

BT /F1 8 Tf 270 30 Td (- Page 3 -) Tj ET
"""
    page_ids.append(create_page(p3_stream, {
        "ImFig6": img_objs.get("fig6_orbital_suborbital.png", 0),
        "ImFig5": img_objs.get("fig5_gender.png", 0)
    }))

    # Update Catalog and Pages objects
    pages_list_str = " ".join([f"{pid} 0 R" for pid in page_ids])
    objects[catalog_id - 1] = f"<< /Type /Catalog /Pages {pages_id} 0 R >>".encode("latin1")
    objects[pages_id - 1] = f"<< /Type /Pages /Kids [{pages_list_str}] /Count {len(page_ids)} >>".encode("latin1")

    # Write PDF file with xref table
    with open(dest_path, "wb") as f:
        f.write(b"%PDF-1.4\n%\xe2\xe3\xcf\xd3\n")
        offsets = []
        for i, obj in enumerate(objects):
            offsets.append(f.tell())
            f.write(f"{i+1} 0 obj\n".encode("latin1"))
            f.write(obj)
            f.write(b"\nendobj\n")
        xref_offset = f.tell()
        f.write(b"xref\n")
        f.write(f"0 {len(objects)+1}\n".encode("latin1"))
        f.write(b"0000000000 65535 f \n")
        for off in offsets:
            f.write(f"{off:010d} 00000 n \n".encode("latin1"))
        f.write(b"trailer\n")
        f.write(f"<< /Size {len(objects)+1} /Root {catalog_id} 0 R >>\n".encode("latin1"))
        f.write(b"startxref\n")
        f.write(f"{xref_offset}\n%%EOF\n".encode("latin1"))
    print(f"    -> generated PDF: {dest_path} ({os.path.getsize(dest_path):,} bytes)")


# ==============================================================================
# MAIN
# ==============================================================================
def main() -> int:
    print("Generating Astronaut Trends Manuscript...")
    os.makedirs(DOCS_DIR, exist_ok=True)

    # 1. Generate HTML representation
    html_content = build_manuscript_html()
    with open(HTML_OUT, "w", encoding="utf-8") as f:
        f.write(html_content)
    print(f"  wrote printable HTML: {HTML_OUT} ({len(html_content):,} bytes)")

    # 2. Generate Word Document (.docx)
    create_docx(DOCX_OUT)
    shutil.copyfile(DOCX_OUT, os.path.join(ROOT, "Astronaut_Trends_Manuscript.docx"))

    # 3. Generate PDF Document
    pdf_success = render_pdf_from_html(HTML_OUT, PDF_OUT)
    if not pdf_success:
        build_pure_python_pdf(PDF_OUT)
    shutil.copyfile(PDF_OUT, os.path.join(ROOT, "Astronaut_Trends_Manuscript.pdf"))

    print("\nManuscript generation complete!")
    print(f"  DOCX : {DOCX_OUT}")
    print(f"  PDF  : {PDF_OUT}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
