# Contributing

Thanks for your interest in Astronaut Trends! This is an open education & outreach
project and contributions are welcome — from data refreshes to new visualisations.

## Refreshing the data

The dataset is derived entirely from the [Planet4589 catalogues](https://planet4589.org/space/astro/lists/),
which are updated periodically by Jonathan McDowell. To pull the latest data:

```bash
pip install -r requirements.txt
python scripts/build_dataset.py     # downloads fresh source, rebuilds everything
python scripts/make_figures.py      # regenerates static figures + tables
```

Commit the updated `data/raw/`, `data/processed/`, `docs/data/astronaut_data.json`,
`figures/`, and `docs/SUMMARY_TABLES.md`. The dashboard picks up new data automatically.

## Ideas we'd love help with

See the **Goals & ideas** section of the [README](README.md) for the roadmap.
High-value, well-scoped items:

- Age-at-first-flight and career-span analyses (birth year is already extracted).
- Vehicle / programme breakdown (Shuttle vs Soyuz vs Shenzhou vs commercial).
- A GitHub Action that refreshes the data on a monthly schedule and redeploys.
- Data-validation checks / tests for the parser.

## Ground rules

- **Keep it reproducible.** Any number shown on the site or in the README should be
  regenerable from `build_dataset.py`. Don't hand-edit `data/processed/` or the JSON bundle.
- **Keep the dashboard self-contained.** Libraries are vendored in `docs/libs/`; please
  don't add external CDN or network dependencies (it must render offline for archival).
- **Respect the source licence.** Underlying data is © Jonathan McDowell / Planet4589;
  always credit it. Our derived data/figures are CC-BY-4.0 and code is MIT.
- **Match the house style** for charts (see the palette in `docs/index.html`): fixed
  categorical colours per country, ordered blue ramp for duration bands, light/dark support.

## Submitting changes

Open an issue to discuss anything substantial first, then send a pull request against
`main` with a clear description. If your change affects the data or figures, note which
scripts you ran.
