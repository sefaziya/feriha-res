# RES — Research Execution System

Modular, config-driven, checkpointed multivariate analysis framework for Turkish academic production data.

## Structure

```
00_Data/          Excel inputs
01_Config/        config.yml
02_Core/          run_engine.R, orchestrator.R
03_Modules/       io, discovery, preprocess, analysis, visualization
04_Execution/     run_field.R, run_viz.R
05_Output/        structured per academic_field / date
06_Meta/          summary_builder.R, meta_analysis.R
08_Reports/       Quarto export templates
```

## Quick Start

```bash
cd /Users/ziya/Downloads/feriha-res
Rscript 02_Core/run_engine.R
Rscript 04_Execution/run_viz.R
Rscript 06_Meta/run_meta.R
```

## tmux

```bash
chmod +x run_res.sh
./run_res.sh
tmux attach -t RES
```

## Config

Edit `01_Config/config.yml` for execution mode, filters, and analysis parameters. No hardcoded academic fields — discovery is automatic from Excel data.

## Source Reference

- **Loglar:** Uc katman:
  - `05_Output/engine_run.log` — tum calistirmalar append (timestamp basliklariyla, onceki silinmez)
  - `05_Output/_logs/engine_{YYYYMMDD_HHMMSS}.log` — engine bazli ayri dosya
  - `05_Output/{field}/{date}/Logs/{field}_{YYYYMMDD_HHMMSS}.log` — alan bazli ayri dosya

- **Hardcoded alan yok:** `academic_field` ve `research_field` degerleri veriden keşfedilir.
- **Analiz metodolojisi:** `feriha-sosyal.R` ile ayni (PERMANOVA, PERMDISP, NMDS, ENVFIT, XGBoost).
- **Tum parametreler:** `01_Config/config.yml` uzerinden yonetilir (`num_vars`, `drop_vars`, `title_filter`, analiz ayarlari).

**Test data (default):** `ferihaXfiloloji.xlsx` (~6k rows, 11 research fields).

**Full-scale data:** replace with `ferihaXsosyal.xlsx` in `00_Data/` for production runs.
