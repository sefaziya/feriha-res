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

### Ilk kurulum (sunucu / yeni makine)

```bash
git clone git@github.com:sefaziya/feriha-res.git
cd feriha-res
chmod +x setup.sh run_res.sh run_monitor.sh
./setup.sh
```

`setup.sh` sistem bagimliliklerini (cmake, libglpk-dev, pandoc) ve tum R paketlerini `renv::restore()` ile kurar.
`DESCRIPTION` dosyasindaki acik bagimliliklar (`xgboost`, `shiny` vb.) lockfile'a dahildir.

### Calistirma

```bash
cd /Users/ziya/Downloads/feriha-res
Rscript 02_Core/run_engine.R
Rscript 04_Execution/run_viz_cli.R
Rscript 06_Meta/run_meta.R
```

## tmux

```bash
chmod +x run_res.sh
./run_res.sh
tmux attach -t RES
```

## Monitor (web arayuzu)

Engine ilerlemesini izlemek ve tmux uzerinden yeniden baslatmak icin Shiny dashboard:

```bash
chmod +x run_monitor.sh
./run_monitor.sh
# Yerel tarayici (SSH tuneli ile):
ssh -L 8788:127.0.0.1:8788 root@SUNUCU_IP
# http://127.0.0.1:8788
```

Ayarlar: `07_Monitor/monitor_config.yml` + sunucuda `monitor_config.local.yml` (sifreler git'e gitmez).

Varsayilan `host: 127.0.0.1` — RStudio Server ile cakismaz.

### Uzak erisim (mobil / farkli cihaz, SSH olmadan)

Sunucuda `07_Monitor/monitor_config.local.yml` olusturun (`monitor_config.example.yml` ornegi):

```yaml
host: "0.0.0.0"
auth:
  enabled: true
  username: "res"
  password: "guclu-sifre"
```

Firewall: `ufw allow 8788/tcp` — ardından `./run_monitor.sh` yeniden baslatin.

Tarayicida: `http://116.203.112.26:8788` — kullanici adi + sifre ile giris (RStudio mantigi).

Not: HTTP uzerinden sifre gider; uzun vadede HTTPS (nginx + Let's Encrypt) onerilir. RStudio portu 8787, monitor 8788 — ayri servisler.

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
