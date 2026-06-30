#!/usr/bin/env bash
# RES — ilk kurulum (Ubuntu/Debian sunucu)
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$PROJECT_DIR"

echo "==> Sistem paketleri (root gerekir)..."
if command -v apt-get >/dev/null 2>&1; then
  sudo apt-get update -qq
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    cmake libglpk-dev pandoc \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
else
  echo "apt-get yok; sistem bagimliliklerini manuel kurun (cmake, libglpk-dev, pandoc)."
fi

echo "==> R paketleri (renv)..."
if ! Rscript -e 'requireNamespace("renv", quietly=TRUE)' 2>/dev/null; then
  Rscript -e 'install.packages("renv", repos="https://cloud.r-project.org")'
fi

Rscript -e 'renv::restore(prompt=FALSE)'

echo "==> Kurulum tamam."
echo "    Veri: 00_Data/*.xlsx"
echo "    Engine: ./run_res.sh  veya  Rscript 02_Core/run_engine.R"
echo "    Monitor: ./run_monitor.sh  (SSH tuneli: ssh -L 8788:127.0.0.1:8788 user@SUNUCU)"
