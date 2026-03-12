#!/usr/bin/env bash

set -e

# --------------------------
# icons (pak-style output)
# --------------------------

info()   { printf "ℹ %s\n" "$1"; }
ok()     { printf "✔ %s\n" "$1"; }
act()    { printf "→ %s\n" "$1"; }
fail()   { printf "✖ %s\n" "$1"; exit 1; }

OS="$(uname -s)"

info "Detecting operating system"

# --------------------------
# macOS
# --------------------------

install_mac() {

  info "macOS detected"

  command -v brew >/dev/null || fail "Homebrew not installed"

  install_if_missing () {

    pkg=$1

    if brew list --versions "$pkg" >/dev/null; then
      ok "$pkg already installed"
    else
      act "Installing $pkg"
      brew install "$pkg"
      ok "$pkg installed"
    fi
  }

  deps=(

    # build tools
    pkg-config
    cmake

    # numerical libraries
    openblas

    # compression
    zlib
    xz
    bzip2

    # networking / xml
    libxml2
    curl
    openssl

    # graphics + fonts
    freetype
    fontconfig
    cairo
    harfbuzz
    fribidi

    freetype
    fontconfig
    cairo
    harfbuzz
    fribidi
    libpng
    libtiff
    jpeg
    webp
    udunits

    # image stack
    libpng
    libtiff
    jpeg
    webp

    # spatial stack
    gdal
    geos
    proj
    udunits

    # misc tools
    git
    pandoc
    quarto
  )

  info "Checking system libraries"

  for dep in "${deps[@]}"; do
    install_if_missing "$dep"
  done
}

# --------------------------
# Ubuntu / Debian
# --------------------------

install_apt() {

  info "Linux detected (apt)"

  act "Updating package index"
  sudo apt update

  act "Installing system libraries"

  sudo apt install -y \
    build-essential \
    gfortran \
    pkg-config \
    cmake \
    libopenblas-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libudunits2-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    git \
    pandoc \
    quarto-cli

  ok "System libraries installed"
}

# --------------------------
# Fedora / RHEL
# --------------------------

install_dnf() {

  info "Linux detected (dnf)"

  act "Installing system libraries"

  sudo dnf install -y \
    gcc \
    gcc-c++ \
    make \
    gcc-gfortran \
    pkgconf-pkg-config \
    cmake \
    openblas-devel \
    zlib-devel \
    udunits2-devel\
    bzip2-devel \
    xz-devel \
    libxml2-devel \
    libcurl-devel \
    openssl-devel \
    freetype-devel \
    fontconfig-devel \
    cairo-devel \
    harfbuzz-devel \
    fribidi-devel \
    libpng-devel \
    libtiff-devel \
    libjpeg-devel \
    libwebp-devel \
    gdal-devel \
    geos-devel \
    proj-devel \
    udunits2-devel \
    git \
    pandoc \
    quarto

  ok "System libraries installed"
}

# --------------------------
# OS switch
# --------------------------

case "$OS" in

  Darwin)
    install_mac
    ;;

  Linux)

    if command -v apt >/dev/null; then
      install_apt

    elif command -v dnf >/dev/null; then
      install_dnf

    else
      fail "Unsupported Linux distribution"
    fi
    ;;

  *)
    fail "Unsupported operating system: $OS"
    ;;

esac

echo
ok "All system dependencies ready"
echo
echo "Next step:"
echo "  renv::restore()"
echo
