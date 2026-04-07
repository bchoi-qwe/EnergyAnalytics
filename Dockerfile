FROM rocker/r-ver:4.5.3

ENV DEBIAN_FRONTEND=noninteractive \
    CRAN_REPO=https://cloud.r-project.org \
    PORT=8080 \
    SHINY_HOST=0.0.0.0 \
    # Ensure arrow installs the full C++ library automatically
    LIBARROW_MINIMAL=false 

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        git \
        cmake \
        libcurl4-openssl-dev \
        libssl-dev \
        libx11-dev \
        libuv1-dev \
        zlib1g-dev \
        pandoc \
        libicu-dev \
        # Common dependencies for R packages like bslib/plotly
        libxml2-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff-dev \
        libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages('pak', repos = Sys.getenv('CRAN_REPO'))"

WORKDIR /app

COPY . /app

# Clean up any Mac-compiled binaries before building for Linux
RUN rm -f src/*.so src/*.o src/*.dll

RUN R -q -e "pak::local_install(root = '.', dependencies = TRUE, ask = FALSE)"

# Pre-cache Google Fonts to prevent startup delays
RUN mkdir -p /home/appuser/.cache/R/sass \
    && R -q -e "library(EnergyAnalytics); try(bslib::font_google('IBM Plex Sans'), silent=TRUE); try(bslib::font_google('IBM Plex Mono'), silent=TRUE)" \
    && chown -R appuser:appuser /home/appuser/.cache

RUN useradd --create-home --shell /bin/bash appuser \
    && chown -R appuser:appuser /app

USER appuser

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=5 \
  CMD curl --fail --silent "http://127.0.0.1:${PORT}/" >/dev/null || exit 1

CMD ["Rscript", "scripts/run-app.R"]
