# ---------- build stage ----------
FROM haskell:9.14.1-bookworm AS build
WORKDIR /app

# Dependencies first, so they are cached when only the source changes.
# (cabal.project.freeze is optional; the * makes the COPY work without it.)
COPY fps-viewer.cabal cabal.project.freeze* ./
RUN cabal update && cabal build --only-dependencies

COPY app ./app
COPY src ./src
RUN cabal build && cp "$(cabal list-bin fps-viewer)" /app/fps-viewer-bin

# ---------- runtime stage ----------
FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends libgmp10 libffi8 zlib1g libnuma1 ca-certificates \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --system --no-create-home app

COPY --from=build /app/fps-viewer-bin /app/fps-viewer
COPY player_data /data
RUN chmod -R a+rX /data

USER app
ENV PORT=8080
EXPOSE 8080
CMD ["/app/fps-viewer", "/data"]
