"""
overlays.py — load Berlin's greens / water / streets / Bezirke shapefiles once
and provide a helper to overlay them on a Matplotlib axis. Loading is expensive
(streets is ~50 MB and slow), so the orchestrator calls `load_overlays(...)`
once at startup and passes the resulting Overlays bundle into every map plot.
"""
from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import geopandas as gpd

import config


# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------
REPO_ROOT     = config.REPO_ROOT
ARSW_SHP_DIR  = REPO_ROOT / "ARSW2015" / "ARSW2015-toolkit" / "shapefile"
GREEN_SHP     = ARSW_SHP_DIR / "BerlinGreen.shp"
WATER_SHP     = ARSW_SHP_DIR / "BerlinWater.shp"
BEZIRKE_SHP   = config.SHP_BEZIRKE23
STREETS_SHP   = REPO_ROOT / "Data" / "Shapefiles-2022" / "Berlin" \
                  / "TransportNetworkParts2006" / "Streets.shp"


@dataclass
class Overlays:
    """Container for Berlin map overlay layers, all reprojected to a common CRS."""
    crs_epsg: int = 25833
    green:   Optional[gpd.GeoDataFrame] = None
    water:   Optional[gpd.GeoDataFrame] = None
    streets: Optional[gpd.GeoDataFrame] = None
    bezirke: Optional[gpd.GeoDataFrame] = None


def load_overlays(crs_epsg: int = 25833, *,
                  with_streets: bool = True,
                  with_bezirke: bool = True,
                  verbose: bool = True) -> Overlays:
    """Load Berlin context shapefiles, reproject to `crs_epsg`, return Overlays.

    Missing files are tolerated (a warning is printed and the field is left None).

    Parameters
    ----------
    crs_epsg : EPSG code for the working CRS (default 25833 = ETRS89/UTM 33N,
               matches the orchestrator's block GeoDataFrame).
    with_streets : load the streets shapefile (the heavy one).
    with_bezirke : load the 23-Bezirke boundary shapefile.
    """
    ov = Overlays(crs_epsg=crs_epsg)

    def _load(path: Path, label: str) -> Optional[gpd.GeoDataFrame]:
        if not path.exists():
            if verbose:
                print(f"  overlays: {label} not found at {path} (skipping)")
            return None
        try:
            g = gpd.read_file(path).to_crs(epsg=crs_epsg).reset_index(drop=True)
            if verbose:
                print(f"  overlays: {label:<8s}  {len(g):>7d} features")
            return g
        except Exception as e:
            if verbose:
                print(f"  overlays: {label} failed to load: {e!r}")
            return None

    ov.green = _load(GREEN_SHP, "green")
    ov.water = _load(WATER_SHP, "water")
    if with_streets:
        ov.streets = _load(STREETS_SHP, "streets")
    if with_bezirke:
        ov.bezirke = _load(BEZIRKE_SHP, "bezirke")
    return ov


def add_overlays_to_ax(ax, overlays: Overlays, *,
                       alpha_green:   float = 0.35,
                       alpha_water:   float = 0.35,
                       alpha_streets: float = 0.35,
                       green_color:   str = "forestgreen",
                       water_color:   str = "royalblue",
                       streets_color: str = "white",
                       bezirke_color: str = "black",
                       bezirke_linewidth: float = 0.35,
                       zorder_green:   int = 2,
                       zorder_water:   int = 2,
                       zorder_streets: int = 3,
                       zorder_bezirke: int = 4) -> None:
    """Render the overlays on `ax`. Call **after** the choropleth has been drawn.

    Layering (defaults):
      0. (data choropleth: zorder 1 — caller responsibility)
      1. greens & water  (zorder 2, alpha 0.35 — let the choropleth show through)
      2. streets         (zorder 3, alpha 0.35 — very thin white hairlines)
      3. Bezirke borders (zorder 4 — black on top, thin)

    Override alphas downward when the data cmap is in similar hues
    (e.g. set alpha_green=0.15 for an amenity map in YlGnBu).
    """
    if overlays.green is not None and not overlays.green.empty:
        overlays.green.plot(
            ax=ax, color=green_color, alpha=alpha_green,
            linewidth=0.1, edgecolor=green_color, zorder=zorder_green)
    if overlays.water is not None and not overlays.water.empty:
        overlays.water.plot(
            ax=ax, color=water_color, alpha=alpha_water,
            linewidth=0.1, edgecolor=water_color, zorder=zorder_water)
    if overlays.streets is not None and not overlays.streets.empty:
        overlays.streets.plot(
            ax=ax, color=streets_color, alpha=alpha_streets,
            linewidth=0.1, zorder=zorder_streets)
    if overlays.bezirke is not None and not overlays.bezirke.empty:
        overlays.bezirke.boundary.plot(
            ax=ax, color=bezirke_color, linewidth=bezirke_linewidth,
            zorder=zorder_bezirke)
