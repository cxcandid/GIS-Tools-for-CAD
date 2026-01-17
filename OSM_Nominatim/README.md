# OSM_NOMINATIM.LSP (command OSM)

This is an AutoLISP routine that uses the [OpenStreetMap Nominatim Search API](https://nominatim.org/release-docs/latest/api/Search/) to search for locations in OpenStreetMap.
It is based on the [Nominator Locator Filter](https://github.com/rduivenvoorde/nominatim_locator_filter) Plugin for QGIS.
It utilizes [DuckDB](https://duckdb.org/) and [DuckDB Spatial](https://duckdb.org/docs/extensions/spatial/overview.html) to send queries to the global Nominatim search service and to convert and transform the resulting geometries.

## Installation of required Tools
To run the command `OSM` we must download [duckdb.exe](https://duckdb.org/docs/installation/?version=stable&environment=cli&platform=win&download_method=direct&architecture=x86_64) and set the global AutoLISP variable `*DUCKDB*` in `osm_nominatim.lsp` to the full path of `duckdb.exe`.
Instead of changing `*DUCKDB*`, we can place `duckdb.exe` in the Windows search path.

DuckDB uses GDAL in the background, which requires the files `header.dxf` and `trailer.dxf` for the conversion process.
For this reason, we need to copy `header.dxf` and `trailer.dxf` (see https://github.com/cxcandid/GIS-Tools-for-CAD/tree/main/OSM_Nominatim/gdal) to a local directory (e.g. `D:\GDAL`) and adjust the global AutoLISP variable `*GDAL_DATA*` accordingly.
```lisp
(setq *GDAL_DATA* “D:\\GDAL”)
```

If we like to use `OSM` in AutoCAD, we need to download [ICONV.EXE](https://en.wikipedia.org/wiki/Iconv) as well.
There is no need for this in BricsCAD/Lite.

## How to use the command "OSM"?
By entering `OSM` in the command line, we can search for worldwide locations (i.e. places, addresses, streets, countries,...) in OpenStreetMap.

`<C>RS (EPSG:9272)/Create <H>atch (No)/Search Nominatim, paste Google Maps URL or enter coordinates (x,y or lat,lon): Vienna`

The command has two options:

|         |         |
| ------- | ------- |
| <**C**>RS | sets the current coordinate reference system (defined as EPSG code) |
| Create <H>atch | enables/disables HATCH creation when Polygons/Multipolygons are returned |

If we enter a search term (i.e. Atzbach), `OSM` sends the query to the global Nominatim search service and shows the results in a dialog window.

![Nominatim Search Results](./images/osm_nominatim_results.jpg)

After we select one or more results in the dialog box and press the OK button, `OSM` loads the geometries into Model Space and zooms to the extents of the loaded entities.
The resulting geometries are automatically transformed to the selected target CRS.

![Nominatim Search Geometry](./images/osm_nominatim_geom.jpg)

If the result consists of points, OSM inserts the `osm_point` block for each point instead of using `POINT` entities.

> [!TIP]
> To do a local search (so only within current windows extent), we need to put & in front of the search term (i.e. "&Gmund"). We can even add a country code to the search term to ONLY search that country (i.e. "Gmund at")

If we enter x,y coordinates, OSM inserts the `osm_point` block in the given location and zooms to this point.
`OSM` expects the coordinates to be in the selected CRS. However, if the coordinates are in the WGS84 range, `OSM` expects a Lat,Lon input (i.e. 48.2084786,16.3731661).
We can also enter Google Maps URLs as search text (i.e. `https://www.google.com/maps/@48.2084786,16.3731661,20z?entry=ttu&g_ep=EgoyMDI2MDExMy4wIKXMDSoASAFQAw%3D%3D`).

## LWPOLYLINE/POLYLINE vs HATCH
DuckDB/GDAL converts Polygon and MultiPolygon features to DXF HATCH entities by default. To write these features as LWPOLYLINE/POLYLINE entities instead, we must set a Windows system variable `DXF_WRITE_HATCH=FALSE`.
This is the default option in `OSM`. If we want HATCH entities, we must press `H` to toggle the HATCH creation.

## Credit

Credit goes to *Alan J. Thomson* (I used his List Select Dialog code), to *Richard Duivenvoorde* (for his idea with the Nominatim Locator Filter for QGIS), to the makers of `DuckDB` and to *Evan Rouault* (master of `GDAL`).