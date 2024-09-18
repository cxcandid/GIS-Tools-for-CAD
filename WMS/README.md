# WMS.LSP

This is an AutoLISP routine that imports raster image tiles into AutoCAD or BricsCAD (Lite).
It utilizes `GDAL` ([gdalwarp.exe](https://gdal.org/en/latest/programs/gdalwarp.html)) to generate raster image tiles from external map services or from every raster format GDAL can read ([GDAL Raster drivers](https://gdal.org/en/latest/drivers/raster/index.html)).
Using [GDAL Virtual File Systems](https://gdal.org/en/latest/user/virtual_file_systems.html) we can even import images from AWS S3, Microsoft Azure, Google Cloud Storage or others.
During development, I focused on the open configurability of map services, tile sizes and tile names as well as the ability to exchange drawings with external partners without having to send them the loaded raster tiles.

## Installation of required GDAL Tools
To run the command `WMS`, we first need to install GDAL on your system ([GDAL Windows](https://gdal.org/en/latest/download.html#windows)).
I use [GISInternals](https://www.gisinternals.com) to install GDAL on my Windows PC, or sometimes call GDAL tools from my [QGIS](https://www.qgis.org) installation. 
If you open the GISInternals link, go to `Stable Releases` and select a `MSVCxxxx/x64` download page
(i.e. [release-1930-x64-gdal-3-9-1-mapserver-8-2-0](https://www.gisinternals.com/query.html?content=filelist&file=release-1930-x64-gdal-3-9-1-mapserver-8-2-0.zip).
Then download the generic installer for the GDAL core components (i.e. [gdal-3.9.1-1930-x64-core.msi](https://download.gisinternals.com/sdk/downloads/release-1930-x64-gdal-3-9-1-mapserver-8-2-0/gdal-3.9.1-1930-x64-core.msi)).
Install GDAL and add the location of the core components to the Windows PATH environment variable, or add the full GDAL path to the line `(setq *GDALWARP* "gdalwarp.exe")` in `wms.lsp`.

We must also set additional Windows environment variables:

|         |         |
| ------- | ------- |
| GDAL_DATA | path to `gdal-data` directory |
| PROJ_LIB | path to the file `proj.db`, which contains the definitions of all spatial reference systems |
| GDAL_HTTP_UNSAFESSL | YES ... only set this if we have problems with HTTPS/SSL connections |

## Define default values
If we want to roll out `WMS.LSP` company-wide, we can define some default values at the beginning of the code.

```
;; *** Define Default Values ***
(setq *WMS* "\\\\MyFileServer\\WMS") ; default Directory to store WMS images
(setq *GDALWARP* "C:\\OSGeo4W\\bin\\gdalwarp.exe")
(setq *WMS_CFG_FILE* "\\\\MyFileServer\\WMS\\Config\\austria.cfg")
(setq *WMS_TRANSPARENCY* -1)
(setq *WMS_BACK* 'T)
;; *** End of Default Value Definition ***
```

## How to use the command "WMS"?
By entering `WMS` in the command line, we can import raster image tiles from external map services into our Model Space.
All raster image tiles are saved in the specified image directory for further use.

`<D>irectory (M:\WMS)/config<F>ile/<C>RS (EPSG:31287)/<S>ervice/<W>indow/<R>estore/<T>ransparency (-1...BYLAYER)/send to <B>ack (YES)/<Z>oom to Grid Extents/Pick point <OpenStreetMap>:`

The command has the following options:

|         |         |
| ------- | ------- |
| <**D**>irectory | opens a dialog to select a folder for saving the images. The folder location will be stored in Windows registry. |
| config<**F**>ile | opens a dialog to select a WMS configuration. The selected file location will be stored in Windows registry. |
| <**C**>RS | opens a dialog for selecting a coordinate reference system. |
| <**S**>ervice | opens a dialog for selecting a configured Map Service. |
| <**W**>indow | asks for two corner points to define a rectangular area into which the images are loaded. |
| <**R**>estore | restores image tiles created with WMS and not found in current drawing (esp. for the exchange of drawings with external partners). |
| <**T**>ransparency | sets image transparency. Integer values between -1 and 90 are allowed (-1 = BYLAYER). |
| send to <**B**>ack | sents image to the background. |
| <**Z**>oom to Grid Extents | zooms to the extents of the current map service grid. The grid is only displayed if it is not too tight. |

> [!TIP]
> We can enter the letter between the angle brackets to call an option.

## How to configure new Map Services?
For reasons of simplicity, I have chosen AutoLISP as the configuration file format.
This means that each configuration entry corresponds to a list.
Unfortunately, this procedure is not optimal, as no validation takes place when the configuration file is read. Crashes occur if the configuration is incorrect.
Please test a new configuration thoroughly before passing it on.

The structure of the list follows this pattern:
```
(
  ( ( ("name" . "<Name of the Service>")
      ("datasource" . "<GDAL datasource description>")
      ("datasource_2" . "<GDAL datasource description>")
      ...
      ("datasource_n" . "<GDAL datasource description>")
      ("options" . "<GDALWARP options>")
      ("srs" . "<Spatial Reference System for the Image Tiles>")
      ("dwglayer" . "<Layer Name>")
      ("format" . "<Image Format>")
      ("extent" . "<Extent of the Image Tile Grid>")
      ("tileprefix" . "<Image Tile Prefix>")
      ("tilesize" . "<Tile Size>")
      ("imagesize" . "<Image Size>")
    )
    (<Lambda Function to define image tile names>)
  )
  ...
  ( (<another Map Service configuration>) )
  ...
)
```
Every map service configuration belongs to a certain spatial reference system (SRS).
We can use service names more than once if they are assigned to different SRSs (see example below).

The following table explains all options in detail:

|         |         |
| ------- | ------- |
| name | any name for the map service |
| datasource | GDAL service description that can be used in GDALWARP. Fortunately, GDAL also allows us to use inline XML descriptions instead of XML files. This enables us to add every kind of [WMS](https://gdal.org/en/latest/drivers/raster/wms.html), [WMTS](https://gdal.org/en/latest/drivers/raster/wmts.html) and [TMS](https://gdal.org/en/latest/drivers/raster/wms.html#tms) services. In total, we can use more than 150 [raster formats](https://gdal.org/en/latest/drivers/raster/index.html) as datasources and even retrieve images from databases like PostGIS or SQLITE/GeoPackage. |
| datasource_n | optional datasources, starting with "datasource_2", that are used if the previous data sources fail. Unfortunately, this is not intended for parallel processing, because there is no parallel processing yet.  |
| options | optional [GDALWARP](https://gdal.org/en/latest/programs/gdalwarp.html) options and File Format Creation options (i.e. [GeoTIFF](https://gdal.org/en/latest/drivers/raster/gtiff.html#creation-options)). |
| srs | definition of a Spatial Reference System for the resulting image tiles. |
| dwglayer | name of the layer on which the image tiles are to be placed. |
| format | image tile [MIME-Type](https://wiki.selfhtml.org/wiki/MIME-Type/%C3%9Cbersicht#I) (i.e. image/tiff, image/jpeg, image/png). |
| extent | spatial dimension of the image tile grid, defined as list of the grid origin point and the opposite corner. |
| tileprefix | prefix for the image tile name. |
| tilesize | size of the image tile in drawing units. |
| imagesize | size of the image tile in pixels. |
| lambda function | AutoLISP lambda function to calculate the image tile name. Parameters are: `x` pick point x, `y` pick point y, `x0` tile grid origin x, `y0` tile grid origin y, `prefix` tile prefix |

> [!TIP]
> We can add inline comments using the syntax `;| comment |;`.
         
Below is an example configuration of 3 map services for Austria:

1. OpenStreetMap (EPSG:31287): simple tile naming scheme (i.e. `osm3846_EPSG31287.tif`)
2. OpenStreetMap (EPSG:9272): simple tile naming scheme (i.e. `osm-18000_5324000_EPSG9272.tif`)
3. Basemap.at Ortho (EPSG:9272): complex tile naming scheme according to the former sheet section of the Austrian digital cadastral map (i.e. `dop4433-34_2_EPSG9272.tif`)

`austria.cfg`:
```
(
  ( ( ;| OpenStreetMap Tile Map Service - Austria Lambert (EPSG:31287) |;
      ;| ------------------------------------------------------------- |;
      ("name" . "OpenStreetMap")
      ("datasource" . "<GDAL_WMS><Service name='TMS'><ServerUrl>https://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>14</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><Cache/></GDAL_WMS>")
      ("options" . "-co COMPRESS=JPEG -co PHOTOMETRIC=YCBCR -co TILED=YES -co JPEG_QUALITY=95 -r cubic")
      ("srs" . "EPSG:31287")
      ("dwglayer" . "OSM")
      ("format" . "image/tiff")
      ("extent" . "((107720 284970)(694940 575950))")
      ("tileprefix" . "osm")
      ("tilesize" . "(10000 10000)")
      ("imagesize" . "(3000 3000)")
    )
    (lambda ( x y x0 y0 prefix / dx dy nr nrsub nrtxt)
          (setq t1 (fix (/ x 10000))
                t2 (fix (/ y 10000))
          )
          (strcat prefix (itoa t1) (itoa t2))
    )
  )
  ( ( ("name" . "OpenStreetMap")
      ("datasource" . "<GDAL_WMS><Service name='TMS'><ServerUrl>https://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>17</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><Cache/></GDAL_WMS>")
      ("options" . "-co COMPRESS=JPEG -co PHOTOMETRIC=YCBCR -co TILED=YES -co JPEG_QUALITY=95 -r cubic")
      ("srs" . "EPSG:9272")
      ("dwglayer" . "OSM")
      ("format" . "image/tiff")
      ("extent" . "((-116000 5130000)(116000 5410000))")
      ("tileprefix" . "osm")
      ("tilesize" . "(2000 2000)")
      ("imagesize" . "(4000 4000)")
    )
    (lambda ( x y x0 y0 prefix / dx dy nr nrsub nrtxt)
          (setq t1 (+ x0 (* (fix (/ (- x x0) 2000)) 2000))
                t2 (+ y0 (* (fix (/ (- y y0) 2000)) 2000))
          )
          (strcat prefix (itoa t1) "_" (itoa t2))
    )
  )
  ( ( ("name" . "Basemap.at Ortho")
      ("datasource" . "<GDAL_WMS><Service name='TMS'><ServerUrl>https://mapsneu.wien.gv.at/basemap/bmaporthofoto30cm/normal/google3857/${z}/${y}/${x}.jpeg</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>20</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><UnsafeSSL>true</UnsafeSSL><Cache/></GDAL_WMS>")
      ("options" . "-co COMPRESS=JPEG -co PHOTOMETRIC=YCBCR -co TILED=YES -co JPEG_QUALITY=95")
      ("srs" . "EPSG:9272")
      ("dwglayer" . "X_DOP")
      ("format" . "image/tiff")
      ("extent" . "((-116250 5130000)(116000 5409000))")
      ("tileprefix" . "dop")
      ("tilesize" . "(625 500)")
      ("imagesize" . "(2000 1600)")
    )
    (lambda ( x y x0 y0 prefix / dx dy nr nrsub nrtxt)
          (setq t1 (fix (/ x 10000))
                t2 (- (fix (/ y 10000)) 499)
          )
          (if (minusp x)
            (setq x  (+ x 10000 (abs (* t1 10000)))
                  t1 (+ t1 45)
            )
            (setq x  (- x (* t1 10000))
                  t1 (+ t1 46)
            )
          )
          (setq y   (- y (* 10000 (fix (/ y 10000))))
                dy    (- 9 (fix (/ y 1000)))
                dx    (1+ (fix (/ x 1250)))
                nr    (+ dx (* dy 8))
                dx    (1+ (fix (/ (- x (* 1250 (fix (/ x 1250)))) 625)))
                dy    (- 1 (fix (/ (- y (* 1000 (fix (/ y 1000)))) 500)))
                nrsub (itoa (+ dx (* 2 dy)))
                nrtxt (if (< nr 10)
                        (strcat "0" (itoa nr))
                        (itoa nr)
                      )
          )
          (strcat prefix (itoa t1) (itoa t2) "-" nrtxt "_" nrsub)
    )
  )
)
```
If we choose `image/tiff` as raster image tile format, GDAL creates `GeoTIFF` files, that can be greatly reduced in size ([GeoTiff Compression for Dummies](https://blog.cleverelephant.ca/2015/02/geotiff-compression-for-dummies.html)).

## Restoring "WMS" raster image tiles
To restore raster image tiles because they are missing, or to update them, we need to call the `WMS` option `<R>estore`.
Each imported raster image tile has additional XEDs (Extended Entity Data) attached, which are required for this image restoration (i.e. `(-3 ("GIS-Tools-for-CAD" (1000 . "OpenStreetMap") (1000 . "EPSG:9272"))`).
If we need to send drawings with attached `WMS` raster image tiles to an external partner, we must also provide them with the `WMS` configuration file so that they can restore the images.

## What to do if a map service fails?
`WMS` creates a small batch script (`run.bat`) in the AutoCAD/BicsCAD Temp-directory (call the command `TEMPPREFIX` to find the directory).
We can call the `run.bat` file via the Windows Command line to check the error messages.
If we import many image tiles at once using the `<W>indow` option and something goes wrong, we can stop the import by pressing `Ctrl+C` multiple times.

## Credit

Credit goes to *Alan J. Thomson* (I used his List Select Dialog code) and, as always when it comes to GDAL, to *Even Rouault* (for his tireless willingness to help with GDAL: https://github.com/OSGeo/gdal/issues).

Thanks a lot, guys!
