;;---------------------------------------------------------------;;
;;  OSM_NOMINATIM.LSP                                            ;;
;;  Command: OSM                                                 ;;
;;                                                               ;;
;;  This is an implementation of an OpenStreetMap Nominatim      ;;
;;  Search function for BricsCAD (Lite) and AutoCAD.             ;;
;;  The idea is based on the "QGIS Nominatim Locator Filter"     ;;
;;  (https://github.com/rduivenvoorde/nominatim_locator_filter), ;;
;;  originally developed by Richard Duivenvoorde.                ;;
;;                                                               ;;
;;  The AutoLISP routine calls DuckDB CLI (duckdb.exe) to        ;;
;;  fetch the query results via Nominatim API                    ;;
;;  (see https://duckdb.org/docs/installation).                  ;;
;;  The resulting geometries are automatically transformed       ;;
;;  to the selected target CRS (defined as EPSG Code) and        ;;
;;  loaded into the Layer "OSM".                                 ;;
;;                                                               ;;
;;  DuckDB uses GDAL in the background, which requires the files ;;
;;  header.dxf and trailer.dxf for the conversion process.       ;;
;;  For this reason, we need to copy the header.dxf and          ;;
;;  trailer.dxf files                                            ;;
;;  (see https://github.com/cxcandid/GIS-Tools-for-CAD/tree/main/;;
;;  OSM_Nominatim/gdal) to a local directory (e.g., D:\GDAL) and ;;
;;  adjust the global AutoLISP variable *GDAL_DATA* accordingly  ;;
;;  (setq *GDAL_DATA* “D:\\GDAL”).                               ;;
;;                                                               ;;
;;  To do a local search (so only within current windows         ;;
;;  extent), we need to put & in front of the search term        ;;
;;  (i.e. "&Gmund").                                             ;;
;;  We can even add a country code to the search term to ONLY    ;;
;;  search that country (i.e. "Gmund at").                       ;;
;;                                                               ;;
;;  Google Maps URLs (@lat,lon) are supported.                   ;;
;;  Direct coordinate input (x,y or lat,lon) also works.         ;;
;;  Coordinates are directly placed on the map.                  ;;
;;                                                               ;;
;;  If DOSLIB (© Robert McNeel & Associates) is installed, it is ;;
;;  used for displaying the multi-selection-list.                ;;
;;  (https://wiki.mcneel.com/doslib/home)                        ;;
;;                                                               ;;
;;  If AutoCAD is used, we need to download Gnu-C ICONV.EXE      ;;
;;  to convert between UTF-8 and Windows-1252 encodings.         ;;
;;  At the time of writing OSM_NOMINATIM.LSP, no method for      ;;
;;  encoding conversion was implemented in DuckDB.               ;;
;;---------------------------------------------------------------;;
;;  Author: Christoph Candido, Copyright © 2024-2026             ;;
;;  (https://github.com/cxcandid)                                ;;
;;---------------------------------------------------------------;;
;;  Version 1.2 - 01-17-2026: added Google Maps URL support      ;;
;;                            and local CRS coordinate support   ;;
;;  Version 1.1 - 10-05-2024: added option to toggle hatch       ;;
;;     creation; added batch file creation for setting env vars. ;;
;;  Version 1.0 - 09-18-2024: Initial implementation             ;;
;;---------------------------------------------------------------;;

;; Set full filename for external tools (i.e. "c:\\duckdb\\duckdb.exe") 
;; or place the tools in your Windows system path.
(setq *DUCKDB* "duckdb.exe"
      *ICONV* "iconv.exe"   ; only needed for AutoCAD
      *GDAL_DATA* "D:\\GDAL" ; directory with header.dxf and trailer.dxf
      *DXF_WRITE_HATCH* "FALSE"
)

(defun C:OSM ( / *error* createOsmPoint getVPExtents extractCoords transformAndPlacePoint placeLocalPoint
     crs varlist oldvars cont search epsg tempfile tempfile2 wso vp minx miny maxx maxy macro cmd 
     lst res idlist osmids en ss ss_new i pt f batfile x lat lon coords isWGS84)

  (defun *error* (msg)
    (mapcar '(lambda (x) (setvar (car x) (cdr x))) oldvars)
    (princ)
  )

  (defun createOsmPoint()
    (if (not (tblsearch "BLOCK" "osm_point"))
      (progn
        (entmake (list '(0 . "BLOCK")
                       '(2 . "osm_point")
                       '(10 0 0 0)
                       '(70 . 0)
                 )
        )
        (entmake (list '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                       '(90 . 2)
                       '(70 . 0)
                       '(10 -10 0 0)
                       '(10 10 0 0)
                 )
        )
        (entmake (list '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                       '(90 . 2)
                       '(70 . 0)
                       '(10 0 -10 0)
                       '(10 0 10 0)
                 )
        )
        (entmake (list '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                       '(90 . 4)
                       '(70 . 1)
                       '(10 -5 -5 0)
                       '(10 5 -5 0)
                       '(10 5 5 0)
                       '(10 -5 5 0)
                 )
        )
        (entmake (list '(0 . "ENDBLK")
                       '(8 . "0")
                 )
        )
      )
    )
    (princ)
  )

  (defun getVPExtents ( )
      ( (lambda (offset)
          ( (lambda (viewctr)
              (list
                (mapcar '- viewctr offset)
                (mapcar '+ viewctr offset)
              )
            )
            (getvar "viewctr")
          )
        )
        ( (lambda (halfHeight aspectRatio)
            (list
              (* halfHeight aspectRatio)
              halfHeight
            )
          )
          (* 0.5 (getvar "viewsize"))
          (apply '/ (getvar "screensize"))
        )
      )
  )

  ;; Extract coordinates from input (Google @ format or simple x,y)
  ;; Returns: (coord1 coord2 isWGS84flag) or nil
  ;;   isWGS84flag = T if coordinates are in WGS84 range (GPS), nil if local CRS
  (defun extractCoords (input / rxo match coord1 coord2 coord1str coord2str isGoogle)
    (setq rxo (vlax-create-object "VBScript.RegExp")
          isGoogle nil)
    (vlax-put-property rxo 'IgnoreCase :vlax-true)
    (vlax-put-property rxo 'Global :vlax-false)
    
    ;; Try Google Maps format first: @lat,lon
    (vlax-put-property rxo 'Pattern "@(-?[0-9]+\\.?[0-9]*)\\s*,\\s*(-?[0-9]+\\.?[0-9]*)")
    (if (setq match (vlax-invoke-method rxo 'Execute input))
      (if (> (vlax-get-property match 'Count) 0)
        (progn
          (setq match (vlax-get-property match 'Item 0)
                isGoogle T)
          (if (> (vlax-get-property (vlax-get-property match 'SubMatches) 'Count) 1)
            (progn
              (setq coord1str (vlax-variant-value (vlax-get-property (vlax-get-property match 'SubMatches) 'Item 0))
                    coord2str (vlax-variant-value (vlax-get-property (vlax-get-property match 'SubMatches) 'Item 1)))
            )
          )
        )
      )
    )
    
    ;; If not Google format, try simple x,y or lat,lon
    (if (not isGoogle)
      (progn
        (vlax-put-property rxo 'Pattern "^\\s*(-?[0-9]+\\.?[0-9]*)\\s*,\\s*(-?[0-9]+\\.?[0-9]*)\\s*$")
        (if (setq match (vlax-invoke-method rxo 'Execute input))
          (if (> (vlax-get-property match 'Count) 0)
            (progn
              (setq match (vlax-get-property match 'Item 0))
              (if (> (vlax-get-property (vlax-get-property match 'SubMatches) 'Count) 1)
                (progn
                  (setq coord1str (vlax-variant-value (vlax-get-property (vlax-get-property match 'SubMatches) 'Item 0))
                        coord2str (vlax-variant-value (vlax-get-property (vlax-get-property match 'SubMatches) 'Item 1)))
                )
              )
            )
          )
        )
      )
    )
    
    (if rxo (vlax-release-object rxo))
    
    ;; Parse coordinates
    (if (and coord1str coord2str)
      (progn
        (setq coord1 (atof coord1str)
              coord2 (atof coord2str))
        
        ;; Check if coordinates are in WGS84 (GPS) range
        (if (and (>= coord1 -90) (<= coord1 90)
                 (>= coord2 -180) (<= coord2 180)
                 (or (/= coord1 0.0) (/= coord2 0.0)))
          ;; Valid WGS84 coordinates
          (progn
            (princ (strcat "\nInterpreted as WGS84 (GPS) - Lat: " (rtos coord1 2 8) ", Lon: " (rtos coord2 2 8)))
            (list coord1 coord2 T)
          )
          ;; Outside WGS84 range - interpret as local CRS coordinates
          (progn
            (princ (strcat "\nInterpreted as local CRS coordinates - X: " (rtos coord1 2 8) ", Y: " (rtos coord2 2 8)))
            (list coord1 coord2 nil)
          )
        )
      )
      nil
    )
  )

  ;; Transform WGS84 coordinates to target CRS and place point marker
  (defun transformAndPlacePoint (lat lon targetCrs / wso tempfile batfile f pt ss en)
    (setq tempfile (vl-filename-mktemp "nom.dxf")
          batfile (vl-filename-mktemp "nom.bat")
          wso (vlax-create-object "WScript.Shell")
          f (open batfile "w")
    )
    (foreach x (list (strcat "set GDAL_DATA=" *GDAL_DATA*)
                     (strcat "\"" *DUCKDB* "\" -c \"install spatial;load spatial; COPY (SELECT ST_Transform(ST_Point(" (rtos lon 2 8) "," (rtos lat 2 8) "),'EPSG:4326','EPSG:" targetCrs "',True) as geom,'OSM' as layer) TO '" tempfile "' WITH (FORMAT GDAL, DRIVER 'DXF');\"")
               )
      (write-line x f)
    )
    (close f)
    (vlax-invoke-method wso "Run" batfile 0 'T)
    
    (if (findfile tempfile)
      (progn
        (setq en (entlast))
        (command "_.INSERT" (strcat "*" tempfile) "0,0" "" "")
        (if (not en)
          (setq ss (ssget "x"))
          (progn
            (setq ss (ssadd))
            (while (not (equal (setq en (entnext en)) (entlast)))
              (ssadd en ss)
            )
            (ssadd (entlast) ss)
          )
        )
        ;; Replace point with osm_point block
        (if (setq ss_new (ssget "x" '((0 . "POINT")(8 . "OSM"))))
          (progn
            (createOsmPoint)
            (setq i 0)
            (while (setq en (ssname ss_new i))
              (setq pt (cdr (assoc 10 (entget en))))
              (setvar "CLAYER" "OSM")
              (command "_.INSERT" "osm_point" pt "" "" "")
              (entdel en)
              (setq i (1+ i))
            )
            (setq ss (ssadd))
            (ssadd (entlast) ss)
          )
        )
        (command "_.ZOOM" "_Object" ss "")
        (vl-file-delete tempfile)
        (vl-file-delete batfile)
        (princ (strcat "\nPoint placed at Lat: " (rtos lat 2 8) ", Lon: " (rtos lon 2 8)))
      )
      (princ "\n* Error creating point *")
    )
    (princ)
  )

  ;; Place point in local CRS (no transformation needed)
  (defun placeLocalPoint (x y / pt)
    (createOsmPoint)
    (if (not (tblsearch "LAYER" "OSM"))
      (command "_.LAYER" "_M" "OSM" "")
    )
    (setvar "CLAYER" "OSM")
    (command "_.INSERT" "osm_point" (list x y 0) "" "" "")
    (setq ss (entlast))
    (command "_.ZOOM" "_Object" ss "")
    (princ (strcat "\nPoint placed at X: " (rtos x 2 8) ", Y: " (rtos y 2 8)))
    (princ)
  )

  (setq crs (vl-registry-read "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "CRS"))
  (if (not crs) (setq crs "3857"))
  (setq varlist '(("CMDECHO" . 0)("CLAYER" . "0")("OSMODE" . 0)("INSUNITS" . 0)("INSUNITSDEFSOURCE" . 0)("INSUNITSDEFTARGET" . 0))
        oldvars (mapcar '(lambda (x) (cons (car x) (getvar (car x)))) varlist)
  )
  (mapcar '(lambda (x) (setvar (car x) (cdr x))) varlist)
  
  (setq cont 'T)
  (while (and cont (/= "" (setq search (getstring T (strcat "\n<C>RS (EPSG:" crs ")/Create <H>atch (" (if (= *DXF_WRITE_HATCH* "FALSE") "No" "Yes") ")/Search Nominatim, paste Google Maps URL or enter coordinates (x,y or lat,lon): ")))))
    (cond 
      ((member (strcase search) '("C" "CR" "CRS"))
        (setq epsg (getint (strcat "Coordinate Reference System - EPSG:<" crs ">: ")))
        (if epsg
          (progn
            (setq crs (itoa epsg))
            (vl-registry-write "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "CRS" crs)
          )
        )
      )
      ((member (strcase search) '("H" "HATCH"))
        (if (= *DXF_WRITE_HATCH* "FALSE") 
          (setq *DXF_WRITE_HATCH* "TRUE")
          (setq *DXF_WRITE_HATCH* "FALSE")
        )
      )
      ;; Check if input contains coordinates
      ((setq coords (extractCoords search))
       (setq cont 'nil
             coord1 (car coords)
             coord2 (cadr coords)
             isWGS84 (caddr coords))
       (if isWGS84
         ;; WGS84 coordinates - transform to target CRS
         (transformAndPlacePoint coord1 coord2 crs)
         ;; Local CRS coordinates - place directly
         (placeLocalPoint coord1 coord2)
       )
      )
      ('T
        (setq cont 'nil
              tempfile (vl-filename-mktemp "tmp.txt")
              tempfile2 (vl-filename-mktemp "tmp.txt")
              wso (vlax-create-object "WScript.Shell")
        )
        (if (= (substr search 1 1) "&")
          (progn
            (setq search (substr search 2)
                  vp (getVPExtents)
                  minx (caar vp)
                  miny (cadar vp)
                  maxx (caadr vp)
                  maxy (cadadr vp)
                  macro "create temp macro to_wgs84(x,y,epsg) as (replace(rtrim(substr(st_astext(st_transform(st_point(x,y),'EPSG:'||epsg,'EPSG:4326',true)),8),')'),' ',','));"
                  cmd (strcat "cmd /c chcp 65001 & " *DUCKDB* " -list -noheader -c \"install spatial;load spatial; " macro "select '''('||group_concat('(\"\"'||category ||': '||display_name||'\"\" . \"\"' || substr(osm_type,1,1) || osm_id ||'\"\")',' ')||')' as result from st_read('/vsicurl/https://nominatim.openstreetmap.org/search?limit=100&q=' ||regexp_replace(hex('" search "'),'(.{2})','%\\1','g') || '&format=geojson&polygon_geojson=0&bounded=1&viewbox='||to_wgs84(" (rtos minx 2) "," (rtos miny 2) "," crs ")||','||to_wgs84(" (rtos maxx 2) "," (rtos maxy 2) "," crs "));\" >\"" tempfile "\"")
            )
          )
          (setq cmd (strcat "cmd /c chcp 65001 & " *DUCKDB* " -list -noheader -c \"install spatial;load spatial; select '''('||group_concat('(\"\"'||category ||': '||display_name||'\"\" . \"\"' || substr(osm_type,1,1) || osm_id ||'\"\")',' ')||')' as result from st_read('/vsicurl/https://nominatim.openstreetmap.org/search?limit=100&q=' ||regexp_replace(hex('" search "'),'(.{2})','%\\1','g') || '&format=geojson&polygon_geojson=0');\" >\"" tempfile "\""))
        )
        
        (vlax-invoke-method wso "Run" cmd 0 'T)
        (if (= "AUTOCAD" (strcase (getvar 'product)))
          (progn
            (setq cmd (strcat "cmd /c " *ICONV* " -f \"UTF-8\" -t \"windows-1252\" \"" tempfile "\" >\"" tempfile2 "\""))
            (vlax-invoke-method wso "Run" cmd 0 'T)
            (vl-file-delete tempfile)
            (setq tempfile tempfile2)
          )
        )
        
        (setq lst (load tempfile))
        (vl-file-delete tempfile)
        (if (not lst)
          (princ "\n* nothing found *")
          (progn
            (setq res 
              (if dos_multilist
                (dos_multilist "Nominatim Search" "Select Nominatim Result" (mapcar 'car lst) '(0))
                (listselect "Nominatim Search" "Select Nominatim Result" 20 100 "false" (mapcar 'car lst) 'nil)
              )
            )
            (if res
              (progn
                (setq idlist (mapcar '(lambda (x) (cdr (assoc x lst))) res)) 
                (setq osmids (car idlist))
                (foreach x (cdr idlist) (setq osmids (strcat osmids "," x)))
                (setq en (entlast)
                      f (open (setq batfile (vl-filename-mktemp "nom.bat")) "w")
                      tempfile (vl-filename-mktemp "nom.dxf")
                )
                (foreach x (list (strcat "set DXF_WRITE_HATCH=" *DXF_WRITE_HATCH*)
                                 (strcat "set GDAL_DATA=" *GDAL_DATA* "")
                                 (strcat "\"" *DUCKDB* "\" -c \"install spatial;load spatial; COPY (SELECT ST_Transform(geom,'EPSG:4326','EPSG:" crs "',True) as geom,'OSM' as layer FROM st_read('/vsicurl_streaming/https://nominatim.openstreetmap.org/lookup?osm_ids=" osmids "&format=geojson&polygon_geojson=1')) TO '" tempfile "' WITH (FORMAT GDAL, DRIVER 'DXF');\"")
                           )
                  (write-line x f)
                )
                (close f)
                (vlax-invoke-method wso "Run" batfile 0 'T)
                (command "_.INSERT" (strcat "*" tempfile) "0,0" "" "")
                (if (not en)
                  (setq ss (ssget "x"))
                  (progn
                    (setq ss (ssadd))
                    (while (not (equal (setq en (entnext en)) (entlast)))
                      (ssadd en ss)
                    )
                    (ssadd (entlast) ss)
                  )
                )
                (command "_.ZOOM" "_Object" ss "")
                (vl-file-delete tempfile)
                (vl-file-delete batfile)
                ;; replace all points with inserts of block "osm_point"
                (if (setq ss (ssget "x" '((0 . "POINT")(8 . "OSM"))))
                  (progn
                    (createOsmPoint) ; create block "osm_point" if it is not available
                    (setq i 0)
                    (while (setq en (ssname ss_new i))
                      (setq pt (cdr (assoc 10 (entget en))))
                      (setvar "CLAYER" "OSM")
                      (command "_.INSERT" "osm_point" pt "" "" "")
                      (entdel en)
                      (setq i (1+ i))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (mapcar '(lambda (x) (setvar (car x) (cdr x))) oldvars)
  (princ)
)

;; List Select Dialog (Temp DCL list box selection, based on provided list)
 ;; title - list box title
 ;; label - label for list box
 ;; height - height of box
 ;; width - width of box
 ;; multi - selection method ["true": multiple, "false": single]
 ;; lst - list of strings to place in list box
 ;; Alan J. Thompson, 09.23.08 / 05.17.10 (rewrite)
(defun listselect (title label height width multi lst default / fn fo d item f pos)
 (setq fo (open (setq fn (vl-filename-mktemp "" "" ".dcl")) "w"))
 (foreach x (list (strcat "list_select : dialog { label = \"" title "\"; initial_focus = \"lst\"; spacer;")
                  (strcat ": list_box { label = \"" label "\";" "key = \"lst\";")
                  (strcat "allow_accept = true; height = " (vl-princ-to-string height) ";")
                  (strcat "width = " (vl-princ-to-string width) ";")
                  (strcat "multiple_select = " multi "; } spacer; ok_cancel; }")
            )
   (write-line x fo)
 )
 (close fo)
 (new_dialog "list_select" (setq d (load_dialog fn)))
 (start_list "lst")
 (mapcar (function add_list) lst)
 (end_list)
 (if default
    (progn
      (setq pos (vl-position default lst))
      (if (not pos) (setq pos "0") (setq pos (itoa pos)))
    )
    (setq pos "0")
 )
 (setq item (set_tile "lst" pos))
 (action_tile "lst" "(setq item $value)")
 (setq f (start_dialog))
 (unload_dialog d)
 (vl-file-delete fn)
 (if (= f 1)
   ((lambda (s / i s l)
      (while (setq i (vl-string-search " " s))
        (setq l (cons (nth (atoi (substr s 1 i)) lst) l))
        (setq s (substr s (+ 2 i)))
      )
      (reverse (cons (nth (atoi s) lst) l))
    )
     item
   )
 )
)

(princ
  (strcat
    "\n:: OSM_Nominatim.lsp | © 2024-2026 Christoph Candido | https://github.com/cxcandid ::"
    "\n:: OSM Nominatim Search & Coordinate Import (WGS84 or local CRS) - Type \"OSM\" to Invoke ::"
  )
)
(princ)
