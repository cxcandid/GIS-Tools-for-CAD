;;---------------------------------------------------------------;;
;;  WMS.LSP                                                      ;;
;;  Command: WMS                                                 ;;
;;                                                               ;;
;;  This is an AutoLISP routine for BricsCAD (Lite) and AutoCAD  ;;
;;  to import Web Map Service (WMS) images.                      ;;
;;                                                               ;;
;;  GDALWARP is used to reproject the images to the given CRS.   ;;
;;  (https://gdal.org/en/latest/programs/gdalwarp.html)          ;;
;;  If DOSLIB (McNeel) is installed, it is used for folder       ;;
;;  dialogs and for calling the executable "gdalwarp.exe".       ;;
;;                                                               ;;
;;  To run WMS, you must first install GDAL                      ;;
;;  (https://gdal.org/en/latest/download.html#windows)           ;;
;;  and add GDALWARP.EXE directory to the Windows System Path or ;;
;;  set the global AutoLISP variable *GDALWARP* to the full      ;;
;;  executable name.                                             ;;
;;                                                               ;;
;;  GDALWARP.EXE also requires 2 Windows system variables:       ;;
;;  GDAL_DATA ... path to "gdal-data" directory                  ;;
;;  PROJ_LIB ... path to the file "proj.db"                      ;;
;;---------------------------------------------------------------;;
;;  Author: Christoph Candido, Copyright © 2024                  ;;
;;  (https://github.com/cxcandid)                                ;;
;;---------------------------------------------------------------;;
;;  Versions:                                                    ;;
;;  1.0 (09-13-2024): initial implementation                     ;;
;;  1.1 (12-12-2025): added OSMODE=0 to prevent shifting         ;;
;;---------------------------------------------------------------;;
(vl-load-com)
(regapp "GIS-Tools-for-CAD")

;; *** Define Default Values ***
;;(setq *WMS_IMAGE_DIR* (vl-string-right-trim "\\" (getvar "TEMPPREFIX"))) ; set *WMS_IMAGE_DIR* to your Temp directory
;(setq *WMS_IMAGE_DIR* "d:\\WMS") ; default Directory to store WMS images
(setq *WMS* "m:\\WMS") ; default Directory to store WMS images
(setq *WMS_CFG_FILE* "g:\\eng\\acad\\tools\\civil-survey\\wms-RAG.cfg")
(setq *GDALWARP* "g:\\eng\\acad\\tools\\gdal\\gdalwarp.exe")
(setq *WMS_TRANSPARENCY* -1)
(setq *WMS_BACK* 'T)
;; *** End of Default Value Definition ***

(setq *WMS_SERVICE* 'nil
      *WMS_CFG* 'nil
)

(defun readfile (filename / f rec res)
  (setq f (open filename "r") res "")
  (while (setq rec (read-line f))
    (setq res (strcat res (vl-string-left-trim " " rec)))
  )
  (close f)
  res
)

(defun unique ( l )
  (if l (cons (car l) (unique (vl-remove (car l) (cdr l)))))
)

(defun get_wms_cfg (service wmscfglist / lst)
  (setq lst (mapcar '(lambda (a) (if (= service (cdr (assoc "name" (car a)))) a 0)) wmscfglist))
  (car (vl-remove-if-not 'listp lst))
)

(defun get_wms_services (crs / lst)
  (setq lst (mapcar '(lambda (a) (if (= crs (cdr (assoc "srs" (car a)))) a 0)) *WMS_CFG*))
  (vl-remove-if-not 'listp lst)
)
(defun setWMSGrid (cfg / alist extent)
  (setq alist (car cfg)
        extent (read (cdr (assoc "extent" alist)))
  )
  (setvar "GRIDSTYLE" 0)
  (setvar "GRIDDISPLAY" 4)
  (setvar "GRIDUNIT" (read (cdr (assoc "tilesize" alist))))
  (setvar "GRIDMAJOR" 1)
  (setvar "LIMMIN" (car extent))
  (setvar "LIMMAX" (cadr extent))
  (setvar "GRIDMODE" 1)
  'T
)
(defun restoreSysVars (oldvars)
  (mapcar (quote (lambda(x) (setvar (car x) (cdr x)))) oldvars)
)


(defun C:WMS (/ newerr olderr sysvars oldvars cfg wmsDir pt pt2 cont crslist servicelist crs wmsservice
                alist extent minx maxx miny maxy tilesize width height minx2 maxx2 miny2 maxy2 x y)

  (defun newerr (msg)
    (if (or (= msg "Function cancelled") (= msg "quit / exit abort"))
      (princ)
      (princ (strcat "error: " msg))
    )
    (setq *error* olderr)
    (command-s "_.UNDO" "_End")
    (restoreSysVars oldvars)
    (princ)
  )

  (setq olderr *error* 
        *error* newerr
  )

  ;save system variables
  (setq sysvars '("CMDECHO" "INSUNITS" "CLAYER" "GRIDSTYLE" "GRIDDISPLAY" "GRIDUNIT" "GRIDMAJOR" "LIMMIN" "LIMMAX" "GRIDMODE" "OSMODE"))
  (setq oldvars (mapcar (quote (lambda(x) (cons x (getvar x)))) sysvars))
        
  (setvar "CMDECHO" 0)
  (setvar "INSUNITS" 0)
  
  ;fetch WMS_CFG directory from Registry
  (setq cfg (vl-registry-read "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_CFG_FILE"))
  (if (and cfg (findfile cfg))
    (setq *WMS_CFG_FILE* cfg)
    (progn
      (setq cfg (getfiled "Select Configuration File" (if *WMS_CFG_FILE* *WMS_CFG_FILE* "") "cfg" 8))
      (if cfg
        (progn
          (setq *WMS_CFG_FILE* cfg)
          (vl-registry-write "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_CFG_FILE" *WMS_CFG_FILE*)
        )
        (vl-exit-with-error "")
      )
    )
  )
  ;fetch WMS image directory from Registry
  (setq wmsDir (vl-registry-read "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_IMAGE_DIR"))
  (if (and wmsDir (vl-file-directory-p wmsDir))
    (setq *WMS_IMAGE_DIR* wmsDir)
    (progn
      (setq wmsDir (if dos_getdir (dos_getdir "Select WMS Image Folder" (if *WMS_IMAGE_DIR* *WMS_IMAGE_DIR* "")) (browseForFolder "Select WMS Image Folder")))
      (if wmsDir
        (progn
          (setq *WMS_IMAGE_DIR* (vl-string-right-trim "\\" wmsDir))
          (vl-registry-write "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_IMAGE_DIR" *WMS_IMAGE_DIR*)
        )
        (vl-exit-with-error "")
      )
    )
  )  
  (setq files (vla-get-files (vla-get-preferences (vlax-get-acad-object)))
        supportPath (strcase (vla-get-supportpath files) T)
  )

  (if (= 0 (logand 1 (getvar "UNDOCTL")))
    (command "_.UNDO" "_All")
  )
  (command "_.UNDO" "_End" "_.UNDO" "_Group")
  (setq pt2 nil
        cont T
  )

  (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*)))
  (setq crslist (unique (mapcar '(lambda (a) (cdr (assoc "srs" (car a)))) *WMS_CFG*)))
  (if (or (not *WMS_EPSG*) (not (member *WMS_EPSG* crslist))) (setq *WMS_EPSG* (car crslist)))
  (setq servicelist (mapcar '(lambda (a) (cdr (assoc "name" (car a)))) (get_wms_services *WMS_EPSG*))) 
  (if (or (not *WMS_SERVICE*) (not (member *WMS_SERVICE* servicelist)))  (setq *WMS_SERVICE* (car servicelist)))
  
  (while
    (and cont
         (setWMSGrid (get_wms_cfg *WMS_SERVICE* (get_wms_services *WMS_EPSG*)))
         (not (initget "Directory configFile Crs Service Window Restore Transparency sendBack Zoom"))
         (setq pt (getpoint (strcat "\nImage <D>irectory/config<F>ile/<C>RS (" *WMS_EPSG* ")/<S>ervice/<W>indow/<R>estore/<T>ransparency (" (itoa *WMS_TRANSPARENCY*) (if (= *WMS_TRANSPARENCY* -1) "...BYLAYER" "") ")/send to <B>ack (" (if *WMS_BACK* "YES" "NO") ")/<Z>oom to Extents/Pick point <" *WMS_SERVICE* ">: ")))
         (member pt '("Directory" "configFile" "Crs" "Service" "Window" "Restore" "Transparency" "sendBack" "Zoom"))
    )
    (cond
       ((= "Directory" pt)
          (setq wmsDir (if dos_getdir (dos_getdir "Select WMS Image Folder" *WMS_IMAGE_DIR*) (browseForFolder "Select WMS Image Folder")))
          (if wmsDir
            (progn
              (setq *WMS_IMAGE_DIR* (vl-string-right-trim "\\" wmsDir))
              (vl-registry-write "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_IMAGE_DIR" *WMS_IMAGE_DIR*)
            )
          )
          (princ (strcat "\nWMS Image Folder: \"" *WMS_IMAGE_DIR* "\""))
       )
       ((= "configFile" pt)
          (setq cfg (getfiled "Select Configuration File" *WMS_CFG_FILE* "cfg" 8))
          (if cfg
            (progn
              (setq *WMS_CFG_FILE* cfg)
              (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*)))
              (setq crslist (unique (mapcar '(lambda (a) (cdr (assoc "srs" (car a)))) *WMS_CFG*)))
              (if (not (member *WMS_EPSG* crslist)) (setq *WMS_EPSG* (car crslist)))
              (setq servicelist (mapcar '(lambda (a) (cdr (assoc "name" (car a)))) (get_wms_services *WMS_EPSG*)))
              (if (not (member *WMS_SERVICE* servicelist)) (setq *WMS_SERVICE* (car servicelist)))
              (vl-registry-write "HKEY_CURRENT_USER\\GIS-Tools-for-CAD" "WMS_CFG_FILE" *WMS_CFG_FILE*)
            )
          )
          (princ (strcat "\nConfiguration File: \"" *WMS_CFG_FILE* "\""))
       )
       ((= "Crs" pt)
          (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*))
                crslist (unique (mapcar '(lambda (a) (cdr (assoc "srs" (car a)))) *WMS_CFG*))
          )
          (setq crs (car (listselect "Select Referencesystem" "CRS Code" 10 10 "false" (vl-sort crslist '<) *WMS_EPSG*)))
          (if crs
            (progn
              (setq *WMS_EPSG* crs
                    servicelist (mapcar '(lambda (a) (cdr (assoc "name" (car a)))) (get_wms_services *WMS_EPSG*)))
              (if (or (not *WMS_SERVICE*) (not (member *WMS_SERVICE* servicelist)))  (setq *WMS_SERVICE* (car servicelist)))
            )
          )
       )
       ((= "Service" pt)
          (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*))
                wmsservice (car (listselect "Select Map Service" "Map Service" 20 40 "false" (vl-sort servicelist '<) *WMS_SERVICE*))
          )
          (if wmsservice (setq *WMS_SERVICE* wmsservice))
       )
       ((= "Window" pt)
        (if (not
              (and (setq pt (getpoint "\n1. Corner point: "))
                   (setq pt2 (getcorner pt "\n2. Corner point: "))
              )
            )
          (setq pt nil)
          (setq cont nil)
        )
       )
       ((= "Restore" pt)
          (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*))
                cont nil
          )
       )
       ((= "Transparency" pt)
         (setq cont2 T)
         (while cont2
           (setq tr (getint "\nTransparency (-1..90): "))
           (if (not tr) (setq cont2 nil))
           (if (and (>= tr -1)
                    (<= tr 90)
               )
             (setq *WMS_TRANSPARENCY* tr
                   cont2 nil
             )
           )
         )
       )
       ((= "sendBack" pt)
         (if *WMS_BACK*
           (setq *WMS_BACK* 'nil)
           (setq *WMS_BACK* 'T)
         )
       )
       ((= "Zoom" pt)
         (if (and *WMS_CFG* *WMS_SERVICE* *WMS_EPSG*)
           (progn
             (setq cfg (get_wms_cfg *WMS_SERVICE* (get_wms_services *WMS_EPSG*))
                   alist (car cfg)
                   extent (read (cdr (assoc "extent" alist)))
             )
             (command "_.ZOOM" "_W" (car extent) (cadr extent))
           )
         )
       )
    )
  )
  (if (= pt "Restore")
    (wmsrestore)
    (if pt
      (progn
        (setq cfg (get_wms_cfg *WMS_SERVICE* (get_wms_services *WMS_EPSG*))
              alist (car cfg)
              tilesize (read (cdr (assoc "tilesize" alist)))
              width (car tilesize)
              height (cadr tilesize)
              extent (read (cdr (assoc "extent" alist)))
              minx (* width (fix (/ (apply 'min (mapcar 'car extent)) width)))
              maxx (* width (fix (/ (apply 'max (mapcar 'car extent)) width)))
              miny (* height (fix (/ (apply 'min (mapcar 'cadr extent)) height)))
              maxy (* height (fix (/ (apply 'max (mapcar 'cadr extent)) height)))
        )
        (if pt2
          (if (and (check_bounds pt minx miny maxx maxy) (check_bounds pt2 minx miny maxx maxy))
            (progn
              (setq minx2 (+ minx (* (fix (/ (abs (- (apply 'min (list (car pt) (car pt2))) minx)) width)) width))
                    maxx2 (if (eq (/ (- (apply 'max (list (car pt) (car pt2))) minx) width)
                                 (fix (/ (- (apply 'max (list (car pt) (car pt2))) minx) width))
                              )
                            (+ minx (* (fix (/ (abs (- (apply 'max (list (car pt) (car pt2))) minx)) width)) width))
                            (+ minx (* (1+ (fix (/ (abs (- (apply 'max (list (car pt) (car pt2))) minx)) width))) width))
                          )
                    miny2 (+ miny (* (fix (/ (abs (- (apply 'min (list (cadr pt) (cadr pt2))) miny)) height)) height))
                    maxy2 (if (eq (/ (- (apply 'max (list (cadr pt) (cadr pt2))) miny) height)
                                 (fix (/ (- (apply 'max (list (cadr pt) (cadr pt2))) miny) height))
                              )
                             (+ miny (* (fix (/ (abs (- (apply 'max (list (cadr pt) (cadr pt2))) miny))  height)) height))
                             (+ miny (* (1+ (fix (/ (abs (- (apply 'max (list (cadr pt) (cadr pt2))) miny))  height))) height))
                          )
                    x    minx2
                    y    miny2
              )
              (setq cont T)
              (while (and cont (< y maxy2))
                (while (and cont (< x maxx2))
                  (setq cont (load_wms (list (+ x (/ width 2)) (+ y (/ height 2))) 'nil cfg nil)
                           x (+ x width)
                  )
                )
                (setq x minx2)
                (setq y (+ y height))
              )
            )
            (princ "\n*** Area is outside the valid coordinate system limits! ***")
          )
          (if pt 
            (if (check_bounds pt minx miny maxx maxy)
              (load_wms pt 'nil cfg nil)
              (princ "\n*** Point is outside the valid coordinate system limits! ***")
            )
          )
        )
      )
    )
  )
  (command "_.UNDO" "_End")
  (restoreSysVars oldvars)
  (princ)
)

(defun check_bounds (pt minx miny maxx maxy / x y)
  (setq x (car pt)
        y (cadr pt)
  )
  (cond
    ((and (>= x minx) (<= x maxx) (>= y miny) (<= y maxy)) T)
    (T nil)
  )
)

(defun wmsrestore ( / ss ent xdata service crs servicelist cfg alist tilesize width height size pt width_old height_old)
  (if (not *WMS_CFG*) (setq *WMS_CFG* (read (readfile *WMS_CFG_FILE*))))
  (if (setq ss (ssget "x" '((0 . "IMAGE")(-3 ("GIS-Tools-for-CAD")))))
    (progn
      (setq i 0)
      (while (> (sslength ss) 0)
        (setq ent (entget (setq e (ssname ss 0)) '("GIS-Tools-for-CAD"))
              xdata (cdadr (assoc -3 ent))
              service (cdar xdata)
              crs (cdadr xdata)
              servicelist (mapcar '(lambda (a) (cdr (assoc "name" (car a)))) (get_wms_services crs))
              cfg (vlax-ldata-get e "GIS-Tools-for-CAD")
        )
        (if cfg
          (progn
            (setq alist (car cfg)
                  imagesize (read (cdr (assoc "imagesize" alist)))
                  width (car imagesize)
                  height (cadr imagesize)
                  size (cdr (assoc 13 ent))
                  pt (mapcar '1+ (cdr (assoc 10 ent)))
                  width_old (car size)
                  height_old (cadr size)
            )
            (if (and
                  (= width_old width)
                  (= height_old height)
                )
              (progn
                (if (not (setq image_path (load_wms pt 'T cfg nil)))
                  (progn
                    (setq cfg (get_wms_cfg service (get_wms_services crs))
                          image_path (load_wms pt 'T cfg nil)
                    )
                  )
                )
                ; replace image path
                (setq img (vlax-ename->vla-object e))
                (vla-put-imagefile img image_path)
                
                (setq i (1+ i))
              )
              (princ (strcat "\n*** Service \"" service "\": image size (" (itoa width) "x" (itoa height) ") <> original size (" (rtos width_old 2 0) "x" (rtos height_old 2 0) ") ***"))
            )
          )
        )
        (setq ss (ssdel e ss))
      )
      (if (> i 0)
        (progn
          (princ "\n*** Image(s) restored ***")
          (if (isBrx)
            (princ "\n*** To reaload all missing images, select the marked rows in the Attachments panel and clear the image cache. ***")
            (princ "\n*** Reload all images with command \"_IMAGE\". ***")
          )
        )
        (princ "\n*** No images restored. ***")
      )
    )
    (princ "\n*** No image found ***")
  )
  (princ)
)

(defun setTransparency (en transparency / ent)
  (setq ent (vlax-ename->vla-object en))
  (vla-put-entitytransparency ent transparency)
)

(defun load_wms (pt reload cfg n / cont x y alist datasource options tileprefix tileNameFunc extent x0 y0 tileName tilesize width height imagesize imagewidth imageheight 
                                 bbox_minx bbox_maxx bbox_miny bbox_maxy format srs url layers dwglayer wmsloaded ext xdata en ent wms cmd tempfile f rec output cont tmp)
  (setq cont T)
  (if 
    (vl-catch-all-error-p
      (if pt
        (progn
          (setq alist (car cfg)
                n (if (not n) 1 n)
                datasource (if (= n 1) (cdr (assoc "datasource" alist)) (cdr (assoc (strcat "datasource_" (itoa n)) alist)))
          )
          (if (not datasource)
            (vl-exit-with-error "\n**** Image creation did not succeed. ****")
          )
          (setq x  (car pt)
                y  (cadr pt)
                tilesize (read (cdr (assoc "tilesize" alist)))
                width (car tilesize)
                height (cadr tilesize)
                name (cdr (assoc "name" alist))
                options (cdr (assoc "options" alist))
                tileprefix (cdr (assoc "tileprefix" alist))
                tileNameFunc (last cfg)
                extent (car (read (cdr (assoc "extent" alist))))
                x0 (* width (fix (/ (car extent) width)))
                y0 (* height (fix (/ (cadr extent) height)))
                tileName (apply tileNameFunc (list x y x0 y0 tileprefix))
                imagesize (read (cdr (assoc "imagesize" alist)))
                imagewidth (car imagesize)
                imageheight (cadr imagesize)
                bbox_minx (+ x0 (* (fix (abs (/ (- x x0) width))) width))
                bbox_maxx (+ bbox_minx width)
                bbox_miny (+ y0 (* (fix (abs (/ (- y y0) height))) height))
                bbox_maxy (+ bbox_miny height)
                format (cdr (assoc "format" alist))
                srs (cdr (assoc "srs" alist))
                dwglayer (cdr (assoc "dwglayer" alist))
          )
          (setq wmsloaded nil)
          (cond
            ((= format "image/jpeg") (setq ext ".jpg"))
            ((= format "image/tiff") (setq ext ".tif"))
            ((= format "image/png") (setq ext ".png"))
          )

          (setq wms (strcat *WMS_IMAGE_DIR* "\\" tileName "_" (vl-string-subst "" ":" srs) ext))
          (if (and (findfile wms)
                   (not (setq wmsloaded (isWMSloaded (vl-filename-base wms))))
              )
            (progn
                (setq xdata (list (list -3 (list "GIS-Tools-for-CAD" (cons 1000 name)(cons 1000 srs))))) 
                (command "_.LAYER" "_make" dwglayer "")
                (setvar "OSMODE" 0)
                (if (isBrx)
                    (command "_.-imageattach" "_f" wms (strcat (itoa bbox_minx) "," (itoa bbox_miny)) width "0")
                    (command "_.-image" "" wms (strcat (itoa bbox_minx) "," (itoa  bbox_miny)) width "0")
                )
                (if *WMS_BACK*
                  (command "_.draworder" "_l" "" "_b")
                )
                (setq en (entlast)
                      ent (append (entget en) xdata)
                )
                (entmod ent)
                (vlax-ldata-put en "GIS-Tools-for-CAD" cfg)
                (if (/= *WMS_TRANSPARENCY* -1)
                  (setTransparency en *WMS_TRANSPARENCY*)
                )
                      
                (princ (strcat "\n**** Image " (findfile wms) " loaded. ****")) 
            )
            (progn
              (if wmsloaded
                (princ (strcat "\n**** Image \"" wms "\" already exists. ****"))
                (if (IsFolderWriteable *WMS_IMAGE_DIR*)
                  (progn
                    (princ (strcat "\n**** Generating Image " wms " with Datasource " (itoa n) "... press Ctrl+C to cancel ****\n"))
                    (setq cmd 
                          (strcat *GDALWARP* " --config GDAL_CACHEMAX 1024 -co NUM_THREADS=ALL_CPUS"
                                  " -t_srs " srs " "
                                  options 
                                  " -te " (itoa bbox_minx) " " (itoa bbox_miny) " " (itoa bbox_maxx) " " (itoa bbox_maxy)
                                  " -te_srs " srs
                                  " -ts " (itoa imagewidth) " " (itoa imageheight) " "
                          )
                    )
                    (setq datasource (vl-string-subst (itoa bbox_minx) "%ulx" datasource)
                          datasource (vl-string-subst (itoa bbox_maxy) "%uly" datasource)
                          datasource (vl-string-subst (itoa bbox_maxx) "%lrx" datasource)
                          datasource (vl-string-subst (itoa bbox_miny) "%lry" datasource)
                          datasource (vl-string-subst (itoa imagewidth) "%sizex" datasource)
                          datasource (vl-string-subst (itoa imageheight) "%sizey" datasource)
                          cmd (strcat cmd
                                  "\"" datasource "\""
                                  " "
                                  wms
                                  " > " (setq output (strcat (getvar "TEMPPREFIX") "wms.log"))
                              )
                    )
                    (setq  tempfile (strcat (getvar "TEMPPREFIX") "run.bat")
                           f (open tempfile "w")
                    )
                    (write-line cmd f)
                    (close f)
                    ;(setq wso (vlax-create-object "WScript.Shell"))
                    ;(vlax-invoke-method wso "Run" tempfile 2 t)
                    
                    (if dos_exewait
                      (dos_exewait tempfile 2)
                      (acet-sys-command tempfile)
                    )
                    ;(if dos_exewait
                    ;  (setq cont (dos_exewait cmd 2))
                    ;  (acet-sys-command cmd)
                    ;)
                    (setq f (open output "r")
                          cont nil
                    )
                    (while (and (not cont) (setq rec (read-line f)))
                      (if (= "done." (substr rec (- (strlen rec) 4)))
                        (setq cont T)
                      )
                    )
                    (close f)
                    
                    (if cont
                      (if (not reload)
                        (if (findfile wms)
                            (progn
                                (setq xdata (list (list -3 (list "GIS-Tools-for-CAD" (cons 1000 *WMS_SERVICE*)(cons 1000 *WMS_EPSG*)))))
                                (command "_.LAYER" "_make" dwglayer "")
                                (setvar "OSMODE" 0)
                                (if (isBrx)
                                    (command "_.-imageattach" "_f" wms (strcat (itoa bbox_minx) "," (itoa bbox_miny)) width "0")
                                    (command "_.-image" "" wms (strcat (itoa bbox_minx) "," (itoa bbox_miny)) width "0")
                                )
                                ;; set background transparency
                                (command "_.transparency" "_l" "" "_on")
                                (if *WMS_BACK*
                                  (command "_.draworder" "_l" "" "_b")
                                )
                                (setq en (entlast)
                                      ent (append (entget en) xdata)
                                )
                                (entmod ent)
                                (vlax-ldata-put en "GIS-Tools-for-CAD" cfg)
                                (if (/= *WMS_TRANSPARENCY* -1)
                                  (setTransparency en *WMS_TRANSPARENCY*)
                                )
                                (princ (strcat "\n**** Image " (findfile wms) " loaded. ****"))
                            )
                            (princ (strcat "\n**** Cannot find Image \"" wms "\". ****"))
                        )
                      )
                      (progn
                        ;; delete the invalid image if process was cancelled by user
                        ;(if (findfile (setq tmp (strcat *WMS_IMAGE_DIR* "\\" wms)))
                        (if (findfile (setq tmp wms))
                          (vl-file-delete tmp)
                        )
                        (if (not (cdr (assoc (strcat "datasource_" (itoa (1+ n))) alist)))
                          (progn
                            (princ "\n**** Image creation cancelled ****")
                            (restoreSysVars oldvars)
                            (vl-exit-with-error "\n**** Image creation did not succeed. ****")
                          )
                          (load_wms pt 'nil cfg (1+ n))
                        )
                      )
                    )
                  )
                  (princ (strcat "\n**** Cannot create image. Folder \"" *WMS_IMAGE_DIR* "\" is read only! ****"))
                )
              )
            )
          )
        )
      )
    ) 'nil wms
  )
)
(defun isWMSloaded (wms / ret ss e)
  (setq ret nil)
  (if (setq ss (ssget "x" '((0 . "image"))))
    (foreach e (ssnamex ss)
      (if (= wms (vla-get-name (vlax-ename->vla-object (cadr e)))) (setq ret T))
    )
  )
  ret
)

(defun IsBrx( / )
   (= (strcase (getvar "program")) "BRICSCAD")
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
(defun IsFolderWriteable ( foldername )
    (and
        (vl-file-directory-p foldername)
        (   (lambda ( tempfilename / handle )
                (if (setq handle (open tempfilename "w"))  
                    (progn
                        (close handle)
                        (vl-file-delete tempfilename)
                        t
                    )
                )
            )
            (vl-filename-mktemp "xxxxxx" foldername ".tmp")
        )    
    )
)
(defun browseForFolder (title / sh folder parentfolder folderobject result)
  (vl-load-com)
  (setq sh (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
  (setq folder (vlax-invoke-method sh 'BrowseForFolder 0 title 0))
  (vlax-release-object sh)

  (if folder
    (progn
      (setq parentfolder (vlax-get-property folder 'ParentFolder))
      (setq FolderObject (vlax-invoke-method ParentFolder 'ParseName (vlax-get-property Folder 'Title)))
      (setq result (vlax-get-property FolderObject 'Path))
      (mapcar 'vlax-release-object (list folder parentfolder folderobject))
      result
    )
  )
)
(princ "\n:: WMS.lsp | © 2024 Christoph Candido | https://github.com/cxcandid ::")
(princ "\n:: Import WMS images - Type \"WMS\" to Invoke ::")
(princ)
