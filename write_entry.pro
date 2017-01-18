PRO read_runnb, rundir, arxdir, verb

  intkeylist = ['n_spec','nx0','nky0','nz0','nv0','nw0','itime']
  fltkeylist = ['lx','ly','omn','omte','omti','beta','q0','shat',$
                'curv','trpeps','hyp_x','hyp_z','hyp_v','dt_max']
  strkeylist = ['init_cond','nonlinear','read_checkpoint']

  outformat = INTARR(3,13)

  outformat[0,*] = [1,3,3,3,3,3,0,0,0,0,0,0,0] ;not itime
  outformat[1,*] = [6,6,4,4,4,5,3,4,4,4,4,4,4] ;not dt_max
  outformat[2,*] = [3,1,0,0,0,0,0,0,0,0,0,0,0] ;not read_checkpoint

  FORWARD_FUNCTION get_lines, read_par, mruns

  runnb_in = ''
  READ, 'enter run number (#[-#] for archiving, s[#[-#]] for displaying): ', $
    runnb_in
  IF runnb_in EQ 's' THEN BEGIN
    SPAWN, 'emacs arxiv.t &'
    runnb_in = ''
  ENDIF ELSE BEGIN
    IF STRMID(runnb_in,0,1) EQ 's' THEN BEGIN
      ;runnb = FIX(STRTRIM(STRMID(runnb_in,1,STRLEN(runnb_in))))
      runnbarr = mruns(runnb_in,1)
      FOR runnb = runnbarr[0], runnbarr[1] DO BEGIN
        IF runnbarr[0] NE runnbarr[1] THEN $
          PRINT, '>> run ', STRTRIM(STRING(runnb),2), ':'
        lines = get_lines(runnb,arxdir)
        display_run, lines.fl, lines.pl
      ENDFOR
    ENDIF ELSE BEGIN
      ;runnb = FIX(STRTRIM(STRMID(runnb_in,0,STRLEN(runnb_in))))
      runnbarr = mruns(runnb_in,0)
      FOR runnb = runnbarr[0], runnbarr[1] DO BEGIN
        IF runnbarr[0] NE runnbarr[1] THEN $
          PRINT, '>> run ', STRTRIM(STRING(runnb),2), ':'
        writestr = $
          read_par(rundir,runnb,intkeylist,fltkeylist,strkeylist,outformat)
        writeit = 'y'
        IF verb EQ 1 THEN BEGIN
          lines = get_lines(0,arxdir)
          display_run, lines.fl, writestr
          READ, 'write into archive (y/n): ', writeit
        ENDIF
        IF writeit EQ 'y' THEN write_run, runnb, writestr, arxdir ELSE $
          PRINT, 'not writing run'
      ENDFOR
    ENDELSE
  ENDELSE
END


FUNCTION read_par, rundir, runnb, intkeylist, fltkeylist, strkeylist, outformat
  FORWARD_FUNCTION rm0es

  stemp = SIZE(intkeylist)
  ikldim = stemp[1]
  stemp = SIZE(fltkeylist)
  fkldim = stemp[1]
  stemp = SIZE(strkeylist)
  skldim = stemp[1]
  intvals = LONARR(ikldim)
  fltvals = FLTARR(fkldim)
  strvals = STRARR(skldim)

  parpath = rundir + 'parameters_' + STRTRIM(STRING(runnb),2)
  OPENR, parlun, parpath, err=err, /GET_LUN
  IF err NE 0 THEN STOP, parpath, ' does not exist'

;  nrgpath = rundir + 'nrg_' + STRTRIM(STRING(runnb),2)
;  OPENR, nrglun, nrgpath, err=err, /GET_LUN
;  IF err NE 0 THEN STOP, nrgpath, ' does not exist'
;  firststep_time = 0.0
;  READF, nrglun, firststep_time
;  IF firststep_time GT 0.0 THEN follow_run = 1 ELSE follow_run = 0
;  FREE_LUN, nrglun

  line = ''
  isp = -1
  spname = ''
  WHILE NOT EOF(parlun) DO BEGIN
    val = ''
    READF, parlun, line
    key_val = STRTRIM(STRSPLIT(line,'=',/EXTRACT),2)
    key = key_val[0]
    IF N_ELEMENTS(key_val) GT 1 THEN val = key_val[1]

    intvar = -1L
    FOR i = 0, ikldim - 1 DO $
      IF key EQ intkeylist[i] THEN intvals[i] = LONG(val)

    fltvar = -1.0
    FOR i = 0, fkldim - 1 DO $
      IF key EQ fltkeylist[i] THEN fltvals[i] = FLOAT(val)

    strvar = ''
    FOR i = 0, skldim - 1 DO $
      IF key EQ strkeylist[i] THEN strvals[i] = val

; species handling

    nsp = intvals[0] > 1
    IF STRPOS(key,'species') GE 0 THEN isp = isp + 1
    IF isp + 1 LE nsp THEN BEGIN
      IF key EQ 'name' THEN spname = STRMID(val,1,1)
      IF key EQ 'omn' THEN fltvals[2] = FLOAT(val)
      IF key EQ 'omt' THEN BEGIN
        IF spname EQ 'e' THEN fltvals[3] = FLOAT(val)
        IF spname EQ 'i' THEN fltvals[4] = FLOAT(val)
      ENDIF
    ENDIF

  ENDWHILE

  FREE_LUN, parlun

;--- start looking at special fields

  ;simulation time, calculated for linear run; lintime = tsteps * dt
  lintime = intvals[6] * fltvals[13]

  FOR i = 0, skldim - 1 DO BEGIN
    IF (strkeylist[i] EQ 'nonlinear') AND (strvals[i] EQ 'T') THEN $
      strvals[i] = 'N'
    IF (strkeylist[i] EQ 'nonlinear') AND (strvals[i] EQ 'F') THEN $
      strvals[i] = 'L'
  ENDFOR

  IF strvals[2] EQ 'T' THEN follow_run = 1 ELSE follow_run = 0

;--- end looking at special fields

  ;compose output string
  outstring = rm0es(runnb)
  FOR i = 0, 4 - STRLEN(outstring) DO outstring = outstring + ' '
  outstring = outstring + ':'

  truncated = 0
  FOR i = 0, ikldim - 2 DO BEGIN
    addstring = rm0es(intvals[i])
    IF STRLEN(addstring) GT outformat[0,i] THEN BEGIN
      addstring = STRMID(addstring,0,outformat[0,i])
      truncated = 1
      PRINT, 'truncated ', intkeylist[i]
    ENDIF
    FOR j = 0, outformat[0,i] - STRLEN(addstring) DO $
      addstring = ' ' + addstring
    outstring = outstring + addstring
  ENDFOR
  FOR i = 0, fkldim - 2 DO BEGIN
    addstring = rm0es(fltvals[i])
    IF fltkeylist[i] EQ 'ly' THEN addstring = STRMID(addstring,0,6)
    IF STRLEN(addstring) GT outformat[1,i] THEN BEGIN
      addstring = STRMID(addstring,0,outformat[1,i])
      truncated = 1
      PRINT, 'truncated ', fltkeylist[i]
    ENDIF
    FOR j = 0, outformat[1,i] - STRLEN(addstring) DO $
      addstring = ' ' + addstring
    outstring = outstring + addstring
  ENDFOR
  FOR i = 0, skldim - 2 DO BEGIN
    addstring = strvals[i]
    IF STRMID(addstring,0,1) EQ "'" THEN addstring = $
      STRMID(addstring,1,STRLEN(addstring)-2)
    IF STRLEN(addstring) GT outformat[2,i] THEN BEGIN
      addstring = STRMID(addstring,0,outformat[2,i])
      truncated = 1
      PRINT, 'truncated ', strkeylist[i]
    ENDIF
    FOR j = 0, outformat[2,i] - STRLEN(addstring) DO $
      addstring = ' ' + addstring
    outstring = outstring + addstring
  ENDFOR

  addstring = rm0es(lintime)
  temp = STRSPLIT(addstring,'.',/EXTRACT)
  addstring = temp[0]
  IF STRLEN(addstring) GT 5 THEN BEGIN
    addstring = STRMID(addstring,0,5)
    truncated = 1
    PRINT, 'truncated lintime'
  ENDIF
  FOR j = 0, 5 - STRLEN(addstring) DO $
    addstring = ' ' + addstring
  outstring = outstring + addstring

  IF follow_run EQ 1 THEN BEGIN
    outstring = outstring + ' F'
    PRINT, 'follow-up run'
  ENDIF

  IF truncated EQ 1 THEN BEGIN
;    IF follow_run EQ 0 THEN outstring = outstring + '  '
;    outstring = outstring + ' TR'
    PRINT, 'output truncated'
  ENDIF

  RETURN, outstring

END


PRO write_run, runnb, writestr, arxdir

  arxpath = arxdir + 'arxiv.t'
  temparxpath = arxdir + 'temparx'
  OPENR, arxlun, arxpath, /GET_LUN
  OPENW, tarxlun, temparxpath, ERROR=err, /GET_LUN
  IF err NE 0 THEN BEGIN
    PRINT, 'error opening temp file'
    RETURN
  ENDIF
  run_inserted = 0
  line = ' '
  WHILE NOT EOF(arxlun) DO BEGIN
    READF, arxlun, line
    IF (STRMID(line,0,4) EQ 'run ') OR (STRMID(line,0,4) EQ '----') THEN $
      line_run = 0 ELSE line_run = FIX(STRTRIM(STRMID(line,0,4),2))
    ;PRINT, 'run found: ', STRTRIM(STRMID(line,0,4),2)
;change the number of digits for the run number:
;IF STRMID(line,0,4) EQ 'run ' THEN line = 'run  ' + STRMID(line,4,1000)
;IF line_run NE 0 THEN BEGIN
;  ltemp = '     '
;  STRPUT, ltemp, STRTRIM(STRING(line_run),2)
;  line = ltemp + STRMID(line,4,1000)
;ENDIF
    IF runnb GT line_run THEN PRINTF, tarxlun, line
    IF (runnb LT line_run) AND (run_inserted EQ 0) THEN BEGIN $
      run_inserted = 1
      PRINTF, tarxlun, writestr
      PRINT, 'run inserted'
    ENDIF
    IF (runnb LT line_run) AND (run_inserted EQ 1) THEN PRINTF, tarxlun, line
    IF runnb EQ line_run THEN BEGIN
      run_inserted = 1
      overwrite_run = ' '
      READ, 'run already exists, overwrite (y/n): ', overwrite_run
      IF overwrite_run EQ 'y' THEN BEGIN
        PRINTF, tarxlun, writestr
        PRINT, 'run replaced'
      ENDIF ELSE BEGIN
        PRINTF, tarxlun, line
        PRINT, 'run not replaced'
      ENDELSE
    ENDIF
  ENDWHILE
  IF run_inserted EQ 0 THEN BEGIN
    PRINTF, tarxlun, writestr
    PRINT, 'run appended'
  ENDIF
  FREE_LUN, arxlun
  OPENW, arxlun, 'arxiv.t'
  POINT_LUN, tarxlun, 0
  WHILE NOT EOF(tarxlun) DO BEGIN
    READF, tarxlun, line
    PRINTF, arxlun, line
  ENDWHILE
  FREE_LUN, arxlun
  FREE_LUN, tarxlun
  SPAWN, 'rm ' + arxdir + 'temparx'

END


FUNCTION rm0es, flt_in, prec=prec
; This function takes a float/integer/string and returns a string
; with blanks and leading/trailing zeroes removed. If arrays of
; floats/integers/strings are provided, string arrays of same
; dimension are returned. If prec is set to an integer value, prec
; digits after the decimal point are returned. Note that setting
; prec=0 does not perform a ROUND conversion, use that function
; instead to get the nearest integer

  str_out_arr = STRARR(N_ELEMENTS(flt_in))

  FOR k = 1, N_ELEMENTS(flt_in) DO BEGIN
    str_temp = STRTRIM(STRING(flt_in[k-1]),2)
    str_temp_length = STRLEN(str_temp)

    intdetect = 1B
    FOR i = 0, str_temp_length - 1 DO $
      IF STRMID(str_temp,i,1) EQ '.' THEN intdetect = 0B

    IF intdetect EQ 1B THEN str_out = str_temp ELSE BEGIN
      IF KEYWORD_SET(prec) THEN BEGIN
        flt_mod = FLOAT(str_temp)
        flt_mod = FLOAT(ROUND(flt_mod*10.0^prec)) * 10.0^(-prec)
        str_temp = STRTRIM(STRING(flt_mod),2)
        str_temp_length = STRLEN(str_temp)
      ENDIF

      act_char = '0'
      i = 1
      WHILE (act_char EQ '0') AND (i LE str_temp_length) DO BEGIN
        j = str_temp_length - i
        act_char = STRMID(str_temp,j,1)
        i = i + 1
      ENDWHILE
      str_out = STRMID(str_temp,0,j+1)
      str_out_length = STRLEN(str_out)
      IF STRMID(str_out,str_out_length-1,1) EQ '.' THEN $
        str_out = STRMID(str_out,0,str_out_length-1)
    ENDELSE

    str_out_arr[k-1] = str_out
  ENDFOR

  IF N_ELEMENTS(flt_in) EQ 1 THEN RETURN, str_out_arr[0]
  RETURN, str_out_arr

END

FUNCTION rm0es1, flt_in

; warning: this is an older version

;this function takes a float/integer and returns a
;string with blanks and trailing zeroes removed

  str_temp = STRTRIM(STRING(flt_in),2)
  str_temp_length = STRLEN(str_temp)

  ;if input is an integer, return string trimmed input
  intdetect = 1B
  FOR i = 0, str_temp_length - 1 DO $
    IF STRMID(str_temp,i,1) EQ '.' THEN intdetect = 0B
  IF intdetect EQ 1B THEN RETURN, str_temp

  act_str = '0'
  i = 1
  WHILE (act_str EQ '0') AND (i LE str_temp_length) DO BEGIN
    j = str_temp_length - i
    act_str = STRMID(str_temp,j,1)
    i = i + 1
  ENDWHILE
  str_out = STRMID(str_temp,0,j+1)
  str_out_temp_length_mo = STRLEN(str_out)-1
  IF STRMID(str_out,str_out_temp_length_mo,1) EQ '.' THEN $
    str_out = STRMID(str_out,0,str_out_temp_length_mo)

  RETURN, str_out

END


FUNCTION get_lines, runnb, arxdir
  arxpath = arxdir + 'arxiv.t'
  OPENR, arxlun, arxpath, /GET_LUN
  firstlinedone = 0
  line_run = 0
  line = ''
  line_print = ''
  WHILE (NOT EOF(arxlun)) AND (line_run LE runnb) DO BEGIN
    READF, arxlun, line
    IF firstlinedone EQ 0 THEN BEGIN
      firstline = line
      firstlinedone = 1
    ENDIF
    IF (STRMID(line,0,5) EQ 'run  ') OR (STRMID(line,0,5) EQ '-----') THEN $
      line_run = 0 ELSE line_run = FIX(STRTRIM(STRMID(line,0,5),2))
    IF runnb EQ line_run THEN line_print = line
  ENDWHILE
  FREE_LUN, arxlun
  RETURN, {fl:firstline,pl:line_print}
END

PRO display_run, firstline, line_print
  PRINT, '--------------------------------------------------------------'
  PRINT, STRMID(firstline,6,62)
  PRINT, STRMID(line_print,6,62)
  PRINT, '--------------------------------------------------------------'
  PRINT, STRMID(firstline,69,STRLEN(firstline))
  PRINT, STRMID(line_print,69,STRLEN(firstline))
  PRINT, '--------------------------------------------------------------'
END


FUNCTION mruns, runnbstr, showrun
; function takes mult. runnb string and returns integer array

  dashindex = 0
  FOR i = showrun, STRLEN(runnbstr) DO $
    IF STRMID(runnbstr,i,1) EQ '-' THEN dashindex = i
  runnb_int = INTARR(2)
  IF dashindex GT 0 THEN BEGIN
    runnb_int[0] = FIX(STRMID(runnbstr,showrun,dashindex-showrun))
    runnb_int[1] = FIX(STRMID(runnbstr,dashindex+1,STRLEN(runnbstr)))
  ENDIF ELSE BEGIN
    runnb_int[0] = FIX(STRMID(runnbstr,showrun,STRLEN(runnbstr)))
    runnb_int[1] = runnb_int[0]
  ENDELSE
  IF runnb_int[0] GT runnb_int[1] THEN BEGIN
    tempvar0 = runnb_int[1]
    runnb_int[1] = runnb_int[0]
    runnb_int[0] = tempvar0
  ENDIF

  RETURN, runnb_int
END

