REPORT Y_REQUEST_DOWNLOAD.


DATA: v_trkorr TYPE trkorr.


SELECT-OPTIONS s_reqest FOR v_trkorr OBLIGATORY NO INTERVALS.

PARAMETERS:

*p_reqest TYPE trkorr OBLIGATORY,
p_folder(255) TYPE c LOWER CASE, p_sepr OBLIGATORY.

DATA:

folder TYPE string,

retval LIKE TABLE OF ddshretval WITH HEADER LINE,

fldvalue LIKE help_info-fldvalue,

transdir TYPE text255,

filename(255),

trfile(20) TYPE c,

datatab TYPE TABLE OF text8192 WITH HEADER LINE,

len TYPE i,

flen TYPE i.

TYPE-POOLS: sabc, stms, trwbo.

INITIALIZATION.

  CONCATENATE sy-sysid 'K*' INTO s_reqest-low.

  IF sy-opsys = 'Windows NT'.

    p_sepr = '\'.

  ELSE.

    p_sepr = '/'.

  ENDIF.

  TYPE-POOLS sscr.

  DATA:   restrict       TYPE sscr_restrict,
          opt_list       TYPE sscr_opt_list,
          ass            TYPE sscr_ass.


* Restringir Seleções com intervalo para o parâmetro so_afabe
  CLEAR opt_list.
  MOVE 'JUST_EQ'  TO opt_list-name.
  MOVE 'X' TO opt_list-options-eq.
  APPEND opt_list TO restrict-opt_list_tab.
  CLEAR ass.
  MOVE: 'S'            TO ass-kind,
        'S_REQEST'   TO ass-name,
        'I'            TO ass-sg_main,
        'JUST_EQ'      TO ass-op_main.
  APPEND ass TO restrict-ass_tab.
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = restrict.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_reqest-low.

  DATA:
  tt_system           TYPE TABLE OF tmscsys WITH HEADER LINE,
  es_selected_request TYPE trwbo_request_header,
  es_selected_task    TYPE trwbo_request_header,
  iv_organizer_type   TYPE trwbo_calling_organizer,
  is_selection        TYPE trwbo_selection.

  iv_organizer_type = 'W'. is_selection-reqstatus = 'R'.

  CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
    EXPORTING
      iv_organizer_type   = iv_organizer_type
      is_selection        = is_selection
    IMPORTING
      es_selected_request = es_selected_request
      es_selected_task    = es_selected_task.

  s_reqest-low = es_selected_request-trkorr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.

  DATA: title TYPE string.

  title = 'Select target folder'(005).

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = title
    CHANGING
      selected_folder = folder
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  CALL FUNCTION 'CONTROL_FLUSH'
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  p_folder = folder.

*AT SELECTION-SCREEN ON p_reqest.
*
*  DATA: request_info TYPE stms_wbo_request,
*
*  request_infos TYPE stms_wbo_requests.
*
*  REFRESH request_infos.
*
*  CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_REQUEST'
*    EXPORTING
*      iv_request                 = p_reqest
*      iv_header_only             = 'X'
*    IMPORTING
*      et_request_infos           = request_infos
*    EXCEPTIONS
*      read_config_failed         = 1
*      table_of_requests_is_empty = 2
*      system_not_available       = 3
*      OTHERS                     = 4.
*
*  IF sy-subrc <> 0.
*
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*
*  ENDIF.
*
*  CLEAR request_info.
*
*  READ TABLE request_infos INTO request_info INDEX 1.
*
*  IF sy-subrc NE 0
*  OR request_info-e070-trkorr IS INITIAL.
*
*    MESSAGE e398(00) WITH 'Request'(006) p_reqest 'not found'(007).
*
*  ELSEIF request_info-e070-trstatus NE 'R'.
*
*    MESSAGE e398(00)
*    WITH 'You must release request'(008)
*    request_info-e070-trkorr
*    'before downloading'(009).
*
*  ENDIF.

START-OF-SELECTION.


  folder = p_folder.
  DATA: t_e070 TYPE TABLE OF e070 WITH HEADER LINE.
  DATA: l_request LIKE e070-trkorr.
  DATA: l_like(20).
*  l_request = p_reqest.

  DATA l_index TYPE sy-index.
  DATA: request_info TYPE stms_wbo_request,
            request_infos TYPE stms_wbo_requests.

  LOOP AT s_reqest.

    l_index = sy-tabix.

    REFRESH request_infos.

    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_REQUEST'
      EXPORTING
        iv_request                 = s_reqest-low
        iv_header_only             = 'X'
      IMPORTING
        et_request_infos           = request_infos
      EXCEPTIONS
        read_config_failed         = 1
        table_of_requests_is_empty = 2
        system_not_available       = 3
        OTHERS                     = 4.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    CLEAR request_info.

    READ TABLE request_infos INTO request_info INDEX 1.

    IF sy-subrc NE 0
    OR request_info-e070-trkorr IS INITIAL.
      DELETE s_reqest INDEX l_index.
      MESSAGE e398(00) WITH 'Request'(006) s_reqest-low 'not found'(007).

*    ELSEIF request_info-e070-trstatus NE 'R'.
*      DELETE s_reqest INDEX l_index.
*      MESSAGE e398(00)
*      WITH 'You must release request'(008)
*      request_info-e070-trkorr
*      'before downloading'(009).

    ENDIF.

  ENDLOOP.

  SELECT * FROM e070 INTO TABLE t_e070
    WHERE trkorr IN s_reqest.

  LOOP AT t_e070.
*    p_reqest = t_e070-trkorr.
    CONCATENATE t_e070-trkorr+3(7) '.' t_e070-trkorr(3) INTO trfile.

    CALL FUNCTION 'RSPO_R_SAPGPARAM'
      EXPORTING
        name   = 'DIR_TRANS'
      IMPORTING
        value  = transdir
      EXCEPTIONS
        error  = 0
        OTHERS = 0.

    PERFORM copy_file USING 'cofiles' trfile.
    trfile(1) = 'R'.
    PERFORM copy_file USING 'data' trfile.
    trfile(1) = 'D'.
    PERFORM copy_file USING 'data' trfile.

  ENDLOOP.

*---------------------------------------------------------------------*

* FORM copy_file *

*---------------------------------------------------------------------*

* --> SUBDIR * * --> FNAME *

*---------------------------------------------------------------------*

FORM copy_file USING subdir fname.

  DATA:

  auth_filename TYPE authb-filename,

  gui_filename TYPE string.

  CONCATENATE transdir subdir fname

  INTO filename

  SEPARATED BY p_sepr.

  REFRESH datatab.

  CLEAR flen.

  auth_filename = filename.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity         = sabc_act_read
      filename         = auth_filename
    EXCEPTIONS
      no_authority     = 1
      activity_unknown = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.

    FORMAT COLOR COL_NEGATIVE.

    WRITE: / 'Read access denied. File'(001),

    filename.

    FORMAT COLOR OFF. EXIT.

  ENDIF.

  OPEN DATASET filename FOR INPUT IN BINARY MODE.

  IF sy-subrc NE 0.

    FORMAT COLOR COL_TOTAL.

    WRITE: / 'File open error'(010), filename.

    FORMAT COLOR OFF. EXIT.

  ENDIF.

  DO.

    CLEAR len.

    READ DATASET filename INTO datatab LENGTH len.

    flen = flen + len.

    IF len > 0. APPEND datatab. ENDIF.

    IF sy-subrc NE 0.

      EXIT.

    ENDIF.

  ENDDO.

  CLOSE DATASET filename.

  CONCATENATE p_folder '\' fname INTO gui_filename.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = flen
      filename                = gui_filename
      filetype                = 'BIN'
    CHANGING
      data_tab                = datatab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 24.

  IF sy-subrc = 0.

    WRITE: / 'File'(002), filename, 'downloaded. Length'(003), flen.

  ELSE.

    FORMAT COLOR COL_NEGATIVE.

    WRITE: / 'File download error. Filename:'(004), filename.

    FORMAT COLOR OFF.

  ENDIF.

ENDFORM. "copy_file


*Selection texts
*----------------------------------------------------------
* P_FOLDER         Pasta de destino
* P_SEPR         Separador
* S_REQEST         Request