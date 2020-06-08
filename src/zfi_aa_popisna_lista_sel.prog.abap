*&---------------------------------------------------------------------*
*& Include          ZFI_AA_POPISNA_LISTA_SEL
*&---------------------------------------------------------------------*
TABLES: csksz, anlz,t527x, pa0001, anla.

DATA: gt_anla0 TYPE STANDARD TABLE OF anla0,
      gt_anlav TYPE STANDARD TABLE OF anlav,
      gt_anlv  TYPE STANDARD TABLE OF anlv,
      gt_anlt  TYPE STANDARD TABLE OF anlt,
      gt_anlz  TYPE STANDARD TABLE OF anlz,
      gt_anlb  TYPE STANDARD TABLE OF anlb,
      gt_anlk  TYPE STANDARD TABLE OF anlk,
      gt_anlp  TYPE STANDARD TABLE OF anlp,
      gt_anek  TYPE STANDARD TABLE OF anek,
      gt_anepv TYPE STANDARD TABLE OF anepv,
      gt_anlcv TYPE STANDARD TABLE OF anlcv.

DATA: gt_data TYPE STANDARD TABLE OF zfi_aa_popisna_lista,
      gs_data LIKE LINE OF gt_data.

DATA: go_cont TYPE REF TO cl_gui_custom_container.
DATA: go_grd TYPE REF TO cl_gui_alv_grid.

DATA: code_ok TYPE sy-ucomm.

DATA: gt_fld TYPE lvc_t_fcat,
      gs_fld TYPE lvc_s_fcat.

SELECTION-SCREEN BEGIN OF BLOCK list WITH FRAME TITLE TEXT-000.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_list1 RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND ucom.
SELECTION-SCREEN COMMENT 10(18) TEXT-001 FOR FIELD p_list1.
PARAMETERS: p_list2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 35(18) TEXT-002 FOR FIELD p_list2.
PARAMETERS: p_list3 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 65(22) TEXT-003 FOR FIELD p_list3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK list.

SELECTION-SCREEN BEGIN OF BLOCK selection1 WITH FRAME TITLE TEXT-006.
PARAMETERS p_bukrs TYPE bukrs.
SELECT-OPTIONS so_anlkl FOR anla-anlkl.
SELECTION-SCREEN END OF BLOCK selection1.

SELECTION-SCREEN BEGIN OF BLOCK selection2 WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS: so_kost FOR csksz-kostl.
SELECT-OPTIONS: so_stort FOR anlz-stort.
SELECT-OPTIONS: so_raumn FOR anlz-raumn.
SELECT-OPTIONS: so_orgeh FOR t527x-orgeh.
SELECT-OPTIONS: so_pernr FOR pa0001-pernr.
SELECTION-SCREEN END OF BLOCK selection2.

SELECTION-SCREEN BEGIN OF BLOCK podesavanja WITH FRAME TITLE TEXT-005.
PARAMETERS p_datum TYPE sy-datum.
SELECTION-SCREEN END OF BLOCK podesavanja.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_list1 = 'X'.
      IF screen-name CS 'so_stort'.
        screen-active = 1.
      ELSEIF screen-name CS 'so_raumn'.
        screen-active = 1.
      ELSEIF screen-name CS 'so_orgeh'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_pernr'.
        screen-active = 0.
      ENDIF.
    ELSEIF p_list2 = 'X'.
      IF screen-name CS 'so_stort'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_raumn'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_orgeh'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_pernr'.
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-name CS 'so_stort'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_raumn'.
        screen-active = 0.
      ELSEIF screen-name CS 'so_orgeh'.
        screen-active = 1.
      ELSEIF screen-name CS 'so_pernr'.
        screen-active = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  DATA: ls_callback TYPE ldbcb,
        lt_callback TYPE STANDARD TABLE OF ldbcb.

  DATA: ls_selections TYPE rsparams,
        lt_selections TYPE STANDARD TABLE OF rsparams.

*  CLEAR ls_callback.
*  ls_callback-ldbnode = 'ANLA0'.
*  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
*  ls_callback-cb_prog = sy-repid.
*  ls_callback-cb_form = 'CALLBACK_ANLA0'.
*  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLAV'.
  ls_callback-get = 'X'.
  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLAV'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLV'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLV'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLT'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLT'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLZ'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLZ'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLB'.
  ls_callback-get = 'X'.
  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLB'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLCV'.
  ls_callback-get = 'X'.
  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLCV'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLK'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLK'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANLP'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANLP'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANEK'.
  ls_callback-get = 'X'.
  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANEK'.
  APPEND ls_callback TO lt_callback.

  CLEAR ls_callback.
  ls_callback-ldbnode = 'ANEPV'.
  ls_callback-get = 'X'.
*  ls_callback-get_late = 'X'.
  ls_callback-cb_prog = sy-repid.
  ls_callback-cb_form = 'CALLBACK_ANEPV'.
  APPEND ls_callback TO lt_callback.

********************************************************************
  " selection parameters
  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'BERDATUM'.
  ls_selections-low = p_datum.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'XEINZEL'.
  ls_selections-low = 'X'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'PA_XANLG'.
  ls_selections-low = 'X'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'PA_XSETL'.
  ls_selections-low = 'X'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'BEREICH1'.
  ls_selections-low = '32'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'PA_VERSN'.
  ls_selections-low = '000'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'PA_IMAGN'.
  ls_selections-low = '0000'.
  APPEND ls_selections TO lt_selections.

*  CLEAR ls_selections.
*  ls_selections-kind = 'P'.
*  ls_selections-selname = 'PA_AI_ID'.
*  ls_selections-low = '000000000000'.
*  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections.
  ls_selections-kind = 'P'.
  ls_selections-selname = 'BER1_TXT'.
  ls_selections-low = 'IFRS lok.v'.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections. "DONE
  ls_selections-kind = 'S'.
  ls_selections-selname = 'SO_KOSTL'.
  ls_selections-sign = so_kost-sign.
  ls_selections-option = so_kost-option.
  ls_selections-low = so_kost-low.
  ls_selections-high = so_kost-high.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections. "DONE
  ls_selections-kind = 'S'.
  ls_selections-selname = 'SO_STORT'.
  ls_selections-sign = so_stort-sign.
  ls_selections-option = so_stort-option.
  ls_selections-low = so_stort-low.
  ls_selections-high = so_stort-high.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections. "DONE
  ls_selections-kind = 'S'.
  ls_selections-selname = 'SO_ANLKL'.
  ls_selections-sign = so_anlkl-sign.
  ls_selections-option = so_anlkl-option.
  ls_selections-low = so_anlkl-low.
  ls_selections-high = so_anlkl-high.
  APPEND ls_selections TO lt_selections.

  CLEAR ls_selections. "done
  ls_selections-kind = 'P'.
  ls_selections-selname = 'BUKRS'.
  ls_selections-low = p_bukrs.
  APPEND ls_selections TO lt_selections.

  CALL FUNCTION 'LDB_PROCESS'
    EXPORTING
      ldbname                     = 'ADA'
*     VARIANT                     =
*     EXPRESSIONS                 =
*     field_selection             =
*     DYN_NODE_TYPES              =
    TABLES
      callback                    = lt_callback
      selections                  = lt_selections
    EXCEPTIONS
      ldb_not_reentrant           = 1
      ldb_incorrect               = 2
      ldb_already_running         = 3
      ldb_error                   = 4
      ldb_selections_error        = 5
      ldb_selections_not_accepted = 6
      variant_not_existent        = 7
      variant_obsolete            = 8
      variant_error               = 9
      free_selections_error       = 10
      callback_no_event           = 11
      callback_node_duplicate     = 12
      callback_no_program         = 13
      callback_no_cbform          = 14
      dyn_node_no_type            = 15
      dyn_node_invalid_type       = 16
      OTHERS                      = 17.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*  BREAK-POINT.

  CALL SCREEN 100.

FORM callback_anla0 USING
      name TYPE ldbn-ldbnode
      workarea TYPE anla0
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anla0.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anla0.

ENDFORM.

FORM callback_anlav USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlav
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlav.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlav.

ENDFORM.

FORM callback_anlv USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlv
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlv.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlv.

ENDFORM.

FORM callback_anlt USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlt
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlt.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlt.

ENDFORM.

FORM callback_anlz USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlz
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlz.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlz.

ENDFORM.

FORM callback_anlb USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlb
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlb.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlb.

ENDFORM.

FORM callback_anlcv USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlcv
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlcv.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlcv.

ENDFORM.

FORM callback_anlk USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlk
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlk.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlk.

ENDFORM.

FORM callback_anlp USING
      name TYPE ldbn-ldbnode
      workarea TYPE anlp
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anlp.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anlp.

ENDFORM.

FORM callback_anek USING
      name TYPE ldbn-ldbnode
      workarea TYPE anek
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anek.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anek.

ENDFORM.


FORM callback_anepv USING
      name TYPE ldbn-ldbnode
      workarea TYPE anepv
      mode TYPE c
      selected TYPE c.
  DATA: ls_positions TYPE anepv.

  MOVE-CORRESPONDING workarea TO ls_positions.
  APPEND ls_positions TO gt_anepv.

ENDFORM.

FORM make_fld.

  LOOP AT gt_fld INTO gs_fld.

    CASE gs_fld-fieldname.
      WHEN 'NAZ_SRED'.
        gs_fld-reptext = 'Naziv sredstva'.
        gs_fld-seltext = 'Naziv sredstva'.
        gs_fld-scrtext_s = 'Naz. sred.'.
        gs_fld-scrtext_m = 'Naziv sredstva'.
        gs_fld-scrtext_l = 'Naziv sredstva'.
      WHEN 'NAZ_LOKAC'.
        gs_fld-reptext = 'Naziv lokacije'.
        gs_fld-seltext = 'Naziv lokacije'.
        gs_fld-scrtext_s = 'Naz. lok.'.
        gs_fld-scrtext_m = 'Naziv lokacije'.
        gs_fld-scrtext_l = 'Naziv lokacije'.
      WHEN 'NAZ_MESTA'.
        gs_fld-reptext = 'Naziv mesta troška'.
        gs_fld-seltext = 'Naziv mesta troška'.
        gs_fld-scrtext_s = 'Naz. MT'.
        gs_fld-scrtext_m = 'Naziv mesta troška'.
        gs_fld-scrtext_l = 'Naziv MT'.
      WHEN 'NAZ_PROS'.
        gs_fld-reptext = 'Naziv prostorije'.
        gs_fld-seltext = 'Naziv prostorije'.
        gs_fld-scrtext_s = 'Naz. prost.'.
        gs_fld-scrtext_m = 'Naziv prostorije'.
        gs_fld-scrtext_l = 'Naz. prostorije'.
      WHEN 'RAUMN'.
        gs_fld-reptext = 'Prostorija'.
        gs_fld-seltext = 'Prostorija'.
        gs_fld-scrtext_s = 'Prostorija'.
        gs_fld-scrtext_m = 'Prostorija'.
        gs_fld-scrtext_l = 'Prostorija'.
      WHEN 'INVNR'.
        gs_fld-reptext = 'Inventarni broj'.
        gs_fld-seltext = 'Inventarni broj'.
        gs_fld-scrtext_s = 'Inven. br.'.
        gs_fld-scrtext_m = 'Inventarni broj'.
        gs_fld-scrtext_l = 'Inventarni br.'.
      WHEN 'RBR'.
        gs_fld-reptext = 'Redni broj'.
        gs_fld-seltext = 'Redni broj'.
        gs_fld-scrtext_s = 'Rbr.'.
        gs_fld-scrtext_l = 'Redni broj'.
        gs_fld-scrtext_m = 'Rbr.'.
      WHEN 'ANLN1'.
        gs_fld-reptext = 'SAP broj sredstva'.
        gs_fld-seltext = 'SAP broj sredstva'.
        gs_fld-scrtext_s = 'SAP br. sred.'.
        gs_fld-scrtext_l = 'SAP broj sredstva'.
        gs_fld-scrtext_m = 'SAP br. sredstva'.
      WHEN 'PERNR'.
        gs_fld-reptext = 'Kadrovski broj'.
        gs_fld-seltext = 'Kadrovski broj'.
        gs_fld-scrtext_s = 'Kadr. br.'.
        gs_fld-scrtext_l = 'Kadrovski broj'.
        gs_fld-scrtext_m = 'Kadrovski br.'.
      WHEN 'IME'.
        gs_fld-reptext = 'Ime i prezime zaposlenog'.
        gs_fld-seltext = 'Ime i prezime zaposlenog'.
        gs_fld-scrtext_s = 'Ime i prez.'.
        gs_fld-scrtext_l = 'Ime i prezime zaposlenog'.
        gs_fld-scrtext_m = 'Ime i prez.'.
      WHEN 'KOLICINA'.
        gs_fld-reptext = 'Količina'.
        gs_fld-seltext = 'Količina'.
        gs_fld-scrtext_s = 'Količina'.
        gs_fld-scrtext_l = 'Količina'.
        gs_fld-scrtext_m = 'Količina'.
      WHEN 'NAPOMENA'.
        gs_fld-reptext = 'Napomena'.
        gs_fld-seltext = 'Napomena'.
        gs_fld-scrtext_s = 'Napomena'.
        gs_fld-scrtext_l = 'Napomena'.
        gs_fld-scrtext_m = 'Napomena'.
    ENDCASE.
    MODIFY gt_fld FROM gs_fld.
  ENDLOOP.
ENDFORM.

FORM print.

  DATA: fm_name         TYPE rs38l_fnam,
        fp_docparams    TYPE sfpdocparams,
        fp_outputparams TYPE sfpoutputparams.

  CALL FUNCTION 'FP_JOB_OPEN'                   "& Form Processing: Call Form
    CHANGING
      ie_outputparams = fp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
*            <error handling>
  ENDIF.

  IF p_list1 = 'X'.
    BREAK vkucenic.
    SORT gt_data ASCENDING BY kostl stort raumn pernr anln1.

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'ZFI_AA_POPISNA_LISTA' " forma za osnovna sredstva
      IMPORTING
        e_funcname = fm_name.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.

    fp_docparams-dynamic = 'X'.

    CALL FUNCTION fm_name
      EXPORTING
        /1bcdwb/docparams = fp_docparams
        it_data           = gt_data
        iv_type_list      = '1'
*         IMPORTING
*       /1BCDWB/FORMOUTPUT       =
      EXCEPTIONS
        usage_error       = 1
        system_error      = 2
        internal_error    = 3.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.

  ELSEIF p_list2 = 'X'.
    SORT gt_data ASCENDING BY kostl stort.

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'ZFI_AA_POPISNA_LISTA2' " forma za podstanice i mreze
      IMPORTING
        e_funcname = fm_name.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.

    fp_docparams-dynamic = 'X'.

    CALL FUNCTION fm_name
      EXPORTING
        /1bcdwb/docparams = fp_docparams
        it_data           = gt_data
        iv_type_list      = '2'
*         IMPORTING
*       /1BCDWB/FORMOUTPUT       =
      EXCEPTIONS
        usage_error       = 1
        system_error      = 2
        internal_error    = 3.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.
  ELSEIF p_list3 = 'X'.
    SORT gt_data ASCENDING BY kostl orgeh pernr.

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'ZFI_AA_POPISNA_LISTA3' " forma za alat i sitan inventar
      IMPORTING
        e_funcname = fm_name.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.

    fp_docparams-dynamic = 'X'.

    CALL FUNCTION fm_name
      EXPORTING
        /1bcdwb/docparams = fp_docparams
        it_data           = gt_data
        iv_type_list      = '3'
*         IMPORTING
*       /1BCDWB/FORMOUTPUT       =
      EXCEPTIONS
        usage_error       = 1
        system_error      = 2
        internal_error    = 3.
    IF sy-subrc <> 0.
*  <error handling>
    ENDIF.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
*    IMPORTING
*     E_RESULT             =
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
*            <error handling>
  ENDIF.

ENDFORM.
