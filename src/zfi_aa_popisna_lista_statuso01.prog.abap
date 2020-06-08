*----------------------------------------------------------------------*
***INCLUDE ZFI_AA_POPISNA_LISTA_STATUSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'POPISNA_LISTA'.
  SET TITLEBAR 'POPISNA_LISTA'.

  DATA: ls_anlav   LIKE LINE OF gt_anlav,
        lv_ime     TYPE string,
        lv_prezime TYPE string.

  DATA: lv_variant TYPE string.

  DELETE ADJACENT DUPLICATES FROM gt_anlav.
  CLEAR gt_data.

  IF p_list1 EQ 'X'. " lista osnovna sredstva
    lv_variant = '/OSN.SREDSTV'.
    LOOP AT gt_anlav INTO ls_anlav.

      " deo preuzet iz standardnog programa
      IF NOT ls_anlav-deakt IS INITIAL.
        CHECK ls_anlav-deakt GT p_datum.
      ENDIF.
      CHECK ls_anlav-inken = 'X'.
*      IF ls_anlav-invnr IS INITIAL.
*        WRITE ls_anlav-anln1  TO ls_anlav-invnr+00(12).
*        ls_anlav-invnr+13(04) = ls_anlav-anln2.
*        CONDENSE ls_anlav-invnr.
*      ENDIF.

      " da li zadovoljavaju podaci selection screen za sobu-prostoriju
      IF ls_anlav-raumn IN so_raumn.
        MOVE-CORRESPONDING ls_anlav TO gs_data.
*        IF gs_data-kostl IS INITIAL.
*           gs_data-kostl = 1.
*        ENDIF.

        " naziv sredstva sredstva
        gs_data-naz_sred = ls_anlav-txt50.

        " ime i prezime zaposlenog
        SELECT SINGLE vorna, nachn FROM m_premn
          INTO ( @lv_ime, @lv_prezime )
          WHERE pernr = @gs_data-pernr.
        CONCATENATE lv_ime lv_prezime INTO gs_data-ime SEPARATED BY space.

        " naziv lokacije
        SELECT SINGLE ktext FROM t499s INTO gs_data-naz_lokac
          WHERE werks = ls_anlav-werks
            AND stand = gs_data-stort.

        " naziv mesta troska
        SELECT SINGLE ktext FROM cskt INTO gs_data-naz_mesta
          WHERE spras = sy-langu
            AND kostl = gs_data-kostl.

        " adresa mesta troska
        SELECT SINGLE stras FROM csks INTO gs_data-stras
          WHERE kostl = gs_data-kostl.

        " organzaciona jedinica
        SELECT SINGLE orgeh FROM pa0001
          INTO gs_data-orgeh
          WHERE kostl = gs_data-kostl
            AND pernr = gs_data-pernr.

        " opis prostorije
        SELECT SINGLE opis FROM zfi_aa_soba_os
          INTO gs_data-naz_pros
          WHERE raumn = gs_data-raumn.

        APPEND gs_data TO gt_data.
        CLEAR: gs_data, lv_ime, lv_prezime.
      ENDIF.
    ENDLOOP.

*    SORT gt_data ASCENDING BY pernr.

  ELSEIF p_list2 EQ 'X'. " lista podstanice i mreze

    lv_variant = '/PODS.MREZE'.
    LOOP AT gt_anlav INTO ls_anlav.
      IF NOT ls_anlav-deakt IS INITIAL.                     "> 546126
        CHECK ls_anlav-deakt GT p_datum.                    "> 803017
      ENDIF.
      CHECK ls_anlav-inken = 'X'.
*      IF ls_anlav-invnr IS INITIAL.
*        WRITE ls_anlav-anln1  TO ls_anlav-invnr+00(12).
*        ls_anlav-invnr+13(04) = ls_anlav-anln2.
*        CONDENSE ls_anlav-invnr.
*      ENDIF.
      MOVE-CORRESPONDING ls_anlav TO gs_data.

      " naziv sredstva sredstva
      gs_data-naz_sred = ls_anlav-txt50.

      " ime i prezime zaposlenog
*      SELECT SINGLE vorna, nachn FROM m_premn
*        INTO (@lv_ime, @lv_prezime )
*        WHERE pernr = @gs_data-pernr.
*      CONCATENATE lv_ime lv_prezime INTO gs_data-ime SEPARATED BY space.

      " naziv lokacije
      SELECT SINGLE ktext FROM t499s INTO gs_data-naz_lokac
        WHERE werks = ls_anlav-werks
          AND stand = gs_data-stort.

      " naziv mesta troska
      SELECT SINGLE ktext FROM cskt INTO gs_data-naz_mesta
        WHERE spras = sy-langu
          AND kostl = gs_data-kostl.

      " adresa mesta troska
      SELECT SINGLE stras FROM csks INTO gs_data-stras
        WHERE kostl = gs_data-kostl.

      " organzaciona jedinica
*      SELECT SINGLE orgeh FROM pa0001
*        INTO gs_data-orgeh
*        WHERE kostl = gs_data-kostl
*          AND pernr = gs_data-pernr.

      " opis prostorije
      SELECT SINGLE opis FROM zfi_aa_soba_os
        INTO gs_data-naz_pros
        WHERE raumn = gs_data-raumn.

      APPEND gs_data TO gt_data.
      CLEAR: gs_data, lv_ime, lv_prezime.
    ENDLOOP.

  ELSEIF p_list3 EQ 'X'. " alat i sitan inventar

    lv_variant = '/ALATI_INVEN'.
    LOOP AT gt_anlav INTO ls_anlav.
      IF NOT ls_anlav-deakt IS INITIAL.                     "> 546126
        CHECK ls_anlav-deakt GT p_datum.                    "> 803017
      ENDIF.
      CHECK ls_anlav-inken = 'X'.
*      IF ls_anlav-invnr IS INITIAL.
*        WRITE ls_anlav-anln1  TO ls_anlav-invnr+00(12).
*        ls_anlav-invnr+13(04) = ls_anlav-anln2.
*        CONDENSE ls_anlav-invnr.
*      ENDIF.

      IF ls_anlav-pernr IN so_pernr .

        MOVE-CORRESPONDING ls_anlav TO gs_data.

        " organzaciona jedinica
        SELECT SINGLE orgeh FROM pa0001
          INTO gs_data-orgeh
          WHERE kostl = gs_data-kostl
            AND pernr = gs_data-pernr.

        IF gs_data-orgeh IN so_orgeh.
          " naziv sredstva sredstva
          gs_data-naz_sred = ls_anlav-txt50.

          " ime i prezime zaposlenog
          SELECT SINGLE vorna, nachn FROM m_premn
            INTO ( @lv_ime, @lv_prezime )
            WHERE pernr = @gs_data-pernr.
          CONCATENATE lv_ime lv_prezime INTO gs_data-ime SEPARATED BY space.

          " naziv lokacije
*          SELECT SINGLE ktext FROM t499s INTO gs_data-naz_lokac
*            WHERE werks = ls_anlav-werks
*              AND stand = gs_data-stort.

          " naziv mesta troska
          SELECT SINGLE ktext FROM cskt INTO gs_data-naz_mesta
            WHERE spras = sy-langu
              AND kostl = gs_data-kostl.

          " adresa mesta troska
          SELECT SINGLE stras FROM csks INTO gs_data-stras
            WHERE kostl = gs_data-kostl.

          " opis prostorije
*          SELECT SINGLE opis FROM zfi_aa_soba_os
*            INTO gs_data-naz_pros
*            WHERE raumn = gs_data-raumn.

          APPEND gs_data TO gt_data.
          CLEAR: gs_data, lv_ime, lv_prezime.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA: lv_count TYPE i VALUE 1.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    <fs_data>-rbr = lv_count.
    ADD 1 TO lv_count.
  ENDLOOP.

  IF go_cont IS INITIAL.

    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT go_grd
      EXPORTING
        i_parent = go_cont.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_AA_POPISNA_LISTA'
      CHANGING
        ct_fieldcat      = gt_fld[].
    IF sy-subrc <> 0.
    ENDIF.

    PERFORM make_fld.

    DATA: ls_variant TYPE disvariant.

    ls_variant-report = sy-repid.
    ls_variant-variant = lv_variant.

    CALL METHOD go_grd->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = gt_data
        it_fieldcatalog               = gt_fld
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ELSE.

    CALL METHOD go_grd->refresh_table_display
*      EXPORTING
*        is_stable      =
*        i_soft_refresh =
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

  ENDIF.
ENDMODULE.
