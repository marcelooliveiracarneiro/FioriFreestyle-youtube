class ZCL_ZSHP_GW_VOO_DPC_EXT definition
  public
  inheriting from ZCL_ZSHP_GW_VOO_DPC
  create public .

public section.
protected section.

  methods SCARRSET_CREATE_ENTITY
    redefinition .
  methods SCARRSET_DELETE_ENTITY
    redefinition .
  methods SCARRSET_GET_ENTITY
    redefinition .
  methods SCARRSET_GET_ENTITYSET
    redefinition .
  methods SCARRSET_UPDATE_ENTITY
    redefinition .
  methods SPFLISET_CREATE_ENTITY
    redefinition .
  methods SPFLISET_GET_ENTITY
    redefinition .
  methods SPFLISET_GET_ENTITYSET
    redefinition .
  methods SPFLISET_UPDATE_ENTITY
    redefinition .
  methods SPFLISET_DELETE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSHP_GW_VOO_DPC_EXT IMPLEMENTATION.


  method SCARRSET_CREATE_ENTITY.

    DATA: ls_entity LIKE er_entity.

    TRY.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_entity
        ).

      CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

    ENDTRY.

    INSERT scarr FROM ls_entity.

    IF sy-subrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '1'                 " Message Class
        iv_msg_number             = '1'                 " Message Number
        iv_msg_v1                 = 'Erro ao cadastrar'                 " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ELSE.

      er_entity = ls_entity.

    ENDIF.

  endmethod.


  method SCARRSET_DELETE_ENTITY.

    DATA: lv_carrid  TYPE scarr-carrid,
          lv_sysubrc TYPE sy-subrc.


    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.

    DELETE FROM scarr
    WHERE carrid EQ lv_carrid.
    lv_sysubrc = sy-subrc.

    IF lv_sysubrc EQ '0'.

      DELETE FROM spfli
      WHERE carrid EQ lv_carrid.
      lv_sysubrc = sy-subrc.

      IF lv_sysubrc EQ '0'.

        DELETE FROM sflight
        WHERE carrid EQ lv_carrid.
        lv_sysubrc = sy-subrc.

      ENDIF.

    ENDIF.

    IF lv_sysubrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '3'                 " Message Class
        iv_msg_number             = '3'                 " Message Number
        iv_msg_v1                 = 'Erro ao excluir SCARR' " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ENDIF.

  endmethod.


  method SCARRSET_GET_ENTITY.

    DATA: lv_carrid TYPE scarr-carrid.

    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.


    SELECT SINGLE * FROM scarr
      INTO @er_entity
      WHERE carrid EQ @lv_carrid.

  endmethod.


  method SCARRSET_GET_ENTITYSET.

    DATA: lr_carrid   TYPE RANGE OF scarr-carrid,
          lr_carrname TYPE RANGE OF scarr-carrname,
          lr_currcode TYPE RANGE OF scarr-currcode.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).

      CASE ls_filters-property.
        WHEN 'Carrid'.
          lr_carrid = VALUE #( FOR carrid IN ls_filters-select_options ( CORRESPONDING #( carrid ) ) ).

        WHEN 'Currname'.
          lr_carrname = VALUE #( FOR carrname IN ls_filters-select_options ( CORRESPONDING #( carrname ) ) ).

        WHEN 'Currcode'.
          lr_currcode = VALUE #( FOR currcode IN ls_filters-select_options ( CORRESPONDING #( currcode ) ) ).

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    SELECT * FROM scarr
      INTO TABLE @et_entityset
      WHERE carrid IN @lr_carrid
      AND currcode IN @lr_currcode.

*    /iwbep/cl_mgw_data_util=>filtering(
*      EXPORTING
*        it_select_options = it_filter_select_options
*      CHANGING
*        ct_data           = et_entityset
*    ).
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING
        is_paging = is_paging                 " paging structure
      CHANGING
        ct_data   = et_entityset
    ).
    /iwbep/cl_mgw_data_util=>orderby(
      EXPORTING
        it_order = it_order                 " the sorting order
      CHANGING
        ct_data  = et_entityset
    ).

  endmethod.


  method SCARRSET_UPDATE_ENTITY.

    DATA: lv_carrid TYPE scarr-carrid,
          ls_entity LIKE er_entity.

    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.

    TRY.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_entity
        ).

      CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

    ENDTRY.

    UPDATE scarr
    SET carrname = ls_entity-carrname
        currcode = ls_entity-currcode
        url      = ls_entity-url
    WHERE carrid EQ lv_carrid.

    IF sy-subrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '2'                 " Message Class
        iv_msg_number             = '2'                 " Message Number
        iv_msg_v1                 = 'Erro ao atualizar' " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ELSE.

      er_entity = ls_entity.

    ENDIF.

  endmethod.


  method SPFLISET_CREATE_ENTITY.

    DATA: ls_entity LIKE er_entity,
          ls_spfli  TYPE spfli.

    TRY.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_entity
        ).

      CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

    ENDTRY.

    MOVE-CORRESPONDING ls_entity TO ls_spfli.

    INSERT spfli FROM ls_spfli.

    IF sy-subrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '1'                 " Message Class
        iv_msg_number             = '1'                 " Message Number
        iv_msg_v1                 = 'Erro ao cadastrar' " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ELSE.

      er_entity = ls_entity.

    ENDIF.

  endmethod.


  method SPFLISET_DELETE_ENTITY.

    DATA: lv_carrid  TYPE spfli-carrid,
          lv_connid  TYPE spfli-connid,
          lv_sysubrc TYPE sy-subrc.

    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
    IF line_exists( it_key_tab[ name = 'Connid' ] ).
      lv_connid = it_key_tab[ name = 'Connid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.

    DELETE FROM  spfli
    WHERE carrid EQ lv_carrid
    AND connid EQ lv_connid.
    lv_sysubrc = sy-subrc.

    IF lv_sysubrc EQ 0.

      DELETE FROM sflight
      WHERE carrid EQ lv_carrid
      AND connid EQ lv_connid.
      lv_sysubrc = sy-subrc.

    ENDIF.

    IF lv_sysubrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '2'                 " Message Class
        iv_msg_number             = '2'                 " Message Number
        iv_msg_v1                 = 'Erro ao atualizar' " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ENDIF.

  endmethod.


  method SPFLISET_GET_ENTITY.

    DATA: lv_carrid TYPE spfli-carrid,
          lv_connid TYPE spfli-connid,
          ls_spfli  TYPE spfli.

    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
    IF line_exists( it_key_tab[ name = 'Connid' ] ).
      lv_connid = it_key_tab[ name = 'Connid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.

    SELECT SINGLE * FROM spfli
      INTO @ls_spfli
      WHERE carrid EQ @lv_carrid
      AND connid EQ @lv_connid.

    MOVE-CORRESPONDING ls_spfli TO er_entity.


  endmethod.


  method SPFLISET_GET_ENTITYSET.

    DATA: lr_carrid TYPE RANGE OF spfli-carrid,
          lr_connid TYPE RANGE OF spfli-connid.

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<fs_filter>).

      CASE <fs_filter>-property.
        WHEN 'Carrid'.
          lr_carrid = VALUE #( FOR carrid IN <fs_filter>-select_options ( CORRESPONDING #( carrid ) ) ).

        WHEN 'Connid'.
          lr_connid = VALUE #( FOR connid IN <fs_filter>-select_options ( CORRESPONDING #( connid ) ) ).

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    SELECT * FROM spfli
      INTO TABLE @DATA(lt_spfli)
      WHERE carrid IN @lr_carrid
        AND connid IN @lr_connid.

    MOVE-CORRESPONDING lt_spfli TO et_entityset.

    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset
    ).
    /iwbep/cl_mgw_data_util=>orderby(
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset
    ).


  endmethod.


  method SPFLISET_UPDATE_ENTITY.

    DATA: lv_carrid TYPE sflight-carrid,
          lv_connid TYPE sflight-connid,
          ls_entity LIKE er_entity.

    IF line_exists( it_key_tab[ name = 'Carrid' ] ).
      lv_carrid = it_key_tab[ name = 'Carrid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
    IF line_exists( it_key_tab[ name = 'Connid' ] ).
      lv_connid = it_key_tab[ name = 'Connid' ]-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.

    TRY.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_entity
        ).
      CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

    ENDTRY.

    UPDATE spfli
    SET countryfr = ls_entity-countryfr
        cityfrom  = ls_entity-cityfrom
        airpfrom  = ls_entity-airpfrom
        countryto = ls_entity-countryto
        cityto    = ls_entity-cityto
        airpto    = ls_entity-airpto
        fltime    = ls_entity-fltime
        deptime   = ls_entity-deptime
        arrtime   = ls_entity-arrtime
        distance  = ls_entity-distance
        distid    = ls_entity-distid
        fltype    = ls_entity-fltype
        period    = ls_entity-period
    WHERE carrid EQ lv_carrid
      AND connid EQ lv_connid.

    IF sy-subrc NE 0.

      me->mo_context->get_message_container( )->add_message(
        iv_msg_type               = 'E'                 " Message Type
        iv_msg_id                 = '2'                 " Message Class
        iv_msg_number             = '2'                 " Message Number
        iv_msg_v1                 = 'Erro ao atualizar' " Message Variable
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = me->mo_context->get_message_container( ).

    ELSE.

      er_entity = ls_entity.

    ENDIF.

  endmethod.
ENDCLASS.
