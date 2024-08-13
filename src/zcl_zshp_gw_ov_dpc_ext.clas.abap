class ZCL_ZSHP_GW_OV_DPC_EXT definition
  public
  inheriting from ZCL_ZSHP_GW_OV_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.

  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods OVHEADERSET_CREATE_ENTITY
    redefinition .
  methods OVHEADERSET_DELETE_ENTITY
    redefinition .
  methods OVHEADERSET_GET_ENTITY
    redefinition .
  methods OVHEADERSET_GET_ENTITYSET
    redefinition .
  methods OVHEADERSET_UPDATE_ENTITY
    redefinition .
  methods OVITEMSET_CREATE_ENTITY
    redefinition .
  methods OVITEMSET_DELETE_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITYSET
    redefinition .
  methods OVITEMSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSHP_GW_OV_DPC_EXT IMPLEMENTATION.


 method MENSAGEMSET_CREATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_CREATE_ENTITY'.

  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_DELETE_ENTITY'.

  endmethod.


  method MENSAGEMSET_GET_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_GET_ENTITY'.

  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_GET_ENTITYSET'.

  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_UPDATE_ENTITY'.

  endmethod.


  METHOD ovheaderset_create_entity.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVHEADERSET_CREATE_ENTITY'.

DATA: ld_lastid TYPE int4.
  DATA: ls_cab    TYPE zovheader.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  MOVE-CORRESPONDING er_entity TO ls_cab.

  ls_cab-criacao_data    = sy-datum.
  ls_cab-criacao_hora    = sy-uzeit.
  ls_cab-criacao_usuario = sy-uname.

  SELECT SINGLE MAX( ordemid )
    INTO ld_lastid
    FROM zovheader.

  ls_cab-ordemid = ld_lastid + 1.
  INSERT zovheader FROM ls_cab.
  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao inserir ordem'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  " atualizando
  MOVE-CORRESPONDING ls_cab TO er_entity.

  CONVERT
    DATE ls_cab-criacao_data
    TIME ls_cab-criacao_hora
    INTO TIME STAMP er_entity-datacriacao
    TIME ZONE 'UTC'. "sy-zonlo.

  ENDMETHOD.


  method OVHEADERSET_DELETE_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVHEADERSET_DELETE_ENTITY'.

  DATA: ls_key_tab LIKE LINE OF it_key_tab.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'OrdemId não informado'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  DELETE FROM zovitem WHERE ordemid = ls_key_tab-value.
  " Vinicius 28/04/2024 - Comentando validação pois algumas ordens podem
  " não tem itens
*  IF sy-subrc <> 0.
*    ROLLBACK WORK.
*
*    lo_msg->add_message_text_only(
*      EXPORTING
*        iv_msg_type = 'E'
*        iv_msg_text = 'Erro ao remover itens'
*    ).
*
*    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
*      EXPORTING
*        message_container = lo_msg.
*  ENDIF.

  DELETE FROM zovheader WHERE ordemid = ls_key_tab-value.
  IF sy-subrc <> 0.
    ROLLBACK WORK.

    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao remover ordem'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  COMMIT WORK AND WAIT.


  endmethod.


  method OVHEADERSET_GET_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVHEADERSET_GET_ENTITY'.

DATA: ld_ordemid TYPE zovheader-ordemid.
  DATA: ls_key_tab LIKE LINE OF it_key_tab.
  DATA: ls_cab     TYPE zovheader.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  " input
  READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Id da ordem não informado'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.
  ld_ordemid = ls_key_tab-value.

  SELECT SINGLE *
    INTO ls_cab
    FROM zovheader
   WHERE ordemid = ld_ordemid.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING ls_cab TO er_entity.

    er_entity-criadopor = ls_cab-criacao_usuario.

    CONVERT DATE ls_cab-criacao_data
            TIME ls_cab-criacao_hora
       INTO TIME STAMP er_entity-datacriacao
       TIME ZONE 'UTC'. "sy-zonlo.
  ELSE.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Id da ordem não encontrado'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  endmethod.


  METHOD ovheaderset_get_entityset.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVHEADERSET_GET_ENTITYSET'.

    DATA: lt_cab       TYPE STANDARD TABLE OF zovheader.
    DATA: ls_cab       TYPE zovheader.
    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lr_ordemid   TYPE RANGE OF zovheader-ordemid,
          lr_clienteid TYPE RANGE OF zovheader-clienteid.

    DATA: lt_orderby   TYPE STANDARD TABLE OF string.
    DATA: ld_orderby   TYPE string.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).

      CASE ls_filters-property.
        WHEN 'OrdemId'.
          lr_ordemid = VALUE #( FOR ordemid IN ls_filters-select_options ( CORRESPONDING #( ordemid ) ) ).

        WHEN 'ClienteId'.
          lr_clienteid = VALUE #( FOR clienteid IN ls_filters-select_options ( CORRESPONDING #( clienteid ) ) ).

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.


    " montando orderby dinâmico
    LOOP AT it_order INTO DATA(ls_order).
      TRANSLATE ls_order-property TO UPPER CASE.
      TRANSLATE ls_order-order TO UPPER CASE.
      IF ls_order-order = 'DESC'.
        ls_order-order = 'DESCENDING'.
      ELSE.
        ls_order-order = 'ASCENDING'.
      ENDIF.
      APPEND |{ ls_order-property } { ls_order-order }|
          TO lt_orderby.
    ENDLOOP.
    CONCATENATE LINES OF lt_orderby INTO ld_orderby SEPARATED BY ', '. " Ajuste 05/06/2024

    " ordenação obrigatória caso nenhuma seja definida
    IF ld_orderby = '' .
      ld_orderby = 'OrdemId ASCENDING'.
    ENDIF.

    SELECT *
      FROM zovheader
     WHERE (iv_filter_string)
     "WHERE ordemid IN @lr_ordemid
     "  AND clienteid IN @lr_clienteid
  ORDER BY (ld_orderby)
      INTO TABLE @lt_cab
     UP TO @is_paging-top ROWS
    OFFSET @is_paging-skip.

    LOOP AT lt_cab INTO ls_cab.
      CLEAR ls_entityset.
      MOVE-CORRESPONDING ls_cab TO ls_entityset.

      ls_entityset-criadopor = ls_cab-criacao_usuario.

      CONVERT DATE ls_cab-criacao_data
              TIME ls_cab-criacao_hora
         INTO TIME STAMP ls_entityset-datacriacao
         TIME ZONE sy-zonlo.

      APPEND ls_entityset TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  method OVHEADERSET_UPDATE_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVHEADERSET_UPDATE_ENTITY'.

DATA: ld_error TYPE flag.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  er_entity-ordemid = it_key_tab[ name = 'OrdemId' ]-value.

  " validações
  IF er_entity-clienteid = 0.
    ld_error = 'X'.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Cliente vazio'
    ).
  ENDIF.

  IF er_entity-totalordem < 10.
    ld_error = 'X'.
    lo_msg->add_message(
      EXPORTING
        iv_msg_type   = 'E'
        iv_msg_id     = 'ZOV'
        iv_msg_number = 1
        iv_msg_v1     = 'R$ 10,00'
        iv_msg_v2     = |{ er_entity-ordemid }|
    ).
  ENDIF.

  IF ld_error = 'X'.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg
        http_status_code  = 500.
  ENDIF.

  UPDATE zovheader
     SET clienteid  = er_entity-clienteid
         totalitens = er_entity-totalitens
         totalfrete = er_entity-totalfrete
         totalordem = er_entity-totalordem
         status     = er_entity-status
   WHERE ordemid    = er_entity-ordemid.

  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao atualizar ordem'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  endmethod.


  METHOD ovitemset_create_entity.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVITEMSET_CREATE_ENTITY'.

    DATA: ls_item TYPE zovitem.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM zovitem
       WHERE ordemid = er_entity-ordemid.

      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    INSERT zovitem FROM ls_item.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  method OVITEMSET_DELETE_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVITEMSET_DELETE_ENTITY'.

DATA: ls_item    TYPE zovitem.
  DATA: ls_key_tab LIKE LINE OF it_key_tab.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  ls_item-ordemid = it_key_tab[ name = 'OrdemId' ]-value.
  ls_item-itemid  = it_key_tab[ name = 'ItemId' ]-value.

  DELETE FROM zovitem
   WHERE ordemid = ls_item-ordemid
     AND itemid  = ls_item-itemid.
  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao remover item'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  endmethod.


  method OVITEMSET_GET_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVITEMSET_GET_ENTITY'.

DATA: ls_key_tab LIKE LINE OF it_key_tab.
  DATA: ls_item    TYPE zovitem.
  DATA: ld_error   TYPE flag.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  " input
  READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
  IF sy-subrc <> 0.
    ld_error = 'X'.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Id da ordem não informado'
    ).
  ENDIF.
  ls_item-ordemid = ls_key_tab-value.

  READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ItemId'.
  IF sy-subrc <> 0.
    ld_error = 'X'.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Id do item não informado'
    ).
  ENDIF.
  ls_item-itemid = ls_key_tab-value.

  IF ld_error = 'X'.
    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  SELECT SINGLE *
    INTO ls_item
    FROM zovitem
   WHERE ordemid = ls_item-ordemid
     AND itemid  = ls_item-itemid.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING ls_item TO er_entity.
  ELSE.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Item não encontrado'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  endmethod.


  method OVITEMSET_GET_ENTITYSET.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVITEMSET_GET_ENTITYSET'.

DATA: ld_ordemid       TYPE int4.
  DATA: lt_ordemid_range TYPE RANGE OF int4.
  DATA: ls_ordemid_range LIKE LINE OF lt_ordemid_range.
  DATA: ls_key_tab       LIKE LINE OF it_key_tab.

  " input
  READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
  IF sy-subrc = 0.
    ld_ordemid = ls_key_tab-value.

    CLEAR ls_ordemid_range.
    ls_ordemid_range-sign   = 'I'.
    ls_ordemid_range-option = 'EQ'.
    ls_ordemid_range-low    = ld_ordemid.
    APPEND ls_ordemid_range TO lt_ordemid_range.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_entityset
    FROM zovitem
   WHERE ordemid IN lt_ordemid_range.

  endmethod.


  method OVITEMSET_UPDATE_ENTITY.

*  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
*    EXPORTING
*      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
*      method = 'OVITEMSET_UPDATE_ENTITY'.

DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  er_entity-ordemid  = it_key_tab[ name = 'OrdemId' ]-value.
  er_entity-itemid   = it_key_tab[ name = 'ItemId' ]-value.
  er_entity-precotot = er_entity-quantidade * er_entity-precouni.

  UPDATE zovitem
     SET material   = er_entity-material
         descricao  = er_entity-descricao
         quantidade = er_entity-quantidade
         precouni   = er_entity-precouni
         precotot   = er_entity-precotot
   WHERE ordemid    = er_entity-ordemid
     AND itemid     = er_entity-itemid.

  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao atualizar item'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  endmethod.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA : ls_deep_entity  TYPE ZCL_ZSHP_GW_OV_mpc_ext=>ty_ov_deepentity.
    DATA : ls_deep_item    TYPE ZCL_ZSHP_GW_OV_mpc_ext=>ts_ovitem.

    DATA : ls_cab          TYPE zovheader.
    DATA : lt_item         TYPE STANDARD TABLE OF zovitem.
    DATA : ls_item         TYPE zovitem.
    DATA : ld_updkz        TYPE char1.
    DATA : ld_datahora(14) TYPE c.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    CALL METHOD io_data_provider->read_entry_data
      IMPORTING
        es_data = ls_deep_entity.

    " cabeçalho
    IF ls_deep_entity-ordemid = 0.
      ld_updkz = 'I'.

      MOVE-CORRESPONDING ls_deep_entity TO ls_cab.

      "ls_cab-criacao_data    = sy-datum.
      "ls_cab-criacao_hora    = sy-uzeit.
      "ls_cab-criacao_usuario = sy-uname.

      ld_datahora            = ls_deep_entity-datacriacao.
      ls_cab-criacao_data    = ld_datahora(8).
      ls_cab-criacao_hora    = ld_datahora+8(6).
      ls_cab-criacao_usuario = ls_deep_entity-criadopor.

      SELECT SINGLE MAX( ordemid )
        INTO ls_cab-ordemid
        FROM zovheader.

      ls_cab-ordemid = ls_cab-ordemid + 1.
    ELSE.
      ld_updkz = 'U'.

      " carregando dados atuais
      SELECT SINGLE *
        INTO ls_cab
        FROM zovheader
       WHERE ordemid = ls_deep_entity-ordemid.

      ls_cab-clienteid  = ls_deep_entity-clienteid.
      ls_cab-status     = ls_deep_entity-status.
      ls_cab-totalitens = ls_deep_entity-totalitens.
      ls_cab-totalfrete = ls_deep_entity-totalfrete.
      ls_cab-totalordem = ls_cab-totalitens + ls_cab-totalfrete.
    ENDIF.

    " item
    LOOP AT ls_deep_entity-toovitem INTO ls_deep_item.
      MOVE-CORRESPONDING ls_deep_item TO ls_item.

      ls_item-ordemid = ls_cab-ordemid.
      APPEND ls_item TO lt_item.
    ENDLOOP.

    " persistência cabeçalho
    IF ld_updkz = 'I'.
      INSERT zovheader FROM ls_cab.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao inserir ordem'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ELSE.
      MODIFY zovheader FROM ls_cab.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao atualizar ordem'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    " persistência itens
    DELETE FROM zovitem WHERE ordemid = ls_cab-ordemid.
    IF lines( lt_item ) > 0.
      INSERT zovitem FROM TABLE lt_item.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao inserir itens'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    COMMIT WORK AND WAIT.

    " atualizando deep entity de retorno

    " cabeçalho
    ls_deep_entity-ordemid = ls_cab-ordemid.
    CONVERT DATE ls_cab-criacao_data
            TIME ls_cab-criacao_hora
            INTO TIME STAMP ls_deep_entity-datacriacao
            TIME ZONE 'UTC'. "sy-zonlo.

    " item
    LOOP AT ls_deep_entity-toovitem ASSIGNING FIELD-SYMBOL(<ls_deep_item>).
      <ls_deep_item>-ordemid = ls_cab-ordemid.
    ENDLOOP.

    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = ls_deep_entity
      CHANGING
        cr_data = er_deep_entity.

  ENDMETHOD.
ENDCLASS.
