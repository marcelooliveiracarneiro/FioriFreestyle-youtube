class ZCL_ZSHP_GW_OV_DPC_EXT definition
  public
  inheriting from ZCL_ZSHP_GW_OV_DPC
  create public .

public section.
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
  methods OVITEMSET_DELETE_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITYSET
    redefinition .
  methods OVITEMSET_UPDATE_ENTITY
    redefinition .
  methods OVITEMSET_CREATE_ENTITY
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


  method OVHEADERSET_CREATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVHEADERSET_CREATE_ENTITY'.

  endmethod.


  method OVHEADERSET_DELETE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVHEADERSET_DELETE_ENTITY'.

  endmethod.


  method OVHEADERSET_GET_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVHEADERSET_GET_ENTITY'.

  endmethod.


  method OVHEADERSET_GET_ENTITYSET.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVHEADERSET_GET_ENTITYSET'.

  endmethod.


  method OVHEADERSET_UPDATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVHEADERSET_UPDATE_ENTITY'.

  endmethod.


  method OVITEMSET_CREATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVITEMSET_CREATE_ENTITY'.

  endmethod.


  method OVITEMSET_DELETE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVITEMSET_DELETE_ENTITY'.

  endmethod.


  method OVITEMSET_GET_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVITEMSET_GET_ENTITY'.

  endmethod.


  method OVITEMSET_GET_ENTITYSET.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVITEMSET_GET_ENTITYSET'.

  endmethod.


  method OVITEMSET_UPDATE_ENTITY.

  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'OVITEMSET_UPDATE_ENTITY'.

  endmethod.
ENDCLASS.
