CLASS zcl_zshp_gw_ov_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zshp_gw_ov_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_ov_deepentity,
        ordemid     TYPE i,
        datacriacao TYPE timestamp,
        criadopor   TYPE c LENGTH 20,
        clienteid   TYPE i,
        totalitens  TYPE p LENGTH 8 DECIMALS 2,
        totalfrete  TYPE p LENGTH 8 DECIMALS 2,
        totalordem  TYPE p LENGTH 8 DECIMALS 2,
        status      TYPE c LENGTH 1,
        toOVItem    TYPE TABLE OF ts_ovitem WITH DEFAULT KEY,
      END OF ty_ov_deepentity .

    METHODS define
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZSHP_GW_OV_MPC_EXT IMPLEMENTATION.


  METHOD define.

    DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    super->define( ).

    lo_entity_type = model->get_entity_type( iv_entity_name = 'OVHeader' ).
    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZSHP_GW_OV_MPC_EXT=>TY_OV_DEEPENTITY' ).

  ENDMETHOD.
ENDCLASS.
