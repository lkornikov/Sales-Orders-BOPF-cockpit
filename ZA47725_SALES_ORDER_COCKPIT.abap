*&---------------------------------------------------------------------*
*& Report ZA47725_SALES_ORDER_COCKPIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT za47725_sales_order_cockpit.

*Selection screen for vendor1 and vendor2.
SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "sales orders
    PARAMETERS o_id TYPE /bobf/demo_customer_id DEFAULT '100'.
    PARAMETERS o_new RADIOBUTTON GROUP one DEFAULT 'X'. "Create
    PARAMETERS o_upd RADIOBUTTON GROUP one.             "Update
    PARAMETERS o_del RADIOBUTTON GROUP one.             "Delete
    PARAMETERS o_show RADIOBUTTON GROUP one.             "Show
  SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 101.

SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. "Customers
    PARAMETERS c_id TYPE /bobf/demo_customer_id DEFAULT '100'.
    PARAMETERS c_addr TYPE /bobf/demo_address_text DEFAULT '1234 Boardwalk Ave.'.
    PARAMETERS c_new RADIOBUTTON GROUP two DEFAULT 'X'. "Create
    PARAMETERS c_upd RADIOBUTTON GROUP two.             "Update
    PARAMETERS c_del RADIOBUTTON GROUP two.             "Delete
    PARAMETERS c_show RADIOBUTTON GROUP two.             "Show
  SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 102.

SELECTION-SCREEN BEGIN OF SCREEN 103 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003. "Products
    PARAMETERS p_id TYPE /bobf/demo_customer_id DEFAULT '100'.
    PARAMETERS p_new RADIOBUTTON GROUP thre DEFAULT 'X'. "Create
    PARAMETERS p_upd RADIOBUTTON GROUP thre.             "Update
    PARAMETERS p_del RADIOBUTTON GROUP thre.             "Delete
    PARAMETERS p_show RADIOBUTTON GROUP thre.             "Show
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 103.
*Tabstrip control
SELECTION-SCREEN BEGIN OF TABBED BLOCK tbl FOR 20 LINES.
  SELECTION-SCREEN TAB (15) name1 USER-COMMAND ucomm1 DEFAULT SCREEN 101.
  SELECTION-SCREEN TAB (17) name2 USER-COMMAND ucomm2 DEFAULT SCREEN 102.
  SELECTION-SCREEN TAB (17) name3 USER-COMMAND ucomm3 DEFAULT SCREEN 103.
SELECTION-SCREEN END OF BLOCK tbl.

PARAMETERS:
* Hidden parameters to store the last selected tab strip
  pa_dynnr LIKE tbl-dynnr NO-DISPLAY,
  pa_acttb LIKE tbl-activetab NO-DISPLAY.

CLASS lcl_demo DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id.
    CLASS-METHODS:
      display_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id.
    CLASS-METHODS:
      change_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id
                                iv_new_addr    TYPE /bobf/demo_address_text.

  PRIVATE SECTION.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_bo_conf  TYPE REF TO /bobf/if_frw_configuration.

    METHODS:
      constructor RAISING /bobf/cx_frw.

    METHODS:
      display_messages IMPORTING io_message
                                   TYPE REF TO /bobf/if_frw_message.

    METHODS:
      get_customer_for_id IMPORTING iv_customer_id         TYPE /bobf/demo_customer_id
                          RETURNING VALUE(rv_customer_key) TYPE /bobf/conf_key
                          RAISING   /bobf/cx_frw.
    METHODS:
      get_node_table IMPORTING iv_key         TYPE /bobf/conf_key
                               iv_node_key    TYPE /bobf/obm_node_key
                               iv_edit_mode   TYPE /bobf/conf_edit_mode
                                 DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                     RETURNING VALUE(rr_data) TYPE REF TO data
                     RAISING   /bobf/cx_frw,

      get_node_row IMPORTING iv_key         TYPE /bobf/conf_key
                             iv_node_key    TYPE /bobf/obm_node_key
                             iv_edit_mode   TYPE /bobf/conf_edit_mode

                               DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                             iv_index       TYPE i DEFAULT 1
                   RETURNING VALUE(rr_data) TYPE REF TO data
                   RAISING   /bobf/cx_frw,

      get_node_table_by_assoc IMPORTING iv_key         TYPE /bobf/conf_key
                                        iv_node_key    TYPE /bobf/obm_node_key
                                        iv_assoc_key   TYPE /bobf/obm_assoc_key
                                        iv_edit_mode   TYPE /bobf/conf_edit_mode
                                          DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                              RETURNING VALUE(rr_data) TYPE REF TO data
                              RAISING   /bobf/cx_frw,

      get_node_row_by_assoc IMPORTING iv_key         TYPE /bobf/conf_key
                                      iv_node_key    TYPE /bobf/obm_node_key
                                      iv_assoc_key   TYPE /bobf/obm_assoc_key
                                      iv_edit_mode   TYPE /bobf/conf_edit_mode
                                        DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                                      iv_index       TYPE i DEFAULT 1
                            RETURNING VALUE(rr_data) TYPE REF TO data
                            RAISING   /bobf/cx_frw.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD constructor.
    "Obtain a reference to the BOPF transaction manager:
    me->mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
    "Obtain a reference to the BOPF service manager:
    me->mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /bobf/if_demo_customer_c=>sc_bo_key ).
    "Access the metadata for the /BOBF/DEMO_CUSTOMER BO:
    me->mo_bo_conf = /bobf/cl_frw_factory=>get_configuration( /bobf/if_demo_customer_c=>sc_bo_key ).
  ENDMETHOD.                 " METHOD constructor

  METHOD display_messages.
    "Method-Local Data Declarations:
    DATA lt_messages TYPE /bobf/t_frw_message_k.
    DATA lv_msg_text TYPE string.
    FIELD-SYMBOLS <ls_message> LIKE LINE OF lt_messages.

    "Sanity check:
    CHECK io_message IS BOUND.

    "Output each of the messages in the collection:
    io_message->get_messages( IMPORTING et_message = lt_messages ).
    LOOP AT lt_messages ASSIGNING <ls_message>.
      lv_msg_text = <ls_message>-message->get_text( ).
      WRITE: / lv_msg_text.
    ENDLOOP.
  ENDMETHOD.                 " METHOD display_messages

  METHOD create_customer.
    "Method-Local Data Declarations:
    DATA lo_driver   TYPE REF TO lcl_demo.
    DATA lt_mod      TYPE /bobf/t_frw_modification.
    DATA lo_change   TYPE REF TO /bobf/if_tra_change.
    DATA lo_message  TYPE REF TO /bobf/if_frw_message.
    DATA lv_rejected TYPE boole_d.
    DATA lx_bopf_ex  TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg  TYPE string.
    DATA lr_s_root     TYPE REF TO /bobf/s_demo_customer_hdr_k.
    DATA lr_s_txt      TYPE REF TO /bobf/s_demo_short_text_k.
    DATA lr_s_txt_hdr  TYPE REF TO /bobf/s_demo_longtext_hdr_k.
    DATA lr_s_txt_cont TYPE REF TO /bobf/s_demo_longtext_item_k.

    FIELD-SYMBOLS: <ls_mod> LIKE LINE OF lt_mod.

    "Use the BOPF API to create a new customer record:
    TRY.
        "Instantiate the driver class:
        CREATE OBJECT lo_driver.

        "Build the ROOT node:
        CREATE DATA lr_s_root.
        lr_s_root->key = /bobf/cl_frw_factory=>get_new_key( ).
        lr_s_root->customer_id    = iv_customer_id.
        lr_s_root->sales_org      = 'AMER'.
        lr_s_root->cust_curr      = 'USD'.
        lr_s_root->address_contry = 'US'.
        lr_s_root->address        = '1234 Any Street'.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-key         = lr_s_root->key.
        <ls_mod>-data        = lr_s_root.

        "Build the ROOT_TEXT node:
        CREATE DATA lr_s_txt.
        lr_s_txt->key      = /bobf/cl_frw_factory=>get_new_key( ).
        lr_s_txt->text     = 'Sample Customer Record'.
        lr_s_txt->language = sy-langu.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root_text.
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root.
        <ls_mod>-association = /bobf/if_demo_customer_c=>sc_association-root-root_text.
        <ls_mod>-source_key  = lr_s_root->key.
        <ls_mod>-key         = lr_s_txt->key.
        <ls_mod>-data        = lr_s_txt.

        "Build the ROOT_LONG_TEXT node:
        "If you look at the node type for this node, you'll notice that
        "it's a "Delegated Node". In other words, it is defined in terms
        "of the /BOBF/DEMO_TEXT_COLLECTION business object. The following
        "code accounts for this indirection.
        CREATE DATA lr_s_txt_hdr.
        lr_s_txt_hdr->key = /bobf/cl_frw_factory=>get_new_key( ).

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node            = /bobf/if_demo_customer_c=>sc_node-root_long_text.
        <ls_mod>-change_mode     = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-source_node     = /bobf/if_demo_customer_c=>sc_node-root.
        <ls_mod>-association     = /bobf/if_demo_customer_c=>sc_association-root-root_long_text.
        <ls_mod>-source_key      = lr_s_root->key.
        <ls_mod>-key             = lr_s_txt_hdr->key.
        <ls_mod>-data            = lr_s_txt_hdr.

        "Create the CONTENT node:
        CREATE DATA lr_s_txt_cont.
        lr_s_txt_cont->key          = /bobf/cl_frw_factory=>get_new_key( ).
        lr_s_txt_cont->language     = sy-langu.
        lr_s_txt_cont->text_type    = 'MEMO'.
        lr_s_txt_cont->text_content = 'Demo customer created via BOPF API.'.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = lo_driver->mo_bo_conf->query_node( iv_proxy_node_name = 'ROOT_LONG_TXT.CONTENT' ).
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root_long_text.
        <ls_mod>-source_key  = lr_s_txt_hdr->key.
        <ls_mod>-key         = lr_s_txt_cont->key.
        <ls_mod>-data        = lr_s_txt_cont.

        <ls_mod>-association =
          lo_driver->mo_bo_conf->query_assoc(
            iv_node_key   = /bobf/if_demo_customer_c=>sc_node-root_long_text
            iv_assoc_name = 'CONTENT' ).

        "Create the customer record:
        CALL METHOD lo_driver->mo_svc_mngr->modify
          EXPORTING
            it_modification = lt_mod
          IMPORTING
            eo_change       = lo_change
            eo_message      = lo_message.

        "Check for errors:
        IF lo_message IS BOUND.
          IF lo_message->check( ) EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
        ENDIF.

        "Apply the transactional changes:
        CALL METHOD lo_driver->mo_txn_mngr->save
          IMPORTING
            eo_message  = lo_message
            ev_rejected = lv_rejected.

        IF lv_rejected EQ abap_true.
          lo_driver->display_messages( lo_message ).
          RETURN.
        ENDIF.

        "If we get to here, then the operation was successful:
        WRITE: / 'Customer', iv_customer_id, 'created successfully.'.
      CATCH /bobf/cx_frw INTO lx_bopf_ex.
        lv_err_msg = lx_bopf_ex->get_text( ).
        WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD create_customer


  METHOD get_customer_for_id.
    "Method-Local Data Declarations:
    DATA lo_driver        TYPE REF TO lcl_demo.
    DATA lt_parameters    TYPE /bobf/t_frw_query_selparam.
    DATA lt_customer_keys TYPE /bobf/t_frw_key.
    DATA lx_bopf_ex       TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg       TYPE string.

    FIELD-SYMBOLS <ls_parameter> LIKE LINE OF lt_parameters.
    FIELD-SYMBOLS <ls_customer_key> LIKE LINE OF lt_customer_keys.

    "Instantiate the test driver class:
    CREATE OBJECT lo_driver.

    "Though we could conceivably lookup the customer using an SQL query,
    "the preferred method of selection is a BOPF query:
    APPEND INITIAL LINE TO lt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-attribute_name = /bobf/if_demo_customer_c=>sc_query_attribute-root-select_by_attributes-customer_id.
    <ls_parameter>-sign           = 'I'.
    <ls_parameter>-option         = 'EQ'.
    <ls_parameter>-low            = iv_customer_id.

    CALL METHOD lo_driver->mo_svc_mngr->query
      EXPORTING
        iv_query_key            = /bobf/if_demo_customer_c=>sc_query-root-select_by_attributes
        it_selection_parameters = lt_parameters
      IMPORTING
        et_key                  = lt_customer_keys.

    "Return the matching customer's KEY value:
    READ TABLE lt_customer_keys INDEX 1 ASSIGNING <ls_customer_key>.
    IF sy-subrc EQ 0.
      rv_customer_key = <ls_customer_key>-key.
    ENDIF.
  ENDMETHOD.                 " METHOD get_customer_for_id

  METHOD display_customer.
    "Method-Local Data Declarations:
    DATA lo_driver       TYPE REF TO lcl_demo.
    DATA lv_customer_key TYPE /bobf/conf_key.
    DATA lx_bopf_ex      TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg      TYPE string.

    DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.
    DATA lr_s_text TYPE REF TO /bobf/s_demo_short_text_k.


    "Try to display the selected customer:
    TRY.
        "Instantiate the test driver class:
        CREATE OBJECT lo_driver.

        "Lookup the customer's key attribute using a query:
        lv_customer_key = lo_driver->get_customer_for_id( iv_customer_id ).

        "Display the header-level details for the customer:
        lr_s_root ?=
          lo_driver->get_node_row(

                    iv_key = lv_customer_key
                    iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
                    iv_index = 1 ).

        WRITE: / 'Display Customer', lr_s_root->customer_id.
        ULINE.
        WRITE: / 'Sales Organization:', lr_s_root->sales_org.
        WRITE: / 'Address:', lr_s_root->address.
        SKIP.

        "Traverse to the ROOT_TEXT node to display the customer short text:
        lr_s_text ?=
          lo_driver->get_node_row_by_assoc(

            iv_key = lv_customer_key
            iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
            iv_assoc_key = /bobf/if_demo_customer_c=>sc_association-root-root_text
            iv_index = 1 ).
        WRITE: / 'Short Text:', lr_s_text->text.
      CATCH /bobf/cx_frw INTO lx_bopf_ex.
        lv_err_msg = lx_bopf_ex->get_text( ).
        WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD display_customer



  METHOD get_node_table.
    "Method-Local Data Declarations:
    DATA lt_key       TYPE /bobf/t_frw_key.
    DATA ls_node_conf TYPE /bobf/s_confro_node.
    DATA lo_change    TYPE REF TO /bobf/if_tra_change.

    DATA lo_message   TYPE REF TO /bobf/if_frw_message.



    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.



    "Lookup the node's configuration:
    CALL METHOD mo_bo_conf->get_node
      EXPORTING
        iv_node_key = iv_node_key
      IMPORTING
        es_node     = ls_node_conf.



    "Use the node configuration metadata to create the result table:
    CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN rr_data->* TO <lt_data>.



    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.



    CALL METHOD mo_svc_mngr->retrieve
      EXPORTING
        iv_node_key = iv_node_key
        it_key      = lt_key
      IMPORTING
        eo_message  = lo_message
        eo_change   = lo_change
        et_data     = <lt_data>.



    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_table



  METHOD get_node_row.
    "Method-Local Data Declarations:
    DATA lr_t_data TYPE REF TO data.



    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.



    "Lookup the node data:
    lr_t_data =
      get_node_table( iv_key       = iv_key
                      iv_node_key  = iv_node_key
                      iv_edit_mode = iv_edit_mode ).



    IF lr_t_data IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.



    "Try to pull the record at the specified index:
    ASSIGN lr_t_data->* TO <lt_data>.
    READ TABLE <lt_data> INDEX iv_index ASSIGNING <ls_row>.
    IF sy-subrc EQ 0.
      GET REFERENCE OF <ls_row> INTO rr_data.
    ELSE.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_row



  METHOD get_node_table_by_assoc.
    "Method-Local Data Declarations:
    DATA lt_key         TYPE /bobf/t_frw_key.
    DATA ls_node_conf   TYPE /bobf/s_confro_node.
    DATA ls_association TYPE /bobf/s_confro_assoc.
    DATA lo_change      TYPE REF TO /bobf/if_tra_change.
    DATA lo_message     TYPE REF TO /bobf/if_frw_message.



    FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.



    "Lookup the association metadata to find out more
    "information about the target sub-node:
    CALL METHOD mo_bo_conf->get_assoc
      EXPORTING
        iv_assoc_key = iv_assoc_key
        iv_node_key  = iv_node_key
      IMPORTING
        es_assoc     = ls_association.



    IF ls_association-target_node IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.



    "Use the node configuration metadata to create the result table:

    ls_node_conf = ls_association-target_node->*.



    CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
    ASSIGN rr_data->* TO <lt_data>.



    "Retrieve the target node:
    APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
    <ls_key>-key = iv_key.



    CALL METHOD mo_svc_mngr->retrieve_by_association
      EXPORTING
        iv_node_key    = iv_node_key
        it_key         = lt_key
        iv_association = iv_assoc_key
        iv_fill_data   = abap_true
      IMPORTING
        eo_message     = lo_message
        eo_change      = lo_change
        et_data        = <lt_data>.



    "Check the results:
    IF lo_message IS BOUND.
      IF lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_table_by_assoc

  METHOD get_node_row_by_assoc.
    "Method-Local Data Declarations:
    DATA lr_t_data TYPE REF TO data.

    FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.

    "Lookup the node data:
    lr_t_data =
      get_node_table_by_assoc( iv_key       = iv_key
                               iv_node_key  = iv_node_key
                               iv_assoc_key = iv_assoc_key
                               iv_edit_mode = iv_edit_mode ).

    IF lr_t_data IS NOT BOUND.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.

    "Try to pull the record at the specified index:
    ASSIGN lr_t_data->* TO <lt_data>.
    READ TABLE <lt_data> INDEX iv_index ASSIGNING <ls_row>.
    IF sy-subrc EQ 0.
      GET REFERENCE OF <ls_row> INTO rr_data.
    ELSE.
      RAISE EXCEPTION TYPE /bobf/cx_dac.
    ENDIF.
  ENDMETHOD.                 " METHOD get_node_row_by_assoc

  METHOD change_customer.
    "Method-Local Data Declarations:
    DATA lo_driver       TYPE REF TO lcl_demo.
    DATA lv_customer_key TYPE /bobf/conf_key.
    DATA lt_mod          TYPE /bobf/t_frw_modification.
    DATA lo_change       TYPE REF TO /bobf/if_tra_change.
    DATA lo_message      TYPE REF TO /bobf/if_frw_message.
    DATA lv_rejected     TYPE boole_d.
    DATA lx_bopf_ex      TYPE REF TO /bobf/cx_frw.
    DATA lv_err_msg      TYPE string.

    FIELD-SYMBOLS:
      <ls_mod> LIKE LINE OF lt_mod.

    DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.

    "Try to change the address on the selected customer:
    TRY.
        "Instantiate the test driver class:
        CREATE OBJECT lo_driver.

        "Access the customer ROOT node:
        lv_customer_key = lo_driver->get_customer_for_id( iv_customer_id ).

        lr_s_root ?=
          lo_driver->get_node_row( iv_key = lv_customer_key
                                   iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
                                   iv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive
                                   iv_index = 1 ).


        "Change the address string on the customer:
        lr_s_root->address = iv_new_addr.


        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_update.
        <ls_mod>-key         = lr_s_root->key.
        <ls_mod>-data        = lr_s_root.



        "Update the customer record:
        CALL METHOD lo_driver->mo_svc_mngr->modify
          EXPORTING
            it_modification = lt_mod
          IMPORTING
            eo_change       = lo_change
            eo_message      = lo_message.



        "Check for errors:
        IF lo_message IS BOUND.
          IF lo_message->check( ) EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
        ENDIF.



        "Apply the transactional changes:
        CALL METHOD lo_driver->mo_txn_mngr->save
          IMPORTING
            eo_message  = lo_message
            ev_rejected = lv_rejected.



        IF lv_rejected EQ abap_true.
          lo_driver->display_messages( lo_message ).
          RETURN.
        ENDIF.



        "If we get to here, then the operation was successful:
        WRITE: / 'Customer', iv_customer_id, 'updated successfully.'.
      CATCH /bobf/cx_frw INTO lx_bopf_ex.
        lv_err_msg = lx_bopf_ex->get_text( ).
        WRITE: / lv_err_msg.
    ENDTRY.
  ENDMETHOD.                 " METHOD change_customer

ENDCLASS.

*---------------------------------------------------------------------*
* Initialization
*---------------------------------------------------------------------
INITIALIZATION.

* To return to the previous selected tab strip
  IMPORT tbl-dynnr FROM MEMORY ID 'TabStripScreen' .
  IMPORT tbl-activetab FROM MEMORY ID 'TabStripActive'.

  name1 = 'Sales Order'.
  name2 = 'Customers'.
  name3 = 'Products'.

  PERFORM generate_new_customer_no.

AT SELECTION-SCREEN OUTPUT. " PBO
* if the last choice is saved
  IF NOT pa_dynnr IS INITIAL.
*   activate the last choice
    tbl-dynnr     = pa_dynnr.
    tbl-activetab = pa_acttb.
*   clear the saved choice to make it only once
    pa_dynnr = ''.
    pa_acttb = ''.
  ENDIF.

AT SELECTION-SCREEN. " PAI
* if the list is started (F8)
  IF sy-ucomm = 'ONLI'.
*   save the last choice
    pa_dynnr = tbl-dynnr.
    pa_acttb = tbl-activetab.
  ENDIF.

*--------------------------------------------------------------------*
START-OF-SELECTION.

* To return to the previous selected tab strip
  EXPORT tbl-dynnr TO MEMORY ID 'TabStripScreen'.
  EXPORT tbl-activetab TO MEMORY ID 'TabStripActive'.

*  WRITE: / tabstrip_tbl-activetab. " show which tabstrip is active

  CASE tabstrip_tbl-activetab.
    WHEN 'UCOMM1'.
    WHEN 'UCOMM2'.
      CASE 'X'.
        WHEN c_new.
          lcl_demo=>create_customer( c_id ).
        WHEN c_upd.
          lcl_demo=>change_customer( EXPORTING iv_customer_id = c_id
                                               iv_new_addr = c_addr ).
        WHEN c_show.
          lcl_demo=>display_customer( c_id ).
      ENDCASE.
    WHEN 'UCOMM3'.
  ENDCASE.
*&---------------------------------------------------------------------*
*& Form generate_new_customer_no
*&---------------------------------------------------------------------*
*& We need next unique number for the customer BO
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM generate_new_customer_no .

  DATA mo_bo_conf  TYPE REF TO /bobf/if_frw_configuration.
  "Access the metadata for the /BOBF/DEMO_CUSTOMER BO:
  TRY .
    mo_bo_conf = /bobf/cl_frw_factory=>get_configuration( /bobf/if_demo_customer_c=>sc_bo_key ).
  CATCH /bobf/cx_frw.
    RETURN.
  ENDTRY.


  DATA lo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
  DATA lt_params TYPE /bobf/t_frw_query_selparam.
  FIELD-SYMBOLS <ls_param> LIKE LINE OF lt_params.
  DATA lt_key TYPE /bobf/t_frw_key.
  FIELD-SYMBOLS <ls_key> LIKE LINE OF lt_key.
  DATA lt_root TYPE /bobf/t_demo_customer_hdr_k.
  DATA ls_root TYPE /bobf/s_demo_customer_hdr_k.
  DATA lv_min TYPE i.
  DATA lv_number TYPE i.

  APPEND INITIAL LINE TO lt_params ASSIGNING <ls_param>.
  <ls_param>-attribute_name = 'CUSTOMER_ID'.
  <ls_param>-sign = 'I'.
  <ls_param>-option = 'CP'.
  <ls_param>-low = '*'.

  "Obtain a reference to the BOPF service manager:
  lo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /bobf/if_demo_customer_c=>sc_bo_key ).

  lo_svc_mngr->query(
    EXPORTING
      iv_query_key = /bobf/if_demo_customer_c=>sc_query-root-select_by_elements
      it_selection_parameters = lt_params
      iv_fill_data = abap_true
    IMPORTING
      et_data = lt_root
      et_key = lt_key ).

  SORT lt_root BY customer_id.


  lv_min = 1.
  LOOP AT lt_root INTO ls_root.
    TRY.
        lv_number = ls_root-customer_id.
      CATCH cx_sy_conversion_no_number.
        CONTINUE. "Non numeric number, skip this customer
    ENDTRY.

    IF lv_number <> lv_min.
      " This number lv_min is available. We can create customer this number.
      EXIT.
    ELSE.
      " Try next number
      lv_min = lv_min + 1.
    ENDIF.
  ENDLOOP.

  c_id = lv_min.
  CONDENSE c_id.

ENDFORM.