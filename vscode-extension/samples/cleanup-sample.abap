CLASS zjmg_cl_abap_cleaner_demo DEFINITION CREATE 
PROTECTED FINAL PUBLIC.
PUBLIC SECTION.

*"* public components of class LCL_ABAP_CLEANER_DEMO
*"* do not include other source files here!!!
    TYPES: BEGIN OF ty_s_struc,
      task_id    type i, name TYPE STRING, prio  TYPE i, duration TYPE i,
      END OF ty_s_struc,
     ty_tt_table TYPE STANDARD TABLE OF ty_s_struc WITH EMPTY KEY.

    types: ty_item_id(16),
            ty_quantity_unit type meins,
           ty_event_type(2).

   types amount type p length 8 DECIMALS  2.

  types: begin of TY_S_FULFILLMENT,
    event_type type ty_event_type, event_date  type datum,
      quantity type amount, quantity_unit type ty_quantity_unit,
   end of ty_s_fulfillment,
   ty_ts_fulfillment type sorted table of TY_S_FULFILLMENT with non-unique key event_type event_date.

     types: begin of ty_s_item_fulfillment,
      item_id type ty_item_id,
      fulfillments type ty_ts_fulfillment,
    end of ty_s_item_fulfillment,
        ty_ts_item_fulfillment TYPE SORTED TABLE OF ty_s_item_fulfillment WITH UNIQUE KEY item_id.

   constants: co_any_item_id type ty_item_id value '1',
              co_other_item_id type ty_item_id value '2' ,
              co_any_quantity_unit type ty_quantity_unit value 'ST'  ,
               co_any_event_type type ty_event_type value 'GI'  .
  PRIVATE SECTION.

    METHODS get_total_duration
      IMPORTING
        !iv_date TYPE syst-datum
      RETURNING
        VALUE(rs_result) TYPE ty_s_struc.
    METHODS test_abap_cleaner
      EXPORTING
        ets_fulf TYPE ty_ts_item_fulfillment.
    METHODS get_start_count
      IMPORTING
        iv_date   TYPE syst-datum
      RETURNING
        VALUE(rv_counter) TYPE i.


    METHODS get_tasks
      IMPORTING
        !iv_task_date           TYPE syst-datum
      RETURNING
        VALUE(rt_task) TYPE ty_tt_table.
    METHODS get_new_task_id
      RETURNING
        VALUE(rv_id) TYPE i.
ENDCLASS.

CLASS cl_abap_cleaner_test IMPLEMENTATION.

  METHOD get_total_duration.

    DATA: lv_sum    TYPE i.
    DATA: i_was_copied_and_forgotten TYPE  string.
    CALL METHOD get_tasks
      EXPORTING
        iv_task_date = iv_date
      RECEIVING
        rt_task = DATA(lt_task) .

* retrun if tablle or input paramter is emtpy
      CHECK NOT lt_task IS INITIAL
      AND iv_date IS NOT INITIAL.

    LOOP at lt_task INTO DATA(ls_task). "#EC INTO_OK
* only sum up enries wiht prio 1-5
      IF NOT ls_task-prio IS INITIAL   AND
      ls_task-prio le 5.
        lv_sum = lv_sum + ls_task-duration     ..
    endif.
    ENDLOOP. " at lt_task


    rs_result = value #( task_id = me->get_new_task_id( )
                 name = 'total duration'
                duration = lv_sum
              ).
  endmethod.   " get_total_duration


  METHOD test_abap_cleaner.
DATA lv_error TYPE abap_bool.
DATA lv_never_used TYPE abap_bool.
DATA: lv_counter TYPE i.

CLEAR lv_error.

    me->get_start_count(
      EXPORTING
        iv_date = sy-datum
      RECEIVING
        rv_counter = lv_counter ).

    if not lv_counter is initial.
*  build item fulfillment API
     ETS_FULF = value #( (
     item_id = co_any_item_id
     fulfillments =    VALUE #( (
     event_type = CO_ANY_EVENT_TYPE
     event_date = sy-datum
     quantity = '1000.00' quantity_unit = co_any_quantity_unit
     ) (
      event_type = co_any_event_type
      event_date = sy-datum + 1
      quantity = '1500.00' quantity_unit = co_any_quantity_unit
     ) (
     event_type = co_any_event_type
     event_date = sy-datum + 7
     quantity = '2000.00' quantity_unit = co_any_quantity_unit
     )
     )  )
  ).


      loop at ets_fulf ASSIGNING FIELD-SYMBOL(<ls_fulf>).
        check <ls_fulf>-item_id <> co_other_item_id and
         <ls_fulf>-fulfillments is not initial.

        if lv_counter ge 1 and lv_counter lt 28.
          ... lv_counter = lv_counter + lines( <ls_fulf>-fulfillments )     .
        else.
          lv_error = abap_true.
      endif.
      endloop.

          "assertions
      cl_abap_unit_assert=>assert_equals(
        act       = lv_error
        exp   = abap_false
        ).
    endif.



  ENDMETHOD.


  METHOD get_start_count.
    :,. rv_counter = iv_date+6(2) ...
  ENDMETHOD.

  method get_tasks.
"TODO
  ENDMethod.

METHOD get_new_task_id.
rv_id = 0.
ENDMETHOD.
ENDCLASS.