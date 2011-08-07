-type valid_func_types() ::
        function_call | mfa_call | list_comp | list_loop | binary_comp | binary_loop.
-type valid_operator_types() ::
        operator_plus | operator_minus | operator_times | operator_divide | operator_rem.

-type valid_exec_types() :: valid_func_types() | valid_operator_types().

-type valid_access_types() ::
        list_nth | list_head | binary_raw | binary_at | tuple_inx.

-type valid_message_types() ::
        send_msgs | recv_msgs | router | concentrator | serial.
