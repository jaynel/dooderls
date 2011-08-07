var menu_opts = {
  data_access: 
    '<option value="list_nth">List Nth</option>\n \
     <option value="list_head">List Head</option>\n \
     <option value="binary_raw">Native Binary</option>\n \
     <option value="binary_at">Binary Module</option>\n \
     <option value="tuple_inx">Tuple Index</option>',
  operator:
    '<option value="operator_plus">Operator +</option>\n \
     <option value="operator_minus">Operator -</option>\n \
     <option value="operator_times">Operator *</option>\n \
     <option value="operator_divide">Operator div</option>\n \
     <option value="operator_rem">Operator rem</option>\n \
     <option value="operator_gt">Operator &gt;</option>\n \
     <option value="operator_lt">Operator &lt;</option>\n \
     <option value="operator_eq">Operator ==</option>\n \
     <option value="operator_eeq">Operator =:=</option>',
  iterate:
    '<option value="function_call">Function Call</option>\n \
     <option value="mfa_call">Module:Function(Args)</option>\n \
     <option value="list_comp" disabled>List Comprehension</option>\n \
     <option value="list_loop" disabled>List Function Loop</option>\n \
     <option value="binary_comp" disabled>Binary Comprehension</option>\n \
     <option value="binary_loop" disabled>Binary Function Loop</option>',
  process:
    '<option value="spawn" disabled>Spawn</option>\n \
     <option value="spawn_link" disabled>Spawn Link</option>\n \
     <option value="exit" disabled>Exit</option>\n \
     <option value="link" disabled>Link / Unlink</option>\n \
     <option value="monitor" disabled>Monitor / Demonitor</option>\n \
     <option value="spawn_expire" disabled>Spawn Expire</option>\n \
     <option value="timers" disabled>Launch Timers</option>\n \
     <option value="sup_child" disabled>Supervisor Start Child</option>',
  message:
    '<option value="send_msgs">Send Messages</option>\n \
     <option value="recv_msgs">Receive Messages</option>\n \
     <option value="router" disabled>N-way Router</option>\n \
     <option value="concentrator" disabled>N-way Concentrator</option>\n \
     <option value="serial" disabled>Serial Pipeline</option>',
  data_manip:
    '<option value="lists_mapfoldl" disabled>lists:mapfoldl</option>\n \
     <option value="lists_mapfoldl" disabled>lists:mapfoldr</option>\n \
     <option value="lists_sort" disabled>lists:sort</option>\n \
     <option value="lists_zip" disabled>lists:zip</option>'
  }

  function change_bench_menu(bench_type) {
    var form = document.benchmark_request;
    var bench = form.bench;
    bench.options.length = 0;
    bench.innerHTML = menu_opts[bench_type];
  }

  var form_desc = {
    list_nth:
      'Tests list access by generating a list of random<br>\
       elements and timing the execution of lists:nth/2<br>\
       removing from the head.<br>\
       <p>List size: <input name="datasize" type="number" min="1" max="10000" value="100" required> (1 - 10000)</p>\
       <p>Nth position: <input name="num_execs" type="number" min="1" max="9999" value="1000" required> (1 - 9999)</p>',
    list_head:
      'Tests list access by generating a list of random<br>\
       elements and timing the repeated execution of [H|T]<br>\
       pattern-matching.<br>\
       <p>List size: <input name="datasize" type="number" min="1" max="10000" value="100" required> (1 - 10000)</p>\
       <p>Nth position: <input name="num_execs" type="number" min="1" max="9999" value="1000" required> (1 - 9999)</p>',
    binary_raw:
      'Tests binary access by generating a binary of random<br>\
       elements and timing the execution of<br>\
       << _Skip:Random, C, _Rest/binary >><br>\
       <p>Binary size: <input name="datasize" type="number" min="1" max="10000" value="100" required> (1 - 10000)</p>\
       <p>Accesses: <input name="num_execs" type="number" min="1" max="9999" value="1000" required> (1 - 9999)</p>',
    binary_at:
      'Tests binary access by generating a list of random<br>\
       elements and timing the execution of binary:at/2.<br>\
       <p>Binary size: <input name="datasize" type="number" min="1" max="10000" value="100" required> (1 - 10000)</p>\
       <p>Accesses: <input name="num_execs" type="number" min="1" max="9999" value="1000" required> (1 - 9999)</p>',
    tuple_inx:
      'Tests tuple access by generating a tuple of random<br>\
       elements and timing the execution of element/2.<br>\
       <p>Tuple size: <input name="datasize" type="number" min="1" max="10000" value="100" required> (1 - 10000)</p>\
       <p>Accesses: <input name="num_execs" type="number" min="1" max="9999" value="1000" required> (1 - 9999)</p>',
    operator_plus:
      'Tests the plus operator (+/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to add them all.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_minus:
      'Tests the minus operator (-/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to subtract them all.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_times:
      'Tests the multiply operator (*/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to multiply them all.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_divide:
      'Tests the divide operator (div/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to divide them all.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_rem:
      'Tests the remainder operator (rem/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to compute remainder on them all.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_gt:
      'Tests the greater than operator (&gt;/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to compare them.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_lt:
      'Tests the less than operator (&lt;/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to compare them.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_eq:
      'Tests the equal operator (==/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to compare them.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    operator_eeq:
      'Tests the exact equal operator (=:=/2) by randomly generating LoopCount integer pairs<br>\
       then using a list comprehension to compare them.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    function_call:
      'Tests function calling by calling decr(LoopCount) until it reaches 0.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    mfa_call:
      'Tests M:F(A) calling by decrementing LoopCount until it reaches 0.<br>\
       <p>Loop count: <input name="num_execs" type="number" min="1" max="1000000" value="10000" required> (1 - 1000000)</p>',
    list_comp: "",
    list_loop: "",
    binary_comp: "",
    binary_loop: "",
    spawn: "",
    spawn_link: "",
    exit: "",
    link: "",
    monitor: "",
    spawn_expire: "",
    timers: "",
    sup_child: "",
    send_msgs:
      'Tests message sending by generating a random set of messages<br>\
       and sending them to a local process, which is not reading the queue.<br>\
       <p>Msg count: <input name="num_execs" type="number" min="1" max="1000000" value="1000" required> (1 - 1000000)</p>',
    recv_msgs:
      'Tests message receive by generating a random set of messages<br>\
       and sending them to a local process, which is not reading the queue.<br>\
       <p>Msg count: <input name="num_execs" type="number" min="1" max="1000000" value="1000" required> (1 - 1000000)</p>',
    router: "",
    concentrator: "",
    serial: "",
    lists_mapfoldl: "",
    lists_mapfoldr: "",
    lists_sort: "",
    lists_zip: ""
  }
            
  function change_bench_desc(benchmark) {
    var bench = document.benchmark_request.bench;
    var bench_desc = document.getElementById("benchmark_desc");
    bench_desc.innerHTML = form_desc[benchmark];
  }
