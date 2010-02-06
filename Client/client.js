//var ws_location = "123.243.79.173";
var ws_location = "localhost";
var ws_port = 9876;
$(document).ready(function() {
   if ("WebSocket" in window) {
      var ignoring_server = false;
      var ws = new WebSocket("ws://" + ws_location + ":" + ws_port + "/");
      var opened = false;
      var doc = "";
      var apply = function (ops) {
         var position = 0;
         for (var op_ in ops) {
            var op = ops[op_];
            if (op.type == "Retain") {
               position += op.data;
            } else if (op.type == "Delete") {
               doc = doc.substring(0,position) + doc.substr(0 + op.data.length);
            } else if (op.type == "Insert") {
               
               doc = doc.substring(0,position) + op.data + doc.substr(position);
            }
         }
         $('#output').text(doc);
      }
      ws.onopen = function() { 
         $('#connectionStatus').text('Connection opened');
         opened = true;
      };
      ws.onmessage = function(evt) {
         if (!ignoring_server) {
            var result;
            eval(" result = " + evt.data)
            apply(result[1]);
            $('#currentVersion').text('current version:' + result[0]);
         }
      };
      ws.onclose = function() {
         $('#connectionStatus').text('Connection closed');
         opened = false;
      };
      $('#sendbutton').click(function () {
         if (opened) {
            ws.send($('#inputbox').val());
            $('#inputbox').val('').focus();
         };
      });
   } else {
      $('#connectionStatus').append('<p>Your browser does not support web sockets</p>');
   }
});
