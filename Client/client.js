//Opardum Client

function go() {
var embed = tiki.require("embedded");
var node = document.getElementById("edit");
var bespin = embed.useBespin(node);
var oldLines = "";
var oldText = "";
var connection = new WebSocket("ws://localhost:9988");
var d_c = 0;
var suppress = false;
var opList = [];

function hideChange (f) {
   var oldsuppress = suppress;
   suppress = true;
   f();
   suppress = oldsuppress;
   oldLines = bespin._getTextStorage().lines.slice(0);
   oldText  = bespin.getValue();
}

function apply(op) {
  d_c = 0;
  var lines = bespin._getTextStorage().lines;
  var text  = bespin.getValue();
  var selection = bespin.getSelection();
  var selectionStart = flatten(selection.start,lines);
  var selectionEnd   = flatten(selection.end,lines);
  console.log("s: " + selectionStart, "e: " + selectionEnd);
  console.log(selection.start);
  var x = 0;
  var oldx = 0;
  for (var i in op) {
     if (op[i].type == "Retain") {
        x += op[i].data-0;
        oldx += op[i].data-0;
     } else if (op[i].type == "Insert" && op[i].data.length > 0) {
        var foo = unflatten(x,lines);
        console.log("foo",foo);
        bespin.replace({start:foo, end:foo},op[i].data,false);        
        lines = bespin._getTextStorage().lines;
        if (selectionStart >= x) selectionStart += op[i].data.length;
        if (selectionEnd >= x) selectionEnd     += op[i].data.length;
        x +=  op[i].data.length;
     } else if (op[i].type == "Delete") {
        var foo = unflatten(x,lines);                      
        var bar = unflatten(x + op[i].data.length,lines);                      
        bespin.replace({start:foo, end:bar},'',false);        
        lines = bespin._getTextStorage().lines;
        if (selectionStart >= x && selectionStart <= x + op[i].data.length) selectionStart = x;
        else if (selectionStart >= x) selectionStart -= op[i].data.length;
        if (selectionEnd >= x && selectionEnd <= x + op[i].data.length) selectionEnd = x;
        else if (selectionEnd >= x) selectionEnd -= op[i].data.length;
        oldx += op[i].data.length;
     }
  }
  console.log("=> s: " + selectionStart, "e: " + selectionEnd);
//  bespin.setValue(newtext);  
  lines = bespin._getTextStorage().lines;
  selection.start = unflatten(selectionStart,lines);
  selection.end = unflatten(selectionEnd,lines);
  console.log(selection.start);
  bespin.setSelection(selection);
}

function isSingleton(a) {
  return (a.start.col == a.end.col
       && a.start.row == a.end.row);
}

function unflatten (point,str)  {
   var count = 0;
   var row =0;
   for (var i = 0; i < str.length; i++) {
      if (count + str[i].length + 1 > point) {
         break;
      } 
      count = count + str[i].length + 1;
      row ++;
   }
   return { row: row, col: point - count};
}

function flatten (point,str)  {
   var count = 0;
   for (var i = 0; i < point.row; i++) {
      count += str[i].length;
      count++;
   }
   return count + point.col;
}

function transport (op) {
   console.log("sending..");
   connection.send(JSON.stringify([d_c,op]));
   opList.push(op);
   d_c++;
}

connection.onopen = function () {
   hideChange(function() {
      bespin.setValue("");
   });
   connection.send(prompt("Enter document name"));
}

connection.onmessage = function (evt) {
   console.log("Message recieved!" + evt.data);
   hideChange(function() {
      var str = evt.data.charAt(0) != "[" ? evt.data.slice(1) : evt.data; 
      var input = eval(str);      
      var d_s = input[0];      
      opList.splice(0,d_s);
      var newList = [];
      var op = input[1];
      for (var i = 0; i < opList.length; i++) {
         var result = transform(op,opList[i]);
         newList.push(result[1]);
         op = result[0];
      }
      apply(op);
      opList = newList;
   });
}

connection.onclose = function () {
   console.log("Connection lost.");
}


bespin.addEventListener('textChange', function(e) {
  if (suppress || isSingleton(e.oldRange) && isSingleton(e.newRange)) return;

  var lines = bespin._getTextStorage().lines.slice(0);
  var text  = bespin.getValue();
  var editStart = flatten(e.newRange.start,lines);
  var editEnd = flatten(e.newRange.end,lines);
  var op = [];
  if (editStart > 0) op.push({type:"Retain", data:"" + editStart});
  if (isSingleton(e.oldRange)) {    
     op.push({type:"Insert", data:bespin.getText(e.newRange)});
  } else {
     var oldStart = flatten(e.oldRange.start, oldLines);
     var oldEnd = flatten(e.oldRange.end, oldLines);
     if (isSingleton(e.newRange)) {
       op.push({type:"Delete", data:oldText.substring(oldStart,oldEnd)});
     } else {
       op.push({type:"Delete", data:oldText.substring(oldStart,oldEnd)});
       op.push({type:"Insert", data:bespin.getText(e.newRange)});
     }
  }
  if (text.length - editEnd > 0) op.push({type:"Retain", data:"" + (text.length -  editEnd)});
  console.log(JSON.stringify(op));
  transport(op);
  oldLines = lines;
  oldText = text;
});
}
