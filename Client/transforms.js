

function oplength (op) {
   return op.type == "Retain" ? op.data - 0
        : op.type == "Insert" ? op.data.length
        : op.type == "Delete" ? op.data.length
        : null;
}

function sub (n,op) {
   return op.type == "Retain" ? {type: "Retain", data: ""+ (op.data - n) }
        : op.type == "Insert" ? {type: "Insert", data: op.data.slice(n)}
        : op.type == "Delete" ? {type: "Delete", data: op.data.slice(n)}
        : null;
}

function first (n, op) {
   return op.type == "Retain" ? {type: "Retain", data: ""+n }
        : op.type == "Insert" ? {type: "Insert", data: op.data.slice(0,n)}
        : op.type == "Delete" ? {type: "Delete", data: op.data.slice(0,n)}
        : null;
}

function normalize(op) {
   var ret = [];
   var current = {type:"Insert", data:"", flag: true};
   for (var i = 1; i < op.length; i++) {
     if (op[i].type == current.type) {
        if (current.type == "Retain") {           
           current = {type:"Retain", data: ((current.data-0) + (op[i].data-0))+""};
        } else {
           current = {type:current.type, data: current.data + op[i].data};
        }
     } else {
        if (  (op[i].type == "Retain" && op[i].data-0 != 0 )
           || (op[i].data != "")) 
        current = op[i];
     }         
   }
}


function transform(ops, ags) {
   var out1 = [];
   var out2 = [];
   while (ops.length > 0 && ags.length > 0) {
      var op = ops.shift();
      var ag = ags.shift();
      var l_op = oplength(op);
      var l_ag = oplength(ag);
      if (op.type == "Retain" && ag.type == "Retain") {
         var out = {type:"Retain", data: Math.min(l_op,l_ag)+""};
         out1.push(out);
         out2.push(out);
         if      (l_op > l_ag) ops.unshift(sub(l_ag,op));
         else if (l_op < l_ag) ags.unshift(sub(l_op,ag));         
      } else if (op.type == "Insert") {
         ags.unshift(ag);
         out1.push(op);
         out2.push({type: "Retain", data:""+l_op});
      } else if (ag.type == "Insert") {
         ops.unshift(op);
         out1.push({type: "Retain", data:""+l_ag});
         out2.push(ag);
      } else if (op.type == "Delete") {
         out1.push(op);
         if      (l_op > l_ag) ops.unshift(sub(l_ag,op));
         else if (l_op < l_ag) ags.unshift(sub(l_op,ag));         
      } else if (ag.type == "Delete") {
         out2.push(ag);
         if      (l_op > l_ag) ops.unshift(sub(l_ag,op));
         else if (l_op < l_ag) ags.unshift(sub(l_op,ag));         
      }
   }
   while (ops.length > 0) {
      var op = ops.shift();
      var l_op = oplength(op);
      out1.push(op);
      out2.push({type: "Retain", data:""+l_op});
   }
   while (ags.length > 0) {
      var ag = ags.shift();
      var l_ag = oplength(ag);
      out1.push({type: "Retain", data:""+l_ag});
      out2.push(ag);
   }
   return [out1,out2];
}
