Test toy examples:
  $ instrumentation2 toy/vfunexported.js toy/vfunexported.json -o -
  Genrating -
  let exec = require('child_process').exec;
  
  moduke.exports = function f(x) {
    return exec(x);
  };
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let x = esl_symbolic.string("x");
  module.exports(x);
  $ instrumentation2 toy/vfunretbyexport.js toy/vfunretbyexport.json -o -
  Genrating -
  function f1(a) {
    return function f2(b) {
      if (b > 0) {
        eval(a);
      }
    };
  };
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: code-injection
  let a = esl_symbolic.string("a");
  var ret_f1 = f1(a);
  let b = esl_symbolic.number("b");
  ret_f1(b);
  $ instrumentation2 toy/vfunpropofexportedobj.js toy/vfunpropofexportedobj.json -o -
  Genrating -
  let Obj = (function () {
    function Obj(source) { this.source = source; }
  
    Obj.prototype.f = function (obj) {
      if (obj.cond > 0) {
        eval(this.source);
      }
    }
  
    return Obj;
  })();
  
  module.exports.Obj = Obj;
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: code-injection
  let source = esl_symbolic.string("source");
  var ret_module_exports_Obj = module.exports.Obj(source);
  let obj = { cond: esl_symbolic.number("cond") };
  ret_module_exports_Obj.f(obj);
  $ instrumentation2 toy/example-20.js toy/example-20.json -o -
  Genrating -
  // testing taint from parameter to eval function call
  // multi scope function using this
  
  module.exports = function f(x) {
    this.input = x;
  
    f.prototype.ev = function() {
      let self = this;
      eval(self.input);
    }
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: code-injection
  let x = esl_symbolic.string("x");
  module.exports(x);
  module.exports.ev();
