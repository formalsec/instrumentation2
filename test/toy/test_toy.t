Test VFunRetByExport:
  $ instrumentation2 vfunretbyexport.js vfunretbyexport.json -o -
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
