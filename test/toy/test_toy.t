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
  
  const esl_symbolic = require('esl_symbolic');
  // Vuln: code-injection
  let a = esl_symbolic.string("a");
  var ret_f1 = f1(a);
  // Vuln: code-injection
  let b = esl_symbolic.number("b");
  ret_f1(b);
  All OK!
