Test unit:
  $ instrumentation2 -o - unit/identity.js unit/any.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.any("some_arg");
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/array.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = [ esl_symbolic.string("some_arg0") ];
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/array2.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg =
    [ esl_symbolic.string("some_arg0"), esl_symbolic.boolean("some_arg1"), esl_symbolic.number("some_arg2") ];
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/bool.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.boolean("some_arg");
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/function.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.function("some_arg");
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/lazy_object.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: path-traversal
  let some_arg = esl_symbolic.lazy_object();
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/number.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.number("some_arg");
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/object.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = {  };
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/polluted_object2.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: prototype-pollution
  let some_arg = esl_symbolic.polluted_object(2);
  module.exports(some_arg);
  console.log(({}).toString);
  $ instrumentation2 -o - unit/identity.js unit/polluted_object3.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: prototype-pollution
  let some_arg = esl_symbolic.polluted_object(3);
  module.exports(some_arg);
  console.log(({}).toString);
  $ instrumentation2 -o - unit/identity.js unit/string.json
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.string("some_arg");
  module.exports(some_arg);
  $ instrumentation2 -o - unit/identity.js unit/union.json
  Genrating -
  Genrating -
  Genrating -
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.string("some_arg");
  module.exports(some_arg);
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.boolean("some_arg");
  module.exports(some_arg);
  module.exports = function identity(some_arg) {
    return some_arg
  }
  
  let esl_symbolic = require("esl_symbolic");
  esl_symbolic.sealProperties(Object.prototype);
  // Vuln: command-injection
  let some_arg = esl_symbolic.number("some_arg");
  module.exports(some_arg);
