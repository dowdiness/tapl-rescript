// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function isNumericVal(_term) {
  while(true) {
    var term = _term;
    if (typeof term !== "object") {
      if (term === "Zero") {
        return true;
      } else {
        return false;
      }
    }
    if (term.TAG !== "Succ") {
      return false;
    }
    _term = term._0;
    continue ;
  };
}

function isVal(term) {
  if (typeof term === "object") {
    if (isNumericVal(term)) {
      return true;
    } else {
      return false;
    }
  }
  switch (term) {
    case "True" :
    case "False" :
        return true;
    default:
      if (isNumericVal(term)) {
        return true;
      } else {
        return false;
      }
  }
}

var NoRuleApplies = /* @__PURE__ */Caml_exceptions.create("SmallStepArith.NoRuleApplies");

function eval1(term) {
  if (typeof term !== "object") {
    throw {
          RE_EXN_ID: NoRuleApplies,
          _1: term,
          Error: new Error()
        };
  }
  switch (term.TAG) {
    case "If" :
        var t1 = term._0;
        if (typeof t1 !== "object") {
          switch (t1) {
            case "True" :
                return term._1;
            case "False" :
                return term._2;
            default:
              
          }
        }
        var t1$p = eval1(t1);
        return {
                TAG: "If",
                _0: t1$p,
                _1: term._1,
                _2: term._2
              };
    case "Succ" :
        var t1$p$1 = eval1(term._0);
        return {
                TAG: "Succ",
                _0: t1$p$1
              };
    case "Pred" :
        var nv1 = term._0;
        if (typeof nv1 !== "object") {
          if (nv1 === "Zero") {
            return "Zero";
          }
          
        } else if (nv1.TAG === "Succ") {
          var nv1$1 = nv1._0;
          if (isNumericVal(nv1$1)) {
            return nv1$1;
          }
          
        }
        var t1$p$2 = eval1(nv1);
        return {
                TAG: "Pred",
                _0: t1$p$2
              };
    case "IsZero" :
        var nv1$2 = term._0;
        if (typeof nv1$2 !== "object") {
          if (nv1$2 === "Zero") {
            return "True";
          }
          
        } else if (nv1$2.TAG === "Succ" && isNumericVal(nv1$2._0)) {
          return "False";
        }
        var t1$p$3 = eval1(nv1$2);
        return {
                TAG: "IsZero",
                _0: t1$p$3
              };
    
  }
}

function $$eval(_term) {
  while(true) {
    var term = _term;
    console.log(term);
    if (isVal(term)) {
      return term;
    }
    var t$p = eval1(term);
    _term = t$p;
    continue ;
  };
}

console.log("Succ(Succ(Succ(Succ(Succ(Zero)))))");

$$eval({
      TAG: "Succ",
      _0: {
        TAG: "Succ",
        _0: {
          TAG: "Succ",
          _0: {
            TAG: "Succ",
            _0: {
              TAG: "Succ",
              _0: "Zero"
            }
          }
        }
      }
    });

console.log("If(True, If(False, False, False), True)");

$$eval({
      TAG: "If",
      _0: "True",
      _1: {
        TAG: "If",
        _0: "False",
        _1: "False",
        _2: "False"
      },
      _2: "True"
    });

export {
  isNumericVal ,
  isVal ,
  NoRuleApplies ,
  eval1 ,
  $$eval ,
}
/*  Not a pure module */
