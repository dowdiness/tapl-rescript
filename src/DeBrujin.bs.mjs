// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__List from "@rescript/core/src/Core__List.bs.mjs";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function pickFreshName(ctx, _name) {
  while(true) {
    var name = _name;
    var match = Core__List.getBy(ctx, (function(name){
        return function (param) {
          return name === param[0];
        }
        }(name)));
    if (match === undefined) {
      return [
              Core__List.add(ctx, [
                    name,
                    "NameBind"
                  ]),
              name
            ];
    }
    _name = match[0] + "'";
    continue ;
  };
}

function indexToName(ctx, x) {
  var match = Core__List.get(ctx, x);
  if (match !== undefined) {
    return match[0];
  } else {
    return "[" + String(x) + " bad index]";
  }
}

function printTerm(ctx, t) {
  switch (t.TAG) {
    case "Var" :
        var n = t._1;
        if (Core__List.length(ctx) === n) {
          return indexToName(ctx, t._0);
        } else {
          return "[" + String(Core__List.length(ctx)) + " " + String(n) + " bad index]";
        }
    case "Abs" :
        var match = pickFreshName(ctx, t._0);
        return "(λ " + match[1] + ". " + printTerm(match[0], t._1) + ")";
    case "App" :
        return "(" + printTerm(ctx, t._0) + " " + printTerm(ctx, t._1) + ")";
    
  }
}

function shift(d, t) {
  var walk = function (c, t) {
    switch (t.TAG) {
      case "Var" :
          var n = t._1;
          var k = t._0;
          if (k >= c) {
            return {
                    TAG: "Var",
                    _0: k + d | 0,
                    _1: n + d | 0
                  };
          } else {
            return {
                    TAG: "Var",
                    _0: k,
                    _1: n + d | 0
                  };
          }
      case "Abs" :
          return {
                  TAG: "Abs",
                  _0: t._0,
                  _1: walk(c + 1 | 0, t._1)
                };
      case "App" :
          return {
                  TAG: "App",
                  _0: walk(c, t._0),
                  _1: walk(c, t._1)
                };
      
    }
  };
  return walk(0, t);
}

function subst(j, s, t) {
  var walk = function (c, t) {
    switch (t.TAG) {
      case "Var" :
          var k = t._0;
          if (k === (j + c | 0)) {
            return shift(c, s);
          } else {
            return {
                    TAG: "Var",
                    _0: k,
                    _1: t._1
                  };
          }
      case "Abs" :
          return {
                  TAG: "Abs",
                  _0: t._0,
                  _1: walk(c + 1 | 0, t._1)
                };
      case "App" :
          return {
                  TAG: "App",
                  _0: walk(c, t._0),
                  _1: walk(c, t._1)
                };
      
    }
  };
  return walk(0, t);
}

function substTop(s, t) {
  return shift(-1, subst(0, shift(1, s), t));
}

function isVal(_ctx, t) {
  switch (t.TAG) {
    case "Abs" :
        return true;
    case "Var" :
    case "App" :
        return false;
    
  }
}

var NoRuleApplies = /* @__PURE__ */Caml_exceptions.create("DeBrujin.NoRuleApplies");

function eval1(ctx, t) {
  switch (t.TAG) {
    case "Var" :
    case "Abs" :
        throw {
              RE_EXN_ID: NoRuleApplies,
              _1: t,
              Error: new Error()
            };
    case "App" :
        var v1 = t._0;
        switch (v1.TAG) {
          case "Abs" :
              var v2 = t._1;
              if (isVal(ctx, v2)) {
                return substTop(v2, v1._1);
              }
              break;
          case "Var" :
          case "App" :
              break;
          
        }
        var t2 = t._1;
        if (isVal(ctx, v1)) {
          var t2$p = eval1(ctx, t2);
          return {
                  TAG: "App",
                  _0: v1,
                  _1: t2$p
                };
        }
        var t1$p = eval1(ctx, v1);
        return {
                TAG: "App",
                _0: t1$p,
                _1: t2
              };
        break;
    
  }
}

function $$eval(ctx, _t) {
  while(true) {
    var t = _t;
    console.log(printTerm(ctx, t));
    if (isVal(ctx, t)) {
      return t;
    }
    var t$p = eval1(ctx, t);
    _t = t$p;
    continue ;
  };
}

$$eval(/* [] */0, {
      TAG: "App",
      _0: {
        TAG: "Abs",
        _0: "x",
        _1: {
          TAG: "Var",
          _0: 0,
          _1: 1
        }
      },
      _1: {
        TAG: "Abs",
        _0: "y",
        _1: {
          TAG: "Var",
          _0: 0,
          _1: 1
        }
      }
    });

$$eval(/* [] */0, {
      TAG: "App",
      _0: {
        TAG: "Abs",
        _0: "x",
        _1: {
          TAG: "Abs",
          _0: "y",
          _1: {
            TAG: "Var",
            _0: 1,
            _1: 2
          }
        }
      },
      _1: {
        TAG: "Abs",
        _0: "z",
        _1: {
          TAG: "Var",
          _0: 0,
          _1: 1
        }
      }
    });

export {
  pickFreshName ,
  indexToName ,
  printTerm ,
  shift ,
  subst ,
  substTop ,
  isVal ,
  NoRuleApplies ,
  eval1 ,
  $$eval ,
}
/*  Not a pure module */