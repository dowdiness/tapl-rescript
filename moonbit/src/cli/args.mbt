// CLI arguments parser

///|
pub struct Arg {
  name : String
  flag : Bool?
  value : String?
  short : String?
} derive(Show, Eq)

///|
pub fn Arg::new(
  name : String,
  flag? : Bool,
  value? : String,
  short? : String,
) -> Arg {
  Arg::{ name, flag, value, short }
}

enum MatchedArg {
  Flag
  Value(String)
} derive(Show)

///|
struct Cli {
  name : String
  path : @path.Path
  args : Array[Arg]
  matched_args : Map[String, MatchedArg]
}

///|
pub impl Show for Cli with output(self, logger) {
  logger
  ..write_string("Name: \{self.name}")
  ..write_string("\nPath: \{self.path.to_string()}")
  ..write_string("\nArgs: ")
  ..write_object(self.args)
  ..write_string("\nMatched: ")
  ..write_iter(self.matched_args.iter())
}

///|
pub fn Cli::new(name : String, arg_config : Array[Arg]) -> Cli raise {
  let all_args = @sys.get_cli_args()
  let path = @path.Path(all_args[0])
  let args = all_args[1:].to_array()
  if args.length() > arg_config.length() {
    fail("Too many arguments: \{args}")
  }
  fn handleArg(
    arg : StringView,
    pred : (String) -> (Arg) -> Bool,
  ) -> (String, MatchedArg) raise {
    let arg = arg.to_string()
    let parts = split_at(arg, '=')
    match parts {
      Some((key, value)) => {
        match arg_config.iter().find_first(pred(key)) {
          Some(selected) => (selected.name, MatchedArg::Value(value.to_string()))
          None => fail("Invalid arg: \{arg}")
        }
      }
      None => (arg, MatchedArg::Flag)
    }
  }

  let matched_args = Map::from_array(args.mapi(fn(i, arg) {
    match arg {
      // long arg
      [.. "--", .. arg] =>
        handleArg(arg, fn(name) { fn(opt) { opt.name == name } })
      // short arg
      [.. "-", .. arg] =>
        handleArg(arg, fn(short) {
          fn(opt) { opt.short.unwrap_or_default() == short }
        })
      value => (arg_config[i].name, MatchedArg::Value(value.to_string()))
    }
  }))
  Cli::{ name, path, args: arg_config, matched_args }
}

// Helper functions

///| Split a string into two elements tuple at the first occurrence of a delimiter.
fn split_at(s : String, delimiter : Char) -> (String, String)? {
  match s.find(delimiter.to_string()) {
    Some(i) => {
      let before = try? s[:i]
      let after = try? s[i + 1:]
      match (before, after) {
        (Ok(b), Ok(a)) => return Some((b.to_string(), a.to_string()))
        _ => return None
      }
    }
    None => return None
  }
}

// ===== split_at Helper Tests =====

test "split_at with valid delimiter" {
  let result = split_at("key=value", '=')
  inspect(result, content="Some((\"key\", \"value\"))")
}

test "split_at with multiple delimiters - splits at first" {
  let result = split_at("key=value=extra", '=')
  inspect(result, content="Some((\"key\", \"value=extra\"))")
}

test "split_at with no delimiter" {
  let result = split_at("nodelimiter", '=')
  inspect(result, content="None")
}

test "split_at with empty string" {
  let result = split_at("", '=')
  inspect(result, content="None")
}

test "split_at with delimiter at start" {
  let result = split_at("=value", '=')
  inspect(result, content="Some((\"\", \"value\"))")
}

test "split_at with delimiter at end" {
  let result = split_at("key=", '=')
  inspect(result, content="Some((\"key\", \"\"))")
}

test "split_at with space delimiter" {
  let result = split_at("hello world", ' ')
  inspect(result, content="Some((\"hello\", \"world\"))")
}
