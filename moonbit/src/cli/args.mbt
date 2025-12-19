// CLI引数パーサー (pico-argsベース)

///|
pub struct Opt {
  name : String
  flag : Bool?
  value : String?
  short : String?
} derive(Show, Eq)

pub fn Opt::new(name: String, flag?: Bool, value?: String, short?: String) -> Opt {
  Opt::{
    name,
    flag,
    value,
    short
  }
}

struct Arg {
  name: String
  value: String?
} derive(Show)

struct Cli {
  name: String
  path: @path.Path
  options: Array[Opt]
  args: Array[Arg]
}

impl Show for Cli with output(self, logger) {
  logger
  ..write_string("Name: \{self.name}")
  ..write_string("\nPath: \{self.path.to_string()}")
  ..write_string("\nOptions: ")
  ..write_object(self.options)
  ..write_string("\nArgs: ")
  ..write_iter(self.args.iter())
}

pub fn Cli::new(name: String, options: Array[Opt]) -> Cli raise {
  let all_args = @sys.get_cli_args()
  let path = @path.Path(all_args[0])
  let args = all_args[1:].to_array()
  if args.length() > options.length() { fail("Too many arguments: \{args}") }

  fn handleOption(option: StringView, pred: (String) -> (Opt) -> Bool) -> Arg raise {
    let option = option.to_string()
    let parts = split_once(option, "=")
    match parts {
      Some((key, value)) => {
        let selected = options
          .iter()
          .find_first(pred(key))
        match selected {
          Some(selected) => Arg::{ name: selected.name, value: Some(value) }
          None => fail("Invalid option: \{option}")
        }
      }
      None => fail("Invalid option: \{option}")
    }
  }
  let args = args.mapi(fn(i, arg) {
    match arg {
      // long option
      [.."--", ..option] => {
        handleOption(
          option,
          fn(name){
            fn(opt){ opt.name == name }
          }
        )
      }
      // short option
      [.."-", ..option] => {
        handleOption(
          option,
          fn(short){
            fn(opt){ opt.short.unwrap_or_default() == short }
          }
        )
      }
      value => Arg::{ name: options[i].name, value: Some(value) }
    }
  })

  Cli::{
    name,
    path,
    options,
    args
  }
}

///|
pub struct Arguments {
  path : String
  args : Array[Opt]
}

impl Default for Arguments with default() {
  let all_args = @sys.get_cli_args()
  let path = all_args[0]
  let args = all_args[1:].to_array()
  let args = args.map(fn(arg) {
    match arg {
      [.."--", ..option] | [.."-", ..option] => {
        let option = option.to_string()
        let parts = split_once(option, "=")
        match parts {
          Some((key, value)) => Opt::new(key, value=value, flag=true)
          None => Opt::new(option, flag=true)
        }
      }
      _ => Opt::new(arg)
    }
  })
  Arguments::{ path, args }
}

pub fn Arguments::new() -> Arguments {
  Arguments::default()
}

/// 指定されたフラグが存在するかチェック
// pub fn Arguments::contains_flag(self : Arguments, flag : String) -> Bool {
//   for i = 0; i < self.args.length(); i = i + 1 {
//     match self.args[i] {
//       Flag(name) => {
//         if name == flag {
//           return true
//         }
//       }
//       _ => ()
//     }
//   }
//   false
// }

// ヘルパー関数

/// 文字列を指定された文字で最初の1回だけ分割
fn split_once(s : String, delimiter : String) -> (String, String)? {
  match s.find(delimiter) {
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
