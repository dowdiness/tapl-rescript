fn main {
  let options = [
    Opt::new("code"),
    Opt::new("input", short="i"),
    Opt::new("output", value="lambda.s", short="o")
  ]
  let cli = try? Cli::new("lambda", options)
  match cli {
    Ok(cli) => println(cli)
    Err(e) => println("Error: \{e}")
  }
  let code = "1 + 2"
  let debug_mode = false
  let output_file = "lambda.s"
  // コンパイル処理
  try {
    if debug_mode { println("Code:\n\{code}\n") }
    let term = @parser.parse(code)
    if debug_mode { println("Parsed:\n\{@parser.print_term(term)}\n") }
    let renamed = @rename.rename(term)
    if debug_mode { println("Renamed:\n\{@parser.print_term(renamed)}\n") }
    let anf = @compile.convert(renamed)
    if debug_mode { println("ANF:\n" + anf.to_string() + "\n\n" + @compile.print_anf(anf) + "\n") }
    let assembly = @lowering.lowering(anf)
    if debug_mode { println("Assembly:\n" + assembly) }
    @fs.write_string_to_file(output_file, assembly)
    println("Successfully compiled to \{output_file}")
  } catch {
    @parser.TokenizationError(s) => println("TokenizationError: \{s}")
    @parser.ParseError(s) => println("ParseError: \{s}")
    @rename.RenameError(s) => println("RenameError: \{s}")
    @lowering.LoweringError(s) => println("LoweringError: \{s}")
    e => println("Error: \{e}")
  }
}

/// ヘルプメッセージを表示
// fn help() -> String {
//   let help =
//     #|Lambda Calculus Compiler
//     #|
//     #|USAGE:
//     #|  lambda [OPTIONS] <SOURCE>
//     #|
//     #|ARGUMENTS:
//     #|  <SOURCE>              Lambda calculus expression to compile
//     #|
//     #|OPTIONS:
//     #|  -d, --debug           Enable debug mode (show intermediate results)
//     #|  -o, --output <FILE>   Specify output file (default: lambda.s)
//     #|  -h, --help            Print help information
//     #|
//     #|EXAMPLES:
//     #|  lambda "(λx.λy.x y) 1 2"
//     #|  lambda --debug "(λx.x) 42"
//     #|  lambda --output result.s "(λx.x) 1"

//   help
// }
