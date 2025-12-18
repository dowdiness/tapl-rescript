fn main {
  let args = @sys.get_cli_args()
  if args.length() < 2 {
    println("Please provide source code or a path to code as argument.")
    return
  }
  let code = args.get(1).unwrap_or("(λx.λy.x y) 1 2")
  try {
    let term = @parser.parse(code)
    let renamed = @rename.rename(term)
    let assembly = @lowering.lowering(renamed)
    println("Code: " + code)
    println("Parsed: " + term.to_string())
    println("Renamed: " + renamed.to_string())
    println(assembly)
  } catch {
    @parser.TokenizationError(s) => println(s)
    @parser.ParseError(s) => println(s)
    @rename.RenameError(s) => println(s)
    @lowering.LoweringError(s) => println(s)
    e => println(e)
  }
}
