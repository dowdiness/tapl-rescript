fn main {
  let args = @sys.get_cli_args()
  if args.length() < 2 {
    println("Please provide source code or a path to code as argument.")
    return
  }
  let code = args.get(1).unwrap_or("(λx.λy.x y) 1 2")
  try {
    println("Code:\n\{code}\n")
    let term = @parser.parse(code)
    println("Parsed:\n\{@parser.print_term(term)}\n")
    let renamed = @rename.rename(term)
    println("Renamed:\n\{@parser.print_term(renamed)}\n")
    let anf = @compile.convert(renamed)
    println("ANF:\n" + anf.to_string() + "\n\n" + @compile.print_anf(anf) + "\n")
    let assembly = @lowering.lowering(anf)
    println("Assembly:\n" + assembly)
  } catch {
    @parser.TokenizationError(s) => println(s)
    @parser.ParseError(s) => println("ParseError: \{s}")
    @rename.RenameError(s) => println("RenameError: \{s}")
    @lowering.LoweringError(s) => println("LoweringError: \{s}")
    e => println(e)
  }
}
