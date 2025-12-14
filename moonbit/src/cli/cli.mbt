fn main {
  let args = @sys.get_cli_args()
  let code = args.get(1).unwrap_or("(λx.λy.x y) 1 2")
  let term = try? @parser.parse(code)
  let renamed = @rename.rename(term.unwrap())

  println("Code: " + code)
  println("Parsed: " + term.to_string())
  println("Renamed: " + renamed.to_string())
}
