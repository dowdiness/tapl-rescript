// WIP
// ((λ x. x) (λ y. y))

exception Syntax_Error(string)

module Stream = {
  type t = {
    location: ref<int>,
    source: string,
  }

  let make = str => { location: ref(0), source: str }
  let next = stream => {
    let location = stream.location.contents
    let source = stream.source
    stream.location := location + 1
    String.get(source, location)
  }

  let isWhitespace = (str: string) => {
    switch str {
      | " " | "\t" | "\n" | "\r" => true
      | _ => false
    }
  }

  // let rec skipWhitespace = stream => {

  // }
}

type token =
  | LeftParen | RightParen
  | Identifier(string)
  | Lambda
  | End

let stringOfToken = token =>
  switch token {
    | LeftParen => "("
    | RightParen => ")"
    | Identifier(name) => name
    | Lambda => "λ"
    | End => "End"
  }

let toCharCode = c => String.charCodeAt(c, 0)

let isAlphabet = (c) => c >= "a" && c <= "z" || c >= "A" && c <= "Z"

let source = "((λ x. x) (λ y. y))"

let lexer = source => {
  open Stream
  let stream = make(source)
  let rec lex = acc => {
    switch stream->next {
      | Some(c) => {
        switch c {
          | "(" => lex(list{LeftParen, ...acc})
          | ")" => lex(list{RightParen, ...acc})
          | "λ" => lex(list{Lambda, ...acc})
          | c if isAlphabet(c) => lex(list{Identifier(c), ...acc})
          | _ => lex(acc)
        }
      }
      | None => acc
    }
  }
  lex(list{})->List.reverse
}

// Console.log(lexer("(λa)")->List.toArray)
