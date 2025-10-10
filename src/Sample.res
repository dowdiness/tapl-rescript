/* トークンの定義 */
type token =
  | NUMBER(int)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQUALS

let isDigit = (c) => {
  c >= "0" && c <= "9"
}

let isWhitespace = (c) => {
  c->String.trim->String.length == 0
}

/* トークンを識別する関数 */
let identifyToken = (tokenString: string): token => {
  switch (tokenString) {
  | "+" => PLUS
  | "-" => MINUS
  | "*" => TIMES
  | "/" => DIVIDE
  | "=" => ASSIGN
  | "==" => EQUALS
  | _ => {
      /* 数値かどうかチェック */
      let isNumber = isDigit(tokenString)
      if (isNumber) {
        NUMBER(int_of_string(tokenString))
      } else {
        // Console.log(tokenString)
        failwith("Invalid token")
      }
    }
  }
}

/* トークン文字列の一部として許可される文字かどうかを判定する関数 */
let isTokenCharacter = (ch: option<string>): bool => {
  switch (ch) {
    | Some(ch) => {
        switch String.charAt(ch, 0) {
          | "+" | "-" | "*" | "/" | "=" => true
          | _ => isDigit(ch)
        }
    }
    | None => false
  }
}

/* 入力文字列をトークンに分割する関数 */
let tokenize = (input: string): array<token> => {
  let currentIndex = ref(0)
  let tokens = ref([])

  /* トークンを識別しながら走査 */
  while (currentIndex.contents < String.length(input)) {
    // 空白文字列をスキップ
    while isWhitespace(input->String.get(currentIndex.contents)->Option.getOr("Not whitespace")) {
      currentIndex := currentIndex.contents + 1
    }

    let tokenStart = ref(currentIndex.contents)
    let tokenEnd = ref(currentIndex.contents)

    tokenEnd := tokenEnd.contents + 1

    /* トークンの終端を探す */
    while (
      tokenEnd.contents < String.length(input) &&
      isTokenCharacter(input->String.get(tokenEnd.contents
    ))) {
      tokenEnd := tokenEnd.contents + 1
    }

    /* トークン文字列を取得 */
    let tokenString = String.substring(input, ~start=tokenStart.contents, ~end=tokenEnd.contents)->String.trim

    Console.log3(tokenStart.contents, tokenEnd.contents, tokenString)

    /* トークンを識別して配列に追加 */
    tokens := Array.concat(tokens.contents, [identifyToken(tokenString)])

    /* 次のトークンへ */
    currentIndex := tokenEnd.contents
  }

  tokens.contents
}

/* パーサー関数 */
let parse = (input: string) => {
  /* 入力文字列をトークンに分割 */
  let tokens = tokenize(input)
  tokens
  /* パース処理 */
}

let program = "5 + 3 == 8"
Console.log(program)

Console.log(parse(program))
