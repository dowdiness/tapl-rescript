// CLI arguments parser tests

// ===== Arg Tests =====

test "Arg::new with all fields" {
  let arg = Arg::new("verbose", flag=true, value="test", short="v")
  inspect(arg.name, content="verbose")
  inspect(arg.flag, content="Some(true)")
  inspect(arg.value, content="Some(\"test\")")
  inspect(arg.short, content="Some(\"v\")")
}

test "Arg::new with name only" {
  let arg = Arg::new("debug")
  inspect(arg.name, content="debug")
  inspect(arg.flag, content="None")
  inspect(arg.value, content="None")
  inspect(arg.short, content="None")
}

test "Arg::new with flag" {
  let arg = Arg::new("help", flag=true)
  inspect(arg.name, content="help")
  inspect(arg.flag, content="Some(true)")
}

test "Arg::new with value" {
  let arg = Arg::new("output", value="file.txt")
  inspect(arg.name, content="output")
  inspect(arg.value, content="Some(\"file.txt\")")
}

test "Arg::new with short flag" {
  let arg = Arg::new("phase", short="p")
  inspect(arg.name, content="phase")
  inspect(arg.short, content="Some(\"p\")")
}

test "Arg equality - same values" {
  let arg1 = Arg::new("test", flag=true, value="value", short="t")
  let arg2 = Arg::new("test", flag=true, value="value", short="t")
  inspect(arg1 == arg2, content="true")
}
