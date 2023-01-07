//// Based on gleeunit/should, verbatim, renamed to avoid conflicts.
////
//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

if erlang {
  pub external fn equal(a, a) -> Nil =
    "gleeunit2_ffi" "should_equal"

  pub external fn not_equal(a, a) -> Nil =
    "gleeunit2_ffi" "should_not_equal"

  pub external fn be_ok(Result(a, b)) -> a =
    "gleeunit2_ffi" "should_be_ok"

  pub external fn be_error(Result(a, b)) -> b =
    "gleeunit2_ffi" "should_be_error"
}

if javascript {
  external fn stringify(anything) -> String =
    "../gleam.mjs" "inspect"

  external fn crash(String) -> anything =
    "../gleeunit2_ffi.mjs" "crash"

  pub fn equal(a, b) {
    case a == b {
      True -> Nil
      _ ->
        crash("\n\t" <> stringify(a) <> "\n\tshould equal \n\t" <> stringify(b))
    }
  }

  pub fn not_equal(a, b) {
    case a != b {
      True -> Nil
      _ ->
        crash("\n" <> stringify(a) <> "\nshould not equal \n" <> stringify(b))
    }
  }

  pub fn be_ok(a) {
    case a {
      Ok(value) -> value
      _ -> crash("\n" <> stringify(a) <> "\nshould be ok")
    }
  }

  pub fn be_error(a) {
    case a {
      Error(error) -> error
      _ -> crash("\n" <> stringify(a) <> "\nshould be error")
    }
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

pub fn fail() -> Nil {
  be_true(False)
}
