import «Advent2023»

def main (args : List String) : IO Unit :=
  let day := (args.get? 0).getD "1"
  let path := System.mkFilePath [(args.get? 1).getD s!"inputs/{day}"]
  match day with
  | "1" => «1».solve path
  | "2" => «2».solve path
  | _ => IO.println s!"Not yet implemented: {day}"
