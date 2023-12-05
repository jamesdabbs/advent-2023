namespace «1»

def digits :=
  let ds := List.range 10
  List.zip (List.map toString ds) ds

def digitWords :=
  List.zip
    ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    (List.range 10)

def decons (pairs : List (String × Nat)) (str : String) :=
  let rec go := fun
    | ((pre, val) :: rest), s => if s.startsWith pre
      then (some val, s.drop 1)
      else go rest s
    | [], s => (none, s.drop 1)
  go pairs str

-- TODO: remove partial
partial def gather (pairs : List (String × Nat)) : String → Option Nat × Option Nat :=
  let rec go := fun
    | a, b, "" => (a, b)
    | a, b, s =>
      let (v, s') := decons pairs s
      go (if Option.isSome a then a else v) (if Option.isSome v then v else b) s'

  go none none

def evaluate
  | (some a, some b) => 10 * a + b
  | _ => 0

def run pairs :=
  Array.foldl Nat.add 0 ∘ Array.map (evaluate ∘ gather pairs)

def solve (input : System.FilePath) := do
  let lines ← IO.FS.lines input

  -- 55477
  IO.println $ run digits lines

  -- 54431
  IO.println $ run (digits ++ digitWords) lines

end «1»
