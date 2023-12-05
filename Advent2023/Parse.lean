import Lean.Data.Parsec

open Lean Parsec

namespace Parse

def nat : Parsec Nat := do
  let ds <- many digit
  pure (String.mk ds.toList).toNat!

def sepBy1 {α : Type} (p : Parsec α) (sep : String) : Parsec (Array α) := do
  let a <- p
  let as <- many (skipString sep *> p)
  pure $ Array.mkArray1 a ++ as

def lines {α : Type} (parser : Parsec α) (lines : List String) : Except String (List α) :=
  let rec go : List α -> List String -> Except String (List α) := fun
    | acc, [] => Except.ok acc.reverse
    | acc, l :: ls  => match Lean.Parsec.run parser l with
      | Except.error m => Except.error m
      | Except.ok p => go (p :: acc) ls
  go [] lines

def input {α : Type} (parser : Parsec α) (path : System.FilePath) : IO (List α) := do
  let lines ← IO.FS.lines path

  match Parse.lines parser lines.toList with
    | Except.error e => do
      IO.println $ "Failed to parse " ++ e
      IO.Process.exit 1
    | Except.ok result => pure result

end Parse
