import Lean.Data.Parsec
import Lean.Data.HashMap

import «Advent2023».Parse
import «Advent2023».Util

open Lean Parsec

namespace «2»

inductive Color
  | blue
  | red
  | green
deriving Repr, BEq, Hashable

abbrev Partial := Lean.HashMap Color Nat

structure Game where
  id : Nat
  parts : Array Partial

namespace Parse

  def color : Parsec Color
    := (skipString "blue" *> pure Color.blue)
    <|> (skipString "red" *> pure Color.red)
    <|> (skipString "green" *> pure Color.green)

  def part : Parsec (Array (Nat × Color)) := flip Parse.sepBy1 ", " $ do
    let n <- Parse.nat
    skipString " "
    let c <- color
    pure (n, c)

  def collect (parts : Array (Nat × Color)) : Partial :=
    parts.map (fun (n,c) => (c,n))
    |> Array.toList
    |> Lean.HashMap.ofList

  def game : Parsec Game := do
    skipString "Game "
    let id <- Parse.nat
    skipString ": "
    let parts <- Parse.sepBy1 (collect <$> part) "; "
    pure $ Game.mk id parts

end Parse

def totals (game : Game) (color : Color) :=
  game.parts.toList.map (fun p => p.findD color 0)
  |> List.maximum?
  |> flip Option.getD 0

def possible (game : Game) : Bool :=
  (totals game Color.red <= 12) &&
  (totals game Color.green <= 13) &&
  (totals game Color.blue <= 14)

def power (game : Game) : Nat :=
  #[Color.red, Color.green, Color.blue].map (totals game)
  |> Array.foldl Nat.mul 1

def solve (input : System.FilePath) : IO Unit := do
  let games <- Parse.input Parse.game input

  -- 2632
  IO.println $ games.filter possible |> List.map Game.id |> sum

  -- 69629
  IO.println $ games.map power |> sum

end «2»
