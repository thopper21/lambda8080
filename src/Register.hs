module Register
  ( Register8, Register16
  ) where

data Register8
  = A
  | B
  | C
  | D
  | E
  | H
  | L

data Register16
  = BC
  | DE
  | HL
  | PC
  | SP
