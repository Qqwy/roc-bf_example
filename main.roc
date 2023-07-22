app "brainroc"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        # pf.File,
        # pf.Path,
        pf.Stdout,
        # pf.Task,
    ]
    provides [main] to pf

main =
  Stdout.line "Hello, world!"

tokenize : List U8 -> List Op
tokenize = \bytes ->
  beginState = (0, [], [])
  (_pc, instructions, _jumpStack) = List.walk bytes beginState tokenizeOne
  instructions

tokenizeOne = \(pc, instructions, jumpStack), char ->
  when char is
    '>' ->
      instructions2 = List.append instructions Next
      (pc + 1, instructions2, jumpStack)

    '<' ->
      instructions2 = List.append instructions Prev
      (pc + 1, instructions2, jumpStack)

    '+' ->
      instructions2 = List.append instructions Inc
      (pc + 1, instructions2, jumpStack)

    '-' ->
      instructions2 = List.append instructions Dec
      (pc + 1, instructions2, jumpStack)

    '.' ->
      instructions2 = List.append instructions Output
      (pc + 1, instructions2, jumpStack)

    ',' ->
      instructions2 = List.append instructions Input
      (pc + 1, instructions2, jumpStack)

    '[' ->
      instructions2 = List.append instructions JumpForward
      jumpStack2 = List.append jumpStack pc
      (pc + 1, instructions2, jumpStack2)

    ']' ->
      when pop jumpStack is
        Err _ -> crash "Malformed BF program"
        Ok (location, jumpStack2) ->
          instructions2 = List.append instructions (JumpBackward location)
          (pc + 1, instructions, jumpStack2)

    ' ' | '\n' | '\t' ->
      (pc, instructions, jumpStack)

    other ->
      when Str.fromUtf8 [ other ] is
          Ok str -> 
              crash "unexpected character: '\(str)'" 

          Err _ -> 
              crash "unexpected non-utf8 character"  


pop : List a -> Result (a, List a) [ListWasEmpty]
pop = \list ->
  when List.last list is
    Err x -> Err x
    Ok elem ->
      rest = List.dropLast list
      Ok (elem, rest)

Op : [
  Next,
  Prev,
  Inc,
  Dec,
  Input,
  Output,
  JumpForward,
  JumpBackward Nat,
]

