def probs-opcode [word: string] : nothing -> bool {
  $word | parse --regex "^r?\\d+$" | is-empty
}

let supported_opcodes = [
  "add"
  "and"
  "asl"
  "asr"
  "cmp"
  "eor"
  "ldr"
  "lsl"
  "lsr"
  "mov"
  "mul"
  "nop"
  "orr"
  "r10"
  "r11"
  "r12"
  "r13"
  "r14"
  "r15"
  "ror"
  "rrx"
  "str"
  "sub"
  "sb"
  "sl"
  "fp"
  "ip"
  "sp"
  "lr"
  "pc"
];

def main [--only-unsupported (-u)] {
  ls lens-benchmarks/
  | where ($it.name | str ends-with ".s")
  | each {
    let name = $in.name;
    open $name
    | split words
    | sort
    | uniq
    | where (probs-opcode $it)
    | where not $only_unsupported or not ($it in $supported_opcodes)
    | { name: $name, opcodes: $in }
  }
}

def "main histogram" [--only-unsupported (-u)] {
  main --only-unsupported=$only_unsupported
  | get opcodes
  | flatten
  | sort
  | uniq --count
  | sort-by count
  | rename -c { value: opcode }
  | where not ($it.opcode in $supported_opcodes)
}
