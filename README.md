```
Syntax: iter-sched <filename>

Where filename contains lines that either start with 
4 spaces or ':[0-9]: ', denoting a scheduling item
in a scheduling group bearing that number.

Example input
:0: action_a1
:0: action_a2
:1: action_b2

Example output

Schedling order #1: [(0,1),(1,1),(0,1)]
--------------------------------------------------
:0: action_a1
:1: action_b2
:0: action_a2

Schedling order #2: [(0,2),(1,1)]
--------------------------------------------------
:0: action_a1
:0: action_a2
:1: action_b2

Schedling order #3: [(1,1),(0,2)]
--------------------------------------------------
:1: action_b2
:0: action_a1
:0: action_a2
```