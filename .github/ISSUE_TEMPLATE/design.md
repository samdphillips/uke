---
name: Design proposal
about: Propose an API shape or semantic change that needs discussion before implementation
title: "[Design] "
labels: design, needs-design-decision
---

## Problem

What limitation, gap, or awkwardness are you trying to address?
(e.g. "There's is no easy way to sort a dataframe or column.")

## Proposed API shape

Sketch the interface as you imagine it — function signatures, example usage,
or a short code snippet. Doesn't need to be final.

```racket
;; example
(dataframe-sort df '(col-a col-b))
```

## Semantics / edge cases

- What happens with missing columns?
- Any interaction with immutability guarantees?

## Alternatives considered

Other shapes you thought about and why you didn't go with them (even briefly).

## Affected components

- [ ] uke-lib
- [ ] uke-extra-lib
- [ ] uke-machete

## Breaking change?

- [ ] Yes
- [ ] No
