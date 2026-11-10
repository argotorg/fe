A recv arm field whose pattern destructures, such as `V { t: (x, y) }` or `V { pair: Pair { n, .. } }`, binds what the pattern names; before, compiling the arm failed with internal error 16-0004.
