# Calculate First and Follow sets for a CFG

Running the following in a repl will give the first set of non-terminal symbol
"D" in the following grammar. Calling the `followSet` function instead will
return the follow set of "D".

```haskell
firstSet (Grammar [
            Rule { lhs = (N"A"), rhs = [[N"B", N"C"], [N"B", N"C", N"D", T"w"]] },
            Rule { lhs = (N"B"), rhs = [[T"y"], [Epsilon]] },
            Rule { lhs = (N"C"), rhs = [[T"x"], [Epsilon]] },
            Rule { lhs = (N"D"), rhs = [[T"z"]] }
          ]
         ) (N "D")
```
