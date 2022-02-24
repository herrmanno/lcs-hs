# Longest Common Subsequence
As probably requested by final exam HTWK's course 'Software Engineering C559-2'

## run
`runhaskell lcs.hs <from> <to>`

### Example
```
❯ runhaskell lcs.hs ABACA BBAAC

      A  B  A  C  A
   0 ←0 ←0 ←0 ←0 ←0
B ↑0 ↑0 ↖1 ←1 ←1 ←1
B ↑0 ↑0 ↖1 ↑1 ↑1 ↑1
A ↑0 ↖1 ↑1 ↖2 ←2 ↖2
A ↑0 ↖1 ↑1 ↖2 ↑2 ↖3
C ↑0 ↑1 ↑1 ↑2 ↖3 ↑3

insertAfter(5,C)
delete(4)
replace(1,B)
```
