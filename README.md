## Swiss Tournament Probability Computations

This tool provides probabilities for the records of top cut and near top cut
competitors in a swiss tournament that cuts to a single elimination phase.

### Reading the output

`pprintDist` will output a series of lines that look like the following:

```10.21%    (Rankings [(WinLoss 7 2,25),(WinLoss 8 1,7)],3)```

At the far left, there is a percentage that is the chance of this result happening.
In the middle, there is the distribution of records in top cut. In this case, we
had a tournament that cut to top 32, of which 7 players were 8-1 and 25 players were
7-2. Finally, we have the number of players with the same record as the worst record
in top cut that didn't make it. In this case, 3 players who were 7-2 did not make
top cut.
