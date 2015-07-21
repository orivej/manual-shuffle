The purpose of this package is to help shuffle playing cards.

MANUAL-SHUFFLE(N) returns three values:

- how to distribute cards between heaps
- how to collect heaps into a deck
- the underlying permutation

## Example

To shuffle four cards:

     (manual-shuffle:manual-shuffle 4) ; returns:
     ; (1 2 2 1) ; heaps
     ; (1 2)     ; heap ordering
     ; (3 2 4 1) ; permutation

Let's enumerate original deck with letters A B C D.

According to (3 2 4 1), the final ordering will be C B D A.

To follow (1 2 2 1), we form the first heap with A, the second with B, put C on top of the second heap to form C B, and put D on top of the first heap to form D A.  (When shuffling decks of typical sizes, it is convenient to find the right heap by placing five heaps in a row.)

To follow (1 2), we take heap 1 with D A, then we put heap 2 on top of it to form C B D A.
