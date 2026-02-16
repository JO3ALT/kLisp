% 4x4 Sudoku solver for the current Prolog subset
% Board cells:
% R1: C11 C12 C13 C14
% R2: C21 C22 C23 C24
% R3: C31 C32 C33 C34
% R4: C41 C42 C43 C44

digit(1).
digit(2).
digit(3).
digit(4).

neq(1,2). neq(1,3). neq(1,4).
neq(2,1). neq(2,3). neq(2,4).
neq(3,1). neq(3,2). neq(3,4).
neq(4,1). neq(4,2). neq(4,3).

all_diff4(A,B,C,D) :-
  neq(A,B), neq(A,C), neq(A,D),
  neq(B,C), neq(B,D),
  neq(C,D).

sudoku(C11,C12,C13,C14,
       C21,C22,C23,C24,
       C31,C32,C33,C34,
       C41,C42,C43,C44) :-
  digit(C11), digit(C12), digit(C13), digit(C14),
  digit(C21), digit(C22), digit(C23), digit(C24),
  digit(C31), digit(C32), digit(C33), digit(C34),
  digit(C41), digit(C42), digit(C43), digit(C44),

  all_diff4(C11,C12,C13,C14),
  all_diff4(C21,C22,C23,C24),
  all_diff4(C31,C32,C33,C34),
  all_diff4(C41,C42,C43,C44),

  all_diff4(C11,C21,C31,C41),
  all_diff4(C12,C22,C32,C42),
  all_diff4(C13,C23,C33,C43),
  all_diff4(C14,C24,C34,C44),

  all_diff4(C11,C12,C21,C22),
  all_diff4(C13,C14,C23,C24),
  all_diff4(C31,C32,C41,C42),
  all_diff4(C33,C34,C43,C44).

% Puzzle instance:
% 1 . . 4
% . 4 1 .
% . 1 4 .
% 4 . . 1
?- sudoku(1,B12,B13,4,
          B21,4,1,B24,
          B31,1,4,B34,
          4,B42,B43,1).
