% Tiny rule-based EN->JA sample (pattern: svo/3)
subj_ja(i,watashi).
subj_ja(you,anata).
verb_ja(like,suki).
verb_ja(eat,taberu).
obj_ja(apples,ringo).
obj_ja(sushi,sushi).

translate(svo(S,V,O), ja(SJ,OJ,VJ)) :-
  subj_ja(S,SJ),
  verb_ja(V,VJ),
  obj_ja(O,OJ).

?- translate(svo(i,like,apples),J).
