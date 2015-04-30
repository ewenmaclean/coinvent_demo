#program base.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spec Emaj7 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec(spec_Emaj7).
hasId(spec_Emaj7,0).

%% sorts %%
hasSort(spec_Emaj7,sort_Chord,0).
isNonDataSort(spec_Emaj7,sort_Chord).
sortHasPriority(spec_Emaj7,sort_Chord,1).

hasSort(spec_Emaj7,sort_Note,0).
isDataSort(spec_Emaj7,sort_Note).
sortHasPriority(spec_Emaj7,sort_Note,1).

%% operators %%
hasOp(spec_Emaj7,po_0,0).
opHasSort(spec_Emaj7,po_0,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_0).
opHasPriority(spec_Emaj7,po_0,1).

hasOp(spec_Emaj7,po_1,0).
opHasSort(spec_Emaj7,po_1,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_1).
opHasPriority(spec_Emaj7,po_1,1).

hasOp(spec_Emaj7,po_2,0).
opHasSort(spec_Emaj7,po_2,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_2).
opHasPriority(spec_Emaj7,po_2,1).

hasOp(spec_Emaj7,po_3,0).
opHasSort(spec_Emaj7,po_3,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_3).
opHasPriority(spec_Emaj7,po_3,1).

hasOp(spec_Emaj7,po_4,0).
opHasSort(spec_Emaj7,po_4,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_4).
opHasPriority(spec_Emaj7,po_4,1).

hasOp(spec_Emaj7,po_5,0).
opHasSort(spec_Emaj7,po_5,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_5).
opHasPriority(spec_Emaj7,po_5,1).

hasOp(spec_Emaj7,po_6,0).
opHasSort(spec_Emaj7,po_6,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_6).
opHasPriority(spec_Emaj7,po_6,1).

hasOp(spec_Emaj7,po_7,0).
opHasSort(spec_Emaj7,po_7,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_7).
opHasPriority(spec_Emaj7,po_7,1).

hasOp(spec_Emaj7,po_8,0).
opHasSort(spec_Emaj7,po_8,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_8).
opHasPriority(spec_Emaj7,po_8,1).

hasOp(spec_Emaj7,po_9,0).
opHasSort(spec_Emaj7,po_9,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_9).
opHasPriority(spec_Emaj7,po_9,1).

hasOp(spec_Emaj7,po_plus,0).
opHasSort(spec_Emaj7,po_plus,sort_Note,arg1,0).
opHasSort(spec_Emaj7,po_plus,sort_Note,arg2,0).
opHasSort(spec_Emaj7,po_plus,sort_Note,domain,0).
isNonDataOp(spec_Emaj7,po_plus).
opHasPriority(spec_Emaj7,po_plus,1).

hasOp(spec_Emaj7,po_c,0).
opHasSort(spec_Emaj7,po_c,sort_Chord,domain,0).
isNonDataOp(spec_Emaj7,po_c).
opHasPriority(spec_Emaj7,po_c,1).

hasOp(spec_Emaj7,po_root,0).
opHasSort(spec_Emaj7,po_root,sort_Chord,arg1,0).
opHasSort(spec_Emaj7,po_root,sort_Note,domain,0).
isNonDataOp(spec_Emaj7,po_root).
opHasPriority(spec_Emaj7,po_root,1).

hasOp(spec_Emaj7,po_suc,0).
opHasSort(spec_Emaj7,po_suc,sort_Note,arg1,0).
opHasSort(spec_Emaj7,po_suc,sort_Note,domain,0).
isNonDataOp(spec_Emaj7,po_suc).
opHasPriority(spec_Emaj7,po_suc,1).

hasOp(spec_Emaj7,po_x,0).
opHasSort(spec_Emaj7,po_x,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_x).
opHasPriority(spec_Emaj7,po_x,1).

hasOp(spec_Emaj7,po_x1,0).
opHasSort(spec_Emaj7,po_x1,sort_Note,domain,0).
isDataOp(spec_Emaj7,po_x1).
opHasPriority(spec_Emaj7,po_x1,1).

%% predicates %%
hasPred(spec_Emaj7,po_hasAbsNote,0).
predHasSort(spec_Emaj7,po_hasAbsNote,sort_Chord,arg1,0).
predHasSort(spec_Emaj7,po_hasAbsNote,sort_Note,arg2,0).
predHasPriority(spec_Emaj7,po_hasAbsNote,1).

hasPred(spec_Emaj7,po_hasRelNote,0).
predHasSort(spec_Emaj7,po_hasRelNote,sort_Chord,arg1,0).
predHasSort(spec_Emaj7,po_hasRelNote,sort_Note,arg2,0).
predHasPriority(spec_Emaj7,po_hasRelNote,1).

%% axioms %%

%% Axiom Ax-abse4:p:2 %%
hasAxiom(spec_Emaj7,0,0).
axHasEquivalenceClass(spec_Emaj7,0,0,0).
isNonDataAx(spec_Emaj7,0).
axHasPriority(spec_Emaj7,0,2).
axInvolvesPredOp(spec_Emaj7,0,po_c,0).
axInvolvesPredOp(spec_Emaj7,0,po_4,0).
axInvolvesPredOp(spec_Emaj7,0,po_hasAbsNote,0).


%% Axiom Ax-abse8:p:3 %%
hasAxiom(spec_Emaj7,1,0).
axHasEquivalenceClass(spec_Emaj7,1,1,0).
isNonDataAx(spec_Emaj7,1).
axHasPriority(spec_Emaj7,1,3).
axInvolvesPredOp(spec_Emaj7,1,po_8,0).
axInvolvesPredOp(spec_Emaj7,1,po_c,0).
axInvolvesPredOp(spec_Emaj7,1,po_hasAbsNote,0).


%% Axiom Ax-abse3:p:1 %%
hasAxiom(spec_Emaj7,2,0).
axHasEquivalenceClass(spec_Emaj7,2,2,0).
isNonDataAx(spec_Emaj7,2).
axHasPriority(spec_Emaj7,2,1).
axInvolvesPredOp(spec_Emaj7,2,po_c,0).
axInvolvesPredOp(spec_Emaj7,2,po_3,0).
axInvolvesPredOp(spec_Emaj7,2,po_hasAbsNote,0).


%% Axiom Ax-roote4:p:2 %%
hasAxiom(spec_Emaj7,3,0).
axHasEquivalenceClass(spec_Emaj7,3,3,0).
isNonDataAx(spec_Emaj7,3).
axHasPriority(spec_Emaj7,3,2).
axInvolvesPredOp(spec_Emaj7,3,po_c,0).
axInvolvesPredOp(spec_Emaj7,3,po_root,0).
axInvolvesPredOp(spec_Emaj7,3,po_4,0).


%% Axiom Ax-rele0:p:3 %%
hasAxiom(spec_Emaj7,4,0).
axHasEquivalenceClass(spec_Emaj7,4,4,0).
isNonDataAx(spec_Emaj7,4).
axHasPriority(spec_Emaj7,4,3).
axInvolvesPredOp(spec_Emaj7,4,po_0,0).
axInvolvesPredOp(spec_Emaj7,4,po_c,0).
axInvolvesPredOp(spec_Emaj7,4,po_hasRelNote,0).


%% Axiom Ax-rele4:p:3 %%
hasAxiom(spec_Emaj7,5,0).
axHasEquivalenceClass(spec_Emaj7,5,5,0).
isNonDataAx(spec_Emaj7,5).
axHasPriority(spec_Emaj7,5,3).
axInvolvesPredOp(spec_Emaj7,5,po_c,0).
axInvolvesPredOp(spec_Emaj7,5,po_4,0).
axInvolvesPredOp(spec_Emaj7,5,po_hasRelNote,0).


%% Axiom Ax-rele11:p:1 %%
hasAxiom(spec_Emaj7,6,0).
axHasEquivalenceClass(spec_Emaj7,6,6,0).
isNonDataAx(spec_Emaj7,6).
axHasPriority(spec_Emaj7,6,1).
axInvolvesPredOp(spec_Emaj7,6,po_c,0).
axInvolvesPredOp(spec_Emaj7,6,po_x1,0).
axInvolvesPredOp(spec_Emaj7,6,po_hasRelNote,0).


%% Axiom Ax6 %%
hasAxiom(spec_Emaj7,12,0).
axHasEquivalenceClass(spec_Emaj7,12,12,0).
isNonDataAx(spec_Emaj7,12).
axHasPriority(spec_Emaj7,12,1).
axInvolvesPredOp(spec_Emaj7,12,po_Chord,0).
axInvolvesPredOp(spec_Emaj7,12,po_hasAbsNote,0).
axInvolvesPredOp(spec_Emaj7,12,po_6,0).
axInvolvesPredOp(spec_Emaj7,12,po_1,0).
axInvolvesPredOp(spec_Emaj7,12,po_7,0).
axInvolvesPredOp(spec_Emaj7,12,po_hasRelNote,0).
axInvolvesPredOp(spec_Emaj7,12,po_var,0).
axInvolvesPredOp(spec_Emaj7,12,po_root,0).
axInvolvesSort(spec_Emaj7,12,sort_Chord,0).


%% Axiom AxSucc0:p:100:r:0 %%
hasAxiom(spec_Emaj7,18,0).
axHasEquivalenceClass(spec_Emaj7,18,18,0).
isNonDataAx(spec_Emaj7,18).
axHasPriority(spec_Emaj7,18,100).
axInvolvesPredOp(spec_Emaj7,18,po_1,0).
axInvolvesPredOp(spec_Emaj7,18,po_0,0).
axInvolvesPredOp(spec_Emaj7,18,po_suc,0).


%% Axiom AxSucc1:p:100:r:0 %%
hasAxiom(spec_Emaj7,19,0).
axHasEquivalenceClass(spec_Emaj7,19,19,0).
isNonDataAx(spec_Emaj7,19).
axHasPriority(spec_Emaj7,19,100).
axInvolvesPredOp(spec_Emaj7,19,po_1,0).
axInvolvesPredOp(spec_Emaj7,19,po_2,0).
axInvolvesPredOp(spec_Emaj7,19,po_suc,0).


%% Axiom AxSucc2:p:100:r:0 %%
hasAxiom(spec_Emaj7,20,0).
axHasEquivalenceClass(spec_Emaj7,20,20,0).
isNonDataAx(spec_Emaj7,20).
axHasPriority(spec_Emaj7,20,100).
axInvolvesPredOp(spec_Emaj7,20,po_3,0).
axInvolvesPredOp(spec_Emaj7,20,po_2,0).
axInvolvesPredOp(spec_Emaj7,20,po_suc,0).


%% Axiom AxSucc3:p:100:r:0 %%
hasAxiom(spec_Emaj7,21,0).
axHasEquivalenceClass(spec_Emaj7,21,21,0).
isNonDataAx(spec_Emaj7,21).
axHasPriority(spec_Emaj7,21,100).
axInvolvesPredOp(spec_Emaj7,21,po_3,0).
axInvolvesPredOp(spec_Emaj7,21,po_suc,0).
axInvolvesPredOp(spec_Emaj7,21,po_4,0).


%% Axiom AxSucc4:p:100:r:0 %%
hasAxiom(spec_Emaj7,22,0).
axHasEquivalenceClass(spec_Emaj7,22,22,0).
isNonDataAx(spec_Emaj7,22).
axHasPriority(spec_Emaj7,22,100).
axInvolvesPredOp(spec_Emaj7,22,po_5,0).
axInvolvesPredOp(spec_Emaj7,22,po_suc,0).
axInvolvesPredOp(spec_Emaj7,22,po_4,0).


%% Axiom AxSucc5:p:100:r:0 %%
hasAxiom(spec_Emaj7,23,0).
axHasEquivalenceClass(spec_Emaj7,23,23,0).
isNonDataAx(spec_Emaj7,23).
axHasPriority(spec_Emaj7,23,100).
axInvolvesPredOp(spec_Emaj7,23,po_5,0).
axInvolvesPredOp(spec_Emaj7,23,po_suc,0).
axInvolvesPredOp(spec_Emaj7,23,po_6,0).


%% Axiom AxSucc6:p:100:r:0 %%
hasAxiom(spec_Emaj7,24,0).
axHasEquivalenceClass(spec_Emaj7,24,24,0).
isNonDataAx(spec_Emaj7,24).
axHasPriority(spec_Emaj7,24,100).
axInvolvesPredOp(spec_Emaj7,24,po_suc,0).
axInvolvesPredOp(spec_Emaj7,24,po_7,0).
axInvolvesPredOp(spec_Emaj7,24,po_6,0).


%% Axiom AxSucc7:p:100:r:0 %%
hasAxiom(spec_Emaj7,25,0).
axHasEquivalenceClass(spec_Emaj7,25,25,0).
isNonDataAx(spec_Emaj7,25).
axHasPriority(spec_Emaj7,25,100).
axInvolvesPredOp(spec_Emaj7,25,po_8,0).
axInvolvesPredOp(spec_Emaj7,25,po_suc,0).
axInvolvesPredOp(spec_Emaj7,25,po_7,0).


%% Axiom AxSucc8:p:100:r:0 %%
hasAxiom(spec_Emaj7,26,0).
axHasEquivalenceClass(spec_Emaj7,26,26,0).
isNonDataAx(spec_Emaj7,26).
axHasPriority(spec_Emaj7,26,100).
axInvolvesPredOp(spec_Emaj7,26,po_9,0).
axInvolvesPredOp(spec_Emaj7,26,po_8,0).
axInvolvesPredOp(spec_Emaj7,26,po_suc,0).


%% Axiom AxSucc9:p:100:r:0 %%
hasAxiom(spec_Emaj7,27,0).
axHasEquivalenceClass(spec_Emaj7,27,27,0).
isNonDataAx(spec_Emaj7,27).
axHasPriority(spec_Emaj7,27,100).
axInvolvesPredOp(spec_Emaj7,27,po_9,0).
axInvolvesPredOp(spec_Emaj7,27,po_x,0).
axInvolvesPredOp(spec_Emaj7,27,po_suc,0).


%% Axiom AxSucc10:p:100:r:0 %%
hasAxiom(spec_Emaj7,28,0).
axHasEquivalenceClass(spec_Emaj7,28,28,0).
isNonDataAx(spec_Emaj7,28).
axHasPriority(spec_Emaj7,28,100).
axInvolvesPredOp(spec_Emaj7,28,po_x,0).
axInvolvesPredOp(spec_Emaj7,28,po_x1,0).
axInvolvesPredOp(spec_Emaj7,28,po_suc,0).


%% Axiom AxSucc11:p:100:r:0 %%
hasAxiom(spec_Emaj7,29,0).
axHasEquivalenceClass(spec_Emaj7,29,29,0).
isNonDataAx(spec_Emaj7,29).
axHasPriority(spec_Emaj7,29,100).
axInvolvesPredOp(spec_Emaj7,29,po_0,0).
axInvolvesPredOp(spec_Emaj7,29,po_x1,0).
axInvolvesPredOp(spec_Emaj7,29,po_suc,0).


%% Axiom AxSucc0-11:p:100:r:0 %%
hasAxiom(spec_Emaj7,41,0).
axHasEquivalenceClass(spec_Emaj7,41,41,0).
isNonDataAx(spec_Emaj7,41).
axHasPriority(spec_Emaj7,41,100).
axInvolvesPredOp(spec_Emaj7,41,po_var,0).
axInvolvesPredOp(spec_Emaj7,41,po_0,0).
axInvolvesPredOp(spec_Emaj7,41,po_x1,0).
axInvolvesPredOp(spec_Emaj7,41,po_suc,0).
axInvolvesPredOp(spec_Emaj7,41,po_Note,0).
axInvolvesSort(spec_Emaj7,41,sort_Note,0).


%% Axiom AxSuccEql:p:100:r:0 %%
hasAxiom(spec_Emaj7,42,0).
axHasEquivalenceClass(spec_Emaj7,42,42,0).
isNonDataAx(spec_Emaj7,42).
axHasPriority(spec_Emaj7,42,100).
axInvolvesPredOp(spec_Emaj7,42,po_var,0).
axInvolvesPredOp(spec_Emaj7,42,po_Note,0).
axInvolvesPredOp(spec_Emaj7,42,po_suc,0).
axInvolvesSort(spec_Emaj7,42,sort_Note,0).


%% Axiom AxSuccDontKnow:p:100:r:0 %%
hasAxiom(spec_Emaj7,43,0).
axHasEquivalenceClass(spec_Emaj7,43,43,0).
isNonDataAx(spec_Emaj7,43).
axHasPriority(spec_Emaj7,43,100).
axInvolvesPredOp(spec_Emaj7,43,po_var,0).
axInvolvesPredOp(spec_Emaj7,43,po_0,0).
axInvolvesPredOp(spec_Emaj7,43,po_plus,0).
axInvolvesPredOp(spec_Emaj7,43,po_Note,0).
axInvolvesSort(spec_Emaj7,43,sort_Note,0).


%% Axiom AxSuccAdd:p:100:r:0 %%
hasAxiom(spec_Emaj7,44,0).
axHasEquivalenceClass(spec_Emaj7,44,44,0).
isNonDataAx(spec_Emaj7,44).
axHasPriority(spec_Emaj7,44,100).
axInvolvesPredOp(spec_Emaj7,44,po_plus,0).
axInvolvesPredOp(spec_Emaj7,44,po_suc,0).
axInvolvesSort(spec_Emaj7,44,sort_Note,0).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% spec Fmin7 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec(spec_Fmin7).
hasId(spec_Fmin7,1).

%% sorts %%
hasSort(spec_Fmin7,sort_Chord,0).
isNonDataSort(spec_Fmin7,sort_Chord).
sortHasPriority(spec_Fmin7,sort_Chord,1).

hasSort(spec_Fmin7,sort_Note,0).
isDataSort(spec_Fmin7,sort_Note).
sortHasPriority(spec_Fmin7,sort_Note,1).

%% operators %%
hasOp(spec_Fmin7,po_0,0).
opHasSort(spec_Fmin7,po_0,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_0).
opHasPriority(spec_Fmin7,po_0,1).

hasOp(spec_Fmin7,po_1,0).
opHasSort(spec_Fmin7,po_1,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_1).
opHasPriority(spec_Fmin7,po_1,1).

hasOp(spec_Fmin7,po_2,0).
opHasSort(spec_Fmin7,po_2,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_2).
opHasPriority(spec_Fmin7,po_2,1).

hasOp(spec_Fmin7,po_3,0).
opHasSort(spec_Fmin7,po_3,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_3).
opHasPriority(spec_Fmin7,po_3,1).

hasOp(spec_Fmin7,po_4,0).
opHasSort(spec_Fmin7,po_4,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_4).
opHasPriority(spec_Fmin7,po_4,1).

hasOp(spec_Fmin7,po_5,0).
opHasSort(spec_Fmin7,po_5,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_5).
opHasPriority(spec_Fmin7,po_5,1).

hasOp(spec_Fmin7,po_6,0).
opHasSort(spec_Fmin7,po_6,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_6).
opHasPriority(spec_Fmin7,po_6,1).

hasOp(spec_Fmin7,po_7,0).
opHasSort(spec_Fmin7,po_7,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_7).
opHasPriority(spec_Fmin7,po_7,1).

hasOp(spec_Fmin7,po_8,0).
opHasSort(spec_Fmin7,po_8,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_8).
opHasPriority(spec_Fmin7,po_8,1).

hasOp(spec_Fmin7,po_9,0).
opHasSort(spec_Fmin7,po_9,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_9).
opHasPriority(spec_Fmin7,po_9,1).

hasOp(spec_Fmin7,po_plus,0).
opHasSort(spec_Fmin7,po_plus,sort_Note,arg1,0).
opHasSort(spec_Fmin7,po_plus,sort_Note,arg2,0).
opHasSort(spec_Fmin7,po_plus,sort_Note,domain,0).
isNonDataOp(spec_Fmin7,po_plus).
opHasPriority(spec_Fmin7,po_plus,1).

hasOp(spec_Fmin7,po_c,0).
opHasSort(spec_Fmin7,po_c,sort_Chord,domain,0).
isNonDataOp(spec_Fmin7,po_c).
opHasPriority(spec_Fmin7,po_c,1).

hasOp(spec_Fmin7,po_root,0).
opHasSort(spec_Fmin7,po_root,sort_Chord,arg1,0).
opHasSort(spec_Fmin7,po_root,sort_Note,domain,0).
isNonDataOp(spec_Fmin7,po_root).
opHasPriority(spec_Fmin7,po_root,1).

hasOp(spec_Fmin7,po_suc,0).
opHasSort(spec_Fmin7,po_suc,sort_Note,arg1,0).
opHasSort(spec_Fmin7,po_suc,sort_Note,domain,0).
isNonDataOp(spec_Fmin7,po_suc).
opHasPriority(spec_Fmin7,po_suc,1).

hasOp(spec_Fmin7,po_x,0).
opHasSort(spec_Fmin7,po_x,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_x).
opHasPriority(spec_Fmin7,po_x,1).

hasOp(spec_Fmin7,po_x1,0).
opHasSort(spec_Fmin7,po_x1,sort_Note,domain,0).
isDataOp(spec_Fmin7,po_x1).
opHasPriority(spec_Fmin7,po_x1,1).

%% predicates %%
hasPred(spec_Fmin7,po_hasAbsNote,0).
predHasSort(spec_Fmin7,po_hasAbsNote,sort_Chord,arg1,0).
predHasSort(spec_Fmin7,po_hasAbsNote,sort_Note,arg2,0).
predHasPriority(spec_Fmin7,po_hasAbsNote,1).

hasPred(spec_Fmin7,po_hasRelNote,0).
predHasSort(spec_Fmin7,po_hasRelNote,sort_Chord,arg1,0).
predHasSort(spec_Fmin7,po_hasRelNote,sort_Note,arg2,0).
predHasPriority(spec_Fmin7,po_hasRelNote,1).

%% axioms %%

%% Axiom Ax-absf5:p:1 %%
hasAxiom(spec_Fmin7,45,0).
axHasEquivalenceClass(spec_Fmin7,45,45,0).
isNonDataAx(spec_Fmin7,45).
axHasPriority(spec_Fmin7,45,1).
axInvolvesPredOp(spec_Fmin7,45,po_c,0).
axInvolvesPredOp(spec_Fmin7,45,po_5,0).
axInvolvesPredOp(spec_Fmin7,45,po_hasAbsNote,0).


%% Axiom Ax-absf8:p:3 %%
hasAxiom(spec_Fmin7,46,0).
axHasEquivalenceClass(spec_Fmin7,46,1,0).
isNonDataAx(spec_Fmin7,46).
axHasPriority(spec_Fmin7,46,3).
axInvolvesPredOp(spec_Fmin7,46,po_8,0).
axInvolvesPredOp(spec_Fmin7,46,po_c,0).
axInvolvesPredOp(spec_Fmin7,46,po_hasAbsNote,0).


%% Axiom Ax-absf0:p:2 %%
hasAxiom(spec_Fmin7,47,0).
axHasEquivalenceClass(spec_Fmin7,47,46,0).
isNonDataAx(spec_Fmin7,47).
axHasPriority(spec_Fmin7,47,2).
axInvolvesPredOp(spec_Fmin7,47,po_0,0).
axInvolvesPredOp(spec_Fmin7,47,po_c,0).
axInvolvesPredOp(spec_Fmin7,47,po_hasAbsNote,0).


%% Axiom Ax-absf3:p:2 %%
hasAxiom(spec_Fmin7,48,0).
axHasEquivalenceClass(spec_Fmin7,48,2,0).
isNonDataAx(spec_Fmin7,48).
axHasPriority(spec_Fmin7,48,2).
axInvolvesPredOp(spec_Fmin7,48,po_c,0).
axInvolvesPredOp(spec_Fmin7,48,po_3,0).
axInvolvesPredOp(spec_Fmin7,48,po_hasAbsNote,0).


%% Axiom Ax-rootf5:p:1 %%
hasAxiom(spec_Fmin7,49,0).
axHasEquivalenceClass(spec_Fmin7,49,47,0).
isNonDataAx(spec_Fmin7,49).
axHasPriority(spec_Fmin7,49,1).
axInvolvesPredOp(spec_Fmin7,49,po_5,0).
axInvolvesPredOp(spec_Fmin7,49,po_c,0).
axInvolvesPredOp(spec_Fmin7,49,po_root,0).


%% Axiom Ax-relf0:p:3 %%
hasAxiom(spec_Fmin7,50,0).
axHasEquivalenceClass(spec_Fmin7,50,4,0).
isNonDataAx(spec_Fmin7,50).
axHasPriority(spec_Fmin7,50,3).
axInvolvesPredOp(spec_Fmin7,50,po_0,0).
axInvolvesPredOp(spec_Fmin7,50,po_c,0).
axInvolvesPredOp(spec_Fmin7,50,po_hasRelNote,0).


%% Axiom Ax-relf3:p:2 %%
hasAxiom(spec_Fmin7,51,0).
axHasEquivalenceClass(spec_Fmin7,51,48,0).
isNonDataAx(spec_Fmin7,51).
axHasPriority(spec_Fmin7,51,2).
axInvolvesPredOp(spec_Fmin7,51,po_c,0).
axInvolvesPredOp(spec_Fmin7,51,po_3,0).
axInvolvesPredOp(spec_Fmin7,51,po_hasRelNote,0).


%% Axiom Ax-relf7:p:2 %%
hasAxiom(spec_Fmin7,52,0).
axHasEquivalenceClass(spec_Fmin7,52,49,0).
isNonDataAx(spec_Fmin7,52).
axHasPriority(spec_Fmin7,52,2).
axInvolvesPredOp(spec_Fmin7,52,po_c,0).
axInvolvesPredOp(spec_Fmin7,52,po_7,0).
axInvolvesPredOp(spec_Fmin7,52,po_hasRelNote,0).


%% Axiom Ax-relf10:p:1 %%
hasAxiom(spec_Fmin7,53,0).
axHasEquivalenceClass(spec_Fmin7,53,50,0).
isNonDataAx(spec_Fmin7,53).
axHasPriority(spec_Fmin7,53,1).
axInvolvesPredOp(spec_Fmin7,53,po_x,0).
axInvolvesPredOp(spec_Fmin7,53,po_c,0).
axInvolvesPredOp(spec_Fmin7,53,po_hasRelNote,0).


%% Axiom Ax6 %%
hasAxiom(spec_Fmin7,59,0).
axHasEquivalenceClass(spec_Fmin7,59,12,0).
isNonDataAx(spec_Fmin7,59).
axHasPriority(spec_Fmin7,59,1).
axInvolvesPredOp(spec_Fmin7,59,po_Chord,0).
axInvolvesPredOp(spec_Fmin7,59,po_hasAbsNote,0).
axInvolvesPredOp(spec_Fmin7,59,po_6,0).
axInvolvesPredOp(spec_Fmin7,59,po_1,0).
axInvolvesPredOp(spec_Fmin7,59,po_7,0).
axInvolvesPredOp(spec_Fmin7,59,po_hasRelNote,0).
axInvolvesPredOp(spec_Fmin7,59,po_var,0).
axInvolvesPredOp(spec_Fmin7,59,po_root,0).
axInvolvesSort(spec_Fmin7,59,sort_Chord,0).


%% Axiom AxSucc0:p:100:r:0 %%
hasAxiom(spec_Fmin7,65,0).
axHasEquivalenceClass(spec_Fmin7,65,18,0).
isNonDataAx(spec_Fmin7,65).
axHasPriority(spec_Fmin7,65,100).
axInvolvesPredOp(spec_Fmin7,65,po_1,0).
axInvolvesPredOp(spec_Fmin7,65,po_0,0).
axInvolvesPredOp(spec_Fmin7,65,po_suc,0).


%% Axiom AxSucc1:p:100:r:0 %%
hasAxiom(spec_Fmin7,66,0).
axHasEquivalenceClass(spec_Fmin7,66,19,0).
isNonDataAx(spec_Fmin7,66).
axHasPriority(spec_Fmin7,66,100).
axInvolvesPredOp(spec_Fmin7,66,po_1,0).
axInvolvesPredOp(spec_Fmin7,66,po_2,0).
axInvolvesPredOp(spec_Fmin7,66,po_suc,0).


%% Axiom AxSucc2:p:100:r:0 %%
hasAxiom(spec_Fmin7,67,0).
axHasEquivalenceClass(spec_Fmin7,67,20,0).
isNonDataAx(spec_Fmin7,67).
axHasPriority(spec_Fmin7,67,100).
axInvolvesPredOp(spec_Fmin7,67,po_3,0).
axInvolvesPredOp(spec_Fmin7,67,po_2,0).
axInvolvesPredOp(spec_Fmin7,67,po_suc,0).


%% Axiom AxSucc3:p:100:r:0 %%
hasAxiom(spec_Fmin7,68,0).
axHasEquivalenceClass(spec_Fmin7,68,21,0).
isNonDataAx(spec_Fmin7,68).
axHasPriority(spec_Fmin7,68,100).
axInvolvesPredOp(spec_Fmin7,68,po_3,0).
axInvolvesPredOp(spec_Fmin7,68,po_suc,0).
axInvolvesPredOp(spec_Fmin7,68,po_4,0).


%% Axiom AxSucc4:p:100:r:0 %%
hasAxiom(spec_Fmin7,69,0).
axHasEquivalenceClass(spec_Fmin7,69,22,0).
isNonDataAx(spec_Fmin7,69).
axHasPriority(spec_Fmin7,69,100).
axInvolvesPredOp(spec_Fmin7,69,po_5,0).
axInvolvesPredOp(spec_Fmin7,69,po_suc,0).
axInvolvesPredOp(spec_Fmin7,69,po_4,0).


%% Axiom AxSucc5:p:100:r:0 %%
hasAxiom(spec_Fmin7,70,0).
axHasEquivalenceClass(spec_Fmin7,70,23,0).
isNonDataAx(spec_Fmin7,70).
axHasPriority(spec_Fmin7,70,100).
axInvolvesPredOp(spec_Fmin7,70,po_5,0).
axInvolvesPredOp(spec_Fmin7,70,po_suc,0).
axInvolvesPredOp(spec_Fmin7,70,po_6,0).


%% Axiom AxSucc6:p:100:r:0 %%
hasAxiom(spec_Fmin7,71,0).
axHasEquivalenceClass(spec_Fmin7,71,24,0).
isNonDataAx(spec_Fmin7,71).
axHasPriority(spec_Fmin7,71,100).
axInvolvesPredOp(spec_Fmin7,71,po_suc,0).
axInvolvesPredOp(spec_Fmin7,71,po_7,0).
axInvolvesPredOp(spec_Fmin7,71,po_6,0).


%% Axiom AxSucc7:p:100:r:0 %%
hasAxiom(spec_Fmin7,72,0).
axHasEquivalenceClass(spec_Fmin7,72,25,0).
isNonDataAx(spec_Fmin7,72).
axHasPriority(spec_Fmin7,72,100).
axInvolvesPredOp(spec_Fmin7,72,po_8,0).
axInvolvesPredOp(spec_Fmin7,72,po_suc,0).
axInvolvesPredOp(spec_Fmin7,72,po_7,0).


%% Axiom AxSucc8:p:100:r:0 %%
hasAxiom(spec_Fmin7,73,0).
axHasEquivalenceClass(spec_Fmin7,73,26,0).
isNonDataAx(spec_Fmin7,73).
axHasPriority(spec_Fmin7,73,100).
axInvolvesPredOp(spec_Fmin7,73,po_9,0).
axInvolvesPredOp(spec_Fmin7,73,po_8,0).
axInvolvesPredOp(spec_Fmin7,73,po_suc,0).


%% Axiom AxSucc9:p:100:r:0 %%
hasAxiom(spec_Fmin7,74,0).
axHasEquivalenceClass(spec_Fmin7,74,27,0).
isNonDataAx(spec_Fmin7,74).
axHasPriority(spec_Fmin7,74,100).
axInvolvesPredOp(spec_Fmin7,74,po_9,0).
axInvolvesPredOp(spec_Fmin7,74,po_x,0).
axInvolvesPredOp(spec_Fmin7,74,po_suc,0).


%% Axiom AxSucc10:p:100:r:0 %%
hasAxiom(spec_Fmin7,75,0).
axHasEquivalenceClass(spec_Fmin7,75,28,0).
isNonDataAx(spec_Fmin7,75).
axHasPriority(spec_Fmin7,75,100).
axInvolvesPredOp(spec_Fmin7,75,po_x,0).
axInvolvesPredOp(spec_Fmin7,75,po_x1,0).
axInvolvesPredOp(spec_Fmin7,75,po_suc,0).


%% Axiom AxSucc11:p:100:r:0 %%
hasAxiom(spec_Fmin7,76,0).
axHasEquivalenceClass(spec_Fmin7,76,29,0).
isNonDataAx(spec_Fmin7,76).
axHasPriority(spec_Fmin7,76,100).
axInvolvesPredOp(spec_Fmin7,76,po_0,0).
axInvolvesPredOp(spec_Fmin7,76,po_x1,0).
axInvolvesPredOp(spec_Fmin7,76,po_suc,0).


%% Axiom AxSucc0-11:p:100:r:0 %%
hasAxiom(spec_Fmin7,88,0).
axHasEquivalenceClass(spec_Fmin7,88,41,0).
isNonDataAx(spec_Fmin7,88).
axHasPriority(spec_Fmin7,88,100).
axInvolvesPredOp(spec_Fmin7,88,po_var,0).
axInvolvesPredOp(spec_Fmin7,88,po_0,0).
axInvolvesPredOp(spec_Fmin7,88,po_x1,0).
axInvolvesPredOp(spec_Fmin7,88,po_suc,0).
axInvolvesPredOp(spec_Fmin7,88,po_Note,0).
axInvolvesSort(spec_Fmin7,88,sort_Note,0).


%% Axiom AxSuccEql:p:100:r:0 %%
hasAxiom(spec_Fmin7,89,0).
axHasEquivalenceClass(spec_Fmin7,89,42,0).
isNonDataAx(spec_Fmin7,89).
axHasPriority(spec_Fmin7,89,100).
axInvolvesPredOp(spec_Fmin7,89,po_var,0).
axInvolvesPredOp(spec_Fmin7,89,po_Note,0).
axInvolvesPredOp(spec_Fmin7,89,po_suc,0).
axInvolvesSort(spec_Fmin7,89,sort_Note,0).


%% Axiom AxSuccDontKnow:p:100:r:0 %%
hasAxiom(spec_Fmin7,90,0).
axHasEquivalenceClass(spec_Fmin7,90,43,0).
isNonDataAx(spec_Fmin7,90).
axHasPriority(spec_Fmin7,90,100).
axInvolvesPredOp(spec_Fmin7,90,po_var,0).
axInvolvesPredOp(spec_Fmin7,90,po_0,0).
axInvolvesPredOp(spec_Fmin7,90,po_plus,0).
axInvolvesPredOp(spec_Fmin7,90,po_Note,0).
axInvolvesSort(spec_Fmin7,90,sort_Note,0).


%% Axiom AxSuccAdd:p:100:r:0 %%
hasAxiom(spec_Fmin7,91,0).
axHasEquivalenceClass(spec_Fmin7,91,44,0).
isNonDataAx(spec_Fmin7,91).
axHasPriority(spec_Fmin7,91,100).
axInvolvesPredOp(spec_Fmin7,91,po_plus,0).
axInvolvesPredOp(spec_Fmin7,91,po_suc,0).
axInvolvesSort(spec_Fmin7,91,sort_Note,0).




