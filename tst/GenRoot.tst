gap> genrts := [[1,-1,0],[0,2,-1]];; gencrts := [[1,-1,0 ],[0,1,0]];;
gap> W := ReflectionGroupPermutingRoots(genrts, gencrts);
Group([ (1,3)(2,5)(6,8), (1,4)(2,6)(3,7) ])
gap> rts := [[1,-1,0],[0,1,-1]];; # roots and coroots of GL3(K)
gap> W := ReflectionGroupByRoots(rts, rts);
Group([ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
  [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 1, 0 ] ] ])
gap> CartanMatrix(W);                                              
[ [ 2, -1 ], [ -1, 2 ] ]
gap> SemisimpleRank(W);
2
gap> LieRank(W);
3
