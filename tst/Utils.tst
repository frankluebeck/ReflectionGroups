
gap> FullOrbits(SymmetricGroup(7),[2,3,4,3,1]);
[ 2, 3, 4, 3, 1, 5, 6, 7 ]
gap> v := MutableIdentityMat(2, GF(2));
[ <a GF2 vector of length 2>, <a GF2 vector of length 2> ]
gap> gens := GeneratorsOfGroup(SL(2,2));
[ <an immutable 2x2 matrix over GF2>, <an immutable 2x2 matrix over GF2> ]
gap> orbs := FullOrbits(gens, v);
[ <an immutable GF2 vector of length 2>, <an immutable GF2 vector of length 2>
    , <an immutable GF2 vector of length 2> ]
gap> orbs[1] = v[1];
true
gap> ForAll(orbs, x-> not IsMutable(x));
true
gap> FullOrbits([(1,2,3),(1,4)(2,3)], []);
[  ]
gap> FullOrbits([(1,2,3),(1,4)(2,3)], [1]);
[ 1, 2, 4, 3 ]
gap> FullOrbits(TrivialGroup(), [1]);
[ 1 ]
gap> FullOrbits([], [1]);
[ 1 ]
gap> FullOrbits([], []);
[  ]
