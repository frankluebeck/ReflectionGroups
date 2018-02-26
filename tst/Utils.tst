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
gap> perms := [(1,2,4),(4,5,6)];;
gap> ow := OrbitsAndWordsPerms(perms, [6,5..1], []);
[ [ [ 6, 0, 0 ], [ 4, 2, 6 ], [ 1, 1, 4 ], [ 5, 2, 4 ], [ 2, 1, 1 ] ], 
  [ [ 3, 0, 0 ] ] ]
gap> # as application we compute a transversal of the first orbit
gap> tr := [];; tr[ow[1][1][1]] := ();;
gap> for i in [2..Length(ow[1])] do 
>      a := ow[1][i]; tr[a[1]] := tr[a[3]]*perms[a[2]];
>    od;
gap> tr;
[ (1,2,4,5,6), (1,4,5,6,2),, (4,5,6), (4,6,5), () ]
gap> LinearlyIndependentRows([]);
[  ]
gap> mat := [[0,0,0],[1,1,1],[1,2,3],[2,3,4]];;
gap> LinearlyIndependentRows(mat);
[ 2, 3 ]
gap> LinearlyIndependentRows(mat, 1);
[ 2 ]
gap> LinearlyIndependentRows(mat, 0);
[  ]
gap> LinearlyIndependentRows(mat, 4);
[ 2, 3 ]
gap> mat := [[0,0,0],[1,1,1],[1,2,3],[2,3,4]];;
gap> seb := SEBasis(mat);
rec( heads := [ 1, 2, 0 ], pivots := [ 1, 2 ], 
  vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ] ] )
gap> AddToSEBasis(seb, [1,2,4]);
true
gap> AddToSEBasis(seb, [1,2,3]);
false
gap> seb;
rec( heads := [ 1, 2, 3 ], pivots := [ 1, 2, 3 ], 
  vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ], [ 0, 0, 1 ] ] )
gap> gens := [ [[0,1,-1],[0,1,0],[-1,0,2]], [[-2,1,0],[1,-4,1],[0,-3,1]] ];;
gap> SpinVectors(gens, mat);
rec( heads := [ 1, 2, 3 ], pivots := [ 1, 2, 3 ], 
  vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ], [ 0, 0, 1 ] ] )
gap> mat := [[1,0,1,0,1],[0,0,1,1,1],[2,0,0,1,4]];
[ [ 1, 0, 1, 0, 1 ], [ 0, 0, 1, 1, 1 ], [ 2, 0, 0, 1, 4 ] ]
gap> rimat := RightInverseMatrix(mat);
[ [ 1/3, -1/3, 1/3 ], [ 0, 0, 0 ], [ 2/3, 1/3, -1/3 ], [ -2/3, 2/3, 1/3 ], 
  [ 0, 0, 0 ] ]
gap> mat*rimat;
[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]
gap> mat := [[1,0,1,0,1],[0,0,1,1,1],[1,0,2,1,2]];
[ [ 1, 0, 1, 0, 1 ], [ 0, 0, 1, 1, 1 ], [ 1, 0, 2, 1, 2 ] ]
gap> RightInverseMatrix(mat);
fail
