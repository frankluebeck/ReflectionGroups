gap> W := ReflectionGroup([(1,2),(2,3),(3,4)]);; 
gap> Reflections(W);
[ (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) ]
gap> GeneratingReflections(W);
[ (1,2), (2,3), (3,4) ]
gap> OrdersGeneratingReflections(W);
[ 2, 2, 2 ]
gap> NrGeneratingReflections(W);
3

# repeated and unnecessary generators
gap> W := ReflectionGroup([(1,2),(2,3),(3,4),(2,3),(2,4)]);; 
gap> Reflections(W);
[ (1,2), (2,3), (3,4), (2,3), (2,4), (1,3), (1,4) ]
gap> GeneratingReflections(W);
[ (1,2), (2,3), (3,4), (2,3), (2,4) ]
gap> OrdersGeneratingReflections(W);
[ 2, 2, 2, 2, 2 ]
gap> NrGeneratingReflections(W);
5

# corner case of no generators
gap> e := TrivialGroup();;
gap> Setter(GeneratingReflections)(e, []);
gap> Reflections(e);
[  ]
gap> ep := Group(());;
gap> SetGeneratingReflections(ep, []);
gap> Reflections(ep);
[  ]

# nonsense, but no check
gap> bad := ReflectionGroup([(2,3,4,5),(1,2,3,4)]);
Group([ (2,3,4,5), (1,2,3,4) ])
gap> Reflections(bad);
[ (2,3,4,5), (1,2,3,4), (2,4)(3,5), (2,5,4,3), (1,3)(2,4), (1,4,3,2), 
  (1,5,3,4), (1,3,4,5), (1,3)(4,5), (1,4,3,5), (1,4)(3,5), (1,5,4,3), 
  (1,2,4,5), (1,2,5,4), (1,4,5,2), (1,5,2,4), (1,4)(2,5), (1,5)(2,4), 
  (1,5,4,2), (1,2)(4,5), (1,4,2,5), (1,3,5,2), (1,5,2,3), (1,3,2,5), 
  (1,2,3,5), (1,5,3,2), (1,2,5,3), (1,5)(2,3), (1,2)(3,5), (1,3)(2,5), 
  (1,4,2,3), (2,4,5,3), (2,5,3,4), (2,4,3,5), (1,3,4,2), (1,2,4,3), 
  (1,3,2,4), (2,3,5,4), (1,2)(3,4), (2,5)(3,4), (2,3)(4,5), (1,4)(2,3), 
  (1,3,5,4), (1,4,5,3), (1,5)(3,4) ]
gap> W := ReflectionGroup([(1,2),(2,3),(3,4)]);;
gap> ChangeLabelsGeneratingReflections(W, "abc");
gap> ChangeLabelsReflections(W, [1..Length(Reflections(W))]+100);
gap> LabelsGeneratingReflections(W);
[ 101, 102, 103 ]
gap> g := ReflectionGroup([[[3,-2],[4,-3]], [[1,0],[0,-1]]]);
Group([ [ [ 3, -2 ], [ 4, -3 ] ], [ [ 1, 0 ], [ 0, -1 ] ] ])
gap> CanGeneratingReflections(g);
true
gap> GeneratingReflections(g);
[ [ [ 3, -2 ], [ 4, -3 ] ], [ [ 1, 0 ], [ 0, -1 ] ] ]

#gap> Reflections(g);   # infinite loop
gap> W := ReflectionGroup([(1,2), (2,3), (3,4)]);;
gap> U := ReflectionSubgroupByLabels(W, [2, 3]);; Reflections(U);
[ (2,3), (3,4), (2,4) ]
gap> ReflectionsInclusion(U);  
[ 2, 3, 5 ]
gap> GeneratingReflectionsInclusion(U);
[ 2, 3 ]
gap> ReflectionsRestriction(U);
[ , 1, 2,, 3 ]
gap> g := ReflectionGroup([[[3,-2],[4,-3]], [[1,0],[0,-1]]]);;
gap> u := ReflectionSubgroupByPositions(g, [1]);
Group([ [ [ 3, -2 ], [ 4, -3 ] ] ])
gap> GeneratingReflectionsInclusion(u);
[ 1 ]
gap> Reflections(u);
[ [ [ 3, -2 ], [ 4, -3 ] ] ]
gap> u := ReflectionSubgroupByLabels(g, []);
Group([  ])
gap> Reflections(u);
[  ]
gap> GeneratingReflectionsInclusion(u);
[  ]
