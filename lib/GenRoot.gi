############################################################################
##  
#W  GenRoot.gi                                                  Frank Lübeck
##  
##  The files prg/GenRoot.g{d,i} contain basic functions for roots, coroots,
##  reflections and  reflection groups  which have an  associated reflection
##  representation.
##  

#################  basics for reflection matrices ######################
##  
##  - coroot from root and order of reflection (arg[2] pos. integer)
##    coroot = (1-E(order)) / norm(root) * ComplConj(root)
##  - or the eigenvalue zeta is given as arg[2], then
##    coroot = (1-zeta) / norm(root) * ComplConj(root)
##  - or arg[2] = r/s is positive rational, this is interpreted as
##    zeta = E(s)^r
##  Above we need `ComplexConj(root)' since we don't interpret this
##  as argument for the standard scalar product but as element of the
##  dual space wrt. the dual basis.
##  
InstallGlobalFunction(ReflectionEigenvalue, function(a)
  if IsInt(a) and a>0 then
    return E(a);
  elif IsRat(a) and a>0 then
    return E(DenominatorRat(a))^NumeratorRat(a);
  else
    return a;
  fi;
end);

############################################################################
##  
##  <#GAPDoc Label="DistinguishedCoroot">
##  <Subsection Label="ss:DistRefl">
##  <Heading>Distinguished Reflections and Coroots</Heading>
##  The following  notion is  only needed for  complex reflections  of order
##  larger than two.<P/>
##  We  call  a  reflection <Emph>distinguished</Emph>  if  the  non-trivial
##  eigenvalue  is of  the  form  <C>E(d)</C> where  <C>d</C>  is the  order
##  of  the  reflection.   Let  a  reflection  <M>s</M>  be   defined  by  a
##  root  <M>r</M>  and coroot  <M>r^\vee</M>.  Then  we call  <M>r^\vee</M>
##  <Emph>distinguished</Emph>  (with respect  to <M>r</M>)  if <M>s</M>  is
##  distinguished.<P/>
##  There  is always  a unique  scalar  multiple of  <M>r^\vee</M> which  is
##  distinguished and  determines with <M>r</M> a  reflection that generates
##  the same cyclic group as <M>s</M>.
##  </Subsection>
##  
##  <ManSection >
##  <Heading>Computing Coroots</Heading>
##  <Oper Arg="root[, eigenvalue]" Name="Coroot" />
##  <Oper Arg="root, coroot" Name="DistinguishedCoroot" />
##  <Returns>coroot as vector</Returns>
##  <Description>
##  The optional argument <Arg>eigenvalue</Arg> can be specified in any of
##  the formats explained in <Ref Subsect="ss:SpecEigenValRefl" />. Its
##  default value is <C>-1</C>.<P/>
##  
##  The   first  operation   <Ref  Oper="Coroot"   />  returns   the  coroot
##  corresponding to the given <Arg>root</Arg> and <Arg>eigenvalue</Arg>, as
##  explained in <Ref Subsect="ss:ReflRootCoroot"  />.<P/>
##  
##  The operation <Ref Oper="DistinguishedCoroot" /> returns the multiple of
##  the  given <Arg>coroot</Arg>  which  is distinguished  and defines  with
##  <Arg>root</Arg> a reflection of the same  order (it is a primitive power
##  of the reflection defined by <Arg>root</Arg> and <Arg>coroot</Arg>).
##  
##  <Example> 
##  gap> r := [1, 1];   
##  [ 1, 1 ]
##  gap> Coroot(r);
##  [ 1, 1 ]
##  gap> cr := Coroot(r, 2/3);
##  [ -1/2*E(3)-E(3)^2, -1/2*E(3)-E(3)^2 ]
##  gap> dcr := DistinguishedCoroot(r, cr);
##  [ -E(3)-1/2*E(3)^2, -E(3)-1/2*E(3)^2 ]
##  gap> dcr = (1+E(3)^2) * cr;
##  true
##  </Example> 
##  </Description> 
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(Coroot, ["IsVector", "IsCyc"], function(root, a)
  local   conjroot, zeta;
  zeta := ReflectionEigenvalue(a);
  conjroot := ComplexConjugate(root);
  return (1-zeta)/ (root*conjroot) * conjroot;
end);
##  default order is two, resp. eigenvalue is -1
InstallMethod(Coroot, ["IsVector"], function(root)
  return Coroot(root, -1);
end);
InstallMethod(DistinguishedCoroot, ["IsVector", "IsVector"], 
function(root, coroot) 
  local zeta, want, c, ev;
  zeta := 1 - coroot*root;
  want := E(Order(zeta));
  if zeta = want then
    return coroot;
  fi;
  c := 1;
  ev := zeta;
  while ev <> want do
    ev := ev*zeta;
    c := 1 + c*zeta;
  od;
  return c * coroot;
end);
InstallMethod(DistinguishedCoroot, ["IsVector", "IsCyc"], function(root, a)
  local zeta;
  zeta := ReflectionEigenvalue(a);
  return Coroot(root, E(Order(zeta)));
end);

###########################################################################
##  
##  This is similar to `ReflectionMat' in library, but that is no operation.
##  
##  <#GAPDoc Label="ReflectionMatrix">
##  <ManSection >
##  <Oper Arg="root, coroot" Name="ReflectionMatrix" />
##  <Oper Arg="root[, eigenvalue]" Name="ReflectionMatrix" 
##        Label="with eigenvalue" />
##  <Returns>matrix for reflection</Returns>
##  <Description>
##  
##  In the first  form <A>root</A> and <A>coroot</A> must be  vectors over a
##  cyclotomic field of the same  dimension. While <A>root</A> is considered
##  as element  of a vector space  <M>V</M> with respect to  some basis, the
##  vector <A>coroot</A>  is considered  as element of  the dual  space with
##  respect to the dual basis. The operation returns the reflection  <M>s: V
##  \rightarrow V</M>,  <M>x \mapsto  x -  <A>coroot</A>(x) <A>root</A></M>,
##  see <Ref Subsect="ss:ReflRootCoroot"/> above for more details. It is not 
##  checked that the map is a reflection, i.e., has finite order (if in
##  doubt check <C>Order(1-<A>coroot</A>*<A>root</A>)</C>). The reflection
##  is returned as a matrix which acts from the right on elements of <M>V</M>. 
##  <P/>
##  
##  In the second form of the command the coroot is first computed with 
##  <C>Coroot(<A>root</A>[, <A>eigenvalue</A>])</C>,
##  see <Ref Oper="Coroot"/>.
##  
##  <Example>
##  gap> r := ReflectionMatrix([1,1,1],[0,0,1-E(3)]);
##  [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 2*E(3)+E(3)^2, 2*E(3)+E(3)^2, E(3) ] ]
##  gap> Order(r);
##  3
##  gap> r := ReflectionMatrix([0,1,1],3);
##  [ [ 1, 0, 0 ], [ 0, -1/2*E(3)^2, E(3)+1/2*E(3)^2 ], 
##    [ 0, E(3)+1/2*E(3)^2, -1/2*E(3)^2 ] ]
##  gap> r := ReflectionMatrix([1,1,0],2/3);         
##  [ [ -1/2*E(3), 1/2*E(3)+E(3)^2, 0 ], [ 1/2*E(3)+E(3)^2, -1/2*E(3), 0 ], 
##    [ 0, 0, 1 ] ]
##  gap> r := ReflectionMatrix([1,1,1]);             
##  [ [ 1/3, -2/3, -2/3 ], [ -2/3, 1/3, -2/3 ], [ -2/3, -2/3, 1/3 ] ]
##  gap> Order(r);
##  2
##  </Example>
##  </Description>
##  </ManSection>
##  
##  <#/GAPDoc>
##  
##  reflection from a root and coroot 
##  arg: root, coroot;  result: matrix for x-> x - (x*coroot) root 
##  root*coroot <> 0
InstallMethod(ReflectionMatrix, ["IsVector", "IsVector"], 
function(root, coroot)
  local   dim,  id;
  dim := Length(root);
  id := IdentityMat(dim);
  return List([1..dim], i-> id[i]-coroot[i]*root);
end);

InstallMethod(ReflectionMatrix, ["IsVector", "IsCyc"], function(root, a)
  return ReflectionMatrix(root, Coroot(root, a));
end);
##  default order is two (for vectors in Euclidean space with standard
##  scalar product)
InstallMethod(ReflectionMatrix, ["IsVector"], function(root)
  return ReflectionMatrix(root, Coroot(root, -1));
end);

###########################################################################
##  
##  <#GAPDoc Label="ReflectionGroupByRoots">
##  <ManSection >
##  <Func Arg="roots, coroots" Name="ReflectionGroupByRoots" />
##  <Func Arg="roots[, eigenvals]" Name="ReflectionGroupByRoots" 
##  Label="with eigenvalues"/>
##  <Returns>a reflection group as matrix group</Returns>
##  <Description>
##  
##  The arguments <A>roots</A> and <A>coroots</A> or <A>eigenvals</A>,
##  respectively, must be lists of the same lengths. Entries in the same
##  positions specify a root-coroot pair. Eigenvalues can be specified in
##  any format that is explained in <Ref Subsect="ss:SpecEigenValRefl" />.
##  If not given they are assumed as <M>-1</M>.
##  <P/>
##  
##  This  function   returns  the matrix group generated by the reflections
##  which are computed by <Ref Oper="ReflectionMatrix"/> for each
##  root-coroot pair.
##  <P/>
##  
##  <Example><![CDATA[
##  gap> # infinite dihedral group, generators have the same root
##  gap> rts := [[1,0],[1,0]];
##  [ [ 1, 0 ], [ 1, 0 ] ]
##  gap> crts := [[2,0],[2,1]];
##  [ [ 2, 0 ], [ 2, 1 ] ]
##  gap> g := ReflectionGroupByRoots(rts,crts);
##  Group([ [ [ -1, 0 ], [ 0, 1 ] ], [ [ -1, 0 ], [ -1, 1 ] ] ])
##  gap> Size(g);
##  infinity
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
InstallGlobalFunction(ReflectionGroupByRoots, function(roots, arg...)
  local evorcr, coroots, gens, res, i;
  if Length(arg) > 0 then
    evorcr := arg[1];
  else
    evorcr := List(roots, a-> -1);
  fi;
  coroots := [];
  # compute coroot if only eigenvalue given
  for i in [1..Length(roots)] do
##  Should we better use distiguished coroots here?
##      if IsList(evorcr[i]) then
##        Add(coroots, DistinguishedCoroot(roots[i], evorcr[i]));
##      else
##        Add(coroots, Coroot(roots[i], E(Order(evorcr[i]))));
##      fi;
    # we take the roots and coroots as given
    if IsVector(evorcr[i]) then
      Add(coroots, evorcr[i]);
    else
      Add(coroots, Coroot(roots[i], evorcr[i]));
    fi;
  od;
  gens := List([1..Length(roots)], i-> ReflectionMatrix(roots[i], coroots[i]));
  res := ReflectionGroup(gens);
  # see next attribute
  Setter(MatricesGeneratingReflections)(res, gens);
  Setter(GeneratingRoots)(res, roots);
  Setter(GeneratingCoroots)(res, coroots);
  return res;
end);

###########################################################################
##  
##  <#GAPDoc Label="MatricesGeneratingReflections">
##  
##  <ManSection >
##  <Attr Arg="W" Name="MatricesGeneratingReflections" />
##  <Returns>list of matrices</Returns>
##  <Description>
##  
##  This    attribute   returns    for    a   group    <A>W</A>   in    <Ref
##  Filt="CanGeneratingRoots"/>    the   list    of   reflection    matrices
##  corresponding to the generating roots  and coroots of <A>W</A>; see <Ref
##  Oper="ReflectionMatrix"/>.<P/>
##  
##  Note that these must not be the <C>GeneratingReflections(<A>W</A>)</C>.
##  
##  <Example>
##  gap> GeneratingReflections(W);        
##  [ (1,5)(2,4)(6,8), (1,3)(2,6)(5,7) ]
##  gap> MatricesGeneratingReflections(W);
##  [ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
##    [ [ 1, 0, 0 ], [ 0, -1, 1 ], [ 0, 0, 1 ] ] ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(MatricesGeneratingReflections, ["CanGeneratingRoots"], 
function(G)
  local   gr,  gcr;
  gr := GeneratingRoots(G);
  gcr := GeneratingCoroots(G);
  return List([1..Length(gr)], i-> ReflectionMatrix(gr[i], gcr[i]));
end);

###########################################################################
##  
##  <#GAPDoc Label="GeneratingRoots">
##  <ManSection >
##  <Heading>Getting generating roots and coroots</Heading>
##  <Attr Arg="W" Name="GeneratingRoots" />
##  <Returns>list of row vectors</Returns>
##  <Attr Arg="W" Name="GeneratingCoroots" />
##  <Returns>list of row vectors</Returns>
##  <Attr Arg="W" Name="NrGeneratingRoots" />
##  <Returns>a non-negative integer</Returns>
##  <Description>
##  
##  For   a  group   <A>W</A>   in   <Ref  Filt="CanGeneratingRoots"/>   the
##  attribute   <Ref  Attr="GeneratingRoots"/>   returns  a   list  of   row
##  vectors  over   the  cyclotomics.  This   is  interpreted  as   list  of
##  roots  in  a  complex  vector   space  <M>V</M>  with  respect  to  some
##  basis   (see  <Ref   Oper="ReflectionMatrix"/>).   The  attribute   <Ref
##  Attr="GeneratingCoroots"/> also returns  a list (of same  length) of row
##  vectors, which are  this time interpreted as elements of  the dual space
##  <M>V^\vee</M> with  respect to  the dual  basis. (From  the mathematical
##  point of view it  would be nicer to have column  vectors as coroots, but
##  these would be unpleasant to type  and have less efficient arithmetic in
##  &GAP;.) <P/>
##  
##  The  attribute <Ref  Attr="NrGeneratingRoots"/>  returns  the number  of
##  generating roots.<P/>
##  
##  If <A>W</A> has already stored  the generating roots and the eigenvalues
##  of the generating  reflections but no generating coroots,  then the dual
##  space <M>V^\vee</M> is identified with  <M>V</M> via the standard scalar
##  product. The generating coroots are  then computed (and they are unique)
##  such  that  the  corresponding  reflections leave  the  standard  scalar
##  product invariant, see&nbsp;<Ref Subsect="ss:ReflRootCoroot"/>.<P/>
##  
##  </Description>
##  </ManSection>
##  
##  <#/GAPDoc>
##  
##  if `GeneratingCoroots' not given, we take `Coroot' with eigenvalue of
##  reflection given in `EigenvaluesGeneratingReflections'
InstallMethod(GeneratingCoroots, ["CanGeneratingRoots"], 
function(G)
  local   gr,  gro;
  gr := GeneratingRoots(G);
  gro := EigenvaluesGeneratingReflections(G);
  return List([1..Length(gr)], i-> Coroot(gr[i], gro[i]));
end);

###########################################################################
##  
##  <#GAPDoc Label="EigenvaluesGeneratingReflections">
##  <ManSection >
##  <Attr Arg="W" Name="EigenvaluesGeneratingReflections" />
##  <Returns>list of eigenvalues</Returns>
##  <Description>
##  
##  For  <A>W</A> in  <Ref Filt="CanGeneratingReflections"/>  this attribute
##  returns  the   list  of  non-trivial  eigenvalues   for  the  generating
##  reflections if these are known.  Recall that these eigenvalues are roots
##  of unity.<P/>
##  
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
##  if `EigenvaluesGeneratingReflections' and `GeneratingCoroots' not given
##  the eigenvalues are assumed as -1 
InstallMethod(EigenvaluesGeneratingReflections, ["CanGeneratingRoots"], 
function(G)
  return 0*[1..Length(GeneratingRoots(G))] - 1;
end);
##  else we can compute them
InstallMethod(EigenvaluesGeneratingReflections, 
        ["CanGeneratingRoots and HasGeneratingCoroots"],  
function(G)
  local gr, gcr;
  gr := GeneratingRoots(G);
  gcr := GeneratingCoroots(G);
  return List([1..Length(gr)], i-> 1 - gr[i]*gcr[i]);
end);

InstallMethod(NrGeneratingRoots, ["CanGeneratingRoots"], 
G -> Length(GeneratingRoots(G))  );

##  same as number of generating reflections
InstallMethod(NrGeneratingReflections, ["CanGeneratingRoots"],
G -> NrGeneratingRoots(G)  );

###########################################################################
##  
##  <#GAPDoc Label="LieRanks">
##  <ManSection >
##  <Heading>Ranks of Groups with Generating Roots</Heading>
##  <Attr Arg="W" Name="LieRank" />
##  <Attr Arg="W" Name="SemisimpleRank" />
##  <Returns>an integer</Returns>
##  <Description>
##  <Index>Lie rank</Index>
##  <Index >semisimple rank</Index>
##  
##  The  group <A>W</A>  must  be in  <Ref Filt="CanGeneratingRoots"/>.  The
##  attribute <Ref  Attr="LieRank"/> is  the dimension  of the  vector space
##  containing the roots. The  attribute <Ref Attr="SemisimpleRank"/> is the
##  dimension of the vector space spanned by the (generating) roots.
##  <P/>
##  Note that  in general  the dimensions  of the  spans of  the generating 
##  roots and of the generating coroots can be different.<P/>
##  
##  The  names  of  these  attributes   come  from  the  special  case  that
##  the  generating  roots  and  coroots   form  a  root  system  (see  <Ref
##  Sect="sec:RootSystems" />).
##  
##  <Example>
##  gap> rts := [[1,-1,0],[0,1,-1]];; # roots and coroots of GL3(K)
##  gap> W := ReflectionGroupByRoots(rts, rts);
##  Group([ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
##    [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 1, 0 ] ] ])
##  gap> LieRank(W);                                              
##  3
##  gap> SemisimpleRank(W);
##  2
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
InstallMethod(SemisimpleRank, ["CanGeneratingRoots"],
G -> RankMat(GeneratingRoots(G))  );

##  `LieRank' is the dimension of the space containing the roots
InstallMethod(LieRank, ["CanGeneratingRoots"], 
function(G)
  local   rts;
  rts := GeneratingRoots(G);
  if Length(rts) = 0 then
    if HasReflectionParent(G) then
      return LieRank(ReflectionParent(G));
    fi;
    Error("Don't have roots. Please, tell me my Lie rank!\n");
  fi;
  return Length(rts[1]);
end);

###########################################################################
##  
##  <#GAPDoc Label="CartanMatrixAttr">
##  <ManSection >
##  <Attr Arg="W" Name="CartanMatrix" />
##  <Returns>a matrix</Returns>
##  <Description>
##  
##  The  group <A>W</A>  must be  in <Ref  Filt="CanGeneratingRoots"/>. This
##  attribute returns  a  square  matrix  <C>C</C> of  size  the  number  of
##  generating  roots   where  the  entry  <C>i,j</C>   is  the  <C>i</C>-th
##  generating coroot evaluated at the <C>j</C>-th generating root.
##  <P/>
##  Note that  the matrix <C>C</C> determines  the reflection representation
##  of <A>W</A> up to isomorphism if  the generating roots together with the
##  intersection of  all kernels  of the generating  coroots span  the whole
##  vectorspace <M>V</M>.
##  <P/>
##  Also note  that in  general this matrix  is not a  Cartan matrix  in the
##  sense of the  filter <Ref Oper="IsCartanMatrix"/> which  will be defined
##  in the next section.
##  <Example>
##  gap> rts := [[1,-1,0],[0,1,-1]];; # roots and coroots of GL3(K)
##  gap> W := ReflectionGroupByRoots(rts, rts);
##  Group([ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
##    [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 1, 0 ] ] ])
##  gap> CartanMatrix(W);                                              
##  [ [ 2, -1 ], [ -1, 2 ] ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
InstallMethod(CartanMatrix, ["CanGeneratingRoots"],
G -> GeneratingCoroots(G) * TransposedMat(GeneratingRoots(G))  );

################  computing roots, coroots, permutations ############

##  all `Roots' by orbit algorithm
InstallMethod(Roots, ["CanGeneratingRoots"],
G -> FullOrbits(MatricesGeneratingReflections(G), GeneratingRoots(G)) );

# to find position of given root quickly
InstallMethod(SortedRoots, ["CanGeneratingRoots"], function(W)
  local r, p;
  r := ShallowCopy(Roots(W));
  p := [1..Length(r)];
  SortParallel(r, p);
  MakeImmutable(r);
  IsSet(r);
  return [r, p];
end);
InstallMethod(PositionRoot, ["CanGeneratingRoots", "IsObject"],
function(W, r)
  local sr, p;
  sr := SortedRoots(W);
  p := Position(sr[1], r);
  if p = fail then
    return fail;
  else
    return sr[2][p];
  fi;
end);

##  `PermsRoots': permutation of `MatricesGeneratingReflections' on `Roots'
InstallMethod(PermsRoots, ["CanGeneratingRoots"],
function(G)
  local   rts, pp, mats;
  rts := ShallowCopy(Roots(G));
  pp := Sortex(rts)^-1;
  mats := MatricesGeneratingReflections(G);
  return List(mats, m-> (Sortex(rts * m)^-1)^pp);
end);

##  this is an attribute just for caching purposes
InstallMethod(OrbitsAndWordsPermsCGR, ["CanGeneratingRoots"],
function(G)
  return OrbitsAndWordsPerms(PermsRoots(G), RootsInclusion(G), []);
end);

InstallMethod(RootOrbitRepresentatives, ["CanGeneratingRoots"],
        function(G)
  local   res,  ow,  orb,  i,  a;
  res := [];
  ow := OrbitsAndWordsPermsCGR(G);
  for orb in ow do
    i := orb[1][1];
    for a in orb do
      res[a[1]] := i;
    od;
  od;
  return res;
end);

##  all `Coroots' in same order as `Roots'
InstallMethod(Coroots, ["CanGeneratingRoots"],
function(G)
  local mats, ow, res, a, orb, i;
  # matrices of generators on V^\vee are transposed of those on V
  mats := List(MatricesGeneratingReflections(G), TransposedMat);
  ow := OrbitsAndWordsPermsCGR(G);
  res := ShallowCopy(GeneratingCoroots(G));
  for orb in ow do
    for i in [2..Length(orb)] do
      a := orb[i];
      res[a[1]] := res[a[3]] * mats[a[2]];
    od;
  od;
  return res;
end);

##  default method for `GeneratingReflections' returns matrices
InstallMethod(GeneratingReflections, ["CanGeneratingRoots"], 
        MatricesGeneratingReflections);

##  we overwrite `Reflections' by a more efficient function which also
##  guarantees that they are now in bijection with (and ordered as)
##  the `Roots'
##  (we use an extra function which can be applied to different sets
##  of generators)
InstallMethod(Reflections, ["CanGeneratingRoots"],
function(G)
  local   ow,  go,  pgr,  res,  orb,  i,  a,  g;
  ow := OrbitsAndWordsPermsCGR(G);
  go := OrdersGeneratingReflections(G);
  res := ShallowCopy(GeneratingReflections(G));
  for orb in ow do
    for i in [2..Length(orb)] do
      a := orb[i];
      g := res[a[2]];
      # this is    res[a[3]] ^ g
      res[a[1]] := g^(go[a[2]]-1) * res[a[3]] * g;
    od;
  od;
  return res;
end);
  
################  reflection subgroups with roots ##################

##  transfer the roots to a reflection subgroup, here l is list of positions
##  in list of reflections
InstallMethod(ReflectionSubgroupByPositions, ["CanGeneratingRoots", "IsList"],
        function(G, l)
  local   res,  pos;
  res := ReflectionSubgroupBasic(G, l);
  if not IsSubset([1..NrGeneratingRoots(G)], l) then
    # non parabolic, in particular G is finite
    SetGeneratingRoots(res, Roots(G){l});
    SetGeneratingCoroots(res, Coroots(G){l});
  else
    SetGeneratingRoots(res, GeneratingRoots(G){l});
    SetGeneratingCoroots(res, GeneratingCoroots(G){l});
  fi;
  return res;
end);

        
##  `RootsInclusion' and `RootsRestriction'. Here `ReflectionsInclusion' and
##  `ReflectionsRestriction' are reduced to the functions for the roots.
##  Functions for `Roots' and similar are reduced to RootsInclusion and
##  RootsRestriction. So, the actual work is done here.
InstallMethod(RootsInclusion, ["CanGeneratingRoots"],
G-> [1..Length(Roots(G))] );
InstallMethod(RootsInclusion, ["CanGeneratingRoots and HasReflectionParent"],
function(G)
  local rp;
  rp := Roots(ReflectionParent(G));
  return List(Roots(G), x-> Position(rp, x));
end);

InstallMethod(RootsRestriction, ["CanGeneratingRoots"],
function(G)
  local   ri,  res,  i;
  ri := RootsInclusion(G);
  res := [];
  for i in [1..Length(ri)] do
    res[ri[i]] := i;
  od;
  return res;
end);

InstallMethod(ReflectionsInclusion, ["CanGeneratingRoots"], RootsInclusion);
InstallMethod(ReflectionsRestriction, ["CanGeneratingRoots"], RootsRestriction);

##### some functions for reflection subgroups by reduction to parent #########

InstallMethod(Roots, ["HasReflectionParent and HasRootsInclusion"],
G-> Roots(ReflectionParent(G)){RootsInclusion(G)} );
InstallMethod(Coroots, ["HasReflectionParent and HasRootsInclusion"],
G-> Coroots(ReflectionParent(G)){RootsInclusion(G)} );
InstallMethod(Reflections, ["HasReflectionParent and HasRootsInclusion"],
G-> Reflections(ReflectionParent(G)){RootsInclusion(G)} );
InstallMethod(GeneratingRoots, ["HasReflectionParent and \
               HasGeneratingReflectionsInclusion and CanGeneratingRoots"], 
G-> Roots(ReflectionParent(G)){GeneratingReflectionsInclusion(G)} );
InstallMethod(GeneratingRoots, 
        ["HasReflectionParent and HasRootsInclusion"], 
function(G)
  local rp, ri;
  rp := ReflectionParent(G);
  ri := RootsInclusion(G){[1..NrGeneratingRoots(G)]};
  if IsSubset([1..NrGeneratingRoots(rp)], ri) then
    return GeneratingRoots(rp){ri};
  else
    return Roots(rp){ri};
  fi;
end);
InstallMethod(GeneratingCoroots, 
        ["HasReflectionParent and HasRootsInclusion"], 
function(G)
  local rp, ri;
  rp := ReflectionParent(G);
  ri := RootsInclusion(G){[1..NrGeneratingRoots(G)]};
  if IsSubset([1..NrGeneratingRoots(rp)], ri) then
    return GeneratingCoroots(rp){ri};
  else
    return Coroots(rp){ri};
  fi;
end);

###########################################################################
##  
##  <#GAPDoc Label="ReflectionGroupPermutingRoots">
##  <ManSection >
##  <Func Arg="roots, coroots" Name="ReflectionGroupPermutingRoots" />
##  <Func Arg="roots[, eigenvals]" Name="ReflectionGroupPermutingRoots" 
##  Label="with eigenvalues"/>
##  <Returns>a reflection group as permutation group</Returns>
##  <Description>
##  
##  This  function   returns  a  group   <C>G</C>  isomorphic  to   the  one
##  generated  by  the reflections  corresponding  to  the given  roots  and
##  coroots.  The  first   and  second  argument  must  be   lists  of  same
##  length.   Then   <A>roots</A><C>[i]</C>   and   <A>coroots</A><C>[i]</C>
##  or   <A>eigenvals</A><C>[i]</C>,   respectively,    determine   a   <Ref
##  Oper="ReflectionMatrix"/>.  The  second  argument can  be  omitted,  the
##  default is  that each  reflection has non-trivial  eigenvalue <M>-1</M>.
##  <P/>
##  
##  The  elements of  the returned  group  <C>G</C> are  represented by  the
##  permutations they  induce on the <C>Roots(G)</C>,  see Section&nbsp;<Ref
##  Sect="sec:IsPermRoot"/>. <P/>
##  
##  This function  can only be applied  if <C>Roots(G)</C> is finite  and if
##  the generating roots are linearly independent.
##  <Example><![CDATA[
##  gap> genrts := [[1,-1,0],[0,2,-1]];; gencrts := [[1,-1,0 ],[0,1,0]];;
##  gap> W := ReflectionGroupPermutingRoots(genrts, gencrts);
##  Group([ (1,3)(2,5)(6,8), (1,4)(2,6)(3,7) ])
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallGlobalFunction(ReflectionGroupPermutingRoots, function(arg...)
  local g, gens, res, attr;
  g := CallFuncList(ReflectionGroupByRoots, arg);
  if RankMat(GeneratingRoots(g)) <> NrGeneratingRoots(g) then
    Error(Concatenation("For group permuting roots, the generating roots \n",
                        "must be linearly independent.\n"));
  fi;
  gens := PermsRoots(g);
  res := ReflectionGroup(gens);
  for attr in [GeneratingRoots, GeneratingCoroots,
               MatricesGeneratingReflections, Roots, PermsRoots] do
    Setter(attr)(res, attr(g));
  od;
  return res;
end);

