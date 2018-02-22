############################################################################
##  
#W  GenRefl.gi                                                  Frank Lübeck
##  
##  The files  prg/GenRefl.g{d,i} contain basic  functions for all  kinds of
##  reflection groups: accessing generating reflections, getting and setting
##  labels  for reflections,  the  basic functions  for creating  reflection
##  subgroups. Groups  which don't  have additional information  about their
##  generating  reflections  are certainly  not  very  useful as  reflection
##  groups.
##  

###########################################################################
##  
##  <#GAPDoc Label="ReflectionGroup">
##  <ManSection >
##  <Oper Arg="obj" Name="ReflectionGroup" />
##  <Meth Arg="gens" Name="ReflectionGroup" Label="for group elements"/>
##  <Returns>a reflection group</Returns>
##  <Description>
##  This operation is  either used to create a reflection  group from a list
##  <A>gens</A> of  group elements or  to find the  corresponding reflection
##  group from a related object  (e.g., from a <Ref Oper="ReflectionCoset"/>
##  or a <Ref Oper="CoxeterWord"/>).<P/>
##  
##  In  the first  case the  generator list  <A>gens</A> must  be a  list of
##  generating reflections  (this is not  checked). Then this call  does the
##  same  as  <C>Group(<A>gens</A>)</C>,  except  that  the  attribute  <Ref
##  Attr="GeneratingReflections"/> is set in addition.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(ReflectionGroup, ["IsList"], function(gens)
  local res;
  res := Group(gens);
  SetGeneratingReflections(res, GeneratorsOfGroup(res));
  return res;
end);

############################################################################
##  
##  <#GAPDoc Label="Reflections">
##  <ManSection >
##  <Attr Arg="W" Name="Reflections" />
##  <Returns>list of group elements</Returns>
##  <Description>
##  For  a group  <A>W</A>  in  <Ref Filt="CanGeneratingReflections"/>  this
##  attribute returns a  list containing all reflections  of <A>W</A>. These
##  are  by definition  all  elements  of <A>W</A>  which  are conjugate  to
##  one  of  the generating  reflections  or  to  one of  their  non-trivial
##  powers.  This list  always  starts with  the  generating reflections  of
##  <A>W</A>, in  certain cases it may contain repeated elements (see, e.g.,
##  <Ref Attr="Reflections" Label="for CanGeneratingRoots" />). <P/>
##  
##  Do  not  call this  function  on  a group  with  an  infinite number  of
##  reflections.
##  
##  <Example>
##  gap> W := ReflectionGroup([(1,2),(2,3),(3,4)]);; Reflections(W); 
##  [ (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(Reflections, ["CanGeneratingReflections"], function(G)
  local   gr, start, a, b, one;
  gr := GeneratingReflections(G);
  start := ShallowCopy(gr);
  one := One(G);
  for a in gr do
    b := a*a;
    while b <> one do
      if not b in start then 
        Add(start, b);
      fi;
      b := b*a;
    od;
  od;
  return FullOrbits(gr, start);
end);

###########################################################################
##  
#F  ReflectionParent( W )           [Attr]
##  
##  <#GAPDoc Label="ReflectionParent">
##  <ManSection >
##  <Attr Arg="W" Name="ReflectionParent" />
##  <Returns>a reflection (over-)group</Returns>
##  <Description>
##  
##  Each reflection group  <A>W</A> in &RefGrp; has a  reflection parent. If
##  not set otherwise (e.g., in <Ref Oper="ReflectionSubgroupByPositions"/>)
##  a group is  its own reflection parent. The &RefGrp;  programs try to use
##  additional structures (root system, reflection representation, ...) of a
##  parent group for reflection subgroups as well.
##  
##  <Example>
##  gap> s5 := Group((1,2,3,4,5),(1,2,3,4));;   
##  gap> W := Subgroup(s5, [(1,2,3,4),(2,3,4)]);;
##  gap> Setter(GeneratingReflections)(W, [(1,2),(2,3),(3,4)]);
##  gap> Parent(W) = s5; ReflectionParent(W) = W;
##  true
##  true
##  gap> U := ReflectionSubgroupByLabels(W, [2, 3]);;
##  gap> U2 := ReflectionSubgroupByLabels(U, [3]);;
##  gap> Parent(U2) = U; ReflectionParent(U2) = W;
##  true
##  true
##  gap> GeneratingReflections(U2);
##  [ (3,4) ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
## ReflectionParent: by default the group itself
InstallMethod(ReflectionParent, ["CanGeneratingReflections"], IdFunc);

###########################################################################
##  
##  <#GAPDoc Label="GeneratingReflections">
##  <ManSection >
##  <Attr Arg="W" Name="GeneratingReflections" />
##  <Returns>list of group elements</Returns>
##  <Attr Arg="W" Name="OrdersGeneratingReflections" />
##  <Returns>list of element orders</Returns>
##  <Attr Arg="W" Name="NrGeneratingReflections" />
##  <Returns>a non-negative integer</Returns>
##  
##  <Description>
##  The  argument <A>W</A>  for these  operations must  be a  group in  <Ref
##  Filt="CanGeneratingReflections"/>.  The  first   operation  returns  the
##  generating reflections as  list of group elements,  the second operation
##  returns a list  of corresponding element orders and  the third operation
##  returns the length of these lists. <P/>
##  
##  In the example we create the  symmetric group <M>S_4</M> as a reflection
##  group. Here  the group  stores the  generating reflections  but actually
##  there  is  not much  one can  do with them.  We will  see later  in <Ref
##  Chap="chap:NatRep"/>  that  it  is  advantageous to  create  this  group
##  with <Ref  BookName="Reference" Func="SymmetricGroup" />  (after loading
##  &RefGrp;).
##  
##  <Example><![CDATA[
##  gap> W:=ReflectionGroup([(1,2),(2,3),(3,4)]);
##  <reflection group of unknown type>
##  gap> GeneratingReflections(W);
##  [ (1,2), (2,3), (3,4) ]
##  gap> OrdersGeneratingReflections(W);
##  [ 2, 2, 2 ]
##  gap> NrGeneratingReflections(W);
##  3]]>
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(NrGeneratingReflections, ["CanGeneratingReflections"],
function(G) 
  return Length(GeneratingReflections(G)); 
end);

##  orders of generators by inspection
InstallMethod(OrdersGeneratingReflections, ["CanGeneratingReflections"],
function(G)
  return List(GeneratingReflections(G), Order);
end);
       
###########################################################################
##  
##  <#GAPDoc Label="LabelsGeneratingReflections">
##  <ManSection >
##  <Attr Arg="W" Name="LabelsGeneratingReflections" />
##  <Attr Arg="W" Name="LabelsReflections" />
##  <Returns>list of labels</Returns>
##  <Description>
##  
##  For  a group  <A>W</A> in  <Ref Filt="CanGeneratingReflections"/>  these
##  attributes  return  lists  of  labels  for  each  generating  reflection
##  or   for  each   reflection  returned   by  <Ref   Attr="Reflections"/>,
##  respectively. These <E>labels</E> are  used for printing and referencing
##  purposes,   see  for   example   <Ref   Oper="CoxeterWord"/>  and   <Ref
##  Oper="ReflectionSubgroupByLabels"/>.  A  label  can essentially  be  any
##  &GAP; object, but it should be printed by a short string. <P/>
##  
##  The  default labels  are either  integers  giving the  positions in  the
##  list  of reflections,  or for  reflection subgroups  they are  inherited
##  from  the  <Ref  Attr="ReflectionParent"/>  group.  The  labels  can  be
##  changed  with <Ref  Oper="ChangeLabelsGeneratingReflections"/> and  <Ref
##  Oper="ChangeLabelsReflections"/>, respectively.
##  
##  <Example>
##  gap> W:=ReflectionGroup([(1,2),(2,3),(3,4)]);;   
##  gap> LabelsGeneratingReflections(W);
##  [ 1 .. 3 ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
##  `Labels[Generating]Reflections': by default positions in parent
##  slight complication since we may not have the full list of reflections
##  but a change of the generator labels should also change the corresponding 
##  entries there.
InstallMethod(LabelsGeneratingReflections, ["CanGeneratingReflections"],
function(G)
  local P, incl;
  if Tester(LabelsReflections)(G) then
    return LabelsReflections(G){[1..NrGeneratingReflections(G)]};
  else
    P := ReflectionParent(G);
    if IsIdenticalObj(P, G) then
      return [1..NrGeneratingReflections(G)];
    fi;
    incl := GeneratingReflectionsInclusion(G);
    if IsSubset([1..NrGeneratingReflections(G)], incl) then
      return LabelsGeneratingReflections(P){incl};
    else
      return LabelsReflections(P){incl};
    fi;
  fi;
end);
InstallMethod(LabelsReflections, ["CanGeneratingReflections"],
function(G)
  local P, res;
  P := ReflectionParent(G);
  if IsIdenticalObj(P, G) then
    res := ReflectionsInclusion(G);
  else
    res := LabelsReflections(P){ReflectionsInclusion(G)};
  fi;
  if Tester(LabelsGeneratingReflections)(G) then
    res := ShallowCopy(res);
    res{[1..NrGeneratingReflections(G)]} := LabelsGeneratingReflections(G);
  fi;
  return res;
end);

###########################################################################
##  
##  <#GAPDoc Label="ChangeLabelsGeneratingReflections">
##  <ManSection >
##  <Oper Arg="W, labels" Name="ChangeLabelsGeneratingReflections" />
##  <Oper Arg="W, labels" Name="ChangeLabelsReflections" />
##  <Returns>nothing</Returns>
##  <Description>
##  
##  These  functions   can  be   used  to   change  explicitly   the  labels
##  returned   by   <Ref   Attr="LabelsGeneratingReflections"/>   and   <Ref
##  Attr="LabelsReflections"/>, respectively. Here <A>W</A>  must be a group
##  in <Ref Filt="CanGeneratingReflections"/> and <A>labels</A> must be list
##  of new labels which has the appropriate length. Use these functions with
##  care,  only  the  labels  of  <A>W</A>, not  its  reflection  parent  or
##  childrens are changed.
##  <P/>
##  Of  course, be  careful that  the previous  labels become  invalid after
##  using these functions.
##  <Example>
##  gap> W:=ReflectionGroup([(1,2),(2,3),(3,4)]);;
##  gap> ChangeLabelsGeneratingReflections(W, "abc");
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallMethod(ChangeLabelsGeneratingReflections,
        ["CanGeneratingReflections", "IsList"],
function(G, l)
  local   lgr, lr;
  lgr := LabelsGeneratingReflections(G);
  if Length(l) <> Length(lgr) then
    Error("list must have length ", Length(lgr)); 
  fi;
  G!.LabelsGeneratingReflections := Immutable(l);
  if Tester(LabelsReflections)(G) then
    lr := ShallowCopy(G!.LabelsReflections);
    lr{[1..NrGeneratingReflections(G)]} := G!.LabelsGeneratingReflections; 
    G!.LabelsReflections := lr;
  fi;
end);
InstallMethod(ChangeLabelsReflections,
        ["CanGeneratingReflections", "IsList"],
function(G, l)
  local   lr;
  lr := LabelsReflections(G);
  if Length(l) <> Length(lr) then
    Error("list must have length ", Length(lr)); 
  fi;
  G!.LabelsReflections := Immutable(l);
  if Tester(LabelsGeneratingReflections)(G) then
    G!.LabelsGeneratingReflections :=
      G!.LabelsReflections{[1..NrGeneratingReflections(G)]}; 
  fi;
end);

###########################################################################
##  
##  <#GAPDoc Label="ReflectionSubgroupGeneric">
##  <ManSection >
##  <Heading>Creating reflection subgroups</Heading>
##  <Oper Arg="W, poss" Name="ReflectionSubgroupByPositions" />
##  <Oper Arg="W, labs" Name="ReflectionSubgroupByLabels" />
##  <Oper Arg="W, elts" Name="ReflectionSubgroupByElements" />
##  <Returns>a reflection subgroup</Returns>
##  <Description>
##  
##  These  functions   return  subgroups  of   a  group  <A>W</A>   in  <Ref
##  Filt="CanGeneratingReflections"/> which are generated by a subset of its
##  reflections, therefore these are reflection groups themselves. <P/>
##  
##  The three operations provide three ways to specify the reflections which
##  generate the subgroup. The first gets a list <A>poss</A> of positions of
##  these generators in the list <Ref Func="Reflections" /><C>(<A>W</A>)</C>
##  (which starts with the list  of generating reflections of <A>W</A>). The
##  second gets a  list <A>labs</A> of labels for the  reflections, see <Ref
##  Attr="LabelsReflections"/>. And  for the third the  argument <A>elts</A>
##  must be a list of elements of <A>W</A> which are reflections.<P/>
##  
##  Any   reflection    group   in   &RefGrp;   has    an   attribute   <Ref
##  Attr="ReflectionParent"/>;  for a  reflection subgroup  this equals  the
##  <C>ReflectionParent(<A>W</A>)</C>. <P/>
##  
##  A  reflection   subgroup  can  be   asked  for  the  embedding   of  its
##  reflections   into   those   of   the  reflection   parent,   see   <Ref
##  Attr="ReflectionsInclusion"/> and  <Ref Attr="ReflectionsRestriction"/>.
##  <P/>
##  
##  The  default  labels  (see   <Ref  Attr="LabelsReflections"/>)  for  the
##  reflections  of a  reflection subgroup  are those  of the  corresponding
##  elements in the reflection parent.<P/>
##  
##  Note that it can happen that  the generating reflections of the returned
##  subgroup may not be the reflections given in the argument.
##  
##  <Example>
##  gap> W := ReflectionGroup([(1,2), (2,3), (3,4)]);;
##  gap> Reflections(W);
##  [ (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) ]
##  gap> U := ReflectionSubgroupByElements(W, [(2,3), (1,4)]);;
##  gap> LabelsReflections(W);
##  [ 1 .. 6 ]
##  gap> LabelsGeneratingReflections(U);
##  [ 2, 6 ]
##  gap> U2 := ReflectionSubgroupByPositions(U, [2]);;
##  gap> LabelsGeneratingReflections(U2);
##  [ 6 ]
##  </Example>
##  
##  (Note  for  programmers: Usually  new  methods  for  this need  only  be
##  installed for the first of the  three operations since the other two are
##  generically reduced to the first one.)
##  
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  

##  ReflectionSubgroupBasic: 2nd arg is list of positions in Reflections(..).
##  In the basic case we just create the corresponding subgroup. Function
##  can be used in other methods which do additional work.
InstallGlobalFunction(ReflectionSubgroupBasic, function(G, l)
  local   pgr,  lr,  genr,  gincl,  res;
  pgr := [1..NrGeneratingReflections(G)];
  if IsSubset(pgr, l) then
    if Length(Set(l))<>Length(l) then
      l := Set(l);
    fi;
    genr := GeneratingReflections(G){l};
    gincl := GeneratingReflectionsInclusion(G){l};
  else
    genr := Reflections(G){l};
    gincl := ReflectionsInclusion(G){l};
  fi;
  # here the 'NC' is important in case of infinite groups
  res := SubgroupNC(G, genr);
  SetGeneratingReflections(res, genr);
  SetReflectionParent(res, ReflectionParent(G));
  SetGeneratingReflectionsInclusion(res, gincl);
  return res;
end);

##  Basic function is the general method.
InstallMethod(ReflectionSubgroupByPositions, 
              ["CanGeneratingReflections", "IsList"], ReflectionSubgroupBasic);

###########################################################################
##  
#F  ReflectionsInclusion( W )           [Oper]
#F  GeneratingReflectionsInclusion( W )           [Oper]
#F  ReflectionsRestriction( W )           [Oper]
##  
##  <#GAPDoc Label="ReflectionsInclusion">
##  <ManSection >
##  <Oper Arg="W" Name="ReflectionsInclusion" />
##  <Oper Arg="W" Name="GeneratingReflectionsInclusion" />
##  <Oper Arg="W" Name="ReflectionsRestriction" />
##  <Returns>lists of positions</Returns>
##  <Description>
##  
##  Let <A>W</A> be  a reflection group and <C>G</C>  its reflection parent,
##  see  <Ref Attr="ReflectionParent"/>.  Furthermore, let  <C>reflW</C> and
##  <C>reflG</C>  be the  lists of  reflections <C>Reflections(<A>W</A>)</C>
##  and <C>Reflections(G)</C>, respectively. <P/>
##  
##  Then    <C>ReflectionsInclusion(<A>W</A>)</C>    is   a    list    which
##  contains    in     position    <C>i</C>    the    position     of    the
##  <C>i</C>-th   element  of   <C>reflW</C>  in   the  list   <C>reflG</C>.
##  <C>GeneratingReflectionsInclusion(<A>W</A>)</C> is  just the  first part
##  of  the same  list which  corresponds to  the generating  reflections of
##  <A>W</A>. This is often sufficient to  know and works also when the list
##  of all reflections is very long or even infinite.<P/>
##  
##  The  list returned  by  <C>ReflectionsRestriction(W)</C>  is unbound  in
##  position  <C>i</C> if  the <C>i</C>-th  element of  <C>reflG</C> is  not
##  contained in <C>reflW</C>. Otherwise it is bound to the position of this
##  element in <C>reflW</C>.
##  
##  <Example>
##  gap> W := ReflectionGroup([(1,2), (2,3), (3,4)]);; Reflections(W);
##  [ (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) ]
##  gap> U := ReflectionSubgroupByLabels(W, [2, 3]);; Reflections(U);
##  [ (2,3), (3,4), (2,4) ]
##  gap> ReflectionsInclusion(U);  
##  [ 2, 3, 5 ]
##  gap> GeneratingReflectionsInclusion(U);
##  [ 2, 3 ]
##  gap> ReflectionsRestriction(U);
##  [ , 1, 2,, 3 ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
##  avoid to compute all reflections if only generating ones are needed
InstallMethod(GeneratingReflectionsInclusion, ["CanGeneratingReflections"],
function(G)
  local   rp,  gen,  rpgen,  rpp;
  rp := ReflectionParent(G);
  gen := GeneratingReflections(G);
  rpgen := GeneratingReflections(rp);
  if IsIdenticalObj(G, rp) then
    return [1..NrGeneratingReflections(G)];
  elif IsSubset(rpgen, gen) then
    rpp := [1..NrGeneratingReflections(rp)];
    return rpp{List(gen, x-> Position(rpgen, x))};
  else
    return ReflectionsInclusion(G){[1..NrGeneratingReflections(G)]};
  fi;
end);

##  positions of all reflections in list of reflections of parent
InstallMethod(ReflectionsInclusion, ["CanGeneratingReflections"],
function(G)
  local   refs;
  if IsIdenticalObj(G, ReflectionParent(G)) then
    return [1..Length(Reflections(G))];
  else
    refs := Reflections(ReflectionParent(G));
    return List(Reflections(G), r-> Position(refs, r));
  fi;
end);

##  positions of all reflections of parent in list of reflections
##  (position unbound if not in subgroup)
InstallMethod(ReflectionsRestriction, ["CanGeneratingReflections"],
        function(G)
  local   inc,  res,  i;
  if IsIdenticalObj(G, ReflectionParent(G)) then
    return ReflectionsInclusion(G);
  else
    inc := ReflectionsInclusion(G);
    res := [];
    for i in [1..Length(inc)] do
      res[inc[i]] := i;
    od;
    return res;
  fi;
end);

##  generic `ReflectionSubgroup' by labels instead of positions:
InstallMethod(ReflectionSubgroupByLabels,
        ["CanGeneratingReflections", "IsList"],
function(G, l)
  local   lr,  ll;
  # first check is subset of generating reflections (G may be infinite)
  lr := LabelsGeneratingReflections(G);
  if IsSubset(lr, l) then
    ll := List(l, x-> Position(lr, x));
  else
    lr := LabelsReflections(G);
    ll := List(l, x-> Position(lr, x));
  fi;
  return ReflectionSubgroupByPositions(G, ll);
end);

##  generic `ReflectionSubgroup' by elements instead of positions:
InstallMethod(ReflectionSubgroupByElements,
        ["CanGeneratingReflections", "IsList"],
function(G, l)
  local   lr,  ll;
  lr := GeneratingReflections(G);
  # first check is subset of generating reflections (G may be infinite)
  if IsSubset(lr, l) then
    ll := List(l, x-> Position(lr, x));
  else
    lr := Reflections(G);
    ll := List(l, x-> Position(lr, x));
  fi;
  return ReflectionSubgroupByPositions(G, ll);
end);

##  default for GeneratorsOfGroup is GeneratingReflections
InstallMethod(GeneratorsOfGroup, ["CanGeneratingReflections"],
       GeneratingReflections);
InstallMethod(GeneratorsOfGroup, 
       ["IsGroup and IsHandledByNiceMonomorphism and CanGeneratingReflections"],
       GeneratingReflections);
