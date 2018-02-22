############################################################################
##  
#W  Utils.gd                                                    Frank Lübeck
##  
##  The  files   Utils.g{d,i}  contain   some  utility  functions   for  the
##  ReflectionGroups package.
## 

###########################################################################
##  
##  <#GAPDoc Label="FullOrbits">
##  <ManSection >
##  <Oper Name="FullOrbits" Arg="gens, pts[, opr]" />
##  <Returns>list of points</Returns>
##  <Description>
##  The argument <A>gens</A> is a list of group elements or a group which is
##  acting  on points  in  the  list <A>pts</A>  via  the action  <A>opr</A>
##  (Default is <Ref BookName="Reference" Oper="OnPoints" />). This function
##  returns a list of points that  starts with the given list <A>pts</A> and
##  contains all orbits of points in <A>pts</A> under <A>gens</A>.
##  <Example>
##  gap> FullOrbits(SymmetricGroup(7),[2,3,4,3,1]);
##  [ 2, 3, 4, 3, 1, 5, 6, 7 ]
##  gap> v := MutableIdentityMat(2, GF(2));;
##  gap> gens := GeneratorsOfGroup(SL(2,2));;
##  gap> orbs := FullOrbits(gens, v);
##  [ <an immutable GF2 vector of length 2>, 
##    <an immutable GF2 vector of length 2>,
##    <an immutable GF2 vector of length 2> ]
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
DeclareGlobalFunction("FullOrbits");

