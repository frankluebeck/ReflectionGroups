############################################################################
##  
#W  GenRoot.gd                                                  Frank Lübeck
##  
##  The files prg/GenRoot.g{d,i} contain basic functions for roots, coroots,
##  reflections and  reflection groups  which have an  associated reflection
##  representation.
##  

###########################################################################
##  
##  <#GAPDoc Label="CanGeneratingRoots">
##  <ManSection >
##  <Filt Name="CanGeneratingRoots" Arg="W" />
##  <Description>
##  This  basic  filter  in  the &RefGrp;  package  returns  <K>true</K>  if
##  <A>W</A> is a group on which the attribute <Ref Attr="GeneratingRoots"/>
##  can  be  called and  <K>false</K>  otherwise.  There are  several  other
##  filters  in &RefGrp;,  e.g., <Ref  Filt="CanSimpleRoots"/>, which  imply
##  <Ref Filt="CanGeneratingRoots"/>. <P/>
##  
##  This filter  implies <Ref Filt="CanGeneratingReflections"/> and  the map
##  which sends the generating reflections to the reflections defined by the
##  generating  roots and  coroots  (in  the given  orders)  must define  an
##  isomorphism.<P/>
##  
##  That  is, for  groups  in <Ref  Filt="CanGeneratingRoots"/>  we have  an
##  explicit  reflection representation.  The reflection  representation for
##  such a  group <C>W</C> must  map the full list  <C>Reflections(W)</C> to
##  the reflections defined by  <C>Roots(W)</C> and <C>Coroots(W)</C> in the
##  same order.<P/>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
DeclareFilter("CanGeneratingRoots");
InstallTrueMethod(CanGeneratingReflections, CanGeneratingRoots);

##  Basic operations
DeclareOperation("Coroot", [IsVector]);
DeclareOperation("Coroot", [IsVector, IsCyc]);
DeclareOperation("DistinguishedCoroot", [IsVector, IsVector]);
DeclareOperation("ReflectionMatrix", [IsVector]);
DeclareOperation("ReflectionMatrix", [IsVector, IsObject]);

##  Utility function
DeclareGlobalFunction("ReflectionEigenvalue");

##  Creating reflection groups by roots
DeclareGlobalFunction("ReflectionGroupByRoots");
DeclareGlobalFunction("ReflectionGroupPermutingRoots");

##  Attributes for groups with generating roots
DeclareAttribute("GeneratingRoots", CanGeneratingRoots);
##  General implication `HasGeneratingRoots' -> `CanGeneratingRoots'
InstallTrueMethod(CanGeneratingRoots, HasGeneratingRoots);
DeclareAttribute("GeneratingCoroots", CanGeneratingRoots);
DeclareAttribute("EigenvaluesGeneratingReflections", CanGeneratingRoots);
DeclareAttribute("NrGeneratingRoots", CanGeneratingRoots);
DeclareAttribute("MatricesGeneratingReflections", CanGeneratingRoots);
DeclareAttribute("Roots", CanGeneratingRoots);
DeclareAttribute("SortedRoots", CanGeneratingRoots);
DeclareAttribute("Coroots", CanGeneratingRoots);
DeclareAttribute("IndependentRoots", CanGeneratingRoots);
DeclareAttribute("BaseExtensionRoots", CanGeneratingRoots);
DeclareAttribute("PermsRoots", CanGeneratingRoots);
DeclareAttribute("AllPermsRoots", CanGeneratingRoots);
DeclareAttribute("RootOrbitRepresentatives", CanGeneratingRoots);
DeclareAttribute("RootsInclusion", CanGeneratingRoots);
InstallTrueMethod(CanGeneratingRoots, HasRootsInclusion);
DeclareAttribute("RootsRestriction", CanGeneratingRoots);

## We abuse notation for a more general setting
DeclareAttribute("CartanMatrix", CanGeneratingRoots);
DeclareAttribute("SemisimpleRank", CanGeneratingRoots);
DeclareAttribute("LieRank", CanGeneratingRoots);

##  used internally by several functions
DeclareAttribute("OrbitsAndWordsPermsCGR", CanGeneratingRoots);

##  More operations
DeclareOperation("PositionRoot", [CanGeneratingRoots, IsObject]);
