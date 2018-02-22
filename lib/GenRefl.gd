############################################################################
##  
#W  GenRefl.gd                                                  Frank Lübeck
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
##  <#GAPDoc Label="CanGeneratingReflections">
##  <ManSection >
##  <Filt Name="CanGeneratingReflections" Arg="W" />
##  <Description>
##  This    is    a    basic     filter    in    the    &RefGrp;    package.
##  It    returns    <K>true</K>   if    <A>W</A>    is    a   group    with
##  attribute    <Ref   Attr="GeneratingReflections"/>    and   <K>false</K>
##  otherwise.   There    are   several    other   filters    in   &RefGrp;,
##  e.g.,    <Ref    Filt="CanCoxeterGenerators"/>,   which    imply    <Ref
##  Filt="CanGeneratingReflections"/>. <P/>
##  
##  Note that a  group in <Ref Filt="CanGeneratingReflections"/>  can be any
##  kind  of group  which  can be  constructed in  &GAP;.  This filter  only
##  implies that there <E>exists</E> a  faithful representation of the group
##  which  maps the  list  of abstract  generating  reflections onto  actual
##  (complex) reflections as explained  in <Ref Label="lab:ComplRefl"/>. But
##  in general such a representation must not be known for that group.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
DeclareFilter("CanGeneratingReflections");
InstallTrueMethod(IsGroup, CanGeneratingReflections);


##  The attributes for groups in CanGeneratingReflections.

DeclareAttribute("GeneratingReflections", CanGeneratingReflections);
##  Generic implication HasGeneratingReflections --> CanGeneratingReflections
InstallTrueMethod(CanGeneratingReflections, HasGeneratingReflections);
DeclareAttribute("NrGeneratingReflections", CanGeneratingReflections);
DeclareAttribute("OrdersGeneratingReflections", CanGeneratingReflections);
DeclareAttribute("Reflections", CanGeneratingReflections);
DeclareAttribute("LabelsGeneratingReflections", CanGeneratingReflections);
DeclareAttribute("LabelsReflections", CanGeneratingReflections);
DeclareAttribute("ReflectionParent", CanGeneratingReflections);
DeclareAttribute("GeneratingReflectionsInclusion", CanGeneratingReflections);
DeclareAttribute("ReflectionsInclusion", CanGeneratingReflections);
DeclareAttribute("ReflectionsRestriction", CanGeneratingReflections);

##  This has two purposes: generating a group, and attribute of related objects
DeclareAttribute("ReflectionGroup", IsObject);

##  Utilities.
DeclareOperation("ChangeLabelsGeneratingReflections", 
                 [CanGeneratingReflections, IsList]);
DeclareOperation("ChangeLabelsReflections", 
                 [CanGeneratingReflections, IsList]);

##  Generating reflection subgroups.
DeclareOperation("ReflectionSubgroupByPositions", 
                 [CanGeneratingReflections, IsList]);
DeclareOperation("ReflectionSubgroupByLabels", 
                 [CanGeneratingReflections, IsList]);
DeclareOperation("ReflectionSubgroupByElements", 
                 [CanGeneratingReflections, IsList]);

DeclareGlobalFunction("ReflectionSubgroupBasic");

