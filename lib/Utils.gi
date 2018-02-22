############################################################################
##  
#W  Utils.gi                                                    Frank Lübeck
##  
##  The  files   Utils.g{d,i}  contain   some  utility  functions   for  the
##  ReflectionGroups package.
##  


############################################################################
##  
##  <#GAPDoc Label="FullOrbits">
##  <ManSection >
##  <Func Name="FullOrbits" Arg="gens, pts[, act]" />
##  </ManSection>
##  <#/GAPDoc>
InstallGlobalFunction(FullOrbits, function(gens, pts, act...)
  local orb, sorb, pp, p, g, ls;
  if Length(act) = 0 then
    act := OnPoints;
  else
    act := act[1];
  fi;
  if IsGroup(gens) then
    gens := GeneratorsOfGroup(gens);
  fi;
  orb := List(pts, Immutable);
  gens := Immutable(gens);
  sorb := Set(orb);
  ls := Length(sorb);
  for p in orb do 
    for g in gens do
      pp := act(p, g);
      AddSet(sorb, pp);
      if Length(sorb) > ls then
        Add(orb, pp);
        ls := ls+1;
      fi;
    od;
  od;
  return orb;
end);



