############################################################################
##  
#W  Utils.gi                                                    Frank Lübeck
##  
##  The  files   Utils.g{d,i}  contain   some  utility  functions   for  the
##  ReflectionGroups package.
##  

###########################################################################
##  
##  <#GAPDoc Label="FullOrbits">
##  <ManSection >
##  <Func Name="FullOrbits" Arg="gens, pts[, act]" />
##  <Returns>list of points</Returns>
##  <Description>
##  The argument <A>gens</A> is a list of group elements or a group which is
##  acting  on points  in  the  list <A>pts</A>  via  the action  <A>opr</A>
##  (Default is <Ref BookName="Reference" Oper="OnPoints" />). This function
##  returns a list of points that  starts with the given list <A>pts</A> and
##  contains all orbits of points in <A>pts</A> under <A>gens</A>.
##  <Example><![CDATA[
##  gap> FullOrbits(SymmetricGroup(7),[2,3,4,3,1]);
##  [ 2, 3, 4, 3, 1, 5, 6, 7 ]
##  gap> v := MutableIdentityMat(2, GF(2));;
##  gap> gens := GeneratorsOfGroup(SL(2,2));;
##  gap> orbs := FullOrbits(gens, v);
##  [ <an immutable GF2 vector of length 2>, 
##    <an immutable GF2 vector of length 2>,
##    <an immutable GF2 vector of length 2> ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
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

###########################################################################
##  
##  <#GAPDoc Label="OrbitsAndWordsPerms">
##  <ManSection >
##  <Oper Name="OrbitsAndWordsPerms" Arg="perms, pts, start" />
##  <Returns>list of lists</Returns>
##  <Description>
##  The arguments are a list <A>perms</A>  of permutations, a list of points
##  <A>pts</A> which must be invariant  under the action of <A>perms</A> and
##  a list <A>start</A> consisting of point in <A>pts</A>.<P/>
##  
##  The result  is a list which  contains for each orbit  of <A>perms</A> on
##  <A>pts</A>  a list  of  triples <C>[[i0,0,0],  ..., [ik,j,jk],  ...]</C>
##  where <C>i0</C>  is a  representative of  the orbit  and in  the further
##  triples <C>[ik,j,jk]</C> the  first entry <C>ik</C> is new  point of the
##  orbit such that <C>ik^<A>perms</A>[j] = jk</C> with <C>jk</C> an earlier
##  point of the orbit.
##  <Example><![CDATA[
##  gap> ow := OrbitsAndWordsPerms([(1,2,4),(4,5,6)], [6,5..1], []);
##  [ [ [ 6, 0, 0 ], [ 4, 2, 6 ], [ 1, 1, 4 ], [ 5, 2, 4 ], [ 2, 1, 1 ] ], 
##    [ [ 3, 0, 0 ] ] ]
##  gap> # as application we compute a transversal of the first orbit
##  gap> tr := [];; tr[ow[1][1][1]] := ();;
##  gap> for i in [2..Length(ow[1])] do 
##  >      a := ow[1][i]; tr[a[1]] := tr[a[3]]*perms[a[2]];
##  >    od;
##  gap> tr;
##  [ (1,2,4,5,6), (1,4,5,6,2),, (4,5,6), (4,6,5), () ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
# for each orbit of <perms> on [1..maxpt] a list [[i_0,0,0],...
# [i_k, j, i_k^(perms[j]^-1)]...] where the third point in these tripels
# is earlier in that orbit
InstallGlobalFunction(OrbitsAndWordsPerms, function(perms, pts, start)
  local   lp,  res,  l,  i,  r,  a,  p,  j, max;
  lp := Length(perms);
  res := [];
  max := Maximum(pts);
  l := BlistList([1..max], [1..max]);
  for i in Concatenation(start, pts) do
    if l[i] then
      r := [[i,0,0]];
      l[i] := false;
      for a in r do
        for p in [1..lp] do
          j := a[1]^perms[p];
          if l[j] then
            Add(r, [j, p, a[1]]);
            l[j] := false;
          fi;
        od;
      od;
      Add(res, r);
    fi;
  od;
  return res;
end);

#############################################################################
##  
##  <#GAPDoc Label="SEBasis">
##  <ManSection >
##  <Heading>Semiechelon Bases and Spinning Vectors</Heading>
##  <Func Arg="vecs" Name="SEBasis" />
##  <Returns>a record</Returns>
##  <Func Arg="sebasis, vec" Name="AddToSEBasis" />
##  <Returns><K>true</K> or <K>false</K></Returns>
##  <Func Arg="gens, vecs[, sebasis]" Name="SpinVectors"/>
##  <Returns>a record</Returns>
##  <Description>
##  The  first function  <Ref  Func="SEBasis" />  returns  a record  similar
##  to  <Ref BookName="Reference"  Attr="SemiEchelonMat"/>.  In addition  it
##  returns an  entry <C>pivots</C> which  is a list containing  in position
##  <C>i</C> the column  of the pivot in row  <C>vectors[i]</C>. The entries
##  of the record  are mutable such that they can  be extended. The function
##  <Ref  Func="AddToSEBasis"  />  gets  reduces a  vector  <A>vec</A>  with
##  respect to the  semiechelon basis <A>sebasis</A>. If  the reduced vector
##  is non-zero, it is appended to  the semiechelon basis and <K>true</K> is
##  returned. Otherwise <K>false</K> is returned. <P/>
##  
##  The  function <Ref  Func="SpinVectors" />  gets  a list  of row  vectors
##  <A>vecs</A> as seed and a list  of matrices <A>gens</A> which act on the
##  row vectors.  It returns  a semiechelon  basis of  the space  spanned by
##  <A>vecs</A> and their images under  the monoid generated by <A>gens</A>.
##  Optionally, the function  can get as argument  <A>sebasis</A> the result
##  of a previous call (with the same argument <A>gens</A>).
##  
##  <Example>
##  gap> mat := [[0,0,0],[1,1,1],[1,2,3],[2,3,4]];;
##  gap> seb := SEBasis(mat);
##  rec( heads := [ 1, 2, 0 ], pivots := [ 1, 2 ], 
##    vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ] ] )
##  gap> AddToSEBasis(seb, [1,2,4]);
##  true
##  gap> AddToSEBasis(seb, [1,2,3]);
##  false
##  gap> seb;
##  rec( heads := [ 1, 2, 3 ], pivots := [ 1, 2, 3 ], 
##    vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ], [ 0, 0, 1 ] ] )
##  gap> gens := [ [[0,1,-1],[0,1,0],[-1,0,2]], [[-2,1,0],[1,-4,1],[0,-3,1]] ];;
##  gap> SpinVectors(gens, mat);
##  rec( heads := [ 1, 2, 3 ], pivots := [ 1, 2, 3 ], 
##    vectors := [ [ 1, 1, 1 ], [ 0, 1, 2 ], [ 0, 0, 1 ] ] )
##  </Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##  
InstallGlobalFunction(SEBasis, function(mat)
  local seb, piv, h, i;
  seb := SemiEchelonMat(mat);
  seb := rec(vectors := ShallowCopy(seb.vectors),
             heads := ShallowCopy(seb.heads));
  piv := [];
  h := seb.heads;
  for i in [1..Length(h)] do
    if h[i] <> 0 then
      piv[h[i]] := i;
    fi;
  od;
  seb.pivots := piv;
  return seb;
end);

InstallGlobalFunction(AddToSEBasis, function(seb, v)
  local rows, pivots, len, vv, c, pos, i;
  rows := seb.vectors;
  pivots := seb.pivots;
  len := Length(rows);
  vv := ShallowCopy(v);
  for i in [1..len] do
    c := vv[pivots[i]];
    if not IsZero(c) then
      AddRowVector(vv, rows[i], -c);
    fi;
  od;
  pos := PositionNonZero(vv);
  if pos <= Length(vv) then
    if not IsOne(vv[pos]) then
      vv := vv/vv[pos];
    fi;
    Add(rows, vv);
    Add(pivots, pos);
    seb.heads[pos] := len + 1;
    return true;
  else
    return false;
  fi;
end);

InstallGlobalFunction(SpinVectors, function(gens, vecs, sebasis...)
  local seb, new, v, g;
  if Length(sebasis) > 0 then
    seb := sebasis[1];
    new := [];
    for v in vecs do
      if AddToSEBasis(seb, v) then
        Add(new, seb.vectors[Length(seb.vectors)]);
      fi;
    od;
  else
    seb := SEBasis(vecs);
    new := ShallowCopy(seb.vectors);
  fi;
  for v in new do
    for g in gens do
      if AddToSEBasis(seb, v*g) then
        Add(new, seb.vectors[Length(seb.vectors)]);
      fi;
    od;
  od;
  return seb;
end);



###########################################################################
##  
##  <#GAPDoc Label="LinearlyIndependentRows">
##  <ManSection >
##  <Func Arg="mat[, max]" Name="LinearlyIndependentRows" />
##  <Returns>list of positions</Returns>
##  <Description>
##  This function  gets a  matrix or a  list of vectors  and returns  a list
##  <C>l</C> of indices such that <C><A>mat</A>{l}</C> is a maximal linearly
##  independent subset of the rows of <A>mat</A>.<P/>
##  
##  If  the optional  argument <A>max</A>  is given  then the  function will
##  return when  it has found  <A>max</A> linearly independent rows  (so the
##  result <C>l</C> will be of length at most <A>max</A>).
##  <Example>
##  gap> mat := [[0,0,0],[1,1,1],[1,2,3],[2,3,4]];;
##  gap> LinearlyIndependentRows(mat);
##  [ 2, 3 ]
##  gap> LinearlyIndependentRows(mat, 1);
##  [ 2 ]
##  gap> LinearlyIndependentRows(mat, 4);
##  [ 2, 3 ]
##  </Example>
##  </Description>
##  
##  </ManSection>
##  <#/GAPDoc>
# args:  mat[, max]        
# returns indices of set of lin. indep. rows (stops, when max rows were found)
InstallGlobalFunction(LinearlyIndependentRows, function(mat, max...)
  local b, res, i;
  if Length(max) > 0 then
    max := max[1];
  else
    max := Length(mat);
  fi;
  if Length(mat) = 0 then
    return [];
  fi;
  b := SEBasis([0*mat[1]]);
  res := [];
  for i in [1..Length(mat)] do
    if AddToSEBasis(b, mat[i]) then
      Add(res, i);
    fi;
    if Length(res) = max then
      return res;
    fi;
  od;
  return res;
end);

###########################################################################
##  
##  <#GAPDoc Label="RightInverseMatrix">
##  <ManSection >
##  <Func Arg="mat" Name="RightInverseMatrix" />
##  <Returns>a matrix or <K>fail</K></Returns>
##  <Description>
##  This function returns a right inverse of the given matrix <A>mat</A>, if
##  the rows  of <A>mat</A> are linearly  independent. Otherwise <K>fail</K>
##  is returned.
##  <Example>
##  gap> mat := [[1,0,1,0,1],[0,0,1,1,1],[2,0,0,1,4]];
##  [ [ 1, 0, 1, 0, 1 ], [ 0, 0, 1, 1, 1 ], [ 2, 0, 0, 1, 4 ] ]
##  gap> rimat := RightInverseMatrix(mat);
##  [ [ 1/3, -1/3, 1/3 ], [ 0, 0, 0 ], [ 2/3, 1/3, -1/3 ], [ -2/3, 2/3, 1/3 ], 
##    [ 0, 0, 0 ] ]
##  gap> mat*rimat;
##  [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]
##  gap> mat := [[1,0,1,0,1],[0,0,1,1,1],[1,0,2,1,2]];
##  [ [ 1, 0, 1, 0, 1 ], [ 0, 0, 1, 1, 1 ], [ 1, 0, 2, 1, 2 ] ]
##  gap> RightInverseMatrix(mat);
##  fail
##  </Example>
##  </Description>
##  
##  </ManSection>
##  <#/GAPDoc>
InstallGlobalFunction(RightInverseMatrix, function(mat)
  local   tr,  r,  inv,  res;
  tr := TransposedMat(mat);
  r := LinearlyIndependentRows(tr);
  if Length(r) < Length(mat) then
    return fail;
  fi;
  inv := TransposedMat(tr{r}^-1);
  res := 0 * ShallowCopy(tr);
  res{r} := inv;
  return res;
end);

