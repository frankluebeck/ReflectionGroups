############################################################################
##  
#W  PackageInfo.g      GAP package ReflectionGroups             Frank Lübeck
##  
##  Meta information for the package.

SetPackageInfo( rec(

PackageName := "ReflectionGroups",
Subtitle := "Basic functionality for reflection groups",
CurrentVersion := function()
  local this, dir, v;
  this := INPUT_FILENAME();
  dir := this{[1..Length(this)-13]};
  v := StringFile(Concatenation(dir, "VERSION"));
  NormalizeWhitespace(v);
  return v;
end,
Version := ~.CurrentVersion(),
# dd/mm/yyyy
Date := "20/02/2018",
Persons := [
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Frank",
    LastName := "Lübeck",
    WWWHome := "http://www.math.rwth-aachen.de/~Frank.Luebeck",
    Email := "Frank.Luebeck@Math.RWTH-Aachen.De",
    PostalAddress := Concatenation(
               "Frank Lübeck\n",
               "Lehrstuhl D für Mathematik\n",
               "RWTH Aachen\n",
               "Pontdriesch 14/16\n",
               "52062 Aachen\n",
               "GERMANY" ),
    Place := "Aachen",
    Institution := "Lehrstuhl D für Mathematik, RWTH Aachen",
  ),
],

SourceRepository := rec(
  Type := "git",
  URL := 
     "https://github.com/https://github.com/frankluebeck/ReflectionGroups.git/",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  :=
              "http://www.math.rwth-aachen.de/~Frank.Luebeck/ReflectionGroups/",
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL      := Concatenation( ~.PackageWWWHome, 
                                 "ReflectionGroups-", ~.Version ),
ArchiveFormats := ".tar.gz",
Status := "dev",
AbstractHTML   :=  "",
PackageDoc := rec(
  BookName  := "ReflectionGroups",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Basic functionality for reflection groups",
),
Dependencies := rec(
  GAP := ">= 4.8",
  NeededOtherPackages := [ [ "GAPDoc", ">= 1.5" ] ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),
AvailabilityTest := ReturnTrue,
TestFile := "tst/testall.g",
));


