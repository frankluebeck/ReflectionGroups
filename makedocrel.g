############################################################################
##  
#W  makedocrel.g        GAP package ReflectionGroups            Frank Lübeck
##  
##  Create package documentation within the main package directory with
##        gap makedocrel.g
##  

pathtodoc:= "doc";;
pathtopkg:= ".";;
main:= "ReflectionGroups";;
pkgname:= "ReflectionGroups";;
bookname:= "ReflectionGroups";;
if not IsBound(pathtoroot) then
  pathtoroot:= "../../..";
fi;
files := List(DirectoryContents("./lib"), fn-> Concatenation("../lib/", fn));
files := Filtered(files, a-> a{[Length(a)-1,Length(a)]} = ".g" or
                             a{[Length(a)-2..Length(a)]} in [".gi", ".gd"]);
Append(files, [
]);

MakeGAPDocDoc( pathtodoc, main, files, bookname, pathtoroot, "MathJax" );;
CopyHTMLStyleFiles(pathtodoc);
GAPDocManualLabFromSixFile( bookname, 
    Concatenation( pathtodoc, "/manual.six" ) );
