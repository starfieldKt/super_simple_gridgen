@echo off

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022

rem ----------------------------------------------------------------------
rem ifort compile
rem ----------------------------------------------------------------------
ifort .\src\iric.f90  /Qopenmp /nostandard-realloc-lhs /MD /c
ifort .\src\super_simple_gridgen.f90  /Qopenmp /nostandard-realloc-lhs /MD /c
ifort *.obj .\lib\iriclib.lib -o ".\install\super_simple_gridgen.exe"

del *.obj
del *.mod
