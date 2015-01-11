# FSQL : Makefile -- Makefile for FSQL
#
# Copyright (C) 2015 gahag
# All rights reserved.
#
# This software may be modified and distributed under the terms
# of the BSD license.  See the LICENSE file for details.

Build = ghc
Strip = strip
Pack  = upx --best

OutFlag = -o $(OutputFile)
Include = -idirs:$(SrcDir)
ObjFlag = -outputdir $(ObjDir)
DynFlag = -dynamic
OptFlag = -O2


CheckDirs =  [ -d "$(BinDir)" ] || mkdir "$(BinDir)" ;\
						 [ -d "$(ObjDir)" ] || mkdir "$(ObjDir)" ;\
             [ -d "$(ObjDir)$(DynFlag)" ] || mkdir "$(ObjDir)$(DynFlag)" ;\
             [ -d "$(ObjDir)$(OptFlag)" ] || mkdir "$(ObjDir)$(OptFlag)"


MainSrc = src/Main.hs
             
BinDir = Build/Bin/
ObjDir = Build/Obj

SrcDir = src/

OutputFile = $(BinDir)fsql


.PHONY: clean all dyn opt

clean :
	rm -rf $(ObjDir) $(ObjDir)$(DynFlag) $(ObjDir)$(OptFlag)

all : $(SrcDir) clean
	$(CheckDirs)
	$(Build) $(ObjFlag) $(Include) $(OutFlag) $(MainSrc)


dyn : $(SrcDir) clean
	$(CheckDirs)
	$(Build) $(DynFlag) $(ObjFlag)$(DynFlag) $(Include) $(OutFlag)$(DynFlag) $(MainSrc)
	$(Strip) $(OutputFile)$(DynFlag)

opt : $(SrcDir) clean
	$(CheckDirs)
	$(Build) $(OptFlag) $(ObjFlag)$(OptFlag) $(Include) $(OutFlag)$(OptFlag) $(MainSrc)
	$(Strip) $(OutputFile)$(OptFlag)
	$(Pack)  $(OutputFile)$(OptFlag)
