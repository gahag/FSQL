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


BinDir = Build/Bin/
ObjDir = Build/Obj

SrcDir = src/

OutputFile = $(BinDir)fsql


all :
	$(CheckDirs)
	$(Build) $(ObjFlag) $(Include) $(OutFlag) src/Main.hs


dyn : $(LibDir)
	$(CheckDirs)
	$(Build) $(DynFlag) $(ObjFlag)$(DynFlag) $(Include) $(OutFlag)$(DynFlag) src/Main.hs

opt :
	$(CheckDirs)
	$(Build) $(OptFlag) $(ObjFlag)$(OptFlag) $(Include) $(OutFlag)$(OptFlag) src/Main.hs
	$(Strip) $(OutputFile)$(OptFlag)
	$(Pack)  $(OutputFile)$(OptFlag)
	
