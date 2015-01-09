# FSQL : Makefile -- Makefile for FSQL
#
# Copyright (C) 2015 Gabriel Silva Bastos
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

OutputFile = $(BinDir)$(basename $(notdir ${f}))


all :
	$(CheckDirs)
	$(Build) $(ObjFlag) $(Include) $(OutFlag) ${f}


dyn : $(LibDir)
	$(CheckDirs)
	$(Build) $(DynFlag) $(ObjFlag)$(DynFlag) $(Include) $(OutFlag)$(DynFlag) ${f}

opt :
	$(CheckDirs)
	$(Build) $(OptFlag) $(ObjFlag)$(OptFlag) $(Include) $(OutFlag)$(OptFlag) ${f}
	$(Strip) $(OutputFile)$(OptFlag)
	$(Pack)  $(OutputFile)$(OptFlag)
	
