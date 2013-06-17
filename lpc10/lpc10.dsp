# Microsoft Developer Studio Project File - Name="lpc10" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=lpc10 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "lpc10.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "lpc10.mak" CFG="lpc10 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "lpc10 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "lpc10 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe

!IF  "$(CFG)" == "lpc10 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Ox /Ot /Oa /Ow /Og /Oi /Op /Oy /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# SUBTRACT CPP /Z<none> /Os
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "lpc10 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# SUBTRACT CPP /WX
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "lpc10 - Win32 Release"
# Name "lpc10 - Win32 Debug"
# Begin Source File

SOURCE=.\analys.c
# End Source File
# Begin Source File

SOURCE=.\bsynz.c
# End Source File
# Begin Source File

SOURCE=.\chanwr.c
# End Source File
# Begin Source File

SOURCE=.\dcbias.c
# End Source File
# Begin Source File

SOURCE=.\decode.c
# End Source File
# Begin Source File

SOURCE=.\deemp.c
# End Source File
# Begin Source File

SOURCE=.\difmag.c
# End Source File
# Begin Source File

SOURCE=.\dyptrk.c
# End Source File
# Begin Source File

SOURCE=.\encode.c
# End Source File
# Begin Source File

SOURCE=.\energy.c
# End Source File
# Begin Source File

SOURCE=.\f2clib.c
# End Source File
# Begin Source File

SOURCE=.\ham84.c
# End Source File
# Begin Source File

SOURCE=.\hp100.c
# End Source File
# Begin Source File

SOURCE=.\invert.c
# End Source File
# Begin Source File

SOURCE=.\irc2pc.c
# End Source File
# Begin Source File

SOURCE=.\ivfilt.c
# End Source File
# Begin Source File

SOURCE=.\lpcdec.c
# End Source File
# Begin Source File

SOURCE=.\lpcenc.c
# End Source File
# Begin Source File

SOURCE=.\lpcini.c
# End Source File
# Begin Source File

SOURCE=.\lpfilt.c
# End Source File
# Begin Source File

SOURCE=.\median.c
# End Source File
# Begin Source File

SOURCE=.\mload.c
# End Source File
# Begin Source File

SOURCE=.\onset.c
# End Source File
# Begin Source File

SOURCE=.\pitsyn.c
# End Source File
# Begin Source File

SOURCE=.\placea.c
# End Source File
# Begin Source File

SOURCE=.\placev.c
# End Source File
# Begin Source File

SOURCE=.\preemp.c
# End Source File
# Begin Source File

SOURCE=.\prepro.c
# End Source File
# Begin Source File

SOURCE=.\random.c
# End Source File
# Begin Source File

SOURCE=.\rcchk.c
# End Source File
# Begin Source File

SOURCE=.\synths.c
# End Source File
# Begin Source File

SOURCE=.\tbdm.c
# End Source File
# Begin Source File

SOURCE=.\voicin.c
# End Source File
# Begin Source File

SOURCE=.\vparms.c
# End Source File
# End Target
# End Project
