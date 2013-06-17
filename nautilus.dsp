# Microsoft Developer Studio Project File - Name="nautilus" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=nautilus - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "nautilus.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "nautilus.mak" CFG="nautilus - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "nautilus - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "nautilus - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "nautilus - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "rsaref" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "ORDER_DCBA" /D NTP_CLASSES=X(ntp_udp) /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:console /machine:I386
# SUBTRACT LINK32 /profile /incremental:yes /debug /nodefaultlib

!ELSEIF  "$(CFG)" == "nautilus - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /I "rsaref" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "ORDER_DCBA" /D NTP_CLASSES=X(ntp_udp) /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "nautilus - Win32 Release"
# Name "nautilus - Win32 Debug"
# Begin Source File

SOURCE=.\bitstrm.c
# End Source File
# Begin Source File

SOURCE=.\cli.c
# End Source File
# Begin Source File

SOURCE=.\comm.c
# End Source File
# Begin Source File

SOURCE=.\config.c
# End Source File
# Begin Source File

SOURCE=.\crc.c
# End Source File
# Begin Source File

SOURCE=.\crypt.c
# End Source File
# Begin Source File

SOURCE=.\des3.c
# End Source File
# Begin Source File

SOURCE=.\dhparams.c
# End Source File
# Begin Source File

SOURCE=.\getopt.c
# End Source File
# Begin Source File

SOURCE=.\idea.c
# End Source File
# Begin Source File

SOURCE=.\init.c
# End Source File
# Begin Source File

SOURCE=.\lpc10.c
# End Source File
# Begin Source File

SOURCE=.\naut_bf.c
# End Source File
# Begin Source File

SOURCE=.\nsp.c
# End Source File
# Begin Source File

SOURCE=.\ntp.c
# End Source File
# Begin Source File

SOURCE=.\ntp_tab.c
# End Source File
# Begin Source File

SOURCE=.\ntp_udp.c
# End Source File
# Begin Source File

SOURCE=.\README.17A
# End Source File
# Begin Source File

SOURCE=.\sha.c
# End Source File
# Begin Source File

SOURCE=.\sp2bits.c
# End Source File
# Begin Source File

SOURCE=.\sp64.c
# End Source File
# Begin Source File

SOURCE=.\sp85.c
# End Source File
# Begin Source File

SOURCE=.\talk.c
# End Source File
# Begin Source File

SOURCE=.\util.c
# End Source File
# Begin Source File

SOURCE=.\versions.c
# End Source File
# Begin Source File

SOURCE=.\win32.c
# End Source File
# Begin Source File

SOURCE=.\win32a.h
# End Source File
# Begin Source File

SOURCE=.\win32ai.c
# End Source File
# Begin Source File

SOURCE=.\win32ao.c
# End Source File
# End Target
# End Project
