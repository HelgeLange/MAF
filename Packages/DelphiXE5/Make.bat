@echo off
set IdeDir="C:\DelphiXE
set OutputDirBPL="..\..\..\MAF_Release\Trial\Delphi2010"
set OutputDirDCP="..\..\..\MAF_Release\Trial\Delphi2010\dcu"
set OutputDirDCU="..\..\..\MAF_Release\Trial\Delphi2010\dcu"
Set OutPutDirs=-LE%OutputDirBPL% -LN%OutputDirDCP% -N0%OutputDirDCU%
set compilerOptions=-$O+ -$W- -$H+ -$X+ -$J-

mkdir %OutputDirBPL%
mkdir %OutputDirDCU%
del %OutputDirBPL%\*.* /y
del %OutputDirDCU%\*.* /y

%IdeDir%\Bin\dcc32.exe" -Z -B -DTrial;Release %OutPutDirs% MAF_Base.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DTrial;Release;Package_Build %OutPutDirs% MAF_BaseDesign.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DTrial;Release; %OutPutDirs% MAF_Visual.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DTrial;Release; %OutPutDirs% MAF_VisualDesign.dpk %compilerOptions%

set OutputDirBPL="..\..\..\MAF_Release\Release\Delphi2010"
set OutputDirDCP="..\..\..\MAF_Release\Release\Delphi2010\dcu"
set OutputDirDCU="..\..\..\MAF_Release\Release\Delphi2010\dcu"
Set OutPutDirs=-LE%OutputDirBPL% -LN%OutputDirDCP% -N0%OutputDirDCU%

mkdir %OutputDirBPL%
mkdir %OutputDirDCU%
del %OutputDirBPL%\*.* /y
del %OutputDirDCU%\*.* /y

%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease %OutPutDirs% MAF_Base.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease;Package_Build %OutPutDirs% MAF_BaseDesign.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease; %OutPutDirs% MAF_Visual.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease; %OutPutDirs% MAF_VisualDesign.dpk %compilerOptions%

set OutputDirBPL="..\..\..\MAF_Release\PE\Delphi2010"
set OutputDirDCP="..\..\..\MAF_Release\PE\Delphi2010\dcu"
set OutputDirDCU="..\..\..\MAF_Release\PE\Delphi2010\dcu"
Set OutPutDirs=-LE%OutputDirBPL% -LN%OutputDirDCP% -N0%OutputDirDCU%

mkdir %OutputDirBPL%
mkdir %OutputDirDCU%
del %OutputDirBPL%\*.* /y
del %OutputDirDCU%\*.* /y

%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease;PE; %OutPutDirs% MAF_Base.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease;Package_Build;PE; %OutPutDirs% MAF_BaseDesign.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease;PE; %OutPutDirs% MAF_Visual.dpk %compilerOptions%
%IdeDir%\Bin\dcc32.exe" -Z -B -DRelease;PE; %OutPutDirs% MAF_VisualDesign.dpk %compilerOptions%

