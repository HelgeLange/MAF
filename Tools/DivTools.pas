unit DivTools;

interface

{$DEFINE Delphi3Below}
{$IFDEF VER130} //Delphi 5
  {$UNDEF Delphi3Below}
{$ELSE}
  {$IFDEF VER120} //Delphi 4
    {$UNDEF Delphi3Below}
  {$ENDIF}
{$ENDIF}

uses Classes;

type DirectoryType = (_WINDOWS, _TEMP, _SYSTEM, _CURRENT);
     TUserOrCompany = (_USER, _COMPANY, _BOTH);
     IP_HostName = (_IP, _HOSTNAME);
     ShortcutType = (_DESKTOP, _QUICKLAUNCH, _SENDTO, _STARTMENU);

const Win32MajorVersion: Integer = 0; // contains the version number of Windows (eg. 95 / 98 / 5 ...)
      Win32MinorVersion: Integer = 0; // containts the minor version numbers eg (4.10 --> .10(=Minor))
      Win32BuildNumber: Integer = 0;  // contains the build number of Windows (eg. 2195 ....)
      Win32CSDVersion: string = '';

function KommaToPunkt(S: String): String;
function GetLocalUserName: string;
function GetLocalIPAddressOrHostName(IP_Or_HostName: IP_HostName): string;
function GetWindowsTyp: Word;
function GetWindowsVersion: String;
function GetOnlineStatus : Boolean;
procedure TaskBar_Hide;
function Windows_UpTime: String;
function GetIE_URL(var List: TStringList): Integer;
function GetRunningApps: TStringList;
function GetUsername: String;
function IsAdmin: Boolean;
function GetWinVersion: string;

implementation

uses MMSystem, MPlayer, Registry, Winsock, ShlObj, ActiveX, ComObj, Forms, SysUtils,
     Windows, Dialogs, WinINet, OleCtrls, ShDocVw;

function KommaToPunkt(S: String): String;
begin
 While Pos(',', S) > 0 Do
  S[Pos(',', S)] := '.';
 Result := S;
end;

function CoCreateGuid(var guid: TGUID): HResult; stdcall; far external 'ole32.dll';

function WinExecAndWait32(FileName: string; Visibility: Integer): Integer;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
           zAppName, { pointer to command line string }
           nil, { pointer to process security attributes }
           nil, { pointer to thread security attributes }
           false, { handle inheritance flag }
           CREATE_NEW_CONSOLE or { creation flags }
           NORMAL_PRIORITY_CLASS,
           nil, { pointer to new environment block }
           nil, { pointer to current directory name }
           StartupInfo, { pointer to STARTUPINFO }
           ProcessInfo) then
    Result := -1 { pointer to PROCESS_INF }
  else
  begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    Result := 0;
//    GetExitCodeProcess(ProcessInfo.hProcess, Result);
  end;
end;

function Get_MACAddress: string;
var g: TGUID;
    i: Byte;
begin
 Result := '';
 CoCreateGUID(g);
 for i := 2 to 7 do
  Result := Result + IntToHex(g.D4[i], 2);
end;

function GetIE_URL(var List: TStringList): Integer;
var sw  : TShellWindows;
    i,j : integer;
    ie  : IWebBrowser;
begin
 i := 0;
 List := TStringList.Create;
 sw := TShellWindows.Create(nil);
 for j := 0 to (sw.Count-1) do begin
  sw.item(j).QueryInterface(IWebBrowser, ie);
  if ie <> nil then begin
   List.Add(ie.Get_LocationURL);
   Inc(i);
  end;
 end;
 If i = 0 Then List.Destroy;
 Result := i;
end;

function Windows_UpTime: String;
var
  ndays: double;
  ticks: LongInt;
  btime: TDateTime;
begin
  {The GetTickCount function retrieves the number of
   milliseconds that have elapsed since Windows was started.}
  ticks := GetTickCount;

  {to convert this to the number of days, divide by number of
   milliseconds in a day, 24*60*60*1000=86400000}
  ndays := ticks/86400000;

  {to calculate the boot time we subtract the number-of-days-since-boot from the DateTime now.
   This works because a TDateTime is a double value which holds days and decimal days}
  bTime := now-ndays;

  {display the message}
  Result := FormatDateTime('"Windows started on" dddd, mmmm d, yyyy, '+'"at" hh:nn:ss AM/PM', bTime) + #10#13 +
   'Its been up for '+IntToStr(Trunc(nDays)) + ' days,'+FormatDateTime(' h "hours," n "minutes," s "seconds"',ndays);
end;

// must be placed in FormCreate-Event
procedure TaskBar_Hide;
begin
 SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) or
                                   WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW );
end;

function CDSerialNumber(CD: Char): string;
{ func to return the serial number on the CD.  Works on audio & data CDs. }
var
  mp : TMediaPlayer;
  msp : TMCI_INFO_PARMS;
  MediaString : array[0..255] of char;
  ret : longint;
begin
  mp := TMediaPlayer.Create(nil);
  try
    mp.Visible := false;
    mp.Parent := Application.MainForm;
    mp.Shareable := true;
    mp.DeviceType := dtCDAudio;
    mp.FileName := CD + ':';
    mp.Open; 
    Application.ProcessMessages;
    FillChar(MediaString, sizeof(MediaString), #0);
    FillChar(msp, sizeof(msp), #0);
    msp.lpstrReturn := @MediaString;
    msp.dwRetSize := 255;
    ret := mciSendCommand(Mp.DeviceId,
                          MCI_INFO,
                          MCI_INFO_MEDIA_IDENTITY,
                          longint(@msp));
    if Ret <> 0 then
      begin
        MciGetErrorString(ret, @MediaString, sizeof(MediaString));
        Result := StrPas(MediaString);
      end
    else 
      Result := StrPas(MediaString);
    mp.Close; 
    Application.ProcessMessages;
  finally
    mp.free;
  end;
end;

// liefert System-Verzeichnisse zurück
function GetDirectory(Dir: DirectoryType): string;
var
  Path: array [0..260] of Char;
begin
  case Dir of
    _WINDOWS: GetWindowsDirectory(Path, Sizeof(Path));
    _SYSTEM : GetSystemDirectory(Path, Sizeof(Path));
    _TEMP   : GetTempPath(Sizeof(Path), Path);
    _CURRENT: GetCurrentDirectory(Sizeof(Path), Path);
  end;
  Result := StrPas(Path);
end;

function GetWindowsTyp: Word;
var VerInfo: TOSVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwPlatformID;
  { The results are as followed: }
  {case VerInfo.dwPlatformID of
    VER_PLATFORM_WIN32S: Platform is 'Windows 3.1x running Win32s';
    VER_PLATFORM_WIN32_WINDOWS: Platform is 'Windows 95/98';
    VER_PLATFORM_WIN32_NT: Platform is 'Windows NT';
  end;}
end;

function GetWindowsVersion: String;
var VersionInfo: TOSVersionInfo;
    Platform : String;
begin
 VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
 GetVersionEx(VersionInfo);
 Platform := 'Windows ';
 With VersionInfo Do Begin
  Case dwPlatformid of
   0 : Platform := Platform + IntToStr(dwMajorVersion)
       + '.'+ IntToStr(VersionInfo.dwMinorVersion);
   1 : Platform := Platform + IntToStr(dwMajorVersion)
       + '.'+ IntToStr(VersionInfo.dwMinorVersion);
   2 : Platform := Platform + 'NT ' + IntToStr(dwMajorVersion)
       + '.'+ IntToStr(VersionInfo.dwMinorVersion);
  End;
  Platform := Platform + ' (Build ' + IntToStr(dwBuildNumber) + ')';
 End;
 Result := Platform;
end;

function GetUserAndCompanyName(UserOrCompany: TUserOrCompany): string;
{ func to return the user's name or company name or both.             }
{ User and company info is stored in different places in the registry }
{ on different computers - even though the OS is the same! These are  } 
{ all I can find, it you find any others let me know. Thanks to       }
{ Andrew Balahonov <sspp@cityline.ru> for making me aware of this.    }
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFDEF Delphi3Below}
    if Reg.OpenKey('\SOFTWARE\MICROSOFT\WINDOWS NT\CURRENTVERSION', False) then
    {$ELSE}
    if Reg.OpenKeyReadOnly('\SOFTWARE\MICROSOFT\WINDOWS NT\CURRENTVERSION') then
    {$ENDIF}
      case UserOrCompany of
        _USER   : Result := Reg.ReadString('RegisteredOwner');
        _COMPANY: Result := Reg.ReadString('RegisteredOrganization');
        _BOTH   : Result := Reg.ReadString('RegisteredOwner') + #13 +
                            Reg.ReadString('RegisteredOrganization');
      end;

    if (Result = '') or (Result = '' + #13 + '') then
      { Try another key. }
      begin
        Reg.CloseKey;
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        {$IFDEF Delphi3Below}
        if Reg.OpenKey('\SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION', False) then
        {$ELSE}
        if Reg.OpenKeyReadOnly('\SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION') then
        {$ENDIF}
          case UserOrCompany of
            _USER   : Result := Reg.ReadString('RegisteredOwner');
            _COMPANY: Result := Reg.ReadString('RegisteredOrganization');
            _BOTH   : Result := Reg.ReadString('RegisteredOwner') + #13 +
                                Reg.ReadString('RegisteredOrganization'); 
          end; 
      end;

    if (Result = '') or (Result = '' + #13 + '') then
      { Try another key. }
      begin
        Reg.CloseKey;
        Reg.RootKey := HKEY_CURRENT_USER;
        {$IFDEF Delphi3Below}
        if Reg.OpenKey('\SOFTWARE\MICROSOFT\MS SETUP (ACME)\USER INFO', False) then
        {$ELSE}
        if Reg.OpenKeyReadOnly('\SOFTWARE\MICROSOFT\MS SETUP (ACME)\USER INFO') then
        {$ENDIF}
          case UserOrCompany of
            _USER   : Result := Reg.ReadString('DefName');
            _COMPANY: Result := Reg.ReadString('DefCompany');
            _BOTH   : Result := Reg.ReadString('DefName') + #13 +
                                Reg.ReadString('DefCompany');
          end; 
      end;

    if (Result = '') or (Result = '' + #13 + '') then
      { Could not find info. }
      if UserOrCompany = _BOTH then
        Result := 'Undefined' + #13 + 'Undefined'
      else
        Result := 'Undefined';
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function GetLocalUserName: string;
{ func to retrieve the local username if logged on. }
var
  UserNameStr: string;
  NetUserNameLength : Cardinal;
begin
  NetUserNameLength := 50;
  SetLength(UserNameStr, NetUserNameLength);
  Windows.GetUserName(PChar(UserNameStr), NetUserNameLength);
  if GetLastError = ERROR_NOT_LOGGED_ON then
    Result := '<not logged on>'
  else
    begin
      SetLength(UserNameStr, StrLen(pChar(UserNameStr)));
      Result := UserNameStr;
    end;
end;

function GetLocalIPAddressOrHostName(IP_Or_HostName: IP_HostName): string;
var
  p : PHostEnt;
  s : array[0..128] of char;
  p2 : pchar;
  wVersionRequested : WORD;
  wsaData : TWSAData;
begin
  {Start up WinSock}
  wVersionRequested := MAKEWORD(1, 1);
  WSAStartup(wVersionRequested, wsaData);
  {Get the computer name} 
  GetHostName(@s, 128);
  p := GetHostByName(@s);
  if IP_Or_HostName = _HOSTNAME then
    Result := p^.h_Name 
  else
    begin
      {Get the IpAddress}
      p2 := iNet_ntoa(PInAddr(p^.h_addr_list^)^);
      Result := p2;
    end;
  {Shut down WinSock}
  WSACleanup;
end;

// Use LaunchControlPanelApplet() to launch a control panel applet of your choice.
// For example, if you wanted to run the display properties from Control Panel,
// call LaunchControlPanelApplet('Desk.cpl', False). The 'False' parameter tells
// the function to not wait until the display properties is closed before continuing
// onto the next line of code.  Note: this function uses WinExecAndWait32() and WinExec32().

function LaunchControlPanelApplet(Applet: string; Wait: boolean): boolean;
begin
  if (UpperCase(Copy(Applet, Length(Applet)-3, Length(Applet))) <> '.CPL') and
    (ExtractFileExt(Applet) = '') then
    Applet := Applet + '.cpl';
  Result := False;
  if Wait then
    Result := WinExecAndWait32('Rundll32.exe Shell32.dll,Control_RunDLL ' + Applet, SW_SHOWNORMAL) <> -1;
end;

procedure SetModifyDate(fName, fDate: string);
{ Proc to modify the Modify Date of a file.                            }
{ Usage: SetModifyDate('C:\Temp\SomeFile.txt', '12/31/1998 00:00:00'); }
var
  Age,
  FHandle: integer;
  LocalFileTime,
  FileTime: TFileTime;
  FileBuf: file;
begin
  if FileExists(fName) then
    begin
      AssignFile(FileBuf, fName);
      Reset(FileBuf);
      try
        Age := DateTimeToFileDate(StrToDateTime(fDate));
        FHandle := TFileRec(FileBuf).Handle;
        DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime);
        LocalFileTimeToFileTime(LocalFileTime, FileTime);
        // to set the files' last modified date
        SetFileTime(FHandle, nil, nil, @FileTime);
        // to set the files' created date
        SetFileTime(FHandle, @FileTime, nil, nil);
        // to set the files' last accessed date
        SetFileTime(FHandle, nil, @FileTime, nil);
      finally
        CloseFile(FileBuf);
     end; 
    end
  else
    MessageDlg('File does not exist: ' + fName, mtError, [mbOK], 0);
end;

// Here's how to create a shortcut on the desktop, start menu, send to menu, or
// quick launch toolbar. Simple usage is like this:
// CreateShortcut('c:\winnt\system32\notepad.exe', _DESKTOP);
// to create a shortcut of Notepad.exe on the desktop.

procedure CreateShortcut(FileName: string; Location: ShortcutType);
{ proc to create a shortcut on the desktop or startmenu. }
var
  MyObject : IUnknown;
  MySLink : IShellLink;
  MyPFile : IPersistFile;
  Directory,
  LinkName : string;
  WFileName : WideString;
  MyReg,
  QuickLaunchReg : TRegIniFile;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;

  MySLink.SetPath(PChar(FileName));

  MyReg := TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
  try
    LinkName := ChangeFileExt(FileName, '.lnk');
    LinkName := ExtractFileName(LinkName);
    case Location of
      _DESKTOP    : Directory := MyReg.ReadString('Shell Folders', 'Desktop', '');
      _STARTMENU  : Directory := MyReg.ReadString('Shell Folders', 'Start Menu', '');
      _SENDTO     : Directory := MyReg.ReadString('Shell Folders', 'SendTo', '');
      _QUICKLAUNCH:
        begin
          QuickLaunchReg := TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\GrpConv');
          try 
            Directory := QuickLaunchReg.ReadString('MapGroups', 'Quick Launch', '');
          finally 
            QuickLaunchReg.Free; 
          end; 
        end;
    end;

    if Directory <> '' then
      begin
        WFileName := Directory + '\' + LinkName;
        MyPFile.Save(PWChar(WFileName), False);
      end;
  finally
    MyReg.Free;
  end;
end;

function GetOnlineStatus : Boolean;
var ConTypes : Integer;
begin
  ConTypes := INTERNET_CONNECTION_MODEM + INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
  if (InternetGetConnectedState(@ConTypes, 0) = False) then Result := False else Result := True;
end;

var List: TStringList;

// To get a list of all runing apps then use EnumWindows.
function EnumWindowsProc(HW: HWND; var Data: lParam): bool; stdcall;
var Caption: String;
begin
 Result := True;
 if (GetWindowLong(hw, GWL_HWNDPARENT) = 0) then begin
  SetLength(Caption, 100);
  GetWindowText(hw, PChar(Caption), 100);
  SetLength(Caption, StrLen(PChar(Caption)));
  List.Add(Caption);  // Add apps caption to the list
 end;
end; { EnumProc }

function GetRunningApps: TStringList;
begin
 List := TStringList.Create;
 EnumWindows(@EnumWindowsProc, 0);
 Result := List;
end;

function GetUsername: String;
var
  Buffer: array[0..255] of Char;
  Size: DWord;
begin
  Size := SizeOf(Buffer);
  if not Windows.GetUserName(Buffer, Size) then
    RaiseLastOSError; //RaiseLastWin32Error; {Bis D5};
  SetString(Result, Buffer, Size - 1);
end;

function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
  (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;

var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result := False;
  bSuccess:=False;
  ptgGroups:=nil;
  psidAdministrators:=nil;
  try
    bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
      hAccessToken);
    if not bSuccess then
    begin
      if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
        hAccessToken);
    end;
    if bSuccess then
    begin
      GetMem(ptgGroups, 1024);
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
        ptgGroups, 1024, dwInfoBufferSize);
      if bSuccess then
      begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
          SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
          0, 0, 0, 0, 0, 0, psidAdministrators);
        {$R-}
        for x := 0 to ptgGroups.GroupCount - 1 do
          if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
          begin
            Result := True;
            Break;
          end;
        {$R+}
      end;
    end;
  finally
    if bSuccess then
      CloseHandle(hAccessToken);
    if Assigned(ptgGroups) then
      FreeMem(ptgGroups);
    if Assigned(psidAdministrators) then
      FreeSid(psidAdministrators);
  end;
end;

function GetWinVersion: string;
begin
  result:='Unbekannte Version';
  case Win32Platform of
    1:// 9x-Reihe
      If Win32MajorVersion=4 Then Begin
        Case Win32MinorVersion of
            0: result:='Windows 95';
            10: result:='Windows 98';
            90: result:='Windows Me';
        end;
      end;
  2: // NT-Reihe
     Case Win32MajorVersion of
         3:IF Win32MinorVersion=51 then
              result:='Windows NT 3.51';
         4:If Win32MinorVersion=0 then
             result:='Windows NT 4';
         5:Case Win32MinorVersion of
              0: result:='Windows 2000';
              1: result:='Windows XP';
              2: result:='Windows Server 2003';
           end;
         6:Result:='Windows Vista / Windows Server "Longhorn"';
     End;
  end;
  //Win32CSDVersion enthält Informationen zu Servicepacks
  if Win32CSDVersion<>'' then
    result:=result+' '+Win32CSDVersion;
end;

end.
