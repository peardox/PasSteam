unit OSHelp;

interface

{$if defined(MSWINDOWS)}
uses
  Windows, TLHelp32, SysUtils;

  function ProcessRunning (sExeName: String) : Boolean;
{$endif}


implementation

{$if defined(MSWINDOWS)}
function ProcessRunning (sExeName: String) : Boolean;
{ -> sExeName : Name of the EXE without path. Does not have to be the full EXE name. }

var
    hSnapShot : THandle;
    ProcessEntry32 : TProcessEntry32;

begin
    Result := false;

    hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);
    Win32Check (hSnapShot <> INVALID_HANDLE_VALUE);

    sExeName := LowerCase (sExeName);

    FillChar (ProcessEntry32, SizeOf (TProcessEntry32), #0);
    ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

    if (Process32First (hSnapShot, ProcessEntry32)) then
        repeat
            if (Pos (sExeName,
                     LowerCase (ProcessEntry32.szExeFile)) = 1) then
            begin
                Result := true;
                Break;
            end; { if }
        until (Process32Next (hSnapShot, ProcessEntry32) = false);

    CloseHandle (hSnapShot);
end;
{$endif}

end.
