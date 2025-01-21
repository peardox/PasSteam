unit castlefilesutils;

interface

{$IFDEF MACOS}{$define DARWIN}{$ENDIF}

uses {$IFDEF MACOS}Macapi.CoreFoundation, {$ENDIF}{$ifdef fpc}{$ifdef darwin}MacOSAll,{$endif}{$endif}
  SysUtils, CastleLog;

{$if defined(darwin)}
var
  BundlePathCached: Boolean;
  BundlePathCache: string;

{ Main directory of the current macOS bundle, including final slash.
  Empty string if we're not run from a bundle. }
function BundlePath: string;

function InclPathDelim(const s: string): string;

{$endif}

implementation
{$if defined(darwin)}
function InclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS) and not defined(FPC)}
  { On Windows, also accept / as final path delimiter.
    FPC does it automatically. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(S);
  {$endif}
  Result := IncludeTrailingPathDelimiter(S);
end;

function BundlePath: string;
{ Based on
  http://wiki.freepascal.org/OS_X_Programming_Tips#How_to_obtain_the_path_to_the_Bundle }
var
  bundle: CFBundleRef;
  pathRef: CFUrlRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
begin
  if not BundlePathCached then
  begin
    bundle := CFBundleGetMainBundle();
    if bundle = nil then
    begin
      BundlePathCache := '';
      WritelnLog('We cannot detect our macOS AppBundle. Probably the application was run directly (like a Unix application, without being wrapped in a directory like "xxx.app"). Some GUI features (like application menu) will not work without running through AppBundle.');
    end else
    begin
      pathRef := CFBundleCopyBundleUrl(bundle);
      pathCFStr := CFUrlCopyFileSystemPath(pathRef, kCFUrlPOSIXPathStyle);
      CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
      CFRelease(pathRef);
      CFRelease(pathCFStr);
      BundlePathCache := String(pathStr);
      {$ifdef FPC}
      BundlePathCache := InclPathDelim(BundlePathCache);
      {$else}
      BundlePathCache := InclPathDelim(BundlePathCache);
      {$endif}
    end;
    BundlePathCached := true;
  end;
  Result := BundlePathCache;
end;
{$endif}

end.

