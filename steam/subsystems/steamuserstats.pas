{
  Copyright 2023-2024 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit steamuserstats;
{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TSteamUserStats = class(TSteamSubsystem)
  private
    function CallbackHandler(const C: TCallbackMsg): Boolean;
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
  end;

implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamUserStats }

constructor TSteamUserStats.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamUserStats(
    SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
  FOnCallback := CallbackHandler;
end;

destructor TSteamUserStats.Destroy;
begin
  inherited;
end;

// Callbacks

function TSteamUserStats.CallbackHandler(const C: TCallbackMsg): Boolean;
begin
  Result := False;
end;

end.

