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

{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }
unit CastleSteam;

{$I castleconf.inc}

interface

{$define CASTLE_DEBUG_STEAM_API_TESTING}

uses Classes, CTypes, SysUtils,
  {$ifndef fpc}System.Generics.Collections,{$else}Contnrs,Generics.Collections,{$endif}
  SteamTypes,
  SteamSubSystem,
  SteamApps,
  SteamInput,
  SteamFriends,
  SteamUser,
//  SteamUserStats,
  SteamUtils,
  CastleInternalSteamApi;

type
  TAppId = SteamTypes.TAppId;
  TAchievementChanged = (AchievedChanged, IconChanged, ImageChanged);
  TSteamAchievement = Class;

  TAchievementUpdatedEvent = procedure(AValue: TSteamAchievement; const WhatChanged: TAchievementChanged) of object;

  TCastleSteam = class;
  // Forward declaration for TSteamAchievement.Create

  { TSteamAchievement encapulates a Steam Achievement

    The only value Steam REALLY cares about is the Achievement Key ID (FKey)

    Steam does, however, allow the retrieval of a lot of other Achievement-related
    fields that may be useful to some developer

    The only thing you can really do with an Achievement is set or clear it
    nence only the Achieved property is writable
  }

  TSteamAchievement = Class
    strict private
      FOwner: TObject;
      FAchId: UInt32;
      FKey: String;
      FName: String;
      FDesc: String;
      FHidden: Boolean;
      FAchieved: Boolean;
      FDoneDate: TDateTime;
      FIcon: CInt;
      FOnAchievementUpdated: TAchievementUpdatedEvent;
      procedure SetAchieved(const AChecked: Boolean);
    protected
      function GetIcon(SteamUserStats: Pointer; AchievementId: UInt32): CInt;
      { Makes a bunch of Steam API calls to fill in all the properties }
      procedure Populate(SteamUserStats: Pointer; AchievementId: UInt32);
    public
      constructor Create(AOwner: TCastleSteam);
      destructor Destroy; override;
      { Read-Only : The Achievement API Key - Really important to steam }
      property Key: String read FKey;
      { Read-Only : The Achievement Name }
      property Name: String read FName;
      { Read-Only : The Achievement Description }
      property Desc: String read FDesc;
      { Read-Only : If the achievemnt is hidden from the user or not }
      property Hidden: Boolean read FHidden;
      { This will set or clear the achievment status which will also
        trigger a Steam API that may send a Steaam Notification to
        the user that the Achievement has been completed (a little
        notification window pops-up in game if permitted)
      }
      property Achieved: Boolean read FAchieved write SetAchieved;
      { Read-Only : When the Achievement was completed }
      property DoneDate: TDateTime read FDoneDate;
      { Read-Only : Achievement Icon handle, used to fetch the actual image.
        This may trigger an asynchronous fetch if the actual bitmap isn't
        available (which we catch and handle to for you)
      }
      property Icon: CInt read FIcon write FIcon;
      property OnAchievementUpdated: TAchievementUpdatedEvent
        read FOnAchievementUpdated write FOnAchievementUpdated;

  end;

  { TIndexedAchievementList is a replacement for the previous TStringList version

    This is enabled by a Generic class (TIndexedObjectList) that mixes
    a TObjectList with a TObjectDictionary

    The TObjectlist (FList) owns all the TSteamAchievement objects and
    the TObjectDictionary (FDict) owns nothing but references the Achievements
    by their Api Key ID (FKey)

    The overhead is minimal as there's only one set of TSteamAchievement Objects

    The benefit is that the Achievements can be iterated over in a loop or
    directly accessed by their Key via FindKey which is the way that Steam
    refers to the Achievements in any API calls

    Strictly speaking this object is for convenience only as the Steam API
    really expects you to already know all the Key IDs anyway
  }

  TIndexedObjectList<V: class> = class
  strict private
    FList: {$ifdef fpc}specialize{$endif} TObjectList<V>;
    FDict: {$ifdef fpc}specialize{$endif} TObjectDictionary<String, V>;
    FCapacity: NativeInt;
    function GetObject(Index: Integer): V;
    procedure SetObject(Index: Integer; AValue: V);
    function GetObjectCount: NativeInt;
    procedure SetCapacity(const AValue: NativeInt);
  public
    { Creates the list then dict }
    constructor Create;
    { Destroys the dict then the list }
    destructor Destroy; override;
    { Add a new TObj to both the list and dict }
    function Add(const AKey: String; const Value: V): NativeInt;
    { Returns the TObj for a specified API Key or Nil of it doesn't exist}
    function FindKey(const AKey: String): V;
    { Clear dict then list }
    procedure Clear;
    { Set the Capacity of list and dict }
    property Capacity: NativeInt read FCapacity write SetCapacity;
    { Returns number of items in list/dict }
    property Count: NativeInt read GetObjectCount;
    { Get or Set TObj by numerical index. Default property }
    property Objects[Index: Integer]: V read GetObject write SetObject; default;
  end;

  TSteamAchievementList = TIndexedObjectList<TSteamAchievement>;

  { Integration with Steam.
    See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
    for usage.
    Create an instance of this class (passing to the constructor
    the Steam application id), observe properties like @link(Enabled)
    and @link(UserStatsReceived), and call methods like @link(SetAchievement)
    to interact with Steam API. }
  TCastleSteam = class
  strict private
    FAppId: TAppId;
    FEnabled: Boolean;
    FUserID: CUserID;
    FSteamApps: TSteamApps;
    FSteamFriends: TSteamFriends;
    FSteamInput: TSteamInput;
    FSteamUser: TSteamUser;
    FSteamUtils: TSteamUtils;
    FAchievements: TSteamAchievementList;
    FUserStatsReceived: Boolean;
    FOnUserStatsReceived: TNotifyEvent;
    StoreStats: Boolean;
    // Pointers for ISteam....
    SteamUserStats: Pointer;
    procedure CallbackUserStatsReceived(P: PUserStatsReceived);
    procedure CallbackUserAchievementIconFetched(P: PUserAchievementIconFetched);
    procedure GetAchievements;
    { Makes a warning.

      For now if some operation failed, we log an error using @link(WriteLnWarning).
      If it will be useful in the future, we could also raise an exception
      here, to make the issue more prominent.
      But it seems just logging the error is more standard,
      it's also the practice of SteamWorks examples. This way failure
      to communicate with Steam doesn't stop the game, it just logs a warning. }
    procedure SteamError(const ErrorMsg: String);
    { Runs callbacks, performs saving of user stats.
      According to SteamWorks documentation you should run this
      at least 10 times a second, better if every frame. }
    procedure Update(Sender: TObject);
    { Callback may be set after event is available so trigger it if ready
      As of 1.61 Stats are no longer async so this callback would not happen
      without this setter }
    procedure SetOnUserStatsReceived(const AValue: TNotifyEvent);
    function SteamVerifyLoad(P: Pointer; const S: String): Boolean;
  protected
    SteamClient: Pointer;
    SteamUserHandle: HSteamUser;
    SteamPipeHandle: HSteamPipe;
  public
    { Connect to Steam and initialize everything.

      @orderedList(
        @item(It tries to connect to Steam API and checks if the game was run
          through Steam or through exe file.

          In the latter case the game will
          automatically restart through Steam.
          See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
          for a description how does the Steam
          integration behave, when it restarts the game.
        )

        @item(Will request user stats from Steam.

          Only once user stats are received, you can use achievements.
          Observe @link(UserStatsReceived) or use event @link(OnUserStatsReceived)
          to know when it happens.
        )
      )
    }
    constructor Create(const AAppId: TAppId);
    destructor Destroy; override;

    property Apps: TSteamApps read FSteamApps;
    property Friends: TSteamFriends read FSteamFriends;
    property Input: TSteamInput read FSteamInput;
    property User: TSteamUser read FSteamUser;
    property Utils: TSteamUtils read FSteamUtils;

    { Steam application id, given when creating this. }
    property AppId: TAppId read FAppId;

    { Do we have Steam integration available.

      If @true, this means that Steam dynamic library was found,
      and we don't need to restart the application.
      See https://castle-engine.io/steam for more information.

      This is the normal state of things when the game is run through Steam.
      It means that all features of this class work as they should. }
    property Enabled: Boolean read FEnabled;

    // Public Interface Access
    property ISteamUserStats: Pointer read SteamUserStats;

    { Achievement names for this game (read-only).
      These are IDs of achievements, can be used in other calls
      like @link(SetAchievement) or @link(GetAchievement).
      This field is initialized when @link(UserStatsReceived) becomes @true,
      it is @nil before. }
    property Achievements: TSteamAchievementList read FAchievements;

    { Have we received user stats from Steam.
      Before this is @true, methods of this class dealing with user stats
      (like achievements) don't do anything. }
    property UserStatsReceived: Boolean read FUserStatsReceived;

    { The currently logged in Steam User's ID }
    property UserId: CUserID read FUserId;

    { We have received user stats from Steam.
      Right before calling this event, @link(UserStatsReceived) changed to @true
      and @link(Achievements) have been filled with achievement names.
      From now on you can use Steam features that depend on it,
      like @link(Achievements). }
    property OnUserStatsReceived: TNotifyEvent
      read FOnUserStatsReceived write SetOnUserStatsReceived;

    { Set achievement as "achieved",
      Steam will automatically show a small overlay indicating that the
      achievement is obtained. }
    procedure SetAchievement(const AchievementId: String);

    { Is this achievement "achieved". }
    function GetAchievement(const AchievementId: String): Boolean;

    { Set achievement as "not achieved".
      Should not be necessary for normal usage (the convention is that
      once users achieve something, it stays achieved forever).
      But this is useful for testing purposes -- you may want to clear own
      achievements during testing. }
    procedure ClearAchievement(const AchievementId: String);

    { Clears all achievements from the connected user.
      @seealso ClearAchievement }
    procedure ClearAllAchievements;

    { Show Steam overlay "progress towards achievement" e.g. "Wins 33/100".

      Don't use this when achievement is already achieved.
      Doing so will only result in an error from Steam, visible as a warning in logs:
      "Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress".

      Calling this with CurrentProgress >= MaxProgress @italic(does not)
      mark the achievement as achieved. It only shows the progress in the overlay.
      You still have to call @link(SetAchievement) to make it achieved. }
    procedure IndicateAchievementProgress(const AchievementId: String;
      const CurrentProgress, MaxProgress: UInt32);
  end;

implementation

uses DateUtils,
  CastleLog, CastleUtils, CastleApplicationProperties;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
var
  DebugTextAnsi: AnsiString;
begin
  DebugTextAnsi := pchDebugText;
  WriteLnLog('Steam Warning: "%s" (Severity: %d)', [DebugTextAnsi, NSeverity]);
end;

{ TCastleSteam --------------------------------------------------------------- }

constructor TCastleSteam.Create(const AAppId: TAppId);

  { Set some conditions and set Enabled.
    It may also restart the game through Steam if it was run through exe,
    which means it will Halt the current process. }
  procedure CheckEnabledAndRestart;
  var
    InitMsg: Array[0..1023] of Char;
  begin
    //SetLength(InitMsg, 1024);
    FEnabled := false;
    { Is Steam library available at runtime. }
    if SteamLibrary <> nil then
    begin
     {$if STEAM_API_VERSION >= 1.61}
     WriteLnLog('SteamAPI using new');
     {$elseif STEAM_API_VERSION < 1.61}
     WriteLnLog('SteamAPI using old');
     {$endif}
      // Initialize Steam API
      {$if STEAM_API_VERSION >= 1.61}
      if SteamAPI_InitFlat(@InitMsg) = k_ESteamAPIInitResult_OK then
      {$else}
      if SteamAPI_Init() then
      {$endif}
      begin
        {$if STEAM_API_VERSION = 1.61}
        WriteLnLog('SteamAPI with USE_TESTING_API using ' + SteamLibraryName);
        {$else}
        WriteLnLog('SteamAPI using ' + SteamLibraryName);
        {$endif}
        WriteLnLog('SteamAPI_Init successful');

        // If the app was started through EXE - restart the game through Steam
        if SteamAPI_RestartAppIfNecessary(AppId) then
        begin
          WriteLnLog('The app was run through exe directly (not by clicking "Play" in Steam), restarting through Steam.');
          Halt(0);
        end else
        begin
          FEnabled := true;
          WriteLnLog('The app was started through Steam (or runs in developer mode with steam_appid.txt), Steam integration enabled OK.');
        end;
      end else
        WriteLnWarning(String(PAnsiChar(@InitMsg)) + SLineBreak + 'SteamAPI_Init failed. This means Steam does not run in the background, but you run the application in development mode (with steam_appid.txt). In this case Steam integration will not work. See https://castle-engine.io/steam for information how to test the Steam integration.');
    end else
    begin
      {$warnings off} // ignore FPC warnings about unreachable code, since SteamLibraryName is constant
      if SteamLibraryName <> '' then
        WriteLnWarning('Steam dynamic library "%s" not found', [SteamLibraryName])
      else
        WriteLnWarning('Steam is not supported on this platform');
      {$warnings on}
    end;
  end;

  procedure InitialSteamCalls;
  begin
    if not Enabled then
      Exit;
    SteamClient := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

    // Set a callback for Steam warnings
    SteamAPI_ISteamClient_SetWarningMessageHook(SteamClient,
      {$ifdef FPC}@{$endif} WarningHook);

    SteamUserHandle := SteamAPI_GetHSteamUser();
    SteamPipeHandle := SteamAPI_GetHSteamPipe();

    // Init SteamUser
    FSteamUser := TSteamUser.Create(SteamClient, SteamUserHandle, SteamPipeHandle);
    {
    SteamUser := SteamAPI_ISteamClient_GetISteamUser(
       SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSER_INTERFACE_VERSION);
    SteamVerifyLoad(SteamUser, 'SteamUser');
    }
    // Init SteamApps
    FSteamApps := TSteamApps.Create(SteamClient, SteamUserHandle, SteamPipeHandle);

    // Init SteamFriends
    FSteamFriends := TSteamFriends.Create(SteamClient, SteamUserHandle, SteamPipeHandle);

    // Init SteamUtils
    FSteamUtils := TSteamUtils.Create(SteamClient, SteamUserHandle, SteamPipeHandle);

    // Init SteamInput
    FSteamInput := TSteamInput.Create(SteamClient, SteamUserHandle, SteamPipeHandle);

    // Init SteamUserStats and request UserStats (will wait for callback, handled in Update)
    SteamUserStats := SteamAPI_ISteamClient_GetISteamUserStats(
      SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
    SteamVerifyLoad(SteamUserStats, 'SteamUserStats');

    FUserId := User.SteamID;
    {$if STEAM_API_VERSION < 1.61}
    SteamAPI_ISteamUserStats_RequestCurrentStats(SteamUserStats);
    {$else}
    if SteamUserStats <> Nil then
      begin
        GetAchievements;
        FUserStatsReceived := true;
        if Assigned(OnUserStatsReceived) then
          OnUserStatsReceived(Self);
      end;
    {$endif}

    SteamAPI_ManualDispatch_Init();
  end;

begin
  inherited Create;
  FAppId := AAppId;

  StoreStats := false;
  FUserStatsReceived := false; // waiting for callback

  InitializeSteamLibrary;
  CheckEnabledAndRestart;
  if FEnabled then
    begin
      InitialSteamCalls;
      ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif} Update);
    end;

end;

destructor TCastleSteam.Destroy;
begin
  FreeAndNil(FAchievements);
  if Enabled then
    begin
      if Assigned(FSteamApps) then
        FreeAndNil(FSteamApps);
      if Assigned(FSteamFriends) then
        FreeAndNil(FSteamFriends);
      if Assigned(FSteamInput) then
        FreeAndNil(FSteamInput);
      if Assigned(FSteamUser) then
        FreeAndNil(FSteamUser);
      if Assigned(FSteamUtils) then
        FreeAndNil(FSteamUtils);
      SteamAPI_ISteamClient_BReleaseSteamPipe(SteamClient, SteamPipeHandle);
      SteamAPI_Shutdown();
    end;
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef FPC}@{$endif} Update);
  inherited;
end;

procedure TCastleSteam.CallbackUserAchievementIconFetched(
  P: PUserAchievementIconFetched);
var
  S: String;
  Found: TSteamAchievement;
begin
  if P{$ifdef fpc}^{$endif}.m_nIconHandle = 0 then
    begin
      {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
      WriteLnLog('Steam', 'No Icon Available');
      {$endif}
      Exit;
    end;

  S := String(P{$ifdef fpc}^{$endif}.m_rgchAchievementName);
  if Length(S) > 0 then
    begin
      Found := FAchievements.FindKey(S);
      if Found <> Nil then

        begin
          {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
          WriteLnLog('Image ==>', 'Fetched UserAchievementIcon from Steam for %s with Handle %d',
          [S, (P^).m_nIconHandle]);
          {$endif}
          Found.Icon := P{$ifdef fpc}^{$endif}.m_nIconHandle;
          if Assigned(Found.OnAchievementUpdated) then
            Found.OnAchievementUpdated(Found, ImageChanged);
        end;

    end;
end;

procedure TCastleSteam.CallbackUserStatsReceived(P: PUserStatsReceived);
begin
  WriteLnLog('Steam', 'Received UserStats from Steam');
  FUserStatsReceived := true;
  GetAchievements;
  if Assigned(OnUserStatsReceived) then
    OnUserStatsReceived(Self);
end;

procedure TCastleSteam.GetAchievements;
var
  NumAchievements: UInt32;
  I: Integer;
  SteamAchievement: TSteamAchievement;
begin
  if not Enabled then
    Exit;

  FAchievements := TSteamAchievementList.Create;

  NumAchievements := SteamAPI_ISteamUserStats_GetNumAchievements(SteamUserStats);
  if NumAchievements > 0 then
    for I := 0 to NumAchievements - 1 do
      begin
        SteamAchievement := TSteamAchievement.Create(Self);
        SteamAchievement.Populate(SteamUserStats, I);
        FAchievements.Add(SteamAchievement.Key, SteamAchievement);
      end;
  WriteLnLog('Steam Achievements: %d', [Achievements.Count]);
end;

procedure TCastleSteam.SteamError(const ErrorMsg: String);
begin
  WriteLnWarning(ErrorMsg);
end;

function TCastleSteam.SteamVerifyLoad(P: Pointer; const S: String): Boolean;
begin
  Result := True;
  if P = Nil then
    begin
      SteamError(Format('Failed to load %s', [S]));
      Result := False;
    end;
end;

const
  SUserStatsNotReceived = 'User stats not received yet from Steam (wait for TCastleSteam.UserStatsReceived or observe TCastleSteam.OnUserStatsReceived).';

procedure TCastleSteam.SetAchievement(const AchievementId: String);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_SetAchievement(SteamUserStats, PAnsiChar(AchievementIdAnsi)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_SetAchievement');
    StoreStats := true;
  end else
    SteamError('SetAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.SetOnUserStatsReceived(const AValue: TNotifyEvent);
begin
  FOnUserStatsReceived := AValue;
  if Assigned(FOnUserStatsReceived) and FUserStatsReceived then
    FOnUserStatsReceived(Self);
end;

function TCastleSteam.GetAchievement(const AchievementId: String): Boolean;
var
  CAchieved: TSteamBool;
  AchievementIdAnsi: AnsiString;
begin
  Result := false;
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if SteamAPI_ISteamUserStats_GetAchievement(SteamUserStats,
        PAnsiChar(AchievementIdAnsi), @CAchieved) then
    begin
      Result := CAchieved;
    end else
      SteamError('Failed to SteamAPI_ISteamUserStats_GetAchievement');
  end else
    SteamError('GetAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAchievement(const AchievementId: String);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_ClearAchievement(SteamUserStats, PAnsiChar(AchievementIdAnsi)) then
      SteamError('Failed to SteamAPI_ISteamUserStats_ClearAchievement');
    StoreStats := true;
  end else
    SteamError('ClearAchievement failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.ClearAllAchievements;
begin

  if not Enabled then
    Exit;
  if UserStatsReceived then
    FAchievements.Clear;

end;

procedure TCastleSteam.IndicateAchievementProgress(const AchievementId: String;
  const CurrentProgress, MaxProgress: UInt32);
var
  AchievementIdAnsi: AnsiString;
begin
  if not Enabled then
    Exit;
  if UserStatsReceived then
  begin
    AchievementIdAnsi := AnsiString(AchievementId);
    if not SteamAPI_ISteamUserStats_IndicateAchievementProgress(SteamUserStats,
       PAnsiChar(AchievementIdAnsi), CurrentProgress, MaxProgress) then
      SteamError('Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress');
    StoreStats := true; // not really necessary it seems
  end else
    SteamError('IndicateAchievementProgress failed. ' + SUserStatsNotReceived);
end;

procedure TCastleSteam.Update(Sender: TObject);

  { Request callbacks from Steam API if any pending.

    We don't use SteamAPI_RunCallbacks, as it
    - Requires quite hacky code, to simulate C++ class VMT, with a specially
      crafted record and assembler help.
      See
      https://github.com/castle-engine/castle-engine/blob/af09fba831c76db45dde7e0d230c1304dffbd4d3/src/services/steam/castleinternalsteamcallback.pas
      that we removed in
      https://github.com/castle-engine/castle-engine/commit/28d9a2e4558b0b7f66c77c5e93f47bddec186285 .
    - Crashes on Linux/x86_64 (maybe because of the above).

    Following the Steamworks docs recommendation, we use manual dispatching,
    see https://partner.steamgames.com/doc/sdk/api#manual_dispatch
    and example code at SteamAPI_ManualDispatch_Init.
  }
  procedure SteamRunCallbacks;
  { Extra logging of unhandled Steam callbacks (it's normal that we have some). }
  {$define CASTLE_DEBUG_STEAM_CALLBACKS}
  var
    Callback: TCallbackMsg;
    PCallCompleted: PSteamAPICallCompleted;
    PTmpCallResult: Pointer;
    BFailed: TSteamBool;
    CallbackHandled: Boolean;
  begin
//    if Assigned(FSteamInput) and FSteamInput.Enabled then
//      FSteamInput.RunFrame();
    SteamAPI_ManualDispatch_RunFrame(SteamPipeHandle);
    while SteamAPI_ManualDispatch_GetNextCallback(SteamPipeHandle, @Callback) do
    begin
      // Look at callback.m_iCallback to see what kind of callback it is,
      // and dispatch to appropriate handler(s)
//      WritelnWarning('Steam', 'Got Callback : ' + IntToStr(Callback.m_iCallback));
      CallbackHandled := False;
      if Not(CallbackHandled) and Assigned(FSteamInput) then
        CallbackHandled := FSteamInput.RunCallback(Callback);
      if Not(CallbackHandled) and Assigned(FSteamApps) then
        CallbackHandled := FSteamApps.RunCallback(Callback);
      if Not(CallbackHandled) and Assigned(FSteamFriends) then
        CallbackHandled := FSteamFriends.RunCallback(Callback);
      if Not(CallbackHandled) and Assigned(FSteamUtils) then
        CallbackHandled := FSteamUtils.RunCallback(Callback);
{
      if Not(CallbackHandled) and Assigned(FSteamUser) then
        CallbackHandled := FSteamUser.RunCallback(Callback);
      if Not(CallbackHandled) and Assigned(FSteamUserStats) then
        CallbackHandled := FSteamUser.RunCallbackStats(Callback);
}
      if CallbackHandled = False then
        begin
          case Callback.m_iCallback of
            TSteamAPICallCompleted.k_iCallback:
              begin
                // TODO: remove this warning once this code is tested
                WritelnWarning('Steam', 'SteamAPICallCompleted callback: TODO untested handling');

                // Check for dispatching API call results
                PCallCompleted := PSteamAPICallCompleted(Callback.m_pubParam);
                PTmpCallResult := GetMem(Callback.m_cubParam);
                if SteamAPI_ManualDispatch_GetAPICallResult(SteamPipeHandle,
                    PCallCompleted^.m_hAsyncCall, PTmpCallResult, Callback.m_cubParam,
                    Callback.m_iCallback, @BFailed) then
                begin
                  { Dispatch the call result to the registered handler(s) for the
                    call identified by pCallCompleted^.m_hAsyncCall

                    Note (CGE): The piece of code handling SteamAPICallCompleted
                    is adjusted from the example code in C API about
                    SteamAPI_ManualDispatch_GetNextCallback.
                    But right now, we don't have any need for this,
                    and the log below never happens in our Steam usage so far.
                  }
                  {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
                  WritelnLog('Steam', 'Dispatch the call result to the handlers for m_iCallback %d, m_hAsyncCall %d', [
                    PCallCompleted^.m_iCallback,
                    PCallCompleted^.m_hAsyncCall
                  ]);
                  {$endif}
                end;
                FreeMem(PTmpCallResult);
              end;
            TUserStatsReceived.k_iCallback:
              begin
                {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
                WritelnLog('Steam', 'Handle Stats Received (m_iCallback : %d)', [Callback.m_iCallback]);
                {$endif}
                CallbackUserStatsReceived(PUserStatsReceived(Callback.m_pubParam));
              end;
            TUserAchievementIconFetched.k_iCallback:
              begin
                if(SizeOf(TUserAchievementIconFetched) <> Callback.m_cubParam) then
                  WritelnLog('Callbacks', 'TUserAchievementIconFetched Size = %d, should be %d)', [SizeOf(TUserAchievementIconFetched), Callback.m_cubParam])
                else
                  CallbackUserAchievementIconFetched(PUserAchievementIconFetched(Callback.m_pubParam));
              end;
            else
              begin
                {$ifdef CASTLE_DEBUG_STEAM_CALLBACKS}
                WritelnLog('Steam', 'Callback m_iCallback %d, we ignore it now', [
                  Callback.m_iCallback
                ]);
                {$endif}
              end;
          end;
      end;
      // We must call this before next SteamAPI_ManualDispatch_GetNextCallback.
      SteamAPI_ManualDispatch_FreeLastCallback(SteamPipeHandle);
    end;
  end;

begin
  if not Enabled then
    Exit;

  SteamRunCallbacks;

  // If we have unsaved changes, try saving them; if failed - repeat
  if UserStatsReceived and StoreStats then
    if SteamAPI_ISteamUserStats_StoreStats(SteamUserStats) then // repeat it every Update until success
      StoreStats := false;
end;

{ TSteamAchievement }

constructor TSteamAchievement.Create(AOwner: TCastleSteam);
begin
  Inherited Create;
  FOwner := AOwner;
end;

destructor TSteamAchievement.Destroy;
begin
  inherited;
end;

function TSteamAchievement.GetIcon(SteamUserStats: Pointer; AchievementId: UInt32): CInt;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementIcon(SteamUserStats, PAnsiChar(AnsiString(FKey)));
  if Result <> 0 then
    begin
      {$if defined(CASTLE_DEBUG_STEAM_API_TESTING)}
      WritelnLog('GetAchievementIcon returned Icon Handle %d for %s', [Result, FKey]);
      {$endif}
    end
  else
    WritelnLog('GetAchievementIcon Pending');
end;

procedure TSteamAchievement.Populate(SteamUserStats: Pointer; AchievementId: UInt32);
var
  pchName: PAnsiChar;
  pchHidden: PAnsiChar;
  bAchieved: TSteamBool;
  uDate: UInt32;
begin
  FAchId := AchievementId;
  pchName := SteamAPI_ISteamUserStats_GetAchievementName(SteamUserStats, AchievementId);
  FKey := String(pchName);
  FName := String(SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'name'));
  FDesc := String(SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'desc'));
  pchHidden := SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute(SteamUserStats, pchName, 'hidden');
  if CompareStr(String(pchHidden), '1') = 0 then
    FHidden := True
  else
    FHidden := False;
  SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime(SteamUserStats, pchName, @bAchieved, @uDate);
  FAchieved := bAchieved;
  FDoneDate := UnixToDateTime(uDate);
  FIcon := GetIcon(SteamUserStats, AchievementId);

end;

procedure TSteamAchievement.SetAchieved(const AChecked: Boolean);
var
  NIcon: CInt;
begin
  if not(FOwner is TCastleSteam) then
    Exit;

  FAchieved := AChecked;
  if FAchieved then
    TCastleSteam(FOwner).SetAchievement(FKey)
  else
    TCastleSteam(FOwner).ClearAchievement(FKey);
  NIcon := GetIcon(TCastleSteam(FOwner).ISteamUserStats, FIcon);
  if (NIcon <> FIcon) then
    begin
      FIcon := NIcon;
      if Assigned(OnAchievementUpdated) then
        OnAchievementUpdated(Self, IconChanged);
    end;
end;

{ TIndexedObjectList<TObj> }

function TIndexedObjectList<V>.Add(
  const AKey: String; const Value: V): NativeInt;
begin
  Result := FList.Add(Value);
  FDict.Add(AKey, Value);
end;

procedure TIndexedObjectList<V>.Clear;
begin
  FDict.Clear;
  FList.Clear;
end;

constructor TIndexedObjectList<V>.Create;
begin
  FList := TObjectList<V>.Create;
  FDict := TObjectDictionary<String, V>.Create();
end;

destructor TIndexedObjectList<V>.Destroy;
begin
  FreeAndNil(FDict);
  FreeAndNil(FList);
  inherited;
end;

function TIndexedObjectList<V>.FindKey(
  const AKey: String): V;
begin
  if not FDict.TryGetValue(AKey, Result) then
    Result := Nil;
end;

function TIndexedObjectList<V>.GetObject(Index: Integer): V;
begin
  Result := FList[Index];
end;

function TIndexedObjectList<V>.GetObjectCount: NativeInt;
begin
  Result := FList.Count;
end;

procedure TIndexedObjectList<V>.SetCapacity(const AValue: NativeInt);
begin
  FCapacity := AValue;
  FList.Capacity := FCapacity;
  FDict.Capacity := FCapacity;
end;

procedure TIndexedObjectList<V>.SetObject(Index: Integer; AValue: V);
begin
  FList[Index] := AValue;
end;

end.

