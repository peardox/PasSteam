unit steamtypes;

interface

{ STEAM_API_VERSION allows switching between an established tested API
  and a newer API that is being tested. It makes upgrading between two
  APIs far simpler as when defined an exception may be raised when
  loading the library. The 'testing' library should be renamed to
  include it's version as a suffix e.g. steam_api64.dll would be
  renamed to steam_api64_161.dll and STEAMLIBVER, below, set to the matching
  suffix.

  After successful testing the constants below should all be the same
  until a new API upgrade is required - essentially making the define = !define
  and STEAMLIBVER should be an empty string ('')
}

const
  STEAM_API_VERSION = 1.61;

  { Versions of Steam API interfaces.
    Correspond to Steamworks 1.xx controlled by API_XXX with fallback to 1.57 version. }
{$if STEAM_API_VERSION = 1.61}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient021'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION013'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  STEAMAPPS_INTERFACE_VERSION = 'STEAMAPPS_INTERFACE_VERSION008'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUSER_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMFRIENDS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches STEAMINPUT_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  STEAMLIBVER = '_161';
{$elseif STEAM_API_VERSION = 1.57}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUSER_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches accessor in steam_api_flat.h
  STEAMLIBVER = '';
{$endif}

implementation

end.
