unit YOS_WorldManager;

{$mode objfpc}{$H+}

interface

uses
  ScriptHookV, Natives, Windows, ctypes, Classes, SysUtils, YOS_Screen, YOS_Utils;

procedure KillStoryScripts;
procedure ClearWorldMap;
procedure ResetPlayerInfo;
procedure ResetPlayerLocation;

implementation

uses
  YOS_Script;

var
  DisabledScriptList, DisabledMapList, AudioFlagList: TStrings;

procedure KillStoryScripts;
var
  i: integer;
  h: Hash;
begin
  STOP_CUTSCENE_IMMEDIATELY;
  DESTROY_MOBILE_PHONE;
  SET_MISSION_FLAG(BOOL(0));
  SET_RANDOM_EVENT_FLAG(BOOL(0));
  for i := 0 to DisabledScriptList.Count - 1 do
      begin
        FORCE_CLEANUP_FOR_ALL_THREADS_WITH_THIS_NAME(PChar(DisabledScriptList[i]), 1);
        TERMINATE_ALL_SCRIPTS_WITH_THIS_NAME(PChar(DisabledScriptList[i]));
        h := GET_HASH_KEY(PChar(DisabledScriptList[i]));
        if (HAS_SCRIPT_WITH_NAME_HASH_LOADED(h) <> BOOL(0)) then
           SET_SCRIPT_WITH_NAME_HASH_AS_NO_LONGER_NEEDED(h);
        SET_SCRIPT_AS_NO_LONGER_NEEDED(PChar(DisabledScriptList[i]));
      end;
end;

procedure ClearWorldMap;
var
  i: integer;
  worldObjectCount: cint;
  worldObjectData: array [0..16383] of cint;
  b, next: Blip;
  plyrp: Ped;
  plyrv: Vehicle;
begin
  // START
  DO_SCREEN_FADE_OUT(1);

  // Specifically closes North Yankton
  SET_ZONE_ENABLED(GET_ZONE_FROM_NAME_ID(PChar('PrLog')), BOOL(0));
  SET_MAPDATACULLBOX_ENABLED(PChar('Prologue_Main'), BOOL(0));
  SET_MAPDATACULLBOX_ENABLED(PChar('prologue'), BOOL(0));

  // Removes Disabled maps (by default, only North Yankton)
  for i := 0 to DisabledMapList.Count - 1 do
      REMOVE_IPL(PChar(DisabledMapList[i]));

  // Activate OUTSIDE interior
  SET_INTERIOR_ACTIVE(0, BOOL(1));
  REFRESH_INTERIOR(0);
  SET_MINIMAP_IN_PROLOGUE(BOOL(0));
  UNLOCK_MINIMAP_ANGLE;
  UNLOCK_MINIMAP_POSITION;

  // Expand world limits to an uniform, large value
  EXTEND_WORLD_BOUNDARY_FOR_PLAYER(12500.0, 12500.0, 30.0);
  EXTEND_WORLD_BOUNDARY_FOR_PLAYER(-12500.0, -12500.0, 30.0);

  // Reset weather and datetime
  CLEAR_TIMECYCLE_MODIFIER;
  CLEAR_WEATHER_TYPE_PERSIST;
  SET_WEATHER_TYPE_NOW(PChar('EXTRASUNNY'));
  UNLOAD_ALL_CLOUD_HATS;
  CLEAR_WEATHER_TYPE_PERSIST;
  SET_CLOCK_DATE(2013, 07, 31);
  SET_CLOCK_TIME(07, 30, 0);
  SET_TIME_SCALE(1.0);

  // Reset world flags
  SET_RANDOM_TRAINS(BOOL(1));
  for i := 0 to AudioFlagList.Count - 1 do
      SET_AUDIO_FLAG(PChar(AudioFlagList[i]), BOOL(0));
  SET_CREATE_RANDOM_COPS(BOOL(1));
  SET_CREATE_RANDOM_COPS_NOT_ON_SCENARIOS(BOOL(1));
  SET_CREATE_RANDOM_COPS_ON_SCENARIOS(BOOL(1));
  SET_RANDOM_BOATS(BOOL(1));
  SET_GARBAGE_TRUCKS(BOOL(1));
  for i := 1 to 15 do
      ENABLE_DISPATCH_SERVICE(cint(i), BOOL(1));
  SET_ALL_VEHICLE_GENERATORS_ACTIVE;

  // Clear game objects
  CLEAR_BRIEF;
  CLEAR_ALL_HELP_MESSAGES;
  CLEAR_SMALL_PRINTS;
  DELETE_ALL_TRAINS;
  REMOVE_ALL_COVER_BLOCKING_AREAS;
  REMOVE_SCENARIO_BLOCKING_AREAS;
  SET_ROADS_BACK_TO_ORIGINAL(-12500.0, -12500.0, -200.0, 12500.0, 12500.0, 5000.0, 1);
  SET_PED_PATHS_BACK_TO_ORIGINAL(-12500.0, -12500.0, -200.0, 12500.0, 12500.0, 5000.0, 1);
  CLEAR_GPS_CUSTOM_ROUTE;
  CLEAR_GPS_PLAYER_WAYPOINT;
  DELETE_WAYPOINTS_FROM_THIS_PLAYER;
  SET_STUNT_JUMPS_CAN_TRIGGER(BOOL(0));

  plyrp := GET_PLAYER_PED(GET_PLAYER_INDEX);
  plyrv := GET_VEHICLE_PED_IS_USING(plyrp);

  worldObjectCount := worldGetAllPickups(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      begin
        if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
           SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
        REMOVE_PICKUP(worldObjectData[i]);
      end;
  worldObjectCount := worldGetAllPeds(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      if (worldObjectData[i] <> plyrp) then
         begin
           if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
              SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
           DELETE_PED(@worldObjectData[i]);
         end;
  worldObjectCount := worldGetAllVehicles(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      if (worldObjectData[i] <> plyrv) then
         begin
           if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
              SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
           DELETE_VEHICLE(@worldObjectData[i]);
         end;
  worldObjectCount := worldGetAllObjects(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      if (GET_ENTITY_TYPE(worldObjectData[i]) = 3) then
         begin
           if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) <> BOOL(0)) then
              DELETE_OBJECT(@worldObjectData[i]);
         end;
  CLEAR_AREA(0.0, 0.0, 0.0, 25000.0, BOOL(1), BOOL(0), BOOL(0), BOOL(0));
  for i := 0 to 16383 do
      begin
        b := GET_FIRST_BLIP_INFO_ID(cint(i));
        while (DOES_BLIP_EXIST(b) <> BOOL(0)) do
              begin
                next := GET_NEXT_BLIP_INFO_ID(cint(i));
                REMOVE_BLIP(@b);
                b := next;
              end;
      end;

  // END
  DO_SCREEN_FADE_IN(1);
end;

procedure ResetPlayerInfo;
var
  plyr: Player;
  veh: Vehicle;
  h: Hash;
begin
  // START
  DO_SCREEN_FADE_OUT(1);
  plyr := GET_PLAYER_INDEX;

  // Unlock special abilities for players
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_zero')), 0);
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_one')), 0);
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_two')), 0);

  // Reset wanted level
  SET_MAX_WANTED_LEVEL(5);
  SET_WANTED_LEVEL_MULTIPLIER(1.0);
  SET_FAKE_WANTED_LEVEL(0);
  CLEAR_PLAYER_WANTED_LEVEL(plyr);
  SET_POLICE_IGNORE_PLAYER(plyr, BOOL(0));
  SET_POLICE_RADAR_BLIPS(BOOL(1));

  // Reset player to Michael, default outfit
  veh := GET_VEHICLE_PED_IS_USING(GET_PLAYER_PED(plyr));

  // Reset player skin
  h := GET_HASH_KEY(PChar('player_zero'));
  REQUEST_MODEL(h);
  while (HAS_MODEL_LOADED(h) = BOOL(0)) do
        begin
          GameScreen.DrawLoadingScreen;
          ScriptHookVWait(0);
        end;
  SET_PLAYER_MODEL(plyr, h);
  SET_PED_DEFAULT_COMPONENT_VARIATION(GET_PLAYER_PED(plyr));
  SET_MODEL_AS_NO_LONGER_NEEDED(h);

  // Reset player flags
  SPECIAL_ABILITY_RESET(plyr, 0);
  ENABLE_SPECIAL_ABILITY(plyr, BOOL(1), 0);
  SET_DISPATCH_COPS_FOR_PLAYER(plyr, BOOL(1));
  SET_PED_MAX_HEALTH(GET_PLAYER_PED(plyr), 200);
  SET_ENTITY_HEALTH(GET_PLAYER_PED(plyr), 200, 0, 0);
  SET_PLAYER_MAX_ARMOUR(plyr, 200);
  SET_PED_ARMOUR(GET_PLAYER_PED(plyr), 0);
  SET_PLAYER_CONTROL(plyr, BOOL(1), 0);
  SET_PLAYER_CAN_DO_DRIVE_BY(plyr, BOOL(1));
  SET_PLAYER_CAN_BE_HASSLED_BY_GANGS(plyr, BOOL(1));
  SET_EVERYONE_IGNORE_PLAYER(plyr, BOOL(0));
  DISPLAY_HUD(BOOL(1));
  DISPLAY_RADAR(BOOL(1));

  // Put player out at the ocean, on a Jet Ski
  ResetPlayerLocation;

  // Delete the former player vehicle, required, because of session change!
  if (veh <> 0) then
     begin
       if (IS_ENTITY_A_MISSION_ENTITY(veh) = BOOL(0)) then
          SET_ENTITY_AS_MISSION_ENTITY(veh, BOOL(1), BOOL(1));
       DELETE_VEHICLE(@veh);
     end;

  // END
  DO_SCREEN_FADE_IN(1);
end;

procedure ResetPlayerLocation;
var
  plyr: Player;
  veh: Vehicle;
  h: hash;
begin
  plyr := GET_PLAYER_INDEX;
  h := GET_HASH_KEY(PChar('Seashark2'));
  REQUEST_MODEL(h);
  while (HAS_MODEL_LOADED(h) = BOOL(0)) do
        begin
          GameScreen.DrawLoadingScreen;
          ScriptHookVWait(0);
        end;
  SET_ENTITY_COORDS(GET_PLAYER_PED(plyr), DefaultRespawnLocation.X, DefaultRespawnLocation.Y, DefaultRespawnLocation.Z, BOOL(0), BOOL(0), BOOL(0), BOOL(0));
  SET_ENTITY_HEADING(GET_PLAYER_PED(plyr), DefaultRespawnLocation.A);
  veh := CREATE_VEHICLE(h, DefaultRespawnLocation.X, DefaultRespawnLocation.Y, DefaultRespawnLocation.Z, DefaultRespawnLocation.A, BOOL(0), BOOL(0), BOOL(0));
  SET_PED_INTO_VEHICLE(GET_PLAYER_PED(plyr), veh, -1);
  SET_VEHICLE_AS_NO_LONGER_NEEDED(@veh);
  SET_MODEL_AS_NO_LONGER_NEEDED(h);
end;

initialization
  DisabledScriptList := ReadRawData('yos_data/data/GTAScriptList.dat');
  DisabledMapList := ReadRawData('yos_data/data/NorthYanktonList.dat');
  AudioFlagList := ReadRawData('yos_data/data/AudioFlags.dat');

finalization
  DisabledScriptList.Free;
  DisabledMapList.Free;
  AudioFlagList.Free;

end.

