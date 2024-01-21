unit YOS_WorldManager;

{$mode objfpc}{$H+}

interface

uses
  ScriptHookV, Natives, Windows, ctypes, Classes, SysUtils, YOS_Screen, YOS_Utils, YOS_DataFiles;

procedure DoWorldCleanup;
procedure ResetPlayerLocation;

implementation

var
  DisabledScriptList, DisabledMapList: TStrings;

procedure DoWorldCleanup;
var
  h: Hash;
  plyr: player;
  pp: Ped;
  pv: Vehicle;
  i: integer;
  worldObjectData: array [0..16383] of cint;
  worldObjectCount: cint;
  b, bNext: Blip;
begin
  plyr := GET_PLAYER_INDEX;
  pp := GET_PLAYER_PED(plyr);
  pv := GET_VEHICLE_PED_IS_USING(pp);

  // Kill story (very forceful)
  SET_MISSION_FLAG(BOOL(0));
  SET_RANDOM_EVENT_FLAG(BOOL(0));
  SET_MINIGAME_IN_PROGRESS(BOOL(0));
  STOP_CUTSCENE_IMMEDIATELY;
  REMOVE_CUTSCENE;
  DESTROY_MOBILE_PHONE;
  SET_PLAYER_IS_REPEATING_A_MISSION(BOOL(0));
  SET_CREDITS_ACTIVE(BOOL(0));
  if (IS_NEW_LOAD_SCENE_ACTIVE <> BOOL(0)) then
     NEW_LOAD_SCENE_STOP;
  RENDER_SCRIPT_CAMS(BOOL(0), BOOL(0), 0, BOOL(1), BOOL(0), 0);
  DESTROY_ALL_CAMS(BOOL(0));
  for i := 0 to DisabledScriptList.Count - 1 do
      begin
        TERMINATE_ALL_SCRIPTS_WITH_THIS_NAME(PChar(DisabledScriptList[i]));
        FORCE_CLEANUP_FOR_ALL_THREADS_WITH_THIS_NAME(PChar(DisabledScriptList[i]), 1);
        h := GET_HASH_KEY(PChar(DisabledScriptList[i]));
        if (HAS_SCRIPT_WITH_NAME_HASH_LOADED(h) <> BOOL(0)) then
           SET_SCRIPT_WITH_NAME_HASH_AS_NO_LONGER_NEEDED(h);
        SET_SCRIPT_AS_NO_LONGER_NEEDED(PChar(DisabledScriptList[i]));
      end;
  CLEAR_CODE_REQUESTED_AUTOSAVE;
  CLEAR_REPLAY_STATS;

  // Remove North Yankton
  for i := 0 to DisabledMapList.Count - 1 do
      REMOVE_IPL(PChar(DisabledMapList[i]));
  SET_ZONE_ENABLED(GET_ZONE_FROM_NAME_ID(PChar('PrLog')), BOOL(0));
  SET_MAPDATACULLBOX_ENABLED(PChar('Prologue_Main'), BOOL(0));
  SET_MAPDATACULLBOX_ENABLED(PChar('prologue'), BOOL(0));
  SET_INTERIOR_ACTIVE(0, BOOL(1));
  REFRESH_INTERIOR(0);

  // Restore radar functionality
  SET_MINIMAP_IN_PROLOGUE(BOOL(0));
  if (getGameVersion >= VER_1_0_2189_0_STEAM) then
     SET_USE_ISLAND_MAP(BOOL(0));
  UNLOCK_MINIMAP_ANGLE;
  UNLOCK_MINIMAP_POSITION;
  DISPLAY_HUD(BOOL(1));
  DISPLAY_RADAR(BOOL(1));

  // Reset radar
  SET_MINIMAP_HIDE_FOW(BOOL(1)); // Minimqap FOW cannot be saved, and 3D-era maps were always uncovered. So that should be OK.
  CLEAR_GPS_CUSTOM_ROUTE;
  CLEAR_GPS_MULTI_ROUTE;
  if (getGameVersion >= VER_1_0_877_1_STEAM) then
     CLEAR_ALL_BLIP_ROUTES;
  CLEAR_GPS_PLAYER_WAYPOINT;
  CLEAR_GPS_FLAGS;
  CLEAR_GPS_RACE_TRACK;
  GOLF_TRAIL_SET_ENABLED(BOOL(0));
  DELETE_WAYPOINTS_FROM_THIS_PLAYER;
  CUSTOM_MINIMAP_CLEAR_BLIPS;
  SET_POLICE_RADAR_BLIPS(BOOL(1));
  for i := 0 to 16383 do
      begin
        b := GET_FIRST_BLIP_INFO_ID(cint(i));
        while (DOES_BLIP_EXIST(b) <> BOOL(0)) do
              begin
                bNext := GET_NEXT_BLIP_INFO_ID(cint(i));
                REMOVE_BLIP(@b);
                b := bNext;
              end;
      end;

  // Clear UI
  CLEAR_BRIEF;
  CLEAR_ALL_HELP_MESSAGES;
  CLEAR_SMALL_PRINTS;
  REMOVE_WARNING_MESSAGE_OPTION_ITEMS;
  THEFEED_RESET_ALL_PARAMETERS;
  if (getGameVersion >= VER_1_0_1290_1_STEAM) then
     begin
       CLEAR_FAKE_CONE_ARRAY;
       CLEAR_VALID_VEHICLE_HIT_HASHES;
     end;

  // Reset weather, set initial datetime
  SET_WEATHER_TYPE_NOW(PChar('EXTRASUNNY'));
  CLEAR_EXTRA_TCMODIFIER;
  DISABLE_MOON_CYCLE_OVERRIDE;
  CLEAR_TIMECYCLE_MODIFIER;
  CLEAR_WEATHER_TYPE_PERSIST;
  CLEAR_OVERRIDE_WEATHER;
  UNLOAD_ALL_CLOUD_HATS;
  SET_CLOUDS_ALPHA(1.0);
  PAUSE_CLOCK(BOOL(0));
  SET_TIME_SCALE(1.0);
  SET_CLOCK_DATE(2013, 07, 31);
  SET_CLOCK_TIME(07, 30, 0);

  // Reset emergency services
  for i := 1 to 15 do
      ENABLE_DISPATCH_SERVICE(cint(i), BOOL(1));
  SET_CREATE_RANDOM_COPS(BOOL(1));
  SET_CREATE_RANDOM_COPS_NOT_ON_SCENARIOS(BOOL(1));
  SET_CREATE_RANDOM_COPS_ON_SCENARIOS(BOOL(1));
  SET_MAX_WANTED_LEVEL(5);
  SET_WANTED_LEVEL_MULTIPLIER(1.0);
  SET_FAKE_WANTED_LEVEL(0);
  CLEAR_PLAYER_WANTED_LEVEL(plyr);
  RESET_WANTED_LEVEL_DIFFICULTY(plyr);
  SET_POLICE_IGNORE_PLAYER(plyr, BOOL(0));
  SET_DISPATCH_COPS_FOR_PLAYER(plyr, BOOL(1));
  RESET_PLAYER_ARREST_STATE(plyr);
  RESET_DISPATCH_SPAWN_BLOCKING_AREAS;
  RESET_DISPATCH_IDEAL_SPAWN_DISTANCE;
  RESET_WANTED_RESPONSE_NUM_PEDS_TO_SPAWN;
  RESET_LAW_RESPONSE_DELAY_OVERRIDE;
  CANCEL_ALL_POLICE_REPORTS;

  // Reset world flags
  SET_RANDOM_TRAINS(BOOL(1));
  SET_RANDOM_BOATS(BOOL(1));
  SET_GARBAGE_TRUCKS(BOOL(1));
  SET_ALL_VEHICLE_GENERATORS_ACTIVE;
  CLEAR_TACTICAL_NAV_MESH_POINTS;
  REMOVE_NAVMESH_REQUIRED_REGIONS;
  REMOVE_ALL_COVER_BLOCKING_AREAS;
  REMOVE_SCENARIO_BLOCKING_AREAS;
  RESET_SCENARIO_GROUPS_ENABLED;
  RESET_SCENARIO_TYPES_ENABLED;
  RESET_EXCLUSIVE_SCENARIO_GROUP;
  CLEAR_PED_NON_CREATION_AREA;
  REMOVE_SHOCKING_EVENT_SPAWN_BLOCKING_AREAS;
  if (getGameVersion >= VER_1_0_573_1_STEAM) then
     REMOVE_ALL_AIR_DEFENCE_SPHERES;
  if (getGameVersion >= VER_1_0_1290_1_STEAM) then
     CLEAR_EXTENDED_PICKUP_PROBE_AREAS;
  SET_ARTIFICIAL_LIGHTS_STATE(BOOL(0));
  SET_STUNT_JUMPS_CAN_TRIGGER(BOOL(0));
  SET_ROADS_BACK_TO_ORIGINAL(-12500.0, -12500.0, -200.0, 12500.0, 12500.0, 5000.0, 1);
  SET_PED_PATHS_BACK_TO_ORIGINAL(-12500.0, -12500.0, -200.0, 12500.0, 12500.0, 5000.0, 1);
  GRASSBATCH_DISABLE_FLATTENING;
  RESET_DEEP_OCEAN_SCALER;
  SET_GRAVITY_LEVEL(0);
  CLEAR_RESTART_COORD_OVERRIDE;
  CLEAR_SCENARIO_SPAWN_HISTORY;
  SPAWNPOINTS_CANCEL_SEARCH;

  // Reset streaming
  SET_STREAMING(BOOL(1));
  SET_RENDER_HD_ONLY(BOOL(0));
  CLEAR_HD_AREA;
  CLEAR_FOCUS;
  CLEAR_ALL_PICKUP_REWARD_TYPE_SUPPRESSION;
  STOP_ANY_PED_MODEL_BEING_SUPPRESSED;
  SET_REDUCE_VEHICLE_MODEL_BUDGET(BOOL(0));
  SET_REDUCE_PED_MODEL_BUDGET(BOOL(0));
  ANIMPOSTFX_STOP_ALL;
  ENABLE_CLOWN_BLOOD_VFX(BOOL(0));
  ENABLE_ALIEN_BLOOD_VFX(BOOL(0));
  STOP_ALL_GARAGE_ACTIVITY;
  for i := 0 to GetAudioFlagCount - 1 do
      if GetAudioFlagDefaultValue(i) then
         SET_AUDIO_FLAG(PChar(GetAudioFlagName(i)), BOOL(1))
      else
         SET_AUDIO_FLAG(PChar(GetAudioFlagName(i)), BOOL(0));

  // Reset AI stuff
  RESET_AI_MELEE_WEAPON_DAMAGE_MODIFIER;
  RESET_AI_WEAPON_DAMAGE_MODIFIER;
  SET_RIOT_MODE_ENABLED(BOOL(0));
  SET_ALL_RANDOM_PEDS_FLEE(plyr, BOOL(0));
  SET_PLAYER_CAN_BE_HASSLED_BY_GANGS(plyr, BOOL(1));
  SET_EVERYONE_IGNORE_PLAYER(plyr, BOOL(0));

  // Do a full world cleanup
  worldObjectCount := worldGetAllPickups(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      begin
        if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
           SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
        REMOVE_PICKUP(worldObjectData[i]);
      end;
  worldObjectCount := worldGetAllPeds(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      if (worldObjectData[i] <> pp) then
         begin
           if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
              SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
           DELETE_PED(@worldObjectData[i]);
         end;
  worldObjectCount := worldGetAllVehicles(pcint(worldObjectData), Length(worldObjectData));
  for i := 0 to worldObjectCount - 1 do
      if (worldObjectData[i] <> pv) then
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
  DELETE_ALL_TRAINS;
  STOP_FIRE_IN_RANGE(0.0, 0.0, 0.0, 25000.0);
  CLEAR_AREA(0.0, 0.0, 0.0, 25000.0, BOOL(1), BOOL(0), BOOL(0), BOOL(0));

  // Expand world limits to an uniform, large value
  RESET_WORLD_BOUNDARY_FOR_PLAYER;
  EXTEND_WORLD_BOUNDARY_FOR_PLAYER(12500.0, 12500.0, 30.0);
  EXTEND_WORLD_BOUNDARY_FOR_PLAYER(-12500.0, -12500.0, 30.0);

  // Reset player flags
  SET_PLAYER_IS_IN_ANIMAL_FORM(BOOL(0));
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_zero')), 0);
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_one')), 0);
  SPECIAL_ABILITY_UNLOCK(GET_HASH_KEY(PChar('player_two')), 0);
  SPECIAL_ABILITY_RESET(plyr, 0);
  ENABLE_SPECIAL_ABILITY(plyr, BOOL(1), 0);
  RESET_PLAYER_INPUT_GAIT(plyr);
  RESET_PLAYER_STAMINA(plyr);
  SET_PLAYER_CONTROL(plyr, BOOL(1), 0);
  SET_PLAYER_CAN_DO_DRIVE_BY(plyr, BOOL(1));
  SET_PLAYER_CAN_USE_COVER(plyr, BOOL(1));
  SET_PLAYER_INVINCIBLE(plyr, BOOL(0));
  SET_PLAYER_MAX_ARMOUR(plyr, 200);

  // Change player model, apply properties, and setup initial stuff
  h := GET_HASH_KEY(PChar('player_zero'));
  while (HAS_MODEL_LOADED(h) = BOOL(0)) do
        begin
          REQUEST_MODEL(h);
          GameScreen.DrawLoadingScreen;
          ScriptHookVWait(0);
        end;
  SET_PLAYER_MODEL(plyr, h);
  SET_PED_DEFAULT_COMPONENT_VARIATION(GET_PLAYER_PED(plyr));
  SET_PED_MAX_HEALTH(GET_PLAYER_PED(plyr), 200);
  SET_ENTITY_HEALTH(GET_PLAYER_PED(plyr), 200, 0, 0);
  SET_PED_ARMOUR(GET_PLAYER_PED(plyr), 0);
  SET_MODEL_AS_NO_LONGER_NEEDED(h);
  ResetPlayerLocation;
  if (pv <> 0) then
     begin
       if (IS_ENTITY_A_MISSION_ENTITY(pv) = BOOL(0)) then
          SET_ENTITY_AS_MISSION_ENTITY(pv, BOOL(1), BOOL(1));
       DELETE_VEHICLE(@pv);
     end;
end;

procedure ResetPlayerLocation;
var
  plyr: Player;
  veh: Vehicle;
  h: hash;
begin
  plyr := GET_PLAYER_INDEX;
  h := GET_HASH_KEY(PChar('Seashark2'));
  while (HAS_MODEL_LOADED(h) = BOOL(0)) do
        begin
          REQUEST_MODEL(h);
          GameScreen.DrawLoadingScreen;
          ScriptHookVWait(0);
        end;
  SET_ENTITY_COORDS_NO_OFFSET(GET_PLAYER_PED(plyr), 0.0, 0.0, 0.0, BOOL(0), BOOL(0), BOOL(0));
  veh := CREATE_VEHICLE(h, 4000.0, -4000.0, 0.0, 45.0, BOOL(0), BOOL(0), BOOL(0));
  SET_PED_INTO_VEHICLE(GET_PLAYER_PED(plyr), veh, -1);
  SET_VEHICLE_AS_NO_LONGER_NEEDED(@veh);
  SET_MODEL_AS_NO_LONGER_NEEDED(h);
end;

initialization
  DisabledScriptList := ReadRawData('yos_data/data/GTAScriptList.dat');
  DisabledMapList := ReadRawData('yos_data/data/NorthYanktonList.dat');

finalization
  DisabledScriptList.Free;
  DisabledMapList.Free;

end.

