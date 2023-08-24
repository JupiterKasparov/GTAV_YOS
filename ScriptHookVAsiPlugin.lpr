library ScriptHookVAsiPlugin;

{$mode objfpc}
{$H+}

uses
  Windows, ctypes, ScriptHookV, Natives, YOS_Script, YOS_WorldManager, YOS_Utils, YOS_Screen;

procedure ScriptMain; cdecl;
var
  worldInitComplete: boolean;
  eventId: TMenuEventType;
  sgi: integer;
  isPlayerDead: boolean;
begin
  worldInitComplete := false;
  SET_THIS_SCRIPT_CAN_BE_PAUSED(BOOL(0));
  SET_THIS_SCRIPT_CAN_REMOVE_BLIPS_CREATED_BY_ANY_SCRIPT(BOOL(1));
  while true do
        begin
          // If he have a loading screen, this may be the first game start, or a game reload...
          if (GET_IS_LOADING_SCREEN_ACTIVE <> BOOL(0)) then
             worldInitComplete := false
          // We are in gameplay!
          else if (GET_PLAYER_PED(GET_PLAYER_INDEX) <> 0) then
             begin
               if (not worldInitComplete) then
                  begin
                    worldInitComplete := true;
                    KillStoryScripts;
                    ClearWorldMap;
                    ResetPlayerInfo;
                    MissionScript.Reset;
                    if MissionScript.IsAvailable then
                       GameScreen.MenuMode := gmLoadGame
                    else
                       PushMapNotification('~o~No runnable mission script found!');
                  end;
               if (GameScreen.MenuMode = gmInGame) then
                  begin
                    isPlayerDead := (IS_PLAYER_DEAD(GET_PLAYER_INDEX) <> BOOL(0)) or (IS_PLAYER_BEING_ARRESTED(GET_PLAYER_INDEX, BOOL(0)) <> BOOL(0));
                    if (IS_CONTROL_JUST_RELEASED(0, 244) <> BOOL(0)) and (not isPlayerDead) then
                       begin
                         if MissionScript.IsAvailable then
                            GameScreen.MenuMode := gmLoadGame
                         else
                            PushMapNotification('~o~Without a runnable mission script, the menu is disabled!');
                       end
                    else
                       MissionScript.Run(isPlayerDead);
                  end
               else
                  begin
                    MissionScript.Run(false);
                    if GameScreen.ProcessMenu(eventId, sgi, MissionScript.GetInternalPlayerIndex, MissionScript.IsGameStarted) then
                       begin
                         case eventId of
                              evtNewGame:
                                begin
                                  GameScreen.MenuMode := gmLoadingScreen;
                                  GameScreen.DrawLoadingScreen;
                                  ClearWorldMap;
                                  ResetPlayerInfo;
                                  MissionScript.Reset;
                                end;
                              evtLoadGame:
                                begin
                                  GameScreen.MenuMode := gmLoadingScreen;
                                  GameScreen.DrawLoadingScreen;
                                  ClearWorldMap;
                                  ResetPlayerInfo;
                                  MissionScript.Load(GetSaveFileName(sgi));
                                end;
                              evtSaveGame:
                                MissionScript.Save(GetSaveFileName(sgi));
                         end;
                         if (eventId = evtCancelMenu) and (not MissionScript.IsGameStarted) and (GameScreen.MenuMode = gmLoadGame) then
                            // Prevent the user from exiting (re-entering) into The Prologue via Exiting or Alt-Tab
                            ACTIVATE_FRONTEND_MENU(GET_HASH_KEY(PChar('FE_MENU_VERSION_SP_PAUSE')), BOOL(1), -1)
                         else
                            GameScreen.MenuMode := gmInGame;
                       end;
                  end;
             end;
          // Process game events
          ScriptHookVWait(0);
        end;
end;

procedure PlayerRespawn; cdecl;
var
  respawnLocation: TRespawnLocation;
  useDefaultRestart: boolean;
begin
  SET_THIS_SCRIPT_CAN_BE_PAUSED(BOOL(0));
  while true do
        begin
          if (GET_IS_LOADING_SCREEN_ACTIVE = BOOL(0)) and (GET_PLAYER_PED(GET_PLAYER_INDEX) <> 0) then
             begin
               useDefaultRestart := false;
               if (IS_PLAYER_DEAD(GET_PLAYER_INDEX) <> BOOL(0)) then
                  begin
                    SET_FADE_OUT_AFTER_DEATH(BOOL(1));
                    SET_FADE_IN_AFTER_DEATH_ARREST(BOOL(1));
                    while (IS_PLAYER_DEAD(GET_PLAYER_INDEX) <> BOOL(0)) do
                          ScriptHookVWait(0);
                    if MissionScript.HasOverrideRestart then
                       respawnLocation := MissionScript.OverrideRestartLocation
                    else
                       useDefaultRestart := not MissionScript.FindNearestRespawnLocation(respawnLocation, true);
                    if useDefaultRestart then
                       ResetPlayerLocation
                    else
                       begin
                         SET_ENTITY_COORDS(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.X, respawnLocation.Y, respawnLocation.Z, BOOL(0), BOOL(0), BOOL(0), BOOL(1));
                         SET_ENTITY_HEADING(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.A);
                       end;
                  end
               else if (IS_PLAYER_BEING_ARRESTED(GET_PLAYER_INDEX, BOOL(0)) <> BOOL(0)) then
                  begin
                    SET_FADE_OUT_AFTER_ARREST(BOOL(1));
                    SET_FADE_IN_AFTER_DEATH_ARREST(BOOL(1));
                    while (IS_PLAYER_BEING_ARRESTED(GET_PLAYER_INDEX, BOOL(0)) <> BOOL(0)) do
                          ScriptHookVWait(0);
                    if MissionScript.HasOverrideRestart then
                       respawnLocation := MissionScript.OverrideRestartLocation
                    else
                       useDefaultRestart := not MissionScript.FindNearestRespawnLocation(respawnLocation, false);
                    if useDefaultRestart then
                       ResetPlayerLocation
                    else
                       begin
                         SET_ENTITY_COORDS(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.X, respawnLocation.Y, respawnLocation.Z, BOOL(0), BOOL(0), BOOL(0), BOOL(1));
                         SET_ENTITY_HEADING(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.A);
                       end;
                  end;
             end;
          ScriptHookVWait(10);
        end;
end;

procedure EndDll(reason: PtrInt);
begin
  scriptUnregister(HInstance);
end;

{$R *.res}

begin
  DisableThreadLibraryCalls(HInstance);
  Dll_Process_Detach_Hook := @EndDll;
  scriptRegister(HInstance, @ScriptMain);
  scriptRegisterAdditionalThread(HInstance, @PlayerRespawn);
end.

