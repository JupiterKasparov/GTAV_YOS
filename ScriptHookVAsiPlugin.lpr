library ScriptHookVAsiPlugin;

{$mode objfpc}
{$H+}

uses
  Windows, ctypes, ScriptHookV, Natives, YOS_Script, YOS_WorldManager,
  YOS_Utils, YOS_Screen, YOS_DataFiles;

procedure ScriptMain; cdecl;
var
  worldInitComplete, isPlayerDead, isPlayerBusted, useDefaultRestart: boolean;
  eventId: TMenuEventType;
  sgi: integer;
  respawnLocation: TRespawnLocation;
begin
  worldInitComplete := false;
  isPlayerDead := false;
  isPlayerBusted := false;
  ClearCustomTextures;
  SET_THIS_SCRIPT_CAN_BE_PAUSED(BOOL(0));
  SET_THIS_SCRIPT_CAN_REMOVE_BLIPS_CREATED_BY_ANY_SCRIPT(BOOL(1));
  while true do
        begin
          if (GET_PLAYER_PED(GET_PLAYER_INDEX) <> 0) then
             begin
               if (not worldInitComplete) then
                  begin
                    worldInitComplete := true;

                    // Close loading screen (if it was the Game Start)
                    if (GET_IS_LOADING_SCREEN_ACTIVE <> BOOL(0)) then
                       SHUTDOWN_LOADING_SCREEN;

                    // World cleanup
                    DO_SCREEN_FADE_OUT(0);
                    DoWorldCleanup;
                    DO_SCREEN_FADE_IN(0);
                    MissionScript.Reset;
                    if MissionScript.IsAvailable then
                       GameScreen.MenuMode := gmLoadGame
                    else
                       PushMapNotification('~o~No runnable mission script found!');
                  end;
               if (GameScreen.MenuMode = gmInGame) then
                  begin
                    if (IS_PLAYER_DEAD(GET_PLAYER_INDEX) <> BOOL(0)) then
                       begin
                         PAUSE_DEATH_ARREST_RESTART(BOOL(0));
                         SET_FADE_OUT_AFTER_DEATH(BOOL(1));
                         SET_FADE_IN_AFTER_DEATH_ARREST(BOOL(1));
                         if not isPlayerDead then
                            isPlayerDead := true;
                         if isPlayerBusted then
                            isPlayerBusted := false;
                       end
                    else if (IS_PLAYER_BEING_ARRESTED(GET_PLAYER_INDEX, BOOL(0)) <> BOOL(0)) then
                       begin
                         PAUSE_DEATH_ARREST_RESTART(BOOL(0));
                         SET_FADE_OUT_AFTER_ARREST(BOOL(1));
                         SET_FADE_IN_AFTER_DEATH_ARREST(BOOL(1));
                         if not isPlayerBusted then
                            isPlayerBusted := true;
                         if isPlayerDead then
                            isPlayerDead := false;
                       end
                    else
                       begin
                         // If player WAS dead or arrested, we use custom respawn
                         if isPlayerDead then
                            begin
                              isPlayerDead := false;
                              useDefaultRestart := false;
                              if MissionScript.HasOverrideRestart then
                                 respawnLocation := MissionScript.OverrideRestartLocation
                              else
                                 useDefaultRestart := not MissionScript.FindNearestRespawnLocation(respawnLocation, true);
                              if useDefaultRestart then
                                 ResetPlayerLocation
                              else
                                 begin
                                   SET_ENTITY_COORDS_NO_OFFSET(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.X, respawnLocation.Y, respawnLocation.Z, BOOL(0), BOOL(0), BOOL(0));
                                   SET_ENTITY_HEADING(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.A);
                                 end;
                            end;
                         if isPlayerBusted then
                            begin
                              isPlayerBusted := false;
                              useDefaultRestart := false;
                              if MissionScript.HasOverrideRestart then
                                 respawnLocation := MissionScript.OverrideRestartLocation
                              else
                                 useDefaultRestart := not MissionScript.FindNearestRespawnLocation(respawnLocation, false);
                              if useDefaultRestart then
                                 ResetPlayerLocation
                              else
                                 begin
                                   SET_ENTITY_COORDS_NO_OFFSET(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.X, respawnLocation.Y, respawnLocation.Z, BOOL(0), BOOL(0), BOOL(0));
                                   SET_ENTITY_HEADING(GET_PLAYER_PED(GET_PLAYER_INDEX), respawnLocation.A);
                                 end;
                            end;
                       end;
                    if (IS_CONTROL_JUST_RELEASED(0, 244) <> BOOL(0)) and (IS_SCREEN_FADING_OUT = BOOL(0)) and(IS_SCREEN_FADED_OUT = BOOL(0)) and  (IS_CUTSCENE_PLAYING = BOOL(0)) and (IS_PLAYER_SWITCH_IN_PROGRESS = BOOL(0)) and (not isPlayerDead) and (not isPlayerBusted) then
                       begin
                         if MissionScript.IsAvailable then
                            GameScreen.MenuMode := gmLoadGame
                         else
                            PushMapNotification('~o~Without a runnable mission script, the menu is disabled!');
                       end
                    else
                       MissionScript.Run(GameScreen.MenuMode, isPlayerDead, isPlayerBusted);
                  end
               else
                  begin
                    MissionScript.Run(GameScreen.MenuMode, false, false);
                    if GameScreen.ProcessMenu(eventId, sgi, MissionScript.GetInternalPlayerIndex, MissionScript.IsGameStarted) then
                       begin
                         case eventId of
                              evtNewGame:
                                begin
                                  GameScreen.MenuMode := gmLoadingScreen;
                                  GameScreen.DrawLoadingScreen;
                                  DO_SCREEN_FADE_OUT(0);
                                  DoWorldCleanup;
                                  DO_SCREEN_FADE_IN(0);
                                  MissionScript.Reset;
                                end;
                              evtLoadGame:
                                begin
                                  GameScreen.MenuMode := gmLoadingScreen;
                                  GameScreen.DrawLoadingScreen;
                                  DO_SCREEN_FADE_OUT(0);
                                  DoWorldCleanup;
                                  DO_SCREEN_FADE_IN(0);
                                  MissionScript.Load(sgi);
                                end;
                              evtSaveGame:
                                MissionScript.Save(sgi);
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

procedure EndDll(reason: PtrInt);
begin
  scriptUnregister(HInstance);
end;

{$R *.res}

begin
  DisableThreadLibraryCalls(HInstance);
  Dll_Process_Detach_Hook := @EndDll;
  scriptRegister(HInstance, @ScriptMain);
end.

