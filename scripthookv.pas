unit ScriptHookV;

{$mode objfpc}{$H+}

interface

uses
  Windows, ctypes;

type
  PresentCallback = procedure(ptr: pointer); cdecl;
  KeyboardHandler = procedure(key: DWORD; repeats: WORD; scanCode: BYTE; isExtended, isWithAlt, wasDownBefore, isUpNow: BOOL); cdecl;
  ScriptMainFunc = procedure; cdecl;
  eGameVersion = (
              VER_UNK = -1,

              VER_1_0_335_2_STEAM,
              VER_1_0_335_2_NOSTEAM,
			  
              VER_1_0_350_1_STEAM,
              VER_1_0_350_2_NOSTEAM,
			  
              VER_1_0_372_2_STEAM,
              VER_1_0_372_2_NOSTEAM,
			  
              VER_1_0_393_2_STEAM,
              VER_1_0_393_2_NOSTEAM,
			  
              VER_1_0_393_4_STEAM,
              VER_1_0_393_4_NOSTEAM,
			  
              VER_1_0_463_1_STEAM,
              VER_1_0_463_1_NOSTEAM,
			  
              VER_1_0_505_2_STEAM,
              VER_1_0_505_2_NOSTEAM,
			  
              VER_1_0_573_1_STEAM,
              VER_1_0_573_1_NOSTEAM,
			  
              VER_1_0_617_1_STEAM,
              VER_1_0_617_1_NOSTEAM,
			  
              VER_1_0_678_1_STEAM,
              VER_1_0_678_1_NOSTEAM,
			  
              VER_1_0_757_2_STEAM,
              VER_1_0_757_2_NOSTEAM,
			  
              VER_1_0_757_4_STEAM,
              VER_1_0_757_4_NOSTEAM,
			  
              VER_1_0_791_2_STEAM,
              VER_1_0_791_2_NOSTEAM,
			  
              VER_1_0_877_1_STEAM,
              VER_1_0_877_1_NOSTEAM,
			  
              VER_1_0_944_2_STEAM,
              VER_1_0_944_2_NOSTEAM,
			  
              VER_1_0_1011_1_STEAM,
              VER_1_0_1011_1_NOSTEAM,
			  
              VER_1_0_1032_1_STEAM,
              VER_1_0_1032_1_NOSTEAM,
			  
              VER_1_0_1103_2_STEAM,
              VER_1_0_1103_2_NOSTEAM,
			  
              VER_1_0_1180_2_STEAM,
              VER_1_0_1180_2_NOSTEAM,
			  
              VER_1_0_1290_1_STEAM,
              VER_1_0_1290_1_NOSTEAM,
			  
              VER_1_0_1365_1_STEAM,
              VER_1_0_1365_1_NOSTEAM,
			  
              VER_1_0_1493_0_STEAM,
              VER_1_0_1493_0_NOSTEAM,
			  
              VER_1_0_1493_1_STEAM,
              VER_1_0_1493_1_NOSTEAM,
			  
              VER_1_0_1604_0_STEAM,
              VER_1_0_1604_0_NOSTEAM,
			  
              VER_1_0_1604_1_STEAM,
              VER_1_0_1604_1_NOSTEAM,
			  
              VER_1_0_1737_0_STEAM,
              VER_1_0_1737_0_NOSTEAM,
			  
              VER_1_0_1737_6_STEAM,
              VER_1_0_1737_6_NOSTEAM,
			  
              VER_1_0_1868_0_STEAM,
              VER_1_0_1868_0_NOSTEAM,
			  
              VER_1_0_1868_1_STEAM,
              VER_1_0_1868_1_NOSTEAM,

	          VER_SIZE);

function nativeCall: PUINT64; cdecl; external 'ScriptHookV.dll' name '?nativeCall@@YAPEA_KXZ';
procedure nativePush64(val: UINT64); cdecl; external 'ScriptHookV.dll' name '?nativePush64@@YAX_K@Z';
procedure nativeInit(hash: UINT64); cdecl; external 'ScriptHookV.dll' name '?nativeInit@@YAX_K@Z';
procedure presentCallbackUnregister(cb: PresentCallback); cdecl; external 'ScriptHookV.dll' name '?presentCallbackUnregister@@YAXP6AXPEAX@Z@Z';
procedure presentCallbackRegister(cb: PresentCallback); cdecl; external 'ScriptHookV.dll' name '?presentCallbackRegister@@YAXP6AXPEAX@Z@Z';
procedure drawTexture(id, index, level, time: cint;
                      sizeX, sizeY, centerX, centerY, posX, posY, rotation,
                      screenHeightScaleFactor, r, g, b, a: cfloat); cdecl; external 'ScriptHookV.dll' name '?drawTexture@@YAXHHHHMMMMMMMMMMMM@Z';
function createTexture(const TexFileName: PChar): cint; cdecl; external 'ScriptHookV.dll' name '?createTexture@@YAHPEBD@Z';
procedure keyboardHandlerUnregister(handler: KeyboardHandler); cdecl; external 'ScriptHookV.dll' name '?keyboardHandlerUnregister@@YAXP6AXKGEHHHH@Z@Z';
procedure keyboardHandlerRegister(handler: KeyboardHandler); cdecl; external 'ScriptHookV.dll' name '?keyboardHandlerRegister@@YAXP6AXKGEHHHH@Z@Z';
procedure scriptUnregister(scriptmain: ScriptMainFunc); cdecl; external 'ScriptHookV.dll'  name '?scriptUnregister@@YAXP6AXXZ@Z';
procedure scriptUnregister(module: HINST); cdecl; external 'ScriptHookV.dll' name '?scriptUnregister@@YAXPEAUHINSTANCE__@@@Z';
procedure scriptRegisterAdditionalThread(module: HINST; threadfunc: ScriptMainFunc); cdecl; external 'ScriptHookV.dll' name '?scriptRegisterAdditionalThread@@YAXPEAUHINSTANCE__@@P6AXXZ@Z';
procedure scriptRegister(module: HINST; scriptmain: ScriptMainFunc); cdecl; external 'ScriptHookV.dll' name '?scriptRegister@@YAXPEAUHINSTANCE__@@P6AXXZ@Z';
procedure scriptWait(time: ULONG); cdecl; external 'ScriptHookV.dll' name '?scriptWait@@YAXK@Z';
function getGlobalPtr(globalID: cint): PUINT64; cdecl; external 'ScriptHookV.dll' name '?getGlobalPtr@@YAPEA_KH@Z';
function getGameVersion: eGameVersion; cdecl; external 'ScriptHookV.dll' name '?getGameVersion@@YA?AW4eGameVersion@@XZ';
function getScriptHandleBaseAddress(i: cint): PChar; cdecl; external 'ScriptHookV.dll' name '?getScriptHandleBaseAddress@@YAPEAEH@Z';
function worldGetAllObjects(objects: pcint; count: cint): cint; cdecl; external 'ScriptHookV.dll' name '?worldGetAllObjects@@YAHPEAHH@Z';
function worldGetAllPeds(peds: pcint; count: cint): cint; cdecl; external 'ScriptHookV.dll' name '?worldGetAllPeds@@YAHPEAHH@Z';
function worldGetAllPickups(pickups: pcint; count: cint): cint; cdecl; external 'ScriptHookV.dll' name '?worldGetAllPickups@@YAHPEAHH@Z';
function worldGetAllVehicles(vehicles: pcint; count: cint): cint; cdecl; external 'ScriptHookV.dll' name '?worldGetAllVehicles@@YAHPEAHH@Z';



procedure ScriptHookVWait(time: ULONG);
procedure ScriptHookVTerminate;


implementation


procedure ScriptHookVWait(time: ULONG);
begin
  scriptWait(time);
end;

procedure ScriptHookVTerminate;
begin
  ScriptHookVWait(High(ULONG));
end;

end.

