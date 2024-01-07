unit YOS_Script;

{$mode objfpc}{$H+}

interface

uses
  ScriptHookV, Natives, Windows, ctypes, SysUtils, Classes,
  YOS_Screen, YOS_WorldManager, YOS_Utils, YOS_DataFiles;

const
  SCRIPT_MAJOR_VERSION = 2;
  SCRIPT_MINOR_VERSION = 2;

type
  // Forward declarations
  TMissionThread = class;
  TMissionScript = class;

  // Script base types
  TVariableLocation = (svlDirect = 0, svlLocal = 1, svlGlobal = 2);
  TVariableType = (svtNone = 0, svtInt = 1, svtFloat = 2, svtString = 3, svtVector = 4, svtArray = 5, svtStringArray = 6, svtBlip = 7, svtPickup = 8, svtSavedObject = 9, svtForbiddenCube = 10, svtCarGenerator = 11);
  TConditionalJumpEvaluationMethod  = (cjmpSingle = 0, cjmpIfAnd = 1, cjmpIfOr = 2);
  TNativeParamType = (nptNum32 = 0, nptVarNum32 = 1, nptInt32Ptr = 2, nptFloat32Ptr = 3, nptVector = 4, nptArray = 5, nptString = 6, nptStringPtr = 7);
  TMissionThreadStatus = (tstRunning = 0, tstWaiting = 1, tstFinished = 2);

  // Script complex types
  TVariable = record
    VarType: TVariableType;
    Value: UINT32; // This is a raw, 32-bit data
  end;
  TStringVar = record
    Used: boolean;
    Value: string;
  end;
  TVectorVar = record
    Used: boolean;
    Value: Vector3;
  end;
  TArrayVar = record
    Used: boolean;
    Value: TGTA5Array; // This is an array of raw, 32-bit data
  end;
  TStringArrayVar = record
    Used: boolean;
    Value: array of string;
  end;
  TBlipVar = record
    Used, IsMission, IsShortRange, Selectable, HasRadius: boolean;
    X, Y, Z, Radius: cfloat;
    Sprite, Color, Alpha: cint;
    _handle: Blip;
  end;
  TPickupVar = record
    Used, PickedUp, HasRotation: boolean;
    X, Y, Z, XA, YA, ZA: cfloat;
    PickupHash, ModelHash: Hash;
    Value, RgTime, RgTimeRem: cint;
    _handle: Pickup;
  end;
  TSavedObjectVar = record
    Used: boolean;
    _handle: GTAObject;
  end;
  TForbiddenCubeVar = record
    Used: boolean;
    CubeType: byte;
    X1, Y1, Z1, X2, Y2, Z2: cfloat;
    _handle: cint; // Handle used for Scenario Disabled Cubes
  end;
  TCarGeneratorVar = record
    Used, IsActive: boolean;
    Model: Hash;
    X, Y, Z, A: cfloat;
    _handle: cint;
  end;

  // Data storage types
  TRespawnLocation = record
    X, Y, Z, A: cfloat;
  end;
  TModelSwapRecord = record
    OrigHash, NewHash: Hash;
    X, Y, Z, Radius: cfloat;
  end;
  TModelHideRecord = record
    ModelHash: Hash;
    X, Y, Z, Radius: cfloat;
  end;

  TStoredVehicleData = record
    Model: Hash;
    PriColor, SecColor: cint;
    CustColorPri, CustColorSec: record
      Enabled: boolean;
      R, G, B: cint;
    end;
    WheelType: cint;
    Mods: array [0..49] of record
      ModIndex: cint;
      ModVariation: boolean;
    end;
    BulletProofTires: boolean;
    TyreSmokeColor: record
      R, G, B: cint;
    end;
    NumberPlateIndex: cint;
    NumberPlateText: string;
    PearlColor, RimColor: cint;
    RoofState: boolean; // For Convertibles
    Extras: array [0..60] of boolean;
    Livery: cint;
    NeonColor: record
      R, G, B: cint;
    end;
    WindowTint, InteriorColor, DashboardColor, XenonColor: cint;
  end;
  TStoredPlayerData = record
    X, Y, Z, A: cfloat;
    Health, MaxHealth, Armor: cint;
    Model: Hash;
    OutfitData: record
      Drawables: array [0..11] of record
          DrawableVar, TextureVar, PaletteVar: cint;
      end;
      Props: array [0..2] of record
          PropIndex, PropTexture: cint;
      end;
    end;
    WeaponData: record
      WeaponList: array of record
          HashCode: Hash;
          TintIndex, Ammo: cint;
          Components: array of cint;
      end;
    end;
    IsDriving: boolean;
    VehicleData: TStoredVehicleData;
  end;
  TMultiplayerPersistenceData = record
    Used: boolean;
    Data: TStoredPlayerData;
  end;
  TVehiclePersistenceData = record
    X, Y, Z, A: cfloat;
    VehicleData: TStoredVehicleData;
  end;
  TStoredVehicleSlot = record
    IsStored: boolean;
    VehicleData: TStoredVehicleData;
  end;

  // Mission script thread manager class
  TMissionThread = class (TObject)
  private
    // Management-only properties
    script: TMissionScript;
    lastRunTime, lastRunTimeDiff: INT64;
    nativeCallResult: PUINT64;

    // Persistent properties
    offset: UINT64;
    position: INT64;
    status: TMissionThreadStatus;
    waitTime: UINT32;
    scriptName: string;
    gosubLevel: byte;
    gosubReturn: array [0..127] of INT64;
    localVars: array [0..1023] of TVariable;
    conditionalJumpEvaluationMethod: TConditionalJumpEvaluationMethod;
    conditionalValues: array of boolean;
    timers: array [0..15] of cfloat;
    wbGosubActive: boolean;
    wbGosubLevel: byte;
    wbGosubReturn: INT64;
    isSavedGame, isSaveSuccessful: boolean;
    // Internal functions
    function GetVarData(vloc: TVariableLocation; id: word): UINT32;
    procedure SetVarData(vloc: TVariableLocation; vtype: TVariableType; id: word; value: UINT32);
    function GetVarFloat(vloc: TVariableLocation; id: word): cfloat;
    procedure SetVarFloat(vloc: TVariableLocation; id: word; value: cfloat);
    function GetVarInt(vloc: TVariableLocation; id: word): cint;
    procedure SetVarInt(vloc: TVariableLocation; id: word; value: cint);
    procedure GetVarId(out vloc: TVariableLocation; out id: word);
    function GetVarType(vloc: TVariableLocation; id: word): TVariableType;
    procedure ClearVariable(vloc: TVariableLocation; id: word);
    procedure AddConditionalResult(result, negate: boolean);
    procedure AllocateString(vloc: TVariableLocation; id: word; value: string);
    function GetNextParameterInt: cint;
    function GetNextParameterFloat: cfloat;
    function GetNextParameterString: string;
    procedure CallOpcodeHandler(opcode: word);
    procedure Initialize;
  public
    // Public functions
    constructor Create(owner: TMissionScript; fileOffset: UINT64);
    constructor Create(owner: TMissionScript; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the thread data is loaded from a savegame file
    destructor Destroy; override;
    procedure Save(stream: TStream); // Used, when the thread data is saved into a savegame file
    procedure Run(isPlayerDead: boolean);
    property ThreadName: string read scriptName;
    property ThreadStatus: TMissionThreadStatus read status write status;
  end;

  // Mission script manager class
  TMissionScript = class (TObject)
  private
    // Management-only properties
    bytes: TMemoryStream;
    lastPickupCheckTime, lastPickupCheckTimeDiff: INT64;
    isStarted: boolean;

    // Persistent properties
    threads: array of TMissionThread;
    globalVars: array [0..65535] of TVariable;
    globalStrings: array [0..8191] of TStringVar;
    globalVectors: array [0..8191] of TVectorVar;
    globalArrays: array [0..1023] of TArrayVar;
    globalStringLists: array [0..127] of TStringArrayVar;
    globalBlips: array [0..255] of TBlipVar;
    globalPickups: array [0..255] of TPickupVar;
    globalSavedObjects: array [0..255] of TSavedObjectVar;
    globalForbiddenCubes: array [0..255] of TForbiddenCubeVar;
    globalCarGenerators: array [0..511] of TCarGeneratorVar;
    storedPlayerData: array [0..2] of TMultiplayerPersistenceData;
    storedVehicleData: array of TVehiclePersistenceData;
    storedVehicleSlots: array [0..15] of TStoredVehicleSlot;
    enabledMaps, disabledMaps: TStrings;
    modelSwapRecords: array of TModelSwapRecord;
    modelHideRecords: array of TModelHideRecord;
    hospitalRestarts, policeRestarts: array of TRespawnLocation;
    overrideRestart: TRespawnLocation;
    useOverrideRestart, isOnMission: boolean;
    lastMission: string;
    floatEqualThreshold: cfloat;
    // Internal functions
    procedure KillThreadByName(tname: string);
    procedure CreateThread(offset: UINT64);
    procedure ClearVariable(var variableRecord: TVariable);
    procedure CreateManagedBlip(var blipRecord: TBlipVar);
    procedure UpdateBlip(blipRecord: TBlipVar);
    procedure CreateManagedPickup(var pickupRecord: TPickupVar);
    procedure ManageObjects;
    procedure RestoreManagedGameObject(var objectRecord: TSavedObjectVar; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the script data is loaded from a savegame file
    procedure RestoreForbiddenCube(var forbiddenCubeRecord: TForbiddenCubeVar; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the script data is loaded from a savegame file
    procedure RestoreCarGenerator(var carGeneratorRecord: TCarGeneratorVar; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the script data is loaded from a savegame file
    procedure StoreManagedObject(objectRecord: TSavedObjectVar; stream: TStream); // Used, when the script data is saved into a savegame file
    procedure StoreForbiddenCube(forbiddenCubeRecord: TForbiddenCubeVar; stream: TStream); // Used, when the script data is saved into a savegame file
    procedure StoreCarGenerator(carGeneratorRecord: TCarGeneratorVar; stream: TStream); // Used, when the script data is saved into a savegame file
    procedure StorePlayer(var data: TStoredPlayerData; actor: Ped);
    procedure RestorePlayer(data: TStoredPlayerData; actor: Ped); // NOTE: this applies the stored properties onto the specified ped
    procedure LoadPlayerData(var data: TStoredPlayerData; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the script data is loaded from a savegame file
    procedure SavePlayerData(data: TStoredPlayerData; stream: TStream); // Used, when the script data is saved into a savegame file
    procedure StoreVehicle(var data: TStoredVehicleData; veh: Vehicle);
    function RestoreVehicle(data: TVehiclePersistenceData): Vehicle; // NOTE: this spawns a vehicle in the gameworld
    procedure LoadVehicleData(var data: TStoredVehicleData; stream: TStream; savMajorVer, savMinorVer: integer); // Used, when the script data is loaded from a savegame file
    procedure SaveVehicleData(data: TStoredVehicleData; stream: TStream); // Used, when the script data is saved into a savegame file
  public
    // Public functions
    constructor Create(scriptfile: string);
    destructor Destroy; override;
    procedure Load(slot: integer); // Used, when the script data is loaded from a savegame file
    procedure Save(slot: integer); // Used, when the script data is saved into a savegame file
    procedure Reset;
    procedure Run(isPlayerDead: boolean);
    function IsAvailable: boolean;
    function ThreadCount: integer; // Number of running threads
    function FindNearestRespawnLocation(out location: TRespawnLocation; findHospital: boolean): boolean;
    function GetInternalPlayerIndex: integer;
    function GetActorInternalPlayerIndex(actor: Ped): integer;
    function IsManagedBlip(handle: Blip): boolean;
    function IsManagedPickup(handle: Pickup): boolean;
    function IsManagedObject(handle: GTAObject): boolean;
    function IsManagedCarGenerator(handle: cint): boolean;
    property HasOverrideRestart: boolean read useOverrideRestart;
    property OverrideRestartLocation: TRespawnLocation read overrideRestart;
    property IsGameStarted: boolean read isStarted;
  end;

const
  DefaultRespawnLocation: TRespawnLocation = (X: 4000.0; Y: -4000.0; Z: 0.0; A: 45.0);

var
  MissionScript: TMissionScript;

implementation

var
  _is_dll_init_final_: boolean; // This must be set to true during DLL init / final, to prevent object init/final from calling Natives, and causing a game crash!
  pFreq: LARGE_INTEGER;
  WeaponModels, WeaponCompModels: TStrings;

function CurrentTimeMs: int64;
var
  currentTime: LARGE_INTEGER;
begin
  QueryPerformanceCounter(@currentTime);
  Result := System.trunc((1000 * currentTime.QuadPart) / pFreq.QuadPart);
end;

{$I opcode_bin_helpers.inc}
{$I opcodes.inc}

// ################
// TMissionThread
// ################
{%region /fold 'TMissionThread'}

procedure TMissionThread.CallOpcodeHandler(opcode: word);
begin
  if (opcode >= $8000) then
    OpCodes[opcode - $8000](Self, true)
  else
    OpCodes[opcode](Self, false);
end;

procedure TMissionThread.Initialize;
var
  i: integer;
begin
  script := nil;
  lastRunTime := 0;
  lastRunTimeDiff := 0;
  nativeCallResult := nil;
  offset := 0;
  position := 0;
  status := tstRunning;
  waitTime := 0;
  scriptName := Format('NONAME_%s', [UpCase(HexStr(Pointer(Self)))]);
  gosubLevel := 0;
  for i := 0 to High(gosubReturn) do
      gosubReturn[i] := 0;
  for i := 0 to High(localVars) do
      begin
        localVars[i].VarType := svtNone;
        localVars[i].Value := 0;
      end;
  conditionalJumpEvaluationMethod := cjmpSingle;
  SetLength(conditionalValues, 0);
  for i := 0 to High(timers) do
      timers[i] := 0.0;
  wbGosubActive := false;
  wbGosubLevel := 0;
  wbGosubReturn := 0;
  isSavedGame := false;
  isSaveSuccessful := false;
end;

constructor TMissionThread.Create(owner: TMissionScript; fileOffset: UINT64);
begin
  inherited Create;
  Initialize;
  script := owner;
  offset := fileOffset;
end;

constructor TMissionThread.Create(owner: TMissionScript; stream: TStream; savMajorVer, savMinorVer: integer);
var
  i: integer;
  u32: UINT32;
begin
  inherited Create;
  Initialize;
  script := owner;
  offset := stream.ReadQWord;
  position := INT64(stream.ReadQWord);
  status := TMissionThreadStatus(stream.ReadByte);
  waitTime := stream.ReadDWord;
  scriptName := ReadCString(stream);
  gosubLevel := stream.ReadByte;
  for i := 0 to High(gosubReturn) do
      gosubReturn[i] := INT64(stream.ReadQWord);
  for i := 0 to High(localVars) do
      begin
        localVars[i].VarType := TVariableType(stream.ReadByte);
        localVars[i].Value := stream.ReadDWord;
      end;
  conditionalJumpEvaluationMethod := TConditionalJumpEvaluationMethod(stream.ReadByte);
  SetLength(conditionalValues, integer(stream.ReadDWord));
  for i := 0 to High(conditionalValues) do
      conditionalValues[i] := boolean(stream.ReadByte);
  for i := 0 to High(timers) do
      begin
        u32 := stream.ReadDWord;
        timers[i] := pcfloat(@u32)^;
      end;
  wbGosubActive := (stream.ReadByte <> 0);
  if wbGosubActive then
    begin
      wbGosubLevel := stream.ReadByte;
      wbGosubReturn := INT64(stream.ReadQWord);
    end;
  isSavedGame := true;
end;

destructor TMissionThread.Destroy;
var
  i: integer;
begin
  scriptName := '';
  if (script <> nil) then
    for i := 0 to High(localVars) do
        script.ClearVariable(localVars[i]);
  script := nil;
  SetLength(conditionalValues, 0);
  inherited;
end;

procedure TMissionThread.Save(stream: TStream);
var
  i: integer;
  f: cfloat;
begin
  stream.WriteQWord(offset);
  stream.WriteQWord(position);
  stream.WriteByte(ord(status));
  stream.WriteDWord(waitTime);
  WriteCString(scriptName, stream);
  stream.WriteByte(gosubLevel);
  for i := 0 to High(gosubReturn) do
      stream.WriteQWord(gosubReturn[i]);
  for i := 0 to High(localVars) do
      begin
        stream.WriteByte(ord(localVars[i].VarType));
        stream.WriteDWord(localVars[i].Value);
      end;
  stream.WriteByte(ord(conditionalJumpEvaluationMethod));
  stream.WriteDWord(cint(Length(conditionalValues)));
  for i := 0 to High(conditionalValues) do
      if conditionalValues[i] then
        stream.WriteByte(1)
      else
        stream.WriteByte(0);
  for i := 0 to High(timers) do
      begin
        f := timers[i];
        stream.WriteDWord(PUINT32(@f)^);
      end;
  if wbGosubActive then
    begin
      stream.WriteByte(1);
      stream.WriteByte(wbGosubLevel);
      stream.WriteQWord(wbGosubReturn);
    end
  else
    stream.WriteByte(0);
  isSaveSuccessful := true;
end;

procedure TMissionThread.Run(isPlayerDead: boolean);
var
  currentTime: INT64;
  i: integer;
begin
  if (GameScreen.MenuMode = gmInGame) then
    while (status = tstRunning) or (status = tstWaiting) do
          begin
            // If the wasted-busted return is active, and the player is wasted or busted, perform a return from a wb gosub function
            if wbGosubActive and (gosubLevel > wbGosubLevel) and isPlayerDead then
              begin
                wbGosubActive := false;
                gosubLevel := wbGosubLevel;
                position := wbGosubReturn;
                waitTime := 0;
              end;

            // If we have ANY amount of wait time remaining, we just wait...
            if (waitTime > 0) then
              begin
                currentTime := CurrentTimeMs;
                lastRunTimeDiff := lastRunTimeDiff + (currentTime - lastRunTime);
                if (lastRunTimeDiff > 1000) then // We assume, that an 1 sec lag may means, that the user has paused their game...
                  lastRunTimeDiff := 0
                else if (lastRunTimeDiff > 0) then
                  begin
                    for i := 0 to High(timers) do
                        timers[i] := timers[i] + (lastRunTimeDiff / 1000); // Increase Timers
                    if (lastRunTimeDiff >= waitTime) then
                      waitTime := 0
                    else
                      dec(waitTime, lastRunTimeDiff);
                    lastRunTimeDiff := 0
                  end;
                lastRunTime := CurrentTimeMs;
                break;
              end

            // If we have NO wait time remaining, but we're still in the 'waiting' state, switch back to the 'running' state
            else if (status = tstWaiting) then
              begin
                status := tstRunning;
                lastRunTime := CurrentTimeMs;
                break;
              end

            // Go ahead boy...
            else
              begin
                script.bytes.Position := offset + position;
                inc(position, 2); {--->>> 2b opcode}
                CallOpcodeHandler(script.bytes.ReadWord);
                for i := 0 to High(timers) do
                    timers[i] := timers[i] + ((CurrentTimeMs - lastRunTime) / 1000); // Increase Timers
                lastRunTime := CurrentTimeMs;
              end;
          end
  else
    lastRunTime := CurrentTimeMs;
end;

{%endregion}

// ################
// TMissionScript
// ################
{%region /fold 'TMissionScript'}

procedure TMissionScript.KillThreadByName(tname: string);
var
  i: integer;
begin
  for i := 0 to High(threads) do
      if (LowerCase(threads[i].ThreadName) = LowerCase(tname)) then
        threads[i].ThreadStatus := tstFinished;
end;

procedure TMissionScript.CreateThread(offset: UINT64);
begin
  SetLength(threads, Length(threads) + 1);
  threads[High(threads)] := TMissionThread.Create(Self, offset);
end;

procedure TMissionScript.ClearVariable(var variableRecord: TVariable);
var
  vindex: UINT32;
begin
  // Free the related variable resources
  vindex := variableRecord.Value;
  case variableRecord.VarType of
       svtString:
         begin
           globalStrings[vindex].Used := false;
           globalStrings[vindex].Value := '';
         end;
       svtVector:
         begin
           globalVectors[vindex].Used := false;
           ZeroMemory(@globalVectors[vindex].Value, sizeof(Vector3));
         end;
       svtArray:
         begin
           globalArrays[vindex].Used := false;
           SetLength(globalArrays[vindex].Value, 0);
         end;
       svtStringArray:
         begin
           globalStringLists[vindex].Used := false;
           SetLength(globalStringLists[vindex].Value, 0);
         end;
       svtBlip:
         begin
           globalBlips[vindex].Used := false;
           if (not _is_dll_init_final_) and (globalBlips[vindex]._handle <> 0) then
             begin
               if (DOES_BLIP_EXIST(globalBlips[vindex]._handle) <> BOOL(0)) then
                 REMOVE_BLIP(@globalBlips[vindex]._handle);
             end;
           ZeroMemory(@globalBlips[vindex], sizeof(TBlipVar));
         end;
       svtPickup:
         begin
           globalPickups[vindex].Used := false;
           if (not _is_dll_init_final_) and (globalPickups[vindex]._handle <> 0) then
             begin
               if (DOES_PICKUP_OBJECT_EXIST(globalPickups[vindex]._handle) <> BOOL(0)) then
                 REMOVE_PICKUP(globalPickups[vindex]._handle);
             end;
           ZeroMemory(@globalPickups[vindex], sizeof(TPickupVar));
         end;
       svtSavedObject:
         begin
           globalSavedObjects[vindex].Used := false;
           ZeroMemory(@globalSavedObjects[vindex], sizeof(TSavedObjectVar));
         end;
       svtForbiddenCube:
         begin
           globalForbiddenCubes[vindex].Used := false;
           if (not _is_dll_init_final_) then
             begin
               case globalForbiddenCubes[vindex].CubeType of
                    0: SET_PED_PATHS_BACK_TO_ORIGINAL(globalForbiddenCubes[vindex].X1, globalForbiddenCubes[vindex].Y1, globalForbiddenCubes[vindex].Z1, globalForbiddenCubes[vindex].X2, globalForbiddenCubes[vindex].Y2, globalForbiddenCubes[vindex].Z2, 1);
                    1: SET_ROADS_BACK_TO_ORIGINAL(globalForbiddenCubes[vindex].X1, globalForbiddenCubes[vindex].Y1, globalForbiddenCubes[vindex].Z1, globalForbiddenCubes[vindex].X2, globalForbiddenCubes[vindex].Y2, globalForbiddenCubes[vindex].Z2, 1);
                    2: REMOVE_SCENARIO_BLOCKING_AREA(globalForbiddenCubes[vindex]._handle, BOOL(0));
               end;
             end;
           ZeroMemory(@globalForbiddenCubes[vindex], sizeof(TForbiddenCubeVar));
         end;
       svtCarGenerator:
         begin
           globalCarGenerators[vindex].Used := false;
           if (not _is_dll_init_final_) and globalCarGenerators[vindex].IsActive and (globalCarGenerators[vindex]._handle <> 0) then
             begin
               if (DOES_SCRIPT_VEHICLE_GENERATOR_EXIST(globalCarGenerators[vindex]._handle) <> BOOL(0)) then
                 DELETE_SCRIPT_VEHICLE_GENERATOR(globalCarGenerators[vindex]._handle);
             end;
           ZeroMemory(@globalCarGenerators[vindex], sizeof(TCarGeneratorVar));
         end;
  end;

  // Reset var record
  variableRecord.VarType := svtNone;
  variableRecord.Value := 0;
end;

procedure TMissionScript.CreateManagedBlip(var blipRecord: TBlipVar);
begin
  if blipRecord.HasRadius then
    begin
      blipRecord._handle := ADD_BLIP_FOR_RADIUS(blipRecord.X, blipRecord.Y, blipRecord.Z, blipRecord.Radius);
      SET_BLIP_ALPHA(blipRecord._handle, blipRecord.Alpha);
    end
  else
    begin
      blipRecord._handle := ADD_BLIP_FOR_COORD(blipRecord.X, blipRecord.Y, blipRecord.Z);
      SET_BLIP_SPRITE(blipRecord._handle, blipRecord.Sprite);
    end;
  UpdateBlip(blipRecord);
end;

procedure TMissionScript.UpdateBlip(blipRecord: TBlipVar);
var
  flags: cint;
begin
  if (DOES_BLIP_EXIST(blipRecord._handle) <> BOOL(0)) then
    begin
      SET_BLIP_COLOUR(blipRecord._handle, blipRecord.Color);
      SET_BLIP_AS_SHORT_RANGE(blipRecord._handle, BOOL(blipRecord.IsShortRange and (not blipRecord.HasRadius)));
      if blipRecord.Selectable and (not blipRecord.HasRadius) then
        flags := 2
      else
         flags := 8;
      SET_BLIP_DISPLAY(blipRecord._handle, flags);
    end;
end;

procedure TMissionScript.CreateManagedPickup(var pickupRecord: TPickupVar);
begin
  if pickupRecord.HasRotation then
    pickupRecord._handle := CREATE_PICKUP_ROTATE(pickupRecord.PickupHash, pickupRecord.X, pickupRecord.Y, pickupRecord.Z, pickupRecord.XA, pickupRecord.YA, pickupRecord.ZA, 8, pickupRecord.Value, 2, BOOL(1), pickupRecord.ModelHash)
  else
    pickupRecord._handle := CREATE_PICKUP(pickupRecord.PickupHash, pickupRecord.X, pickupRecord.Y, pickupRecord.Z, 512, pickupRecord.Value, BOOL(1), pickupRecord.ModelHash);
  pickupRecord.PickedUp := false;
end;

procedure TMissionScript.ManageObjects;
var
  currentTime: INT64;
  i: integer;
begin
  // Manages blips, that are not meant to be seen during missions
  for i := 0 to High(globalBlips) do
      if globalBlips[i].Used then
        begin
          if isOnMission then
            begin
              if (not globalBlips[i].IsMission) and (globalBlips[i]._handle <> 0) and (DOES_BLIP_EXIST(globalBlips[i]._handle) <> BOOL(0)) then
                begin
                  REMOVE_BLIP(@globalBlips[i]._handle);
                  globalBlips[i]._handle := 0;
                end;
            end
          else
            begin
              if (not globalBlips[i].IsMission) and ((globalBlips[i]._handle = 0) or (DOES_BLIP_EXIST(globalBlips[i]._handle) = BOOL(0))) then
                CreateManagedBlip(globalBlips[i]);
            end;
        end;

  // Manages pickups' regeneration and 'picked up' property
  for i := 0 to High(globalPickups) do
      if globalPickups[i].Used then
        begin
          if globalPickups[i].PickedUp then
            begin
              if (globalPickups[i].RgTimeRem = 0) and (globalPickups[i].RgTime >= 0) then
                CreateManagedPickup(globalPickups[i])
              else
                begin
                  currentTime := currentTimeMs;
                  lastPickupCheckTimeDiff := lastPickupCheckTimeDiff + (currentTime - lastPickupCheckTime);
                  if (lastPickupCheckTimeDiff > 1000) then // We assume, that an 1 sec lag may means, that the user has paused their game...
                    lastPickupCheckTimeDiff := 0
                  else if (lastPickupCheckTimeDiff > 0) then
                    begin
                      if (lastPickupCheckTimeDiff > globalPickups[i].RgTimeRem) then
                        globalPickups[i].RgTimeRem := 0
                      else
                        dec(globalPickups[i].RgTimeRem, lastPickupCheckTimeDiff);
                      lastPickupCheckTimeDiff := 0;
                    end;
                  lastPickupCheckTime := currentTimeMs;
                end;
            end
          else if (globalPickups[i]._handle = 0) or (DOES_PICKUP_OBJECT_EXIST(globalPickups[i]._handle) = BOOL(0)) then
            begin
              globalPickups[i].PickedUp := true;
              globalPickups[i].RgTimeRem := globalPickups[i].RgTime;
              if (globalPickups[i]._handle <> 0) then
                REMOVE_PICKUP(globalPickups[i]._handle);
              globalPickups[i]._handle := 0;
              lastPickupCheckTimeDiff := 0;
            end;
        end;
  lastPickupCheckTime := currentTimeMs;
end;

procedure TMissionScript.RestoreManagedGameObject(var objectRecord: TSavedObjectVar; stream: TStream; savMajorVer, savMinorVer: integer);
var
  model: Hash;
  u32: UINT32;
  x, y, z, xa, ya, za: cfloat;
begin
  model := stream.ReadDWord;
  u32 := stream.ReadDWord;
  x := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  y := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  z := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  xa := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  ya := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  za := pcfloat(@u32)^;

  // Spawn object
  if (model <> 0) then // Only try to load objects, that were valid upon being saved....
    begin
      while (HAS_MODEL_LOADED(model) = BOOL(0)) do
            begin
              REQUEST_MODEL(model);
              GameScreen.DrawLoadingScreen;
              ScriptHookVWait(0);
            end;
      objectRecord._handle := CREATE_OBJECT(model, x, y, z, BOOL(0), BOOL(1), BOOL(0));
      SET_ENTITY_ROTATION(objectRecord._handle, xa, ya, za, 1, BOOL(1));
      SET_MODEL_AS_NO_LONGER_NEEDED(model);
    end
  else
    objectRecord._handle := 0;
end;

procedure TMissionScript.RestoreForbiddenCube(var forbiddenCubeRecord: TForbiddenCubeVar; stream: TStream; savMajorVer, savMinorVer: integer);
var
  u32: UINT32;
begin
  forbiddenCubeRecord.CubeType := stream.ReadByte;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.X1 := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.Y1 := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.Z1 := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.X2 := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.Y2 := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  forbiddenCubeRecord.Z2 := pcfloat(@u32)^;
  case forbiddenCubeRecord.CubeType of
       0: SET_PED_PATHS_IN_AREA(forbiddenCubeRecord.X1, forbiddenCubeRecord.Y1, forbiddenCubeRecord.Z1, forbiddenCubeRecord.X2, forbiddenCubeRecord.Y2, forbiddenCubeRecord.Z2, BOOL(0), 0);
       1: SET_ROADS_IN_AREA(forbiddenCubeRecord.X1, forbiddenCubeRecord.Y1, forbiddenCubeRecord.Z1, forbiddenCubeRecord.X2, forbiddenCubeRecord.Y2, forbiddenCubeRecord.Z2, BOOL(0), BOOL(1));
       2: forbiddenCubeRecord._handle := ADD_SCENARIO_BLOCKING_AREA(forbiddenCubeRecord.X1, forbiddenCubeRecord.Y1, forbiddenCubeRecord.Z1, forbiddenCubeRecord.X2, forbiddenCubeRecord.Y2, forbiddenCubeRecord.Z2, BOOL(0), BOOL(1), BOOL(1), BOOL(1), 1);
  end;
end;

procedure TMissionScript.RestoreCarGenerator(var carGeneratorRecord: TCarGeneratorVar; stream: TStream; savMajorVer, savMinorVer: integer);
var
  u32: UINT32;
begin
  carGeneratorRecord.IsActive := (stream.ReadByte <> 0);
  carGeneratorRecord.Model := stream.ReadDWord;
  u32 := stream.ReadDWord;
  carGeneratorRecord.X := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  carGeneratorRecord.Y := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  carGeneratorRecord.Z := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  carGeneratorRecord.A := pcfloat(@u32)^;
  if carGeneratorRecord.IsActive then
    carGeneratorRecord._handle := CREATE_SCRIPT_VEHICLE_GENERATOR(carGeneratorRecord.X, carGeneratorRecord.Y, carGeneratorRecord.Z, carGeneratorRecord.A, 5.0, 3.0, carGeneratorRecord.Model, -1, -1, -1, -1, BOOL(1), BOOL(0), BOOL(0), BOOL(0), BOOL(1), -1)
  else
    carGeneratorRecord._handle := 0;
end;

procedure TMissionScript.StoreManagedObject(objectRecord: TSavedObjectVar; stream: TStream);
var
  pos: Vector3;
  cf: cfloat;
begin
  if (objectRecord._handle <> 0) and (GET_ENTITY_TYPE(objectRecord._handle) = 3) then // Only try to write valid properties for actually valid objects
    begin
      stream.WriteDWord(GET_ENTITY_MODEL(objectRecord._handle));
      pos := GET_ENTITY_COORDS(objectRecord._handle, BOOL(0));
      stream.WriteDWord(PUINT32(@pos.x)^);
      stream.WriteDWord(PUINT32(@pos.y)^);
      stream.WriteDWord(PUINT32(@pos.z)^);
      cf := GET_ENTITY_PITCH(objectRecord._handle);
      stream.WriteDWord(PUINT32(@cf)^);
      cf := GET_ENTITY_ROLL(objectRecord._handle);
      stream.WriteDWord(PUINT32(@cf)^);
      cf := GET_ENTITY_HEADING(objectRecord._handle);
      stream.WriteDWord(PUINT32(@cf)^);
    end
  else
    begin
      // Dummy data for invalid, but still referenced objects. All fileds, even the floats, are 4 byte DWORDs. We fill the whole record with zeroes...
      stream.WriteDWord(0); // Model
      stream.WriteDWord(0); // X
      stream.WriteDWord(0); // Y
      stream.WriteDWord(0); // Z
      stream.WriteDWord(0); // XA
      stream.WriteDWord(0); // YA
      stream.WriteDWord(0); // ZA
    end;
end;

procedure TMissionScript.StoreForbiddenCube(forbiddenCubeRecord: TForbiddenCubeVar; stream: TStream);
var
  cf: cfloat;
begin
  stream.WriteByte(forbiddenCubeRecord.CubeType);
  cf := forbiddenCubeRecord.X1;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := forbiddenCubeRecord.Y1;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := forbiddenCubeRecord.Z1;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := forbiddenCubeRecord.X2;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := forbiddenCubeRecord.Y2;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := forbiddenCubeRecord.Z2;
  stream.WriteDWord(PUINT32(@cf)^);
end;

procedure TMissionScript.StoreCarGenerator(carGeneratorRecord: TCarGeneratorVar; stream: TStream);
var
  cf: cfloat;
begin
  if carGeneratorRecord.IsActive then
    stream.WriteByte(1)
  else
    stream.WriteByte(0);
  stream.WriteDWord(carGeneratorRecord.Model);
  cf := carGeneratorRecord.X;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := carGeneratorRecord.Y;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := carGeneratorRecord.Z;
  stream.WriteDWord(PUINT32(@cf)^);
  cf := carGeneratorRecord.A;
  stream.WriteDWord(PUINT32(@cf)^);
end;

procedure TMissionScript.StorePlayer(var data: TStoredPlayerData; actor: Ped);
var
  pos: Vector3;
  veh: Vehicle;
  hash1, hash2: Hash;
  h, i, j, k: integer;
begin
  // Basic data
  pos := GET_ENTITY_COORDS(actor, BOOL(0));
  data.X := pos.x;
  data.Y := pos.y;
  data.Z := pos.z;
  data.A := GET_ENTITY_HEADING(actor);
  data.Health := GET_ENTITY_HEALTH(actor);
  data.MaxHealth := GET_PED_MAX_HEALTH(actor);
  data.Armor := GET_PED_ARMOUR(actor);
  // Model and outfit
  data.Model := GET_ENTITY_MODEL(actor);
  for i := 0 to 11 do
      if (i <> 7) then // Drawable 7 is unused!
        begin
          data.OutfitData.Drawables[i].DrawableVar := GET_PED_DRAWABLE_VARIATION(actor, cint(i));
          data.OutfitData.Drawables[i].TextureVar := GET_PED_TEXTURE_VARIATION(actor, cint(i));
          data.OutfitData.Drawables[i].PaletteVar := GET_PED_PALETTE_VARIATION(actor, cint(i));
        end
      else
        begin
          data.OutfitData.Drawables[i].DrawableVar := 0;
          data.OutfitData.Drawables[i].TextureVar := 0;
          data.OutfitData.Drawables[i].PaletteVar := 0;
        end;
  for i := 0 to 2 do
      begin
        data.OutfitData.Props[i].PropIndex := GET_PED_PROP_INDEX(actor, cint(i), 1);
        data.OutfitData.Props[i].PropTexture := GET_PED_PROP_TEXTURE_INDEX(actor, cint(i));
      end;
  // Weapon data
  SetLength(data.WeaponData.WeaponList, 0);
  for i := 0 to WeaponModels.Count - 1 do
      begin
        hash1 := GET_HASH_KEY(PChar(WeaponModels[i]));
        if (HAS_PED_GOT_WEAPON(actor, hash1, BOOL(0)) <> BOOL(0)) then
          begin
            SetLength(data.WeaponData.WeaponList, Length(data.WeaponData.WeaponList) + 1);
            h := High(data.WeaponData.WeaponList);
            data.WeaponData.WeaponList[h].HashCode := hash1;
            data.WeaponData.WeaponList[h].TintIndex := GET_PED_WEAPON_TINT_INDEX(actor, hash1);
            data.WeaponData.WeaponList[h].Ammo := GET_AMMO_IN_PED_WEAPON(actor, hash1);
            SetLength(data.WeaponData.WeaponList[h].Components, 0);
            for j := 0 to WeaponCompModels.Count - 1 do
                begin
                  hash2 := GET_HASH_KEY(PChar(WeaponCompModels[j]));
                  if (HAS_PED_GOT_WEAPON_COMPONENT(actor, hash1, hash2) <> BOOL(0)) then
                    begin
                      SetLength(data.WeaponData.WeaponList[h].Components, Length(data.WeaponData.WeaponList[h].Components) + 1);
                      k := High(data.WeaponData.WeaponList[h].Components);
                      data.WeaponData.WeaponList[h].Components[k] := hash2;
                    end;
                end;
          end;
      end;
  // Personal vehicle
  veh := GET_VEHICLE_PED_IS_USING(actor);
  data.IsDriving := (veh <> 0);
  if data.IsDriving then
    StoreVehicle(data.VehicleData, veh);
end;

procedure TMissionScript.RestorePlayer(data: TStoredPlayerData; actor: Ped);
var
  i, j: integer;
  vdata: TVehiclePersistenceData;
  veh: Vehicle;
begin
  // Basic data
  SET_ENTITY_COORDS(actor, data.X, data.Y, data.Z, BOOL(0), BOOL(0), BOOL(0), BOOL(0));
  SET_ENTITY_HEADING(actor, data.A);
  SET_ENTITY_HEALTH(actor, data.Health, 0, 0);
  SET_PED_MAX_HEALTH(actor, data.MaxHealth);
  SET_PED_ARMOUR(actor, data.Armor);
  // Model and outfit
  if (actor = GET_PLAYER_PED(GET_PLAYER_INDEX)) then
    begin
      // Can only change the Player's model, but not of Peds'
      while (HAS_MODEL_LOADED(data.model) = BOOL(0)) do
            begin
              REQUEST_MODEL(data.Model);
              GameScreen.DrawLoadingScreen;
              ScriptHookVWait(0);
            end;
      SET_PLAYER_MODEL(GET_PLAYER_INDEX, data.Model);
      actor := GET_PLAYER_PED(GET_PLAYER_INDEX); // Changing Player model invalidates the original Ped!
    end;
  if (GET_ENTITY_MODEL(actor) = data.Model) then
    begin
      // Should only try to apply outfit, if the current Ped model is the same as the stored Ped model
      for i := 0 to 11 do
          if (i <> 7) then // Drawable 7 is unused!
            SET_PED_COMPONENT_VARIATION(actor, cint(i), data.OutfitData.Drawables[i].DrawableVar, data.OutfitData.Drawables[i].TextureVar, data.OutfitData.Drawables[i].PaletteVar);
      for i := 0 to 2 do
          SET_PED_PROP_INDEX(actor, cint(i), data.OutfitData.Props[i].PropIndex, data.OutfitData.Props[i].PropTexture, BOOL(0), 1);
    end;
  // Weapons
  REMOVE_ALL_PED_WEAPONS(actor, BOOL(0));
  for i := 0 to High(data.WeaponData.WeaponList) do
      begin
        while (HAS_WEAPON_ASSET_LOADED(data.WeaponData.WeaponList[i].HashCode) = BOOL(0)) do
              begin
                REQUEST_WEAPON_ASSET(data.WeaponData.WeaponList[i].HashCode, 31, 0);
                GameScreen.DrawLoadingScreen;
                ScriptHookVWait(0);
              end;
        GIVE_WEAPON_TO_PED(actor, data.WeaponData.WeaponList[i].HashCode, data.WeaponData.WeaponList[i].Ammo, BOOL(0), BOOL(0));
        SET_PED_WEAPON_TINT_INDEX(actor, data.WeaponData.WeaponList[i].HashCode, data.WeaponData.WeaponList[i].TintIndex);
        for j := 0 to High(data.WeaponData.WeaponList[i].Components) do
            GIVE_WEAPON_COMPONENT_TO_PED(actor, data.WeaponData.WeaponList[i].HashCode, data.WeaponData.WeaponList[i].Components[j]);
        REMOVE_WEAPON_ASSET(data.WeaponData.WeaponList[i].HashCode);
      end;
  // Personal vehicle
  if data.IsDriving then
    begin
      // We spawn a vehicle at the center of the gameworld, and put the player into it, then put the vehicle at the proper location
      ZeroMemory(@vdata, sizeof(TVehiclePersistenceData));
      CopyMemory(@vdata.VehicleData, @data.VehicleData, sizeof(TStoredVehicleData));
      veh := RestoreVehicle(vdata);
      SET_PED_INTO_VEHICLE(actor, veh, -1);
      SET_ENTITY_COORDS(veh, data.X, data.Y, data.Z, BOOL(0), BOOL(0), BOOL(0), BOOL(1));
      SET_ENTITY_HEADING(veh, data.A);
      SET_VEHICLE_AS_NO_LONGER_NEEDED(@veh);
    end;
end;

procedure TMissionScript.LoadPlayerData(var data: TStoredPlayerData; stream: TStream; savMajorVer, savMinorVer: integer);
var
  u32: UINT32;
  i, j: integer;
begin
  // Basic data
  u32 := stream.ReadDWord;
  data.X := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  data.Y := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  data.Z := pcfloat(@u32)^;
  u32 := stream.ReadDWord;
  data.A := pcfloat(@u32)^;
  data.Health := stream.ReadDWord;
  data.MaxHealth := stream.ReadDWord;
  data.Armor := stream.ReadDWord;
  // Model and outfit
  data.Model := stream.ReadDWord;
  for i := 0 to 11 do
      begin
        data.OutfitData.Drawables[i].DrawableVar := stream.ReadDWord;
        data.OutfitData.Drawables[i].TextureVar := stream.ReadDWord;
        data.OutfitData.Drawables[i].PaletteVar := stream.ReadDWord;
      end;
  for i := 0 to 2 do
      begin
        data.OutfitData.Props[i].PropIndex := stream.ReadDWord;
        data.OutfitData.Props[i].PropTexture := stream.ReadDWord;
      end;
  // Weapons
  SetLength(data.WeaponData.WeaponList, integer(stream.ReadDWord));
  for i := 0 to High(data.WeaponData.WeaponList) do
      begin
        data.WeaponData.WeaponList[i].HashCode := stream.ReadDWord;
        data.WeaponData.WeaponList[i].TintIndex := stream.ReadDWord;
        data.WeaponData.WeaponList[i].Ammo := stream.ReadDWord;
        SetLength(data.WeaponData.WeaponList[i].Components, integer(stream.ReadDWord));
        for j := 0 to High(data.WeaponData.WeaponList[i].Components) do
            data.WeaponData.WeaponList[i].Components[j] := stream.ReadDWord;
      end;
  // Personal vehicle
  data.IsDriving := (stream.ReadByte <> 0);
  if data.IsDriving then
    LoadVehicleData(data.VehicleData, stream, savMajorVer, savMinorVer);
end;

procedure TMissionScript.SavePlayerData(data: TStoredPlayerData; stream: TStream);
var
  i, j: integer;
begin
  // Basic data
  stream.WriteDWord(PUINT32(@data.X)^);
  stream.WriteDWord(PUINT32(@data.Y)^);
  stream.WriteDWord(PUINT32(@data.Z)^);
  stream.WriteDWord(PUINT32(@data.A)^);
  stream.WriteDWord(data.Health);
  stream.WriteDWord(data.MaxHealth);
  stream.WriteDWord(data.Armor);
  // Model and outfit
  stream.WriteDWord(data.Model);
  for i := 0 to 11 do
      begin
        stream.WriteDWord(data.OutfitData.Drawables[i].DrawableVar);
        stream.WriteDWord(data.OutfitData.Drawables[i].TextureVar);
        stream.WriteDWord(data.OutfitData.Drawables[i].PaletteVar);
      end;
  for i := 0 to 2 do
      begin
        stream.WriteDWord(data.OutfitData.Props[i].PropIndex);
        stream.WriteDWord(data.OutfitData.Props[i].PropTexture);
      end;
  // Weapons
  stream.WriteDWord(cint(Length(data.WeaponData.WeaponList)));
  for i := 0 to High(data.WeaponData.WeaponList) do
      begin
        stream.WriteDWord(data.WeaponData.WeaponList[i].HashCode);
        stream.WriteDWord(data.WeaponData.WeaponList[i].TintIndex);
        stream.WriteDWord(data.WeaponData.WeaponList[i].Ammo);
        stream.WriteDWord(cint(Length(data.WeaponData.WeaponList[i].Components)));
        for j := 0 to High(data.WeaponData.WeaponList[i].Components) do
            stream.WriteDWord(data.WeaponData.WeaponList[i].Components[j]);
      end;
  // Personal vehicle
  if data.IsDriving then
    begin
      stream.WriteByte(1);
      SaveVehicleData(data.VehicleData, stream);
    end
  else
    stream.WriteByte(0);
end;

procedure TMissionScript.StoreVehicle(var data: TStoredVehicleData; veh: Vehicle);
var
  i: integer;
begin
  ZeroMemory(@data, sizeof(TStoredVehicleData));
  data.Model := GET_ENTITY_MODEL(veh);
  GET_VEHICLE_COLOURS(veh, @data.PriColor, @data.SecColor);
  data.CustColorPri.Enabled := (GET_IS_VEHICLE_PRIMARY_COLOUR_CUSTOM(veh) <> BOOL(0));
  if data.CustColorPri.Enabled then
    GET_VEHICLE_CUSTOM_PRIMARY_COLOUR(veh, @data.CustColorPri.R, @data.CustColorPri.G, @data.CustColorPri.B);
  data.CustColorSec.Enabled := (GET_IS_VEHICLE_SECONDARY_COLOUR_CUSTOM(veh) <> BOOL(0));
  if data.CustColorSec.Enabled then
    GET_VEHICLE_CUSTOM_SECONDARY_COLOUR(veh, @data.CustColorSec.R, @data.CustColorSec.G, @data.CustColorSec.B);
  data.WheelType := GET_VEHICLE_WHEEL_TYPE(veh);
  for i := Low(data.Mods) to High(data.Mods) do
      begin
        if (i in [17..22]) then
          begin
            if (IS_TOGGLE_MOD_ON(veh, cint(i)) <> BOOL(0)) then
              data.Mods[i].ModIndex := 1
            else
              data.Mods[i].ModIndex := 0;
          end
        else
          begin
            data.Mods[i].ModIndex := GET_VEHICLE_MOD(veh, cint(i));
            data.Mods[i].ModVariation := (GET_VEHICLE_MOD_VARIATION(veh, cint(i)) <> 0);
          end;
      end;
  data.BulletProofTires := (GET_VEHICLE_TYRES_CAN_BURST(veh) <> BOOL(0));
  GET_VEHICLE_TYRE_SMOKE_COLOR(veh, @data.TyreSmokeColor.R, @data.TyreSmokeColor.G, @data.TyreSmokeColor.B);
  data.NumberPlateIndex := GET_VEHICLE_NUMBER_PLATE_TEXT_INDEX(veh);
  data.NumberPlateText := strpas(GET_VEHICLE_NUMBER_PLATE_TEXT(veh));
  GET_VEHICLE_EXTRA_COLOURS(veh, @data.PearlColor, @data.RimColor);
  if (IS_VEHICLE_A_CONVERTIBLE(veh, BOOL(0)) <> BOOL(0)) then
    data.RoofState := (GET_CONVERTIBLE_ROOF_STATE(veh) in [0, 1])
  else
    data.RoofState := false;
  for i := Low(data.Extras) to High(data.Extras) do
      begin
        if (DOES_EXTRA_EXIST(veh, cint(i)) <> BOOL(0)) then
          data.Extras[i] := (IS_VEHICLE_EXTRA_TURNED_ON(veh, cint(i)) <> BOOL(0))
        else
          data.Extras[i] := false;
      end;
  data.Livery := GET_VEHICLE_LIVERY(veh);
  GET_VEHICLE_NEON_COLOUR(veh, @data.NeonColor.R, @data.NeonColor.G, @data.NeonColor.B);
  data.WindowTint := GET_VEHICLE_WINDOW_TINT(veh);
  GET_VEHICLE_EXTRA_COLOUR_5(veh, @data.InteriorColor);
  GET_VEHICLE_EXTRA_COLOUR_6(veh, @data.DashboardColor);
  data.XenonColor := GET_VEHICLE_XENON_LIGHT_COLOR_INDEX(veh);
end;

function TMissionScript.RestoreVehicle(data: TVehiclePersistenceData): Vehicle;
var
  i: integer;
begin
  // Load model
  while (HAS_MODEL_LOADED(data.VehicleData.Model) = BOOL(0)) do
        begin
          REQUEST_MODEL(data.VehicleData.Model);
          GameScreen.DrawLoadingScreen;
          ScriptHookVWait(0);
        end;

  // Spawn car
  Result := CREATE_VEHICLE(data.VehicleData.Model, data.X, data.Y, data.Z, data.A, BOOL(0), BOOL(0), BOOL(0));

  // Setup car
  SET_VEHICLE_COLOURS(Result, data.VehicleData.PriColor, data.VehicleData.SecColor);
  if data.VehicleData.CustColorPri.Enabled then
    SET_VEHICLE_CUSTOM_PRIMARY_COLOUR(Result, data.VehicleData.CustColorPri.R, data.VehicleData.CustColorPri.G, data.VehicleData.CustColorPri.B);
  if data.VehicleData.CustColorSec.Enabled then
    SET_VEHICLE_CUSTOM_SECONDARY_COLOUR(Result, data.VehicleData.CustColorSec.R, data.VehicleData.CustColorSec.G, data.VehicleData.CustColorSec.B);
  SET_VEHICLE_MOD_KIT(Result, 0);
  SET_VEHICLE_WHEEL_TYPE(Result, data.VehicleData.WheelType);
  for i := Low(data.VehicleData.Mods) to High(data.VehicleData.Mods) do
      begin
        if (i in [17..22]) then
          begin
            if (data.VehicleData.Mods[i].ModIndex <> 0) then
              TOGGLE_VEHICLE_MOD(Result, cint(i), BOOL(1));
          end
        else
          begin
            if data.VehicleData.Mods[i].ModVariation then
              SET_VEHICLE_MOD(Result, cint(i), data.VehicleData.Mods[i].ModIndex, BOOL(1))
            else
              SET_VEHICLE_MOD(Result, cint(i), data.VehicleData.Mods[i].ModIndex, BOOL(0));
          end;
      end;
  if data.VehicleData.BulletProofTires then
    SET_VEHICLE_TYRES_CAN_BURST(Result, BOOL(0))
  else
    SET_VEHICLE_TYRES_CAN_BURST(Result, BOOL(1));
  SET_VEHICLE_TYRE_SMOKE_COLOR(Result, data.VehicleData.TyreSmokeColor.R, data.VehicleData.TyreSmokeColor.G, data.VehicleData.TyreSmokeColor.B);
  SET_VEHICLE_NUMBER_PLATE_TEXT_INDEX(Result, data.VehicleData.NumberPlateIndex);
  SET_VEHICLE_NUMBER_PLATE_TEXT(Result, PChar(data.VehicleData.NumberPlateText));
  SET_VEHICLE_EXTRA_COLOURS(Result, data.VehicleData.PearlColor, data.VehicleData.RimColor);
  if (IS_VEHICLE_A_CONVERTIBLE(Result, BOOL(0)) <> BOOL(0)) then
    begin
      if data.VehicleData.RoofState then
        RAISE_CONVERTIBLE_ROOF(Result, BOOL(1))
      else
        LOWER_CONVERTIBLE_ROOF(Result, BOOL(1));
    end;
  for i := Low(data.VehicleData.Extras) to High(data.VehicleData.Extras) do
      if data.VehicleData.Extras[i] then
        SET_VEHICLE_EXTRA(Result, cint(i), BOOL(0));
  SET_VEHICLE_LIVERY(Result, data.VehicleData.Livery);
  SET_VEHICLE_NEON_COLOUR(Result, data.VehicleData.NeonColor.R, data.VehicleData.NeonColor.G, data.VehicleData.NeonColor.B);
  SET_VEHICLE_WINDOW_TINT(Result, data.VehicleData.WindowTint);
  SET_VEHICLE_EXTRA_COLOUR_5(Result, data.VehicleData.InteriorColor);
  SET_VEHICLE_EXTRA_COLOUR_6(Result, data.VehicleData.DashboardColor);
  SET_VEHICLE_XENON_LIGHT_COLOR_INDEX(Result, data.VehicleData.XenonColor);

  // Unload model
  SET_MODEL_AS_NO_LONGER_NEEDED(data.VehicleData.Model);
end;

procedure TMissionScript.LoadVehicleData(var data: TStoredVehicleData; stream: TStream; savMajorVer, savMinorVer: integer);
var
  i: integer;
begin
  if (savMajorVer > 2) or (savMinorVer > 1) then // 2.0, 2.1 compatibility
    begin
      data.Model := stream.ReadDWord;
      data.PriColor := stream.ReadDWord;
      data.SecColor := stream.ReadDWord;
      data.CustColorPri.Enabled := (stream.ReadByte <> 0);
      if data.CustColorPri.Enabled then
        begin
          data.CustColorPri.R := stream.ReadDWord;
          data.CustColorPri.G := stream.ReadDWord;
          data.CustColorPri.B := stream.ReadDWord;
        end;
      data.CustColorSec.Enabled := (stream.ReadByte <> 0);
      if data.CustColorSec.Enabled then
        begin
          data.CustColorSec.R := stream.ReadDWord;
          data.CustColorSec.G := stream.ReadDWord;
          data.CustColorSec.B := stream.ReadDWord;
        end;
      data.WheelType := stream.ReadDWord;
      for i := Low(data.Mods) to High(data.Mods) do
          begin
            data.Mods[i].ModIndex := stream.ReadDWord;
            data.Mods[i].ModVariation := (stream.ReadByte <> 0);
          end;
      data.BulletProofTires := (stream.ReadByte <> 0);
      data.TyreSmokeColor.R := stream.ReadDWord;
      data.TyreSmokeColor.G := stream.ReadDWord;
      data.TyreSmokeColor.B := stream.ReadDWord;
      data.NumberPlateIndex := stream.ReadDWord;
      data.NumberPlateText := ReadCString(stream);
      data.PearlColor := stream.ReadDWord;
      data.RimColor := stream.ReadDWord;
      data.RoofState := (stream.ReadByte <> 0);
      for i := Low(data.Extras) to High(data.Extras) do
          data.Extras[i] := (stream.ReadByte <> 0);
      data.Livery := stream.ReadDWord;
      data.NeonColor.R := stream.ReadDWord;
      data.NeonColor.G := stream.ReadDWord;
      data.NeonColor.B := stream.ReadDWord;
      data.WindowTint := stream.ReadDWord;
      data.InteriorColor := stream.ReadDWord;
      data.DashboardColor := stream.ReadDWord;
      data.XenonColor := stream.ReadDWord;
    end
  else
    begin
      ZeroMemory(@data, sizeof(TStoredVehicleData));

      // Readable data (legacy)
      data.Model := stream.ReadDWord;
      data.PriColor := stream.ReadDWord;
      data.SecColor := stream.ReadDWord;
      stream.Seek(2 * sizeof(cint), soFromCurrent); // Skip unused legacy property: ColorCombination, ModKit
      data.WheelType := stream.ReadDWord;
      data.WindowTint := stream.ReadDWord;
      data.Livery := stream.ReadDWord;
      data.TyreSmokeColor.R := stream.ReadDWord;
      data.TyreSmokeColor.G := stream.ReadDWord;
      data.TyreSmokeColor.B := stream.ReadDWord;
      for i := Low(data.Mods) to High(data.Mods) do
          data.Mods[i].ModIndex := stream.ReadDWord;
      stream.Seek(5 * sizeof(cint), soFromCurrent); // Skip unused legacy property: ModColor1, ModColor2
      data.PearlColor := stream.ReadDWord;
      data.RimColor := stream.ReadDWord;
      data.CustColorPri.R := stream.ReadDWord;
      data.CustColorPri.G := stream.ReadDWord;
      data.CustColorPri.B := stream.ReadDWord;
      data.CustColorSec.R := stream.ReadDWord;
      data.CustColorSec.G := stream.ReadDWord;
      data.CustColorSec.B := stream.ReadDWord;
      data.BulletProofTires := (stream.ReadByte <> 0);

      // Improvisation (non-zero data only)
      data.CustColorPri.Enabled := true;
      data.CustColorSec.Enabled := true;
      data.NumberPlateIndex := -1;
      data.NumberPlateText := '';
      data.RoofState := true;
      data.NeonColor.R := 255;
      data.NeonColor.G := 255;
      data.NeonColor.B := 255;
      data.XenonColor := -1;
    end;
end;

procedure TMissionScript.SaveVehicleData(data: TStoredVehicleData; stream: TStream); // Used, when the script data is saved into a savegame file
var
  i: integer;
begin
  stream.WriteDWord(data.Model);
  stream.WriteDWord(data.PriColor);
  stream.WriteDWord(data.SecColor);
  if data.CustColorPri.Enabled then
    begin
      stream.WriteByte(1);
      stream.WriteDWord(data.CustColorPri.R);
      stream.WriteDWord(data.CustColorPri.G);
      stream.WriteDWord(data.CustColorPri.B);
    end
  else
    stream.WriteByte(0);
  if data.CustColorSec.Enabled then
    begin
      stream.WriteByte(1);
      stream.WriteDWord(data.CustColorSec.R);
      stream.WriteDWord(data.CustColorSec.G);
      stream.WriteDWord(data.CustColorSec.B);
    end
  else
    stream.WriteByte(0);
  stream.WriteDWord(data.WheelType);
  for i := Low(data.Mods) to High(data.Mods) do
      begin
        stream.WriteDWord(data.Mods[i].ModIndex);
        if data.Mods[i].ModVariation then
          stream.WriteByte(1)
        else
          stream.WriteByte(0);
      end;
  if data.BulletProofTires then
    stream.WriteByte(1)
  else
    stream.WriteByte(0);
  stream.WriteDWord(data.TyreSmokeColor.R);
  stream.WriteDWord(data.TyreSmokeColor.G);
  stream.WriteDWord(data.TyreSmokeColor.B);
  stream.WriteDWord(data.NumberPlateIndex);
  WriteCString(data.NumberPlateText, stream);
  stream.WriteDWord(data.PearlColor);
  stream.WriteDWord(data.RimColor);
  if data.RoofState then
    stream.WriteByte(1)
  else
    stream.WriteByte(0);
  for i := Low(data.Extras) to High(data.Extras) do
      if data.Extras[i] then
        stream.WriteByte(1)
      else
        stream.WriteByte(0);
  stream.WriteDWord(data.Livery);
  stream.WriteDWord(data.NeonColor.R);
  stream.WriteDWord(data.NeonColor.G);
  stream.WriteDWord(data.NeonColor.B);
  stream.WriteDWord(data.WindowTint);
  stream.WriteDWord(data.InteriorColor);
  stream.WriteDWord(data.DashboardColor);
  stream.WriteDWord(data.XenonColor);
end;

constructor TMissionScript.Create(scriptfile: string);
var
  i: integer;
begin
  inherited Create;
  bytes := TMemoryStream.Create;
  if FileExists(scriptfile) then
    begin
      bytes.LoadFromFile(scriptfile);
      bytes.Position := 0;
    end;
  lastPickupCheckTime := 0;
  lastPickupCheckTimeDiff := 0;
  SetLength(threads, 0);
  enabledMaps := TStringList.Create;
  disabledMaps := TStringList.Create;
  SetLength(modelSwapRecords, 0);
  SetLength(modelHideRecords, 0);
  for i := 0 to High(storedPlayerData) do
      SetLength(storedPlayerData[i].Data.WeaponData.WeaponList, 0);

  // Initial var table setup...
  for i := 0 to High(globalVars) do
      begin
        globalVars[i].VarType := svtNone;
        globalVars[i].Value := 0;
      end;
   for i := 0 to High(globalStrings) do
       globalStrings[i].Used  := false;
   for i := 0 to High(globalVectors) do
       globalVectors[i].Used := false;
    for i := 0 to High(globalArrays) do
       globalArrays[i].Used := false;
    for i := 0 to High(globalStringLists) do
       globalStringLists[i].Used := false;
    for i := 0 to High(globalBlips) do
       globalBlips[i].Used := false;
    for i := 0 to High(globalPickups) do
       globalPickups[i].Used := false;
    for i := 0 to High(globalSavedObjects) do
       globalSavedObjects[i].Used := false;
    for i := 0 to High(globalForbiddenCubes) do
       globalForbiddenCubes[i].Used := false;
    for i := 0 to High(globalCarGenerators) do
       globalCarGenerators[i].Used := false;

  // Clear script
  Reset;
end;

destructor TMissionScript.Destroy;
begin
  Reset;
  enabledMaps.Free;
  disabledMaps.Free;
  SetLength(modelSwapRecords, 0);
  SetLength(modelHideRecords, 0);
  bytes.Free;
  inherited;
end;

{%region /fold 'Load / Save'}

procedure TMissionScript.Load(slot: integer); // Used, when the script data is loaded from a savegame file
var
  stream: TFileStream;
  u32, u32b, u32c: UINT32;
  hash1: Hash;
  i, j: integer;
  s: string;
  pdata: TStoredPlayerData;
  v_major, v_minor: DWORD;
begin
  // **** START **** //
  stream := TFileStream.Create(GetSaveFileName(slot), fmOpenRead);
  try
    GameScreen.DrawLoadingScreen;
    ScriptHookVWait(0);
    SET_GAME_PAUSED(BOOL(1));
    Reset;
    isStarted := true;

    // **** HEADER **** //
    v_major := stream.ReadDWord; // Savegame major version (for compatibility)
    v_minor := stream.ReadDWord; // Savegame minor version (for compatibility)
    isOnMission := (stream.ReadByte <> 0); // Actually, it's discouraged to save while on a mission, but we must be prepared in advance...
    u32 := stream.ReadDWord;
    floatEqualThreshold := pcfloat(@u32)^;
    SetLength(threads, integer(stream.ReadDWord));
    lastMission := ReadCString(stream);

    // **** THREADS **** //
    for i := 0 to High(threads) do
        threads[i] := TMissionThread.Create(self, stream, v_major, v_minor);

    // **** GLOBAL VARS **** //
    for i := 0 to High(globalVars) do
        begin
          globalVars[i].VarType := TVariableType(stream.ReadByte);
          globalVars[i].Value := stream.ReadDWord;
        end;

    // **** COMPLEX VAR TABLES **** //
    for i := 0 to High(globalStrings) do
        begin
          globalStrings[i].Used := (stream.ReadByte <> 0);
          if globalStrings[i].Used then
            globalStrings[i].Value := ReadCString(stream);
        end;
    for i := 0 to High(globalVectors) do
        begin
          globalVectors[i].Used := (stream.ReadByte <> 0);
          if globalVectors[i].Used then
            begin
              u32 := stream.ReadDWord;
              globalVectors[i].Value.x := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalVectors[i].Value.y := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalVectors[i].Value.z := pcfloat(@u32)^;
            end;
        end;
    for i := 0 to High(globalArrays) do
        begin
          globalArrays[i].Used := (stream.ReadByte <> 0);
          if globalArrays[i].Used then
            begin
              u32 := stream.ReadDWord;
              GTA5_SetArrayLength(globalArrays[i].Value, integer(u32));
              for j := 0 to GTA5_GetArrayLength(globalArrays[i].Value) - 1 do
                  GTA5_SetArrayItem(globalArrays[i].Value, j, stream.ReadDWord);
            end;
        end;
    for i := 0 to High(globalStringLists) do
        begin
          globalStringLists[i].Used := (stream.ReadByte <> 0);
          if globalStringLists[i].Used then
            begin
              SetLength(globalStringLists[i].Value, integer(stream.ReadDWord));
              for j := 0 to High(globalStringLists[i].Value) do
                  globalStringLists[i].Value[j] := ReadCString(stream);
            end;
        end;
    for i := 0 to High(globalBlips) do
        begin
          globalBlips[i].Used := (stream.ReadByte <> 0);
          if globalBlips[i].Used then
            begin
              globalBlips[i].IsMission := (stream.ReadByte <> 0);
              globalBlips[i].IsShortRange := (stream.ReadByte <> 0);
              globalBlips[i].Selectable := (stream.ReadByte <> 0);
              globalBlips[i].HasRadius := (stream.ReadByte <> 0);
              u32 := stream.ReadDWord;
              globalBlips[i].X := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalBlips[i].Y := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalBlips[i].Z := pcfloat(@u32)^;
              if globalBlips[i].HasRadius then
                begin
                  globalBlips[i].Alpha := stream.ReadDWord;
                  u32 := stream.ReadDWord;
                  globalBlips[i].Radius := pcfloat(@u32)^;
                end
              else
                globalBlips[i].Sprite := stream.ReadDWord;
              globalBlips[i].Color := stream.ReadDWord;
              CreateManagedBlip(globalBlips[i]);
            end;
        end;
    for i := 0 to High(globalPickups) do
        begin
          globalPickups[i].Used := (stream.ReadByte <> 0);
          if globalPickups[i].Used then
            begin
              globalPickups[i].PickedUp := (stream.ReadByte <> 0);
              globalPickups[i].HasRotation := (stream.ReadByte <> 0);
              u32 := stream.ReadDWord;
              globalPickups[i].X := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalPickups[i].Y := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              globalPickups[i].Z := pcfloat(@u32)^;
              if globalPickups[i].HasRotation then
                begin
                  u32 := stream.ReadDWord;
                  globalPickups[i].XA := pcfloat(@u32)^;
                  u32 := stream.ReadDWord;
                  globalPickups[i].YA := pcfloat(@u32)^;
                  u32 := stream.ReadDWord;
                  globalPickups[i].ZA := pcfloat(@u32)^;
                end;
              globalPickups[i].PickupHash := stream.ReadDWord;
              globalPickups[i].ModelHash := stream.ReadDWord;
              globalPickups[i].Value := stream.ReadDWord;
              globalPickups[i].RgTime := stream.ReadDWord;
              globalPickups[i].RgTimeRem := stream.ReadDWord;
              if not globalPickups[i].PickedUp then
                CreateManagedPickup(globalPickups[i])
              else
                globalPickups[i]._handle := 0;
            end;
        end;
    for i := 0 to High(globalSavedObjects) do
        begin
          globalSavedObjects[i].Used := (stream.ReadByte <> 0);
          if globalSavedObjects[i].Used then
            RestoreManagedGameObject(globalSavedObjects[i], stream, v_major, v_minor);
        end;
    for i := 0 to High(globalForbiddenCubes) do
        begin
          globalForbiddenCubes[i].Used := (stream.ReadByte <> 0);
          if globalForbiddenCubes[i].Used then
            RestoreForbiddenCube(globalForbiddenCubes[i], stream, v_major, v_minor);
        end;
    for i := 0 to High(globalCarGenerators) do
        begin
          globalCarGenerators[i].Used := (stream.ReadByte <> 0);
          if globalCarGenerators[i].Used then
            RestoreCarGenerator(globalCarGenerators[i], stream, v_major, v_minor);
        end;

    // **** WORLD PERSISTENCE **** //
    u32 := stream.ReadDWord;
    u32b := stream.ReadDWord;
    SET_CLOCK_TIME(u32, u32b, 0);
    u32 := stream.ReadDWord;
    u32b := stream.ReadDWord;
    u32c := stream.ReadDWord;
    SET_CLOCK_DATE(u32, u32b, u32c);
    u32 := stream.ReadDWord;
    SET_CURR_WEATHER_STATE(u32, u32, 1.0);
    CLEAR_WEATHER_TYPE_PERSIST;
    useOverrideRestart := (stream.ReadByte <> 0);
    if useOverrideRestart then
      begin
        u32 := stream.ReadDWord;
        overrideRestart.X := pcfloat(@u32)^;
        u32 := stream.ReadDWord;
        overrideRestart.Y := pcfloat(@u32)^;
        u32 := stream.ReadDWord;
        overrideRestart.Z := pcfloat(@u32)^;
        u32 := stream.ReadDWord;
        overrideRestart.A := pcfloat(@u32)^;
      end;
    SetLength(hospitalRestarts, integer(stream.ReadDWord));
    for i := 0 to High(hospitalRestarts) do
        begin
          u32 := stream.ReadDWord;
          hospitalRestarts[i].X := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          hospitalRestarts[i].Y := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          hospitalRestarts[i].Z := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          hospitalRestarts[i].A := pcfloat(@u32)^;
        end;
    SetLength(policeRestarts, integer(stream.ReadDWord));
    for i := 0 to High(policeRestarts) do
        begin
          u32 := stream.ReadDWord;
          policeRestarts[i].X := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          policeRestarts[i].Y := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          policeRestarts[i].Z := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          policeRestarts[i].A := pcfloat(@u32)^;
        end;

    j := integer(stream.ReadDWord);
    for i := 0 to j - 1 do
        begin
          s := ReadCString(stream);
          enabledMaps.Add(s);
          while (IS_IPL_ACTIVE(PChar(s)) = BOOL(0)) do
                begin
                  REQUEST_IPL(PChar(s));
                  GameScreen.DrawLoadingScreen;
                  ScriptHookVWait(0);
                end;
        end;
    j := integer(stream.ReadDWord);
    for i := 0 to j - 1 do
        begin
          s := ReadCString(stream);
          disabledMaps.Add(s);
          REMOVE_IPL(PChar(s));
        end;

    if (v_major > 2) or (v_minor <> 0) then  // 2.0 compatibility
      begin
        SetLength(modelSwapRecords, integer(stream.ReadDWord));
        for i := 0 to High(modelSwapRecords) do
            begin
              modelSwapRecords[i].OrigHash := stream.ReadDWord;
              modelSwapRecords[i].NewHash := stream.ReadDWord;
              u32 := stream.ReadDWord;
              modelSwapRecords[i].X := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelSwapRecords[i].Y := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelSwapRecords[i].Z := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelSwapRecords[i].Radius := pcfloat(@u32)^;
              CREATE_MODEL_SWAP(modelSwapRecords[i].X, modelSwapRecords[i].Y, modelSwapRecords[i].Z, modelSwapRecords[i].Radius, modelSwapRecords[i].OrigHash, modelSwapRecords[i].NewHash, BOOL(1));
            end;
        SetLength(modelHideRecords, integer(stream.ReadDWord));
        for i := 0 to High(modelHideRecords) do
            begin
              modelHideRecords[i].ModelHash := stream.ReadDWord;
              u32 := stream.ReadDWord;
              modelHideRecords[i].X := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelHideRecords[i].Y := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelHideRecords[i].Z := pcfloat(@u32)^;
              u32 := stream.ReadDWord;
              modelHideRecords[i].Radius := pcfloat(@u32)^;
              CREATE_MODEL_HIDE(modelHideRecords[i].X, modelHideRecords[i].Y, modelHideRecords[i].Z, modelHideRecords[i].Radius, modelHideRecords[i].ModelHash, BOOL(1));
            end;
      end
    else
      begin
        SetLength(modelSwapRecords, 0);
        SetLength(modelHideRecords, 0);
      end;

    // **** PLAYER PERSISTENCE **** //
    ZeroMemory(@pdata, sizeof(TStoredPlayerData));
    LoadPlayerData(pdata, stream, v_major, v_minor);
    RestorePlayer(pdata, GET_PLAYER_PED(GET_PLAYER_INDEX));
    SET_MAX_WANTED_LEVEL(stream.ReadDWord);

    // **** MULIPLAYER PERSISTENCE (Player Switch Feature) **** //
    for i := 0 to High(storedPlayerData) do
        begin
          storedPlayerData[i].Used := (stream.ReadByte <> 0);
          if storedPlayerData[i].Used then
            LoadPlayerData(storedPlayerData[i].Data, stream, v_major, v_minor);
        end;


    // **** GARAGE PERSISTENCE (Stored Cars) **** //
    SetLength(storedVehicleData, integer(stream.ReadDWord));
    for i := 0 to High(storedVehicleData) do
        begin
          u32 := stream.ReadDWord;
          storedVehicleData[i].X := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          storedVehicleData[i].Y := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          storedVehicleData[i].Z := pcfloat(@u32)^;
          u32 := stream.ReadDWord;
          storedVehicleData[i].A := pcfloat(@u32)^;
          LoadVehicleData(storedVehicleData[i].VehicleData, stream, v_major, v_minor);
        end;

    if (v_major > 2) or (v_minor <> 0) then  // 2.0 compatibility
      begin
        for i := 0 to High(storedVehicleSlots)  do
            begin
              storedVehicleSlots[i].IsStored := (stream.ReadByte <> 0);
              if storedVehicleSlots[i].IsStored then
                LoadVehicleData(storedVehicleSlots[i].VehicleData, stream, v_major, v_minor);
            end;
      end
    else
      begin
        for i := 0 to High(storedVehicleSlots)  do
            storedVehicleSlots[i].IsStored := false;
      end;

    // **** STATISTICS **** //
    j := integer(stream.ReadDWord);
    for i := 0 to j - 1 do
        begin
          hash1 := stream.ReadDWord;
          u32 := stream.ReadDWord;
          STAT_SET_INT(hash1, u32, BOOL(1));
        end;
    j := integer(stream.ReadDWord);
    for i := 0 to j - 1 do
        begin
          hash1 := stream.ReadDWord;
          u32 := stream.ReadDWord;
          STAT_SET_FLOAT(hash1, pcfloat(@u32)^, BOOL(1));
        end;
  finally
    stream.Free;
    SET_GAME_PAUSED(BOOL(0));
    ScriptHookVWait(0);
  end;
end;

procedure TMissionScript.Save(slot: integer);
var
  stream: TMemoryStream;
  pdata: TStoredPlayerData;
  i, j: integer;
  hash1: Hash;
  u32: UINT32;
  cf: cfloat;
begin
  // **** START **** //
  stream := TMemoryStream.Create;
  try
    // **** HEADER **** //
    stream.WriteDWord(SCRIPT_MAJOR_VERSION); // Major version
    stream.WriteDWord(SCRIPT_MINOR_VERSION); // Minor version
    if isOnMission then // Actually, it's discouraged to save while on a mission, but we must be prepared in advance...
      stream.WriteByte(1)
    else
      stream.WriteByte(0);
    stream.WriteDWord(PUINT32(@floatEqualThreshold)^);
    stream.WriteDWord(cint(Length(threads)));
    WriteCString(lastMission, stream);

    // **** THREADS **** //
    for i := 0 to High(threads) do
        threads[i].Save(stream);

    // **** GLOBAL VARS **** //
    for i := 0 to High(globalVars) do
        begin
          stream.WriteByte(ord(globalVars[i].VarType));
          stream.WriteDWord(globalVars[i].Value);
        end;

    // **** COMPLEX VAR TABLES **** //
    for i := 0 to High(globalStrings) do
        if globalStrings[i].Used then
          begin
            stream.WriteByte(1);
            WriteCString(globalStrings[i].Value, stream);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalVectors) do
        if globalVectors[i].Used then
          begin
            stream.WriteByte(1);
            stream.WriteDWord(PUINT32(@globalVectors[i].Value.x)^);
            stream.WriteDWord(PUINT32(@globalVectors[i].Value.y)^);
            stream.WriteDWord(PUINT32(@globalVectors[i].Value.z)^);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalArrays) do
        if globalArrays[i].Used then
          begin
            stream.WriteByte(1);
            stream.WriteDWord(cint(GTA5_GetArrayLength(globalArrays[i].Value)));
            for j := 0 to GTA5_GetArrayLength(globalArrays[i].Value) - 1 do
                stream.WriteDWord(GTA5_GetArrayItem(globalArrays[i].Value, j));
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalStringLists) do
        if globalStringLists[i].Used then
          begin
            stream.WriteByte(1);
            stream.WriteDWord(cint(Length(globalStringLists[i].Value)));
            for j := 0 to High(globalStringLists[i].Value) do
                WriteCString(globalStringLists[i].Value[j], stream);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalBlips) do
        if globalBlips[i].Used then
          begin
            stream.WriteByte(1);
            if globalBlips[i].IsMission then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            if globalBlips[i].IsShortRange then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            if globalBlips[i].Selectable then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            if globalBlips[i].HasRadius then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            stream.WriteDWord(PUINT32(@globalBlips[i].X)^);
            stream.WriteDWord(PUINT32(@globalBlips[i].Y)^);
            stream.WriteDWord(PUINT32(@globalBlips[i].Z)^);
            if globalBlips[i].HasRadius then
              begin
                stream.WriteDWord(globalBlips[i].Alpha);
                stream.WriteDWord(PUINT32(@globalBlips[i].Radius)^);
              end
            else
              stream.WriteDWord(globalBlips[i].Sprite);
            stream.WriteDWord(globalBlips[i].Color);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalPickups) do
        if globalPickups[i].Used then
          begin
            stream.WriteByte(1);
            if globalPickups[i].PickedUp then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            if globalPickups[i].HasRotation then
              stream.WriteByte(1)
            else
              stream.WriteByte(0);
            stream.WriteDWord(PUINT32(@globalPickups[i].X)^);
            stream.WriteDWord(PUINT32(@globalPickups[i].Y)^);
            stream.WriteDWord(PUINT32(@globalPickups[i].Z)^);
            if globalPickups[i].HasRotation then
              begin
                stream.WriteDWord(PUINT32(@globalPickups[i].XA)^);
                stream.WriteDWord(PUINT32(@globalPickups[i].YA)^);
                stream.WriteDWord(PUINT32(@globalPickups[i].ZA)^);
              end;
            stream.WriteDWord(globalPickups[i].PickupHash);
            stream.WriteDWord(globalPickups[i].ModelHash);
            stream.WriteDWord(globalPickups[i].Value);
            stream.WriteDWord(globalPickups[i].RgTime);
            stream.WriteDWord(globalPickups[i].RgTimeRem);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalSavedObjects) do
        if globalSavedObjects[i].Used then
          begin
            stream.WriteByte(1);
            StoreManagedObject(globalSavedObjects[i], stream);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalForbiddenCubes) do
        if globalForbiddenCubes[i].Used then
          begin
            stream.WriteByte(1);
            StoreForbiddenCube(globalForbiddenCubes[i], stream);
          end
        else
          stream.WriteByte(0);
    for i := 0 to High(globalCarGenerators) do
        if globalCarGenerators[i].Used then
          begin
            stream.WriteByte(1);
            StoreCarGenerator(globalCarGenerators[i], stream);
          end
        else
          stream.WriteByte(0);

    // **** WORLD PERSISTENCE **** //
    stream.WriteDWord(GET_CLOCK_HOURS);
    stream.WriteDWord(GET_CLOCK_MINUTES);
    stream.WriteDWord(GET_CLOCK_DAY_OF_MONTH);
    stream.WriteDWord(GET_CLOCK_MONTH);
    stream.WriteDWord(GET_CLOCK_YEAR);
    stream.WriteDWord(GET_PREV_WEATHER_TYPE_HASH_NAME);
    if useOverrideRestart  then
      begin
        stream.WriteByte(1);
        stream.WriteDWord(PUINT32(@overrideRestart.X)^);
        stream.WriteDWord(PUINT32(@overrideRestart.Y)^);
        stream.WriteDWord(PUINT32(@overrideRestart.Z)^);
        stream.WriteDWord(PUINT32(@overrideRestart.A)^);
      end
    else
      stream.WriteByte(0);
    stream.WriteDWord(cint(Length(hospitalRestarts)));
    for i := 0 to High(hospitalRestarts) do
        begin
          stream.WriteDWord(PUINT32(@hospitalRestarts[i].X)^);
          stream.WriteDWord(PUINT32(@hospitalRestarts[i].Y)^);
          stream.WriteDWord(PUINT32(@hospitalRestarts[i].Z)^);
          stream.WriteDWord(PUINT32(@hospitalRestarts[i].A)^);
        end;
    stream.WriteDWord(cint(Length(policeRestarts)));
    for i := 0 to High(policeRestarts) do
        begin
          stream.WriteDWord(PUINT32(@policeRestarts[i].X)^);
          stream.WriteDWord(PUINT32(@policeRestarts[i].Y)^);
          stream.WriteDWord(PUINT32(@policeRestarts[i].Z)^);
          stream.WriteDWord(PUINT32(@policeRestarts[i].A)^);
        end;
    stream.WriteDWord(cint(enabledMaps.Count));
    for i := 0 to enabledMaps.Count - 1 do
        WriteCString(enabledMaps[i], stream);
    stream.WriteDWord(cint(disabledMaps.Count));
    for i := 0 to disabledMaps.Count - 1 do
        WriteCString(disabledMaps[i], stream);
    stream.WriteDWord(cint(Length(modelSwapRecords)));
    for i := 0 to High(modelSwapRecords) do
        begin
          stream.WriteDWord(modelSwapRecords[i].OrigHash);
          stream.WriteDWord(modelSwapRecords[i].NewHash);
          stream.WriteDWord(PUINT32(@modelSwapRecords[i].X)^);
          stream.WriteDWord(PUINT32(@modelSwapRecords[i].Y)^);
          stream.WriteDWord(PUINT32(@modelSwapRecords[i].Z)^);
          stream.WriteDWord(PUINT32(@modelSwapRecords[i].Radius)^);
        end;
    stream.WriteDWord(cint(Length(modelHideRecords)));
    for i := 0 to High(modelHideRecords) do
        begin
          stream.WriteDWord(modelHideRecords[i].ModelHash);
          stream.WriteDWord(PUINT32(@modelHideRecords[i].X)^);
          stream.WriteDWord(PUINT32(@modelHideRecords[i].Y)^);
          stream.WriteDWord(PUINT32(@modelHideRecords[i].Z)^);
          stream.WriteDWord(PUINT32(@modelHideRecords[i].Radius)^);
        end;

    // **** PLAYER PERSISTENCE **** //
    ZeroMemory(@pdata, sizeof(TStoredPlayerData));
    StorePlayer(pdata, GET_PLAYER_PED(GET_PLAYER_INDEX));
    SavePlayerData(pdata, stream);
    stream.WriteDWord(GET_MAX_WANTED_LEVEL);

    // **** MULIPLAYER PERSISTENCE (Player Switch Feature) **** //
    for i := 0 to High(storedPlayerData) do
        if storedPlayerData[i].Used then
          begin
            stream.WriteByte(1);
            SavePlayerData(storedPlayerData[i].Data, stream);
          end
        else
          stream.WriteByte(0);

    // **** GARAGE PERSISTENCE (Stored Cars) **** //
    stream.WriteDWord(cint(Length(storedVehicleData)));
    for i := 0 to High(storedVehicleData) do
        begin
          stream.WriteDWord(PUINT32(@storedVehicleData[i].X)^);
          stream.WriteDWord(PUINT32(@storedVehicleData[i].Y)^);
          stream.WriteDWord(PUINT32(@storedVehicleData[i].Z)^);
          stream.WriteDWord(PUINT32(@storedVehicleData[i].A)^);
          SaveVehicleData(storedVehicleData[i].VehicleData, stream);
        end;
    for i := 0 to High(storedVehicleSlots) do
        begin
          if storedVehicleSlots[i].IsStored then
            begin
              stream.WriteByte(1);
              SaveVehicleData(storedVehicleSlots[i].VehicleData, stream);
            end
          else
            stream.WriteByte(0);
        end;

    // **** STATISTICS **** //
    stream.WriteDWord(cint(GetIntStatCount));
    for i := 0 to GetIntStatCount - 1 do
        begin
          hash1 := GET_HASH_KEY(PChar(GetIntStatName(i)));
          STAT_GET_INT(hash1, @u32, -1);
          stream.WriteDWord(hash1);
          stream.WriteDWord(u32);
        end;
    stream.WriteDWord(cint(GetFloatStatCount));
    for i := 0 to GetFloatStatCount - 1 do
        begin
          hash1 := GET_HASH_KEY(PChar(GetFloatStatName(i)));
          STAT_GET_FLOAT(hash1, @cf, -1);
          stream.WriteDWord(hash1);
          stream.WriteDWord(PUINT32(@cf)^);
        end;

    // **** END **** //
    stream.SaveToFile(GetSaveFileName(slot));
  finally
    stream.Free;
  end;
end;

{%endregion}

procedure TMissionScript.Reset;
var
  i, j: integer;
begin
  // Threads
  isStarted := false;
  for i := 0 to High(threads) do
      threads[i].Free;
  SetLength(threads, 0);

  // Global vars
  for i := 0 to High(globalVars) do
      ClearVariable(globalVars[i]);

  // Restore Enabled / Disabled maps
  if not _is_dll_init_final_ then
    for i := 0 to enabledMaps.Count - 1 do
        REMOVE_IPL(PChar(enabledMaps[i]));
  enabledMaps.Clear;
  if not _is_dll_init_final_ then
    for i := 0 to disabledMaps.Count - 1 do
        begin
          while (IS_IPL_ACTIVE(PChar(disabledMaps[i])) = BOOL(0)) do
                begin
                  REQUEST_IPL(PChar(disabledMaps[i]));
                  GameScreen.DrawLoadingScreen;
                  ScriptHookVWait(0);
                end;
        end;
  disabledMaps.Clear;

  if not _is_dll_init_final_ then
    for i := 0 to High(modelSwapRecords) do
        REMOVE_MODEL_SWAP(modelSwapRecords[i].X, modelSwapRecords[i].Y, modelSwapRecords[i].Z, modelSwapRecords[i].Radius, modelSwapRecords[i].NewHash, modelSwapRecords[i].OrigHash, BOOL(0));
  SetLength(modelSwapRecords, 0);
  if not _is_dll_init_final_ then
    for i := 0 to High(modelHideRecords) do
        REMOVE_MODEL_HIDE(modelHideRecords[i].X, modelHideRecords[i].Y, modelHideRecords[i].Z, modelHideRecords[i].Radius, modelHideRecords[i].ModelHash, BOOL(0)); // NOTE: signature requires 'Any (int)', but params are floats
  SetLength(modelHideRecords, 0);

  // Un-store player persistence data
  for i := 0 to High(storedPlayerData) do
      begin
        storedPlayerData[i].Used := false;
        for j := 0 to High(storedPlayerData[i].Data.WeaponData.WeaponList) do
            SetLength(storedPlayerData[i].Data.WeaponData.WeaponList[j].Components, 0);
        SetLength(storedPlayerData[i].Data.WeaponData.WeaponList, 0);
      end;

  // Un-store vehicles
  SetLength(storedVehicleData, 0);
  for i := 0 to High(storedVehicleSlots) do
      storedVehicleSlots[i].IsStored := false;

  // Un-store restart locations
  SetLength(hospitalRestarts, 0);
  SetLength(policeRestarts, 0);
  useOverrideRestart := false;

  // Reset script flags
  isOnMission := false;
  lastMission := 'NONE';
  floatEqualThreshold := 0.0001;

  // Reset statistics
  if not _is_dll_init_final_ then
    begin
      for i := 0 to GetIntStatCount - 1 do
          STAT_SET_INT(GET_HASH_KEY(PChar(GetIntStatName(i))), GetIntStatDefaultValue(i), BOOL(1));
      for i := 0 to GetFloatStatCount - 1 do
          STAT_SET_FLOAT(GET_HASH_KEY(PChar(GetFloatStatName(i))), GetFloatStatDefaultValue(i), BOOL(1));
    end;
end;

procedure TMissionScript.Run(isPlayerDead: boolean);
var
  i, j: integer;
begin
  if IsAvailable then
    begin
      if (GameScreen.MenuMode = gmInGame) then
        begin
          // If not already started, we need to start the very first thread...
          if not isStarted then
            begin
              isStarted := true;
              CreateThread(0);
            end;
        end
      else
        lastPickupCheckTime := CurrentTimeMs;

      // Run threads
      for i := 0 to High(threads) do
          if (threads[i].ThreadStatus <> tstFinished) then
            threads[i].Run(isPlayerDead);

      // Cleanup finished threads
      for i := High(threads) downto 0 do
          if (threads[i].ThreadStatus = tstFinished) then
            begin
              threads[i].Free;
              for j := i to High(threads) - 1 do
                  threads[j] := threads[j + 1];
              SetLength(threads, Length(threads) - 1);
            end;

      // Manage objects
      ManageObjects;
    end;
end;

function TMissionScript.IsAvailable: boolean;
begin
  Result := (bytes.Size > 0) or isStarted;
end;

function TMissionScript.ThreadCount: integer;
var
  i: integer;
begin
  Result := 0;
  if (Length(threads) > 0) then
    for i := 0 to High(threads) do
        if (threads[i].ThreadStatus in [tstRunning, tstWaiting]) then
          inc(Result);
end;

function TMissionScript.FindNearestRespawnLocation(out location: TRespawnLocation; findHospital: boolean): boolean;
var
  i: integer;
  mindiff, diff, xd, yd, zd: cfloat;
  loc: Vector3;
begin
  mindiff := cfloat.MaxValue;
  diff := cfloat.MaxValue;
  if findHospital then
    begin
      Result := (Length(hospitalRestarts) > 0);
      if Result then
        begin
          loc := GET_ENTITY_COORDS(GET_PLAYER_PED(GET_PLAYER_INDEX), BOOL(0));
          for i := 0 to High(hospitalRestarts) do
              begin
                xd := loc.x - hospitalRestarts[i].X;
                yd := loc.y - hospitalRestarts[i].Y;
                zd := loc.z - hospitalRestarts[i].Z;
                diff := System.sqrt((xd * xd) + (yd * yd) + (zd * zd));
                if (diff < mindiff) then
                  begin
                    mindiff := diff;
                    location := hospitalRestarts[i];
                  end;
              end;
        end;
    end
  else
    begin
      Result := (Length(policeRestarts) > 0);
      if Result then
        begin
          loc := GET_ENTITY_COORDS(GET_PLAYER_PED(GET_PLAYER_INDEX), BOOL(0));
          for i := 0 to High(policeRestarts) do
              begin
                xd := loc.x - policeRestarts[i].X;
                yd := loc.y - policeRestarts[i].Y;
                zd := loc.z - policeRestarts[i].Z;
                diff := System.sqrt((xd * xd) + (yd * yd) + (zd * zd));
                if (diff < mindiff) then
                  begin
                    mindiff := diff;
                    location := policeRestarts[i];
                  end;
              end;
        end;
    end;
end;

function TMissionScript.GetInternalPlayerIndex: integer;
begin
  Result := GetActorInternalPlayerIndex(GET_PLAYER_PED(GET_PLAYER_INDEX));
end;

function TMissionScript.GetActorInternalPlayerIndex(actor: Ped): integer;
begin
  if (GET_ENTITY_MODEL(actor) = GET_HASH_KEY(PChar('player_zero'))) then
     Result := 0
  else if (GET_ENTITY_MODEL(actor) = GET_HASH_KEY(PChar('player_one'))) then
     Result := 1
  else if (GET_ENTITY_MODEL(actor) = GET_HASH_KEY(PChar('player_two'))) then
     Result := 2
  else
     Result := -1;
end;

function TMissionScript.IsManagedBlip(handle: Blip): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(globalBlips) do
      if globalBlips[i].Used and (globalBlips[i]._handle = handle) then
        exit(true);
end;

function TMissionScript.IsManagedPickup(handle: Pickup): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(globalPickups) do
      if globalPickups[i].Used and (globalPickups[i]._handle = handle) then
        exit(true);
end;

function TMissionScript.IsManagedObject(handle: GTAObject): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(globalSavedObjects) do
      if globalSavedObjects[i].Used and (globalSavedObjects[i]._handle = handle) then
        exit(true);
end;

function TMissionScript.IsManagedCarGenerator(handle: cint): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(globalCarGenerators) do
      if globalCarGenerators[i].Used and (globalCarGenerators[i]._handle = handle) then
        exit(true);
end;

{%endregion}

initialization
  _is_dll_init_final_ := true;

  QueryPerformanceFrequency(@pFreq);
  WeaponModels := ReadRawData('yos_data/data/WeaponsList.dat');
  WeaponCompModels := ReadRawData('yos_data/data/WeaponCompList.dat');
  MissionScript := TMissionScript.Create('yos_data/mission_script/main.yos');

  _is_dll_init_final_ := false;

finalization
  _is_dll_init_final_ := true;

  MissionScript.Free;
  WeaponModels.Free;
  WeaponCompModels.Free;

end.

