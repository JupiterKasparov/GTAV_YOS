unit YOS_Screen;

{$mode objfpc}{$H+}

interface

uses
  ScriptHookV, Natives, Windows, ctypes, SysUtils, Classes, INIFiles, Math, YOS_Utils;

type
  // Control data types
  TMenuType = (gmInGame, gmLoadGame, gmSaveGame, gmLoadingScreen);
  TMenuEventType = (evtNewGame, evtLoadGame, evtSaveGame, evtCancelMenu);
  TMenuKeyType = (mkNone, mkMouseClick, mkSelect, mkPrevItem, mkNextItem, mkExit);

  TGameScreen = class (TObject)
  private
    currentMenu: TMenuType;
    currentKey: TMenuKeyType;
    itemIndex: integer;
    radioStation: PChar;
    visible: boolean;
    savegameTitles: array [0..15] of string;
    buttonsMovie: cint; // Scaleform movie handle
    procedure LoadSavegameInfo;
    function QuerySavegameInfo(sgi: integer; out title: string): boolean;
    procedure ClearSavegameInfo;
    procedure InitScaleformButtons;
    procedure SetMenu(menu: TMenuType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DrawLoadingScreen;
    function ProcessMenu(out eventId: TMenuEventType; out sgId: integer;playerindex: integer; gameStarted: boolean): boolean; // Returns true, if a game is loaded, saved, new game started, menu cancelled. Event name and savegame id are returned in params!
    property MenuMode: TMenuType read currentMenu write SetMenu;
  end;

var
  GameScreen: TGameScreen;

procedure PushMapNotification(msg: string);
procedure DrawCustomTexture(fileName: string; sizeX, sizeY, posX, posY, ofsX, ofsY, rotation: cfloat; bCentered, bScaled: boolean; r, g, b, a: byte);
procedure ClearCustomTextures;

implementation

{%region /fold 'Menu Helpers'}

type
  TMenuItemType = (mitGameSlot, mitNewGame, mitNoNewGame, mitNoSave, mitTitle);
  TMenuColorType = (clrEnabledSelBg, clrDisabledSelBg, clrEnabledFont, clrDisabledFont, clrTitle);

  TMenuItem = record
    ItemType: TMenuItemType;
    ItemId: string;
    ItemText: string;
    L, T, W, H: cfloat;
  end;

  TMenuColor = record
    ColorId: string;
    R, G, B, A: byte;
  end;

  TMenuTextMapping = record
    InternalTextId, GTATextId: string;
  end;

  TMenuKeyAction = record
    ButtonId: cint;
    ButtonTextId: string;
    ButtonKey: TMenuKeyType;
  end;

const
  // Dimensions are filled in by code, upon DLL initialization!
  MenuItems: array [0..19] of TMenuItem =
             (
              (ItemType: mitGameSlot; ItemId: 'Sg01'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg02'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg03'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg04'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg05'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg06'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg07'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg08'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg09'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg10'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg11'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg12'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg13'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg14'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg15'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitGameSlot; ItemId: 'Sg16'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitNewGame; ItemId: 'NewGame'; ItemText: 'NewGameButton'; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitNoNewGame; ItemId: 'Exit'; ItemText: 'ExitButton'; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitNoSave; ItemId: 'Cancel'; ItemText: 'CancelButton'; L: 0.0; T: 0.0; W: 0.0; H: 0.0),
              (ItemType: mitTitle; ItemId: 'MenuTitle'; ItemText: ''; L: 0.0; T: 0.0; W: 0.0; H: 0.0)
             );

  // These color records are used to set the menu items' colors in various situations. RGBA values are filled in by code, upon DLL initialization!
  MenuColors: array [TMenuColorType] of TMenuColor =
             (
              (ColorId: 'EnabledSelectedBg'; R: 0; G: 0; B: 0; A: 0),
              (ColorId: 'DisabledSelectedBg'; R: 0; G: 0; B: 0; A: 0),
              (ColorId: 'EnabledFont'; R: 0; G: 0; B: 0; A: 0),
              (ColorId: 'DisabledFont'; R: 0; G: 0; B: 0; A: 0),
              (ColorId: 'Title'; R: 0; G: 0; B: 0; A: 0)
             );
  InvisibleColor: TMenuColor = (ColorId: ''; R: 0; G: 0; B: 0; A: 0); // This is for invisible colors...

  // Used to determine keypress events
  MenuKeyActions: array [0..4] of TMenuKeyAction =
             (
              (ButtonId: 32; ButtonTextId: 'UpKey'; ButtonKey: mkPrevItem),
              (ButtonId: 33; ButtonTextId: 'DownKey'; ButtonKey: mkNextItem),
              (ButtonId: 237; ButtonTextId: ''; ButtonKey: mkMouseClick),
              (ButtonId: 23; ButtonTextId: 'SelectKey'; ButtonKey: mkSelect),
              (ButtonId: 244; ButtonTextId: 'ExitKey'; ButtonKey: mkExit)
             );

var
  // Used to map Internal text IDs to GTA5 GXT IDs
  GameTextMapping: array of TMenuTextMapping;

// Gets the ingame text for a given item by its ID
function GetGameTitle(id: string): string;
var
  i: integer;
begin
  Result := '';
  // Find text ID mapping
  for i := 0 to High(GameTextMapping) do
      if (id = GameTextMapping[i].InternalTextId) then
         begin
           Result := strpas(GET_FILENAME_FOR_AUDIO_CONVERSATION(PChar(GameTextMapping[i].GTATextId)));
           break;
         end;
end;

// Loads the menu layout
procedure LoadMenuData;

          function FloatNorm(flt: string): real;
          var
            f: real;
          begin
            if TryStrToFloat(Trim(flt), f, YosFormatSettings) then
               Result := min(1.0, max(0.0, f))
            else
               Result := 0.0;
          end;

          function ByteNorm(byt: string): byte;
          var
            i: integer;
          begin
            if TryStrToInt(Trim(byt), i) then
               Result := byte(min(255, max(0, i)))
            else
               Result := 0;
          end;

var
  datafile: TIniFile;
  keys: TStrings;
  dataline: string;
  color: TMenuColorType;
  i: integer;
begin
  SetLength(GameTextMapping, 0);
  datafile := TIniFile.Create('yos_data/data/menu_layout.dat');
  keys := TStringList.Create;
  try
    // Colors
    for color := Low(TMenuColorType) to High(TMenuColorType) do
        begin
          dataline := Trim(datafile.ReadString('Colors', MenuColors[color].ColorId, ''));
          MenuColors[color].R := ByteNorm(GetDataItem(dataline, 0, [',']));
          MenuColors[color].G := ByteNorm(GetDataItem(dataline, 1, [',']));
          MenuColors[color].B := ByteNorm(GetDataItem(dataline, 2, [',']));
          MenuColors[color].A := ByteNorm(GetDataItem(dataline, 3, [',']));
        end;

    // Text mappings
    keys.Clear;
    datafile.ReadSection('Text', keys);
    SetLength(GameTextMapping, keys.Count);
    for i := 0 to High(GameTextMapping) do
        begin
          GameTextMapping[i].InternalTextId := Trim(keys[i]);
          GameTextMapping[i].GTATextId := Trim(datafile.ReadString('Text', keys[i], ''));
        end;

    // Item dimensions
    for i := 0 to High(MenuItems) do
        begin
          dataline := Trim(datafile.ReadString('Dimensions', MenuItems[i].ItemId, ''));
          MenuItems[i].L := FloatNorm(GetDataItem(dataline, 0, [',']));
          MenuItems[i].T := FloatNorm(GetDataItem(dataline, 1, [',']));
          MenuItems[i].W := FloatNorm(GetDataItem(dataline, 2, [',']));
          MenuItems[i].H := FloatNorm(GetDataItem(dataline, 3, [',']));
        end;
  finally
    datafile.Free;
    keys.Free;
  end;
end;

procedure ClearMenuData;
begin
  SetLength(GameTextMapping, 0);
end;

{%endregion}

{%region /fold 'Public Helpers'}
var
  CustTexFile: array of record
    ID: cint;
    FileName: string;
  end;
  CustTexIndex, CustTexDraw: array of record
    ID, Data: cint;
  end;
  CustTexGlobalLvl, CustTextGlobalLastFrame: cint;

procedure PushMapNotification(msg: string);
begin
  BEGIN_TEXT_COMMAND_THEFEED_POST('STRING');
  ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(msg));
  END_TEXT_COMMAND_THEFEED_POST_TICKER(BOOL(0), BOOL(0));
end;

procedure DrawCustomTexture(fileName: string; sizeX, sizeY, posX, posY, ofsX, ofsY, rotation: cfloat; bCentered, bScaled: boolean; r, g, b, a: byte);
var
  i, j: integer;
  success: boolean;
  id, frameCount, index: cint;
  aspRatio, scaleX, scaleY, positionX, positionY: cfloat;
begin
  // Register texture, if necessary
  success := false;
  for i := 0 to High(CustTexDraw) do
      begin
        if (LowerCase(fileName) = LowerCase(CustTexFile[i].FileName)) then
           begin
             success := true;
             id := CustTexDraw[i].ID;
             break;
           end;
      end;
  if not success then
     begin
       if FileExists(fileName) then
          begin
            id := createTexture(PChar(fileName));
            SetLength(CustTexFile, Length(CustTexFile) + 1);
            CustTexFile[High(CustTexFile)].ID := id;
            CustTexFile[High(CustTexFile)].FileName := fileName;
          end
       else
          exit;
     end;

  // Register texture ID, if necessary
  success := false;
  for i := 0 to High(CustTexIndex) do
      if (CustTexIndex[i].ID = id) then
         begin
           success := true;
           break;
         end;
  if not success then
     begin
       SetLength(CustTexIndex, Length(CustTexIndex) + 1);
       CustTexIndex[High(CustTexIndex)].ID := id;
       CustTexIndex[High(CustTexIndex)].Data := 0;
  end;

  // Register texture draw, if necessary
  success := false;
  for i := 0 to High(CustTexDraw) do
      if (CustTexDraw[i].ID = id) then
         begin
           success := true;
           break;
         end;
  if not success then
     begin
       SetLength(CustTexDraw, Length(CustTexDraw) + 1);
       CustTexDraw[High(CustTexDraw)].ID := id;
       CustTexDraw[High(CustTexDraw)].Data := 0;
     end;

  // Setup texture draw (same approach as ScriptHookVDotNet CustomSprite)
  frameCount := GET_FRAME_COUNT;
  for i := 0 to High(CustTexDraw) do
      if (CustTexDraw[i].ID = id) then
         begin
           if (CustTexDraw[i].Data <> frameCount) then
              begin
                for j := 0 to High(CustTexIndex) do
                    if (CustTexIndex[j].ID = id) then
                       begin
                         CustTexIndex[j].Data := 0;
                         break;
                       end;
                CustTexDraw[i].Data := frameCount;
              end;
           break;
         end;
  if (CustTextGlobalLastFrame <> frameCount) then
     begin
       CustTexGlobalLvl := 0;
       CustTextGlobalLastFrame := frameCount;
     end;
  aspRatio := GET_ASPECT_RATIO(BOOL(0));
  if bScaled then
     scaleX := sizeX / (720.0 * aspRatio)
  else
     scaleX := sizeX / 1280.0;
  scaleY := SizeY / 720.0;
  if bScaled then
     positionX := (posX + ofsX) / (720.0 * aspRatio)
  else
     positionX := (posX + ofsX) / 1280.0;
  positionY := (posY + ofsY) / 720.0;
  if bCentered then
     begin
       positionX := positionX + (scaleX * 0.5);
       positionY := positionY + (scaleY * 0.5);
     end;

  // Draw the texture
  for i := 0 to High(CustTexIndex) do
      if (CustTexIndex[i].ID = id) then
         begin
           index := CustTexIndex[i].Data;
           CustTexIndex[i].Data := index + 1;
           break;
         end;
  CustTexGlobalLvl := CustTexGlobalLvl + 1;
  drawTexture(id, index, CustTexGlobalLvl - 1, 100, scaleX, scaleY / aspRatio, 0.5, 0.5, positionX, positionY, rotation * 0.00277777778, aspRatio, r / 255.0, g / 255.0, b / 255.0, a / 255.0);
end;

procedure ClearCustomTextures;
begin
  SetLength(CustTexFile, 0);
  SetLength(CustTexIndex, 0);
  SetLength(CustTexDraw, 0);
  CustTexGlobalLvl := 0;
  CustTextGlobalLastFrame := 0;
end;

procedure TGameScreen.LoadSavegameInfo;
var
  stream: TFileStream;
  i: integer;
  savefile: string;
begin
  ClearSavegameInfo;
  for i := 0 to 15 do
      begin
        savefile := GetSaveFileName(i);
        if FileExists(savefile) then
           begin
             stream := TFileStream.Create(savefile, fmOpenRead);
             try
               try
                 // NOTE: This procedure treats the savefiles in the exact same way, as the Load / Save methods of the TMissionScript class!
                 stream.Position := 17;
                 savegameTitles[i] := ReadCString(stream) + ' ' +  DateTimeToStr(FileDateToDateTime(FileAge(savefile)), YosFormatSettings, true);
               except
                 savegameTitles[i] := '';
               end;
             finally
               stream.Free;
             end;
           end;
      end;
end;

{%endregion}

function TGameScreen.QuerySavegameInfo(sgi: integer; out title: string): boolean;
begin
  Result := (sgi in [0..15]) and (savegameTitles[sgi] <> '');
  if Result then
     title := savegameTitles[sgi]
  else
     title := '';
end;

procedure TGameScreen.ClearSavegameInfo;
var
  i: integer;
begin
  for i := 0 to 15 do
      savegameTitles[i] := '';
end;

procedure TGameScreen.InitScaleformButtons;
var
  i: integer;
begin
  buttonsMovie := REQUEST_SCALEFORM_MOVIE(PChar('instructional_buttons'));
  while (HAS_SCALEFORM_MOVIE_LOADED(buttonsMovie) = BOOL(0)) do
        ScriptHookVWait(0);

  BEGIN_SCALEFORM_MOVIE_METHOD(buttonsMovie, PChar('CLEAR_ALL'));
  END_SCALEFORM_MOVIE_METHOD;

  BEGIN_SCALEFORM_MOVIE_METHOD(buttonsMovie, PChar('SET_CLEAR_SPACE'));
  SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(200);
  END_SCALEFORM_MOVIE_METHOD;

  for i := 0 to High(MenuKeyActions) do
      begin
        BEGIN_SCALEFORM_MOVIE_METHOD(buttonsMovie, PChar('SET_DATA_SLOT'));
        SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(cint(i));
        SCALEFORM_MOVIE_METHOD_ADD_PARAM_PLAYER_NAME_STRING(GET_CONTROL_INSTRUCTIONAL_BUTTONS_STRING(0, MenuKeyActions[i].ButtonId, BOOL(1)));
        BEGIN_TEXT_COMMAND_SCALEFORM_STRING(PChar('STRING'));
        if (MenuKeyActions[i].ButtonTextId  <> '') then
           ADD_TEXT_COMPONENT_SUBSTRING_KEYBOARD_DISPLAY(PChar(GetGameTitle(MenuKeyActions[i].ButtonTextId)))
        else
           ADD_TEXT_COMPONENT_SUBSTRING_KEYBOARD_DISPLAY(nil);
        END_TEXT_COMMAND_SCALEFORM_STRING;
        END_SCALEFORM_MOVIE_METHOD;
      end;

  BEGIN_SCALEFORM_MOVIE_METHOD(buttonsMovie, PChar('DRAW_INSTRUCTIONAL_BUTTONS'));
  END_SCALEFORM_MOVIE_METHOD;

  BEGIN_SCALEFORM_MOVIE_METHOD(buttonsMovie, PChar('SET_BACKGROUND_COLOR'));
  SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(cint(MenuColors[clrDisabledSelBg].R));
  SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(cint(MenuColors[clrDisabledSelBg].G));
  SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(cint(MenuColors[clrDisabledSelBg].R));
  SCALEFORM_MOVIE_METHOD_ADD_PARAM_INT(cint(MenuColors[clrDisabledSelBg].A));
  END_SCALEFORM_MOVIE_METHOD;
end;

procedure TGameScreen.SetMenu(menu: TMenuType);
begin
  if (not visible) then
     currentMenu := menu;
end;

constructor TGameScreen.Create;
begin
  inherited;
  currentMenu := gmInGame;
  currentKey := mkNone;
  itemIndex := 0;
  radioStation := PChar('OFF');
  visible := false;
  buttonsMovie := 0;
  ClearSavegameInfo;
end;

destructor TGameScreen.Destroy;
begin
  ClearSavegameInfo;
  inherited;
end;

procedure TGameScreen.DrawLoadingScreen;
begin
  if (currentMenu = gmLoadingScreen) then
     begin
       DRAW_RECT(0.5, 0.5, 1.0, 1.0, cint(MenuColors[clrDisabledSelBg].R), cint(MenuColors[clrDisabledSelBg].G), cint(MenuColors[clrDisabledSelBg].B), 255, BOOL(0));
       SET_TEXT_FONT(1);
       SET_TEXT_SCALE(0.0, 1.5);
       SET_TEXT_CENTRE(BOOL(0));
       SET_TEXT_COLOUR(cint(MenuColors[clrTitle].R), cint(MenuColors[clrTitle].G), cint(MenuColors[clrTitle].B), cint(MenuColors[clrTitle].A));
       SET_TEXT_DROPSHADOW(4, 0, 0, 0, 255);
       BEGIN_TEXT_COMMAND_DISPLAY_TEXT(PChar('STRING'));
       ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(GetGameTitle('Ld_FullScr')));
       END_TEXT_COMMAND_DISPLAY_TEXT(0.05, 0.05, 0);
     end
  else
     begin
       DRAW_RECT(0.5, 0.5, 0.5, 0.25, cint(MenuColors[clrDisabledSelBg].R), cint(MenuColors[clrDisabledSelBg].G), cint(MenuColors[clrDisabledSelBg].B), 192, BOOL(0));
       SET_TEXT_FONT(1);
       SET_TEXT_SCALE(0.0, 1.5);
       SET_TEXT_CENTRE(BOOL(1));
       SET_TEXT_COLOUR(cint(MenuColors[clrTitle].R), cint(MenuColors[clrTitle].G), cint(MenuColors[clrTitle].B), cint(MenuColors[clrTitle].A));
       SET_TEXT_DROPSHADOW(4, 0, 0, 0, 255);
       BEGIN_TEXT_COMMAND_DISPLAY_TEXT(PChar('STRING'));
       ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(GetGameTitle('Ld_MicroScr')));
       END_TEXT_COMMAND_DISPLAY_TEXT(0.5, 0.5, 0);
     end;
end;

function TGameScreen.ProcessMenu(out eventId: TMenuEventType; out sgId: integer; playerindex: integer; gameStarted: boolean): boolean;

         // Find savegame index of a button
         function FindSavegameIndex(buttonIndex: integer): integer;
         var
           i,sgi: integer;
         begin
           Result := -1;
           sgi := -1;
           for i := 0 to High(MenuItems) do
               if (MenuItems[i].ItemType = mitGameSlot) then
                  begin
                    inc(sgi);
                    if (buttonIndex = i) then
                       exit(sgi);
                  end;
         end;

         // Determine, if a point on Game Screen is inside a Menu Item
         function IsPointInMenuItem(x, y: cfloat; item: TMenuItem): boolean;
         begin
           Result := (x >= (item.L - (item.W / 2))) and
                     (x <= (item.L + (item.W / 2))) and
                     (y >= (item.T - (item.H / 2))) and
                     (y <= (item.T + (item.H / 2)));
         end;

         // Determine if a particular menu item reacts to events
         function IsButtonReactive(item: TMenuItem): boolean;
         begin
           if (currentMenu in [gmLoadGame, gmSaveGame]) and (item.ItemType <> mitTitle) then
              begin
                if (item.ItemType = mitGameSlot) then
                   Result := true
                else if (currentMenu = gmLoadGame) then
                   Result := (item.ItemType <> mitNoSave)
                else
                   Result := (item.ItemType = mitNoSave);
              end
           else
              Result := false;
         end;

         // Find the next reactive menu item
         function FindNextMenuItem(idx: integer): integer;
         var
           i: integer;
         begin
           Result := -1;
           for i := idx + 1 to High(MenuItems) do
               if IsButtonReactive(MenuItems[i]) then
                  exit(i);
         end;

         // Find the previous reactive menu item
         function FindPrevMenuItem(idx: integer): integer;
         var
           i: integer;
         begin
           Result := -1;
           for i := idx - 1 downto 0 do
               if IsButtonReactive(MenuItems[i]) then
                  exit(i);
         end;

var
  r, g, b, a: cint;
  i, sgi: integer;
  itemText: string;
  itemTextColor, itemBgColor: TMenuColor;
begin
  Result := false;

  // Menu handling
  if (currentMenu <> gmInGame) then
     begin
       // Menu was not visible!
       if not visible then
          begin
            visible := true;
            if (GET_PLAYER_RADIO_STATION_INDEX = 255) then
               radioStation := PChar('OFF')
            else
               radioStation := GET_PLAYER_RADIO_STATION_NAME;
            SET_RADIO_TO_STATION_NAME(PChar('OFF'));
            SET_GAME_PAUSED(BOOL(1));
            InitScaleformButtons;
            LoadSavegameInfo;
            itemIndex := 0;
          end;
       DISABLE_ALL_CONTROL_ACTIONS(0);
       SET_MOUSE_CURSOR_THIS_FRAME;

       // Check, if any key has been pressed
       for i := 0 to High(MenuKeyActions) do
           if (IS_DISABLED_CONTROL_JUST_RELEASED(0, MenuKeyActions[i].ButtonId) <> BOOL(0)) then
              begin
                currentKey := MenuKeyActions[i].ButtonKey;
                break;
              end;
       // This changes the currently highlighhted item...
       if (currentKey = mkPrevItem) then
          begin
            i := FindPrevMenuItem(itemIndex);
            if (i >= 0) then
               begin
                 itemIndex := i;
                 PLAY_SOUND_FRONTEND(-1, PChar('NAV_UP_DOWN'), PChar('HUD_FRONTEND_DEFAULT_SOUNDSET'), BOOL(0));
               end;
            currentKey := mkNone;
          end
       else if (currentKey = mkNextItem) then
          begin
            i := FindNextMenuItem(itemIndex);
            if (i >= 0) then
               begin
                 itemIndex := i;
                 PLAY_SOUND_FRONTEND(-1, PChar('NAV_UP_DOWN'), PChar('HUD_FRONTEND_DEFAULT_SOUNDSET'), BOOL(0));
               end;
            currentKey := mkNone;
          end
       else
          begin
            // Mouse clicks must be converted to 'select' events, if possible
            if (currentKey = mkMouseClick) then
               begin
                 currentKey := mkNone; // If we did not find any clicked item, the click is lost...
                 for i := 0 to High(MenuItems) do
                     begin
                       // Selectable
                       if IsButtonReactive(MenuItems[i]) then
                          begin
                            ENABLE_CONTROL_ACTION(0, 239, BOOL(1));
                            ENABLE_CONTROL_ACTION(0, 240, BOOL(1));
                            if IsPointInMenuItem(GET_CONTROL_NORMAL(0, 239), GET_CONTROL_NORMAL(0, 240), MenuItems[i]) then
                               begin
                                 itemIndex := i;
                                 currentKey := mkSelect;
                                 break;
                               end;
                          end;
                     end;
               end;

            // 'Select' events can get converted to 'Exit' events, or game events, if the selected item is actually selectable
            if (currentKey = mkSelect) and (itemIndex >= 0) then
               begin
                 // Check, if the selected item is active, and can raise an event
                 if (MenuItems[itemIndex].ItemType in [mitNoNewGame, mitNoSave]) then
                    currentKey := mkExit // Menu exited via button select
                 else if (MenuItems[itemIndex].ItemType = mitNewGame) then
                    begin
                      currentKey := mkNone;
                      // Signal event: new game
                      Result := true;
                      eventId := evtNewGame;
                      sgId := -1;
                    end
                 else if (MenuItems[itemIndex].ItemType = mitGameSlot) then
                    begin
                      currentKey := mkNone;
                      sgId := FindSavegameIndex(itemIndex);
                      Result := (sgId in [0..15]);
                      if (currentMenu = gmLoadGame) then
                         begin
                           // Signal event: load game
                           Result := Result and FileExists(GetSaveFileName(sgId));
                           eventId := evtLoadGame;
                         end
                      else
                         eventId := evtSaveGame; // Signal event: save game
                    end;
               end;

            // 'Exit' events must exit menu, without causing a game event
            if (currentKey = mkExit) then
               begin
                 currentKey := mkNone;
                 // Signal event: cancel menu
                 Result := true;
                 eventId := evtCancelMenu;
                 sgId := -1;
               end;
          end;

       // We only draw menu, if we don't output an event (because that hides the menu)...
       if (not Result) and (currentMenu in [gmLoadGame, gmSaveGame]) then
          begin
            // Background
            if gameStarted then
               begin
                 case playerindex of
                      0: GET_HUD_COLOUR(143, @r, @g, @b, @a);  // Michael
                      1: GET_HUD_COLOUR(144, @r, @g, @b, @a);  // Franklin
                      2: GET_HUD_COLOUR(145, @r, @g, @b, @a);  // Trevor
                      else GET_HUD_COLOUR(28, @r, @g, @b, @a); // Net Player 1 ???
                 end;
                 DRAW_RECT(0.5, 0.5, 1.0, 1.0, r, g, b, 128, BOOL(0));
               end
            else
               DRAW_RECT(0.5, 0.5, 1.0, 1.0, 32, 32, 32, 255, BOOL(0));

            // Items
            for i := 0 to High(MenuItems) do
                if (MenuItems[i].ItemType = mitTitle) then
                   begin
                     SET_TEXT_FONT(1);
                     SET_TEXT_SCALE(0.0, 1.5);
                     SET_TEXT_CENTRE(BOOL(0));
                     SET_TEXT_COLOUR(cint(MenuColors[clrTitle].R), cint(MenuColors[clrTitle].G), cint(MenuColors[clrTitle].B), cint(MenuColors[clrTitle].A));
                     SET_TEXT_DROPSHADOW(4, 0, 0, 0, 255);
                     BEGIN_TEXT_COMMAND_DISPLAY_TEXT(PChar('STRING'));
                     if (currentMenu = gmLoadGame) then
                        ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(GetGameTitle('LoadGameTitle')))
                     else
                        ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(GetGameTitle('SaveGameTitle')));
                     END_TEXT_COMMAND_DISPLAY_TEXT(MenuItems[High(MenuItems)].L, MenuItems[High(MenuItems)].T, 0);
                   end
                else if IsButtonReactive(MenuItems[i]) then
                   begin
                     itemText := '';
                     sgi := FindSavegameIndex(i);
                     // Selectable
                     if (not (sgi in [0..15])) or (currentMenu = gmSaveGame) or QuerySavegameInfo(sgi, itemText) then
                        begin
                           itemTextColor := MenuColors[clrEnabledFont];
                           if (itemIndex = i) then
                              itemBgColor := MenuColors[clrEnabledSelBg]
                           else
                              itemBgColor := InvisibleColor;
                        end
                     // Not selectable
                     else
                        begin
                          itemTextColor := MenuColors[clrDisabledFont];
                          if (itemIndex = i) then
                             itemBgColor := MenuColors[clrDisabledSelBg]
                          else
                             itemBgColor := InvisibleColor;
                        end;
                     // Nonexistent savegame slots, and buttons
                     if (itemText = '') then
                        begin
                          if (MenuItems[i].ItemType = mitGameSlot) then
                             begin
                               begin
                                 if not QuerySavegameInfo(sgi, itemText) then
                                    itemText := GetGameTitle('EmptySlot');
                               end;
                             end
                          else
                             itemText := GetGameTitle(MenuItems[i].ItemText);
                        end;
                     // Draw the item
                     DRAW_RECT(MenuItems[i].L, MenuItems[i].T, MenuItems[i].W, MenuItems[i].H, itemBgColor.R, itemBgColor.G, itemBgColor.B, itemBgColor.A, BOOL(0));
                     SET_TEXT_FONT(0);
                     SET_TEXT_SCALE(0.0, 0.4);
                     if (MenuItems[i].ItemType = mitGameSlot) then
                        SET_TEXT_CENTRE(BOOL(0))
                     else
                        SET_TEXT_CENTRE(BOOL(1));
                     SET_TEXT_COLOUR(itemTextColor.R, itemTextColor.G, itemTextColor.B, itemTextColor.A);
                     SET_TEXT_DROPSHADOW(0, 0, 0, 0, 0);
                     BEGIN_TEXT_COMMAND_DISPLAY_TEXT(PChar('STRING'));
                     ADD_TEXT_COMPONENT_SUBSTRING_PLAYER_NAME(PChar(itemText));
                     if (MenuItems[i].ItemType = mitGameSlot) then
                        END_TEXT_COMMAND_DISPLAY_TEXT(MenuItems[i].L - (MenuItems[i].W / 2), MenuItems[i].T - (MenuItems[i].H / 2), 0)
                     else
                        END_TEXT_COMMAND_DISPLAY_TEXT(MenuItems[i].L, MenuItems[i].T - (MenuItems[i].H / 2), 0);
                   end;
            // Instructional buttons movie
            DRAW_SCALEFORM_MOVIE_FULLSCREEN(buttonsMovie, 255, 255, 255, 255, 0);
          end
       // If the menu was changed, we hide the menu, and restore game state
       else if Result and visible then
          begin
            visible := false;
            SET_SCALEFORM_MOVIE_AS_NO_LONGER_NEEDED(@buttonsMovie);
            buttonsMovie := 0;
            SET_GAME_PAUSED(BOOL(0));
            SET_RADIO_TO_STATION_NAME(radioStation);
            ClearSavegameInfo;
          end;
     end;
end;


initialization
  ClearCustomTextures;
  LoadMenuData;
  GameScreen := TGameScreen.Create;

finalization
  GameScreen.Free;
  ClearCustomTextures;
  ClearMenuData;

end.

