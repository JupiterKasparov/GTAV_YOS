unit YOS_Utils;

{$mode objfpc}{$H+}

interface

uses
  Windows, ctypes, Classes, SysUtils;

type
  TGTA5Array = packed array of UINT32;

// Helpers
function CurrentTimeMs: int64;
procedure WriteCString(str: string; stream: TStream);
function ReadCString(stream: TStream): string;
function ReadRawData(filename: string): TStrings;
function GetDataItem(dataline: string; index: integer; separators: TSysCharSet): string;
function GetSaveFileName(sgi: integer): string;
function IsInCube(x, y, z, x1, y1, z1, x2, y2, z2: cfloat): boolean;
function IsInAngledCube(x, y, z, x1, y1, z1, x2, y2, z2, a: cfloat): boolean;

// GTA Array functions. Based on: https://gtaforums.com/topic/789788-function-args-to-pedget_ped_nearby_peds/
procedure GTA5_SetArrayLength(var arr: TGTA5Array; newLength: integer);
procedure GTA5_SetArrayItem(var arr: TGTA5Array; index: integer; newValue: UINT32);
function GTA5_GetArrayLength(var arr: TGTA5Array): integer;
function GTA5_GetArrayItem(var arr: TGTA5Array; index: integer): UINT32;

// Debug commands
procedure Log(category, str: string);

var
  YosFormatSettings: TFormatSettings;

implementation

var
  pFreq: LARGE_INTEGER;
  bHasLogFile: boolean;

function CurrentTimeMs: int64;
var
  currentTime: LARGE_INTEGER;
begin
  QueryPerformanceCounter(@currentTime);
  Result := System.trunc((1000 * currentTime.QuadPart) / pFreq.QuadPart);
end;

procedure WriteCString(str: string; stream: TStream);
var
  i: integer;
begin
  for i := 1 to Length(str) do
      stream.WriteByte(ord(str[i]));
  stream.WriteByte(0);
end;

function ReadCString(stream: TStream): string;
var
  c: char;
begin
  Result := '';
  repeat
    c := chr(stream.ReadByte);
    if (c <> #0) then
       Result := Result + c;
  until c = #0;
end;

function ReadRawData(filename: string): TStrings;
var
  f: System.Text;
  l: string;
begin
  Result := TStringList.Create;
  System.Assign(f, filename);
  {$I-}
  System.Reset(f);
  while not EOF(f) do
        begin
          readln(f, l);
          l := Trim(l);
          if (l <> '') and (l[1] <> ';') then
             Result.Add(l);
        end;
  System.Close(f);
  {$I+}
end;

function GetDataItem(dataline: string; index: integer; separators: TSysCharSet): string;
var
  ci, i: integer;
  s: string;
begin
  ci := 0;
  s := '';
  for i := 1 to Length(dataline) do
      begin
        if (dataline[i] in separators) or (i = Length(dataline)) then
           begin
             if (ci >= index) then
                begin
                  if not (dataline[i] in separators) then
                     s := s + dataline[i]; // The latest character of the line also counts, if not a separator!
                  exit(Trim(s)); // If we've found what do we need, just exit quickly!
                end
             else
                begin
                  s := '';
                  inc(ci);
                end;
           end
        else
           s := s + dataline[i];
      end;
  // By this point, if we haven't found anything, we're already out of the game...
  Result := '';
end;

function GetSaveFileName(sgi: integer): string;
begin
  Result := Format('yos_data/saved_games/YOS_V_savefile_%d.sav', [sgi]);
end;

function IsInCube(x, y, z, x1, y1, z1, x2, y2, z2: cfloat): boolean;
var
  f: cfloat;
begin
  {Let us reorder coords, if they're ill-shaped}
  if (x1 > x2) then
     begin
       f := x2;
       x2 := x1;
       x1 := f;
     end;
  if (y1 > y2) then
     begin
       f := y2;
       y2 := y1;
       y1 := f;
     end;
  if (z1 > z2) then
     begin
       f := z2;
       z2 := z1;
       z1 := f;
     end;
  {Calculate if inside the cube}
  Result := (x >= x1) and (x <= x2) and (y >= y1) and (y <= y2) and (z >= z1) and (z <= z2);
end;

function IsInAngledCube(x, y, z, x1, y1, z1, x2, y2, z2, a: cfloat): boolean;
var
  cx, cy, tx, ty, tx1, ty1, tx2, ty2, rx, ry, rx1, ry1, rx2, ry2, cosa, sina: cfloat;
begin
  // Center of the rectangle
  cx := (x1 + x2) / 2;
  cy := (y1 + y2) / 2;

  // Translated rectangle coordinates
  tx := x - cx;
  ty := y - cy;
  tx1 := x1 - cx;
  ty1 := y1 - cy;
  tx2 := x2 - cx;
  ty2 := y2 - cy;

  // Rectangle angle (rad)
  cosa := cos(a * (pi / 180.0));
  sina := sin(a * (pi / 180.0));

  // Rotated rectangle points
  rx := (tx * cosa) + (ty * sina);
  ry := (-tx * sina) + (ty * cosa);
  rx1 := (tx1 * cosa) + (ty1 * sina);
  ry1 := (-tx1 * sina) + (ty1 * cosa);
  rx2 := (tx2 * cosa) + (ty2 * sina);
  ry2 := (-tx2 * sina) + (ty2 * cosa);

  // Check, if point is in cube
  Result := IsInCube(rx, ry, z, rx1, ry1, z1, rx2, ry2, z2);
end;

procedure GTA5_SetArrayLength(var arr: TGTA5Array; newLength: integer);
var
  oldMaxIndex, i: integer;
  ci: cint;
begin
  if (newLength < 0) then
     newLength := 0;
  oldMaxIndex := High(arr);

  // Set GTA5 array length
  SetLength(arr, 2 + (2 * newLength));
  ci := cint(newLength);
  arr[0] := PUINT32(@ci)^;

  // Fill newly added space with zeroes
  if (High(arr) > oldMaxIndex) then
     for i := oldMaxIndex + 1 to High(arr) do
         begin
           if (i > 1) then // First two items are taboo!
              arr[i] := 0;
         end;
end;

procedure GTA5_SetArrayItem(var arr: TGTA5Array; index: integer; newValue: UINT32);
begin
  arr[2 + (index * 2)] := newValue;
end;

function GTA5_GetArrayLength(var arr: TGTA5Array): integer;
var
  len: UINT32;
begin
  len := arr[0];
  Result := integer(pcint(@len)^);
end;

function GTA5_GetArrayItem(var arr: TGTA5Array; index: integer): UINT32;
begin
  Result := arr[2 + (index * 2)];
end;

procedure Log(category, str: string);
const
  logFileName: string = 'yos_debug.log';
var
  _time: SYSTEMTIME;
  f: System.Text;
begin
  GetLocalTime(_time);
  System.Assign(f, logFileName);
  {$I-}
  if bHasLogFile and FileExists(logFileName) then
     Append(f)
  else
     begin
       bHasLogFile := true;
       Rewrite(f);
     end;
  writeln(f, Format('%.2d/%.2d/%.4d %.2d:%.2d:%.2d.%.3d - [%s]: %s', [_time.Day, _time.Month, _time.Year, _time.Hour, _time.Minute, _time.Second, _time.Millisecond, category, str]));
  System.Close(f);
  {$I+}
end;

initialization
  QueryPerformanceFrequency(@pFreq);
  bHasLogFile := false;
  YosFormatSettings := DefaultFormatSettings;
  YosFormatSettings.DecimalSeparator := '.';
  YosFormatSettings.ShortDateFormat := 'DD MMM YYYY';
  YosFormatSettings.LongTimeFormat := 'hh:nn:ss';

end.

