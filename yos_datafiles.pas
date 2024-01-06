unit YOS_DataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, YOS_Utils;

function GetIntStatCount: integer;
function GetIntStatName(index: integer): string;
function GetIntStatDefaultValue(index: integer): cint;
function GetFloatStatCount: integer;
function GetFloatStatName(index: integer): string;
function GetFloatStatDefaultValue(index: integer): cfloat;
function GetAudioFlagCount: integer;
function GetAudioFlagName(index: integer): string;
function GetAudioFlagDefaultValue(index: integer): boolean;

implementation

var
  IntStats: array of record
    StatName: string;
    DefaultValue: cint;
  end;
  FloatStats: array of record
    StatName: string;
    DefaultValue: cfloat;
  end;
  AudioFlags: array of record
    FlagName: string;
    DefaultValue: boolean;
  end;

function GetIntStatCount: integer;
begin
  Result := Length(IntStats);
end;

function GetIntStatName(index: integer): string;
begin
  if (index >= 0) and (index <= High(IntStats)) then
     Result := IntStats[index].StatName
  else
     Result := '';
end;

function GetIntStatDefaultValue(index: integer): cint;
begin
  if (index >= 0) and (index <= High(IntStats)) then
     Result := IntStats[index].DefaultValue
  else
     Result := 0;
end;

function GetFloatStatCount: integer;
begin
  Result := Length(FloatStats);
end;

function GetFloatStatName(index: integer): string;
begin
  if (index >= 0) and (index <= High(FloatStats)) then
     Result := FloatStats[index].StatName
  else
     Result := '';
end;

function GetFloatStatDefaultValue(index: integer): cfloat;
begin
  if (index >= 0) and (index <= High(FloatStats)) then
     Result := FloatStats[index].DefaultValue
  else
     Result := 0.0;
end;

function GetAudioFlagCount: integer;
begin
  Result := Length(AudioFlags);
end;

function GetAudioFlagName(index: integer): string;
begin
  if (index >= 0) and (index <= High(AudioFlags)) then
     Result := AudioFlags[index].FlagName
  else
     Result := '';
end;

function GetAudioFlagDefaultValue(index: integer): boolean;
begin
  if (index >= 0) and (index <= High(AudioFlags)) then
     Result := AudioFlags[index].DefaultValue
  else
     Result := false;
end;

procedure InitDataLists;
var
  datalines: TStrings;
  i: integer;
  line, datatag: string;
  defint: cint;
  deffloat: cfloat;
begin
  datalines := ReadRawData('yos_data/data/statistics.dat');
  try
    for i := 0 to datalines.Count - 1 do
        begin
          line := Trim(datalines[i]);
          case LowerCase(GetDataItem(line, 1, [#9, ' '])) of
               'i':
                 begin
                   SetLength(IntStats, Length(IntStats) + 1);
                   IntStats[High(IntStats)].StatName := GetDataItem(line, 0, [#9, ' ']);
                   if TryStrToInt(GetDataItem(line, 2, [#9, ' ']), defint) then
                      IntStats[High(IntStats)].DefaultValue := defint
                   else
                      IntStats[High(IntStats)].DefaultValue := 0;
                 end;
               'f':
                 begin
                   SetLength(FloatStats, Length(FloatStats) + 1);
                   FloatStats[High(FloatStats)].StatName := GetDataItem(line, 0, [#9, ' ']);
                   if TryStrToFloat(GetDataItem(line, 2, [#9, ' ']), deffloat, YosFormatSettings) then
                      FloatStats[High(FloatStats)].DefaultValue := deffloat
                   else
                      FloatStats[High(FloatStats)].DefaultValue := 0.0;
                 end;
          end;
        end;
  finally
    datalines.Free;
  end;
  datalines := ReadRawData('yos_data/data/AudioFlags.dat');
  try
    for i := 0 to datalines.Count - 1 do
        begin
          line := Trim(datalines[i]);
          datatag := GetDataItem(line, 0, [#9, ' ']);
          if (datatag <> '') then
             begin
               SetLength(AudioFlags, Length(AudioFlags) + 1);
               AudioFlags[High(AudioFlags)].FlagName := datatag;
               if TryStrToInt(GetDataItem(line, 1, [#9, ' ']), defint) then
                  AudioFlags[High(AudioFlags)].DefaultValue := (defint <> 0)
               else
                  AudioFlags[High(AudioFlags)].DefaultValue := false;
             end;
        end;
  finally
    datalines.Free;
  end;
end;

initialization
  SetLength(IntStats, 0);
  SetLength(FloatStats, 0);
  SetLength(AudioFlags, 0);
  InitDataLists;

finalization
  SetLength(IntStats, 0);
  SetLength(FloatStats, 0);
  SetLength(AudioFlags, 0);

end.

