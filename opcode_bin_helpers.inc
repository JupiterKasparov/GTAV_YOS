{%MainUnit yos_script.pas}

function TMissionThread.GetVarData(vloc: TVariableLocation; id: word): UINT32;
begin
  Result := 0;
  if (vloc = svlLocal) then
     Result := localVars[id].Value
  else
     Result := script.globalVars[id].Value;
end;

procedure TMissionThread.SetVarData(vloc: TVariableLocation; vtype: TVariableType; id: word; value: UINT32);
begin
  if (vloc = svlLocal) then
     begin
       localVars[id].VarType := vtype;
       localVars[id].Value := value;
     end
  else
     begin
       script.globalVars[id].VarType := vtype;
       script.globalVars[id].Value := value;
     end;
end;

function TMissionThread.GetVarFloat(vloc: TVariableLocation; id: word): cfloat;
var
  u32: UINT32;
begin
  u32 := GetVarData(vloc, id);
  Result := pcfloat(@u32)^;
end;

procedure TMissionThread.SetVarFloat(vloc: TVariableLocation; id: word; value: cfloat);
begin
  SetVarData(vloc, svtFloat, id, PUINT32(@value)^);
end;

function TMissionThread.GetVarInt(vloc: TVariableLocation; id: word): cint;
var
  u32: UINT32;
begin
  u32 := GetVarData(vloc, id);
  Result := pcint(@u32)^;
end;

procedure TMissionThread.SetVarInt(vloc: TVariableLocation; id: word; value: cint);
begin
  SetVarData(vloc, svtInt, id, PUINT32(@value)^);
end;

procedure TMissionThread.GetVarId(out vloc: TVariableLocation; out id: word);
begin
  vloc := TVariableLocation(script.bytes.ReadByte);
  id := script.bytes.ReadWord;
  inc(position, 3); {--->>> 1b var loc, 2b var id}
end;

function TMissionThread.GetVarType(vloc: TVariableLocation; id: word): TVariableType;
begin
  if (vloc = svlLocal) then
     Result := localVars[id].VarType
  else
     Result := script.globalVars[id].VarType;
end;

procedure TMissionThread.AddConditionalResult(result, negate: boolean);
begin
  SetLength(conditionalValues, Length(conditionalValues) + 1);
  if negate then
     conditionalValues[High(conditionalValues)] := not result
  else
     conditionalValues[High(conditionalValues)] := result;
end;

procedure TMissionThread.AllocateString(vloc: TVariableLocation; id: word; value: string);
var
  i: cint;
begin
  for i := 0 to High(script.globalStrings) do
      if not script.globalStrings[i].Used then
         begin
           script.globalStrings[i].Used := true;
           script.globalStrings[i].Value := value;
           SetVarData(vloc, svtString, id, PUINT32(@i)^);
           break;
         end;
end;

procedure TMissionThread.ClearVariable(vloc: TVariableLocation; id: word);
begin
  if (vloc = svlLocal) then
     script.ClearVariable(localVars[id])
  else
     script.ClearVariable(script.globalVars[id])
end;

function TMissionThread.GetNextParameterInt: cint;
var
  vloc: TVariableLocation;
  vid: word;
begin
  vloc := TVariableLocation(script.bytes.ReadByte);
  if (vloc = svlDirect) then
     begin
       Result := cint(script.bytes.ReadDWord);
       inc(position, 5); {--->>> 1b var loc, 4b int}
     end
  else
     begin
       vid := script.bytes.ReadWord;
       Result := cint(GetVarData(vloc, vid));
       inc(position, 3); {--->>> 1b var loc, 2b var id}
     end;
end;

function TMissionThread.GetNextParameterFloat: cfloat;
var
  vloc: TVariableLocation;
  vid: word;
  u32: UINT32;
begin
  vloc := TVariableLocation(script.bytes.ReadByte);
  if (vloc = svlDirect) then
     begin
       u32 := script.bytes.ReadDWord;
       inc(position, 5); {--->>> 1b var loc, 4b float}
     end
  else
     begin
       vid := script.bytes.ReadWord;
       u32 := GetVarData(vloc, vid);
       inc(position, 3); {--->>> 1b var loc, 2b var id}
     end;
  Result := pcfloat(@u32)^;
end;

function TMissionThread.GetNextParameterString: string;
var
  vloc: TVariableLocation;
  vid: word;
  jc: integer;
  c: char;
  index: cint;
begin
  vloc := TVariableLocation(script.bytes.ReadByte);
  if (vloc = svlDirect) then
     begin
       Result := '';
       jc := 0;
       repeat
         c := chr(script.bytes.ReadByte);
         inc(jc);
         if (c <> #0) then
            Result := Result + c;
       until c = #0;
       inc(position, 1 + jc); {--->>> 1b var loc, Nb string literal}
     end
  else
     begin
       vid := script.bytes.ReadWord;
       index := cint(GetVarData(vloc, vid));
       Result := script.globalStrings[index].Value;
       inc(position, 3); {--->>> 1b var loc, 2b var id}
     end;
end;

