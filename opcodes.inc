{%MainUnit yos_script.pas}

type
  TOpCodeHandler = procedure(thread: TMissionThread; negateresult: boolean);

{No Operation}
procedure scr_0000_nop(thread: TMissionThread; negateresult: boolean);
begin
   asm
      nop
   end;
end;

{Wait X milliseconds}
procedure scr_0001_wait(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'time'}
   thread.waitTime := thread.GetNextParameterInt;
   thread.status := tstWaiting;
end;

{Jump to label}
procedure scr_0002_jump(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'label'}
   thread.position := thread.script.bytes.ReadQWord - thread.offset;
end;

{Call a subroutine (label)}
procedure scr_0003_gosub(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'label'}
   thread.gosubReturn[thread.gosubLevel] := thread.position + 8; {--->>> 8b label id}
   inc(thread.gosubLevel);
   thread.position := thread.script.bytes.ReadQWord - thread.offset;
end;

{Return from a subroutine}
procedure scr_0004_return(thread: TMissionThread; negateresult: boolean);
begin
   {Return from subroutine}
   thread.position := thread.gosubReturn[thread.gosubLevel - 1];
   dec(thread.gosubLevel);

   {If there's an active WB gosub, and we're at the same or 'upper' level as the WB gosub level, disable the WB gosub}
   if thread.wbGosubActive and (thread.gosubLevel <= thread.wbGosubLevel) then
     thread.wbGosubActive := false;
end;

{Evaluate the conditional result, and jump to label, if the conditional result was false}
procedure scr_0005_jump_if_false(thread: TMissionThread; negateresult: boolean);
var
  result: boolean;
  i: integer;
begin
   {Evaluate conditional result}
   result := false;
   if (Length(thread.conditionalValues) > 0) then
      case thread.conditionalJumpEvaluationMethod of
           cjmpSingle:
             result := thread.conditionalValues[0];
           cjmpIfAnd:
             begin
               result := true;
               for i := 0 to High(thread.conditionalValues) do
                   if not thread.conditionalValues[i] then
                      begin
                        result := false;
                        break;
                      end;
             end;
           cjmpIfOr:
             begin
               result := false;
               for i := 0 to High(thread.conditionalValues) do
                   if thread.conditionalValues[i] then
                      begin
                        result := true;
                        break;
                      end;
             end;
      end;
   {Param: 'label'}
   if not result then
      thread.position := thread.script.bytes.ReadQWord - thread.offset
   else
      inc(thread.position, 8); {--->>> 8b label id}
end;

{Start a conditional block}
procedure scr_0006_if(thread: TMissionThread; negateresult: boolean);
begin
   {Clear conditional values list}
   SetLength(thread.conditionalValues, 0);
   {Param: 'method'}
   thread.conditionalJumpEvaluationMethod := TConditionalJumpEvaluationMethod(thread.script.bytes.ReadByte);
   inc(thread.position, 1); {--->>> 1b evaluation method id}
end;

{End caller thread}
procedure scr_0007_end_thread(thread: TMissionThread; negateresult: boolean);
begin
   thread.status := tstFinished;
end;

{End named thread}
procedure scr_0008_end_named_thread(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'name'}
   thread.script.KillThreadByName(thread.GetNextParameterString);
end;

{Create thread}
procedure scr_0009_create_thread(thread: TMissionThread; negateresult: boolean);
var
  pos: UINT64;
begin
   {Param: 'label'}
   pos := thread.script.bytes.ReadQWord;
   thread.script.CreateThread(pos);
   inc(thread.position, 8); {--->>> 8b label id}
end;

{Set caller thread's name}
procedure scr_000A_set_thread_name(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'name'}
   thread.scriptName := thread.GetNextParameterString;
end;

{Reset a variable}
procedure scr_000B_reset_variable(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
end;

{Set var as int}
procedure scr_000C_set_var_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Save result}
   thread.SetVarInt(vloc, vid, intValue);
end;

{Set var as float}
procedure scr_000D_set_var_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  flt: cfloat;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'float_value'}
   flt := thread.GetNextParameterFloat;
   {Save Result}
   thread.SetVarFloat(vloc, vid, flt);
end;

{Set var as string}
procedure scr_000E_set_var_string(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue: string;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {Save result}
   thread.AllocateString(vloc, vid, stringValue);
end;

{Set var as Vector3}
procedure scr_000F_set_var_vector3(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   for i := 0 to High(thread.script.globalVectors) do
       if not thread.script.globalVectors[i].Used then
          begin
            thread.script.globalVectors[i].Used := true;
            thread.script.globalVectors[i].Value.x := 0.0;
            thread.script.globalVectors[i].Value.y := 0.0;
            thread.script.globalVectors[i].Value.z := 0.0;
            thread.SetVarData(vloc, svtVector, vid, PUINT32(@i)^);
            break;
          end;
end;

{Set var as Array (numeric)}
procedure scr_0010_set_var_array(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   for i := 0 to High(thread.script.globalArrays) do
       if not thread.script.globalArrays[i].Used then
          begin
            thread.script.globalArrays[i].Used := true;
            GTA5_SetArrayLength(thread.script.globalArrays[i].Value, 0);
            thread.SetVarData(vloc, svtArray, vid, PUINT32(@i)^);
            break;
          end;
end;

{Set var as Array (string)}
procedure scr_0011_set_var_stringlist(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   for i := 0 to High(thread.script.globalStringLists) do
       if not thread.script.globalStringLists[i].Used then
          begin
            thread.script.globalStringLists[i].Used := true;
            SetLength(thread.script.globalStringLists[i].Value, 0);
            thread.SetVarData(vloc, svtStringArray, vid, PUINT32(@i)^);
            break;
          end;
end;

{Set a Vector3's X}
procedure scr_0012_set_vector3_x(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  f: cfloat;
begin
   {Param: 'vector_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'x'}
   f := thread.GetNextParameterFloat;
   {Set property}
   thread.script.globalVectors[thread.GetVarInt(vloc, vid)].Value.x := f;
end;

{Set a Vector3's Y}
procedure scr_0013_set_vector3_y(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  f: cfloat;
begin
   {Param: 'vector_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'y'}
   f := thread.GetNextParameterFloat;
   {Set property}
   thread.script.globalVectors[thread.GetVarInt(vloc, vid)].Value.y := f;
end;

{Set a Vector3's Z}
procedure scr_0014_set_vector3_z(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  f: cfloat;
begin
   {Param: 'vector_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'z'}
   f := thread.GetNextParameterFloat;
   {Set property}
   thread.script.globalVectors[thread.GetVarInt(vloc, vid)].Value.z := f;
end;

{Get a Vector3's X}
procedure scr_0015_get_vector3_x(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'vector_var'}
   thread.GetVarId(vloc2, vid2);
   {Save result}
   thread.SetVarFloat(vloc1, vid1, thread.script.globalVectors[thread.GetVarInt(vloc2, vid2)].Value.x);
end;

{Get a Vector3's Y}
procedure scr_0016_get_vector3_y(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'vector_var'}
   thread.GetVarId(vloc2, vid2);
   {Save result}
   thread.SetVarFloat(vloc1, vid1, thread.script.globalVectors[thread.GetVarInt(vloc2, vid2)].Value.y);
end;

{Get a Vector3's Z}
procedure scr_0017_get_vector3_z(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'vector_var'}
   thread.GetVarId(vloc2, vid2);
   {Save result}
   thread.SetVarFloat(vloc1, vid1, thread.script.globalVectors[thread.GetVarInt(vloc2, vid2)].Value.z);
end;

{Set an array (numeric) item as int}
procedure scr_0018_set_array_item_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index, intValue: cint;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   GTA5_SetArrayItem(thread.script.globalArrays[thread.GetVarInt(vloc, vid)].Value, index, PUINT32(@intValue)^);
end;

{Get an array (numeric) item as int}
procedure scr_0019_get_array_item_int(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
  index: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'array_var'}
   thread.GetVarId(vloc2, vid2);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Save result}
   thread.SetVarData(vloc1, svtInt, vid1, GTA5_GetArrayItem(thread.script.globalArrays[thread.GetVarInt(vloc2, vid2)].Value, index));
end;

{Add a new int item to an array (numeric)}
procedure scr_001A_add_array_item_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  arrayIndex, intValue: cint;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Add item}
   arrayIndex := thread.GetVarInt(vloc, vid);
   GTA5_SetArrayLength(thread.script.globalArrays[arrayIndex].Value, GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value) + 1);
   GTA5_SetArrayItem(thread.script.globalArrays[arrayIndex].Value, GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value) - 1, PUINT32(@intValue)^);
end;

{Set an array (numeric) item as float}
procedure scr_001B_set_array_item_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index: cint;
  floatValue: cfloat;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Set item}
   GTA5_SetArrayItem(thread.script.globalArrays[thread.GetVarInt(vloc, vid)].Value, index, PUINT32(@floatValue)^);
end;

{Get an array (numeric) item as float}
procedure scr_001C_get_array_item_float(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
  index: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'array_var'}
   thread.GetVarId(vloc2, vid2);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Save result}
   thread.SetVarData(vloc1, svtFloat, vid1, GTA5_GetArrayItem(thread.script.globalArrays[thread.GetVarInt(vloc2, vid2)].Value, index));
end;

{Add a new float item to an array (numeric)}
procedure scr_001D_add_array_item_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
  arrayIndex: cint;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Add item}
   arrayIndex := thread.GetVarInt(vloc, vid);
   GTA5_SetArrayLength(thread.script.globalArrays[arrayIndex].Value, GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value) + 1);
   GTA5_SetArrayItem(thread.script.globalArrays[arrayIndex].Value, GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value) - 1, PUINT32(@floatValue)^);
end;

{Remove an item from an array (numeric)}
procedure scr_001E_remove_array_item(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index: cint;
  arrayIndex, arrayLength, i: cint;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Remove item}
   arrayIndex := thread.GetVarInt(vloc, vid);
   arrayLength := GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value);
   if (index >= 0) and (index < arrayLength - 1) then
      begin
        for i := index to arrayLength - 2 do
            GTA5_SetArrayItem(thread.script.globalArrays[arrayIndex].Value, i, GTA5_GetArrayItem(thread.script.globalArrays[arrayIndex].Value, i + 1));
        GTA5_SetArrayLength(thread.script.globalArrays[arrayIndex].Value, GTA5_GetArrayLength(thread.script.globalArrays[arrayIndex].Value) - 1);
      end;
end;

{Get the length of an array (numeric)}
procedure scr_001F_get_array_length(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'array_var'}
   thread.GetVarId(vloc2, vid2);
   {Save the result}
   thread.SetVarInt(vloc1, vid1, cint(GTA5_GetArrayLength(thread.script.globalArrays[thread.GetVarInt(vloc2, vid2)].Value)));
end;

{Set array (numeric) length}
procedure scr_0020_set_array_length(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  newLength: cint;
begin
   {Param: 'array_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'new_length'}
   newLength := thread.GetNextParameterInt;
   {Change length}
   GTA5_SetArrayLength(thread.script.globalArrays[thread.GetVarInt(vloc, vid)].Value, integer(newLength));
end;

{Set an array (string) item}
procedure scr_0021_set_stringlist_item(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index: cint;
begin
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Param: 'string_value'}
   thread.script.globalStringLists[thread.GetVarInt(vloc, vid)].Value[index] := thread.GetNextParameterString;
end;

{Get an array (string) item}
procedure scr_0022_get_stringlist_item(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
  index: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc2, vid2);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Save result}
   thread.AllocateString(vloc1, vid1, thread.script.globalStringLists[thread.GetVarInt(vloc2, vid2)].Value[index]);
end;

{Add a new item to an array (string)}
procedure scr_0023_add_stringlist_item(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue: string;
  arrayIndex: cint;
begin
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {Add item}
   arrayIndex := thread.GetVarInt(vloc, vid);
   SetLength(thread.script.globalStringLists[arrayIndex].Value, Length(thread.script.globalStringLists[arrayIndex].Value) + 1);
   thread.script.globalStringLists[arrayIndex].Value[High(thread.script.globalStringLists[arrayIndex].Value)] := stringValue;
end;

{Remove an item from an array (string)}
procedure scr_0024_remove_stringlist_item(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index, arrayIndex, i: cint;
begin
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Remove item}
   arrayIndex := thread.GetVarInt(vloc, vid);
   if (index >= 0) and (index < Length(thread.script.globalStringLists[arrayIndex].Value)) then
      begin
        for i := index to High(thread.script.globalStringLists[arrayIndex].Value) do
            thread.script.globalStringLists[arrayIndex].Value[i] := thread.script.globalStringLists[arrayIndex].Value[i + 1];
        SetLength(thread.script.globalStringLists[arrayIndex].Value, Length(thread.script.globalStringLists[arrayIndex].Value) - 1);
      end;
end;

{Get the length of an array (string)}
procedure scr_0025_get_stringlist_count(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc2, vid2);
   {Save the result}
   thread.SetVarInt(vloc1, vid1, cint(Length(thread.script.globalStringLists[thread.GetVarInt(vloc2, vid2)].Value)));
end;

{Set array (string) length}
procedure scr_0026_set_stringlist_count(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  arrayIndex, newLength: cint;
  oldMaxIndex, i: integer;
begin
   {Param: 'stringlist_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'new_length'}
   newLength := thread.GetNextParameterInt;
   {Change length}
   arrayIndex := thread.GetVarInt(vloc, vid);
   oldMaxIndex := High(thread.script.globalStringlists[arrayIndex].Value);
   SetLength(thread.script.globalStringlists[arrayIndex].Value, newLength);
   if (newLength > (oldMaxIndex + 1)) then
     for i := oldMaxIndex + 1 to High(thread.script.globalStringlists[arrayIndex].Value) do
        thread.script.globalStringlists[arrayIndex].Value[i] := '';
end;

{v -= i}
procedure scr_0027_sub_var_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtFloat) then
     thread.SetVarInt(vloc, vid, cint(round(thread.GetVarFloat(vloc, vid))))
   else if (thread.GetVarType(vloc, vid) <> svtInt) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarInt(vloc, vid, 0);
     end;
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Subtract}
   thread.SetVarInt(vloc, vid, thread.GetVarInt(vloc, vid) - intValue);
end;

{v -= f}
procedure scr_0028_sub_var_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtInt) then
     thread.SetVarFloat(vloc, vid, thread.GetVarInt(vloc, vid) * 1.0)
   else if (thread.GetVarType(vloc, vid) <> svtFloat) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarFloat(vloc, vid, 0.0);
     end;
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Subtract}
   thread.SetVarFloat(vloc, vid, thread.GetVarFloat(vloc, vid) - floatValue);
end;

{v += i}
procedure scr_0029_add_var_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtFloat) then
     thread.SetVarInt(vloc, vid, cint(round(thread.GetVarFloat(vloc, vid))))
   else if (thread.GetVarType(vloc, vid) <> svtInt) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarInt(vloc, vid, 0);
     end;
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Add}
   thread.SetVarInt(vloc, vid, thread.GetVarInt(vloc, vid) + intValue);
end;

{v += f}
procedure scr_002A_add_var_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtInt) then
     thread.SetVarFloat(vloc, vid, thread.GetVarInt(vloc, vid) * 1.0)
   else if (thread.GetVarType(vloc, vid) <> svtFloat) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarFloat(vloc, vid, 0.0);
     end;
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Add}
   thread.SetVarFloat(vloc, vid, thread.GetVarFloat(vloc, vid) + floatValue);
end;

{v *= i}
procedure scr_002B_mul_var_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtFloat) then
     thread.SetVarInt(vloc, vid, cint(round(thread.GetVarFloat(vloc, vid))))
   else if (thread.GetVarType(vloc, vid) <> svtInt) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarInt(vloc, vid, 0);
     end;
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Multiply}
   thread.SetVarInt(vloc, vid, thread.GetVarInt(vloc, vid) * intValue);
end;

{v *= f}
procedure scr_002C_mul_var_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtInt) then
     thread.SetVarFloat(vloc, vid, thread.GetVarInt(vloc, vid) * 1.0)
   else if (thread.GetVarType(vloc, vid) <> svtFloat) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarFloat(vloc, vid, 0.0);
     end;
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Multiply}
   thread.SetVarFloat(vloc, vid, thread.GetVarFloat(vloc, vid) * floatValue);
end;

{v /= i}
procedure scr_002D_div_var_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtFloat) then
     thread.SetVarInt(vloc, vid, cint(round(thread.GetVarFloat(vloc, vid))))
   else if (thread.GetVarType(vloc, vid) <> svtInt) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarInt(vloc, vid, 0);
     end;
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Divide}
   if (intValue = 0) then
     thread.SetVarInt(vloc, vid, 0) // Division by zero will result in zero
   else
     thread.SetVarInt(vloc, vid, thread.GetVarInt(vloc, vid) div intValue);
end;

{v /= f}
procedure scr_002E_div_var_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   if (thread.GetVarType(vloc, vid) = svtInt) then
     thread.SetVarFloat(vloc, vid, thread.GetVarInt(vloc, vid) * 1.0)
   else if (thread.GetVarType(vloc, vid) <> svtFloat) then
     begin
       thread.ClearVariable(vloc, vid);
       thread.SetVarFloat(vloc, vid, 0.0);
     end;
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Divide}
   if (abs(floatValue) < abs(thread.script.floatEqualThreshold)) then
     thread.SetVarFloat(vloc, vid, 0.0) // Division by zero will result in zero
   else
     thread.SetVarFloat(vloc, vid, thread.GetVarFloat(vloc, vid) / floatValue);
end;

{Convert int to string}
procedure scr_002F_convert_int_to_string(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  intValue: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'int_value'}
   intValue := thread.GetNextParameterInt;
   {Convert and save result}
   thread.AllocateString(vloc, vid, IntToStr(intValue));
end;

{Convert float to string}
procedure scr_0030_convert_float_to_string(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  floatValue: cfloat;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'float_value'}
   floatValue := thread.GetNextParameterFloat;
   {Convert and save result}
   thread.AllocateString(vloc, vid, FloatToStr(floatValue, YosFormatSettings));
end;

{Concat strings}
procedure scr_0031_concat_string(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue: string;
  stringIndex: cint;
begin
   {Param: 'var'}
   thread.GetVarId(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {String concat}
   if (thread.GetVarType(vloc, vid) <> svtString) then
     thread.AllocateString(vloc, vid, '');
   if (thread.GetVarType(vloc, vid) = svtString) then // Was the string allocation successful? (if it was already a string, then we're still OK)
     begin
       stringIndex := thread.GetVarInt(vloc, vid);
       thread.script.globalStrings[stringIndex].Value := thread.script.globalStrings[stringIndex].Value + stringValue;
     end;
end;

{Get string length}
procedure scr_0032_get_string_length(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue: string;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {Save result}
   thread.SetVarInt(vloc, vid, cint(Length(stringValue)));
end;

{Get string char code at index}
procedure scr_0033_get_string_char_code_at_index(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue: string;
  index: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Save result}
   if (index <= 0) or (index > Length(stringValue)) then
     thread.SetVarInt(vloc, vid, 0) // If out of bounds, return zero
   else
      thread.SetVarInt(vloc, vid, cint(ord(stringValue[index])));
end;

{Substring}
procedure scr_0034_substring(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  stringValue, s: string;
  startIndex, endIndex: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'string_value'}
   stringValue := thread.GetNextParameterString;
   {Param: 'start_index'}
   startIndex := thread.GetNextParameterInt;
   {Param: 'start_index'}
   endIndex := thread.GetNextParameterInt;
   {Get substring and save result}
   if (startIndex <= 0) then
     startIndex := 1;
   if (endIndex > Length(stringValue)) then
     endIndex := Length(stringValue);
   if (startIndex > endIndex) then
     s := ''
   else
     s := System.Copy(stringValue, startIndex, endIndex - startIndex);
   thread.AllocateString(vloc, vid, s);
end;

{i1 > i2}
procedure scr_0035_cmp_gt_int_int(thread: TMissionThread; negateresult: boolean);
var
  i1, i2: cint;
begin
   {Param: 'i1'}
   i1 := thread.GetNextParameterInt;
   {Param: 'i2'}
   i2 := thread.GetNextParameterInt;
   {Store conditional result}
   thread.AddConditionalResult(i1 > i2, negateresult);
end;

{i > f}
procedure scr_0036_cmp_gt_int_float(thread: TMissionThread; negateresult: boolean);
var
  i: cint;
  f: cfloat;
begin
   {Param: 'i'}
   i := thread.GetNextParameterInt;
   {Param: 'f'}
   f := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult(i > f, negateresult);
end;

{f1 > f2}
procedure scr_0037_cmp_gt_float_float(thread: TMissionThread; negateresult: boolean);
var
  f1, f2: cfloat;
begin
   {Param: 'f1'}
   f1 := thread.GetNextParameterFloat;
   {Param: 'f2'}
   f2 := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult(f1 > f2, negateresult);
end;

{i1 >= i2}
procedure scr_0038_cmp_ge_int_int(thread: TMissionThread; negateresult: boolean);
var
  i1, i2: cint;
begin
   {Param: 'i1'}
   i1 := thread.GetNextParameterInt;
   {Param: 'i2'}
   i2 := thread.GetNextParameterInt;
   {Store conditional result}
   thread.AddConditionalResult(i1 >= i2, negateresult);
end;

{i >= f}
procedure scr_0039_cmp_ge_int_float(thread: TMissionThread; negateresult: boolean);
var
  i: cint;
  f: cfloat;
begin
   {Param: 'i'}
   i := thread.GetNextParameterInt;
   {Param: 'f'}
   f := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult(i >= (f + abs(thread.script.floatEqualThreshold)), negateresult);
end;

{f1 >= f2}
procedure scr_003A_cmp_ge_float_float(thread: TMissionThread; negateresult: boolean);
var
  f1, f2: cfloat;
begin
   {Param: 'f1'}
   f1 := thread.GetNextParameterFloat;
   {Param: 'f2'}
   f2 := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult(f1 >= (f2 + abs(thread.script.floatEqualThreshold)), negateresult);
end;

{i1 = i2}
procedure scr_003B_cmp_eq_int_int(thread: TMissionThread; negateresult: boolean);
var
  i1, i2: cint;
begin
   {Param: 'i1'}
   i1 := thread.GetNextParameterInt;
   {Param: 'i2'}
   i2 := thread.GetNextParameterInt;
   {Store conditional result}
   thread.AddConditionalResult(i1 = i2, negateresult);
end;

{i = f}
procedure scr_003C_cmp_eq_int_float(thread: TMissionThread; negateresult: boolean);
var
  i: cint;
  f: cfloat;
begin
   {Param: 'i'}
   i := thread.GetNextParameterInt;
   {Param: 'f'}
   f := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult((i <= (f + abs(thread.script.floatEqualThreshold))) and (i >= (f - abs(thread.script.floatEqualThreshold))), negateresult);
end;

{f1 = f2}
procedure scr_003D_cmp_eq_float_float(thread: TMissionThread; negateresult: boolean);
var
  f1, f2: cfloat;
begin
   {Param: 'f1'}
   f1 := thread.GetNextParameterFloat;
   {Param: 'f2'}
   f2 := thread.GetNextParameterFloat;
   {Store conditional result}
   thread.AddConditionalResult((f1 <= (f2 + abs(thread.script.floatEqualThreshold))) and (f1 >= (f2 - abs(thread.script.floatEqualThreshold))), negateresult);
end;

{Get var type}
procedure scr_003E_get_var_type(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'var'}
   thread.GetVarId(vloc2, vid2);
   {Save result}
   thread.SetVarInt(vloc1, vid1, cint(ord(thread.GetVarType(vloc2, vid2))));
end;

{Get number of free slots for a given var data type}
procedure scr_003F_get_managed_data_type_free_slots(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  datatype, i, result: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'datatype'}
   datatype := thread.GetNextParameterInt;
   {Calculate and save result}
   if (datatype < ord(Low(TVariableType))) or (datatype > ord(High(TVariableType))) then
     result := 0
   else
     begin
       result := 0;
       case TVariableType(byte(datatype)) of
            svtString:
              for i := 0 to High(thread.script.globalStrings) do
                  if not thread.script.globalStrings[i].Used  then
                    inc(result);
            svtVector:
              for i := 0 to High(thread.script.globalVectors) do
                  if not thread.script.globalVectors[i].Used  then
                    inc(result);
            svtArray:
              for i := 0 to High(thread.script.globalArrays) do
                  if not thread.script.globalArrays[i].Used  then
                    inc(result);
            svtStringArray:
              for i := 0 to High(thread.script.globalStringLists) do
                  if not thread.script.globalStringLists[i].Used  then
                    inc(result);
            svtBlip:
              for i := 0 to High(thread.script.globalBlips) do
                  if not thread.script.globalBlips[i].Used  then
                    inc(result);
            svtPickup:
              for i := 0 to High(thread.script.globalPickups) do
                  if not thread.script.globalPickups[i].Used  then
                    inc(result);
            svtSavedObject:
              for i := 0 to High(thread.script.globalSavedObjects) do
                  if not thread.script.globalSavedObjects[i].Used  then
                    inc(result);
            svtForbiddenCube:
              for i := 0 to High(thread.script.globalForbiddenCubes) do
                  if not thread.script.globalForbiddenCubes[i].Used  then
                    inc(result);
            svtCarGenerator:
              for i := 0 to High(thread.script.globalCarGenerators) do
                  if not thread.script.globalCarGenerators[i].Used  then
                    inc(result);
       end;
     end;
   thread.SetVarInt(vloc, vid, result);
end;

{Set float equal check threshold}
procedure scr_0040_set_float_epsilon(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'epsilon'}
   thread.script.floatEqualThreshold := thread.GetNextParameterFloat;
end;

{Get YOS version (major and minor)}
procedure scr_0041_get_script_engine_version(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save vars to array}
   for i := 0 to High(thread.script.globalArrays) do
       if not thread.script.globalArrays[i].Used then
          begin
            thread.script.globalArrays[i].Used := true;
            GTA5_SetArrayLength(thread.script.globalArrays[i].Value, 2);
            GTA5_SetArrayItem(thread.script.globalArrays[i].Value, 0, SCRIPT_MAJOR_VERSION); // YOS major version
            GTA5_SetArrayItem(thread.script.globalArrays[i].Value, 1, SCRIPT_MINOR_VERSION); // YOS minor version
            thread.SetVarData(vloc, svtArray, vid, PUINT32(@i)^);
            break;
          end;
end;

{Call GTA5 native}
procedure scr_0042_call_native(thread: TMissionThread; negateresult: boolean);
var
  nativeHash: UINT64;
  nativeParamCount: byte;
  i, jc: integer;
  vloc: TVariableLocation;
  ptype: TNativeParamType;
  u32: UINT32;
  c: char;
  s: string;
  vid: word;
begin
   {Param: 'native_hash'}
   nativeHash := thread.script.bytes.ReadQWord;
   inc(thread.position, 8); {--->>> 8b native hash code}
   {Param: 'native_param_count'}
   nativeParamCount := thread.script.bytes.ReadByte;
   inc(thread.position, 1); {--->>> 1b native param count}
   {Initialize native call, and fetch native params}
   nativeInit(nativeHash); // **** NATIVE INIT ****
   for i := 1 to nativeParamCount do
       begin
         {!!!! 1b Var Loc, 1b Native Param Type, 2b Var ID or Nb Literal !!!!
          Binary format differs from another opcodes' byte sequences!
          We need to fetch the parameters manually, without using the thread's helper functions!}
         vloc := TVariableLocation(thread.script.bytes.ReadByte);
         ptype := TNativeParamType(thread.script.bytes.ReadByte);
         inc(thread.position, 2); {--->>> 1b var loc, 1b native param type}

         // Direct value
         if (vloc = svlDirect) then
           begin
             // Direct 32-bit Int or Float
             if (ptype = nptNum32) then
               begin
                 u32 := thread.script.bytes.ReadDWord;
                 inc(thread.position, 4); {--->>> 4b direct number}
                 nativePush64(u32); // **** NATIVE PUSH ****
               end
             // Direct string literal
             // We use a custom reader here, not any helper function, because we need to know the exact jump count, and we also use a custom format!
             else if (ptype = nptString) then
               begin
                 s := '';
                 jc := 0;
                 repeat
                   c := chr(thread.script.bytes.ReadByte);
                   inc(jc);
                   if (c <> #0) then
                     s := s + c;
                 until c = #0;
                 inc(thread.position, jc); {--->>> Nb string literal}
                 nativePush64(UINT64(PChar(s))); // **** NATIVE PUSH ****
               end;
           end

         // Variable
         else
           begin
             vid := thread.script.bytes.ReadWord;
             u32 := thread.GetVarData(vloc, vid);
             inc(thread.position, 2); {--->>> 2b var id}
             case ptype of
                  // 32-bit numeric var, no pointer
                  nptVarNum32:
                    nativePush64(u32); // **** NATIVE PUSH ****
                  // 32-bit numeric var, pointer required
                  nptInt32Ptr, nptFloat32Ptr:
                    begin
                      if (vloc = svlLocal) then
                        nativePush64(UINT64(@thread.localVars[vid].Value)) // **** NATIVE PUSH ****
                      else
                        nativePush64(UINT64(@thread.script.globalVars[vid].Value)); // **** NATIVE PUSH ****
                    end;
                  // Vector3
                  nptVector:
                    nativePush64(UINT64(@thread.script.globalVectors[pcint(@u32)^].Value)); // **** NATIVE PUSH ****
                  // Array
                  nptArray:
                    nativePush64(UINT64(@thread.script.globalArrays[pcint(@u32)^].Value)); // **** NATIVE PUSH ****
                  // String var
                  nptStringPtr:
                    nativePush64(UINT64(PChar(thread.script.globalStrings[pcint(@u32)^].Value))); // **** NATIVE PUSH ****
             end;
           end;
       end;
   thread.nativeCallResult := nativeCall; // **** NATIVE CALL ****
end;

{Get GTA5 native result as int}
procedure scr_0043_get_native_result_int(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  result: cint;
begin
   {Important! Save result first!
    Otherwise, any subsequent native calls in 'ClearVariable' could invalidate the latest native result!}
   result := pcint(thread.nativeCallResult)^;
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save result}
   thread.SetVarInt(vloc, vid, result);
end;

{Get GTA5 native result as float}
procedure scr_0044_get_native_result_float(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  result: cfloat;
begin
   {Important! Save result first!
    Otherwise, any subsequent native calls in 'ClearVariable' could invalidate the latest native result!}
   result := PVector3(thread.nativeCallResult)^.x; // For some reason, the only way to retrieve a floating-point native result in Pascal is to treat it as a Vector3, and save its x coordinate!
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save result}
   thread.SetVarFloat(vloc, vid, result);
end;

{Get GTA5 native result as Vector3}
procedure scr_0045_get_native_result_vector3(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  result: Vector3;
  i: cint;
begin
   {Important! Save result first!
    Otherwise, any subsequent native calls in 'ClearVariable' could invalidate the latest native result!}
   result.x := PVector3(thread.nativeCallResult)^.x;
   result.y := PVector3(thread.nativeCallResult)^.y;
   result.z := PVector3(thread.nativeCallResult)^.z;
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save result}
   for i := 0 to High(thread.script.globalVectors) do
       if not thread.script.globalVectors[i].Used then
          begin
            thread.script.globalVectors[i].Used := true;
            thread.script.globalVectors[i].Value.x := result.x;
            thread.script.globalVectors[i].Value.y := result.y;
            thread.script.globalVectors[i].Value.z := result.z;
            thread.SetVarData(vloc, svtVector, vid, PUINT32(@i)^);
            break;
          end;
end;

{Get GTA5 native result as string}
procedure scr_0046_get_native_result_string(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  result: string;
begin
   {Important! Save result first!
    Otherwise, any subsequent native calls in 'ClearVariable' could invalidate the latest native result!}
   result := strpas(PChar(thread.nativeCallResult));
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save result}
   thread.AllocateString(vloc, vid, result);
end;

{Get internal player index}
procedure scr_0047_get_internal_player_index(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  result: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Save result}
   result := cint(thread.script.GetInternalPlayerIndex);
   thread.SetVarInt(vloc, vid, result);
end;

{Puts back the player at the default starting location, on a jet-ski}
procedure scr_0048_reset_player_location(thread: TMissionThread; negateresult: boolean);
begin
   ResetPlayerLocation;
end;

{Clear stored vehicle for current player}
procedure scr_0049_clear_stored_vehicle_for_player(thread: TMissionThread; negateresult: boolean);
var
  index: integer;
begin
   index := thread.script.GetInternalPlayerIndex;
   if (index <> -1) and thread.script.storedPlayerData[index].Used then // Safe to call
     begin
       thread.script.storedPlayerData[index].Data.IsDriving := false;
       ZeroMemory(@thread.script.storedPlayerData[index].Data.VehicleData, sizeof(TStoredVehicleData));
     end;
end;

{Clear stored location for current player}
procedure scr_004A_reset_stored_location_for_player(thread: TMissionThread; negateresult: boolean);
var
  index: integer;
  playerPed: Ped;
  pos: Vector3;
begin
   index := thread.script.GetInternalPlayerIndex;
   if (index <> -1) and thread.script.storedPlayerData[index].Used then // Safe to call
     begin
       playerPed := GET_PLAYER_PED(GET_PLAYER_INDEX);
       pos := GET_ENTITY_COORDS(playerPed, BOOL(0));
       thread.script.storedPlayerData[index].Data.X := pos.x;
       thread.script.storedPlayerData[index].Data.Y := pos.y;
       thread.script.storedPlayerData[index].Data.Z := pos.z;
       thread.script.storedPlayerData[index].Data.A := GET_ENTITY_HEADING(playerPed);
     end;
end;

{Gives stored player weapons to a ped (with a valid player model)}
procedure scr_004B_give_stored_player_weapons_for_ped(thread: TMissionThread; negateresult: boolean);
var
  actor: Ped;
  index, i, j: integer;
  data: ^TStoredPlayerData;
begin
   {Param: 'actor'}
   actor := thread.GetNextParameterInt;
   {Give weapons}
   index := thread.script.GetActorInternalPlayerIndex(actor);
   if (index <> -1) and thread.script.storedPlayerData[index].Used then // Safe to call
     begin
       REMOVE_ALL_PED_WEAPONS(actor, BOOL(0));
       data := @thread.script.storedPlayerData[index].Data;
       for i := 0 to High(data^.WeaponData.WeaponList) do
           begin
             while (HAS_WEAPON_ASSET_LOADED(data^.WeaponData.WeaponList[i].HashCode) = BOOL(0)) do
                   begin
                     REQUEST_WEAPON_ASSET(data^.WeaponData.WeaponList[i].HashCode, 31, 0);
                     GameScreen.DrawLoadingScreen;
                     ScriptHookVWait(0);
                   end;
             GIVE_WEAPON_TO_PED(actor, data^.WeaponData.WeaponList[i].HashCode, data^.WeaponData.WeaponList[i].Ammo, BOOL(0), BOOL(0));
             SET_PED_WEAPON_TINT_INDEX(actor, data^.WeaponData.WeaponList[i].HashCode, data^.WeaponData.WeaponList[i].TintIndex);
             for j := 0 to High(data^.WeaponData.WeaponList[i].Components) do
                 GIVE_WEAPON_COMPONENT_TO_PED(actor, data^.WeaponData.WeaponList[i].HashCode, data^.WeaponData.WeaponList[i].Components[j]);
             REMOVE_WEAPON_ASSET(data^.WeaponData.WeaponList[i].HashCode);
           end;
     end;
end;

{Store current player}
procedure scr_004C_store_player(thread: TMissionThread; negateresult: boolean);
var
  index: integer;
begin
   index := thread.script.GetInternalPlayerIndex;
   if (index <> -1) then // Safe to call
     begin
       thread.script.storedPlayerData[index].Used := true;
       thread.script.StorePlayer(thread.script.storedPlayerData[index].Data, GET_PLAYER_PED(GET_PLAYER_INDEX));
     end;
end;

{Restore current player}
procedure scr_004D_restore_player(thread: TMissionThread; negateresult: boolean);
var
  index: integer;
begin
   index := thread.script.GetInternalPlayerIndex;
   if (index <> -1) and thread.script.storedPlayerData[index].Used then // Safe to call
     thread.script.RestorePlayer(thread.script.storedPlayerData[index].Data, GET_PLAYER_PED(GET_PLAYER_INDEX));
end;

{Add hospital restart}
procedure scr_004E_add_hospital_restart(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, a: cfloat;
  h: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'angle'}
   a := thread.GetNextParameterFloat;
   {Register hospital restart}
   SetLength(thread.script.hospitalRestarts, Length(thread.script.hospitalRestarts) + 1);
   h := High(thread.script.hospitalRestarts);
   thread.script.hospitalRestarts[h].X := x;
   thread.script.hospitalRestarts[h].Y := y;
   thread.script.hospitalRestarts[h].Z := z;
   thread.script.hospitalRestarts[h].A := a;
end;

{Remove closest hospital restart}
procedure scr_004F_remove_closest_hospital_restart(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, xd, yd, zd, mindiff, diff: cfloat;
  index, i: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Find closest restart location, and remove it}
   mindiff := cfloat.MaxValue;
   diff := cfloat.MaxValue;
   index := -1;
   for i := 0 to High(thread.script.hospitalRestarts) do
       begin
         xd := x - thread.script.hospitalRestarts[i].X;
         yd := y - thread.script.hospitalRestarts[i].Y;
         zd := z - thread.script.hospitalRestarts[i].Z;
         diff := System.sqrt((xd * xd) + (yd * yd) + (zd * zd));
         if (diff < mindiff) then
           begin
             mindiff := diff;
             index := i;
           end;
       end;
   if (index > 0) then
     begin
       for i := index to High(thread.script.hospitalRestarts) - 1 do
           thread.script.hospitalRestarts[i] := thread.script.hospitalRestarts[i + 1];
       SetLength(thread.script.hospitalRestarts, Length(thread.script.hospitalRestarts) - 1);
     end;
end;

{Add police restart}
procedure scr_0050_add_police_restart(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, a: cfloat;
  h: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'angle'}
   a := thread.GetNextParameterFloat;
   {Register police restart}
   SetLength(thread.script.policeRestarts, Length(thread.script.policeRestarts) + 1);
   h := High(thread.script.policeRestarts);
   thread.script.policeRestarts[h].X := x;
   thread.script.policeRestarts[h].Y := y;
   thread.script.policeRestarts[h].Z := z;
   thread.script.policeRestarts[h].A := a;
end;

{Remove closest hospital restart}
procedure scr_0051_remove_closest_police_restart(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, xd, yd, zd, mindiff, diff: cfloat;
  index, i: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Find closest restart location, and remove it}
   mindiff := cfloat.MaxValue;
   diff := cfloat.MaxValue;
   index := -1;
   for i := 0 to High(thread.script.policeRestarts) do
       begin
         xd := x - thread.script.policeRestarts[i].X;
         yd := y - thread.script.policeRestarts[i].Y;
         zd := z - thread.script.policeRestarts[i].Z;
         diff := System.sqrt((xd * xd) + (yd * yd) + (zd * zd));
         if (diff < mindiff) then
           begin
             mindiff := diff;
             index := i;
           end;
       end;
   if (index > 0) then
     begin
       for i := index to High(thread.script.policeRestarts) - 1 do
           thread.script.policeRestarts[i] := thread.script.policeRestarts[i + 1];
       SetLength(thread.script.policeRestarts, Length(thread.script.policeRestarts) - 1);
     end;
end;

{Set an override restart location}
procedure scr_0052_set_override_restart(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, a: cfloat;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'angle'}
   a := thread.GetNextParameterFloat;
   {Register override restart}
   thread.script.overrideRestart.X := x;
   thread.script.overrideRestart.Y := y;
   thread.script.overrideRestart.Z := z;
   thread.script.overrideRestart.A := a;
   thread.script.useOverrideRestart := true;
end;

{Cancel override restart}
procedure scr_0053_cancel_override_restart(thread: TMissionThread; negateresult: boolean);
begin
   thread.script.useOverrideRestart := false;
end;

{Set mission flag}
procedure scr_0054_set_mission_flag(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'flag'}
   thread.script.isOnMission := (thread.GetNextParameterInt <> 0);
end;

{Get, if currently on mission}
procedure scr_0055_is_on_mission(thread: TMissionThread; negateresult: boolean);
begin
   thread.AddConditionalResult(thread.script.isOnMission, negateresult);
end;

{Get (once only), if the current thread was loaded from a savegame}
procedure scr_0056_is_saved_game_loaded(thread: TMissionThread; negateresult: boolean);
begin
   thread.AddConditionalResult(thread.isSavedGame, negateresult);
   thread.isSavedGame := false;
end;

{Show 'Save Game' screen}
procedure scr_0057_show_save_screen(thread: TMissionThread; negateresult: boolean);
var
  i: integer;
begin
   // Refresh all threads' status
   for i := 0 to High(thread.script.threads) do
       begin
         thread.script.threads[i].isSavedGame := false;
         thread.script.threads[i].isSaveSuccessful := false;
       end;

   // Show the save screen
   GameScreen.MenuMode := gmSaveGame;
   thread.waitTime := 0;
   thread.status := tstWaiting;
end;

{Set the latest mission name}
procedure scr_0058_set_latest_mission_name(thread: TMissionThread; negateresult: boolean);
begin
   {Param: 'name'}
   thread.script.lastMission := thread.GetNextParameterString;
end;

{Yield execution to ScriptHookV now!}
procedure scr_0059_process_game_events(thread: TMissionThread; negateresult: boolean);
begin
   ScriptHookVWait(0);
end;

{Draw a custom texture}
procedure scr_005A_draw_custom_texture(thread: TMissionThread; negateresult: boolean);
var
  filename: string;
  index, level, _time, r, g, b, a: cint;
  sx, sy, cx, cy, x, y, rotation, factor: cfloat;
begin
   {Param: 'filename'}
   filename := thread.GetNextParameterString;
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Param: 'level'}
   level := thread.GetNextParameterInt;
   {Param: 'time'}
   _time := thread.GetNextParameterInt;
   {Param: 'x_size'}
   sx := thread.GetNextParameterFloat;
   {Param: 'y_size'}
   sy := thread.GetNextParameterFloat;
   {Param: 'x_center'}
   cx := thread.GetNextParameterFloat;
   {Param: 'y_center'}
   cy := thread.GetNextParameterFloat;
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'rotation'}
   rotation := thread.GetNextParameterFloat;
   {Param: 'factor'}
   factor := thread.GetNextParameterFloat;
   {Param: 'r'}
   r := thread.GetNextParameterInt;
   {Param: 'g'}
   g := thread.GetNextParameterInt;
   {Param: 'b'}
   b := thread.GetNextParameterInt;
   {Param: 'a'}
   a := thread.GetNextParameterInt;
   {Draw texture}
   DrawCustomTexture(filename, index, level, _time, sx, sy, cx, cy, x, y, rotation, factor, byte(r), byte(g), byte(b), byte(a));
end;

{Print debug text}
procedure scr_005B_debug_text(thread: TMissionThread; negateresult: boolean);
var
  dbg_text: string;
begin
   {Param: 'text'}
   dbg_text := thread.GetNextParameterString;

   {Lof}
   Log('DEBUG_TEXT', Format('Thread: ''%s'' - Message: %s', [thread.scriptName, dbg_text]));
end;

{Conditional: FALSE}
procedure scr_005C_false(thread: TMissionThread; negateresult: boolean);
begin
   thread.AddConditionalResult(false, negateresult);
end;

{Conditional: TRUE}
procedure scr_005D_true(thread: TMissionThread; negateresult: boolean);
begin
   thread.AddConditionalResult(true, negateresult);
end;

{Clears all mission blips}
procedure scr_005E_clear_mission_blips(thread: TMissionThread; negateresult: boolean);
var
  i: integer;
  b, next: Blip;
begin
   for i := 0 to 16383 do
      begin
        b := GET_FIRST_BLIP_INFO_ID(cint(i));
        while (DOES_BLIP_EXIST(b) <> BOOL(0)) do
              begin
                next := GET_NEXT_BLIP_INFO_ID(cint(i));
                if not thread.script.IsManagedBlip(b) then
                  REMOVE_BLIP(@b);
                b := next;
              end;
      end;
end;

{Gets, if the Blip with the given handle is a YOS managed blip}
procedure scr_005F_is_blip_managed(thread: TMissionThread; negateresult: boolean);
var
  handle: Blip;
begin
   {Param: 'handle'}
   handle := thread.GetNextParameterInt;
   {Is managed?}
   thread.AddConditionalResult(thread.script.IsManagedBlip(handle), negateresult);
end;

{Creates a radius-type blip}
procedure scr_0060_add_blip_with_radius(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x, y, z, radius: cfloat;
  color, alpha, i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'radius'}
   radius := thread.GetNextParameterFloat;
   {Param: 'color'}
   color := thread.GetNextParameterInt;
   {Param: 'alpha'}
   alpha := thread.GetNextParameterInt;
   {Create the blip}
   for i := 0 to High(thread.script.globalBlips) do
      if not thread.script.globalBlips[i].Used then
        begin
          thread.script.globalBlips[i].Used := true;
          thread.script.globalBlips[i].IsMission := false;
          thread.script.globalBlips[i].IsShortRange := true;
          thread.script.globalBlips[i].Selectable := false;
          thread.script.globalBlips[i].HasRadius := true;
          thread.script.globalBlips[i].X := x;
          thread.script.globalBlips[i].Y := y;
          thread.script.globalBlips[i].Z := z;
          thread.script.globalBlips[i].Radius := radius;
          thread.script.globalBlips[i].Color := color;
          thread.script.globalBlips[i].Alpha := alpha;
          thread.script.CreateManagedBlip(thread.script.globalBlips[i]);
          thread.SetVarData(vloc, svtBlip, vid, PUINT32(@i)^);
          break;
        end;
end;

{Creates an icon-type blip}
procedure scr_0061_add_blip_with_icon(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x, y, z: cfloat;
  sprite, color, i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'sprite'}
   sprite := thread.GetNextParameterInt;
   {Param: 'color'}
   color := thread.GetNextParameterInt;
   {Create the blip}
   for i := 0 to High(thread.script.globalBlips) do
      if not thread.script.globalBlips[i].Used then
        begin
          thread.script.globalBlips[i].Used := true;
          thread.script.globalBlips[i].IsMission := false;
          thread.script.globalBlips[i].IsShortRange := true;
          thread.script.globalBlips[i].Selectable := true;
          thread.script.globalBlips[i].HasRadius := false;
          thread.script.globalBlips[i].X := x;
          thread.script.globalBlips[i].Y := y;
          thread.script.globalBlips[i].Z := z;
          thread.script.globalBlips[i].Sprite := sprite;
          thread.script.globalBlips[i].Color := color;
          thread.script.CreateManagedBlip(thread.script.globalBlips[i]);
          thread.SetVarData(vloc, svtBlip, vid, PUINT32(@i)^);
          break;
        end;
end;

{Set a blip selectable or unselectable}
procedure scr_0062_set_blip_selectable(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  selectable: boolean;
  blipIndex: cint;
begin
   {Param: 'blip_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'selectable'}
   selectable := (thread.GetNextParameterInt <> 0);
   {Set selectable or unselectable}
   blipIndex := thread.GetVarInt(vloc, vid);
   thread.script.globalBlips[blipIndex].Selectable := selectable;
   thread.script.UpdateBlip(thread.script.globalBlips[blipIndex]);
end;

{Set a blip to be visible or hidden during missions}
procedure scr_0063_set_blip_visible_during_missions(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  isMissionVisible: boolean;
  blipIndex: cint;
begin
   {Param: 'blip_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'visible'}
   isMissionVisible := (thread.GetNextParameterInt <> 0);
   {Set selectable or unselectable}
   blipIndex := thread.GetVarInt(vloc, vid);
   thread.script.globalBlips[blipIndex].IsMission := isMissionVisible;
   thread.script.UpdateBlip(thread.script.globalBlips[blipIndex]);
end;

{Set a blip to be visible or hidden ,when player gets too far}
procedure scr_0064_set_blip_visible_at_minimap_edges(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  isLongRange: boolean;
  blipIndex: cint;
begin
   {Param: 'blip_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'visible'}
   isLongRange := (thread.GetNextParameterInt <> 0);
   {Set selectable or unselectable}
   blipIndex := thread.GetVarInt(vloc, vid);
   thread.script.globalBlips[blipIndex].IsShortRange := not isLongRange;
   thread.script.UpdateBlip(thread.script.globalBlips[blipIndex]);
end;

{Set a blip's color}
procedure scr_0065_set_blip_color(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  color: cint;
  blipIndex: cint;
begin
   {Param: 'blip_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'color'}
   color := thread.GetNextParameterInt;
   {Set selectable or unselectable}
   blipIndex := thread.GetVarInt(vloc, vid);
   thread.script.globalBlips[blipIndex].Color := color;
   thread.script.UpdateBlip(thread.script.globalBlips[blipIndex]);
end;

{Clears all mission pickups}
procedure scr_0066_clear_mission_pickups(thread: TMissionThread; negateresult: boolean);
var
  i: integer;
  worldObjectCount: cint;
  worldObjectData: array [0..16383] of cint;
begin
   worldObjectCount := worldGetAllPickups(pcint(worldObjectData), Length(worldObjectData));
   for i := 0 to worldObjectCount - 1 do
      if not thread.script.IsManagedPickup(worldObjectData[i]) then
        begin
          if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) = BOOL(0)) then
            SET_ENTITY_AS_MISSION_ENTITY(worldObjectData[i], BOOL(1), BOOL(1));
          REMOVE_PICKUP(worldObjectData[i]);
        end;
end;

{Gets, if the Pickup with the given handle is a YOS managed pickup}
procedure scr_0067_is_pickup_managed(thread: TMissionThread; negateresult: boolean);
var
  handle: Pickup;
begin
   {Param: 'handle'}
   handle := thread.GetNextParameterInt;
   {Is managed?}
   thread.AddConditionalResult(thread.script.IsManagedPickup(handle), negateresult);
end;

{Creates a spinning pickup}
procedure scr_0068_create_pickup(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x, y, z: cfloat;
  pickupHash, modelHash: Hash;
  value, regenerationTime, i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'pickup_hash'}
   pickupHash := thread.GetNextParameterInt;
   {Param: 'model_hash'}
   modelHash := thread.GetNextParameterInt;
   {Param: 'value'}
   value := thread.GetNextParameterInt;
   {Param: 'regeneration_time'}
   regenerationTime := thread.GetNextParameterInt;
   {Create pickup}
   for i := 0 to High(thread.script.globalPickups) do
      if not thread.script.globalPickups[i].Used then
        begin
          thread.script.globalPickups[i].Used := true;
          thread.script.globalPickups[i].PickedUp := false;
          thread.script.globalPickups[i].HasRotation := false;
          thread.script.globalPickups[i].X := x;
          thread.script.globalPickups[i].Y := y;
          thread.script.globalPickups[i].Z := z;
          thread.script.globalPickups[i].PickupHash := pickupHash;
          thread.script.globalPickups[i].ModelHash := modelHash;
          thread.script.globalPickups[i].Value := value;
          thread.script.globalPickups[i].RgTime := regenerationTime;
          thread.script.globalPickups[i].RgTimeRem := 0;
          thread.script.CreateManagedPickup(thread.script.globalPickups[i]);
          thread.SetVarData(vloc, svtPickup, vid, PUINT32(@i)^);
          break;
        end;
end;

{Creates a static pickup}
procedure scr_0069_create_rotated_pickup(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x, y, z, xa, ya, za: cfloat;
  pickupHash, modelHash: Hash;
  value, regenerationTime, i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'xa'}
   xa := thread.GetNextParameterFloat;
   {Param: 'ya'}
   ya := thread.GetNextParameterFloat;
   {Param: 'za'}
   za := thread.GetNextParameterFloat;
   {Param: 'pickup_hash'}
   pickupHash := thread.GetNextParameterInt;
   {Param: 'model_hash'}
   modelHash := thread.GetNextParameterInt;
   {Param: 'value'}
   value := thread.GetNextParameterInt;
   {Param: 'regeneration_time'}
   regenerationTime := thread.GetNextParameterInt;
   {Create pickup}
   for i := 0 to High(thread.script.globalPickups) do
      if not thread.script.globalPickups[i].Used then
        begin
          thread.script.globalPickups[i].Used := true;
          thread.script.globalPickups[i].PickedUp := false;
          thread.script.globalPickups[i].HasRotation := true;
          thread.script.globalPickups[i].X := x;
          thread.script.globalPickups[i].Y := y;
          thread.script.globalPickups[i].Z := z;
          thread.script.globalPickups[i].XA := xa;
          thread.script.globalPickups[i].YA := ya;
          thread.script.globalPickups[i].ZA := za;
          thread.script.globalPickups[i].PickupHash := pickupHash;
          thread.script.globalPickups[i].ModelHash := modelHash;
          thread.script.globalPickups[i].Value := value;
          thread.script.globalPickups[i].RgTime := regenerationTime;
          thread.script.globalPickups[i].RgTimeRem := 0;
          thread.script.CreateManagedPickup(thread.script.globalPickups[i]);
          thread.SetVarData(vloc, svtPickup, vid, PUINT32(@i)^);
          break;
        end;
end;

{Gets, if the Pickup has been picked up, and didn't regenerate yet!}
procedure scr_006A_has_pickup_been_picked_up(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
begin
   {Param: 'pickup_var'}
   thread.GetVarId(vloc, vid);
   {Has been picked up}
   thread.AddConditionalResult(thread.script.globalPickups[thread.GetVarInt(vloc, vid)].PickedUp, negateresult);
end;

{Clears all mission objects}
procedure scr_006B_clear_mission_objects(thread: TMissionThread; negateresult: boolean);
var
  i: integer;
  worldObjectCount: cint;
  worldObjectData: array [0..16383] of cint;
begin
   worldObjectCount := worldGetAllObjects(pcint(worldObjectData), Length(worldObjectData));
   for i := 0 to worldObjectCount - 1 do
      if (GET_ENTITY_TYPE(worldObjectData[i]) = 3) and (not thread.script.IsManagedObject(worldObjectData[i])) then
        begin
          if (IS_ENTITY_A_MISSION_ENTITY(worldObjectData[i]) <> BOOL(0)) then
            DELETE_OBJECT(@worldObjectData[i]);
        end;
end;

{Gets, if the Object with the given handle is registered to be managed by YOS}
procedure scr_006C_is_object_managed(thread: TMissionThread; negateresult: boolean);
var
  handle: GTAObject;
begin
   {Param: 'handle'}
   handle := thread.GetNextParameterInt;
   {Is managed?}
   thread.AddConditionalResult(thread.script.IsManagedObject(handle), negateresult);
end;

{Registers an object to be managed by YOS}
procedure scr_006D_register_object_for_management(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  handle: GTAObject;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'handle'}
   handle := thread.GetNextParameterInt;
   {Let's sign up...}
   for i := 0 to High(thread.script.globalSavedObjects) do
      if not thread.script.globalSavedObjects[i].Used then
        begin
          thread.script.globalSavedObjects[i].Used := true;
          thread.script.globalSavedObjects[i]._handle := handle;
          thread.script.CreateManagedPickup(thread.script.globalPickups[i]);
          thread.SetVarData(vloc, svtSavedObject, vid, PUINT32(@i)^);
          break;
        end;
end;

{Gets a managed object's handle}
procedure scr_006E_get_managed_object_handle(thread: TMissionThread; negateresult: boolean);
var
  vloc1, vloc2: TVariableLocation;
  vid1, vid2: word;
begin
   {Param: 'result'}
   thread.GetVarId(vloc1, vid1);
   thread.ClearVariable(vloc1, vid1);
   {Param: 'object_var'}
   thread.GetVarId(vloc2, vid2);
   {Save result}
   thread.SetVarInt(vloc1, vid1, thread.script.globalSavedObjects[thread.GetVarInt(vloc2, vid2)]._handle);
end;

{Enables a map file}
procedure scr_006F_enable_map_file(thread: TMissionThread; negateresult: boolean);
var
  mapfile: string;
  index: integer;
begin
   {Param: 'mapfile'}
   mapfile := thread.GetNextParameterString;
   {Enable map}
   index := thread.script.disabledMaps.IndexOf(mapfile);
   if (index >= 0) then
     thread.script.disabledMaps.Delete(index);
   if (thread.script.enabledMaps.IndexOf(mapfile) < 0) then
     thread.script.enabledMaps.Add(mapfile);
   while (IS_IPL_ACTIVE(PChar(mapfile)) = BOOL(0)) do
         begin
           REQUEST_IPL(PChar(mapfile));
           GameScreen.DrawLoadingScreen;
           ScriptHookVWait(0);
         end;
end;

{Disables a map file}
procedure scr_0070_disable_map_file(thread: TMissionThread; negateresult: boolean);
var
  mapfile: string;
  index: integer;
begin
   {Param: 'mapfile'}
   mapfile := thread.GetNextParameterString;
   {Enable map}
   index := thread.script.enabledMaps.IndexOf(mapfile);
   if (index >= 0) then
     thread.script.enabledMaps.Delete(index);
   if (thread.script.disabledMaps.IndexOf(mapfile) < 0) then
     thread.script.disabledMaps.Add(mapfile);
   if (IS_IPL_ACTIVE(PChar(mapfile)) <> BOOL(0)) then
     REMOVE_IPL(PChar(mapfile));
end;

{Memorizes cars from a cube}
procedure scr_0071_store_cars_from_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2: cfloat;
  i, j: integer;
  vehicles: array [0..255] of Vehicle;
  vehCount: cint;
  playerVehicle: Vehicle;
  vpos: Vector3;
  clear: boolean;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Param: 'clear'}
   clear := (thread.GetNextParameterInt <> 0);
   {Forget all stored cars in this cube, and store new}
   for i := High(thread.script.storedVehicleData) downto 0 do
      if IsInCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2) then
        begin
          for j := i to High(thread.script.storedVehicleData) - 1 do
             thread.script.storedVehicleData[j] := thread.script.storedVehicleData[j + 1];
          SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) - 1);
        end;
   playerVehicle := GET_VEHICLE_PED_IS_USING(GET_PLAYER_PED(GET_PLAYER_INDEX));
   vehCount := worldGetAllVehicles(pcint(vehicles), Length(vehicles));
   for i := 0 to vehCount - 1 do
      if (vehicles[i] <> playerVehicle) then
        begin
          vpos := GET_ENTITY_COORDS(vehicles[i], BOOL(0));
          if IsInCube(vpos.x, vpos.y, vpos.z, x1, y1, z1, x2, y2, z2) then
            begin
              SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) + 1);
              thread.script.StoreVehicle(thread.script.storedVehicleData[High(thread.script.storedVehicleData)].VehicleData, vehicles[i]);
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].X := vpos.x;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].Y := vpos.y;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].Z := vpos.z;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].A := GET_ENTITY_HEADING(vehicles[i]);
              if clear then
                begin
                  SET_ENTITY_AS_MISSION_ENTITY(vehicles[i], BOOL(1), BOOL(1));
                  DELETE_VEHICLE(@vehicles[i]);
                end;
            end;
        end;
end;

{Spawns all memorized cars within a cube}
procedure scr_0072_spawn_stored_cars_in_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2: cfloat;
  i: integer;
  veh: Vehicle;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Spawn all stored cars in this cube}
   for i := 0 to High(thread.script.storedVehicleData) do
      if IsInCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2) then
        begin
          veh := thread.script.RestoreVehicle(thread.script.storedVehicleData[i]);
          SET_VEHICLE_AS_NO_LONGER_NEEDED(@veh);
        end;
end;

{Forgets all stored cars within a cube}
procedure scr_0073_forget_stored_cars_in_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2: cfloat;
  i, j: integer;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Forget all stored cars in this cube}
   for i := High(thread.script.storedVehicleData) downto 0 do
      if IsInCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2) then
        begin
          for j := i to High(thread.script.storedVehicleData) - 1 do
             thread.script.storedVehicleData[j] := thread.script.storedVehicleData[j + 1];
          SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) - 1);
        end;
end;

{Reset timer}
procedure scr_0074_reset_timer(thread: TMissionThread; negateresult: boolean);
var
  index: cint;
begin
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Reset timer, if applcable}
   if (index >= 0) and (index < Length(thread.timers)) then
     thread.timers[index] := 0.0;
end;

{Get timer}
procedure scr_0075_get_timer(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  index: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'index'}
   index := thread.GetNextParameterInt;
   {Save result}
   if (index >= 0) and (index < Length(thread.timers)) then
     thread.SetVarFloat(vloc, vid, thread.timers[index])
   else
     thread.SetVarFloat(vloc, vid, 0.0);
end;

{Call a subroutine (label) with automatic wasted-busted return option}
procedure scr_0076_gosub_with_auto_wasted_busted_return(thread: TMissionThread; negateresult: boolean);
var
  returnpos: INT64;
begin
   {Save gosub data}
   returnpos := thread.position + 8; {--->>> 8b label id}

   {Save normal gosub data}
   thread.gosubReturn[thread.gosubLevel] := returnpos;

   {Save WB gosub data}
   thread.wbGosubActive := true;
   thread.wbGosubLevel := thread.gosubLevel;
   thread.wbGosubReturn := returnpos;

   {Param: 'label'; perform normal gosub}
   inc(thread.gosubLevel);
   thread.position := thread.script.bytes.ReadQWord - thread.offset;
end;

{Cancel an automatic wasted-busted return option for thread}
procedure scr_0077_cancel_auto_wasted_busted_return(thread: TMissionThread; negateresult: boolean);
begin
   thread.wbGosubActive := false;
end;

{Create a forbidden for cars cube}
procedure scr_0078_create_forbidden_for_cars_cube(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x1, y1, z1, x2, y2, z2: cfloat;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Save result and apply gameplay effect}
   for i := 0 to High(thread.script.globalForbiddenCubes) do
       if not thread.script.globalForbiddenCubes[i].Used then
          begin
            thread.script.globalForbiddenCubes[i].Used := true;
            thread.script.globalForbiddenCubes[i].CubeType := 1;
            thread.script.globalForbiddenCubes[i].X1 := x1;
            thread.script.globalForbiddenCubes[i].Y1 := y1;
            thread.script.globalForbiddenCubes[i].Z1 := z1;
            thread.script.globalForbiddenCubes[i].X2 := x2;
            thread.script.globalForbiddenCubes[i].Y2 := y2;
            thread.script.globalForbiddenCubes[i].Z2 := z2;
            SET_ROADS_IN_AREA(x1, y1, z1, x2, y2, z2, BOOL(0), BOOL(1));
            thread.SetVarData(vloc, svtForbiddenCube, vid, PUINT32(@i)^);
            break;
          end;
end;

procedure scr_0079_create_forbidden_for_peds_cube(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x1, y1, z1, x2, y2, z2: cfloat;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Save result and apply gameplay effect}
   for i := 0 to High(thread.script.globalForbiddenCubes) do
       if not thread.script.globalForbiddenCubes[i].Used then
          begin
            thread.script.globalForbiddenCubes[i].Used := true;
            thread.script.globalForbiddenCubes[i].CubeType := 0;
            thread.script.globalForbiddenCubes[i].X1 := x1;
            thread.script.globalForbiddenCubes[i].Y1 := y1;
            thread.script.globalForbiddenCubes[i].Z1 := z1;
            thread.script.globalForbiddenCubes[i].X2 := x2;
            thread.script.globalForbiddenCubes[i].Y2 := y2;
            thread.script.globalForbiddenCubes[i].Z2 := z2;
            SET_PED_PATHS_IN_AREA(x1, y1, z1, x2, y2, z2, BOOL(0), 0);
            thread.SetVarData(vloc, svtForbiddenCube, vid, PUINT32(@i)^);
            break;
          end;
end;

{Gets, if the Car Generator with the given handle is a YOS managed car generator}
procedure scr_007A_is_car_generator_managed(thread: TMissionThread; negateresult: boolean);
var
  handle: cint;
  i: integer;
  result: boolean;
begin
   {Param: 'handle'}
   handle := thread.GetNextParameterInt;
   {Is managed?}
   result := false;
   for i := 0 to High(thread.script.globalCarGenerators) do
      if (thread.script.globalCarGenerators[i]._handle <> 0) and (handle = thread.script.globalCarGenerators[i]._handle) then
        begin
          result := true;
          break;
        end;
   thread.AddConditionalResult(result, negateresult);
end;

procedure scr_007B_create_car_generator(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x, y, z, a: cfloat;
  model: Hash;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'a'}
   a := thread.GetNextParameterFloat;
   {Param: 'model'}
   model := thread.GetNextParameterInt;
   {Save result and apply gameplay effect}
   for i := 0 to High(thread.script.globalCarGenerators) do
       if not thread.script.globalCarGenerators[i].Used then
          begin
            thread.script.globalCarGenerators[i].Used := true;
            thread.script.globalCarGenerators[i].IsActive := false;
            thread.script.globalCarGenerators[i].Model := model;
            thread.script.globalCarGenerators[i].X := x;
            thread.script.globalCarGenerators[i].Y := y;
            thread.script.globalCarGenerators[i].Z := z;
            thread.script.globalCarGenerators[i].A := a;
            thread.script.globalCarGenerators[i]._handle := 0;
            thread.SetVarData(vloc, svtCarGenerator, vid, PUINT32(@i)^);
            break;
          end;
end;

procedure scr_007C_set_car_generator_active(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  activate: boolean;
  i: cint;
begin
   {Param: 'cargenerator_var'}
   thread.GetVarId(vloc, vid);
   {Param: 'activate'}
   activate := (thread.GetNextParameterInt <> 0);
   {Activate or deactivate}
   i := thread.GetVarInt(vloc, vid);
   if thread.script.globalCarGenerators[i].IsActive and (not activate) then
     begin
       thread.script.globalCarGenerators[i].IsActive := false;
       if (thread.script.globalCarGenerators[i]._handle <> 0) and (DOES_SCRIPT_VEHICLE_GENERATOR_EXIST(thread.script.globalCarGenerators[i]._handle) <> BOOL(0)) then
         DELETE_SCRIPT_VEHICLE_GENERATOR(thread.script.globalCarGenerators[i]._handle);
       thread.script.globalCarGenerators[i]._handle := 0;
     end
   else if (not thread.script.globalCarGenerators[i].IsActive) and activate then
     begin
       thread.script.globalCarGenerators[i].IsActive := true;
       thread.script.globalCarGenerators[i]._handle := CREATE_SCRIPT_VEHICLE_GENERATOR(thread.script.globalCarGenerators[i].X, thread.script.globalCarGenerators[i].Y, thread.script.globalCarGenerators[i].Z, thread.script.globalCarGenerators[i].A, 5.0, 3.0, thread.script.globalCarGenerators[i].Model, -1, -1, -1, -1, BOOL(1), BOOL(0), BOOL(0), BOOL(0), BOOL(1), -1);
     end;
end;

{Memorizes cars from an angled cube}
procedure scr_007D_store_cars_from_angled_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2, a: cfloat;
  i, j: integer;
  vehicles: array [0..255] of Vehicle;
  vehCount: cint;
  playerVehicle: Vehicle;
  vpos: Vector3;
  clear: boolean;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Param: 'a'}
   a := thread.GetNextParameterFloat;
   {Param: 'clear'}
   clear := (thread.GetNextParameterInt <> 0);
   {Forget all stored cars in this cube, and store new}
   for i := High(thread.script.storedVehicleData) downto 0 do
      if IsInAngledCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2, a) then
        begin
          for j := i to High(thread.script.storedVehicleData) - 1 do
             thread.script.storedVehicleData[j] := thread.script.storedVehicleData[j + 1];
          SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) - 1);
        end;
   playerVehicle := GET_VEHICLE_PED_IS_USING(GET_PLAYER_PED(GET_PLAYER_INDEX));
   vehCount := worldGetAllVehicles(pcint(vehicles), Length(vehicles));
   for i := 0 to vehCount - 1 do
      if (vehicles[i] <> playerVehicle) then
        begin
          vpos := GET_ENTITY_COORDS(vehicles[i], BOOL(0));
          if IsInAngledCube(vpos.x, vpos.y, vpos.z, x1, y1, z1, x2, y2, z2, a) then
            begin
              SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) + 1);
              thread.script.StoreVehicle(thread.script.storedVehicleData[High(thread.script.storedVehicleData)].VehicleData, vehicles[i]);
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].X := vpos.x;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].Y := vpos.y;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].Z := vpos.z;
              thread.script.storedVehicleData[High(thread.script.storedVehicleData)].A := GET_ENTITY_HEADING(vehicles[i]);
              if clear then
                begin
                  SET_ENTITY_AS_MISSION_ENTITY(vehicles[i], BOOL(1), BOOL(1));
                  DELETE_VEHICLE(@vehicles[i]);
                end;
            end;
        end;
end;

{Spawns all memorized cars within an angled cube}
procedure scr_007E_spawn_stored_cars_in_angled_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2, a: cfloat;
  i: integer;
  veh: Vehicle;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Param: 'a'}
   a := thread.GetNextParameterFloat;
   {Spawn all stored cars in this cube}
   for i := 0 to High(thread.script.storedVehicleData) do
      if IsInAngledCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2, a) then
        begin
          veh := thread.script.RestoreVehicle(thread.script.storedVehicleData[i]);
          SET_VEHICLE_AS_NO_LONGER_NEEDED(@veh);
        end;
end;

{Forgets all stored cars within an angled cube}
procedure scr_007F_forget_stored_cars_in_angled_cube(thread: TMissionThread; negateresult: boolean);
var
  x1, y1, z1, x2, y2, z2, a: cfloat;
  i, j: integer;
begin
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Param: 'a'}
   a := thread.GetNextParameterFloat;
   {Forget all stored cars in this cube}
   for i := High(thread.script.storedVehicleData) downto 0 do
      if IsInAngledCube(thread.script.storedVehicleData[i].x, thread.script.storedVehicleData[i].y, thread.script.storedVehicleData[i].z, x1, y1, z1, x2, y2, z2, a) then
        begin
          for j := i to High(thread.script.storedVehicleData) - 1 do
             thread.script.storedVehicleData[j] := thread.script.storedVehicleData[j + 1];
          SetLength(thread.script.storedVehicleData, Length(thread.script.storedVehicleData) - 1);
        end;
end;

{Get (once only), if the current thread has been recently saved}
procedure scr_0080_has_game_been_saved(thread: TMissionThread; negateresult: boolean);
begin
   thread.AddConditionalResult(thread.isSaveSuccessful, negateresult);
   thread.isSaveSuccessful := false;
end;

{Perform a Wasted-Busted return, if possible}
procedure scr_0081_perform_wasted_busted_return(thread: TMissionThread; negateresult: boolean);
begin
   if thread.wbGosubActive then
     begin
       thread.wbGosubActive := false;
       thread.gosubLevel := thread.wbGosubLevel;
       thread.position := thread.wbGosubReturn;
       thread.waitTime := 0;
     end;
end;

{Create a forbidden for scenarios cube}
procedure scr_0082_create_forbidden_for_scenarios_cube(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  x1, y1, z1, x2, y2, z2: cfloat;
  i: cint;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'x1'}
   x1 := thread.GetNextParameterFloat;
   {Param: 'y1'}
   y1 := thread.GetNextParameterFloat;
   {Param: 'z1'}
   z1 := thread.GetNextParameterFloat;
   {Param: 'x2'}
   x2 := thread.GetNextParameterFloat;
   {Param: 'y2'}
   y2 := thread.GetNextParameterFloat;
   {Param: 'z2'}
   z2 := thread.GetNextParameterFloat;
   {Save result and apply gameplay effect}
   for i := 0 to High(thread.script.globalForbiddenCubes) do
       if not thread.script.globalForbiddenCubes[i].Used then
          begin
            thread.script.globalForbiddenCubes[i].Used := true;
            thread.script.globalForbiddenCubes[i].CubeType := 0;
            thread.script.globalForbiddenCubes[i].X1 := x1;
            thread.script.globalForbiddenCubes[i].Y1 := y1;
            thread.script.globalForbiddenCubes[i].Z1 := z1;
            thread.script.globalForbiddenCubes[i].X2 := x2;
            thread.script.globalForbiddenCubes[i].Y2 := y2;
            thread.script.globalForbiddenCubes[i].Z2 := z2;
            thread.script.globalForbiddenCubes[i]._handle := ADD_SCENARIO_BLOCKING_AREA(X1, Y1, Z1, X2, Y2, Z2, BOOL(0), BOOL(1), BOOL(1), BOOL(1), 1);
            thread.SetVarData(vloc, svtForbiddenCube, vid, PUINT32(@i)^);
            break;
          end;
end;

{Swaps map models in an area}
procedure scr_0083_swap_map_models_in_area(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, radius: cfloat;
  oldHash, newHash: Hash;
  i, found: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'radius'}
   radius := thread.GetNextParameterFloat;
   {Param: 'orig_hash'}
   oldHash := thread.GetNextParameterInt;
   {Param: 'new_hash'}
   newHash := thread.GetNextParameterInt;
   {Replace models}
   found := -1;
   for i := 0 to High(thread.script.modelSwapRecords) do
      begin
        if (thread.script.modelSwapRecords[i].OrigHash = oldHash) and (thread.script.modelSwapRecords[i].NewHash = newHash) then
          begin
            if (VDIST(x, y, z, thread.script.modelSwapRecords[i].X, thread.script.modelSwapRecords[i].Y, thread.script.modelSwapRecords[i].Z) < radius) then
              begin
                found := i;
                break; // Found an already active model swap in the vicinity
              end;
          end;
      end;
   if (found < 0) then
     begin
       SetLength(thread.script.modelSwapRecords, Length(thread.script.modelSwapRecords) + 1);
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].X := x;
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].Y := y;
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].Z := z;
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].Radius := radius;
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].OrigHash := oldHash;
       thread.script.modelSwapRecords[High(thread.script.modelSwapRecords)].NewHash := newHash;
       CREATE_MODEL_SWAP(x, y, z, radius, oldHash, newHash, BOOL(1));
     end;
end;

{Restores swapped map models in an area}
procedure scr_0084_restore_map_models_in_area(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, radius: cfloat;
  oldHash, currHash: Hash;
  i, j: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'radius'}
   radius := thread.GetNextParameterFloat;
   {Param: 'orig_hash'}
   oldHash := thread.GetNextParameterInt;
   {Param: 'current_hash'}
   currHash := thread.GetNextParameterInt;
   {Restore models}
   for i := 0 to High(thread.script.modelSwapRecords) do
      begin
        if (thread.script.modelSwapRecords[i].OrigHash = oldHash) and (thread.script.modelSwapRecords[i].NewHash = currHash) then
          begin
            if (VDIST(x, y, z, thread.script.modelSwapRecords[i].X, thread.script.modelSwapRecords[i].Y, thread.script.modelSwapRecords[i].Z) < radius) then
              begin
                REMOVE_MODEL_SWAP(x, y, z, radius, currHash, oldHash, BOOL(0));
                for j := i + 1 to High(thread.script.modelSwapRecords) do
                    thread.script.modelSwapRecords[i] := thread.script.modelSwapRecords[i + 1];
                SetLength(thread.script.modelSwapRecords, Length(thread.script.modelSwapRecords) - 1);
              end;
          end;
      end;
end;

{Disables map models in an area}
procedure scr_0085_disable_map_models_in_area(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, radius: cfloat;
  modelHash: Hash;
  i, found: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'radius'}
   radius := thread.GetNextParameterFloat;
   {Param: 'model_hash'}
   modelHash := thread.GetNextParameterInt;
   {Disable models}
   found := -1;
   for i := 0 to High(thread.script.modelHideRecords) do
      if (thread.script.modelHideRecords[i].ModelHash = modelHash) and (VDIST(x, y, z, thread.script.modelHideRecords[i].X, thread.script.modelHideRecords[i].Y, thread.script.modelHideRecords[i].Z) < radius) then
        begin
          found := i;
          break; // Found an already active model hide in the vicinity
        end;
   if (found < 0) then
     begin
       SetLength(thread.script.modelHideRecords, Length(thread.script.modelHideRecords) + 1);
       thread.script.modelHideRecords[High(thread.script.modelHideRecords)].X := x;
       thread.script.modelHideRecords[High(thread.script.modelHideRecords)].Y := y;
       thread.script.modelHideRecords[High(thread.script.modelHideRecords)].Z := z;
       thread.script.modelHideRecords[High(thread.script.modelHideRecords)].Radius := radius;
       thread.script.modelHideRecords[High(thread.script.modelHideRecords)].ModelHash := modelHash;
       CREATE_MODEL_HIDE(x, y, z, radius, modelHash, BOOL(1));
     end;
end;

{Reenables map models in an area}
procedure scr_0086_reenable_map_models_in_area(thread: TMissionThread; negateresult: boolean);
var
  x, y, z, radius: cfloat;
  modelHash: Hash;
  i, j: integer;
begin
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'radius'}
   radius := thread.GetNextParameterFloat;
   {Param: 'model_hash'}
   modelHash := thread.GetNextParameterInt;
   {Reenable models}
   for i := 0 to High(thread.script.modelHideRecords) do
      if (thread.script.modelHideRecords[i].ModelHash = modelHash) and (VDIST(x, y, z, thread.script.modelHideRecords[i].X, thread.script.modelHideRecords[i].Y, thread.script.modelHideRecords[i].Z) < radius) then
        begin
          REMOVE_MODEL_HIDE(x, y, z, radius, modelHash, BOOL(0)); // NOTE: signature requires 'Any (int)', but params are floats
          for j := i + 1 to High(thread.script.modelHideRecords) do
             thread.script.modelHideRecords[i] := thread.script.modelHideRecords[i + 1];
          SetLength(thread.script.modelHideRecords, Length(thread.script.modelHideRecords) - 1);
        end;
end;

{Stores a single vehicle in the specified slot}
procedure scr_0087_store_single_vehicle(thread: TMissionThread; negateresult: boolean);
var
  veh: Vehicle;
  slot: cint;
begin
   {Param: 'vehicle'}
   veh := thread.GetNextParameterInt;
   {Param: 'slot'}
   slot := thread.GetNextParameterInt;
   {Store vehicle}
   if (slot >= 0) and (slot < Length(thread.script.storedVehicleSlots)) and (GET_ENTITY_TYPE(veh) = 2) then
     begin
       thread.script.storedVehicleSlots[slot].IsStored := true;
       thread.script.StoreVehicle(thread.script.storedVehicleSlots[slot].VehicleData, veh);
     end;
end;

{Restores a single vehicle in the specified slot}
procedure scr_0088_spawn_single_vehicle(thread: TMissionThread; negateresult: boolean);
var
  vloc: TVariableLocation;
  vid: word;
  slot: cint;
  x, y, z, a: cfloat;
  data: TVehiclePersistenceData;
  veh: Vehicle;
begin
   {Param: 'result'}
   thread.GetVarId(vloc, vid);
   thread.ClearVariable(vloc, vid);
   {Param: 'slot'}
   slot := thread.GetNextParameterInt;
   {Param: 'x'}
   x := thread.GetNextParameterFloat;
   {Param: 'y'}
   y := thread.GetNextParameterFloat;
   {Param: 'z'}
   z := thread.GetNextParameterFloat;
   {Param: 'a'}
   a := thread.GetNextParameterFloat;
   {Store vehicle}
   if (slot >= 0) and (slot < Length(thread.script.storedVehicleSlots)) and thread.script.storedVehicleSlots[slot].IsStored then
     begin
       data.VehicleData := thread.script.storedVehicleSlots[slot].VehicleData;
       data.X := x;
       data.Y := y;
       data.Z := z;
       data.A := a;
       veh := thread.script.RestoreVehicle(data);
       thread.SetVarInt(vloc, vid, veh);
     end;
end;

{Forgets a single vehicle in the specified slot}
procedure scr_0089_forget_single_vehicle(thread: TMissionThread; negateresult: boolean);
var
  slot: cint;
begin
   {Param: 'slot'}
   slot := thread.GetNextParameterInt;
   {Forget vehicle}
   if (slot >= 0) and (slot < Length(thread.script.storedVehicleSlots)) then
     thread.script.storedVehicleSlots[slot].IsStored := false;
end;

{Returns, if the specified slot has a single vehicle stored in}
procedure scr_008A_is_single_vehicle_stored(thread: TMissionThread; negateresult: boolean);
var
  slot: cint;
begin
   {Param: 'slot'}
   slot := thread.GetNextParameterInt;
   {Forget vehicle}
   if (slot >= 0) and (slot < Length(thread.script.storedVehicleSlots)) then
     thread.AddConditionalResult(thread.script.storedVehicleSlots[slot].IsStored, negateresult)
   else
     thread.AddConditionalResult(false, negateresult);
end;


{Changes the specified ped's outfit (with a valid player model)}
procedure scr_008B_dress_up_ped_as_protagonist(thread: TMissionThread; negateresult: boolean);
var
  actor: Ped;
  index, i, j: integer;
  data: ^TStoredPlayerData;
begin
   {Param: 'actor'}
   actor := thread.GetNextParameterInt;
   {Dress up ped}
   index := thread.script.GetActorInternalPlayerIndex(actor);
   if (index <> -1) and thread.script.storedPlayerData[index].Used then // Safe to call
     begin
      data := @thread.script.storedPlayerData[index].Data;
      for i := 0 to 11 do
          if (i <> 7) then // Drawable 7 is unused!
            SET_PED_COMPONENT_VARIATION(actor, cint(i), data^.OutfitData.Drawables[i].DrawableVar, data^.OutfitData.Drawables[i].TextureVar, data^.OutfitData.Drawables[i].PaletteVar);
      for i := 0 to 2 do
         SET_PED_PROP_INDEX(actor, cint(i), data^.OutfitData.Props[i].PropIndex, data^.OutfitData.Props[i].PropTexture, BOOL(0), 1);
     end;
end;

{Get GTA5 native result as conditional}
procedure scr_008C_get_native_result_conditional(thread: TMissionThread; negateresult: boolean);
var
  result: BOOL;
begin
   {Important! Save result first!}
   result := PBOOL(thread.nativeCallResult)^;
   {Save result}
   thread.AddConditionalResult(result <> BOOL(0), negateresult);
end;

{OpCode handler table}
var
  OpCodes: array [$0000..$008C] of TOpCodeHandler =
    (
     @scr_0000_nop,
     @scr_0001_wait,
     @scr_0002_jump,
     @scr_0003_gosub,
     @scr_0004_return,
     @scr_0005_jump_if_false,
     @scr_0006_if,
     @scr_0007_end_thread,
     @scr_0008_end_named_thread,
     @scr_0009_create_thread,
     @scr_000A_set_thread_name,
     @scr_000B_reset_variable,
     @scr_000C_set_var_int,
     @scr_000D_set_var_float,
     @scr_000E_set_var_string,
     @scr_000F_set_var_vector3,
     @scr_0010_set_var_array,
     @scr_0011_set_var_stringlist,
     @scr_0012_set_vector3_x,
     @scr_0013_set_vector3_y,
     @scr_0014_set_vector3_z,
     @scr_0015_get_vector3_x,
     @scr_0016_get_vector3_y,
     @scr_0017_get_vector3_z,
     @scr_0018_set_array_item_int,
     @scr_0019_get_array_item_int,
     @scr_001A_add_array_item_int,
     @scr_001B_set_array_item_float,
     @scr_001C_get_array_item_float,
     @scr_001D_add_array_item_float,
     @scr_001E_remove_array_item,
     @scr_001F_get_array_length,
     @scr_0020_set_array_length,
     @scr_0021_set_stringlist_item,
     @scr_0022_get_stringlist_item,
     @scr_0023_add_stringlist_item,
     @scr_0024_remove_stringlist_item,
     @scr_0025_get_stringlist_count,
     @scr_0026_set_stringlist_count,
     @scr_0027_sub_var_int,
     @scr_0028_sub_var_float,
     @scr_0029_add_var_int,
     @scr_002A_add_var_float,
     @scr_002B_mul_var_int,
     @scr_002C_mul_var_float,
     @scr_002D_div_var_int,
     @scr_002E_div_var_float,
     @scr_002F_convert_int_to_string,
     @scr_0030_convert_float_to_string,
     @scr_0031_concat_string,
     @scr_0032_get_string_length,
     @scr_0033_get_string_char_code_at_index,
     @scr_0034_substring,
     @scr_0035_cmp_gt_int_int,
     @scr_0036_cmp_gt_int_float,
     @scr_0037_cmp_gt_float_float,
     @scr_0038_cmp_ge_int_int,
     @scr_0039_cmp_ge_int_float,
     @scr_003A_cmp_ge_float_float,
     @scr_003B_cmp_eq_int_int,
     @scr_003C_cmp_eq_int_float,
     @scr_003D_cmp_eq_float_float,
     @scr_003E_get_var_type,
     @scr_003F_get_managed_data_type_free_slots,
     @scr_0040_set_float_epsilon,
     @scr_0041_get_script_engine_version,
     @scr_0042_call_native,
     @scr_0043_get_native_result_int,
     @scr_0044_get_native_result_float,
     @scr_0045_get_native_result_vector3,
     @scr_0046_get_native_result_string,
     @scr_0047_get_internal_player_index,
     @scr_0048_reset_player_location,
     @scr_0049_clear_stored_vehicle_for_player,
     @scr_004A_reset_stored_location_for_player,
     @scr_004B_give_stored_player_weapons_for_ped,
     @scr_004C_store_player,
     @scr_004D_restore_player,
     @scr_004E_add_hospital_restart,
     @scr_004F_remove_closest_hospital_restart,
     @scr_0050_add_police_restart,
     @scr_0051_remove_closest_police_restart,
     @scr_0052_set_override_restart,
     @scr_0053_cancel_override_restart,
     @scr_0054_set_mission_flag,
     @scr_0055_is_on_mission,
     @scr_0056_is_saved_game_loaded,
     @scr_0057_show_save_screen,
     @scr_0058_set_latest_mission_name,
     @scr_0059_process_game_events,
     @scr_005A_draw_custom_texture,
     @scr_005B_debug_text,
     @scr_005C_false,
     @scr_005D_true,
     @scr_005E_clear_mission_blips,
     @scr_005F_is_blip_managed,
     @scr_0060_add_blip_with_radius,
     @scr_0061_add_blip_with_icon,
     @scr_0062_set_blip_selectable,
     @scr_0063_set_blip_visible_during_missions,
     @scr_0064_set_blip_visible_at_minimap_edges,
     @scr_0065_set_blip_color,
     @scr_0066_clear_mission_pickups,
     @scr_0067_is_pickup_managed,
     @scr_0068_create_pickup,
     @scr_0069_create_rotated_pickup,
     @scr_006A_has_pickup_been_picked_up,
     @scr_006B_clear_mission_objects,
     @scr_006C_is_object_managed,
     @scr_006D_register_object_for_management,
     @scr_006E_get_managed_object_handle,
     @scr_006F_enable_map_file,
     @scr_0070_disable_map_file,
     @scr_0071_store_cars_from_cube,
     @scr_0072_spawn_stored_cars_in_cube,
     @scr_0073_forget_stored_cars_in_cube,
     @scr_0074_reset_timer,
     @scr_0075_get_timer,
     @scr_0076_gosub_with_auto_wasted_busted_return,
     @scr_0077_cancel_auto_wasted_busted_return,
     @scr_0078_create_forbidden_for_cars_cube,
     @scr_0079_create_forbidden_for_peds_cube,
     @scr_007A_is_car_generator_managed,
     @scr_007B_create_car_generator,
     @scr_007C_set_car_generator_active,
     @scr_007D_store_cars_from_angled_cube,
     @scr_007E_spawn_stored_cars_in_angled_cube,
     @scr_007F_forget_stored_cars_in_angled_cube,
     @scr_0080_has_game_been_saved,
     @scr_0081_perform_wasted_busted_return,
     @scr_0082_create_forbidden_for_scenarios_cube,
     @scr_0083_swap_map_models_in_area,
     @scr_0084_restore_map_models_in_area,
     @scr_0085_disable_map_models_in_area,
     @scr_0086_reenable_map_models_in_area,
     @scr_0087_store_single_vehicle,
     @scr_0088_spawn_single_vehicle,
     @scr_0089_forget_single_vehicle,
     @scr_008A_is_single_vehicle_stored,
     @scr_008B_dress_up_ped_as_protagonist,
     @scr_008C_get_native_result_conditional
    );

