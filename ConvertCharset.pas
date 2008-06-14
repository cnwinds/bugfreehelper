unit ConvertCharset;

interface

uses Windows;

function Utf8ToAnsiString(Utf8Str:string; CodePage: Integer = CP_ACP):AnsiString;

implementation

function Utf8ToAnsiString(Utf8Str:string; CodePage: Integer): AnsiString;
var
  I: Integer;
  WBuffer: WideString;
  ch,c1,c2: Byte;
begin
  result:='';
  I := 1;
  while I<=Length(Utf8Str) do
  begin
    ch := Byte(Utf8Str[I]);
    SetLength(WBuffer, Length(WBuffer)+1);
    if (ch and $80) = 0 then   //1-byte
      WBuffer[Length(WBuffer)] := WideChar(ch)
    else
    begin
      if (ch and $e0) = $c0 then
      begin   // 2-byte
        inc(I);
        c1 := Byte(Utf8Str[I]);
        WBuffer[length(WBuffer)] :=
          WideChar((Word(ch and $1f) shl 6) or (c1 and $3f));
      end
      else begin   // 3-byte
        Inc(I);
        c1 := Byte(Utf8Str[I]);
        Inc(I);
        c2 := Byte(Utf8Str[I]);
        WBuffer[Length(WBuffer)]:=WideChar(
            (Word(ch and $0f) shl 12) or
            (Word(c1 and $3f) shl 6) or
            (c2 and $3f));
      end;
    end;
  Inc(I);
  end;   //while
  I := WideCharToMultiByte(
    CodePage,
    WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
    @WBuffer[1], -1, nil, 0, nil, nil);
  if I > 1 then
  begin
    SetLength(result, I-1);
    WideCharToMultiByte(CodePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @WBuffer[1], -1, @result[1], I-1, nil, nil);
  end;
end;

end.
