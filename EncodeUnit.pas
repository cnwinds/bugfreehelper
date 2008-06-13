unit EncodeUnit;

interface

uses Windows;

function Utf8ToAnsiString(utf8str:string; codepage: Integer):ansistring;

implementation

function Utf8ToAnsiString(utf8str:string; codepage: Integer):ansistring;
var
  i:integer;
  buffer:widestring;
  ch,c1,c2:byte;
begin
  result:='';
  i:=1;
  while i<=length(utf8str) do begin
      ch:=byte(utf8str[i]);
      setlength(buffer,length(buffer)+1);
      if   (ch   and   $80)=0   then   //1-byte
            buffer[length(buffer)]:=widechar(ch)   
      else   begin
      if   (ch   and   $e0)   =   $c0   then   begin   //   2-byte
            inc(i);
            c1   :=   byte(utf8str[i]);   
            buffer[length(buffer)]:=widechar((word(ch   and   $1f)   shl   6)   or   (c1   and   $3f));   
        end
        else   begin   //   3-byte   
            inc(i);   
            c1   :=   byte(utf8str[i]);   
            inc(i);   
            c2   :=   byte(utf8str[i]);   
            buffer[length(buffer)]:=widechar(   
                (word(ch   and   $0f)   shl   12)   or
                (word(c1   and   $3f)   shl   6)   or   
                (c2   and   $3f));   
        end;   
        end;
      inc(i);   
    end;   //while   
    i   :=   widechartomultibyte(codepage,
                      wc_compositecheck   or   wc_discardns   or   wc_sepchars   or   wc_defaultchar,
                      @buffer[1],   -1,   nil,   0,   nil,   nil);
    if   i>1   then   begin   
        setlength(result,   i-1);   
        widechartomultibyte(codepage,   
                wc_compositecheck   or   wc_discardns   or   wc_sepchars   or   wc_defaultchar,   
                @buffer[1],   -1,   @result[1],   i-1,   nil,   nil);
    end;   
end;

end.
