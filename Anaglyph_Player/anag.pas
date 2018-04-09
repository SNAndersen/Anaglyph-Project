unit anag;

(*
 Copyright (c) 2018, Soren Andersen. For technical queries please use Github.

 Permission to use, copy, modify, and/or distribute this software for any
 purpose with or without fee is hereby granted, provided that the above
 copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Process, Crt, Unix, LCLType, Arrow, Clipbrd, IniFiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup3: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  BUF_SIZE = 4096;

var
  Form1: TForm1;
  AProcess:TProcess;
  ReturnStr:ansistring;
  x1,x2,y1,y2:integer;
  VideoWidth,VideoHeight:integer;
  filename,subfilename,s0,s1,s2,s3,s:string;
  resstr,stereomode:string;
  FullScreen:boolean;

implementation

{$R *.lfm}

{ TForm1 }

Procedure WriteIniFile;

var
  ini:TINIFile;

begin
  ini:=TINIFile.Create('Anaglyph.ini');
  try
    ini.WriteInteger('Anaglyph','Glasses',Form1.RadioGroup1.ItemIndex);
    ini.WriteBool('Anaglyph','Swap',Form1.CheckBox1.Checked);
  finally
    ini.Free;
  end;
end;

Procedure ReadIniFile;

var
  ini:TINIFile;

begin
  ini:=TINIFile.Create('Anaglyph.ini');
  try
    Form1.RadioGroup1.ItemIndex:=ini.ReadInteger('Anaglyph','Glasses',-1);
    Form1.CheckBox1.Checked:=ini.ReadBool('Anaglyph','Swap',false);
  finally
    ini.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Width:=Screen.Width div 2;
  Form1.Height:=Screen.Height div 2 + 100;
  Panel2.Width:=Screen.Width div 2;
  Panel2.Height:=95;
  ReadIniFile;
  filename:='';
  subfilename:='';
  FullScreen:=false;
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key=VK_UP) or (Key=VK_DOWN) or (Key=VK_LEFT) or (Key=VK_RIGHT) then
  begin
    // values found with showkey -a in lxterminal
    case Key of
      VK_UP    :begin
                  if AProcess.Running then
                  begin
                    AProcess.Input.WriteAnsiString(chr($1B)+'[A');
                  end;
                end;
      VK_DOWN  :begin
                  if AProcess.Running then
                  begin
                    AProcess.Input.WriteAnsiString(chr($1B)+'[B');
                  end;
                end;
      VK_RIGHT :begin
                  if AProcess.Running then
                  begin
                    AProcess.Input.WriteAnsiString(chr($1B)+'[C');
                  end;
                end;
      VK_LEFT  :begin
                  if AProcess.Running then
                  begin
                    AProcess.Input.WriteAnsiString(chr($1B)+'[D');
                  end;
                end;
    end;  //end case
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if AProcess.Running then AProcess.Input.WriteAnsiString(Key);
  if (Key = 'q') or (Key = chr($1B)) then
  begin
    if AProcess.Running then
    begin
      AProcess.Active:=false;
      AProcess.Free;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);

var
  rescap:boolean;
  anaglyph_type:integer;
  ch:char;
  RetBuf:TStringList;

begin
  if AProcess <> nil then
  begin
    if AProcess.Active then exit;
  end;
  if OpenDialog1.Execute then
  begin
    FullScreen:=false;
    filename:=OpenDialog1.FileName;
    s2:='';
    subfilename:=ChangeFileExt(filename,'.srt');
    AProcess:=TProcess.Create(nil);
    AProcess.Executable:='omxplayer';
    AProcess.Active:=true;
    AProcess.Parameters.Clear;
    AProcess.Parameters.Add('-i');
    AProcess.Parameters.Add('--info');
    AProcess.Parameters.Add(filename);
    AProcess.PipeBufferSize:=BUF_SIZE;
    AProcess.Options:=[poUsePipes,poWaitOnExit];
    AProcess.Execute;
    RetBuf:=TStringList.Create;
    RetBuf.LoadFromStream(AProcess.Stderr);
    ReturnStr:=RetBuf.Text;
    RetBuf.Free;
    AProcess.Active:=false;
    AProcess.Free;

    // now find screen resolution and 3D info

    s:=copy(ReturnStr,pos('Video:',ReturnStr),Pos('[SAR',ReturnStr));
    resstr:=copy(s,(Pos('x',s)-4),9);
    while Pos(' ',resstr) > 0 do Delete(resstr,Pos(' ',resstr),1);
    if TryStrToInt(Copy(resstr,1,Pos('x',resstr)-1),VideoWidth) then
    begin
      if TryStrToInt(Copy(resstr,Pos('x',resstr)+1,4),VideoHeight) then
      begin
        rescap:=true;
      end
      else
      begin
        rescap:=false;
      end;
    end
    else
    begin
      rescap:=false;
    end;
    stereomode:='';
    if Pos('left_right',ReturnStr) > 0 then
    begin
      stereomode:='left_right';
      RadioGroup3.ItemIndex:=0;
    end;
    if Pos('top_bottom',ReturnStr) > 0 then
    begin
      stereomode:='top_bottom';
      RadioGroup3.ItemIndex:=1;
    end;
    //if rescap then Label1.Caption:=IntToStr(VideoWidth)+'x'+IntToStr(VideoHeight);
    if stereomode ='' then
    begin
      // no 3D flag in file - look for file extension
      s:=filename;
      s:=Upcase(s);
      if (Pos('SBS',s) > 0) or (Pos('HSBS',s) > 0) then
      begin
        stereomode:='left_right';
        RadioGroup3.ItemIndex:=0;
      end;
      if (Pos('TAB',s) > 0) or (Pos('HTAB',s) > 0) then
      begin
        stereomode:='top_bottom';
        RadioGroup3.ItemIndex:=1;
      end;
    end;
    anaglyph_type:=0;
    if RadioGroup3.ItemIndex = 0 then
    begin
      //sbs
      if RadioGroup1.ItemIndex = 0 then
      begin
        // Red/Cyan
        if CheckBox1.Checked then
        begin
          anaglyph_type:=2;
        end
        else
        begin
          anaglyph_type:=1;
        end;
      end;
    end;
    if RadioGroup3.ItemIndex = 0 then
    begin
      //sbs
      if RadioGroup1.ItemIndex = 1 then
      begin
        // Green/Magenta
        if CheckBox1.Checked then
        begin
          anaglyph_type:=4;
        end
        else
        begin
          anaglyph_type:=3;
        end;
      end;
    end;
    if RadioGroup3.ItemIndex = 1 then
    begin
      //hou
      if RadioGroup1.ItemIndex = 0 then
      begin
        // Red/Cyan
        if CheckBox1.Checked then
        begin
          anaglyph_type:=6;
        end
        else
        begin
          anaglyph_type:=5;
        end;
      end;
    end;
    if RadioGroup3.ItemIndex = 1 then
    begin
      //hou
      if RadioGroup1.ItemIndex = 1 then
      begin
        // Green/Magenta
        if CheckBox1.Checked then
        begin
          anaglyph_type:=8;
        end
        else
        begin
          anaglyph_type:=7;
        end;
      end;
    end;
    Application.ProcessMessages;
    // check if external subtitle present
    if FileExists(subfilename) then
    begin
      s2:='--subtitles '+'"'+subfilename+'"';
    end;
    // set window parameters
    x1:=(Screen.Width div 2) div 2;
    x2:=x1+Form1.Width;

    y1:=((Screen.Height div 2) div 2) - 50;
    y2:=y1+Form1.Height - 100;

    s0:='--anaglyph '+ IntToStr(anaglyph_type);
    s1:='--win '+IntToStr(x1)+','+IntToStr(y1)+','+IntToStr(x2)+','+IntToStr(y2);
    Application.ProcessMessages;

    if stereomode <> '' then
    begin
      (*
      // this structure do NOT work for reasons unknown
      // works fine getting file information see above
      AProcess:=TProcess.Create(nil);
      AProcess.Executable:='omxplayer';
      AProcess.Active:=true;
      AProcess.Parameters.Clear;
      AProcess.Parameters.Add(s0);
      AProcess.Parameters.Add(s1);
      if s2 <> '' then
      begin
        AProcess.Parameters.Add(s2);
      end;
      AProcess.Parameters.Add(filename);
      AProcess.PipeBufferSize:=BUF_SIZE;
      AProcess.Options:=[poUsePipes];
      AProcess.Execute;
      *)

      // Hack using commandline (somewhat unstable)
      if s2 <> '' then
      begin
        s:='omxplayer '+s0+' '+s1+' '+s2+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer '+s0+' '+s1+' '+'"'+filename+'"';
      end;
      AProcess:=TProcess.Create(nil);
      AProcess.Executable:='omxplayer';
      AProcess.Active:=true;
      AProcess.CommandLine:=s;
      AProcess.PipeBufferSize:=BUF_SIZE;
      AProcess.Options:=[poUsePipes];
      AProcess.Execute;

    end
    else
    begin
      // we need to eyeball the stereo mode on screen
      if s2 <> '' then
      begin
        s:='omxplayer '+s1+' '+s2+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer '+s1+' '+'"'+filename+'"';
      end;
      AProcess:=TProcess.Create(nil);
      AProcess.Executable:='omxplayer';
      AProcess.Active:=true;
      AProcess.CommandLine:=s;
      AProcess.PipeBufferSize:=BUF_SIZE;
      AProcess.Options:=[poUsePipes];
      AProcess.Execute;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);

var
  RetBuf:TStringList;

begin
  if FullScreen then exit;
  if filename <> '' then
  begin
    if AProcess.Running then
    begin
      AProcess.Input.WriteAnsiString('q');
      // now read stop position
      RetBuf:=TStringList.Create;
      RetBuf.LoadFromStream(AProcess.Output);
      ReturnStr:=RetBuf.Text;
      RetBuf.Free;
      AProcess.Active:=false;
      AProcess.Free;
    end;
    s:=copy(ReturnStr,pos('Stopped at:',ReturnStr),length(s));
    s:=copy(s,12,length(s));
    while pos(' ',s) > 0 do delete(s,pos(' ',s),1);
    delete(s,9,length(s));
    s3:='-l '+s;
    s1:='--win '+IntToStr(0)+','+IntToStr(0)+','+IntToStr(Screen.Width)+','+IntToStr(Screen.Height);
    if stereomode <> '' then
    begin
      if s2 <> '' then
      begin
        s:='omxplayer -b '+s0+' '+s1+' '+s2+' '+s3+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer -b '+s0+' '+s1+' '+s3+' '+'"'+filename+'"';
      end;
      AProcess:=TProcess.Create(nil);
      AProcess.Executable:='omxplayer';
      AProcess.Active:=true;
      AProcess.CommandLine:=s;
      AProcess.PipeBufferSize:=BUF_SIZE;
      AProcess.Options:=[poUsePipes];
      AProcess.Execute;
      FullScreen:=true;
    end
    else
    begin
      if s2 <> '' then
      begin
        s:='omxplayer -b '+s2+' '+s3+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer -b '+s3+' '+'"'+filename+'"';
      end;
      AProcess:=TProcess.Create(nil);
      AProcess.Executable:='omxplayer';
      AProcess.Active:=true;
      AProcess.CommandLine:=s;
      AProcess.PipeBufferSize:=BUF_SIZE;
      AProcess.Options:=[poUsePipes];
      AProcess.Execute;
      FullScreen:=true;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);

begin
  WriteIniFile;
  if AProcess <> nil then
  begin
    if AProcess.Running then
    begin
      AProcess.Input.WriteAnsiString('q');
      AProcess.Free;
    end;
  end;
  Application.ProcessMessages;
  Application.Terminate;
end;

procedure TForm1.Button4Click(Sender: TObject);

begin
  if filename <> '' then
  begin
    s1:='--win '+IntToStr(0)+','+IntToStr(0)+','+IntToStr(Screen.Width)+','+IntToStr(Screen.Height);
    if s2 <> '' then
    begin
      s:='omxplayer -b '+s0+' '+s1+' '+s2+' '+'"'+filename+'"';
    end
    else
    begin
      s:='omxplayer -b '+s0+' '+s1+' '+'"'+filename+'"';
    end;
    Clipboard.AsText:=s;
    if AProcess.Running then
    begin
      AProcess.Input.WriteAnsiString('q');
      AProcess.Active:=false;
      AProcess.Free;
    end;
    Application.ProcessMessages;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIniFile;
end;

end.

