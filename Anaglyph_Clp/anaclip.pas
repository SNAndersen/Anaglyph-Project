unit anaclip;
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
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Crt, Unix, LCLType, Clipbrd, ComCtrls, IniFiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Image1: TImage;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Process1: TProcess;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    Procedure FindStereoMode(Sender: TObject; var s:string);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  BUF_SIZE = 4096;

var
  Form1: TForm1;
  ReturnStr:ansistring;
  x1,x2,y1,y2:integer;
  VideoWidth,VideoHeight:integer;
  filename,subfilename,s0,s1,s2,s:string;
  resstr,stereomode:string;

implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FindStereoMode(Sender: TObject; var s:string);

var
  anaglyph_type:integer;
  rescap:boolean;
  RetBuf:TStringList;
  ext:string;

begin
    subfilename:=ChangeFileExt(filename,'.srt');
    // look for file extension
    s:=filename;
    s:=Upcase(s);
    stereomode:='';
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
    if stereomode ='' then
    begin
      ext:=ExtractFileExt(s);
      if ext = '.MKV' then
      begin
        // now look for stereo mode information in video file (mkv)
        Process1.Active:=true;
        Process1.Parameters.Clear;
        Process1.Parameters.Add('-i');
        Process1.Parameters.Add('--info');
        Process1.Parameters.Add(filename);

        Process1.PipeBufferSize:=BUF_SIZE;
        Process1.Options:=[poUsePipes,poWaitOnExit];
        Process1.Execute;
        RetBuf:=TStringList.Create;
        RetBuf.LoadFromStream(Process1.Stderr);
        ReturnStr:=RetBuf.Text;
        Process1.Active:=false;

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
      end;
    end;
    anaglyph_type:=0;
    if stereomode <> '' then
    begin
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
    end;
    Application.ProcessMessages;
    // check if external subtitle present
    s2:='';
    if FileExists(subfilename) then
    begin
      s2:='--subtitles '+'"'+subfilename+'"';
    end;
    s0:='--anaglyph '+ IntToStr(anaglyph_type);
    s1:='--win '+IntToStr(0)+','+IntToStr(0)+','+IntToStr(Screen.Width)+','+IntToStr(Screen.Height);
    Application.ProcessMessages;
    // now all this for file path and file names without spaces
    if stereomode <> '' then
    begin
      if s2 <> '' then
      begin
        //s:='-b '+s0+' '+s1+' '+s2+' '+'"'+filename+'"';   // string with embedded " can not be parsed
        s:='-b '+s0+' '+s1+' '+s2+' '+filename;
      end
      else
      begin
        //s:='-b '+s0+' '+s1+' '+'"'+filename+'"';
        s:='-b '+s0+' '+s1+' '+filename;
      end;
    end
    else
    begin
      if s2 <> '' then
      begin
        //s:='-b '+s1+' '+s2+' '+'"'+filename+'"';
        s:='-b '+s1+' '+s2+' '+filename;
      end
      else
      begin
        //s:='-b '+s1+' '+'"'+filename+'"';
        s:='-b '+s1+' '+filename;
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
  if OpenDialog1.Execute then
  begin
    filename:=OpenDialog1.FileName;
    s2:='';
    subfilename:=ChangeFileExt(filename,'.srt');
    Process1.Active:=true;
    //s:='omxplayer --info '+'"'+filename+'"';
    //Process1.CommandLine:=s;

    Process1.Parameters.Clear;
    Process1.Parameters.Add('-i');
    Process1.Parameters.Add('--info');
    Process1.Parameters.Add(filename);

    Process1.PipeBufferSize:=BUF_SIZE;
    Process1.Options:=[poUsePipes,poWaitOnExit];
    Process1.Execute;
    RetBuf:=TStringList.Create;
    RetBuf.LoadFromStream(Process1.Stderr);
    ReturnStr:=RetBuf.Text;
    Process1.Active:=false;

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
    s0:='--anaglyph '+ IntToStr(anaglyph_type);
    s1:='--win '+IntToStr(0)+','+IntToStr(0)+','+IntToStr(Screen.Width)+','+IntToStr(Screen.Height);
    Application.ProcessMessages;
    if stereomode <> '' then
    begin
      if s2 <> '' then
      begin
        s:='omxplayer -b '+s0+' '+s1+' '+s2+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer -b '+s0+' '+s1+' '+'"'+filename+'"';
      end;
    end
    else
    begin
       // we need to eyeball the stereo mode on screen
      if s2 <> '' then
      begin
        s:='omxplayer -b '+s2+' '+'"'+filename+'"';
      end
      else
      begin
        s:='omxplayer -b '+'"'+filename+'"';
      end;
    end;
    // copy to clipboard
    Clipboard.AsText:=s;
    // now minimize the form
    Application.Minimize;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);

var
  info:TsearchRec;
  count,i,i1,i2:Integer;
  s,s1,CurrentDir,FileDir:string;
  f:textfile;

begin
  // create playlist
  ProgressBar1.Position:=0;
  count:=0;
  if CheckBox2.Checked then
  begin
    // read current playlist
    Memo1.Lines.Clear;
    Memo1.Lines.LoadFromFile('playlist.pls');
  end;
  if SelectDirectoryDialog1.Execute then
  begin
    FileDir:=SelectDirectoryDialog1.FileName;
    CurrentDir:=GetCurrentDir;
    ChDir(FileDir);
    Memo2.Lines.Clear;
    if FindFirst('*',faAnyFile,info) = 0 then
    begin
      Repeat
        s:=info.Name;
        if (s <> '.') and (s <> '..') then
        begin
          s1:=ExtractFileExt(s);
          s1:=UpCase(s1);
          if (s1 = '.MP4') or (s1 = '.MKV') or (s1 = '.WMV') or (s1 = '.FLV') then
          begin
            Memo2.Append(s);
          end;
        end;
      until FindNext(info) <> 0;
    end;
    FindClose(info);
    ChDir(CurrentDir);
  end;

  Assignfile(f,'playlist.pls');
  {$I-}
  Rewrite(f);
  {$I+}
  if IOResult = 0 then
  begin
    count:=Memo2.Lines.Count;
    ProgressBar1.Max:=count-1;
    for i:=0 to count-1 do
    begin
      filename:=filedir+'/'+Memo2.Lines[i];
      FindStereoMode(Sender,s);
      writeln(f,s);
      ProgressBar1.Position:=i;
    end;
    CloseFile(f);
  end;

  if CheckBox2.Checked then
  begin
    // read new playlist append to memo
    Assignfile(f,'playlist.pls');
    {$I-}
    Reset(f);
    {$I+}
    for i:=0 to count-1 do
    begin
      readln(f,s);
      Memo1.Lines.Append(s);
    end;
    CloseFile(f);
    Memo1.Lines.SaveToFile('playlist.pls');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);

Var
  i,i1,i2,count:integer;
  s,s1:string;

begin
  // randomize playlist
  if FileExists('playlist.pls') then
  begin
    ProgressBar1.Position:=0;
    Memo1.Lines.Clear;
    Memo1.Lines.LoadFromFile('playlist.pls');
    count:=Memo1.Lines.Count;
    Randomize;
    for i:=0 to (count * 20) do
    begin
      i1:=Random(count);
      i2:=Random(count);
      s:=Memo1.Lines[i1];
      s1:=Memo1.Lines[i2];
      // now swap
      Memo1.Lines[i1]:=s1;
      Memo1.Lines[i2]:=s;
    end;
    Memo1.Lines.SaveToFile('playlist.pls');
  end;
end;

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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIniFile;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReadIniFile;
  filename:='';
  subfilename:='';
  Process1.Executable:='omxplayer';
end;

end.

