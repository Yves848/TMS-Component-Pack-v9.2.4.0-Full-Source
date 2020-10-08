{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015 - 2019                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorMiniHTMLIO;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, SysUtils, ImgList, AdvRichEditorBase, AdvRichEditor, AdvRichEditorIO
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF TMSPACK}
  , GDIPPictureContainer
  {$ENDIF}
  ;

type
  TAdvRichEditorMiniHTMLIO = class(TAdvRichEditorIO)
  private
    FTempRichEdit: TAdvRichEditor;
    FInlineImages: boolean;
    FPlainHTML: boolean;
    FSpaceAsNbSp: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Save(const FileName: string; ImgPath: string = ''); overload; virtual;
    procedure Save(AStream: TStream); overload; override;
    function AsString: string;
    procedure Load(HtmlValue: string; const Images: TCustomImageList; const Pictures: TGDIPPictureContainer = nil); overload;
    procedure Load(FileName: string; Encoding: TEncoding = nil); overload;
    procedure Load(AStream: TStream; Encoding: TEncoding = nil); overload;
    procedure Insert(HtmlValue: string);
  published
    property RichEditor;
    property InlineImages: boolean read FInlineImages write FInlineImages default false;
    property PlainHTML: boolean read FPlainHTML write FPlainHTML default false;
    property SpaceAsNbSp: boolean read FSpaceAsNbSp write FSpaceAsNbSp default True;
  end;


implementation

uses
  StrUtils, Graphics, Clipbrd;

type
  TAdvRichEditorEx = class(TAdvRichEditor);

{ TAdvRichEditorMiniHTMLIO }

procedure TAdvRichEditorMiniHTMLIO.Load(HtmlValue: string;
  const Images: TCustomImageList; const Pictures: TGDIPPictureContainer);
begin
  if (HtmlValue = '') then
    Exit;

  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  RichEditor.Clear;

  TAdvRichEditorEx(RichEditor).ParseHTML(HTMLValue, Images, Pictures);
end;

procedure TAdvRichEditorMiniHTMLIO.Save(const FileName: string;
  ImgPath: string);
var
  sl: TStringList;
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  sl := TStringList.Create;

  try
    if ImgPath = '' then
      ImgPath := ExtractFilePath(FileName);

    sl.Text := AsString;

    if not FSpaceAsNbSp then
      sl.Text := ReplaceStr(sl.Text, '&nbsp;', ' ');

    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorMiniHTMLIO.Load(FileName: string; Encoding: TEncoding = nil);
var
  sl: TStringList;
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName, Encoding);
    Load(sl.Text,nil,nil);
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorMiniHTMLIO.Load(AStream: TStream; Encoding: TEncoding = nil);
var
  ss: TStringStream;
  w: word;
  hasBOM: boolean;
  s: string;
begin
  if Assigned(Encoding) then
    ss := TStringStream.Create('', Encoding)
  else
    ss := TStringStream.Create('');

  try
    hasBOM := false;

    if AStream.Size >= 2  then
    begin
      AStream.Read(w,2);
      hasBom := w = $FEFF;
    end;

    ss.LoadFromStream(AStream);

    s := ss.DataString;

    if hasBOM then
      s := Copy(s,2,Length(s));

    Load(s,nil,nil);
  finally
    ss.Free;
  end;
end;

procedure TAdvRichEditorMiniHTMLIO.Save(AStream: TStream);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create;
  try
    ss.WriteString(AsString);
    ss.Position := 0;
    ss.SaveToStream(AStream);
  finally
    ss.Free;
  end;
end;

function CRLFStrip(s: string): string;
var
  i,ls: Integer;
  lc,nc,cc: char;

begin
  Result := '';
  ls := Length(s);

  s := ReplaceStr(s,#10,'');

  for i := 1 to ls do
  begin
    if i > 1 then
      lc := CharInStr(s,i - 1)
    else
      lc := #0;

    if i + 1 < ls then
      nc := CharInStr(s,i + 1)
    else
      nc := #0;

    cc := CharInStr(s,i);

    if not ( (cc = #13) or (cc = #9) or (cc = #0) ) then
      Result := Result + cc
    else
      if (cc = #13) then
      begin
        if (lc <> '>') or (nc <> '<') then
          Result := Result + ' ';
      end;
  end;
end;

function TAdvRichEditorMiniHTMLIO.AsString: string;
begin
  Result := '';

  if not Assigned(RichEditor) then
    Exit;

  if InlineImages then
    TAdvRichEditorBaseEx(RichEditor).HTMLImages := igInline;

  if PlainHTML then
    Result := CRLFStrip(RichEditor.ContentAsPlainHTML)
  else
    Result := CRLFStrip(RichEditor.ContentAsHTML);

  if not FSpaceAsNbSp then
    Result := ReplaceStr(Result, '&nbsp;', ' ');
end;

constructor TAdvRichEditorMiniHTMLIO.Create(AOwner: TComponent);
begin
  inherited;
  FSpaceAsNbSp := True;
end;

procedure TAdvRichEditorMiniHTMLIO.Insert(HtmlValue: string);
var
  FOrig: TAdvRichEditorBase;
  ms: TMemoryStream;
begin
  FOrig := RichEditor;
  FTempRichEdit := TAdvRichEditor.Create(Self);
  ms := TMemoryStream.Create;

  RichEditor := FTempRichEdit;

  try
    Load(HTMLValue, nil, nil);
    FTempRichEdit.SaveToStream(ms);
    ms.Position := 0;
    TAdvRichEditorEx(FOrig).PasteFromClipboardStream(ms);
  finally
    RichEditor := FOrig;
    ms.Free;
    FTempRichEdit.Free;
  end;

end;

end.
