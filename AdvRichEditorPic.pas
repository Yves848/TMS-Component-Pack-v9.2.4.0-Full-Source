{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2018                                       }
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

unit AdvRichEditorPic;

{$I TMSDEFS.INC}

interface

uses
  Classes
{$IFDEF VCLLIB}
  , Graphics
{$ENDIF}
{$IFDEF LCLLIB}
  , Graphics
{$ENDIF}
{$IFDEF FMXLIB}
  , FMX.Graphics
{$ENDIF}
  ;

{$IFDEF FNCLIB}
type
  TPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);

  {$IFDEF FMXLIB}
  TPicture = class(TBitmap)
  end;
  {$ENDIF}

  TGDIPPicture = class(TPicture)
  private
    FTransparentBitmap: boolean;
  public
    function Empty: boolean;
    function PictureFormat: TPictureFormat;
    procedure GetImageSizes;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property TransparentBitmap: boolean read FTransparentBitmap write FTransparentBitmap;
  end;
{$ENDIF}

{$IFDEF LCLLIB}

type
  TCharSet = set of Char;

function IndexText(const AText: string; const AValues: array of string): integer;
function CharInSet(C: Char; const CharSet: TCharSet): Boolean;
{$ENDIF}

implementation

uses
  SysUtils;

{$IFDEF LCLLIB}
function CharInSet(C: Char; const CharSet: TCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function IndexText(const AText: string; const AValues: array of string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
  begin
    if UpperCase(AText) = UpperCase(AValues[I]) then
    begin
      Result := I;
      break;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FNCLIB}

{ TGDIPPicture }

function TGDIPPicture.Empty: boolean;
begin
  {$IFDEF FMXLIB}
  Result := IsEmpty;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Result := Graphic.Empty;
  {$ENDIF}
end;


procedure TGDIPPicture.LoadFromStream(Stream: TStream);
begin
  Stream.Position := 0;
  {$IFDEF FMXLIB}
  LoadFromStream(Stream);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  inherited LoadFromStream(Stream);
  {$ENDIF}
end;


procedure TGDIPPicture.SaveToStream(Stream: TStream);
begin
  {$IFDEF FMXLIB}
  SaveToStream(Stream);
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Graphic.SaveToStream(Stream);
  {$ENDIF}
end;

procedure TGDIPPicture.GetImageSizes;
begin

end;

function TGDIPPicture.PictureFormat: TPictureFormat;
begin
  Result := pfBMP;
end;

{$ENDIF}

end.
