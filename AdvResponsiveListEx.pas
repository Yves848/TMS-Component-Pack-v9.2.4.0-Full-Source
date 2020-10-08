{*************************************************************************}
{ TMS TAdvResponsiveListEx                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016                                              }
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

unit AdvResponsiveListEx;

interface

uses
  Windows, Classes, Graphics, Types, AdvResponsiveList, JPEG, AdvGraphics;

type
  TResponsiveListItemEx = class(TResponsiveListItem)
  private
    FPicture: TPicture;
    FCustomProp: string;
    procedure SetPicture(const Value: TPicture);
  protected
    procedure DrawItem(AGraphics: TAdvGraphics; ATemplate, AHeaderTemplate, AFooterTemplate: string; ARect: TRect; Focus: boolean); override;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property CustomProp: string read FCustomProp write FCustomProp;
  end;


  TAdvResponsiveListEx = class(TAdvResponsiveList)
  private
  protected
    function GetItemClass: TCollectionItemClass; override;
  published
  end;

procedure Register;

implementation

{ TAdvResponsiveListEx }

function TAdvResponsiveListEx.GetItemClass: TCollectionItemClass;
begin
  Result := TResponsiveListItemEx;
end;

{ TResponsiveListItemEx }

procedure TResponsiveListItemEx.Assign(Source: TPersistent);
begin
  if (Source is TResponsiveListItemEx) then
  begin
    inherited Assign(Source);
    FCustomProp := (Source as TResponsiveListItemEx).CustomProp;
    FPicture.Assign((Source as TResponsiveListItemEx).Picture);
  end;
end;

constructor TResponsiveListItemEx.Create(Collection: TCollection);
begin
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  inherited;
end;

destructor TResponsiveListItemEx.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TResponsiveListItemEx.DrawItem(AGraphics: TAdvGraphics; ATemplate, AHeaderTemplate, AFooterTemplate: string;
  ARect: TRect; Focus: boolean);
begin
  inherited DrawItem(AGraphics, ATemplate, AHeaderTemplate, AFooterTemplate, ARect, Focus);

  if Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty then
    AGraphics.DrawBitmap(ARect.Left, ARect.Top, ARect.Left + FPicture.Graphic.Width, ARect.Top + FPicture.Graphic.Height, FPicture.Graphic);

  AGraphics.DrawText(ARect, CustomProp);
end;

procedure TResponsiveListItemEx.PictureChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TResponsiveListItemEx.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;


procedure Register;
begin
  RegisterComponents('TMS',[TAdvResponsiveListEx]);
end;

end.
