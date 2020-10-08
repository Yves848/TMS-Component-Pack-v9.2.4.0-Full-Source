{********************************************************************}
{ TMS TAdvSmoothPanel Demo                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2014 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{********************************************************************}

unit UAdvSmoothExpanderButtonPanelDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdvSmoothPanel, AdvSmoothExpanderPanel, AdvSmoothExpanderButtonPanel, AdvGDIP,
  ShellAPI;

type
  TForm65 = class(TForm)
    AdvSmoothExpanderButtonPanel1: TAdvSmoothExpanderButtonPanel;
    AdvSmoothExpanderButtonPanel2: TAdvSmoothExpanderButtonPanel;
    procedure FormCreate(Sender: TObject);
    procedure AdvSmoothExpanderButtonPanel1ButtonClick(Sender: TObject;
      ButtonIndex: Integer);
    procedure AdvSmoothExpanderButtonPanel2AnchorClick(Sender: TObject;
      Anchor: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form65: TForm65;

implementation

{$R *.dfm}

  procedure LoadFromRes(resname: string; picture: TAdvGDIPPicture);
  var
    rs: TResourceStream;
  begin
    rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
    picture.LoadFromStream(rs);
    rs.Free;
  end;

procedure TForm65.AdvSmoothExpanderButtonPanel1ButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  AdvSmoothExpanderButtonPanel1.Caption.Text := 'Tag Functions : ' + AdvSmoothExpanderButtonPanel1.Buttons[ButtonIndex].Caption + ' clicked !';
end;

procedure TForm65.AdvSmoothExpanderButtonPanel2AnchorClick(Sender: TObject;
  Anchor: string);
begin
  ShellExecute(0, 'Open', Pchar(Anchor), nil, nil, SW_NORMAL);
end;

procedure TForm65.FormCreate(Sender: TObject);
const
  fruits: array[0..8] of string = ('Lime', 'Strawberry', 'Banana', 'Cherry', 'Watermelon', 'Apple',
    'Grapefruit', 'Cranberry', 'Apricot');
var
  I: Integer;
  pw,ph: integer;
  DPIScale: single;
  begin
  DPIScale := Monitor.PixelsPerInch/Form65.PixelsPerInch;

  pw := Round(AdvSmoothExpanderButtonPanel1.Width / DPIScale);
  ph := Round(AdvSmoothExpanderButtonPanel1.Height / DPIScale);

  AdvSmoothExpanderButtonPanel1.ButtonWidth := (pw - (AdvSmoothExpanderButtonPanel1.ButtonHorizontalMargin * 2)) div 3;
  AdvSmoothExpanderButtonPanel1.ButtonHeight := (ph - AdvSmoothExpanderButtonPanel1.ButtonVerticalMargin * 2) div 3;
  for I := 0 to 8 do
  begin
    if I mod 2 = 0 then
    begin
      with AdvSmoothExpanderButtonPanel1.Buttons.Add do
      begin
        Color := clSkyBlue;
        Caption := fruits[I];
      end;
    end
    else
    begin
      with AdvSmoothExpanderButtonPanel1.Buttons.Add do
      begin
        Color := $003415FE;
        Caption := fruits[I];
      end;
    end;
  end;
end;

end.
