{********************************************************************}
{ TMS TImagePicker Demo                                              }
{                                                                    }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2001 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UImagePickerDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvCombo, ImagePicker, ImgList, System.ImageList;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    ImagePicker1: TImagePicker;
    Label1: TLabel;
    SelTrad: TLabel;
    Button1: TButton;
    GroupBox2: TGroupBox;
    ImagePicker2: TImagePicker;
    ImageList2: TImageList;
    Label3: TLabel;
    SelFlat: TLabel;
    Button2: TButton;
    Label19: TLabel;
    Label18: TLabel;
    procedure e(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ImagePicker2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  ShellAPI;

procedure TForm1.e(Sender: TObject);
begin
  SelTrad.Caption := ImagePicker1.Selection.Caption;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ImagePicker1.SelectByCaption('Green');
  SelTrad.Caption := 'Green';
end;

procedure TForm1.ImagePicker2Change(Sender: TObject);
begin
  SelFlat.Caption := ImagePicker2.Selection.Caption;
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle,'open','https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImagePicker1.SelectByImageIdx(0);
  SelTrad.Caption := ImagePicker1.Selection.Caption;
  ImagePicker2.SelectByImageIdx(0);
  SelFlat.Caption := ImagePicker2.Selection.Caption;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ImagePicker2.SelectByCaption('USA');
  SelFlat.Caption := 'USA';
end;

end.
