unit UDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.ComCtrls, Vcl.Controls, Vcl.StdCtrls, Vcl.BaseImageCollection,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.Menus, AdvTypes;

type
  TForm130 = class(TForm)
    VirtualImageList1: TVirtualImageList;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    Image1: TImage;
    Image2: TImage;
    HeaderControl1: THeaderControl;
    TreeView1: TTreeView;
    MainMenu1: TMainMenu;
    PN1: TMenuItem;
    SVG1: TMenuItem;
    erer1: TMenuItem;
    PNG1: TMenuItem;
    PNG2: TMenuItem;
    PNG3: TMenuItem;
    PNG4: TMenuItem;
    SVG2: TMenuItem;
    SVG3: TMenuItem;
    SVG4: TMenuItem;
    SVG5: TMenuItem;
    SVG6: TMenuItem;
    AdvSVGImageCollection1: TAdvSVGImageCollection;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form130: TForm130;

implementation

{$R *.dfm}

procedure TForm130.FormCreate(Sender: TObject);
begin
  TreeView1.FullExpand;
end;

end.
