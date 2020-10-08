unit Office2019Frame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, GDIPSectionItem,
  GDIPTextItem, GDIPImageTextItem, GDIPPictureContainer, AdvPanel,
  GDIPWedgeItem, GDIPCustomItem, GDIPSplitterItem, CustomItemsContainer,
  AdvPolyList, Vcl.ExtCtrls, GDIPImageItem, AdvHorizontalPolyList;

type
  TOffice2019Frame1 = class(TFrame)
    AdvPanel1: TAdvPanel;
    AdvPolyMenu2: TAdvPolyMenu;
    MenuItem01: TImageTextItem;
    MenuItem09: TSplitterItem;
    MenuItem03: TImageTextItem;
    MenuItem04: TImageTextItem;
    MenuItem10: TWedgeItem;
    AdvPanel3: TAdvPanel;
    AdvPolyList1: TAdvPolyList;
    SectionItem2: TSectionItem;
    ImageTextItem4: TImageTextItem;
    GDIPPictureContainerMenuWhite: TGDIPPictureContainer;
    GDIPPictureContainerMenuColor: TGDIPPictureContainer;
    AdvHorizontalPolyList1: TAdvHorizontalPolyList;
    ImageItem1: TImageItem;
    ImageItem2: TImageItem;
    ImageItem3: TImageItem;
    GDIPPictureContainerSortColor: TGDIPPictureContainer;
    GDIPPictureContainerInv: TGDIPPictureContainer;
    MenuItem05: TSplitterItem;
    MenuItem11: TWedgeItem;
    ImageTextItem5: TImageTextItem;
    ImageTextItem6: TImageTextItem;
    ImageTextItem7: TImageTextItem;
    ImageTextItem8: TImageTextItem;
    ImageTextItem9: TImageTextItem;
    ImageTextItem10: TImageTextItem;
    MenuItem06: TImageTextItem;
    MenuItem07: TImageTextItem;
    MenuItem08: TImageTextItem;
    MenuItem02: TImageTextItem;
    procedure MenuItem11ItemClick(Sender: TObject; Item: TCustomItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}



procedure TOffice2019Frame1.MenuItem11ItemClick(Sender: TObject;
  Item: TCustomItem);
begin
  Application.Terminate;
end;

end.
