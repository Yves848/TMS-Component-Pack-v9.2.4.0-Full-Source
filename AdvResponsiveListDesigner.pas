{*************************************************************************}
{ TMS TAdvResponsiveList                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2016 - 2017                                       }
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

unit AdvResponsiveListDesigner;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, AdvScrollControl, AdvResponsiveList, HTMLProp,
  StdCtrls, ExtCtrls, Spin, ComCtrls, Buttons, AdvGraphics, PngImage;

type
  TTemplateID = (tidContent, tidHeader, tidFooter);

  TAdvResponsiveListDesignerForm = class(TForm)
    pnlBtn: TPanel;
    Button1: TButton;
    Button2: TButton;
    pnlProp: TPanel;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lstConditions: TListView;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    pbVert: TPaintBox;
    pbHorz: TPaintBox;
    List: TAdvResponsiveList;
    Label8: TLabel;
    Label9: TLabel;
    SpinEdit7: TSpinEdit;
    SpinEdit8: TSpinEdit;
    btnTemplate: TButton;
    Label10: TLabel;
    SpinEdit9: TSpinEdit;
    Label11: TLabel;
    SpinEdit10: TSpinEdit;
    Label12: TLabel;
    SpinEdit11: TSpinEdit;
    Label13: TLabel;
    SpinEdit12: TSpinEdit;
    Label14: TLabel;
    SpinEdit13: TSpinEdit;
    Button3: TButton;
    Button4: TButton;
    ckHFull: TCheckBox;
    ckWFull: TCheckBox;
    ckWF: TCheckBox;
    ckWT: TCheckBox;
    ckHF: TCheckBox;
    ckHT: TCheckBox;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure lstConditionsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure SpinEdit1Exit(Sender: TObject);
    procedure ListResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure SpinEdit5Exit(Sender: TObject);
    procedure SpinEdit6Exit(Sender: TObject);
    procedure pbHorzPaint(Sender: TObject);
    procedure pbVertPaint(Sender: TObject);
    procedure btnTemplateClick(Sender: TObject);
    procedure ckWFullClick(Sender: TObject);
    procedure ckHFullClick(Sender: TObject);
    procedure SpinEdit7Change(Sender: TObject);
    procedure SpinEdit8Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ckWFClick(Sender: TObject);
    procedure ckWTClick(Sender: TObject);
    procedure ckHFClick(Sender: TObject);
    procedure ckHTClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    { Public declarations }
    FUpdatingList: boolean;
    FUpdatingSize: boolean;
    FProgUpdate: boolean;
    procedure UpdateListView;
    procedure UpdateSpinEdits;
    procedure UpdateConditions;
    procedure UpdateListSize;
    procedure SetTemplate(templateID: TTemplateID);
  end;


procedure ShowDesigner(List: TAdvResponsiveList);


implementation

uses
  Math;

{$R *.dfm}

procedure TAdvResponsiveListDesignerForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAdvResponsiveListDesignerForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TAdvResponsiveListDesignerForm.Button3Click(Sender: TObject);
begin
  SetTemplate(tidHeader);
end;

procedure TAdvResponsiveListDesignerForm.Button4Click(Sender: TObject);
begin
  SetTemplate(tidFooter);
end;

procedure TAdvResponsiveListDesignerForm.ckHFClick(Sender: TObject);
begin
 if ckHF.Checked and not FProgUpdate then
   SpinEdit3.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.ckHFullClick(Sender: TObject);
begin
 if ckHFull.Checked and not FProgUpdate then
   SpinEdit8.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.ckHTClick(Sender: TObject);
begin
 if ckHT.Checked and not FProgUpdate then
   SpinEdit4.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.ckWFClick(Sender: TObject);
begin
 if ckWF.Checked and not FProgUpdate then
   SpinEdit1.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.ckWFullClick(Sender: TObject);
begin
 if ckWFull.Checked and not FProgUpdate then
   SpinEdit7.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.ckWTClick(Sender: TObject);
begin
 if ckWT.Checked and not FProgUpdate then
   SpinEdit2.Value := -1;
end;

procedure TAdvResponsiveListDesignerForm.DoShow;
begin
  inherited;
  FProgUpdate := false;
  UpdateListView;
end;

procedure TAdvResponsiveListDesignerForm.btnTemplateClick(Sender: TObject);
begin
  SetTemplate(tidContent);
end;

procedure TAdvResponsiveListDesignerForm.FormCanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  if csLoading in ComponentState then
    Exit;

  // minimum size requirements
  Resize := (NewWidth > pnlProp.Width + (Width - ClientWidth)) and (NewHeight > 460);
end;

procedure TAdvResponsiveListDesignerForm.FormCreate(Sender: TObject);
var
  inf: string;
begin
  UpdateListView;

  inf := '-1 = '+ chr($221E);

  ckWF.Caption := inf;
  ckHF.Caption := inf;
  ckWT.Caption := inf;
  ckHT.Caption := inf;
  Height := 674;
end;

procedure TAdvResponsiveListDesignerForm.ListResize(Sender: TObject);
var
  rc: TResponsiveCondition;
begin
  if csLoading in ComponentState then
    Exit;

  if FUpdatingSize then
    Exit;

  FUpdatingSize := true;

  rc := List.GetCondition;
  if Assigned(rc) then
  begin
    if lstConditions.Items.Count > rc.Index then
      lstConditions.ItemIndex := rc.Index;
  end;

  FUpdatingSize := false;
end;

procedure TAdvResponsiveListDesignerForm.lstConditionsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if csLoading in ComponentState then
    Exit;

  if lstConditions.ItemIndex <> -1 then
  begin
    FUpdatingList := true;
    UpdateSpinEdits;
    UpdateListSize;
    FUpdatingList := false;
  end;
end;

procedure TAdvResponsiveListDesignerForm.pbVertPaint(Sender: TObject);
var
  i: integer;
  hf,ht: integer;
  brsh: boolean;
begin
  brsh := true;

  for i := 0 to List.Conditions.Count - 1 do
  begin
    hf := 0;
    ht := pbVert.Height - 1;

    if List.Conditions[i].HeightFrom > - 1 then
      hf := List.Conditions[i].HeightFrom;

    if List.Conditions[i].HeightTo > - 1 then
      ht := List.Conditions[i].HeightTo;

    if brsh then
      pbVert.Canvas.Brush.Color := clInfoBk
    else
      pbVert.Canvas.Brush.Color := clWhite;

    brsh := not brsh;

    pbVert.Canvas.Pen.Color := clSilver;
    pbVert.Canvas.Pen.Style := psSolid;

    pbVert.Canvas.Rectangle(Rect(0, hf, 6, ht));
  end;

end;

procedure TAdvResponsiveListDesignerForm.pbHorzPaint(Sender: TObject);
var
  i: integer;
  wf,wt: integer;
  brsh: boolean;
begin
  brsh := true;

  for i := 0 to List.Conditions.Count - 1 do
  begin
    wf := 0;
    wt := pbHorz.Width - 1;

    if List.Conditions[i].WidthFrom > - 1 then
      wf := List.Conditions[i].WidthFrom;

    if List.Conditions[i].WidthTo > - 1 then
      wt := List.Conditions[i].WidthTo;

    if brsh then
      pbHorz.Canvas.Brush.Color := clInfoBk
    else
      pbHorz.Canvas.Brush.Color := clWhite;

    brsh := not brsh;

    pbHorz.Canvas.Pen.Color := clSilver;
    pbHorz.Canvas.Pen.Style := psSolid;

    pbHorz.Canvas.Rectangle(Rect(wf + 6, 0, Min(pbHorz.Width - 1, wt + 6),pbHorz.Height));
  end;
end;

procedure TAdvResponsiveListDesignerForm.SetTemplate(templateID: TTemplateID);
var
  htmle: THTMLEditor;
  i: integer;
begin
  i := lstConditions.ItemIndex;
  if i = -1 then
  begin
    ShowMessage('No condition selected');
    Exit;
  end;
  //
  htmle := THTMLEditor.Create(Self);

  try
    case templateID of
      tidContent:
        begin
          htmle.Memo1.Lines.Text := List.Conditions[i].Template;
          htmle.Caption := 'Set item content template';
        end;
      tidHeader:
        begin
          htmle.Memo1.Lines.Text := List.Conditions[i].HeaderTemplate;
          htmle.Caption := 'Set item header template';
        end;
      tidFooter:
        begin
          htmle.Memo1.Lines.Text := List.Conditions[i].FooterTemplate;
          htmle.Caption := 'Set item footer template';
        end;
    end;

    if htmle.ShowModal = mrOK then
    begin
      case templateID of
      tidContent: List.Conditions[i].Template := htmle.Memo1.Lines.Text;
      tidHeader: List.Conditions[i].HeaderTemplate := htmle.Memo1.Lines.Text;
      tidFooter: List.Conditions[i].FooterTemplate := htmle.Memo1.Lines.Text;
      end;
    end;
  finally
    htmle.Free;
  end;

end;

procedure TAdvResponsiveListDesignerForm.SpeedButton1Click(Sender: TObject);
begin
  List.Conditions.Add;

  if List.Conditions.Count > 1 then
    List.Conditions[List.Conditions.Count - 1].WidthFrom := List.Conditions[List.Conditions.Count - 2].WidthTo;

  UpdateListView;
  lstConditions.ItemIndex := List.Conditions.Count - 1;
  lstConditions.Items[lstConditions.ItemIndex].MakeVisible(false);
  UpdateSpinEdits;
end;

procedure TAdvResponsiveListDesignerForm.SpeedButton2Click(Sender: TObject);
var
  i: integer;
begin
  i := lstConditions.ItemIndex;
  if i = -1 then
    Exit;

  List.Conditions.Delete(i);
  lstConditions.Items.Delete(i);
  if i < lstConditions.Items.Count then
    lstConditions.ItemIndex := i
  else
    if (lstConditions.Items.Count > 0) and (i > 0) then
      lstConditions.ItemIndex := i - 1;
end;

procedure TAdvResponsiveListDesignerForm.SpeedButton3Click(Sender: TObject);
var
  idx: integer;
begin
  List.Conditions.Add;

  idx := lstConditions.ItemIndex;

  if (List.Conditions.Count > 1) and (idx >= 0) then
    List.Conditions[List.Conditions.Count - 1].Assign(List.Conditions[idx]);

  UpdateListView;
  lstConditions.ItemIndex := List.Conditions.Count - 1;

  lstConditions.Items[lstConditions.ItemIndex].MakeVisible(false);
  UpdateSpinEdits;
end;

procedure TAdvResponsiveListDesignerForm.SpeedButton4Click(Sender: TObject);
var
  cond: TResponsiveCondition;
  idx: integer;
begin
  if lstConditions.ItemIndex > 0 then
  begin
    idx := lstConditions.ItemIndex;
    cond := List.Conditions.Add;

    cond.Assign(List.Conditions[lstConditions.ItemIndex - 1]);

    List.Conditions[lstConditions.ItemIndex - 1].Assign(List.Conditions[lstConditions.ItemIndex]);

    List.Conditions[lstConditions.ItemIndex].Assign(cond);

    List.Conditions.Delete(List.Conditions.Count - 1);

    UpdateListView;
    lstConditions.ItemIndex := idx - 1;
  end;
end;

procedure TAdvResponsiveListDesignerForm.SpeedButton5Click(Sender: TObject);
var
  cond: TResponsiveCondition;
  idx: integer;
begin
  if (lstConditions.ItemIndex >= 0) and (lstConditions.ItemIndex < List.Conditions.Count - 1) then
  begin
    idx := lstConditions.ItemIndex;
    cond := List.Conditions.Add;

    cond.Assign(List.Conditions[lstConditions.ItemIndex + 1]);

    List.Conditions[lstConditions.ItemIndex + 1].Assign(List.Conditions[lstConditions.ItemIndex]);

    List.Conditions[lstConditions.ItemIndex].Assign(cond);

    List.Conditions.Delete(List.Conditions.Count - 1);

    UpdateListView;
    lstConditions.ItemIndex := idx + 1;
  end;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit1Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckWF.Checked := SpinEdit1.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit1Exit(Sender: TObject);
begin
  if not FUpdatingList then
    UpdateConditions;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit2Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckWT.Checked := SpinEdit2.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit3Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckHF.Checked := SpinEdit3.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit4Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckHT.Checked := SpinEdit4.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit5Exit(Sender: TObject);
begin
  if (SpinEdit6.Value <> 0) and (SpinEdit5.Value <> 0) then
    SpinEdit6.Value := 0;

  if not FUpdatingList then
    UpdateConditions;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit6Exit(Sender: TObject);
begin
  if (SpinEdit6.Value <> 0) and (SpinEdit5.Value <> 0) then
    SpinEdit5.Value := 0;

  if not FUpdatingList then
    UpdateConditions;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit7Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckWFull.Checked := SpinEdit7.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.SpinEdit8Change(Sender: TObject);
begin
  FProgUpdate := true;
  ckHFull.Checked := SpinEdit8.Value = -1;
  FProgUpdate := false;
end;

procedure TAdvResponsiveListDesignerForm.UpdateConditions;
var
  i: integer;
  li: TListItem;

begin
  if csLoading in ComponentState then
    Exit;

  i := lstConditions.ItemIndex;
  if i = -1 then
    Exit;

  List.Conditions[i].WidthFrom := SpinEdit1.Value;
  List.Conditions[i].WidthTo := SpinEdit2.Value;
  List.Conditions[i].HeightFrom := SpinEdit3.Value;
  List.Conditions[i].HeightTo := SpinEdit4.Value;

  List.Conditions[i].Columns := SpinEdit5.Value;
  List.Conditions[i].Rows := SpinEdit6.Value;

  List.Conditions[i].ItemWidth := SpinEdit7.Value;
  List.Conditions[i].ItemHeight := SpinEdit8.Value;

  List.Conditions[i].Margins.Left := SpinEdit9.Value;
  List.Conditions[i].Margins.Top := SpinEdit10.Value;
  List.Conditions[i].Margins.Right := SpinEdit11.Value;
  List.Conditions[i].Margins.Bottom := SpinEdit12.Value;
  List.Conditions[i].Category := SpinEdit13.Value;

  li := lstConditions.Items[i];
  li.Caption := inttostr(SpinEdit1.Value);
  li.SubItems.Clear;
  li.SubItems.Add(inttostr(SpinEdit2.Value));
  li.SubItems.Add(inttostr(SpinEdit3.Value));
  li.SubItems.Add(inttostr(SpinEdit4.Value));

  pbVert.Invalidate;
  pbHorz.Invalidate;
end;

procedure TAdvResponsiveListDesignerForm.UpdateListSize;
var
// wf,hf: integer;
//  wt,ht: integer;
  i: integer;
begin
  if FUpdatingSize then
    Exit;

  FUpdatingSize := true;

  i := lstConditions.ItemIndex;
  if i = -1 then
    Exit;

//  wf := StrToInt(lstConditions.Items[i].Caption);
//  wt := StrToInt(lstConditions.Items[i].SubItems[0]);

//  hf := StrToInt(lstConditions.Items[i].SubItems[1]);
  //ht := StrToInt(lstConditions.Items[i].SubItems[2]);

//  if (wt > 0) then
//    Self.ClientWidth := pnlProp.Width + wt - 1 + 6;
//  else
//    if (wf = 0) and (wt > 0) then
//          Self.ClientWidth := pnlProp.Width + wt;


//  if (ht > 0) then
//    Self.ClientHeight := pnlbtn.Height + ht + 6;
//  else
//    if (hf = 0) and (ht > 0) then
//          Self.ClientHeight := pnlProp.Height + ht;

  FUpdatingSize := false;
end;

procedure TAdvResponsiveListDesignerForm.UpdateListView;
var
  i: integer;
  li: TListItem;
begin
  lstConditions.Items.Clear;

  for i := 0 to List.Conditions.Count - 1 do
  begin
    li := lstConditions.Items.Add;
    li.Caption := inttostr(List.Conditions[i].WidthFrom);
    li.SubItems.Add(inttostr(List.Conditions[i].WidthTo));
    li.SubItems.Add(inttostr(List.Conditions[i].HeightFrom));
    li.SubItems.Add(inttostr(List.Conditions[i].HeightTo));
  end;

  if (List.Conditions.Count > 0) and (lstConditions.ItemIndex = -1) then
    lstConditions.ItemIndex := 0;

  pbVert.Invalidate;
  pbHorz.Invalidate;
end;

procedure TAdvResponsiveListDesignerForm.UpdateSpinEdits;
var
  i: integer;
begin
  i := lstConditions.ItemIndex;
  if i = -1 then
    Exit;

  SpinEdit1.Value := List.Conditions[i].WidthFrom;
  SpinEdit2.Value := List.Conditions[i].WidthTo;
  SpinEdit3.Value := List.Conditions[i].HeightFrom;
  SpinEdit4.Value := List.Conditions[i].HeightTo;

  SpinEdit5.Value := List.Conditions[i].Columns;
  SpinEdit6.Value := List.Conditions[i].Rows;

  SpinEdit7.Value := List.Conditions[i].ItemWidth;
  SpinEdit8.Value := List.Conditions[i].ItemHeight;

  SpinEdit9.Value := List.Conditions[i].Margins.Left;
  SpinEdit10.Value := List.Conditions[i].Margins.Top;
  SpinEdit11.Value := List.Conditions[i].Margins.Right;
  SpinEdit12.Value := List.Conditions[i].Margins.Bottom;
  SpinEdit13.Value := List.Conditions[i].Category;
end;

procedure ShowDesigner(List: TAdvResponsiveList);
var
  Dsgnr: TAdvResponsiveListDesignerForm;
begin
  Dsgnr := TAdvResponsiveListDesignerForm.Create(Application);
  Dsgnr.List.Assign(List);

  if Dsgnr.ShowModal = mrOK then
  begin
    List.BeginUpdate;
    List.Assign(Dsgnr.List);
    List.EndUpdate;
  end;
end;

end.
