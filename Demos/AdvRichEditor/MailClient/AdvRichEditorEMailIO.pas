{*************************************************************************}
{ TMS TAdvRichEditorEmailIO                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2019                                       }
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

unit AdvRichEditorEMailIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, SysUtils, VCL.Forms, AdvRichEditor, AdvRichEditorBase,
  IdMessage, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase, IdSMTP, idText,
  idAttachmentFile;

type

  {$IFDEF FNCLIB}
  TNVComponent = class(TTMSFNCCustomComponent)
  protected
    function GetInstance: NativeUInt; override;
  end;
  {$ENDIF}

  {$IFNDEF FNCLIB}
  TNVComponent = class(TComponent);
  {$ENDIF}

  TAdvRichEditorEmailIO = class(TNVComponent)
  private
    FRichEditor: TAdvRichEditorBase;
  protected
    procedure SetRichEditor(AValue: TAdvRichEditorBase); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property RichEditor: TAdvRichEditorBase read FRichEditor write SetRichEditor;
    procedure GenerateEmail(idMessage: TIdMessage; ImgPath: string = ''); virtual;
  end;


implementation

type
  TAdvRichEditorBaseEx = class(TAdvRichEditorBase);

  TAdvRichEditorEx = class(TAdvRichEditor);

{ TAdvRichEditorEmailIO }

constructor TAdvRichEditorEmailIO.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;

  if Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if (AOwner.Components[i] is TAdvRichEditor) then
      begin
        FRichEditor := AOwner.Components[i] as TAdvRichEditor;
        break;
      end;
  end;
end;

procedure TAdvRichEditorEmailIO.GenerateEmail(idMessage: TIdMessage; ImgPath: string= '');
var
  sl: TStringList;
  i: integer;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No richeditor assigned');

  sl := TStringList.Create;

  try
    if ImgPath = '' then
      ImgPath := ExtractFilePath(Application.EXEName);

    TAdvRichEditorBaseEx(FRichEditor).HTMLImages := igID;

    FRichEditor.SpaceAsNbsp := false;
    sl.Text := TAdvRichEditorBaseEx(FRichEditor).GetContentAsHTML(False, ImgPath);

    sl.SaveToFile(ImgPath + 'richeditor.html');

    for i := 0 to FRichEditor.HTMLImageList.Count - 1 do
    begin
      FRichEditor.HTMLImageList.Items[i].SaveToFile(ImgPath + 'img'+i.ToString+'.png');
    end;

    idMessage.IsEncoded := True ;
    idMessage.ContentType := 'multipart/alternative';
    idMessage.ClearBody;


    with TIdText.Create(idMessage.MessageParts, nil) do
    begin
      Body.Text := 'Your reader does not support HTML'+ #13#10 +  FRichEditor.PlainText;
      ContentType := 'text/plain';
    end;

    with TIdText.Create(idMessage.MessageParts, nil) do
    begin
      ContentType := 'multipart/related; type="text/html"';
    end;

    with TIdText.Create(idMessage.MessageParts, nil) do
    begin
      Body.LoadFromFile(ImgPath + 'richeditor.html');
      ContentType := 'text/html';
      ParentPart := 1;
    end;

    for i := 0 to FRichEditor.HTMLImageList.Count - 1 do
    begin
      with TIdAttachmentFile.Create(idMessage.MessageParts, ImgPath + 'img' + i.ToString + '.png') do
      begin
        ContentID := 'image'+ i.ToString;
        ContentType := 'image/png';
        ContentDisposition := 'inline';
        ParentPart := 1;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorEmailIO.SetRichEditor(AValue: TAdvRichEditorBase);
begin
  FRichEditor := AValue;
end;

end.
