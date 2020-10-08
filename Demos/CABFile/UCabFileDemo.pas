{********************************************************************}
{ TCABFILE DEMO                                                      }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998 - 2019                                 }
{            Email : info@tmssoftware.com                            }
{            Website : https://www.tmssoftware.com                   }
{                                                                    }
{********************************************************************}

unit UCabFileDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cabfiles, ComCtrls, ExtCtrls, slstbox, ShellAPI;

type
  TForm1 = class(TForm)
    CABFile1: TCABFile;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    RelPath: TEdit;
    Label1: TLabel;
    CompressGroup: TRadioGroup;
    SFX: TCheckBox;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    GroupBox2: TGroupBox;
    Button6: TButton;
    Button5: TButton;
    Button7: TButton;
    Label19: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CABFile1CompressProgress(sender: TObject; pos, tot: Int64);
    procedure Button7Click(Sender: TObject);
    procedure CABFile1DecompressProgress(sender: TObject; filename: String;
      pos, tot: Int64);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  if Opendialog1.Execute then
  begin
    CABFile1.CABFile := OpenDialog1.FileName;
    CABFile1.GetContents;
    ListView1.Items.Clear;

    for i := 1 to CABFile1.CABFileContents.Count do
    begin
      with ListView1.items.add do
      begin
        Caption := CABFile1.CABFileContents.Items[i - 1].Name;
        SubItems.Add(DateToStr(CABFile1.CABFileContents.Items[i - 1].Date));
        SubItems.Add(IntToStr(CABFile1.CABFileContents.Items[i - 1].Size));
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if RelPath.Text <> '' then
  begin
    CABFile1.TargetPath := RelPath.Text;
    CABFile1.ExtractAll;
  end
  else
    ShowMessage('Please fill in a relative path where you want to extract the files to. (E.g. ''./'')');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if RelPath.Text <> '' then
  begin
    CABFile1.TargetPath := RelPath.Text;
    if Assigned(listview1.Selected) then
      CABFile1.ExtractFile(listview1.Selected.Caption);
  end
  else
    ShowMessage('Please fill in a relative path where you want to extract the files to. (E.g. ''./'')');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i: Integer;
begin
  if RelPath.Text <> '' then
  begin
    CABFile1.CABFileContents.SelectNone;
    for i := 1 to listview1.Items.Count do
    begin
      CABFile1.CABFileContents.Items[i-1].Selected := ListView1.Items[i - 1].Selected;
    end;

    CABFile1.ExtractSelected;
  end
  else
    ShowMessage('Please fill in a relative path where you want to extract the files to. (E.g. ''./'')');
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  I: Integer;

begin
  if listview1.items.Count=0 then
  begin
    ShowMessage('No files selected for compression');
    Exit;
  end;

  case CompressGroup.ItemIndex of
  0: CABFile1.CompressionType := typMSZIP;
  1: CABFile1.CompressionType := typLZX;
  end;

  if SaveDialog1.Execute then
  begin
    CABFile1.CABFile := SaveDialog1.FileName;

    if ExtractFileExt(CABFile1.CABFile) = '' then
      CABFile1.CABFile := CABFile1.CABFile + '.CAB';

    if UpperCase(ExtractFileExt(CABFile1.CABFile)) <> '.CAB' then
    begin
      ShowMessage('Invalid file extension = ' + ExtractFileExt(CABFile1.CABFile));
      Exit;
    end;

    if not FileExists(CABFile1.CABFile) then
    begin
      CABFile1.CABFileContents.Clear;

      for I := 1 to ListView1.Items.Count do
      begin
        with  CABFile1.CABFileContents.Add do
        begin
          Name := ListView1.Items[I - 1].Caption;
          RelPath := ExtractFilePath(ListView1.Items[I - 1].Subitems[2]);
        end;
      end;

      if SFX.Checked then
      begin
        if CABFile1.MakeSFX then
        begin
          ShowMessage('Self extracting EXE file created' + #13 + 'Compression ratio = ' + Format('%.2f', [CABFile1.CompressionRatio]) + '%');
        end;
      end
      else
      begin
        if CABFile1.Compress then
        begin
          ShowMessage('CAB file created' + #13 + 'Compression ratio = ' + Format('%.2f', [CABFile1.CompressionRatio]) + '%');
        end;
      end;
    end
    else
      ShowMessage('Cannot add files to existing CAB file');
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  I: integer;
  RP: string;

  function GetFileSize(AFilename: string):integer;
  var
    F: file of byte;
  begin
    Result := 0;
    AssignFile(F, AFilename);
    {$i-}
    Reset(F);
    {$i+}
    if IOResult = 0 then
    begin
      Result := FileSize(F);
      CloseFile(F);
    end;
  end;

begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  OpenDialog1.Filter := 'All files (*.*)|*.*';
  if OpenDialog1.Execute then
  begin
    RP := RelPath.Text;

    if RP <> '' then
      if RP[Length(RP)] <> '\' then
        RP := RP + '\';
    if (Length(RP) > 0) then
      if RP[1] = '\' then
        Delete(RP, 1, 1);

    for I := 1 to OpenDialog1.Files.Count do
    with ListView1.Items.Add do
    begin
      Caption := OpenDialog1.Files[I - 1];
      SubItems.Add(DateToStr(FileDateToDateTime(FileAge(OpenDialog1.Files[I - 1]))));
      SubItems.Add(IntToStr(GetFilesize(OpenDialog1.Files[I - 1])));
      SubItems.Add(RP + ExtractFileName(OpenDialog1.Files[I - 1]));
    end;
  end;

  OpenDialog1.Filter := 'CAB files (*.CAB)|*.cab|All files (*.*)|*.*';
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
end;

procedure TForm1.CABFile1CompressProgress(sender: TObject; pos,
  tot: Int64);
begin
  if tot = 0 then
    Exit;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ListView1.Items.Clear;
end;

procedure TForm1.CABFile1DecompressProgress(sender: TObject;
  Filename: String; Pos, Tot: Int64);
begin
  ProgressBar1.Position := Round(Pos / Tot * 100);
end;

procedure TForm1.Label19Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://tmssoftware.com/site/tmsvcluipack.asp', nil, nil, SW_SHOWNORMAL);
end;

end.
