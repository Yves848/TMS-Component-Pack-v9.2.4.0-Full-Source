{***************************************************************************}
{ TWebPictureContainer                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 2006 - 2016                                                 }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit WebPictureContainer;

{$I TMSDEFS.INC}

interface

uses
  Classes, PictureContainer, Graphics, AdvGDIP, JPEG, WinInet
  {$IFDEF DELPHI2010_LVL}
  , GIFImg, PngImage
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v2.0.0.0 : New : LoadPicture added to control load mechanism
  //          : New : Capability to load from file added

type
  TWebPictureContainer = class;

  TDownLoadThread = class(TThread)
  private
    FPictureContainer: TWebPictureContainer;
    FWorkerItem: integer;
    FWorkerPosition: int64;
    FWorkerTotalSize: int64;
  protected
    procedure DoProgress;
    procedure DoComplete;
    procedure Execute; override;
    function Download(FISession: HInternet; URL: string): TMemoryStream;
  public
    constructor Create(APictureContainer: TWebPictureContainer);
  end;

  TWebPictureItem = class(TPictureItem)
  private
    FPicture: TAdvGDIPPicture;
    FURL: string;
    FLoaded: boolean;
    FRequested: boolean;
    procedure SetURL(const Value: string);
    procedure SetPicture(const Value: TAdvGDIPPicture);
  protected
    function CreatePicture: TGraphic; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Requested: boolean read FRequested write FRequested;
    property Loaded: boolean read FLoaded write FLoaded;
  published
    property URL: string read FURL write SetURL;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
  end;

  TWebPictureCollection = class(TPictureCollection)
  private
    function GetItem(Index: Integer): TWebPictureItem;
    procedure SetItem(Index: Integer; const Value: TWebPictureItem);
  protected
    function GetItemClass: TCollectionItemClass; override;
    function FindPictureByURL(AURL: string): TGraphic;
  public
    function Add: TWebPictureItem;
    property Items[Index: Integer]: TWebPictureItem read GetItem write SetItem; default;
  published
  end;

  TDownloadCompleteEvent = procedure(Sender: TObject; ItemIndex: integer) of object;
  TDownloadProgressEvent = procedure(Sender: TObject; ItemIndex: integer; Position, TotalSize: int64) of object;

  TLoadPicture = (lpAlways, lpOnRequest);

  TWebPictureContainer = class(TPictureContainer)
  private
    FPicture: TPicture;
    FDownloader: TDownLoadThread;
    FDefaultPicture: TAdvGDIPPicture;
    FDefaultPic: TPicture;
    FOnDownloadComplete: TDownloadCompleteEvent;
    FOnDownloadProgress: TDownloadProgressEvent;
    FLoadPicture: TLoadPicture;
    function GetItems: TWebPictureCollection;
    procedure SetItems(const Value: TWebPictureCollection);
    procedure SetDefaultPicture(const Value: TAdvGDIPPicture);
  protected
    function CreatePicture(WebPic: TAdvGDIPPicture): TPicture; overload;
    function CreatePicture(WebPic: TGraphic): TPicture; overload;
    function CreateItems: TPictureCollection; override;
    procedure DoDownloadComplete(ItemIndex: integer); virtual;
    procedure DoDownloadProgress(ItemIndex: integer; Position,TotalSize: int64); virtual;
    function GetVersionNr: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindPictureByURL(AURL: string): TGraphic;
    function FindPicture(s: string): TGraphic; override;
    function FindBitmap(s: string): TPicture; override;
  published
    property DefaultPicture: TAdvGDIPPicture read FDefaultPicture write SetDefaultPicture;
    property Items: TWebPictureCollection read GetItems write SetItems;
    property LoadPicture: TLoadPicture read FLoadPicture write FLoadPicture default lpAlways;
    property OnDownloadComplete: TDownloadCompleteEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadProgress: TDownloadProgressEvent read FOnDownloadProgress write FOnDownloadProgress;
  end;


implementation

uses
  Windows, SysUtils
  {$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
  {$ENDIF}
  ;

const
  MinGraphicSize = 44;


function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := Graphics.TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    {$IFDEF DELPHIXE_LVL}
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
    {$ENDIF}
  else
    {$IFDEF DELPHI2010_LVL}
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else
    {$ENDIF}
    if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    {$IFDEF DELPHI2010_LVL}
    {$IFDEF DELPHIXE4_LVL}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
    {$ENDIF}
      GraphicClass := TGIFImage
    {$ENDIF}
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

{ TWebPictureCollection }

function TWebPictureCollection.Add: TWebPictureItem;
begin
  Result := TWebPictureItem(inherited Add);
end;

function TWebPictureCollection.FindPictureByURL(AURL: string): TGraphic;
var
  s: string;
  i: Integer;
begin
  Result := nil;
  s := Uppercase(AURL);
  i := 1;
  while i <= Count do
  begin
    if Uppercase(Items[i - 1].URL) = s then
    begin
      Result := Items[i - 1].Picture;
      Break;
    end;
    Inc(i);
  end;
end;

function TWebPictureCollection.GetItem(Index: Integer): TWebPictureItem;
begin
  Result := TWebPictureItem(inherited Items[Index]);
end;

function TWebPictureCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TWebPictureItem;
end;

procedure TWebPictureCollection.SetItem(Index: Integer;
  const Value: TWebPictureItem);
begin
  inherited Items[Index] := Value;
end;

{ TWebPictureContainer }

constructor TWebPictureContainer.Create(AOwner: TComponent);
begin
  inherited;

  FLoadPicture := lpAlways;
  FDownloader := nil;
  FDefaultPicture := TAdvGDIPPicture.Create;
  if not (csDesigning in ComponentState) then
  begin
    FDownloader := TDownloadThread.Create(Self);
    FDownloader.Priority := tpLower;
    FDownloader.FreeOnTerminate := true;
  end;
end;

function TWebPictureContainer.CreateItems: TPictureCollection;
begin
  Result := TWebPictureCollection.Create(Self);
end;

function TWebPictureContainer.CreatePicture(WebPic: TGraphic): TPicture;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Result := nil;

  Stream := TMemoryStream.Create;
  try
    WebPic.SaveToStream(Stream);
    if Stream.Size = 0 then
      Exit;

    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      Exit
    else
    begin
      Graphic := GraphicClass.Create;
      Stream.Position := 0;
      try
        Graphic.LoadFromStream(Stream);
        Result := TPicture.Create;
        Result.Assign(Graphic);
      finally
        Graphic.Free;
      end;
    end;
  finally
    Stream.Free;
  end;

end;

function TWebPictureContainer.CreatePicture(WebPic: TAdvGDIPPicture): TPicture;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Result := nil;

  Stream := TMemoryStream.Create;
  try
    WebPic.SaveToStream(Stream);
    if Stream.Size = 0 then
      Exit;

    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      Exit
    else
    begin
      Graphic := GraphicClass.Create;
      Stream.Position := 0;
      try
        Graphic.LoadFromStream(Stream);
        Result := TPicture.Create;
        Result.Assign(Graphic);
      finally
        Graphic.Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

destructor TWebPictureContainer.Destroy;
begin
  FDefaultPicture.Free;
  if Assigned(FDownloader) then
    FDownloader.Terminate;

  inherited;
end;

procedure TWebPictureContainer.DoDownloadComplete(ItemIndex: integer);
begin
  if Assigned(OnDownloadComplete) then
    OnDownloadComplete(Self, ItemIndex);
  Changed;
end;

procedure TWebPictureContainer.DoDownloadProgress(ItemIndex: integer; Position,
  TotalSize: int64);
begin
  if Assigned(OnDownloadProgress) then
    OnDownloadProgress(Self, ItemIndex, Position, TotalSize);
end;

function TWebPictureContainer.FindBitmap(s: string): TPicture;
var
  i: Integer;
begin
  Result := nil;

  if Assigned(FPicture) then
  begin
    FPicture.Free;
    FPicture := nil;
  end;

  s := Uppercase(s);
  i := 1;

  while i <= Items.Count do
  begin
    if (Uppercase(Items[i - 1].Name) = s) then
    begin
      if (Items[i - 1].Loaded) then
      begin
        FPicture := CreatePicture(Items[i - 1].Picture);
        Result := FPicture;
        Break;
      end
      else
        Items[i - 1].Requested := true;
    end;
    Inc(i);
  end;

  if not Assigned(Result) then
  begin
    if not Assigned(FDefaultPic) then
      FDefaultPic := CreatePicture(DefaultPicture);

    Result := FDefaultPic;
  end;
end;

function TWebPictureContainer.FindPicture(s: string): TGraphic;
var
  i: Integer;
begin
  Result := nil;
  for i := 1 to Items.Count do
  begin
    if (Items[i - 1].Name = s) and (Items[i - 1].Loaded) then
    begin
      Result := Items[i - 1].Picture;
      Break;
    end;
  end;

  if not Assigned(Result) then
    Result := DefaultPicture;
end;

function TWebPictureContainer.FindPictureByURL(AURL: string): TGraphic;
begin
  Result := Items.FindPictureByURL(AURL)
end;

function TWebPictureContainer.GetItems: TWebPictureCollection;
begin
  Result := TWebPictureCollection(inherited Items);
end;

function TWebPictureContainer.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebPictureContainer.SetDefaultPicture(const Value: TAdvGDIPPicture);
begin
  FDefaultPicture.Assign(Value);
end;

procedure TWebPictureContainer.SetItems(const Value: TWebPictureCollection);
begin
  (inherited Items).Assign(Value);
end;

{ TWebPictureItem }

procedure TWebPictureItem.Assign(Source: TPersistent);
begin
  if (Source is TWebPictureItem) then
  begin
    FPicture.Assign((Source as TWebPictureItem).Picture);
  end;
end;

constructor TWebPictureItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TAdvGDIPPicture.Create;
  FRequested := false;
  FLoaded := false;
end;

function TWebPictureItem.CreatePicture: TGraphic;
begin
  Result := TAdvGDIPPicture.Create;
end;

destructor TWebPictureItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TWebPictureItem.SetPicture(const Value: TAdvGDIPPicture);
begin
  FPicture.Assign(Value);
end;

procedure TWebPictureItem.SetURL(const Value: string);
begin
  if (FURL <> Value) then
  begin
    FURL := Value;
    FLoaded := false;
  end;
end;

{ TDownLoadThread }

constructor TDownLoadThread.Create(APictureContainer: TWebPictureContainer);
begin
  inherited Create(False);
  FPictureContainer := APictureContainer;
end;

procedure TDownLoadThread.DoComplete;
begin
  FPictureContainer.DoDownloadComplete(FWorkerItem);
end;

procedure TDownLoadThread.DoProgress;
begin
  FPictureContainer.DoDownloadProgress(FWorkerItem,FWorkerPosition,FWorkerTotalSize);
end;

function TDownLoadThread.Download(FISession: HInternet; URL: string): TMemoryStream;
var
  RBSIZE:dword;
  httpstatus,httpsize,err:integer;
  dwIdx:dword;
  dwBufSize:dword;
  ms:TMemoryStream;
  len:dword;
  cbuf:array[0..255] of char;
  rb:array[0..4095] of byte;
  FIHttp:hinternet;
  Cancel:boolean;

begin
  Result := nil;

  fIHttp := InternetOpenURL(FISession,pchar(URL),nil,0,
   INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD,0);

  if (fIHttp = nil) then
  begin
    //InternetCloseHandle(fISession);
    //DownLoadError('Cannot open http connection');
    //fThreadBusy:=false;
    Exit;
  end;

  try
    dwBufSize := SizeOf(cbuf);
    dwidx := 0;
    HttpQueryInfo(fIHttp,HTTP_QUERY_STATUS_CODE,@cbuf,dwBufSize,dwIdx);

    val(cbuf,httpstatus,err);
    if (httpstatus <> 200) or (err <> 0) then
    begin
      //InternetCloseHandle(fISession);
      InternetCloseHandle(fIHttp);
      //DownLoadError('Cannot open URL '+furl);
      //FThreadBusy:=false;
      Exit;
    end;

    dwBufSize := SizeOf(cbuf);
    dwidx := 0;
    HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_TYPE,@cbuf,dwBufSize,dwIdx);

    if Pos('IMAGE',UpperCase(StrPas(cbuf))) = 0 then
    begin
      //InternetCloseHandle(fISession);
      InternetCloseHandle(fIHttp);
      //DownLoadError('Resource is not of image type : ' + FUrl);
      //fThreadBusy := false;
      Exit;
    end;

    dwBufSize := SizeOf(cbuf);
    dwidx := 0;
    HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_LENGTH,@cbuf,dwBufSize,dwIdx);

    val(cbuf,httpsize,err);
    if (httpsize = 0) or (err <> 0) then
    begin
      InternetCloseHandle(fISession);
      InternetCloseHandle(fIHttp);
      //DownLoadError('Image size is 0');
      //fThreadBusy:=false;
      Exit;
    end;

    //DownLoadProgress(0,httpsize);

    len := 4096;
    RBSIZE := 4096;

    ms := TMemoryStream.Create;

    cancel := false;

    FWorkerTotalSize := httpsize;

    while (len = RBSIZE) and not Cancel do
    begin
      InternetReadFile(fIHttp,@rb,RBSIZE,len);
      if len>0 then ms.WriteBuffer(rb,len);
      //DownLoadProgress(ms.Size,httpsize);
      //DownLoadCancel(cancel);
      FWorkerPosition := ms.Size;
      Synchronize(DoProgress);
    end;

    if not cancel then
    begin
      Result := ms;
      //ms.Position := 0;
      //LoadFromStream(ms);
    end;

      FWorkerPosition := ms.Size;
      Synchronize(DoProgress);
    //ms.Free;
  finally
    InternetCloseHandle(fIHttp);
   // InternetCloseHandle(fISession);

    //FThreadBusy:=false;
  end;
end;

procedure TDownLoadThread.Execute;
var
  i: integer;
  ms: TMemoryStream;
  FISession:hinternet;

begin
  fISession := InternetOpen('WebPictureContainer',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if (fISession = nil) then
  begin
    //DownLoadError('Cannot open internet session');
    //fThreadBusy:=false;
    Exit;
  end;

  while not Terminated do
  begin
    for I := 0 to FPictureContainer.Items.Count - 1 do
    begin
      if (not FPictureContainer.Items[I].Loaded) and
        (FPictureContainer.Items[I].Requested or (FPictureContainer.LoadPicture = lpAlways)) and
        (FPictureContainer.Items[i].URL <> '') then
      begin
        FWorkerItem := i;
        FWorkerPosition := 0;
        FWorkerTotalSize := 0;
        ms := nil;

        try
          if (pos('HTTP:',Uppercase(FPictureContainer.Items[I].URL)) = 1) then
          begin
            ms := Download(FISession, FPictureContainer.Items[I].URL);

            if Assigned(ms) then
            begin
              ms.Position := 0;
              FPictureContainer.Items[I].Picture.LoadFromStream(ms);
              FPictureContainer.Items[I].Loaded := true;
            end;
          end
          else
          begin
            if FileExists(FPictureContainer.Items[I].URL) then
            begin
              try
                FPictureContainer.Items[I].Picture.LoadFromFile(FPictureContainer.Items[I].URL);
                FPictureContainer.Items[I].Loaded := true;
              except
              end;
            end;
          end;
        finally
          if Assigned(ms) then
            ms.Free;
        end;

        Synchronize(DoComplete);
      end;
    end;
    sleep(100);
  end;

  InternetCloseHandle(fISession);
end;




end.
