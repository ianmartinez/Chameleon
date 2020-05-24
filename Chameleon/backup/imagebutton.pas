unit ImageButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ExtDlgs, graphics, Settings, Dialogs;

type

  { TImageButtonFrame }

  TImageButtonFrame = class(TFrame)
    ChangeImageButton: TButton;
    PreviewImage: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    ImageTitleLabel: TLabel;
    MainPanel: TPanel;
    procedure ChangeImageButtonClick(Sender: TObject);
  private

  public   
    SettingCategory : string;
    SettingKey : string;
    FullName: string;
    Thumb: TPicture;
    OverwriteImage: Boolean;
    function GetTitle(): string;
    procedure SetTitle(Value: string);
    procedure LoadThumb();
    procedure SaveThumb(SourceImage: string);
    property Title : string read GetTitle write SetTitle;
  end;

implementation
{$R *.lfm}

{ TImageButtonFrame }

procedure TImageButtonFrame.ChangeImageButtonClick(Sender: TObject);
begin
  OpenPictureDialog.Title := 'Change Wallpaper for ' + ImageTitleLabel.Caption;
  if OpenPictureDialog.Execute then
    if FileExists(OpenPictureDialog.FileName) then begin
      OverwriteImage := true;
      SaveThumb(OpenPictureDialog.FileName);
    end
    else
      raise Exception.Create('File does not exist.');
end;

function TImageButtonFrame.GetTitle() : string;
begin
  Result := ImageTitleLabel.Caption;
end;

procedure TImageButtonFrame.SetTitle(Value: string);
begin
  ImageTitleLabel.Caption := Value;
end;

procedure ResizeBitmap(Bitmap: TBitmap; NewHeight: integer);
var
  buffer: TBitmap;
  NewWidth: integer;
begin
  buffer := TBitmap.Create;
  try
    NewWidth := Round((NewHeight * Bitmap.Width) / Bitmap.Height);
    buffer.SetSize(NewWidth, NewHeight);
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;

procedure TImageButtonFrame.SaveThumb(SourceImage: string);
var
  Picture: TPicture;
  ThumbPath: string;  
  ImagePath: string;
  ThumbDir: string;
  ImageDir: string;
begin   
  ThumbPath :=  GetThumbPath(SettingKey, SettingCategory);  
  ImagePath :=  GetImagePath(SettingKey, SettingCategory);

  if fileexists(SourceImage) then begin
    try
      Picture := TPicture.Create;
      Picture.LoadFromFile(SourceImage);

      (* If loading a new image entirely,
      overwrite the existing image  *)
      if OverwriteImage then begin
        ImageDir := ExtractFilePath(ImagePath);
        if not directoryexists(ImageDir) then
          forcedirectories(ImageDir);

        (* Delete the old image *)
        if fileexists(ImagePath) then
           deletefile(ImagePath);

        (* Copy over the new image *)
        copyfile(SourceImage, ImagePath);
      end;

      (* Generate the thumbnail *)
      ResizeBitmap(Picture.Bitmap, PreviewImage.Height);
      ThumbDir := ExtractFilePath(ThumbPath);

      if not directoryexists(ThumbDir) then
        forcedirectories(ThumbDir);

      Picture.SaveToFile(ThumbPath);
      PreviewImage.Picture := Picture;
    finally
      OverwriteImage := false;
      Picture.Free;
    end;
  end;
end;

procedure TImageButtonFrame.LoadThumb();
var
  ThumbPath: string;
  ImagePath: string;
begin
  ThumbPath :=  GetThumbPath(SettingKey, SettingCategory);
  ImagePath :=  GetImagePath(SettingKey, SettingCategory);

  if not fileexists(ThumbPath) then begin
    SaveThumb(ImagePath);
  end
  else begin
    PreviewImage.Picture.LoadFromFile(ThumbPath);
  end;
end;
end.

