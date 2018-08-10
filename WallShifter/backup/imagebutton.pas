unit imagebutton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ExtDlgs;

type

  { TImageButtonFrame }

  TImageButtonFrame = class(TFrame)
    btnPickImage: TButton;
    imgPreview: TImage;
    dlgOpenPicture: TOpenPictureDialog;
    procedure btnPickImageClick(Sender: TObject);
  private

  public

  end;

implementation
  var
    SettingName : String;
{$R *.lfm}

{ TImageButtonFrame }

procedure TImageButtonFrame.btnPickImageClick(Sender: TObject);
begin
  if dlgOpenPicture.Execute then
    if FileExists(dlgOpenPicture.FileName) then
      imgPreview.Picture.LoadFromFile(dlgOpenPicture.FileName)
    else
      raise Exception.Create('File does not exist.');
end;

end.

