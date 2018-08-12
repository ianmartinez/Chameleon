unit ImageButton;

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
    lblTitle: TLabel;
    procedure btnPickImageClick(Sender: TObject);
    function GetTitle(): string;
    procedure SetTitle(Value: string);
  private

  public
       SettingKey : string;
       property Title : string read GetTitle write SetTitle;
  end;

implementation
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

function TImageButtonFrame.GetTitle() : string;
begin
  Result := lblTitle.Caption;
end;

procedure TImageButtonFrame.SetTitle(Value: string);
begin
  lblTitle.Caption := Value;
end;
end.

