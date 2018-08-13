unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Weather;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin

end;

procedure TSettingsDialog.Button1Click(Sender: TObject);
begin
  Memo1.Caption :=  NormalizeWeatherCondition(Edit1.Caption);
end;

end.

