unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
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

end.

