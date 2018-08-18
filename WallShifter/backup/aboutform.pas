unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VersionSupport;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    btnAbout: TButton;
    btnSettings: TButton;
    Image1: TImage;
    lblAuthor: TLabel;
    lblProgramName: TLabel;
    pnlLabels: TPanel;
    pnlTop: TPanel;
    txtAbout: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.FormCreate(Sender: TObject);
begin
    lblProgramName.Caption := 'WallShifter ' + VersionSupport.GetProductVersion;
end;

end.

