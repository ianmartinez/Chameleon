unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VersionSupport, lclintf;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    btnWebsite: TButton;
    Image1: TImage;
    lblAuthor: TLabel;
    lblProgramName: TLabel;
    pnlLabels: TPanel;
    pnlTop: TPanel;
    txtAbout: TMemo;
    procedure btnWebsiteClick(Sender: TObject);
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

procedure TAboutDialog.btnWebsiteClick(Sender: TObject);
begin
  OpenURL('http://www.atlinsoft.com');
end;

end.

