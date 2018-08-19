unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VersionSupport, lclintf, ComCtrls;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    btnWebsite: TButton;
    Image1: TImage;
    lblAuthor: TLabel;
    lblProgramName: TLabel;
    PageControl1: TPageControl;
    pnlLabels: TPanel;
    pnlTop: TPanel;
    tsLicense: TTabSheet;
    tsChangelog: TTabSheet;
    txtLicense: TMemo;
    txtChangelog: TMemo;
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

