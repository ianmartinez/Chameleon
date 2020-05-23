unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VersionSupport, lclintf, ComCtrls;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    GitHubButton: TButton;
    WebsiteButton: TButton;
    LogoImage: TImage;
    PatreonImage: TImage;
    PatreonLinkLabel: TLabel;
    ProgramNameLabel: TLabel;
    VersionLabel: TLabel;
    InfoPageControl: TPageControl;
    LabelPanel: TPanel;
    TopPanel: TPanel;
    LicenseTab: TTabSheet;
    ChangelogTab: TTabSheet;
    LicenseMemo: TMemo;
    ChangelogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure GitHubButtonClick(Sender: TObject);
    procedure PatreonLinkLabelClick(Sender: TObject);
    procedure WebsiteButtonClick(Sender: TObject);
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
    VersionLabel.Caption := 'Version ' + VersionSupport.GetFileVersion;
end;

procedure TAboutDialog.GitHubButtonClick(Sender: TObject);
begin                     
  OpenURL('https://ianmtz.com/Chameleon');
end;

procedure TAboutDialog.PatreonLinkLabelClick(Sender: TObject);
begin
  OpenURL('https://www.patreon.com/ianmartinez');
end;

procedure TAboutDialog.WebsiteButtonClick(Sender: TObject);
begin                      
  OpenURL('https://github.com/ianmartinez/Chameleon');
end;

end.

