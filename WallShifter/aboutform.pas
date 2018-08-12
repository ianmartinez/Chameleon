unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  , cthreads,
  {$ENDIF}{$ENDIF}
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  ;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    imgIcon: TImage;
    lblProgramName: TLabel;
    lblAuthor: TLabel;
    txtAbout: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutDialog: TAboutDialog;
  FileVerInfo: TFileVersionInfo;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.FormCreate(Sender: TObject);
begin     
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    lblProgramName.Caption := 'WallShifter ' + FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

end.

