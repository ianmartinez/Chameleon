program WallShifter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, synapse, MainForm, imagebutton, Settings, AboutForm, VersionSupport,
  SettingsForm;

{$R *.res}

begin
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TWallShifterForm, WallShifterForm);
  Application.CreateForm(TAboutDialog, AboutDialog);
  Application.CreateForm(TSettingsDialog, SettingsDialog);
  Application.Run;
end.

