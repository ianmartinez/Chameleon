program WallShifter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, imagebutton, Settings, AboutForm, VersionSupport;

{$R *.res}

begin
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TWallShifterForm, WallShifterForm);
  Application.CreateForm(TAboutDialog, AboutDialog);
  Application.Run;
end.

