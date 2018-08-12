program WallShifter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, imagebutton, WeatherReader, Settings, AboutForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TWallShifterForm, WallShifterForm);
  Application.CreateForm(TAboutDIalog, AboutDIalog);
  Application.Run;
end.

