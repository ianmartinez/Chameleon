program Chameleon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, imagebutton, Settings, AboutForm, VersionSupport,
  SettingsForm, Win32, SysUtils;

{$R *.res}
var
  ShouldLoad: boolean;
begin
  ShouldLoad := True;
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;

  (* Load the settings *)
  ProgramSettings := LoadSettings();

  (* Check for command line arguments *)
  if ParamCount > 0 then begin
   (* The argument -a means to start running in the background
   automatically *)
   if ParamStr(1).Equals('-a') then begin
     ProgramSettings.LaunchedWithAutoStart := True;
   end
   (* The argument -s means to start running in the background
   automatically, only if 'Run at Startup'  is checked *)
   else if ParamStr(1).Equals('-s') then begin
     if ProgramSettings.RunAtStartup then begin
        ProgramSettings.LaunchedWithAutoStart := True;
     end else begin (* If not, just close *)
       ShouldLoad := False;
       Application.Terminate();
     end;
   end;
  end;

  if ShouldLoad then begin
    Application.CreateForm(TChameleonForm, ChameleonForm);
    Application.CreateForm(TAboutDialog, AboutDialog);
    Application.CreateForm(TSettingsDialog, SettingsDialog);
  end;

  Application.Run;
end.

