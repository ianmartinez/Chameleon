; Installer for LangPad

#define AppName "Chameleon"
 
; Versioning
#define AppVersion "2.0"   
#define AppVersionStr "2_0"   
#define AppLocation "..\Chameleon\ChameleonRelease.exe"

[Setup]
AppId={{7E322993-B4AD-4AB4-B101-55DF76A5C56F}
AppName={#AppName}
AppVersion={#AppVersion}
WizardStyle=modern
DefaultDirName={autopf}\{#AppName}
DisableProgramGroupPage=yes
Compression=lzma2
SolidCompression=yes
ChangesAssociations=yes
OutputBaseFilename={#AppName}_{#AppVersionStr}_Setup
LicenseFile=..\License
DisableWelcomePage=no

; Set custom messages on welcome screen
[Messages]
WelcomeLabel1=Welcome to the setup wizard for {#AppName} {#AppVersion}.
WelcomeLabel2=Change your wallpaper with the weather, battery level, or time of day.

; Files to copy into program files
[Files]                              
Source: "{#AppLocation}"; DestDir: "{app}";      

; Add to start menu
[Icons]
Name: "{autoprograms}\{#AppName}"; Filename: "{app}\{#AppName}.exe"   

; Allow the user to run the program after setup is complete
[Run]
Filename: "{app}\{#AppName}.exe"; Description: "{cm:LaunchProgram,{#StringChange(AppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

