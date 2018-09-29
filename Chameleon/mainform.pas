unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, Spin, ComCtrls, ButtonPanel, Menus, ImageButton,
  Weather, Settings, AboutForm, SettingsForm, VersionSupport, Windows, Win32,
  eventlog, InterfaceBase;

type

  { TChameleonForm }

  TChameleonForm = class(TForm)
    btnSettings: TButton;
    wallLog: TEventLog;
    lblVersion: TLabel;
    lblProgramName: TLabel;
    pnlLabels: TPanel;
    pnlButtons: TButtonPanel;
    HeatIndexBox: TScrollBox;
    HumidityBox: TScrollBox;
    TemperatureBox: TScrollBox;
    tmrTheme: TTimer;
    trayIcon: TTrayIcon;
    WindSpeedBox: TScrollBox;
    ConditionsBox: TScrollBox;
    TimeBox: TScrollBox;
    btnAbout: TButton;
    gbInterval: TGroupBox;
    gbWallpapers: TGroupBox;
    imgLogo: TImage;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    gbChangeBy: TRadioGroup;
    pnlTop: TPanel;
    rbHeatIndex: TRadioButton;
    rbNone: TRadioButton;
    rbBattery: TRadioButton;
    rbTime: TRadioButton;
    rbConditions: TRadioButton;
    rbWindSpeed: TRadioButton;
    rbTemperature: TRadioButton;
    rbHumidity: TRadioButton;
    BatteryBox: TScrollBox;
    spInterval: TSpinEdit;
    tsBattery: TTabSheet;
    tsTime: TTabSheet;
    tsConditions: TTabSheet;
    tsWindSpeed: TTabSheet;
    tsTemperature: TTabSheet;
    tsHeatIndex: TTabSheet;
    tsHumidity: TTabSheet;
    procedure btnAboutClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    function CreateImageButtonFrame(_SettingCategory: string; _SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
    procedure FormWindowStateChange(Sender: TObject);
    procedure ModeChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure spIntervalChange(Sender: TObject);
    procedure tmrThemeTimer(Sender: TObject);
    procedure trayIconDblClick(Sender: TObject);
  private

  public

  end;

var
  ChameleonForm: TChameleonForm;
  ProgramSettings: TProgramSettings;

implementation

{$R *.lfm}

{ TChameleonForm }

procedure TChameleonForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ProgramSettings := LoadSettings();

  case ProgramSettings.Mode of
    pmNone:
      rbNone.Checked := true;
    pmBattery:
      rbBattery.Checked := true;
    pmTime:
      rbTime.Checked := true;
    pmWeatherConditions:
      rbConditions.Checked := true;
    pmWindSpeed:
      rbWindSpeed.Checked := true;
    pmTemperature:
      rbTemperature.Checked := true;
    pmHumidity:
      rbHumidity.Checked := true;
    pmHeatIndex:
      rbHeatIndex.Checked := true;
    else
      rbNone.Checked := true;
  end;

  spInterval.Value := ProgramSettings.Interval;

  // Battery percentage
  for i:= high(PercentageModes) downto low(PercentageModes) do begin
    CreateImageButtonFrame('Battery', WriteSafeString(PercentageModes[i]), PercentageModes[i], BatteryBox);
  end;

  // Time of day
  for i:= high(TimeModes) downto low(TimeModes) do begin
    CreateImageButtonFrame('Time', WriteSafeString(TimeModes[i]), TimeModes[i], TimeBox);
  end;

  // Weather Conditions
  for i:= high(WeatherConditions) downto low(WeatherConditions) do begin
    CreateImageButtonFrame('WeatherConditions', WriteSafeString(WeatherConditions[i]), WeatherConditions[i], ConditionsBox);
  end;

  // Wind speed
  for i:= high(WindSpeedModes) downto low(WindSpeedModes) do begin
    CreateImageButtonFrame('WindSpeed', WriteSafeString(WindSpeedModes[i]), WindSpeedModes[i], WindSpeedBox);
  end;

  // Temperature
  for i:= high(TemperatureModes) downto low(TemperatureModes) do begin
    CreateImageButtonFrame('Temperature', WriteSafeString(TemperatureModes[i]), TemperatureModes[i], TemperatureBox);
  end;

  // Humidity
  for i:= high(PercentageModes) downto low(PercentageModes) do begin
    CreateImageButtonFrame('Humidity', WriteSafeString(PercentageModes[i]), PercentageModes[i], HumidityBox);
  end;

  // Heat Index
  for i:= high(TemperatureModes) downto low(TemperatureModes) do begin
    CreateImageButtonFrame('HeatIndex', WriteSafeString(TemperatureModes[i]), TemperatureModes[i], HeatIndexBox);
  end;

  // Init event log
  wallLog.LogType := ltFile;
  wallLog.FileName := GetLogFilePath();
  wallLog.Active := True;

  Caption := 'Chameleon ' + VersionSupport.GetProductVersion;
  lblVersion.Caption :=  'Version ' + VersionSupport.GetProductVersion;
end;

procedure TChameleonForm.btnAboutClick(Sender: TObject);
begin
  AboutDialog.ShowModal();
end;

procedure TChameleonForm.btnSettingsClick(Sender: TObject);
begin
  SettingsDialog.State := ProgramSettings.State;
  SettingsDialog.WeatherStationName := ProgramSettings.WeatherStationName;

  if SettingsDialog.ShowModal = mrOK then begin
    ProgramSettings.State := SettingsDialog.cbStates.Text;
    ProgramSettings.WeatherStationName := SettingsDialog.cbStations.Text;
    SaveSettings(ProgramSettings);
  end;
end;

procedure TChameleonForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings(ProgramSettings);
end;

function TChameleonForm.CreateImageButtonFrame(_SettingCategory: string; _SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
var
  ImageButtonFrame : TImageButtonFrame;
  ImagePath: string;
begin
  ImageButtonFrame := TImageButtonFrame.Create(ControlOwner);
  ImagePath := GetImagePath(_SettingKey, _SettingCategory);

  with ImageButtonFrame  do begin
    Name := _SettingCategory + _SettingKey + 'Button';
    Align := alLeft;
    AutoSize := True;
    SettingKey := _SettingKey;   
    SettingCategory := _SettingCategory;
    Title := _Title;
    Parent := ControlOwner;
  end;

  if fileexists(ImagePath) then begin
    ImageButtonFrame.LoadThumb();
  end;

  Result := ImageButtonFrame;
end;

procedure TChameleonForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState <> wsMinimized then
    tmrTheme.Enabled := false;
end;

procedure TChameleonForm.ModeChange(Sender: TObject);
begin
  if rbNone.Checked then begin
    ProgramSettings.Mode := pmNone;
  end
  else if rbBattery.Checked then begin
    ProgramSettings.Mode := pmBattery;
  end
  else if rbTime.Checked then begin
    ProgramSettings.Mode := pmTime;
  end
  else if rbConditions.Checked then begin
    ProgramSettings.Mode := pmWeatherConditions;
  end
  else if rbWindSpeed.Checked then begin
    ProgramSettings.Mode := pmWindSpeed;
  end
  else if rbTemperature.Checked then begin
    ProgramSettings.Mode := pmTemperature;
  end
  else if rbHumidity.Checked then begin
    ProgramSettings.Mode := pmHumidity;
  end
  else if rbHeatIndex.Checked then begin
    ProgramSettings.Mode := pmHeatIndex;
  end
  else begin
    ProgramSettings.Mode := pmNone;
  end;

  SaveSettings(ProgramSettings);
end;

procedure TChameleonForm.OKButtonClick(Sender: TObject);
begin
  Application.Minimize;      
  tmrTheme.Interval := LongWord(spInterval.Value * 1000);
  tmrTheme.Enabled := true;
  tmrThemeTimer(Sender);
  trayIcon.ShowBalloonHint;

  // Hide taskbar icon
  ShowWindow(WidgetSet.AppHandle, SW_Hide);
end;

procedure TChameleonForm.spIntervalChange(Sender: TObject);
begin
  ProgramSettings.Interval := spInterval.Value;
  SaveSettings(ProgramSettings);
end;

procedure TChameleonForm.tmrThemeTimer(Sender: TObject);
var
  CategoryName: string;
  KeyName: string;
  WallpaperPath: string;
begin
  CategoryName := GetCategoryName(ProgramSettings.Mode);

  case ProgramSettings.Mode of
      pmNone:
        exit;
      pmBattery:
        KeyName := WriteSafeString(GetBattery());
      pmTime:
        KeyName := WriteSafeString(GetTime());
      pmWeatherConditions:
        KeyName := WriteSafeString(GetWeatherConditions(ProgramSettings.WeatherStationName));
      pmWindSpeed:
        KeyName := WriteSafeString(GetWindSpeed(ProgramSettings.WeatherStationName));
      pmTemperature:
        KeyName := WriteSafeString(GetTemperature(ProgramSettings.WeatherStationName));
      pmHumidity:
        KeyName := WriteSafeString(GetHumidity(ProgramSettings.WeatherStationName));
      pmHeatIndex:
        KeyName := WriteSafeString(GetHeatIndex(ProgramSettings.WeatherStationName));
      else
        exit;
    end;

   WallpaperPath := GetImagePath(KeyName, CategoryName);      

   wallLog.Info('Changing wallpaper to "' + WallpaperPath + '" for ' + CategoryName + ' = ' + KeyName);
   if not fileexists(WallpaperPath) then wallLog.Error('"' + WallpaperPath + '" does not exist!');

   SetWallpaper(WallpaperPath);
end;

procedure TChameleonForm.trayIconDblClick(Sender: TObject);
begin
  Application.Restore;

  // Show taskbar icon
  ShowWindow(WidgetSet.AppHandle, SW_Show);
end;

end.

