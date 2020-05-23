unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, Spin, ComCtrls, ButtonPanel, Menus, ImageButton,
  Weather, Settings, AboutForm, SettingsForm, VersionSupport, Windows, Win32,
  eventlog, InterfaceBase, lclintf;

type

  { TChameleonForm }

  TChameleonForm = class(TForm)
    AboutButton: TButton;
    SettingsButton: TButton;
    Image2: TImage;
    LogoImage: TImage;
    PatreonImage: TImage;
    PatreonLinkLabel: TLabel;
    ProgramNameLabel: TLabel;
    VersionLabel: TLabel;
    LabelsPanel: TPanel;
    TopPanel: TPanel;
    ChameleonLogger: TEventLog;
    ButtonsPanel: TButtonPanel;
    HeatIndexBox: TScrollBox;
    HumidityBox: TScrollBox;
    TemperatureBox: TScrollBox;
    WallpaperTimer: TTimer;
    ChameleonTrayIcon: TTrayIcon;
    WindSpeedBox: TScrollBox;
    ConditionsBox: TScrollBox;
    TimeBox: TScrollBox;
    IntervalGroupBox: TGroupBox;
    WallpapersGroupBox: TGroupBox;
    RefreshEveryLabel: TLabel;
    RefreshSecondsLabel: TLabel;
    WallpapersTabControl: TPageControl;
    ChangeByGroup: TRadioGroup;
    HeatIndexRadioButton: TRadioButton;
    NoneRadioButton: TRadioButton;
    BatteryRadioButton: TRadioButton;
    TimeRadioButton: TRadioButton;
    ConditionsRadioButton: TRadioButton;
    WindSpeedRadioButton: TRadioButton;
    TemperatureRadioButton: TRadioButton;
    HumidityRadioButton: TRadioButton;
    BatteryBox: TScrollBox;
    InveralSpinEdit: TSpinEdit;
    BatteryTab: TTabSheet;
    TimeTab: TTabSheet;
    ConditionsTab: TTabSheet;
    WindSpeedTab: TTabSheet;
    TemperatureTab: TTabSheet;
    HeatIndexTab: TTabSheet;
    HumidityTab: TTabSheet;
    procedure AboutButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    function CreateImageButtonFrame(_SettingCategory: string; _SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
    procedure FormWindowStateChange(Sender: TObject);
    procedure PatreonLinkLabelClick(Sender: TObject);
    procedure ModeChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure InveralSpinEditChange(Sender: TObject);
    procedure WallpaperTimerTimer(Sender: TObject);
    procedure ChameleonTrayIconDblClick(Sender: TObject);
  private
    FirstShow: boolean;
    AutoStart: boolean;
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
      NoneRadioButton.Checked := true;
    pmBattery:
      BatteryRadioButton.Checked := true;
    pmTime:
      TimeRadioButton.Checked := true;
    pmWeatherConditions:
      ConditionsRadioButton.Checked := true;
    pmWindSpeed:
      WindSpeedRadioButton.Checked := true;
    pmTemperature:
      TemperatureRadioButton.Checked := true;
    pmHumidity:
      HumidityRadioButton.Checked := true;
    pmHeatIndex:
      HeatIndexRadioButton.Checked := true;
    else
      NoneRadioButton.Checked := true;
  end;

  InveralSpinEdit.Value := ProgramSettings.Interval;

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
  ChameleonLogger.LogType := ltFile;
  ChameleonLogger.FileName := GetLogFilePath();
  ChameleonLogger.Active := True;

  Caption := 'Chameleon ' + VersionSupport.GetFileVersion;
  VersionLabel.Caption :=  'Version ' + VersionSupport.GetFileVersion;

  FirstShow := True;
end;

procedure TChameleonForm.AboutButtonClick(Sender: TObject);
begin
  AboutDialog.ShowModal();
end;

procedure TChameleonForm.SettingsButtonClick(Sender: TObject);
begin
  SettingsDialog.State := ProgramSettings.State;
  SettingsDialog.WeatherStationName := ProgramSettings.WeatherStationName;

  if SettingsDialog.ShowModal = mrOK then begin
    ProgramSettings.State := SettingsDialog.StatesComboBox.Text;
    ProgramSettings.WeatherStationName := SettingsDialog.StationsComboBox.Text;
    SaveSettings(ProgramSettings);
  end;
end;

procedure TChameleonForm.FormActivate(Sender: TObject);
begin
   if FirstShow = True then begin
    Refresh();
    if ParamCount > 0 then begin
      if ParamStr(1).Equals('-a') then begin
         OKButtonClick(nil);
         WindowState := wsMinimized;
         AutoStart := True;
      end;
    end;
  end;

  FirstShow := False;
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
    WallpaperTimer.Enabled := false;
end;

procedure TChameleonForm.PatreonLinkLabelClick(Sender: TObject);
begin
  OpenURL('https://www.patreon.com/ianmartinez');
end;

procedure TChameleonForm.ModeChange(Sender: TObject);
begin
  if NoneRadioButton.Checked then begin
    ProgramSettings.Mode := pmNone;
  end
  else if BatteryRadioButton.Checked then begin
    ProgramSettings.Mode := pmBattery;
  end
  else if TimeRadioButton.Checked then begin
    ProgramSettings.Mode := pmTime;
  end
  else if ConditionsRadioButton.Checked then begin
    ProgramSettings.Mode := pmWeatherConditions;
  end
  else if WindSpeedRadioButton.Checked then begin
    ProgramSettings.Mode := pmWindSpeed;
  end
  else if TemperatureRadioButton.Checked then begin
    ProgramSettings.Mode := pmTemperature;
  end
  else if HumidityRadioButton.Checked then begin
    ProgramSettings.Mode := pmHumidity;
  end
  else if HeatIndexRadioButton.Checked then begin
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
  WallpaperTimer.Interval := LongWord(InveralSpinEdit.Value * 1000);
  WallpaperTimer.Enabled := true;
  WallpaperTimerTimer(Sender);
  ChameleonTrayIcon.ShowBalloonHint;

  // Hide taskbar icon
  ShowWindow(WidgetSet.AppHandle, SW_Hide);
end;

procedure TChameleonForm.InveralSpinEditChange(Sender: TObject);
begin
  ProgramSettings.Interval := InveralSpinEdit.Value;
  SaveSettings(ProgramSettings);
end;

procedure TChameleonForm.WallpaperTimerTimer(Sender: TObject);
var
  CategoryName: string;
  KeyName: string;
  WallpaperPath: string;
  Data: string;
  IsWeather: boolean = false;
  Weather: TWeatherData;
begin
  CategoryName := GetCategoryName(ProgramSettings.Mode);

  case ProgramSettings.Mode of
      pmNone:
        exit;
      pmBattery:
        begin
          Data := GetBattery();
        end;
      pmTime:
        begin
          Data := GetTime();
        end;
      pmWeatherConditions:
        begin
          Data := GetWeatherConditions(ProgramSettings.WeatherStationName);
          IsWeather := True;
        end;
      pmWindSpeed:
        begin
          Data := GetWindSpeed(ProgramSettings.WeatherStationName);
          IsWeather := True;
        end;
      pmTemperature:
        begin
          Data := GetTemperature(ProgramSettings.WeatherStationName);
          IsWeather := True;
        end;
      pmHumidity:
        begin
          Data := GetHumidity(ProgramSettings.WeatherStationName);
          IsWeather := True;
        end;
      pmHeatIndex:
        begin
          Data := GetHeatIndex(ProgramSettings.WeatherStationName);
          IsWeather := True;
        end;
      else
        exit;
    end;

    if IsWeather then
    begin
       Weather := GetWeatherByStationName(ProgramSettings.WeatherStationName);
       ChameleonTrayIcon.Hint := PrintWeatherReport(Weather);
    end
    else
       ChameleonTrayIcon.Hint := Data;

   KeyName := WriteSafeString(Data);
   WallpaperPath := GetImagePath(KeyName, CategoryName);
   ChameleonLogger.Info('Changing wallpaper to "' + WallpaperPath + '" for ' + CategoryName + ' = ' + KeyName);
   if not fileexists(WallpaperPath) then ChameleonLogger.Error('"' + WallpaperPath + '" does not exist!');

   SetWallpaper(WallpaperPath);
end;

procedure TChameleonForm.ChameleonTrayIconDblClick(Sender: TObject);
begin
  Application.Restore;

  // If the application is restoring from the
  // first time after auto-starting, the size is
  // all messed up, so this will force windows to
  // resize it to a normal state
  if AutoStart then begin
    WindowState := wsMaximized;
    WindowState := wsNormal;
    AutoStart := False;
  end;

  // Show taskbar icon
  ShowWindow(WidgetSet.AppHandle, SW_Show);        
  ChameleonTrayIcon.Hint := 'Chameleon';
end;

end.
