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
    RefreshEveryLabel: TLabel;
    SettingsButton: TButton;
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
    function CreateImageButtonFrame(ImageCategory: string; ImageKey: string; ImageTitle: string; ControlOwner: TWinControl) : TImageButtonFrame;
    procedure FormWindowStateChange(Sender: TObject);
    procedure PatreonLinkLabelClick(Sender: TObject);
    procedure ModeChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure InveralSpinEditChange(Sender: TObject);
    procedure WallpaperTimerTimer(Sender: TObject);
    procedure ChameleonTrayIconDblClick(Sender: TObject);
    procedure RunAutomatically();
  private
    FirstShow: boolean;
  public

  end;

var
  ChameleonForm: TChameleonForm;

implementation

{$R *.lfm}

{ TChameleonForm }

procedure TChameleonForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  (* Load the UI from the settings *)
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
    CreateImageButtonFrame('Battery', PercentageModes[i], PercentageModes[i], BatteryBox);
  end;

  // Time of day
  for i:= high(TimeModes) downto low(TimeModes) do begin
    CreateImageButtonFrame('Time', TimeModes[i], TimeModes[i], TimeBox);
  end;

  // Weather Conditions
  for i:= high(WeatherConditions) downto low(WeatherConditions) do begin
    CreateImageButtonFrame('Weather Conditions', WeatherConditions[i], WeatherConditions[i], ConditionsBox);
  end;

  // Wind speed
  for i:= high(WindSpeedModes) downto low(WindSpeedModes) do begin
    CreateImageButtonFrame('WindSpeed', WindSpeedModes[i], WindSpeedModes[i], WindSpeedBox);
  end;

  // Temperature
  for i:= high(TemperatureModes) downto low(TemperatureModes) do begin
    CreateImageButtonFrame('Temperature', TemperatureModes[i], TemperatureModes[i], TemperatureBox);
  end;

  // Humidity
  for i:= high(PercentageModes) downto low(PercentageModes) do begin
    CreateImageButtonFrame('Humidity', PercentageModes[i], PercentageModes[i], HumidityBox);
  end;

  // Heat Index
  for i:= high(TemperatureModes) downto low(TemperatureModes) do begin
    CreateImageButtonFrame('Heat Index', TemperatureModes[i], TemperatureModes[i], HeatIndexBox);
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
  if SettingsDialog.ShowModal = mrOK then begin
    ProgramSettings.State := SettingsDialog.StatesComboBox.Text;
    ProgramSettings.WeatherStationName := SettingsDialog.StationsComboBox.Text;
    ProgramSettings.RunAtStartup := SettingsDialog.RunAtStartupCheckbox.Checked;  
    ProgramSettings.ShowChameleonIsRunning := SettingsDialog.ShowChameleonRunningCheckbox.Checked;

    SaveSettings(ProgramSettings);
  end;
end;

procedure TChameleonForm.FormActivate(Sender: TObject);
begin
  (* If this is the first time showing the form *)
   if FirstShow = True then begin
     Refresh();

     (* Auto start if enabled *)
     if ProgramSettings.AutoStartNeeded then begin
       RunAutomatically();
     end;

     WindowState := wsNormal;
     BringToFront();
   end;

   FirstShow := False;
end;

procedure TChameleonForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings(ProgramSettings);
end;

function TChameleonForm.CreateImageButtonFrame(ImageCategory: string;
  ImageKey: string; ImageTitle: string; ControlOwner: TWinControl) : TImageButtonFrame;
var
  ImageButtonFrame : TImageButtonFrame;
  ImagePath: string;
  SafeCategory: string;
  SafeKey: string;
begin
  ImageButtonFrame := TImageButtonFrame.Create(ControlOwner);
  SafeCategory := WriteSafeString(ImageCategory);
  SafeKey := WriteSafeString(ImageKey);
  ImagePath := GetImagePath(SafeKey, SafeCategory);

  with ImageButtonFrame do begin
    Name := SafeCategory + SafeKey + 'Button';
    Align := alLeft;
    AutoSize := True;
    FullName := ImageKey + ' (' +  ImageCategory + ')';
    SettingKey := SafeKey;
    SettingCategory := SafeCategory;
    Title := ImageTitle;
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
  Application.Minimize();      
  WallpaperTimer.Interval := LongWord(InveralSpinEdit.Value * 1000);
  WallpaperTimer.Enabled := true;
  WallpaperTimerTimer(Sender);

  if ProgramSettings.ShowChameleonIsRunning then begin
    ChameleonTrayIcon.ShowBalloonHint;
  end;

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
  Weather: TWeatherData;
  TrayReport: TStringList;
  InvalidStation: Boolean;
begin
  CategoryName := GetCategoryName(ProgramSettings.Mode);
  (* Get weather *)
  InvalidStation := ProgramSettings.WeatherStationName.Equals('');
  if not InvalidStation then begin
    Weather := GetWeatherByStationName(ProgramSettings.WeatherStationName);
  end;

  case ProgramSettings.Mode of
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
          Data := Weather.Conditions;
        end;
      pmWindSpeed:
        begin
          Data := ConvertSpeed(Weather.WindSpeed);
        end;
      pmTemperature:
        begin
          Data := ConvertTemperature(Weather.Temperature);
        end;
      pmHumidity:
        begin
          Data := ConvertPercentage(Weather.Humidity);
        end;
      pmHeatIndex:
        begin
          Data := ConvertTemperature(Weather.HeatIndex);
        end;
      else (* No or invalid mode *)
        Data := '';
    end;

    (* Set the tray icon hint to the report *)
    TrayReport := TStringList.Create;
    try
      TrayReport.Add('Mode: ' + GetCategoryTitle(ProgramSettings.Mode));

      if not InvalidStation then begin
        TrayReport.Add('');
        TrayReport.Add(PrintMiniWeatherReport(Weather));
      end;

      ChameleonTrayIcon.Hint := TrayReport.Text;
    finally
      TrayReport.Free;
    end;

    (* Set the wallpaper if there is any data (i.e. not pmNone) *)
    if not Data.Equals('') then begin
      KeyName := WriteSafeString(Data);
      WallpaperPath := GetImagePath(KeyName, CategoryName);
      ChameleonLogger.Info('Changing wallpaper to "' + WallpaperPath + '" for ' + CategoryName + ' = ' + KeyName);

      if not fileexists(WallpaperPath) then begin
        ChameleonLogger.Error('"' + WallpaperPath + '" does not exist!');
      end;

      SetWallpaper(WallpaperPath);
    end;
end;

procedure TChameleonForm.ChameleonTrayIconDblClick(Sender: TObject);
begin
  Application.Restore;

  // If the application is restoring from the
  // first time after auto-starting, the size is
  // all messed up, so this will force windows to
  // resize it to a normal state
  if ProgramSettings.AutoStartNeeded then begin
    WindowState := wsMaximized;
    WindowState := wsNormal;
    ProgramSettings.AutoStartNeeded := False;
  end;

  // Show taskbar icon
  ShowWindow(WidgetSet.AppHandle, SW_Show);
  Visible := True;
  WindowState := wsNormal;
  BringToFront();
  ChameleonTrayIcon.Hint := 'Chameleon';
end;

procedure TChameleonForm.RunAutomatically();
begin          
  WindowState := wsMinimized;
  OKButtonClick(nil);
  ProgramSettings.AutoStartNeeded := True;
end;

end.
