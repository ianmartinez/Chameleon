unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, Spin, ComCtrls, ButtonPanel, ImageButton,
  Weather, Settings, AboutForm, SettingsForm, VersionSupport;

type

  { TWallShifterForm }

  TWallShifterForm = class(TForm)
    btnSettings: TButton;
    pnlButtons: TButtonPanel;
    HeatIndexBox: TScrollBox;
    HumidityBox: TScrollBox;
    TemperatureBox: TScrollBox;
    WindSpeedBox: TScrollBox;
    ConditionsBox: TScrollBox;
    TimeBox: TScrollBox;
    btnAbout: TButton;
    gbInterval: TGroupBox;
    gbWallpapers: TGroupBox;
    Image1: TImage;
    lblProgramName: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    gbShiftBy: TRadioGroup;
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
    procedure ModeChange(Sender: TObject);
    procedure spIntervalChange(Sender: TObject);
  private

  public

  end;

var
  WallShifterForm: TWallShifterForm;
  ProgramSettings: TProgramSettings;

implementation

{$R *.lfm}

{ TWallShifterForm }

procedure TWallShifterForm.FormCreate(Sender: TObject);
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

  lblProgramName.Caption := 'WallShifter ' + VersionSupport.GetProductVersion;
end;

procedure TWallShifterForm.btnAboutClick(Sender: TObject);
begin
  AboutDialog.ShowModal();
end;

procedure TWallShifterForm.btnSettingsClick(Sender: TObject);
begin
  SettingsDialog.cbStates.SelText := ProgramSettings.State;
  SettingsDialog.cbStations.SelText := ProgramSettings.WeatherStationName;

  if SettingsDialog.ShowModal = mrOK then begin
    beep;
    ProgramSettings.State := SettingsDialog.cbStates.SelText;
    ProgramSettings.WeatherStationName := SettingsDialog.cbStations.SelText;
  end;
end;

procedure TWallShifterForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings(ProgramSettings);
end;

function TWallShifterForm.CreateImageButtonFrame(_SettingCategory: string; _SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
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

procedure TWallShifterForm.ModeChange(Sender: TObject);
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
end;

procedure TWallShifterForm.spIntervalChange(Sender: TObject);
begin
  ProgramSettings.Interval := spInterval.Value;
end;

end.

