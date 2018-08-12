unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, ColorBox, Spin, ComCtrls, ImageButton,
  WeatherReader, Settings, AboutForm, VersionSupport;

type

  { TWallShifterForm }

  TWallShifterForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    function CreateImageButtonFrame(_SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
  private

  public

  end;

var
  WallShifterForm: TWallShifterForm;

implementation

{$R *.lfm}

{ TWallShifterForm }

procedure TWallShifterForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Battery percentage
  i:= high(BatteryModes);
  while i >= low(BatteryModes) do begin
    CreateImageButtonFrame(WriteSafeString('Battery' + BatteryModes[i]), BatteryModes[i], BatteryBox);
    Dec(i);     
  end;

  // Time of day
  i:= high(TimeModes);
  while i >= low(TimeModes) do begin
    CreateImageButtonFrame(WriteSafeString('Time' + TimeModes[i]), TimeModes[i], TimeBox);
    Dec(i);
  end;

  // Wind speed
  i:= high(WindSpeedModes);
  while i >= low(WindSpeedModes) do begin
    CreateImageButtonFrame(WriteSafeString('WindSpeed' + WindSpeedModes[i]), WindSpeedModes[i], WindSpeedBox);
    Dec(i);
  end;
                 
  // Temperature
  i:= high(TemperatureModes);
  while i >= low(TemperatureModes) do begin
    CreateImageButtonFrame(WriteSafeString('Temperature' + TemperatureModes[i]), TemperatureModes[i], TemperatureBox);
    Dec(i);
  end;

  lblProgramName.Caption := 'WallShifter ' + VersionSupport.GetProductVersion;
end;

procedure TWallShifterForm.btnAboutClick(Sender: TObject);
begin
  AboutDialog.ShowModal();
end;

function TWallShifterForm.CreateImageButtonFrame(_SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
var
  ImageButtonFrame : TImageButtonFrame;
begin
  ImageButtonFrame := TImageButtonFrame.Create(ControlOwner);

  with ImageButtonFrame  do begin
    Name := _SettingKey + 'Button';
    Align := alLeft;
    AutoSize := True;
    SettingKey := _SettingKey;
    Title := _Title;

    Parent := ControlOwner;
  end;

  Result := ImageButtonFrame;
end;

end.

