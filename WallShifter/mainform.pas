unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, ColorBox, Spin, ImageButton, WeatherReader, Settings,
  AboutForm;

type

  { TWallShifterForm }

  TWallShifterForm = class(TForm)
    btnSettings: TButton;
    btnAbout: TButton;
    Image1: TImage;
    Label1: TLabel;
    TimeBox: TScrollBox;
    ConditionsBox: TScrollBox;
    TemperatureBox: TScrollBox;
    HumidityBox: TScrollBox;
    WindspeedBox: TScrollBox;
    HeatIndexBox: TScrollBox;
    rbHeatIndex: TRadioButton;
    rbNone: TRadioButton;
    rbBattery: TRadioButton;
    rbTime: TRadioButton;
    rbConditions: TRadioButton;
    rbWindSpeed: TRadioButton;
    rbTemperature: TRadioButton;
    rbHumidity: TRadioButton;
    BatteryBox: TScrollBox;
    ScrollBox1: TScrollBox;
    procedure btnAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function CreateImageButtonFrame(_SettingKey: string; _Title: string; ControlOwner: TWinControl) : TImageButtonFrame;
    procedure ScrollBox1Click(Sender: TObject);
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
  // Battery Percentage
  i:= high(BatteryModes);
  while i >= low(BatteryModes) do begin
    CreateImageButtonFrame(WriteSafeString('Battery' + BatteryModes[i]), BatteryModes[i], BatteryBox);
    Dec(i);
  end;
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

procedure TWallShifterForm.ScrollBox1Click(Sender: TObject);
begin

end;

end.

