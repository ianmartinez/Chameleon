unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, ColorBox, imagebutton;

type

  { TWallShifterForm }

  TWallShifterForm = class(TForm)
    rbHeatIndex: TRadioButton;
    rbNone: TRadioButton;
    rbBattery: TRadioButton;
    rbTime: TRadioButton;
    rbConditions: TRadioButton;
    rbWindSpeed: TRadioButton;
    rbTemperature: TRadioButton;
    rbHumidity: TRadioButton;
    BatteryBox: TScrollBox;
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
    // Battery Percentage -
      i:= 100;
      while i >= 0 do begin
        CreateImageButtonFrame('Battery' + i.ToString(), i.ToString() + '%', BatteryBox);
        Dec(i, 10);
      end;
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

