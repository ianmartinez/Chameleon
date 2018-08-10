unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, ColorBox, imagebutton;

type

  { TWallShifterForm }

  TWallShifterForm = class(TForm)
    ImageButtonFrame1: TImageButtonFrame;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure ImageButtonFrame2Click(Sender: TObject);
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
begin
  ImageButtonFrame1.SettingName := 'Test';
end;

procedure TWallShifterForm.ScrollBox1Click(Sender: TObject);
begin

end;

end.

