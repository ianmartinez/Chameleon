unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    txtAbout: TMemo;
  private

  public

  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.lfm}

end.

