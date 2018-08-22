unit ProgressForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TProgressDialog }

  TProgressDialog = class(TForm)
    lblTask: TLabel;
    pbTask: TProgressBar;
  private

  public

  end;

var
  ProgressDialog: TProgressDialog;

implementation

{$R *.lfm}

end.

