unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ButtonPanel, Weather;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    synWeatherStationXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    tsWeatherStationXML: TTabSheet;
    tsWeather: TTabSheet;
    tpMain: TPageControl;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SettingsDialog: TSettingsDialog;

implementation

{$R *.lfm}

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
var
  Station: WeatherStation;
  WeatherStations: WeatherStationArray;
begin
  synWeatherStationXML.Caption := GetWeatherStationXML();

(*  WeatherStations := GetAllWeatherStations();

  for Station in WeatherStations do
      Memo1.Lines.Add(Station.Name + ' - ' + Station.XmlFile);   *)
end;
end.

