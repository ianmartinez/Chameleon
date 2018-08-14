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
    cbStates: TComboBox;
    cbStations: TComboBox;
    gbState: TGroupBox;
    gbStations: TGroupBox;
    synWeatherStationXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    tsWeatherStationXML: TTabSheet;
    tsWeather: TTabSheet;
    tpMain: TPageControl;
    procedure cbStatesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SettingsDialog: TSettingsDialog;
  StationsXML: string;  
  WeatherStations: WeatherStationArray;

implementation

{$R *.lfm}

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
var
  Station: WeatherStation;
  StateAbbreviation: string;
begin
  StationsXML := GetAllWeatherStationsXML();
  WeatherStations := GetAllWeatherStations(StationsXML);
  synWeatherStationXML.Caption := StationsXML;

  for StateAbbreviation in StateAbreviations do begin
    cbStates.Items.Add(StateAbbreviation);
  end;

  for Station in WeatherStations do begin
    cbStations.Items.Add(Station.Name);
  end;

(*  WeatherStations := GetAllWeatherStations();

  for Station in WeatherStations do
      Memo1.Lines.Add(Station.Name + ' - ' + Station.XmlFile);   *)
end;

procedure TSettingsDialog.cbStatesChange(Sender: TObject);  
const
  StateSep: string = '----';
var             
  Station: WeatherStation;
  FilteredStations: WeatherStationArray;
begin
  if cbStates.Text <> StateSep then begin   
    FilteredStations := GetStationsForState(WeatherStations, cbStates.Text);
    cbStations.Clear;

    for Station in FilteredStations do begin
      cbStations.Items.Add(Station.Name);
    end;
  end;
end;

end.

