unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ButtonPanel, Weather;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    btnGo: TButton;
    ButtonPanel1: TButtonPanel;
    cbStates: TComboBox;
    cbStations: TComboBox;
    gbState: TGroupBox;
    gbStations: TGroupBox;
    gbStations1: TGroupBox;
    synWeatherDataXML: TSynEdit;
    tsWeatherData: TTabSheet;
    txtWeather: TMemo;
    synWeatherStationXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    tsWeatherStationXML: TTabSheet;
    tsWeather: TTabSheet;
    tpMain: TPageControl;
    procedure btnGoClick(Sender: TObject);
    procedure cbStatesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SettingsDialog: TSettingsDialog;
  StationsXML: string;           
  WeatherStations: TWeatherStationArray;
  SelectedStation: TWeatherStation;

implementation

{$R *.lfm}

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
var
  Station: TWeatherStation;
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

end;


procedure TSettingsDialog.cbStatesChange(Sender: TObject);  
const
  StateSep: string = '----';
var             
  Station: TWeatherStation;
  FilteredStations: TWeatherStationArray;
begin
  if cbStates.Text <> StateSep then begin   
    FilteredStations := GetStationsForState(WeatherStations, cbStates.Text);
    cbStations.Clear;

    for Station in FilteredStations do begin
      cbStations.Items.Add(Station.Name);
    end;
  end;
end;

procedure TSettingsDialog.btnGoClick(Sender: TObject);
var
  Weather: TWeatherData;
begin
  SelectedStation := GetStationByName(WeatherStations, cbStations.Text);
  synWeatherDataXML.Caption := GetWeatherDataXML(SelectedStation);
  Weather := GetWeatherData(SelectedStation);
  txtWeather.Text := PrintWeatherReport(Weather);
end;

end.

