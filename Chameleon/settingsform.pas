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
    pnlButtons: TButtonPanel;
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
    procedure CancelButtonClick(Sender: TObject);
    procedure cbStatesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    State: string;
    WeatherStationName: string;
  end;

var
  SettingsDialog: TSettingsDialog;
  StationsXML: string;           
  WeatherStations: TWeatherStationArray;

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

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  if State <> '' then begin
    cbStates.Text := State;
  end;
  if WeatherStationName <> ''  then begin
    cbStations.Text := WeatherStationName;
  end;

  cbStatesChange(Sender);
end;

procedure TSettingsDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
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
  SelectedStation: TWeatherStation;
  Weather: TWeatherData;
begin
  if cbStations.Text = '' then exit;

  SelectedStation := GetStationByName(WeatherStations, cbStations.Text);
  synWeatherDataXML.Caption := GetWeatherDataXML(SelectedStation);
  Weather := GetWeatherData(SelectedStation);
  txtWeather.Text := PrintWeatherReport(Weather);
end;

procedure TSettingsDialog.CancelButtonClick(Sender: TObject);
begin  
  ModalResult := mrCancel;
end;

end.

