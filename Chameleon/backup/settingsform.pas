unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ButtonPanel, Weather;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    GoButton: TButton;
    ButtonsPanel: TButtonPanel;
    StatesComboBox: TComboBox;
    StationsComboBox: TComboBox;
    StateGroupBox: TGroupBox;
    StationsGroupBox: TGroupBox;
    CurrentWeatherGroupBox: TGroupBox;
    WeatherDataXmlEdit: TSynEdit;
    WeatherDataXmlTab: TTabSheet;
    WeatherMemo: TMemo;
    WeatherStationXMLEdit: TSynEdit;
    XmlSyntaxHighlighter: TSynXMLSyn;
    WeatherStationXmlTab: TTabSheet;
    WeatherTab: TTabSheet;
    MainTabControl: TPageControl;
    procedure GoButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure StatesComboBoxChange(Sender: TObject);
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
  WeatherStationXMLEdit.Caption := StationsXML;

  for StateAbbreviation in StateAbreviations do begin
    StatesComboBox.Items.Add(StateAbbreviation);
  end;

  for Station in WeatherStations do begin
    StationsComboBox.Items.Add(Station.Name);
  end;
end;

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  if State <> '' then begin
    StatesComboBox.Text := State;
  end;

  if WeatherStationName <> ''  then begin
    StationsComboBox.Text := WeatherStationName;
  end;

  StatesComboBoxChange(Sender);
end;

procedure TSettingsDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;


procedure TSettingsDialog.StatesComboBoxChange(Sender: TObject);
const
  StateSep: string = '----';
var             
  Station: TWeatherStation;
  FilteredStations: TWeatherStationArray;
begin
  if StatesComboBox.Text <> StateSep then begin
    FilteredStations := GetStationsForState(WeatherStations, StatesComboBox.Text);
    StationsComboBox.Clear;

    for Station in FilteredStations do begin
      StationsComboBox.Items.Add(Station.Name);
    end;
  end;
end;

procedure TSettingsDialog.GoButtonClick(Sender: TObject);
var                
  SelectedStation: TWeatherStation;
  Weather: TWeatherData;
begin
  if StationsComboBox.Text = '' then exit;

  SelectedStation := GetStationByName(WeatherStations, StationsComboBox.Text);
  WeatherDataXmlEdit.Caption := GetWeatherDataXML(SelectedStation);
  Weather := GetWeatherData(SelectedStation);
  WeatherMemo.Text := PrintWeatherReport(Weather);
end;

procedure TSettingsDialog.CancelButtonClick(Sender: TObject);
begin  
  ModalResult := mrCancel;
end;

end.
