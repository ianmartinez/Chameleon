unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ButtonPanel, Weather, Settings;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    WeatherGroupBox: TGroupBox;
    RunGroupBox: TGroupBox;
    RunAtStartupCheckbox: TCheckBox;
    GoButton: TButton;
    ButtonsPanel: TButtonPanel;
    ShowChameleonRunningCheckbox: TCheckBox;
    AlwaysShowWeatherCheckbox: TCheckBox;
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
    LocationTab: TTabSheet;
    MainTabControl: TPageControl;
    procedure GoButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure StatesComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    Settings: TProgramSettings;
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

  RunAtStartupCheckbox.Checked := Settings.RunAtStartup; 
  ShowChameleonRunningCheckbox.Checked := Settings.ShowChameleonIsRunning;
  AlwaysShowWeatherCheckbox.Checked := Settings.AlwaysShowWeather;

  for StateAbbreviation in StateAbreviations do begin
    StatesComboBox.Items.Add(StateAbbreviation);
  end;

  for Station in WeatherStations do begin
    StationsComboBox.Items.Add(Station.Name);
  end;
end;

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  if Settings.State <> '' then begin
    StatesComboBox.Text := Settings.State;
  end;

  StatesComboBoxChange(Sender);

  if Settings.WeatherStationName <> ''  then begin
    StationsComboBox.Text := Settings.WeatherStationName;
  end;
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
