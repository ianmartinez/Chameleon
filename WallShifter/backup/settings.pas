unit Settings;

{$mode objfpc}{$H+}

interface
  type TProgramMode =
    (pmNone, pmBattery, pmTime, pmWeatherConditions, pmWindSpeed, pmTemperature, pmHumidity, pmHeatIndex);

  type TProgramSettings = record
    Mode: TProgramMode;
    Interval: integer;
    State: string;
    WeatherStationName: string;
  end;

  const Folders : array [0..6] of string =
      ('Battery', 'Time', 'WeatherConditions', 'WindSpeed', 'Temperature', 'Humidity', 'HeatIndex');

  const PercentageModes : array [0..11] of string =
      ('0%','1 to 9%','10 to 19%','20 to 29%','30 to 39%', '40 to 49%', '50 to 59%', '60 to 69%', '70 to 79%', '80 to 89%', '90 to 99%', '100%');

  const TimeModes : array [0..23] of string =
      ('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM',
      '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM');

  const WindSpeedModes : array [0..11] of string =
      ('0 MPH','1 to 9 MPH','10 to 19 MPH','20 to 29 MPH','30 to 39 MPH', '40 to 49 MPH', '50 to 59 MPH', '60 to 69 MPH',
      '70 to 79 MPH', '80 to 89 MPH', '90 to 99 MPH', '100+ MPH');

  const TemperatureModes : array [0..20] of string =
      ('<= -50°F', '-49 to -40°F', '-39 to -30°F', '-29 to -20°F', '-19 to -10°F', '-9 to -1°F', '0°F','1 to 9°F','10 to 19°F','20 to 29°F','30 to 39°F',
      '40 to 49°F', '50 to 59°F', '60 to 69°F', '70 to 79°F', '80 to 89°F', '90 to 99°F', '100 to 109°F', '110 to 119°F', '120 to 129°F', '>= 130°F');

  function WriteSafeString(UnsafeString: string) : string;
  function GetLocalFolder() : string;
  function GetImagePath(Key: string; Category: string) : string;  
  function GetThumbPath(Key: string; Category: string) : string; 
  function LoadSettings() : TProgramSettings;
  procedure SaveSettings(ProgramSettings: TProgramSettings);

  function ConvertPercentage(n: integer) : string;  
  function ConvertSpeed(n: integer) : string;
  function ConvertTemperature(n: integer) : string;

  function GetBattery(): string;
  function GetTime(): string;
  function GetWeatherConditions(WeatherStationName: string): string;  
  function GetWindSpeed(WeatherStationName: string): string;       
  function GetTemperature(WeatherStationName: string): string;
  function GetHumidity(WeatherStationName: string): string;
  function GetHeatIndex(WeatherStationName: string): string;
implementation  
  uses
    Classes, SysUtils, Forms, IniFiles, typinfo, windows, weather;

  function WriteSafeString(UnsafeString: string) : string;
  var
    SafeString: string;
  begin
    SafeString := UnsafeString;
    SafeString := SafeString.Replace(' to ','To', [rfReplaceAll]);
    SafeString := SafeString.Replace('=','EqualTo', [rfReplaceAll]);
    SafeString := SafeString.Replace('/','And', [rfReplaceAll]);
    SafeString := SafeString.Replace('&','And', [rfReplaceAll]);
    SafeString := SafeString.Replace('<','LessThan', [rfReplaceAll]);
    SafeString := SafeString.Replace('>','MoreThan', [rfReplaceAll]);
    SafeString := SafeString.Replace('%','Percent', [rfReplaceAll]);
    SafeString := SafeString.Replace('-','Negative', [rfReplaceAll]);
    SafeString := SafeString.Replace('+','Plus', [rfReplaceAll]);
    SafeString := SafeString.Replace('°','Deg', [rfReplaceAll]);
    SafeString := SafeString.Replace(' ','', [rfReplaceAll]);

    result := SafeString;
  end;

  function GetLocalFolder() : string;
  begin
    Result := ExtractFileDir(Application.ExeName);
  end;

  function GetImagePath(Key: string; Category: string) : string;
  begin
    Result := GetLocalFolder() + '\Wallpapers\' + Category + '\' + Key + '.jpg';
  end;

  function GetThumbPath(Key: string; Category: string) : string;
  begin
    Result := GetLocalFolder() + '\Thumb\' + Category + '\' + Key + '.jpg';
  end;

  
  function GetSettingsFilePath() : string;
  begin
    Result := GetLocalFolder() + '\Settings.ini';
  end;

  function LoadSettings() : TProgramSettings;
  var                
    INI: TINIFile;
    ProgramSettings: TProgramSettings;
    SettingsPath: string;
  begin
    SettingsPath := GetSettingsFilePath;

    if fileexists(SettingsPath) then begin
      INI := TINIFile.Create(SettingsPath);

      try
        ProgramSettings.Mode := TProgramMode(INI.ReadInteger('Program', 'ProgramMode', 0));
        ProgramSettings.Interval := INI.ReadInteger('Program','Interval', 60);
        ProgramSettings.State := INI.ReadString('Weather', 'State', '');
        ProgramSettings.WeatherStationName := INI.ReadString('Weather', 'WeatherStationName', '');
      finally
        INI.Free;
      end;
    end
    else begin
      ProgramSettings.Mode := pmNone;
      ProgramSettings.Interval := 60;
      ProgramSettings.State := '';
      ProgramSettings.WeatherStationName := '';
    end;

    Result := ProgramSettings;
  end;

  procedure SaveSettings(ProgramSettings: TProgramSettings);
  var
    INI: TINIFile;
    SettingsPath: string;
  begin
    SettingsPath := GetSettingsFilePath;
    INI := TINIFile.Create(SettingsPath);

    try
      INI.WriteInteger('Program', 'ProgramMode', integer(ProgramSettings.Mode));     
      INI.WriteInteger('Program', 'Interval', ProgramSettings.Interval);
      INI.WriteString('Weather', 'State', ProgramSettings.State);          
      INI.WriteString('Weather', 'WeatherStationName', ProgramSettings.WeatherStationName);

      INI.UpdateFile;
    finally
      INI.Free;
    end;
  end;

  function ConvertPercentage(n: integer) : string;
  begin
    Result := '0%';

    if n >= 100 then begin
      Result := '100%';
    end
    else if (n >= 90) and (n <= 99) then begin
      Result := '90 to 99%';
    end
    else if (n >= 80) and (n <= 89) then begin
      Result := '80 to 89%';
    end
    else if (n >= 70) and (n <= 79) then begin
      Result := '70 to 79%';
    end
    else if (n >= 60) and (n <= 69) then begin
      Result := '60 to 69%';
    end
    else if (n >= 50) and (n <= 59) then begin
      Result := '50 to 59%';
    end
    else if (n >= 40) and (n <= 49) then begin
      Result := '40 to 49%';
    end
    else if (n >= 30) and (n <= 39) then begin
      Result := '30 to 39%';
    end
    else if (n >= 20) and (n <= 29) then begin
      Result := '20 to 29%';
    end
    else if (n >= 10) and (n <= 19) then begin
      Result := '10 to 19%';
    end
    else if (n >= 1) and (n <= 9) then begin
      Result := '1 to 9%';
    end
    else if (n <= 0) then begin
      Result := '0%';
    end;
  end;

  function ConvertSpeed(n: integer) : string;
  begin
    Result := '0 MPH';

    if n >= 100 then begin
      Result := '100+ MPH';
    end
    else if (n >= 90) and (n <= 99) then begin
      Result := '90 to 99 MPH';
    end
    else if (n >= 80) and (n <= 89) then begin
      Result := '80 to 89 MPH';
    end
    else if (n >= 70) and (n <= 79) then begin
      Result := '70 to 79 MPH';
    end
    else if (n >= 60) and (n <= 69) then begin
      Result := '60 to 69 MPH';
    end
    else if (n >= 50) and (n <= 59) then begin
      Result := '50 to 59 MPH';
    end
    else if (n >= 40) and (n <= 49) then begin
      Result := '40 to 49 MPH';
    end
    else if (n >= 30) and (n <= 39) then begin
      Result := '30 to 39 MPH';
    end
    else if (n >= 20) and (n <= 29) then begin
      Result := '20 to 29 MPH';
    end
    else if (n >= 10) and (n <= 19) then begin
      Result := '10 to 19 MPH';
    end
    else if (n >= 1) and (n <= 9) then begin
      Result := '1 to 9 MPH';
    end
    else if (n <= 0) then begin
      Result := '0 MPH';
    end;
  end;

  function ConvertTemperature(n: integer) : string;
  begin
    Result := '0°F';

    if n >= 130 then begin
      Result := '>= 130°F';
    end                
    else if (n >= 120) and (n <= 129) then begin
      Result := '120 to 129°F';
    end
    else if (n >= 110) and (n <= 119) then begin
      Result := '110 to 119°F';
    end
    else if (n >= 100) and (n <= 109) then begin
      Result := '100 to 109°F';
    end
    else if (n >= 90) and (n <= 99) then begin
      Result := '90 to 99°F';
    end
    else if (n >= 80) and (n <= 89) then begin
      Result := '80 to 89°F';
    end
    else if (n >= 70) and (n <= 79) then begin
      Result := '70 to 79°F';
    end
    else if (n >= 60) and (n <= 69) then begin
      Result := '60 to 69°F';
    end
    else if (n >= 50) and (n <= 59) then begin
      Result := '50 to 59°F';
    end
    else if (n >= 40) and (n <= 49) then begin
      Result := '40 to 49°F';
    end
    else if (n >= 30) and (n <= 39) then begin
      Result := '30 to 39°F';
    end
    else if (n >= 20) and (n <= 29) then begin
      Result := '20 to 29°F';
    end
    else if (n >= 10) and (n <= 19) then begin
      Result := '10 to 19°F';
    end
    else if (n >= 1) and (n <= 9) then begin
      Result := '1 to 9°F';
    end
    else if (n = 0) then begin
      Result := '0°F';
    end
    else if (n >= -9) and (n <= -1) then begin
      Result := '-9 to -1°F';
    end
    else if (n >= -19) and (n <= -10) then begin
      Result := '-19 to -10°F';
    end
    else if (n >= -29) and (n <= -20) then begin
      Result := '-29 to -20°F';
    end
    else if (n >= -39) and (n <= -30) then begin
      Result := '-39 to -30°F';
    end
    else if (n >= -49) and (n <= -40) then begin
      Result := '-49 to -40°F';
    end 
    else if (n <+ -50) then begin
      Result := '<= -50°F';
    end;
  end;

  function GetBattery() : string;
  var
    PowerStatus: TSystemPowerStatus;
    Percentage: integer;
  begin
    Percentage := 0;

    if GetSystemPowerStatus(PowerStatus) then begin
      Percentage := integer(PowerStatus.BatteryLifePercent);
    end;

    Result := ConvertPercentage(Percentage);
  end;

  function GetTime() : string;
  var
    current_time : TDateTime;
  begin
    current_time := Now;

    Result := formatdatetime('h ampm', current_time);
  end;

  function GetWeatherByStationName(WeatherStationName: string) : TWeatherData;
  var
    SelectedStation: TWeatherStation;
    WeatherStations: TWeatherStationArray;
  begin
    if (WeatherStationName <> '') then begin
      WeatherStations := GetAllWeatherStations(GetAllWeatherStationsXML());
      SelectedStation := GetStationByName(WeatherStations, WeatherStationName);

      Result := GetWeatherData(SelectedStation);
    end;
  end;

  function GetWeatherConditions(WeatherStationName: string): string;
  begin
    Result := GetWeatherByStationName(WeatherStationName).Conditions;
  end;

  function GetWindSpeed(WeatherStationName: string): string;
  begin
    Result := ConvertSpeed(Round(GetWeatherByStationName(WeatherStationName).WindSpeed));
  end;

  function GetTemperature(WeatherStationName: string): string;
  begin

  end;

  function GetHumidity(WeatherStationName: string): string;
  begin
    Result := ConvertPercentage(GetWeatherByStationName(WeatherStationName).Humidity);
  end;

  function GetHeatIndex(WeatherStationName: string): string;
  begin

  end;

end.

