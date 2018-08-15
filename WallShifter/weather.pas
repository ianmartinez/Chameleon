unit Weather;
(*
  Get weather for the United States and Canada by parsing weather data from:
  http://w1.weather.gov/xml/current_obs/
*)


{$mode objfpc}{$H+}

interface
  const AllStationsXMLFile : string = 'http://w1.weather.gov/xml/current_obs/index.xml';

  const DegF : string = '°F';
  const DegC : string = '°C';

  const WeatherConditions : array [0..23] of string =
      ('Mostly Cloudy', 'Clear', 'A Few Clouds', 'Partly Cloudy', 'Overcast', 'Fog', 'Smoke', 'Freezing Drizzle', 'Hail', 'Mixed Rain and Snow',
      'Rain and Hail', 'Heavy Mixed Rain and Snow', 'Rain Showers', 'Thunderstorm', 'Snow', 'Windy', 'Scattered Showers', 'Freezing Rain',
      'Scattered Thunderstorms', 'Drizzle', 'Heavy Rain', 'Tornado', 'Dust', 'Haze');

  const StateAbreviations : array [0..63] of string =
      ((* United States *) 'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS',  'KY', 'LA',
      'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA','RI', 'SC',
      'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY', '----',
       (* Canada *) 'AB', 'BC', 'MB', 'NB', 'NL', 'NT', 'NS', 'NU', 'ON', 'PE', 'QC', 'SK', 'YT');

  type WeatherStation = record
     Id: string;
	   Name: string;      
	   State: string;
	   XMLUrl: string;
  end;

  (*
    All of the values that can be reliably found in the weather data,
    though Canada does not seem to have condition data for many locations.
  *)
  type WeatherData = record
	   Conditions: string;  
	   Temperature: integer;
	   Humidity: integer;
     HeatIndex: integer;
	   WindSpeed: double;
	   WindDirection: string;
  end;

  type WeatherStationArray = array of WeatherStation;

  function GetAllWeatherStationsXML() : string;
  function GetWeatherDataXML(Station: WeatherStation) : string;
  function GetAllWeatherStations(AllStationsXML: string) : WeatherStationArray;
  function GetStationsForState(AllStations: WeatherStationArray; StateAbbreviation: string) : WeatherStationArray;
  function GetStationByName(AllStations: WeatherStationArray; StationName: string) : WeatherStation;
  function GetWeatherData(Station: WeatherStation) : WeatherData;
  function PrintWeatherReport(Weather: WeatherData) : string;
  function NormalizeWeatherCondition(WeatherCondition: string) : string;
  function CalcHeatIndex(Temperature: integer; Humidity: integer) : integer;

implementation
  uses
    Classes, SysUtils, httpsend, laz2_DOM, laz2_XMLRead, StrUtils;

  procedure SplitString(Delimiter: Char; Str: string; ListOfStrings: TStrings);
  begin
     ListOfStrings.Clear;
     ListOfStrings.Delimiter       := Delimiter;
     ListOfStrings.StrictDelimiter := True;
     ListOfStrings.DelimitedText   := Str;
  end;

  (*
    Some stations like to scream when giving their name:
         FOREMOST AGDM
    This function will calm them down:
         Foremost Agdm
  *)
  function FormatName(sBuffer: string):string;
  var
    iLen, iIndex: integer;
  begin              
    sBuffer := sBuffer.Replace('/', ' / ', [rfReplaceAll]);
    sBuffer := sBuffer.Replace('-', ' - ', [rfReplaceAll]);
    sBuffer := sBuffer.Replace('(', ' ( ', [rfReplaceAll]);
    sBuffer := sBuffer.Replace(')', ' ) ', [rfReplaceAll]);

    iLen := Length(sBuffer);
    sBuffer:= Uppercase(MidStr(sBuffer, 1, 1)) + Lowercase(MidStr(sBuffer,2, iLen));

    for iIndex := 0 to iLen do begin
      if MidStr(sBuffer, iIndex, 1) = ' ' then
          sBuffer := MidStr(sBuffer, 0, iIndex) + Uppercase(MidStr(sBuffer, iIndex + 1, 1)) + Lowercase(MidStr(sBuffer, iIndex + 2, iLen));
    end;
    
    sBuffer := sBuffer.Replace(' / ', '/', [rfReplaceAll]);   
    sBuffer := sBuffer.Replace(' / ', '/', [rfReplaceAll]);
    sBuffer := sBuffer.Replace(' - ', '-', [rfReplaceAll]);
    sBuffer := sBuffer.Replace(' ( ', '(', [rfReplaceAll]);
    sBuffer := sBuffer.Replace(' ) ', ')', [rfReplaceAll]);
    sBuffer := sBuffer.Replace('Exxonmobile','ExxonMobile', [rfReplaceAll]);

    Result := sBuffer;
  end;

  function CreateWeatherStation(Name: string; State: string; XMLURL: string) : WeatherStation;
  var
    Station: WeatherStation;
  begin
    Station.Name := Name;
    Station.State := State;
    Station.XMLUrl := XMLUrl;

    Result := Station;
  end;

  function GetAllWeatherStationsXML() : string;
  var
    Response: TStringList;
  begin
    Response := TStringList.Create;

    Result := '';
    try
      if HTTPSend.HttpGetText(AllStationsXMLFile, Response) then
        Result := Response.Text;
    finally
      Response.Free();
    end;
  end;

  function GetWeatherDataXML(Station: WeatherStation) : string;
  var
    Response: TStringList;
  begin
    Response := TStringList.Create;

    Result := '';
    try
      if HTTPSend.HttpGetText(Station.XMLUrl, Response) then
        Result := Response.Text;
    finally
      Response.Free();
    end;
  end;

  function GetAllWeatherStations(AllStationsXML: string) : WeatherStationArray;
  var
    i: integer;
    StationCount: integer = 0;
    CurrentStation: WeatherStation;
    Stations: WeatherStationArray;
    Doc: TXMLDocument;
    Child: TDOMNode;
    StringStream: TStringStream;
    NodeName: string;
  begin
    Result := nil;

    if AllStationsXML <> '' then begin
      try
        Doc := TXMLDocument.Create;
        StringStream := TStringStream.Create(AllStationsXML);
        ReadXMLFile(Doc, StringStream);
        
        Child := Doc.DocumentElement.FirstChild;
        while Assigned(Child) do
            begin
              NodeName := Child.NodeName;

              if NodeName = 'station' then begin
                // Loop through attributes of weather station
                with Child.ChildNodes do
                try
                  for i := 0 to (Count - 1) do begin
                    if Item[i].NodeName = 'station_name' then begin
                      CurrentStation.Name := FormatName(Item[i].FirstChild.NodeValue);
                    end       
                    else if Item[i].NodeName = 'station_id' then begin
                      CurrentStation.Id := Item[i].FirstChild.NodeValue;
                    end
                    else if Item[i].NodeName = 'state' then begin  
                      CurrentStation.State := Item[i].FirstChild.NodeValue;
                    end
                    else if Item[i].NodeName = 'xml_url' then begin
                      CurrentStation.XMLUrl := Item[i].FirstChild.NodeValue;
                      CurrentStation.XMLUrl := CurrentStation.XMLUrl.Replace('https:', 'http:', [rfReplaceAll]);
                      CurrentStation.XMLUrl := CurrentStation.XMLUrl.Replace('//weather.gov', '//w1.weather.gov', [rfReplaceAll]);
                    end;
                  end;

                  // Station is valid
                  if CurrentStation.Name <> '' then begin
                    // Resize array to accommodate new weather station
                    Inc(StationCount);
                    SetLength(Stations, StationCount);

                    // Add weatherstation
                    Stations[StationCount - 1] :=  CurrentStation;

                    // Reset CurrentStation
                    CurrentStation.Name := '';   
                    CurrentStation.Id := '';
                    CurrentStation.State := '';
                    CurrentStation.XMLUrl := '';
                  end;
                finally
                  Free;
                end;
              end;

              Child := Child.NextSibling;
            end;

        Result := Stations;
      finally
        Doc.Free;
        StringStream.Free;
      end;
    end;
  end;

  function GetStationsForState(AllStations: WeatherStationArray; StateAbbreviation: string) : WeatherStationArray;
  var
    StationCount: integer = 0;
    CurrentStation: WeatherStation; 
    Stations: WeatherStationArray;
  begin
    for CurrentStation in AllStations do begin
      if CurrentStation.State = StateAbbreviation then begin
        Inc(StationCount);
        SetLength(Stations, StationCount);

        Stations[StationCount - 1] := CurrentStation;
      end;
    end;

    Result := Stations;
  end;

  function GetStationByName(AllStations: WeatherStationArray; StationName: string) : WeatherStation;
  var
    Station: WeatherStation;
  begin
    for Station in AllStations do
      if Station.Name = StationName then
        Result := Station;
  end;

  function GetWeatherData(Station: WeatherStation) : WeatherData;
  var
    Weather: WeatherData;
    Doc: TXMLDocument;
    Child: TDOMNode;
    StringStream: TStringStream;
    NodeName: string;
    WeatherDataXML: string;
  begin
    WeatherDataXML := GetWeatherDataXML(Station);

    if WeatherDataXML <> '' then begin
      try
        Doc := TXMLDocument.Create;
        StringStream := TStringStream.Create(WeatherDataXML);
        ReadXMLFile(Doc, StringStream);

        Weather.Conditions := 'NO DATA';
        Weather.WindDirection := 'NO DATA';

        Child := Doc.DocumentElement.FirstChild;
        while Assigned(Child) do
            begin
              NodeName := Child.NodeName;

              if NodeName = 'weather' then begin
                Weather.Conditions := Child.FirstChild.NodeValue;
              end
              else if NodeName = 'temp_f' then begin
                Weather.Temperature := Round(Child.FirstChild.NodeValue.ToDouble());
              end
              else if NodeName = 'relative_humidity' then begin
                Weather.Humidity := Child.FirstChild.NodeValue.ToInteger();
              end
              else if NodeName = 'wind_mph' then begin
                Weather.WindSpeed := Child.FirstChild.NodeValue.ToDouble();
              end
              else if NodeName = 'wind_dir' then begin
                Weather.WindDirection := Child.FirstChild.NodeValue;
              end;

              Child := Child.NextSibling;
            end;

        Weather.HeatIndex := CalcHeatIndex(Weather.Temperature, Weather.Humidity);
        Result := Weather;
      finally
        Doc.Free;
        StringStream.Free;
      end;
    end;
  end;
  
  function PrintWeatherReport(Weather: WeatherData) : string;
  var
    Report: TStringList;
  begin
    Report := TStringList.Create;

    try
      Report.Add('Conditions: ' + Weather.Conditions);
      Report.Add('Temperature: ' + Weather.Temperature.ToString() + DegF);
      Report.Add('Humidity: ' + Weather.Humidity.ToString() + '%');
      Report.Add('Heat Index: ' + Weather.HeatIndex.ToString() + DegF);
      Report.Add('Wind Speed: ' + Weather.WindSpeed.ToString() + ' MPH');
      Report.Add('Wind Direction: ' + Weather.WindDirection);

      Result := Report.Text;
    finally
      Report.Free;
    end;
  end;

  (*
    Many weather conditions returned from weather.gov can be described
    dozens of different ways:
      http://w1.weather.gov/xml/current_obs/weather.php

    This function normalizes them to match those in the array WeatherConditions
    declared above.
  *)
  function NormalizeWeatherCondition(WeatherCondition: string) : string;
  const
    WeatherConditionsIrregular : array [0..23] of string =
      ('Mostly Cloudy|Mostly Cloudy with Haze|Mostly Cloudy and Breezy',
      'Fair|Clear|Fair with Haze|Clear with Haze|Fair and Breezy|Clear and Breezy',
      'A Few Clouds|A Few Clouds with Haze|A Few Clouds and Breezy',
      'Partly Cloudy|Partly Cloudy with Haze|Partly Cloudy and Breezy',
      'Overcast|Overcast with Haze|Overcast and Breezy',
      'Fog/Mist|Fog|Freezing Fog|Shallow Fog|Partial Fog|Patches of Fog|Fog in Vicinity|Freezing Fog in Vicinity|Shallow Fog in Vicinity|Partial Fog in Vicinity|Patches of Fog in Vicinity|Showers in Vicinity Fog|Light Freezing Fog|Heavy Freezing Fog',
      'Smoke',
      'Freezing Rain|Freezing Drizzle|Light Freezing Rain|Light Freezing Drizzle|Heavy Freezing Rain|Heavy Freezing Drizzle|Freezing Rain in Vicinity|Freezing Drizzle in Vicinity',
      'Ice Pellets|Light Ice Pellets|Heavy Ice Pellets|Ice Pellets in Vicinity|Showers Ice Pellets|Thunderstorm Ice Pellets|Ice Crystals|Hail|Small Hail/Snow Pellets|Light Small Hail/Snow Pellets|Heavy small Hail/Snow Pellets|Showers Hail|Hail Showers',
      'Freezing Rain Snow|Light Freezing Rain Snow|Heavy Freezing Rain Snow|Freezing Drizzle Snow|Light Freezing Drizzle Snow|Heavy Freezing Drizzle Snow|Snow Freezing Rain|Light Snow Freezing Rain|Heavy Snow Freezing Rain|Snow Freezing Drizzle|Light Snow Freezing Drizzle|Heavy Snow Freezing Drizzle',
      'Rain Ice Pellets|Light Rain Ice Pellets|Heavy Rain Ice Pellets|Drizzle Ice Pellets|Light Drizzle Ice Pellets|Heavy Drizzle Ice Pellets|Ice Pellets Rain|Light Ice Pellets Rain|Heavy Ice Pellets Rain|Ice Pellets Drizzle|Light Ice Pellets Drizzle|Heavy Ice Pellets Drizzle',
      'Rain Snow|Light Rain Snow|Heavy Rain Snow|Snow Rain|Light Snow Rain|Heavy Snow Rain|Drizzle Snow|Light Drizzle Snow|Heavy Drizzle Snow|Snow Drizzle|Light Snow Drizzle|Heavy Drizzle Snow',
      'Rain Showers|Light Rain Showers|Light Rain and Breezy|Heavy Rain Showers|Rain Showers in Vicinity|Light Showers Rain|Heavy Showers Rain|Showers Rain|Showers Rain in Vicinity|Rain Showers Fog/Mist|Light Rain Showers Fog/Mist|Heavy Rain Showers Fog/Mist|Rain Showers in Vicinity Fog/Mist|Light Showers Rain Fog/Mist|Heavy Showers Rain Fog/Mist|Showers Rain Fog/Mist|Showers Rain in Vicinity Fog/Mist',
      'Thunderstorm|Thunderstorm Rain|Light Thunderstorm Rain|Heavy Thunderstorm Rain|Thunderstorm Rain Fog/Mist|Light Thunderstorm Rain Fog/Mist|Heavy Thunderstorm Rain Fog and Windy|Heavy Thunderstorm Rain Fog/Mist|Thunderstorm Showers in Vicinity|Light Thunderstorm Rain Haze|Heavy Thunderstorm Rain Haze|Thunderstorm Fog|Light Thunderstorm Rain Fog|Heavy Thunderstorm Rain Fog|Thunderstorm Light Rain|Thunderstorm Heavy Rain|Thunderstorm Rain Fog/Mist|Thunderstorm Light Rain Fog/Mist|Thunderstorm Heavy Rain Fog/Mist|Thunderstorm in Vicinity Fog/Mist|Thunderstorm Showers in Vicinity|Thunderstorm in Vicinity Haze|Thunderstorm Haze in Vicinity|Thunderstorm Light Rain Haze|Thunderstorm Heavy Rain Haze|Thunderstorm Fog|Thunderstorm Light Rain Fog|Thunderstorm Heavy Rain Fog|Thunderstorm Hail|Light Thunderstorm Rain Hail|Heavy Thunderstorm Rain Hail|Thunderstorm Rain Hail Fog/Mist|Light Thunderstorm Rain Hail Fog/Mist|Heavy Thunderstorm Rain Hail Fog/Hail|Thunderstorm Showers in Vicinity Hail|Light Thunderstorm Rain Hail Haze|Heavy Thunderstorm Rain Hail Haze|Thunderstorm Hail Fog|Light Thunderstorm Rain Hail Fog|Heavy Thunderstorm Rain Hail Fog|Thunderstorm Light Rain Hail|Thunderstorm Heavy Rain Hail|Thunderstorm Rain Hail Fog/Mist|Thunderstorm Light Rain Hail Fog/Mist|Thunderstorm Heavy Rain Hail Fog/Mist|Thunderstorm in Vicinity Hail|Thunderstorm in Vicinity Hail Haze|Thunderstorm Haze in Vicinity Hail|Thunderstorm Light Rain Hail Haze|Thunderstorm Heavy Rain Hail Haze|Thunderstorm Hail Fog|Thunderstorm Light Rain Hail Fog|Thunderstorm Heavy Rain Hail Fog|Thunderstorm Small Hail/Snow Pellets|Thunderstorm Rain Small Hail/Snow Pellets|Light Thunderstorm Rain Small Hail/Snow Pellets|Heavy Thunderstorm Rain Small Hail/Snow Pellets',
      'Snow|Light Snow|Heavy Snow|Snow Showers|Light Snow Showers|Heavy Snow Showers|Showers Snow|Light Showers Snow|Heavy Showers Snow|Snow Fog/Mist|Light Snow Fog/Mist|Heavy Snow Fog/Mist|Snow Showers Fog/Mist|Light Snow Showers Fog/Mist|Heavy Snow Showers Fog/Mist|Showers Snow Fog/Mist|Light Showers Snow Fog/Mist|Heavy Showers Snow Fog/Mist|Snow Fog|Light Snow Fog|Heavy Snow Fog|Snow Showers Fog|Light Snow Showers Fog|Heavy Snow Showers Fog|Showers Snow Fog|Light Showers Snow Fog|Heavy Showers Snow Fog|Showers in Vicinity Snow|Snow Showers in Vicinity|Snow Showers in Vicinity Fog/Mist|Snow Showers in Vicinity Fog|Low Drifting Snow|Blowing Snow|Snow Low Drifting Snow|Snow Blowing Snow|Light Snow Low Drifting Snow|Light Snow Blowing Snow|Light Snow Blowing Snow Fog/Mist|Heavy Snow Low Drifting Snow|Heavy Snow Blowing Snow|Thunderstorm Snow|Light Thunderstorm Snow|Heavy Thunderstorm Snow|Snow Grains|Light Snow Grains|Heavy Snow Grains|Heavy Blowing Snow|Blowing Snow in Vicinity',
      'Windy|Breezy|Fair and Windy|A Few Clouds and Windy|Partly Cloudy and Windy|Mostly Cloudy and Windy|Overcast and Windy',
      'Showers in Vicinity|Showers in Vicinity Fog/Mist|Showers in Vicinity Fog|Showers in Vicinity Haze',
      'Freezing Rain Rain|Light Freezing Rain Rain|Heavy Freezing Rain Rain|Rain Freezing Rain|Light Rain Freezing Rain|Heavy Rain Freezing Rain|Freezing Drizzle Rain|Light Freezing Drizzle Rain|Heavy Freezing Drizzle Rain|Rain Freezing Drizzle|Light Rain Freezing Drizzle|Heavy Rain Freezing Drizzle',
      'Thunderstorm in Vicinity|Thunderstorm in Vicinity Fog|Thunderstorm in Vicinity Haze',
      'Light Rain|Drizzle|Light Drizzle|Heavy Drizzle|Light Rain Fog/Mist|Drizzle Fog/Mist|Light Drizzle Fog/Mist|Heavy Drizzle Fog/Mist|Light Rain Fog|Drizzle Fog|Light Drizzle Fog|Heavy Drizzle Fog',
      'Rain|Heavy Rain|Rain Fog/Mist|Heavy Rain Fog/Mist|Rain Fog|Heavy Rain Fog',
      'Funnel Cloud|Funnel Cloud in Vicinity|Tornado/Water Spout',
      'Dust|Low Drifting Dust|Blowing Dust|Sand|Blowing Sand|Low Drifting Sand|Dust/Sand Whirls|Dust/Sand Whirls in Vicinity|Dust Storm|Heavy Dust Storm|Dust Storm in Vicinity|Sand Storm|Heavy Sand Storm|Sand Storm in Vicinity',
      'Haze');
  var
    i: integer;
    ConditionNames: TStringList;
    ConditionNamePos: integer;
    NormalizedWeatherCondition: string;
  begin
    NormalizedWeatherCondition := 'Invalid';

    for i := low(WeatherConditions) to high(WeatherConditions) do begin
      ConditionNames := TStringList.Create;
      try
        SplitString('|', WeatherConditionsIrregular[i], ConditionNames);
        for ConditionNamePos := 0 to ConditionNames.Count - 1 do
          if(LowerCase(WeatherCondition) = LowerCase(ConditionNames[ConditionNamePos])) then
            NormalizedWeatherCondition := WeatherConditions[i];
      finally
        ConditionNames.Free();
      end;
    end;

    Result := NormalizedWeatherCondition;
  end;

  (*
      Calculate the heat index (how the temperature actually feels)
      for a specified temperature and humidity.

      Equation from: https://en.wikipedia.org/wiki/Heat_index#Formula
      Table: https://en.wikipedia.org/wiki/Heat_index#Table_of_values
  *)
  function CalcHeatIndex(Temperature: integer; Humidity: integer) : integer;
  var
    t: integer;
    r: integer;
  const
    c1: double = -42.379;
    c2: double = 2.04901523;
    c3: double = 10.14333127;
    c4: double = -0.22475541;
    c5: double = -6.83783e-3;
    c6: double = -5.481717e-2;
    c7: double = 1.22874e-3;
    c8: double = 8.5282e-4;
    c9: double = -1.99e-6;
  begin
    t := Temperature;
    r := Humidity;

    if (t >= 80) and (t <= 112) then begin
      Result := Round(c1 + c2*t + c3*r + c4*t*r + c5*sqr(t) + c6*sqr(r) +
                c7*sqr(t)*r + c8*t*sqr(r) + c9*sqr(t)*sqr(r));
    end
    else begin
      Result := Round(0.5 * (t + 61.0 + ((t-68.0)*1.2) + (r*0.094)));
    end;
  end;

end.


