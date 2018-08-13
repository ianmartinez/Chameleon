unit Weather;
(*
  Parse weather data from:
  http://w1.weather.gov/xml/current_obs/
*)


{$mode objfpc}{$H+}

interface
  const
    WeatherConditions : array [0..23] of string =
      ('Mostly Cloudy', 'Clear', 'A Few Clouds', 'Partly Cloudy', 'Overcast', 'Fog', 'Smoke', 'Freezing Drizzle', 'Hail', 'Mixed Rain and Snow',
      'Rain and Hail', 'Heavy Mixed Rain and Snow', 'Rain Showers', 'Thunderstorm', 'Snow', 'Windy', 'Scattered Showers', 'Freezing Rain',
      'Scattered Thunderstorms', 'Drizzle', 'Heavy Rain', 'Tornado', 'Dust', 'Haze');

  type WeatherStation = record
	   Name: string;
	   XmlFile: string;
  end;

  type WeatherStationArray = array of WeatherStation;

  function GetStationsForState(StateAbbreviation: string) : WeatherStationArray;
  function NormalizeWeatherCondition(WeatherCondition: string) : string;
  function CalcHeatIndex(Temperature: Integer; Humidity: Integer) : Integer;

implementation
  uses
    Classes, SysUtils;

  procedure SplitString(Delimiter: Char; Str: string; ListOfStrings: TStrings);
  begin
     ListOfStrings.Clear;
     ListOfStrings.Delimiter       := Delimiter;
     ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
     ListOfStrings.DelimitedText   := Str;
  end;

  function GetStationsForState(StateAbbreviation: string) : WeatherStationArray;
  begin
    Result := nil;
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
    i: Integer;
    ConditionNames: TStringList;
    ConditionNamePos: Integer;
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
        ConditionNames.Free;
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
  function CalcHeatIndex(Temperature: Integer; Humidity: Integer) : Integer;
  var
    t: Integer;
    r: Integer;
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

    Result := Round(c1 + c2*t + c3*r + c4*t*r + c5*sqr(t) + c6*sqr(r) +
              c7*sqr(t)*r + c8*t*sqr(r) + c9*sqr(t)*sqr(r));
  end;
end.


