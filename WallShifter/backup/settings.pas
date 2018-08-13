unit Settings;

{$mode objfpc}{$H+}

interface  
  const
    PercentageModes : array [0..11] of string =
      ('0%','1 to 9%','10 to 19%','20 to 29%','30 to 39%', '40 to 49%', '50 to 59%', '60 to 69%', '70 to 79%', '80 to 89%', '90 to 99%', '100%');

    TimeModes : array [0..23] of string =
      ('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM',
      '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM');

    WeatherConditionsModes : array [0..23] of string =
      ('Mostly Cloudy', 'Clear', 'A Few Clouds', 'Partly Cloudy', 'Overcast', 'Fog', 'Smoke', 'Freezing Drizzle', 'Hail', 'Mixed Rain and Snow',
      'Rain and Hail', 'Heavy Mixed Rain and Snow', 'Rain Showers', 'Thunderstorm', 'Snow', 'Windy', 'Scattered Showers', 'Freezing Rain',
      'Scattered Thunderstorms', 'Drizzle', 'Heavy Rain', 'Tornado', 'Dust', 'Haze');

    WindSpeedModes : array [0..11] of string =
      ('0 MPH','1 to 9 MPH','10 to 19 MPH','20 to 29 MPH','30 to 39 MPH', '40 to 49 MPH', '50 to 59 MPH', '60 to 69 MPH', '70 to 79 MPH', '80 to 89 MPH', '90 to 99 MPH', '100+ MPH');

    TemperatureModes : array [0..22] of string =
      ('<-70°F', '-69 to -60°F', '-59 to -50°F', '-49 to -40°F', '-39 to -30°F', '-29 to -19°F', '-19 to -10°F', '-9 to -1°F', '0°F','1 to 9°F','10 to 19°F','20 to 29°F','30 to 39°F',
      '40 to 49°F', '50 to 59°F', '60 to 69°F', '70 to 79°F', '80 to 89°F', '90 to 99°F', '100 to 109°F', '110 to 119°F', '120 to 129°F', '>130°F');

  function WriteSafeString(UnsafeString: string) : string;

implementation  
  uses
    Classes, SysUtils;

  function WriteSafeString(UnsafeString: string) : string;
  var
    temp: string;
  begin
    temp := UnsafeString;
    temp := temp.Replace('=','Equals', [rfReplaceAll]);    
    temp := temp.Replace('/','And', [rfReplaceAll]);
    temp := temp.Replace('&','And', [rfReplaceAll]);
    temp := temp.Replace('<','LessThan', [rfReplaceAll]);
    temp := temp.Replace('>','GreaterThan', [rfReplaceAll]);
    temp := temp.Replace('%','Percent', [rfReplaceAll]);
    temp := temp.Replace('-','Negative', [rfReplaceAll]);
    temp := temp.Replace('+','Plus', [rfReplaceAll]);    
    temp := temp.Replace('°','Degrees', [rfReplaceAll]);
    temp := temp.Replace(' ','', [rfReplaceAll]);

    result := temp;
  end;
end.

