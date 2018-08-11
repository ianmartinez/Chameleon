unit WeatherReader;

{$mode objfpc}{$H+}

interface
  function CalcHeatIndex(Temp: Integer; Humidity: Integer) : real;

implementation     
  uses
    Classes, SysUtils;

  function CalcHeatIndex(Temp: Integer; Humidity: Integer) : real;
  begin
        Result := 0.0;
  end;
end.

