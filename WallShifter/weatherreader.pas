unit WeatherReader;

{$mode objfpc}{$H+}

interface
  function CalcHeatIndex(Temp: Integer; Humidity: Integer) : real;

implementation     
  uses
    Classes, SysUtils;

  function CalcHeatIndex(Temp: Integer; Humidity: Integer) : real;
  begin
        // To Do: Implement https://en.wikipedia.org/wiki/Heat_index#Formula
        Result := 0.0;
  end;
end.

