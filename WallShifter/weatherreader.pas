unit WeatherReader;

{$mode objfpc}{$H+}

interface
  function CalcHeatIndex(Temperature: Integer; Humidity: Integer) : Integer;

implementation     
  uses
    Classes, SysUtils;

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

