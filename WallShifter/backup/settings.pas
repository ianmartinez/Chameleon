unit Settings;

{$mode objfpc}{$H+}

interface  
  const
    PercentageModes : array [0..11] of string =
      ('0%','1 to 9%','10 to 19%','20 to 29%','30 to 39%', '40 to 49%', '50 to 59%', '60 to 69%', '70 to 79%', '80 to 89%', '90 to 99%', '100%');

    TimeModes : array [0..23] of string =
      ('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM',
      '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM');

    WindSpeedModes : array [0..11] of string =
      ('0 MPH','1 to 9 MPH','10 to 19 MPH','20 to 29 MPH','30 to 39 MPH', '40 to 49 MPH', '50 to 59 MPH', '60 to 69 MPH',
      '70 to 79 MPH', '80 to 89 MPH', '90 to 99 MPH', '100+ MPH');

    TemperatureModes : array [0..20] of string =
      ('<= -50°F', '-49 to -40°F', '-39 to -30°F', '-29 to -20°F', '-19 to -10°F', '-9 to -1°F', '0°F','1 to 9°F','10 to 19°F','20 to 29°F','30 to 39°F',
      '40 to 49°F', '50 to 59°F', '60 to 69°F', '70 to 79°F', '80 to 89°F', '90 to 99°F', '100 to 109°F', '110 to 119°F', '120 to 129°F', '>= 130°F');

  function WriteSafeString(UnsafeString: string) : string;
  function GetLocalFolder() : string;

implementation  
  uses
    Classes, SysUtils, Forms;

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
  end;

end.

