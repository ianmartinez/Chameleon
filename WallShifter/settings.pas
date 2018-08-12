unit Settings;

{$mode objfpc}{$H+}

interface  
  const
    BatteryModes : array [0..11] of string =
      ('0%','1-9%','10-19%','20-29%','30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100%');

    TimeModes : array [0..23] of string =
      ('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM',
      '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM');

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
    temp := temp.Replace('<','LessThan', [rfReplaceAll]);
    temp := temp.Replace('>','GreaterThan', [rfReplaceAll]);
    temp := temp.Replace('%','Percent', [rfReplaceAll]);
    temp := temp.Replace('-','To', [rfReplaceAll]);
    temp := temp.Replace(' ','', [rfReplaceAll]);

    result := temp;
  end;
end.

