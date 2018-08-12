unit Settings;

{$mode objfpc}{$H+}

interface  
  const
    BatteryModes : array [0..11] of string =
      ('0%','1-9%','10-19%','20-29%','30-39%', '40-49%', '50-59%', '60-69%', '70-79%', '80-89%', '90-99%', '100%');
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
    temp := temp.Replace('-','', [rfReplaceAll]);

    result := temp;
  end;
end.

