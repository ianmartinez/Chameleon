unit Win32;

{$mode objfpc}{$H+}

interface
  procedure SetWallpaper(WallpaperPath: string);

implementation       
  uses
    Classes, SysUtils, windows;

  procedure SetWallpaper(WallpaperPath: string);
  begin
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, pchar(WallpaperPath), SPIF_SENDCHANGE);
  end;
end.

