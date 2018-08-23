unit Win32;
{$mode objfpc}{$H+}

interface
  procedure SetWallpaper(WallpaperPath: string);


implementation               
  uses
    Classes, SysUtils, Windows, Graphics;

  procedure SetWallpaper(WallpaperPath: string);
  begin
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, pchar(WallpaperPath), SPIF_SENDCHANGE);
  end;
end.
