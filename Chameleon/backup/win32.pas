unit Win32;
{$mode objfpc}{$H+}

interface
  procedure SetWallpaper(WallpaperPath: string);
uses
  Classes, SysUtils, Windows, Graphics;


implementation
  procedure SetWallpaper(WallpaperPath: string);
  begin
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, pchar(WallpaperPath), SPIF_SENDCHANGE);
  end;
end.
