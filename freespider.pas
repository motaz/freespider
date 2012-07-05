{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FreeSpider;

interface

uses
  FreeSpiderIDEIntf, register_freespider, CGIUtils, ApacheUtils, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FreeSpiderIDEIntf', @FreeSpiderIDEIntf.Register);
  RegisterUnit('register_freespider', @register_freespider.Register);
end;

initialization
  RegisterPackage('FreeSpider', @Register);
end.
