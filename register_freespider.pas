unit register_freespider; 

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,
  SpiderCGI, SpiderAction, SpiderForm, SpiderTable, SpiderPage;
  
procedure Register;

implementation

const
  ComponentPage = 'FreeSpider';

procedure RegisterUnitSpiderCGI;
begin
  RegisterComponents(ComponentPage, [TSpiderCGI]);
end;

procedure RegisterUnitSpiderAction;
begin
  RegisterComponents(ComponentPage, [TSpiderAction]);
end;

procedure RegisterUnitSpiderForm;
begin
  RegisterComponents(ComponentPage, [TSpiderForm]);
end;  

procedure RegisterUnitSpiderPage;
begin
  RegisterComponents(ComponentPage, [TSpiderPage]);
end;

procedure RegisterUnitSpiderTable;
begin
  RegisterComponents(ComponentPage, [TSpiderTable]);
end;

procedure Register;
begin
  RegisterUnit('SpiderCGI',@RegisterUnitSpiderCGI);
  RegisterUnit('SpiderAction',@RegisterUnitSpiderAction);
  RegisterUnit('SpiderForm',@RegisterUnitSpiderForm);
  RegisterUnit('SpiderPage',@RegisterUnitSpiderPage);
  RegisterUnit('SpiderTable',@RegisterUnitSpiderTable);
end; 

initialization
{$i spidericons.lrs}
 
end.
