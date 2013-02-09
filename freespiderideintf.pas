unit FreeSpiderIDEIntf;

// Comments, 25-1-2013

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormEditingIntf, ProjectIntf, NewItemIntf, LazIDEIntf,
  Controls, Forms, SpiderCGI;

Type

  { TFreeSpiderModuleDescriptor }

  TFreeSpiderModuleDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

  { TFreeSpiderFileDescriptor }

  TFreeSpiderFileDescriptor = Class(TFileDescPascalUnitWithResource)
  Public
    Constructor Create; override;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

  { TFreeSpiderProjectDescriptor }

  TFreeSpiderProjectDescriptor = class(TProjectDescriptor)
  public
    constructor create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
  published
    { Published declarations }
  end;

  { TFreeSpiderApacheProjectDescriptor }

  TFreeSpiderApacheProjectDescriptor = class(TProjectDescriptor)
  public
    constructor create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles(AProject: TLazProject) : TModalResult; override;
  published
    { Published declarations }
  end;

  { TFreeSpiderApacheSecondaryDescriptor }



Procedure Register;

implementation

uses FreeSpider;

Resourcestring
  SSpiderApps     = 'FreeSpider Web Application';
  SSpiderAppName  = 'FreeSpider CGI Web Application';
  SSpiderAppDescr = 'FreeSpider Web application, which is similar to WebBroker in Delphi.';
 // SSpiderName     = 'FreeSpider Data Module';
  //SSpiderDescr    = 'Data Module contains SpiderCGI, and other components';
  SSpiderModuleName  = 'Data Module';
  SSpiderModuleDescr = 'Data Module contains SpiderCGI/SpiderApache and other components';

  ASpiderApps = 'FreeSpider Apache Module web application';
  ASpiderAppName = 'FreeSpider Apache module web application';
  ASpiderAppDescr = 'FreeSpider Apache module library';



Procedure Register;

begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(SSpiderApps));
  RegisterProjectDescriptor(TFreeSpiderProjectDescriptor.Create, SSpiderApps);
  //RegisterProjectFileDescriptor(TFreeSpiderFileDescriptor.Create,SSpiderApps);
  RegisterProjectFileDescriptor(TFreeSpiderModuleDescriptor.Create, SSpiderApps);
 // FormEditingHook.RegisterDesignerBaseClass(TSpiderMapper);
//  FormEditingHook.RegisterDesignerBaseClass(TSpider);

  // Apache module
  RegisterNewItemCategory(TNewIDEItemCategory.Create(ASpiderApps));
  RegisterProjectDescriptor(TFreeSpiderApacheProjectDescriptor.Create, ASpiderApps);
  RegisterProjectFileDescriptor(TFreeSpiderModuleDescriptor.Create, ASpiderApps);
end;


{ TFreeSpiderApacheProjectDescriptor }

constructor TFreeSpiderApacheProjectDescriptor.create;
begin
  inherited create;
  Flags:= Flags - [pfMainUnitHasCreateFormStatements];
  Name:= 'Spider Apache Module Application';
end;

function TFreeSpiderApacheProjectDescriptor.GetLocalizedName: string;
begin
  //Result:= inherited GetLocalizedName;
  Result:= ASpiderAppName;
end;

function TFreeSpiderApacheProjectDescriptor.GetLocalizedDescription: string;
begin
  //Result:= inherited GetLocalizedDescription;
  Result:= ASpiderAppDescr;
end;

function TFreeSpiderApacheProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
Var
  F : TLazProjectFile;
  Src : TStrings;
begin
  Result:= Inherited InitProject(AProject);
  If (Result=mrOK) then
    begin
    AProject.AddPackageDependency('FreeSpider');
    AProject.Title:= 'Spider Apache Module';
    AProject.LazCompilerOptions.Win32GraphicApp:= False;
    AProject.ProjectInfoFile:= 'mod_proj1.lpi';
    F:= AProject.CreateProjectFile('mod_proj1.lpr');
    F.IsPartOfProject:= True;
    AProject.AddFile(F, False);
    AProject.MainFileID:= 0;
    Src:= TStringList.Create;
    try
      With Src do
        begin
          Add('library mod_proj1;');
          Add('');
          Add('{$ifdef fpc}');
          Add(' {$mode objfpc}{$H+}');
          Add('{$endif}');
          Add('');
          Add('{$IFDEF WIN32}');
          Add('  {$DEFINE WINDOWS}');
          Add('{$ENDIF}');
          Add('');
          Add('{$define Apache2_0}');
          Add('');
          Add('uses SysUtils, httpd, apr, apacheadapter, Classes;');
          Add('');
          Add('{ Note:');
          Add(' These configrations are used by apache2.conf or httpd.conf file.');
          Add(' Make sure to confgure it properly in every new Add FreeSpider Apache Module Project');
          Add('}');
          Add('');
          Add('const');
          Add('');
        {$IFDEF WINDOWS}
          Add('       MODULE_NAME = ''mod_proj1.dll'';');
        {$ELSE}
          Add('       MODULE_NAME = ''mod_proj1.so'';');
        {$ENDIF}
          Add('       MODNAME = ''apache_mod1'';');
          Add('       HANDLER_NAME = ''proj1-handler'';');
          Add('');
          Add('{');
          Add(' This is the configuration in apache2.conf file for above settings in Linux:');
          Add('');
          Add('LoadModule apache_mod1 /usr/lib/apache2/modules/mod_proj1.so');
          Add('');
          Add('<Location /proj1>');
          Add('     SetHandler proj1-handler');
          Add('</Location>');
          Add('');
          Add('and this is Windows configuration version of the same exmaple:');
          Add('');
          Add('LoadModule apache_starter c:\projects\ApacheStarter\mod_proj1.dll');
          Add('');
          Add('<Location /proj1>');
          Add('     SetHandler proj1-handler');
          Add('</Location>');
          Add('');
          Add('Example of accessing this module from browser: ');
          Add('   http://localhost/proj1');
          Add('}');
          Add('');
          Add('');
          Add('');
          Add('');
          Add('var');
          Add('   current_module: module; {$ifdef Unix} public name modName; {$endif}');
          Add('   default_module_ptr: Pmodule;');
          Add('');
          Add('');
          Add('');
          Add('exports');
          Add('   current_module name ModName;');
          Add('');
          Add('function DefaultHandler(r: Prequest_rec): Integer; cdecl;');
          Add('begin');
          Add('// Thread pooling is turned to False by default, you can turn it to True');
          Add('   Result:= ProcessHandler(r, TDataModule1, MODULE_NAME, HANDLER_NAME, False);');
          Add('end;');
          Add('');
          Add('');
          Add('procedure RegisterHooks(p: Papr_pool_t); cdecl;');
          Add('begin');
          Add('   ap_hook_handler(@DefaultHandler, nil, nil, APR_HOOK_MIDDLE);');
          Add('end;');
          Add('');
          Add('begin');
          Add('');
          Add('  default_module_ptr := @current_module;');
          Add('  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);');
          Add('');
          Add('  STANDARD20_MODULE_STUFF(current_module);');
          Add('');
          Add('  with current_module do');
          Add('  begin');
          Add('     name := MODULE_NAME;');
          Add('     register_hooks := @RegisterHooks;');
          Add('  end;');
          Add('end.');
      end;
      F.SetSourceText(Src.Text);
    finally
      Src.Free;
    end;
   end;
end;

function TFreeSpiderApacheProjectDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
Var
  FD : TProjectFileDescriptor;
  O : TNewFlags;
begin
  FD:= ProjectFileDescriptors.FindByName('DataModule1');

  O:= [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc];
  Result:= LazarusIDE.DoNewEditorFile(FD, 'main.pas', '', O);
end;


{ TFreeSpiderFileDescriptor }

constructor TFreeSpiderFileDescriptor.Create;
begin
  inherited Create;
  ResourceClass:= TDataModule;
  Name:= 'Data Module';
  UseCreateFormStatements:= False;
end;

function TFreeSpiderFileDescriptor.GetLocalizedName: String;
begin
  Result:=SSpiderAppName;
end;

function TFreeSpiderFileDescriptor.GetLocalizedDescription: String;
begin
  Result:= SSpiderAppDescr;
end;

function TFreeSpiderFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:= inherited GetInterfaceUsesSection + '';
end;

function TFreeSpiderFileDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
Var
  Src : TStrings;
begin
  Src:=TStringList.Create;
  try
    Result:= inherited GetImplementationSource(Filename, SourceName, ResourceName);
    With Src do
    begin
      Add('Procedure RegisterSpider;');
      Add('begin');
      Add('  RegisterSpiderClass(T'+ ResourceName + ')');
      Add('end;');
      Add('');
      Add(Result);
      Add('  RegisterSpider;');
      Result:=Text;
    end;
  finally
    Src.Free;
  end;
end;

{ TFreeSpiderProjectDescriptor }

constructor TFreeSpiderProjectDescriptor.create;

begin
  Inherited;
  Flags:= Flags - [pfMainUnitHasCreateFormStatements];
  Name:= 'Spider Application';
end;


function TFreeSpiderProjectDescriptor.GetLocalizedName: string;
begin
  Result:= SSpiderAppName;
end;

function TFreeSpiderProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:= SSpiderAppDescr;
end;

function TFreeSpiderProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
Var
  F : TLazProjectFile;
  Src : TStrings;

begin
  Result:= Inherited InitProject(AProject);
  If (Result=mrOK) then
    begin
    AProject.AddPackageDependency('FreeSpider');
    AProject.Title:= 'FreeSpider Web Application';
    AProject.LazCompilerOptions.Win32GraphicApp:= False;
    AProject.ProjectInfoFile:= 'project1.lpi';
    F:= AProject.CreateProjectFile('project1.lpr');
    F.IsPartOfProject:= True;
    AProject.AddFile(F,False);
    AProject.MainFileID:= 0;
    Src:= TStringList.Create;
    try
      With Src do
        begin
        Add('Program SpiderProj1;');
        Add('');
        Add('');
        Add('Uses');
        Add('{$IFDEF UNIX}{$IFDEF UseCThreads}');
        Add('  CThreads,');
        Add('{$ENDIF}{$ENDIF}');
        Add('main');
        Add('  { add your units here };');
        Add('');
        Add('begin');
        Add('  DataModule1:= TDataModule1.Create(nil)');
        Add('end.');
        end;
      F.SetSourceText(Src.Text);
    finally
      Src.Free;
    end;
    end;
end;

function TFreeSpiderProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
Var
  FD : TProjectFileDescriptor;
  O : TNewFlags;
begin
  FD:= ProjectFileDescriptors.FindByName('DataModule1');
  O:= [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc];
  Result:= LazarusIDE.DoNewEditorFile(FD, 'main.pas', '', O);
end;

{ TFreeSpiderModuleDescriptor }

constructor TFreeSpiderModuleDescriptor.Create;
begin
  inherited Create;
  Name:= 'DataModule1';

  ResourceClass:= TDataModule;
  UseCreateFormStatements:= False;
  Self.DefaultSourceName:= 'temp';
end;

function TFreeSpiderModuleDescriptor.GetLocalizedName: String;
begin
  Result:= SSpiderModuleName;
end;

function TFreeSpiderModuleDescriptor.GetLocalizedDescription: String;
begin
  Result:= SSpiderModuleDescr;
end;

function TFreeSpiderModuleDescriptor.GetInterfaceUsesSection: String;
begin
  Result:= inherited GetInterfaceUsesSection+'';
end;

function TFreeSpiderModuleDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
Var
  Src : TStrings;
begin
  Src:= TStringList.Create;
  try
    Result:= inherited GetImplementationSource(Filename, SourceName, ResourceName);
    With Src do
    begin
      Add('');
      Add(Result);
      Result:=Text;
    end;
  finally
    Src.Free;
  end;
end;

end.

