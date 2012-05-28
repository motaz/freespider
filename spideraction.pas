{
*******************************************************************************************************************

  SpiderAction:  Contains TSpiderAction component that used to handle request with path information
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 12.Sept.2009

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency

*******************************************************************************************************************

}

unit SpiderAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpiderUtils;

type

  { TSpiderAction }

  TSpiderAction = class(TComponent)
  private
    fPath: string;
    fOnRequest: TSpiderEvent;

    { Private declarations }
  protected
    procedure SetPath(const APath:  string);
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoRequest(fRequest: TSpiderRequest; fResponse: TSpiderResponse);
    { Public declarations }
  published
    property Path: string read fPath write SetPath;
    property OnRequest: TSpiderEvent read fOnRequest write fOnRequest;
    { Published declarations }
  end;

implementation

{ TSpiderAction }

procedure  TSpiderAction.SetPath(const APath: string);
begin
  if (APath <> '') and (APath[Length(APath)] = '/') then
    fPath:= Copy(APath, 1, Length(APath) - 1)
  else
    fPath:= APath;
end;

constructor TSpiderAction.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fPath:= Name;
end;

procedure TSpiderAction.DoRequest(fRequest: TSpiderRequest; fResponse: TSpiderResponse);
begin
  if Assigned(fOnRequest) then
    fOnRequest(Self, fRequest, fResponse);
end;

end.
