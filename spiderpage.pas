{
************************************************************************************************************************

  SpiderPage:   Contains TSpiderPage component which used to link external HTML files, and use Tag for dynamic contents
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 31.7.2012

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency

************************************************************************************************************************

}

unit SpiderPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTagEvent = procedure(Sender: TObject; ATag: string; var TagReplacement: string) of object;

  { TSpiderPage }

  TSpiderPage = class(TComponent)
  private
    fPageFileName: string;
    fOnTag : TTagEvent;
    fTagStart: string;
    fTagStop: string;
    procedure SetTagStop(ATagStop: string);
    { Private declarations }
  protected
    { Protected declarations }
    function GetContent: string;
  public
    constructor Create(TheOwner: TComponent); override;
    property Contents: string read GetContent;
    { Public declarations }
  published
    property PageFileName: string read fPageFileName write fPageFileName;
    property TagStart: string read fTagStart write fTagStart;
    property TagStop: string read fTagStop write SetTagStop;
    property TagStopt: string read fTagStop write SetTagStop;

    property OnTag: TTagEvent read fOnTag write fOnTag;

    { Published declarations }
  end;


implementation

{ TSpiderPage }

procedure TSpiderPage.SetTagStop(ATagStop: string);
begin
  if ATagStop = fTagStart then
   raise Exception.Create('TagStop should not be equal to TagStart')
  else
    fTagStop:= ATagStop;
end;

function TSpiderPage.GetContent: string;
var
  F: TextFile;
  Line: string;
  OnTagAssigned: Boolean;
  ATag: string;
  TagReplacement: string;
begin
  if FileExists(fPageFileName) then
  begin
    OnTagAssigned:= Assigned(fOnTag);

    AssignFile(F, fPageFileName);
    Reset(F);
    while not Eof(F) do
    begin
      Readln(F, Line);
      if OnTagAssigned then
      while (Pos(fTagStart, Line) > 0) and (Pos(fTagStop, Line) > 0) do
      begin
        ATag:= Copy(Line, Pos(fTagStart, Line) + Length(fTagStart), Length(Line));
        if Pos(fTagStop, ATag) > 0 then
        begin
          ATag:= Copy(ATag, 1, Pos(fTagStop, ATag) - 1);
          TagReplacement:= '';
          fOnTag(self, ATag, TagReplacement);
          if TagReplacement = '' then
          begin
            Result:= Result + Copy(Line, 1, Pos(fTagStart, Line) + Length(fTagStart) - 1);
            Delete(Line, 1, Pos(fTagStart, Line) + Length(fTagStart) - 1);
          end
          else
            Line:= StringReplace(Line, fTagStart + ATag + fTagStop, TagReplacement, [rfIgnoreCase]);
        end
        else
        begin
          Result:= Result + Copy(Line, 1, Pos(fTagStart, Line) + Length(fTagStart) - 1);
          Delete(Line, 1, Pos(fTagStart, Line) + Length(fTagStart) - 1);
        end;

      end;
      Result:= Result + Line + #10;
    end;
    CloseFile(F);

  end
  else
    raise Exception.Create('File: ' + fPageFileName + ' not found');
end;

constructor TSpiderPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fTagStart:= '@';
  fTagStop:= ';';
end;

end.
