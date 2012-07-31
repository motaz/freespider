{
*******************************************************************************************************************

  SpiderForm:  Contains TSpiderForm component which used to generate HTML form
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 31.July.2012

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency

  Jul/2010 - Modified byHéctor S. Ponce
    * Add check box

  16.July.2010 adding AddHTML, LabelCSS in AddText by Cem Zafer Demirsoy
*******************************************************************************************************************

}

unit SpiderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TInputType = (itText, itPassword, itButton, itSubmit, itTextArea, itFile, itHidden, itCheckbox);

  { TSpiderForm }

  TSpiderForm = class(TComponent)
  private
    fMethod: string;
    fAction: string;
    fExtraParam: string;
    fPutInTable: Boolean;

    fConentList: TStringList;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddText(AText: string; LabelCSS: string = '');
    procedure AddInput(InputType: TInputType; const InputName: string; const InputValue: string = ''; const ExtraParam: string = '';
      NewLine: Boolean = True);
    procedure AddHTML(AHTMLText: string);
    function Contents: string;
    procedure Clear;
    { Public declarations }
  published
    property Method: string read fMethod write fMethod;
    property Action: string read fAction write fAction;
    property PutInTable: Boolean read fPutInTable write fPutInTable;
    property ExtraParam: string read fExtraParam write fExtraParam;

    { Published declarations }
  end;

implementation


{ TSpiderForm }

constructor TSpiderForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fConentList:= TStringList.Create;
  Method:= 'POST';
  fPutInTable:= True;
end;

destructor TSpiderForm.Destroy;
begin
  fConentList.Free;
  inherited Destroy;
end;

procedure TSpiderForm.AddText(AText: string; LabelCSS: string = '');
var
  TR, TD, STD: string;
  LB, LBT: string;
begin
  if fPutInTable then
  begin
    TR:= '<TR>';
    TD:= '<TD>';
    STD:= '</TD>';
  end;
  LB:= '';
  LBT:= '';
  if LabelCSS <> '' then
  begin
    LB:= '<LABEL ' + LabelCSS + '>';
    LBT:= '</LABEL>';
  end;
  fConentList.Add(TR + TD + LB + AText + LBT + STD);
end;

procedure TSpiderForm.AddInput(InputType: TInputType; const InputName: string; const InputValue: string;
  const ExtraParam: string;  NewLine: Boolean);
var
  STR, TD, STD, NameAttr: string;
begin
  if fPutInTable then
  begin
    TD:= '<TD>';
    STR:= '</TR>';
    STD:= '</TD>';
  end;

  if InputName <> '' then
    NameAttr := ' name="' + InputName + '"'
  else
    NameAttr := '';

  case InputType of
    itText: fConentList.Add(TD + '<input type="text"'+ NameAttr + ' value="' + InputValue + '" ' +
      ExtraParam + '/>' + STD + STR);

    itPassword: fConentList.Add(TD + '<input type="password"' + NameAttr + ' value="' + InputValue + '" ' +
      ExtraParam + '/>' + STD + STR);

    itButton: fConentList.Add(TD + '<input type="button"' + NameAttr + ' value="' + InputValue + '" ' +
      ExtraParam + '/>' + STD + STR);

    itSubmit: fConentList.Add(TD+ '<input type="submit"' + NameAttr + ' value="' + InputValue + '" ' +
      ExtraParam + '/>' + STD + STR);

    itTextArea: fConentList.Add(TD + '<textarea' + NameAttr + ' ' + ExtraParam + '>' + InputValue +
      '</textarea>' + STD + STR);

    itFile: fConentList.Add(TD + '<input type="file"' + NameAttr + ' value="' + InputValue + '" '   + ExtraParam + '/>'
      + STD + STR);

    itHidden: fConentList.Add('<input type="hidden"' + NameAttr + '  value="' + InputValue + '" '  + ExtraParam + '/>');

    itCheckbox: fConentList.Add(TD + '<input type="checkbox" name="' + InputName + '" value="'+ InputValue + '" ' +
      ExtraParam + '/> ' + STD + STR);

  end;
  if (not fPutInTable) and NewLine then
    fConentList.Add('<br />');
end;

procedure TSpiderForm.AddHTML(AHTMLText: string);
begin
  fConentList.Add(AHTMLText);
end;

function TSpiderForm.Contents: string;
begin
  if fPutInTable then
  begin
    fConentList.Insert(0, '<table>');
    fConentList.Add('</table>');
  end;
  Result:= '<form method="' + fMethod + '" action="' + fAction + '" ' + fExtraParam + '>' + #10 +
    fConentList.Text + '</form>';
end;

procedure TSpiderForm.Clear;
begin
  fConentList.Clear;
end;

end.
