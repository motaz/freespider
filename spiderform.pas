{
*******************************************************************************************************************

  SpiderForm:  Contains TSpiderForm component which used to generate HTML form
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 23.Dec.2012

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
    fRowOpened: Boolean;

    fConentList: TStringList;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddText(AText: string; LabelCSS: string = ''; CloseRow: Boolean = False);
    procedure AddInputExt(InputType: TInputType; const InputName: string; const InputValue: string = ''; const ExtraParam: string = '';
      NewLine: Boolean = True; CloseRow: Boolean = True; TextBefor: String = ''; TextAfter: string = '');
    procedure AddHTML(AHTMLText: string);
    procedure AddInput(InputType: TInputType; const InputName: string; const InputValue: string = ''; const ExtraParam: string = '';
      NewLine: Boolean = True);
    function Contents: string;
    procedure Clear;
    procedure NewRow;
    procedure CloseRow;
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
  fRowOpened:= False;
end;

destructor TSpiderForm.Destroy;
begin
  fConentList.Free;
  inherited Destroy;
end;

procedure TSpiderForm.AddText(AText: string; LabelCSS: string; CloseRow: Boolean = False
  );
var
  TR, TD, STD, STR: string;
  LB, LBT: string;
begin
  if fPutInTable then
  begin
    if not fRowOpened then
    begin
      TR:= '<TR>';
      fRowOpened:= True;
    end;
    TD:= '<TD>';
    STD:= '</TD>';
    if CloseRow then
    begin
      STR:= '</TR>';
      fRowOpened:= False;
    end
    else
      STR:= '';
  end;
  LB:= '';
  LBT:= '';
  if LabelCSS <> '' then
  begin
    LB:= '<LABEL ' + LabelCSS + '>';
    LBT:= '</LABEL>';
  end;
  fConentList.Add(TR + TD + LB + AText + LBT + STD + STR);
end;

procedure TSpiderForm.AddInputExt(InputType: TInputType; const InputName: string;
  const InputValue: string; const ExtraParam: string; NewLine: Boolean;
  CloseRow: Boolean = True; TextBefor: String = ''; TextAfter: string = '');
var
  TR, STR, TD, STD, NameAttr: string;
begin
  if fPutInTable then
  begin
    if not fRowOpened then
    begin
      TR:= '<TR>';
      fRowOpened:= True;
    end
    else
     TR:= '';
    TD:= '<TD>';
    STD:= '</TD>';
    if CloseRow then
    begin
      STR:= '</TR>';
      fRowOpened:= False;
    end
    else
      STR:= '';
  end;

  if InputName <> '' then
    NameAttr := ' name="' + InputName + '"'
  else
    NameAttr := '';

  fConentList.Add(TR + TD + TextBefor);

  case InputType of
    itText: fConentList.Add('<input type="text"'+ NameAttr + ' value="' + InputValue + '" ' + ExtraParam + ' />' );

    itPassword: fConentList.Add('<input type="password"' + NameAttr + ' value="' + InputValue + '" ' + ExtraParam + ' />' );

    itButton: fConentList.Add('<input type="button"' + NameAttr + ' value="' + InputValue + '" ' + ExtraParam + '/>');

    itSubmit: fConentList.Add('<input type="submit"' + NameAttr + ' value="' + InputValue + '" ' + ExtraParam + ' />');

    itTextArea: fConentList.Add('<textarea' + NameAttr + ' ' + ExtraParam + '>' + InputValue + '</textarea>');

    itFile: fConentList.Add('<input type="file"' + NameAttr + ' value="' + InputValue + '" '   + ExtraParam + ' />');

    itHidden: fConentList.Add('<input type="hidden"' + NameAttr + '  value="' + InputValue + '" '  + ExtraParam + ' />');

    itCheckbox: fConentList.Add('<input type="checkbox" name="' + InputName + '" value="'+ InputValue + '" ' +
      ExtraParam + ' /> ');

  end;

  fConentList.Add(TextAfter + STD + STR);

  if (not fPutInTable) and NewLine then
    fConentList.Add('<br />');
end;

procedure TSpiderForm.AddHTML(AHTMLText: string);
begin
  fConentList.Add(AHTMLText);
end;

procedure TSpiderForm.AddInput(InputType: TInputType; const InputName: string;
  const InputValue: string; const ExtraParam: string; NewLine: Boolean);
begin
  AddInputExt(InputType, InputName, InputValue, ExtraParam, NewLine);
end;

function TSpiderForm.Contents: string;
begin
  if fPutInTable then
  begin
    fConentList.Insert(0, '<table>');
    fConentList.Add('</table>');
  end;
  Result:= '<form method="' + fMethod + '" action="' + fAction + '" ' + fExtraParam + ' >' + #10 +
    fConentList.Text + '</form>';
end;

procedure TSpiderForm.Clear;
begin
  fConentList.Clear;
  fRowOpened:= False;
end;

procedure TSpiderForm.NewRow;
begin
  if fPutInTable then
  begin
    fConentList.Add('<tr>');
    fRowOpened:= True;
  end;
end;

procedure TSpiderForm.CloseRow;
begin
  if fPutInTable then
  begin
    fConentList.Add('</tr>');
    fRowOpened:= False;
  end;
end;

end.
