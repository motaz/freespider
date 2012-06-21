{
*******************************************************************************************************************

  SpiderTable:  Contains TSpiderTable component which can be used to generate HTML tables, manually
                or from  date set
  Author:       Motaz Abdel Azeem
  email:        motaz@code.sd
  Home page:    http://code.sd
  License:      LGPL
  Last modifie: 12.June.2012

  Jul/2010 - Modified by Luiz Américo
    * Remove LCL dependency

*******************************************************************************************************************

}

unit SpiderTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

type

  THeaderEvent  = procedure(Sender: TObject; DataSet: TDataSet; ColCount: Integer;
    var CellData, BgColor, ExtraParams: string) of object;

  TCellEvent  = procedure(Sender: TObject; DataSet: TDataSet; ColCount, RowCount: Integer;
    var CellData, BgColor, ExtraParams: string) of object;

  { TSpiderTable }

  TSpiderTable = class(TComponent)
  private
    fBorder: Byte;
    fTableExtraParams: string;
    fColumnCount: SmallInt;
    fHeader: array of string;
    fRows: array of array of string;
    fHeaderColor: string;
    fHeaderExtraParams: string;
    fDefaultRowColor: string;
    fRowsColor: array of string;
    fRowsExtraParams: array of string;
    fDataSet: TDataSet;

    fOnDrawHeader : THeaderEvent;
    fOnDrawCell: TCellEvent;

    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColumnCount(ACount: SmallInt);
    procedure SetHeader(Cols: array of string); overload;
    procedure SetHeader(Cols: TStringList); overload;
    procedure AddRow(RowColor, RowExtraParams: string; Cells: array of string); overload;
    procedure AddRow(RowColor: string; Cells: array of string); overload;
    procedure AddRow(Cells: array of string); overload;
    procedure AddRow(Cells: TStringList; BgColor: string = ''); overload;

    function GetContent: string;
    procedure Clear;

    property Contents: string read GetContent;

    { Public declarations }

  published
    property Border: Byte read fBorder write fBorder;
    property TableExtraParams: string read fTableExtraParams write fTableExtraParams;
    property HeaderExtraParams: string read fHeaderExtraParams write fHeaderExtraParams;
    property ColumnCount: SmallInt read fColumnCount write SetColumnCount default 2;
    property HeaderColor: string read fHeaderColor write fHeaderColor;
    property DataSet: TDataSet read fDataSet write fDataSet;
    property RowColor: string read fDefaultRowColor write fDefaultRowColor;

    property OnDrawHeader: THeaderEvent read fOnDrawHeader write fOnDrawHeader;
    property OnDrawDataCell: TCellEvent read fOnDrawCell write fOnDrawCell;



    { Published declarations }
  end;

implementation

{ TSpiderTable }

constructor TSpiderTable.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fDataSet:= nil;
end;

destructor TSpiderTable.Destroy;
begin
  SetLength(fRows, 0);
  setLength(fHeader, 0);
  SetLength(fRowsColor, 0);
  inherited Destroy;
end;

procedure TSpiderTable.SetColumnCount(ACount: SmallInt);
begin
  fColumnCount:= ACount;
end;

procedure TSpiderTable.SetHeader(Cols: array of string);
var
  i: Integer;
begin
  fColumnCount:= Length(Cols);
  SetLength(fHeader, fColumnCount);
  for i:= 0 to fColumnCount - 1 do
    fHeader[i]:= Cols[i];
  SetLength(fRows, 0);
  SetLength(fRowsColor, 0);
  SetLength(fRowsExtraParams, 0);
end;

procedure TSpiderTable.SetHeader(Cols: TStringList);
var
  i: Integer;
begin
  fColumnCount:= Cols.Count;
  SetLength(fHeader, fColumnCount);
  for i:= 0 to fColumnCount - 1 do
    fHeader[i]:= Cols[i];
  SetLength(fRows, 0);
  SetLength(fRowsColor, 0);
  SetLength(fRowsExtraParams, 0);
end;

procedure TSpiderTable.AddRow(RowColor, RowExtraParams: string; Cells: array of string);
var
  i: Integer;
begin
  SetLength(fRows, Length(fRows) + 1);  // Add new row

  SetLength(fRows[High(fRows)], fColumnCount); // add new columns array

  SetLength(fRowsColor, Length(fRowsColor) + 1);
  SetLength(fRowsExtraParams, Length(fRowsExtraParams) + 1);

  if Trim(RowColor) <> '' then
    fRowsColor[High(fRowsColor)]:= Trim(RowColor)
  else
    fRowsColor[High(fRowsColor)]:= Trim(fDefaultRowColor);
  fRowsExtraParams[High(fRowsExtraParams)]:= RowExtraParams;

  for i:= 0 to fColumnCount - 1 do
    fRows[High(fRows), i]:= Cells[i];
end;

procedure TSpiderTable.AddRow(RowColor: string; Cells: array of string);
begin
  AddRow(RowColor, '', Cells);
end;

procedure TSpiderTable.AddRow(Cells: array of string);
begin
  AddRow('', '', Cells);
end;

procedure TSpiderTable.AddRow(Cells: TStringList; BgColor: string = '');
var
  CellsStr: array of string;
  i: Integer;
begin
  SetLength(CellsStr, fColumnCount);
  for i:= 0 to Cells.Count -1 do
    if i < fColumnCount then
      CellsStr[i]:= Cells[i];
  AddRow(BgColor, CellsStr);
  CellsStr:= nil;
end;

function TSpiderTable.GetContent: string;
var
  x, y: Integer;
  BgColor: string;
  CellData: string;
  ExtraParams: string;
begin
  try
    // Header
    Result:= '<table border = ' + IntToStr(fBorder)  + ' ' + fTableExtraParams + '><tr ' + fHeaderExtraParams + ' ';
    if fHeaderColor <> '' then
      Result:= Result + ' bgcolor="' + fHeaderColor + '"';
    Result:= Result + '>';

    // DataSet
    if fDataSet <> nil then
    begin
      if not fDataSet.Active then
        fDataSet.Open;

      // Header
      for x:= 0 to fDataSet.FieldCount - 1 do
      if fDataSet.Fields[x].Visible then
      begin
        Result:= Result + '<th ';
        CellData:= fDataSet.Fields[x].DisplayName;
        BgColor:= '';
        ExtraParams:= '';

        if Assigned(fOnDrawHeader) then
        begin
          fOnDrawHeader(Self, fDataSet, x, CellData, BgColor, ExtraParams);
          if Trim(BgColor) <> '' then
            Result:= Result + 'BgColor="' + BgColor + '" ' + ExtraParams + ' ';
        end;
        Result:= Result + '>' + CellData + '</th>';
      end;

      Result:= Result + '</tr>' + #10;

      // Rows
      fDataSet.First;
      while not fDataSet.EOF do
      begin
        Result:= Result + '<tr bgcolor="' + fDefaultRowColor + '">';
        for x:= 0 to fDataSet.FieldCount -1 do
        if fDataSet.Fields[x].Visible then
        begin
          BgColor:= '';
          ExtraParams:= '';
          CellData:= fDataSet.Fields[x].AsString;
          Result:= Result + '<td ';
          if Assigned(fOnDrawCell) then
          begin
            fOnDrawCell(self, fDataSet, x, fDataSet.RecNo - 1, CellData, BgColor, ExtraParams);
            if BgColor <> '' then
              Result:= Result + 'BgColor="' + BgColor + '" ' + ExtraParams + ' ';
          end;

          Result:= Result + '>' + CellData + '</td>';
        end;
        Result:= Result + '</tr>' + #10;
        fDataSet.Next;
      end;


    end
    else   // Manual table
    begin

      // put header
      if fHeader <> nil then
      begin

        for x:= 0 to fColumnCount - 1  do
        begin
          Result:= Result + '<th ' ;
          BgColor:= '';
          ExtraParams:= '';
          CellData:= fHeader[x];
          if Assigned(fOnDrawHeader) then
          begin
            fOnDrawHeader(Self, nil, x, CellData, BgColor, ExtraParams);
            if Trim(BgColor) <> '' then
              Result:= Result + 'BgColor="' + BgColor + '" ' + ExtraParams + ' ';
          end;
          Result:= Result + '>' + CellData + '</th>';
        end;
        Result:= Result + '</tr>' + #10;

      end;

      // Rows
      if fRows <> nil then
      begin
        for y:= 0 to High(fRows) do
        begin
          Result:= Result + '<tr bgcolor="' + fRowsColor[y] + '" ' + fRowsExtraParams[y] + ' >';
          for x:= 0 to High(fRows[y]) do
          begin
            BgColor:= '';
            ExtraParams:= '';
            Result:= Result + '<td ';
            CellData:= fRows[y, x];
            if Assigned(fOnDrawCell) then
            begin
              fOnDrawCell(self, nil, x, y, CellData, BgColor, ExtraParams);
              if BgColor <> '' then
                Result:= Result + 'BgColor="' + BgColor + '" ' + ExtraParams + ' ';
            end;
            Result:= Result + '>' + CellData + '</td>';
          end;
          Result:= Result + '</tr>' + #10;
        end;
      end;
    end;

    Result:= Result + '</table>' + #10;

  except
  on e: exception do
    Result:= e.Message;
  end;
end;

procedure TSpiderTable.Clear;
begin
  SetLength(fHeader, 0);
  SetLength(fRows, 0);
  SetLength(fRowsColor, 0);
end;


end.

