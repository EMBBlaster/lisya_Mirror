unit lisya_highlighter;
(*
  This is an example how to implement your own highlighter.

  This example does allow to specify different colors for
  - text (defaults to not-highlighted)
  - spaces  (defaults to silver frame)
  - words, separated by spaces, that start with a,e,i,o,u  (defaults to bold)
  - the word "not"  (defaults to red background)

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

  How it works:

  - Creation
    The Highlighter creates Attributes that it can return the Words and Spaces.

  - SetLine
    Is called by SynEdit before a line gets painted (or before highlight info is needed)
    This is also called, each time the text changes fol *all* changed lines
    and may even be called for all lines after the change up to the end of text.

    After SetLine was called "GetToken*" should return information about the
    first token on the line.
    Note: Spaces are token too.

  - Next
    Scan to the next token, on the line that was set by "SetLine"
    "GetToken*"  should return info about that next token.

  - GetEOL
    Returns True, if "Next" was called while on the last token of the line.

  - GetTokenEx, GetTokenAttribute
    Provide info about the token found by "Next"

  - Next, GetEOL. GetToken*
    Are used by SynEdit to iterate over the Line.
    Important: The tokens returned for each line, must represent the original
    line-text (mothing added, nothing left out), and be returned in the correct order.

    They are called very often and should perform ath high speed.

  - GetToken, GetTokenPos, GetTokenKind
    SynEdit uses them e.g for finding matching brackets. If GetTokenKind returns
    different values per Attribute, then brackets only match, if they are of the
    same kind (e.g, if there was a string attribute, brackets outside a string
    would not match brackets inside a string)


*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter
  ,lisya_string_predicates in '../src/lisya_string_predicates.pas';


type

  { TSynDemoHl }


  TSynLisya = class(TSynCustomHighlighter)
  private
    fOperators: TStringList;
    fModOperators: TStringList;
    fOperatorAttri: TSynHighlighterAttributes;
    fModOperatorAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    procedure SetOperatorAttri(AValue: TSynHighlighterAttributes);
    procedure SetModOperatorAttri(AValue: TSynHighlighterAttributes);
    procedure SetStringAttri(AValue: TSynHighlighterAttributes);
    procedure SetNumberAttri(AValue: TSynHighlighterAttributes);
    procedure SetIdentifierAttri(AValue: TSynHighlighterAttributes);
    procedure SetCommentAttri(AValue: TSynHighlighterAttributes);
  protected
    // accesible for the other examples
    FTokenPos, FTokenEnd: Integer;
    FLineText: String;
    fInString: integer;
  public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
  public
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Define Attributes, for the different highlights. *)
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write SetOperatorAttri;
    property ModOperatorAttri: TSynHighlighterAttributes read fModOperatorAttri
      write SetModOperatorAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write SetStringAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write SetNumberAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write SetIdentifierAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write SetCommentAttri;
  end;

implementation

function num_token(s: string): boolean;
begin

end;

constructor TSynLisya.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  (* Create and initialize the attributes *)
  fOperatorAttri := TSynHighlighterAttributes.Create('operator', 'operator');
  AddAttribute(fOperatorAttri);
  fOperatorAttri.Style := [fsBold];

  fModOperatorAttri := TSynHighlighterAttributes.Create('mod_operator', 'mod_operator');
  AddAttribute(fModOperatorAttri);
  fModOperatorAttri.Style:=[fsBold];
  fModOperatorAttri.Foreground := $00000050;

  fStringAttri := TSynHighlighterAttributes.Create('string', 'string');
  AddAttribute(fStringAttri);
  fStringAttri.Foreground := clNavy;

  fNumberAttri := TSynHighlighterAttributes.Create('number', 'number');
  AddAttribute(fNumberAttri);
  fNumberAttri.Foreground := clBlue;

  fIdentifierAttri := TSynHighlighterAttributes.Create('ident', 'ident');
  AddAttribute(fIdentifierAttri);

  fCommentAttri := TSynHighlighterAttributes.Create('comment', 'comment');
  AddAttribute(fCommentAttri);
  fCommentAttri.Style:=[fsItalic];
  fCommentAttri.Foreground := clGreen;

  fOperators := TStringList.Create;
  fOperators.Delimiter:=' ';
  fOperators.DelimitedText:='_ AND ASSEMBLE BLOCK BREAK CASE COND CONST '+
  'CONTINUE ELSE ELT ERROR EXECUTE-FILE FOR FINALLY FUNCTION GOTO IF IF-NIL '+
  'INSET KEY MACRO '+
  'MACRO-SYMBOL NIL OR OTHERWISE PACKAGE PROCEDURE QUOTE RETURN T THEN TRY USE '+
  'VALUE VAR WHEN WHILE WITH';
  fOperators.CaseSensitive:=false;
  fOperators.Sorted := true;

  fModOperators := TStringList.Create;
  fModOperators.Delimiter:=' ';
  fModOperators.DelimitedText:='APPEND DEBUG DEFAULT DELETE INSERT LET POP PUSH SET';
  fModOperators.CaseSensitive:=false;
  fModOperators.Sorted := true;

  fInString := 0;

  // Ensure the HL reacts to changes in the attributes. Do this once, if all attributes are created
  SetAttributesOnChange(@DefHighlightChange);
end;

destructor TSynLisya.Destroy;
begin
    fOperators.Free;
    fModOperators.Free;
    inherited Destroy;
end;

procedure TSynLisya.SetOperatorAttri(AValue: TSynHighlighterAttributes);
begin
    fOperatorAttri.Assign(AValue);
end;

procedure TSynLisya.SetModOperatorAttri(AValue: TSynHighlighterAttributes);
begin
    fModOperatorAttri.Assign(AValue);
end;

procedure TSynLisya.SetStringAttri(AValue: TSynHighlighterAttributes);
begin
    fStringAttri.Assign(AValue);
end;

procedure TSynLisya.SetNumberAttri(AValue: TSynHighlighterAttributes);
begin
    fNumberAttri.Assign(AValue);
end;

procedure TSynLisya.SetIdentifierAttri(AValue: TSynHighlighterAttributes);
begin
  fIdentifierAttri.Assign(AValue);
end;

procedure TSynLisya.SetCommentAttri(AValue: TSynHighlighterAttributes);
begin
  fCommentAttri.Assign(AValue);
end;


procedure TSynLisya.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TSynLisya.Next;
var
  l: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  l := length(FLineText);
  If FTokenPos > l then
    // At line end
    exit
  else
  if (FLineText[FTokenEnd]='"') or ((FTokenEnd=1) and (fInString=1))  then begin
    if FLineText[FTokenEnd]='"' then Inc(fTokenEnd);
    while true do begin
        if fTokenEnd>l then begin fInString := 1; break; end;
        if FLineText[FTokenEnd]='"' then begin fInString:=0; break; end;
        if FLineText[FTokenEnd]='\' then Inc(FTokenEnd);
        Inc(FTokenEnd)
    end;
    Inc(fTokenEnd);
    if fTokenEnd>(l+1) then fTokenEnd := l+1;
  end
  else
  if FLineText[FTokenEnd] in [#9, ' '] then
    // At Space? Find end of spaces
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in [#0..#32]) do inc (FTokenEnd)
  else
  if FLineText[FTokenEnd]='(' then begin
    Inc(FTokenEnd);
  end
  else
  if FLineText[FTokenEnd]=')' then begin
    Inc(FTokenEnd);
  end
  else
  if FLineText[FTokenEnd] in [';'] then FTokenEnd := l+1
  else
    // At None-Space? Find end of None-spaces
    while (FTokenEnd <= l) and not(FLineText[FTokenEnd] in [#9, ' ','(',')',';']) do inc (FTokenEnd)
end;

function TSynLisya.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure TSynLisya.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynLisya.GetTokenAttribute: TSynHighlighterAttributes;
var n: integer;
begin
  // Match the text, specified by FTokenPos and FTokenEnd

  if (FLineText[FTokenPos]='"') or (FLineText[FTokenEnd-1]='"') or (fInString=1) then
    result := StringAttri
  else

  if FLineText[FTokenPos] in [';'] then
    result := CommentAttri
  else

  if fOperators.Find(copy(FLineText, FTokenPos, FTokenEnd - FTokenPos), n) then
    result := OperatorAttri
  else

  if fModOperators.Find(copy(FLineText, FTokenPos, FTokenEnd - FTokenPos), n) then
    result := ModOperatorAttri
  else

  if sp_float(FLineText[FTokenPos..FTokenEnd-1]) or sp_integer(FLineText[FTokenPos..FTokenEnd-1]) then
    Result := NumberAttri
  else

  Result := IdentifierAttri;
end;

function TSynLisya.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynLisya.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynLisya.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynLisya.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  // Map Attribute into a unique number
  a := GetTokenAttribute;
  Result := 0;
  if a = fOperatorAttri then Result := 1;
  if a = fModOperatorAttri then Result := 2;
  if a = fStringAttri then Result := 3;
  if a = fNumberAttri then Result := 4;
  if a = fIdentifierAttri then Result := 5;
  if a = fCommentAttri then Result := 6;
end;

procedure TSynLisya.SetRange(Value: Pointer);
begin
  // Set the current range (for current line)
  // The value is provided from an internal storage, where it was kept since the last scan
  // This is the and value of the previous line, which is used as start for the new line
  fInString := PtrInt(Value);
end;

procedure TSynLisya.ResetRange;
begin
  fInString := 0;
end;

function TSynLisya.GetRange: Pointer;
begin
  // Get a storable copy of the cuurent (working) range
  Result := Pointer(PtrInt(fInString));
end;

end.

