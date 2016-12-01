unit StatesPoolUnit;

interface

type
  TState = record
    Name: String;
    IsDeleted: Boolean;
    case ValueType: (VFlag, VString, VNull) of
      VFlag:
        (Flag: Boolean);
      VString:
        (Data: ShortString);
      VNull:
        ();
  end;

const
  NullState: TState = (Name: ''; IsDeleted: false; ValueType: VNull);

type

  TStatesPool = class
  private
    States: array of TState;

    procedure AddState(StateName: String; Value: TState);
    function GetState(StateName: String): TState;
    procedure SetState(StateName: String; Value: TState);
  public
    property State[StateName: String]: TState read GetState write SetState;
  end;

implementation

{ TStatesPool }

procedure TStatesPool.AddState(StateName: String; Value: TState);
var
  L: Integer;
begin
  L := Length(States);
  SetLength(States, L + 1);
  States[L] := Value;
  States[L].Name := StateName;
  States[L].IsDeleted := false;
end;

function TStatesPool.GetState(StateName: String): TState;
var
  S: TState;
begin
  for S in States do
    if S.Name = StateName then
      Exit(S);
  Exit(NullState);
end;

procedure TStatesPool.SetState(StateName: String; Value: TState);
var
  i: Integer;
begin
  for i := Low(States) to High(States) do
    if States[i].Name = StateName then
    begin
      States[i] := Value;
      Exit;
    end;
  AddState(StateName, Value);
end;

end.
