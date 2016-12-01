program AV;

uses
  Forms,
  AVMainWindow in 'AVMainWindow.pas',
  NeuralNetwork in 'NeuralNetwork.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.HintColor:=$80FF80;
  Application.Run;
end.
