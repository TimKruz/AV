program AV;

{$R *.dres}

uses
  Forms,
  SplashScreenUnit in 'SplashScreenUnit.pas' {SplashScreen},
  MainWindowUnit in 'MainWindowUnit.pas' {MainForm},
  HelpWindowUnit in 'HelpWindowUnit.pas' {HelpForm},
  ModuleOperationsUnit in 'ModuleOperationsUnit.pas',
  ModulesIOThreadsUnit in 'ModulesIOThreadsUnit.pas',
  SystemControlMessagesUnit in 'SystemControlMessagesUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSplashScreen, SplashScreen);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;

end.
