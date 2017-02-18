program GR32_Objects_Demo;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  DrawObjInspect in 'DrawObjInspect.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
