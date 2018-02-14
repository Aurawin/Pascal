program AuProcess;
uses
  {Start of Uses}
  {$i AuProcess.Uses.General.inc}
  {$i AuProcess.Uses.Servers.inc}
  {$i AuProcess.Uses.DBModules.inc}
  {$i coList.Uses.inc};
Type

  AuraProcess=class
  private
    {$i AuProcess.Private.Decs.inc}
  private
    {$i AuProcess.Private.Events.Decs.inc}
  private
    {$i AuProcess.Private.Methods.Decs.inc}
  private
    {$i AuProcess.Private.Start.Decs.inc}
  public
    {$i AuProcess.Public.Decs.inc}
  end;
  {$i AuProcess.Methods.inc}
  {$i AuProcess.Constructor.Run.inc}
  {$i AuProcess.Destructor.Destroy.inc}

  {$R *.res}
begin
  {$i AuProcess.Main.Code.inc}
end.

