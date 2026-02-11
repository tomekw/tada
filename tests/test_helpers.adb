with AUnit.Assertions; use AUnit.Assertions;

package body Test_Helpers is

   use Tada.Commands;
   use Command_Results;

   function Parse_Ok
     (Arguments : CL.Argument_List.Vector;
      Label     : String)
      return Tada.Commands.Command
   is
      Cmd : constant Command_Results.Result :=
        Parse (Arguments);
   begin
      Assert (Cmd.Status = Ok,
              Label & ": expected Ok");
      return Cmd.Value;
   end Parse_Ok;

   procedure Assert_Parses_Error
     (Arguments : CL.Argument_List.Vector;
      Label     : String)
   is
      Cmd : constant Command_Results.Result :=
        Parse (Arguments);
   begin
      Assert (Cmd.Status = Error,
              Label & ": expected Error");
   end Assert_Parses_Error;

end Test_Helpers;
