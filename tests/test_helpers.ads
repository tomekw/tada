with Tada.Commands;
with Tada.CL_Arguments;

package Test_Helpers is

   package CL renames Tada.CL_Arguments;

   function Parse_Ok
     (Arguments : CL.Argument_List.Vector;
      Label     : String)
      return Tada.Commands.Command;

   procedure Assert_Parses_Error
     (Arguments : CL.Argument_List.Vector;
      Label     : String);

end Test_Helpers;
