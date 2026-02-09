with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tada.Commands;
with Tada.CL_Arguments;

procedure Tada.Main is
   use Ada;
   use Ada.Strings.Unbounded;

   use Commands;

   Arguments : constant CL_Arguments.Argument_List.Vector :=
     CL_Arguments.Consume;

   Cmd : constant Command_Results.Result := Commands.Parse (Arguments);
begin
   case Cmd.Status is
      when Command_Results.Ok =>
         Commands.Execute (Cmd.Value);
      when Command_Results.Error =>
         Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & To_String (Cmd.Message));
         Text_IO.New_Line (Text_IO.Standard_Error);
         Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada help' for usage.");
         Command_Line.Set_Exit_Status (Command_Line.Failure);
   end case;
end Tada.Main;
