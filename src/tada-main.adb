with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with Tada.Commands;
with Tada.Config;
with Tada.CL_Arguments;

procedure Tada.Main is
   use Ada;

   Arguments : constant CL_Arguments.Argument_List.Vector := CL_Arguments.Consume;
begin
   Commands.Execute (Commands.Parse (Arguments));
exception
   when E : Config.Manifest_Error =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
   when E : Commands.Execute_Error =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
   when E : Commands.Parse_Error =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Text_IO.New_Line (Text_IO.Standard_Error);
      Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada help' for usage.");
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Tada.Main;
