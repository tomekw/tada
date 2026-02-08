with Tada.Commands;
with Tada.CL_Arguments;

procedure Tada.Main is
   Arguments : constant CL_Arguments.Argument_List.Vector :=
     CL_Arguments.Consume;

   Cmd : constant Commands.Command := Commands.From (Arguments);
begin
   Commands.Execute (Cmd);
end Tada.Main;
