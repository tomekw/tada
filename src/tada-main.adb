with Ada.Command_Line;
with Ada.Text_IO;

procedure Tada.Main is
   use Ada;

   Argument_Count : constant Natural := Command_Line.Argument_Count;

   procedure Print_Usage is
   begin
      Text_IO.Put_Line ("Usage: tada [command] [options]");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Commands:");
      Text_IO.Put_Line ("    init <name> [--exe|--lib]           Create a new project");
      Text_IO.Put_Line ("    build [--profile <p>]               Compile the project");
      Text_IO.Put_Line ("    run [--profile <p>] [-- <args>...]  Build and run the executable");
      Text_IO.Put_Line ("    test [--profile <p>]                Build and run the test suite");
      Text_IO.Put_Line ("    clean                               Remove build artifacts");
      Text_IO.Put_Line ("    help                                Show this message");
   end Print_Usage;

   procedure Print_Unknown_Command (Command_Name : String) is
   begin
      Text_IO.Put_Line ("tada: unknown command '" & Command_Name & "'");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Run 'tada help' for usage.");
   end Print_Unknown_Command;
begin
   if Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   declare
      Command_Name : constant String := Command_Line.Argument (1);
   begin
      if Command_Name = "clean" then
         null;
      elsif Command_Name = "help" then
         Print_Usage;
         return;
      else
         Print_Unknown_Command (Command_Name);
         Command_Line.Set_Exit_Status (Command_Line.Failure);
         return;
      end if;
   end;
end Tada.Main;
