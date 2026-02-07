with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

procedure Tada.Main is
   use Ada;

   package OS renames GNAT.OS_Lib;

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

   procedure Print_Not_In_Project_Root is
   begin
      Text_IO.Put_Line ("Error: could not find `tada.toml` in current directory");
   end Print_Not_In_Project_Root;

   function In_Project_Root return Boolean is
      use type Ada.Directories.File_Kind;

      Project_File : constant String := "tada.toml";
   begin
      return Directories.Exists (Project_File) and then
             Directories.Kind (Project_File) = Directories.Ordinary_File;
   end In_Project_Root;

   procedure Print_Exec_Not_Found (Exec_Name : String) is
   begin
      Text_IO.Put_Line ("Error: could not find executable `" & Exec_Name & "` in PATH");
   end Print_Exec_Not_Found;

   function Exec_On_Path (Exec_Name : String) return Boolean is
      use type GNAT.Strings.String_Access;

      Path : OS.String_Access := OS.Locate_Exec_On_Path (Exec_Name);
      Result : constant Boolean := Path /= null;
   begin
      OS.Free (Path);
      return Result;
   end Exec_On_Path;
begin
   if Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   declare
      Command_Name : constant String := Command_Line.Argument (1);
   begin
      if Command_Name = "clean" then
         if not In_Project_Root then
            Print_Not_In_Project_Root;
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
         end if;

         if Directories.Exists ("target") then
            Text_IO.Put_Line ("Removing target/");
            Directories.Delete_Tree ("target");
         end if;
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
