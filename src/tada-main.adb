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
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: unknown command '" & Command_Name & "'");
      Text_IO.New_Line (Text_IO.Standard_Error);
      Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada help' for usage.");
   end Print_Unknown_Command;

   procedure Print_Not_In_Project_Root is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: could not find `tada.toml` in current directory");
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
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: could not find executable `" & Exec_Name & "` in PATH");
   end Print_Exec_Not_Found;

   function Exec_On_Path (Exec_Name : String) return Boolean is
      use type GNAT.Strings.String_Access;

      Path : OS.String_Access := OS.Locate_Exec_On_Path (Exec_Name);
      Result : constant Boolean := Path /= null;
   begin
      OS.Free (Path);

      return Result;
   end Exec_On_Path;

   function Execute_Build (Project : String; Profile : String) return Boolean is
      GPRBuild_Path : OS.String_Access := OS.Locate_Exec_On_Path ("gprbuild");
      Args : OS.Argument_List (1 .. 4) :=
        [new String'("-P"),
          new String'(Project & ".gpr"),
          new String'("-XBUILD_PROFILE=" & Profile),
          new String'("-p")];
      Result : Boolean;
   begin
      OS.Spawn (GPRBuild_Path.all, Args, Result);

      OS.Free (GPRBuild_Path);
      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   end Execute_Build;

   procedure Print_Invalid_Profile is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: invalid build profile");
   end Print_Invalid_Profile;

   function Is_Valid_Profile return Boolean is
      No_Profile_Flag : constant Boolean := Argument_Count = 1;
      Valid_Profile_Flag : constant Boolean := Argument_Count = 3 and then
                                               Command_Line.Argument (2) = "--profile" and then
                                               (Command_Line.Argument (3) = "debug" or else
                                                Command_Line.Argument (3) = "release");
   begin
      return No_Profile_Flag or else Valid_Profile_Flag;
   end Is_Valid_Profile;

   function Read_Profile return String is
      No_Profile_Flag : constant Boolean := Argument_Count = 1;
   begin
      if No_Profile_Flag then
         return "debug";
      end if;

      if Is_Valid_Profile then
         return Command_Line.Argument (3);
      else
         raise Program_Error with "unreachable";
      end if;
   end Read_Profile;

   function Read_Project_Name return String is
      File : Text_IO.File_Type;
      Line : String (1 .. 256);
      Last : Natural;
      Prefix : constant String := "name = """;
   begin
      Text_IO.Open (File, Text_IO.In_File, "tada.toml");
      Text_IO.Get_Line (File, Line, Last);
      Text_IO.Close (File);

      if Last > Prefix'Length + 1
         and then Line (1 .. Prefix'Length) = Prefix
         and then Line (Last) = '"'
      then
         return Line (Prefix'Length + 1 .. Last - 1);
      else
         raise Constraint_Error
         with "invalid tada.toml format";
      end if;
   end Read_Project_Name;
begin
   if Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   declare
      Command_Name : constant String := Command_Line.Argument (1);
   begin
      if Command_Name = "build" then
         if not In_Project_Root then
            Print_Not_In_Project_Root;
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
         end if;

         if not Is_Valid_Profile then
            Print_Invalid_Profile;
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
         end if;

         if not Exec_On_Path ("gprbuild") then
            Print_Exec_Not_Found ("gprbuild");
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
         end if;

         if not Exec_On_Path ("gnatmake") then
            Print_Exec_Not_Found ("gnatmake");
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
         end if;

         declare
            Project_Name : constant String := Read_Project_Name;
            Build_Profile : constant String := Read_Profile;
         begin
            if not Execute_Build (Project_Name, Build_Profile) then
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;
         end;
      elsif Command_Name = "clean" then
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
