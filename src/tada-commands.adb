with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

package body Tada.Commands is
   use Ada;

   package OS renames GNAT.OS_Lib;

   function From (Arguments : CL_Arguments.Argument_List.Vector) return Command
   is
      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 0 or else
         (Arguments_Count = 1 and then
          Arguments (1) = "help")
      then
         return (Kind => Help);
      elsif Arguments_Count = 1 and then
            Arguments (1) = "clean"
      then
         return (Kind => Clean);
      elsif Arguments_Count = 1 and then
            Arguments (1) = "build"
      then
         return (Kind => Build, Profile => Debug);
      elsif Arguments_Count = 3 and then
            Arguments (1) = "build" and then
            Arguments (2) = "--profile" and then
            (Arguments (3) = "debug" or else
             Arguments (3) = "release")
      then
         declare
            Build_Profile : constant Profile_Kind :=
              (if Arguments (3) = "release" then Release else Debug);
         begin
            return (Kind => Build, Profile => Build_Profile);
         end;
      else
         return (Kind => Invalid, Unknown_Name => To_Unbounded_String (Arguments (1)));
      end if;
   end From;

   function In_Project_Root return Boolean is
      use type Ada.Directories.File_Kind;

      Project_File : constant String := "tada.toml";
   begin
      return Directories.Exists (Project_File) and then
             Directories.Kind (Project_File) = Directories.Ordinary_File;
   end In_Project_Root;

   function Exec_On_Path (Exec_Name : String) return Boolean is
      use type GNAT.Strings.String_Access;

      Path : OS.String_Access := OS.Locate_Exec_On_Path (Exec_Name);
      Result : constant Boolean := Path /= null;
   begin
      OS.Free (Path);

      return Result;
   end Exec_On_Path;

   function Execute_Build (Project : String; Profile : String) return Boolean is
      use type OS.String_Access;

      GPRBuild_Path : OS.String_Access := OS.Locate_Exec_On_Path ("gprbuild");
      Args : OS.Argument_List (1 .. 4) :=
        [new String'("-P"),
          new String'(Project & ".gpr"),
          new String'("-XBUILD_PROFILE=" & Profile),
          new String'("-p")];
      Result : Boolean := False;
   begin
      if GPRBuild_Path /= null then
         OS.Spawn (GPRBuild_Path.all, Args, Result);
      end if;

      OS.Free (GPRBuild_Path);
      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   exception
      when others =>
         OS.Free (GPRBuild_Path);
         for Arg of Args loop
            OS.Free (Arg);
         end loop;

         return False;
   end Execute_Build;

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
         raise Constraint_Error with "invalid tada.toml format";
      end if;
   exception
      when others =>
         if Text_IO.Is_Open (File) then
            Text_IO.Close (File);
         end if;
         raise;
   end Read_Project_Name;

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

   procedure Print_Not_In_Project_Root is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: could not find `tada.toml` in current directory");
   end Print_Not_In_Project_Root;

   procedure Print_Exec_Not_Found (Exec_Name : String) is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: could not find executable `" & Exec_Name & "` in PATH");
   end Print_Exec_Not_Found;

   procedure Print_Unknown_Command (Command_Name : String) is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: unknown command '" & Command_Name & "'");
      Text_IO.New_Line (Text_IO.Standard_Error);
      Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada help' for usage.");
   end Print_Unknown_Command;

   procedure Execute (Cmd : Command) is
   begin
      case Cmd.Kind is
         when Build =>
            if not In_Project_Root then
               Print_Not_In_Project_Root;
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;

            if not Exec_On_Path ("gprbuild") then
               Print_Exec_Not_Found ("gprbuild");
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;

            declare
               Project_Name : constant String := Read_Project_Name;
               Build_Profile : constant String :=
                 (if Cmd.Profile = Release then "release" else "debug");
            begin
               if not Execute_Build (Project_Name, Build_Profile) then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;
            end;
         when Clean =>
            if not In_Project_Root then
               Print_Not_In_Project_Root;
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;

            if Directories.Exists ("target") then
               Text_IO.Put_Line ("Removing target/");
               Directories.Delete_Tree ("target");
            end if;
         when Help =>
            Print_Usage;
         when Invalid =>
            Print_Unknown_Command (To_String (Cmd.Unknown_Name));
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            return;
      end case;
   end Execute;
end Tada.Commands;
