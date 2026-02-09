with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

package body Tada.Commands is
   use Ada;
   use Ada.Strings.Unbounded;

   package OS renames GNAT.OS_Lib;

   package Profile_Results is new Results (Profile_Kind);

   function Image (P : Profile_Kind) return String is
     (Characters.Handling.To_Lower (Profile_Kind'Image (P)));

   function Parse_Profile (Arguments : CL_Arguments.Argument_List.Vector)
     return Profile_Results.Result
   is
      use Profile_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count < 2 then
         return (Status => Ok,
                 Value => Debug);
      elsif Arguments_Count = 2 and then
            Arguments (2) = "--profile"
      then
         return (Status => Error,
                 Message => To_Unbounded_String ("missing profile"));
      else
         if Arguments (2) /= "--profile" then
            return (Status => Error,
                    Message => To_Unbounded_String ("unexpected option '" & Arguments (2) & "'"));
         end if;

         begin
            return (Status => Ok,
                    Value => Profile_Kind'Value (Arguments (3)));
         exception
            when Constraint_Error =>
               return (Status => Error,
                       Message => To_Unbounded_String ("invalid profile '" & Arguments (3) & "'"));
         end;
      end if;
   end Parse_Profile;

   function Parse (Arguments : CL_Arguments.Argument_List.Vector)
     return Command_Results.Result
   is
      use Command_Results;
      use Profile_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 0 then
         return (Status => Ok,
                 Value => (Kind => Help));
      end if;

      declare
         Kind : Command_Kind;
      begin
         Kind := Command_Kind'Value (Arguments (1));

         case Kind is
            when Help =>
               return (Status => Ok,
                       Value => (Kind => Help));
            when Clean =>
               return (Status => Ok,
                       Value => (Kind => Clean));
            when Build =>
               declare
                  Profile : constant Profile_Results.Result := Parse_Profile (Arguments);
               begin
                  if Profile.Status = Ok then
                     return (Status => Ok,
                             Value => (Kind => Build,
                                       Profile => Profile.Value));
                  else
                     return (Status => Error,
                             Message => Profile.Message);
                  end if;
               end;
            when Test =>
               declare
                  Profile : constant Profile_Results.Result := Parse_Profile (Arguments);
               begin
                  if Profile.Status = Ok then
                     return (Status => Ok,
                             Value => (Kind => Test,
                                       Profile => Profile.Value));
                  else
                     return (Status => Error,
                             Message => Profile.Message);
                  end if;
               end;
         end case;
      exception
         when Constraint_Error =>
            return (Status => Error,
                    Message => To_Unbounded_String ("unknown command '" & Arguments (1) & "'"));
      end;
   end Parse;

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

   function Execute_Target (Name : String) return Boolean is
      Result : Boolean := False;
   begin
      OS.Spawn (Name, OS.Argument_List'(1 .. 0 => null), Result);

      return Result;
   exception
      when others =>
         return False;
   end Execute_Target;

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

   procedure Execute (Cmd : Command) is
   begin
      case Cmd.Kind is
         when Help =>
            null;
         when Build | Clean | Test =>
            if not In_Project_Root then
               Print_Not_In_Project_Root;
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;
      end case;

      case Cmd.Kind is
         when Help | Clean =>
            null;
         when Build | Test =>
            if not Exec_On_Path ("gprbuild") then
               Print_Exec_Not_Found ("gprbuild");
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;
      end case;

      case Cmd.Kind is
         when Build =>
            declare
               Project_Name : constant String := Read_Project_Name;
            begin
               if not Execute_Build (Project_Name, Image (Cmd.Profile)) then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;
            end;
         when Test =>
            declare
               Project_Name : constant String := Read_Project_Name;
            begin
               if not Execute_Build (Project_Name & "_tests", Image (Cmd.Profile)) then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;

               if not Execute_Target ("target/" & Image (Cmd.Profile) & "/bin/run_tests") then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;
            end;
         when Clean =>
            if Directories.Exists ("target") then
               Text_IO.Put_Line ("Removing target/");
               Directories.Delete_Tree ("target");
            end if;
         when Help =>
            Print_Usage;
      end case;
   end Execute;
end Tada.Commands;
