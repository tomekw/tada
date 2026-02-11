with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

with Tada.Templates;

package body Tada.Commands is
   use Ada;

   package OS renames GNAT.OS_Lib;

   package Args_Results is new Results (CL_Arguments.Argument_List.Vector);
   package Profile_Results is new Results (Profile_Kind);
   package Project_Name_Results is new Results (Unbounded_String);
   package Project_Type_Results is new Results (Project_Kind);

   package String_Vectors is new Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   Reserved_Words : constant String_Vectors.Vector :=
     ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and", "array", "at",
       "begin", "body",
       "case", "constant",
       "declare", "delay", "delta", "digits", "do",
       "else", "elsif", "end", "entry", "exception",
       "exit",
       "for", "function",
       "generic", "goto",
       "if", "in", "interface", "is",
       "limited", "loop",
       "mod",
       "new", "not", "null",
       "of", "or", "others", "out", "overriding",
       "package", "parallel", "pragma", "private",
       "procedure", "protected",
       "raise", "range", "record", "rem", "renames",
       "requeue", "return", "reverse",
       "select", "separate", "some", "subtype",
       "synchronized",
       "tagged", "task", "terminate", "then", "type",
       "until", "use",
       "when", "while", "with",
       "xor"];

   function Image (P : Profile_Kind) return String is
     (Characters.Handling.To_Lower (Profile_Kind'Image (P)));

   function Image (P : Project_Kind) return String is
   begin
      case P is
         when Exe =>
            return "executable";
         when Lib =>
            return "library";
      end case;
   end Image;

   function Parse_Args (Arguments : CL_Arguments.Argument_List.Vector)
     return Args_Results.Result
   is
      use Args_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);

      Args : CL_Arguments.Argument_List.Vector;

      function Find_Args_Separator return Natural is
      begin
         --  Arguments (1) is a command name
         for I in 2 .. Arguments_Count loop
            if Arguments (I) = "--" then
               return I;
            end if;
         end loop;
         return 0;
      end Find_Args_Separator;

      Args_Separator : constant Natural := Find_Args_Separator;
   begin
      if Args_Separator = Arguments_Count then
         return (Status => Error,
                 Message => To_Unbounded_String ("missing args"));
      elsif Args_Separator /= 0 then
         for I in Args_Separator + 1 .. Arguments_Count loop
            Args.Append (Arguments (I));
         end loop;
      end if;

      return (Status => Ok,
              Value => Args);
   end Parse_Args;

   function Is_Reserved_Word (Name : String) return Boolean is
      use Ada.Characters.Handling;

      Lower_Name : constant String := To_Lower (Name);
   begin
      for Word of Reserved_Words loop
         if Lower_Name = Word then
            return True;
         end if;
      end loop;

      return False;
   end Is_Reserved_Word;

   function Valid_Project_Name (Name : String) return Boolean is
      use Characters.Handling;

      Underscore : constant Character := '_';
   begin
      if Name'Length = 0 or else
         not Is_Letter (Name (Name'First)) or else
         not Is_Alphanumeric (Name (Name'Last)) or else
         Is_Reserved_Word (Name)
      then
         return False;
      else
         for I in Name'First .. Name'Last - 1 loop
            if not (Is_Alphanumeric (Name (I)) or else
                    Name (I) = Underscore)
            then
               return False;
            end if;

            if Name (I) = Underscore and then
               Name (I + 1) = Underscore
            then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Valid_Project_Name;

   function Parse_Project_Name (Arguments : CL_Arguments.Argument_List.Vector)
     return Project_Name_Results.Result
   is
      use Project_Name_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 1 then
         return (Status => Error,
                 Message => To_Unbounded_String ("missing project name"));
      else
         declare
            Project_Name : constant String := Characters.Handling.To_Lower (Arguments (2));
         begin
            if Valid_Project_Name (Project_Name) then
               return (Status => Ok,
                       Value => To_Unbounded_String (Project_Name));
            else
               return (Status => Error,
                       Message => To_Unbounded_String ("invalid project name '" & Arguments (2) & "'"));
            end if;
         end;
      end if;
   end Parse_Project_Name;

   function Parse_Project_Type (Arguments : CL_Arguments.Argument_List.Vector)
     return Project_Type_Results.Result
   is
      use Project_Type_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count < 3 then
         return (Status => Ok,
                 Value => Exe);
      elsif Arguments (3) = "--exe"
      then
         return (Status => Ok,
                 Value => Exe);
      elsif Arguments (3) = "--lib" then
         return (Status => Ok,
                 Value => Lib);
      else
         return (Status => Error,
                 Message => To_Unbounded_String ("invalid project type '" & Arguments (3) & "'"));
      end if;
   end Parse_Project_Type;

   function Parse_Profile (Arguments : CL_Arguments.Argument_List.Vector)
     return Profile_Results.Result
   is
      use Profile_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count < 2 then
         return (Status => Ok,
                 Value => Debug);
      elsif Arguments (2) = "--" then
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
      use Args_Results;
      use Command_Results;
      use Profile_Results;
      use Project_Name_Results;
      use Project_Type_Results;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 0 then
         return (Status => Ok,
                 Value => (Kind => Help));
      end if;

      declare
         Kind : Command_Kind;
      begin
         begin
            Kind := Command_Kind'Value (Arguments (1));
         exception
            when Constraint_Error =>
               return (Status => Error,
                       Message => To_Unbounded_String ("unknown command '" & Arguments (1) & "'"));
         end;

         case Kind is
            when Help =>
               return (Status => Ok,
                       Value => (Kind => Help));
            when Clean =>
               return (Status => Ok,
                       Value => (Kind => Clean));
            when Init =>
               declare
                  Project_Name : constant Project_Name_Results.Result := Parse_Project_Name (Arguments);
                  Project_Type : constant Project_Type_Results.Result := Parse_Project_Type (Arguments);
               begin
                  if Project_Name.Status = Ok and then
                     Project_Type.Status = Ok
                  then
                     return (Status => Ok,
                             Value => (Kind => Init,
                                       Project_Name => Project_Name.Value,
                                       Project_Type => Project_Type.Value));
                  elsif Project_Name.Status = Error and then
                        Project_Type.Status = Error
                  then
                     return (Status => Error,
                             Message => Project_Name.Message & ", " & Project_Type.Message);
                  elsif Project_Name.Status = Error then
                     return (Status => Error,
                             Message => Project_Name.Message);
                  elsif Project_Type.Status = Error then
                     return (Status => Error,
                             Message => Project_Type.Message);
                  end if;

                  raise Program_Error with "unreachable";
               end;
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
            when Run =>
               declare
                  Profile : constant Profile_Results.Result := Parse_Profile (Arguments);
                  Args : constant Args_Results.Result := Parse_Args (Arguments);
               begin
                  if Profile.Status = Ok and then
                     Args.Status = Ok
                  then
                     return (Status => Ok,
                             Value => (Kind => Run,
                                       Run_Profile => Profile.Value,
                                       Args => Args.Value));
                  elsif Profile.Status = Error and then
                        Args.Status = Error
                  then
                     return (Status => Error,
                             Message => Profile.Message & ", " & Args.Message);
                  elsif Profile.Status = Error then
                     return (Status => Error,
                             Message => Profile.Message);
                  elsif Args.Status = Error then
                     return (Status => Error,
                             Message => Args.Message);
                  end if;

                  raise Program_Error with "unreachable";
               end;
         end case;
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

   function Execute_Target (Name : String; Arguments : CL_Arguments.Argument_List.Vector)
     return Boolean
   is
      Result : Boolean := False;

      Arguments_Count : constant Natural := Natural (Arguments.Length);

      Args : OS.Argument_List (1 .. Arguments_Count);
   begin
      for I in 1 .. Arguments_Count loop
         Args (I) := new String'(Arguments (I));
      end loop;

      OS.Spawn (Name, Args, Result);

      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   exception
      when others =>
         for Arg of Args loop
            OS.Free (Arg);
         end loop;

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

   procedure Print_Project_Name_Exists (Project_Name : String) is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: project name `" & Project_Name & "` exists. Aborting.");
   end Print_Project_Name_Exists;

   procedure Print_Something_Went_Wrong is
   begin
      Text_IO.Put_Line (Text_IO.Standard_Error, "Error: something went wrong. Aborting.");
   end Print_Something_Went_Wrong;

   procedure Execute (Cmd : Command) is
   begin
      case Cmd.Kind is
         when Help | Init =>
            null;
         when Build | Clean | Test | Run =>
            if not In_Project_Root then
               Print_Not_In_Project_Root;
               Command_Line.Set_Exit_Status (Command_Line.Failure);
               return;
            end if;
      end case;

      case Cmd.Kind is
         when Help | Clean | Init =>
            null;
         when Build | Test | Run =>
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
               Exe_Suffix : OS.String_Access := OS.Get_Target_Executable_Suffix;
               Exec_Name : constant String :=
                 Directories.Compose
                   (Containing_Directory =>
                      Directories.Compose
                        (Directories.Compose ("target", Image (Cmd.Profile)),
                         "bin"),
                    Name => "run_tests") & Exe_Suffix.all;
            begin
               OS.Free (Exe_Suffix);
               if not Execute_Build (Project_Name & "_tests", Image (Cmd.Profile)) then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;

               if not Execute_Target (Exec_Name, CL_Arguments.Argument_List.Empty_Vector)
               then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;
            end;
         when Run =>
            declare
               Project_Name : constant String := Read_Project_Name;
               Exe_Suffix : OS.String_Access := OS.Get_Target_Executable_Suffix;
               Exec_Name : constant String :=
                 Directories.Compose
                   (Containing_Directory =>
                      Directories.Compose
                        (Directories.Compose ("target", Image (Cmd.Run_Profile)),
                         "bin"),
                    Name => Project_Name) & Exe_Suffix.all;
            begin
               OS.Free (Exe_Suffix);
               if not Execute_Build (Project_Name, Image (Cmd.Run_Profile)) then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;

               if not Execute_Target (Exec_Name, Cmd.Args)
               then
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;
            end;
         when Init =>
            declare
               use Directories;

               New_Project_Name : constant String := To_String (Cmd.Project_Name);
               Root : constant String := Full_Name (New_Project_Name);
            begin
               Text_IO.Put_Line ("Creating new project (" & Image(Cmd.Project_Type) & ")");

               if Exists (Root) then
                  Print_Project_Name_Exists (New_Project_Name);
                  Command_Line.Set_Exit_Status (Command_Line.Failure);
                  return;
               end if;

               Create_Directory (Root);
               Create_Directory (Compose (Root, "src"));
               Create_Directory (Compose (Root, "tests"));

               Set_Directory (Root);
               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, "README.md");
                  Templates.Write_Readme (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, "tada.toml");
                  Templates.Write_Manifest (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, ".gitignore");
                  Templates.Write_Gitignore (F);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & "_config.gpr");
                  Templates.Write_GPR_Config (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & ".gpr");
                  Templates.Write_GPR_Main (F, New_Project_Name, Cmd.Project_Type);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & "_tests.gpr");
                  Templates.Write_GPR_Tests (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               Set_Directory (Compose (Root, "tests"));

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, "run_tests.adb");
                  Templates.Write_Test_Runner (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name &  "_suite.ads");
                  Templates.Write_Test_Suite_Spec (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name &  "_suite.adb");
                  Templates.Write_Test_Suite_Body (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name &  "_test.ads");
                  Templates.Write_Test_Spec (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name &  "_test.adb");
                  Templates.Write_Test_Body (F, New_Project_Name);
                  Text_IO.Close (F);
               end;

               Set_Directory (Compose (Root, "src"));

               declare
                  F : Text_IO.File_Type;
               begin
                  Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & ".ads");
                  Templates.Write_Root_Package_Spec (F, New_Project_Name, Cmd.Project_Type);
                  Text_IO.Close (F);
               end;

               case Cmd.Project_Type is
                  when Exe =>
                     declare
                        F : Text_IO.File_Type;
                     begin
                        Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & "-main.ads");
                        Templates.Write_Main_Spec (F, New_Project_Name);
                        Text_IO.Close (F);
                     end;

                     declare
                        F : Text_IO.File_Type;
                     begin
                        Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & "-main.adb");
                        Templates.Write_Main_Body (F, New_Project_Name);
                        Text_IO.Close (F);
                     end;
                  when Lib =>
                     declare
                        F : Text_IO.File_Type;
                     begin
                        Text_IO.Create (F, Text_IO.Out_File, New_Project_Name & ".adb");
                        Templates.Write_Root_Package_Body (F, New_Project_Name);
                        Text_IO.Close (F);
                     end;
               end case;
            exception
               when others =>
                  Print_Something_Went_Wrong;
                  Command_Line.Set_Exit_Status (Command_Line.Failure);

                  Set_Directory (Containing_Directory (Root));
                  Delete_Tree (Root);
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
