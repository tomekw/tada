with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

with Tada.Config;
with Tada.Templates;

package body Tada.Commands is
   use Ada;

   package OS renames GNAT.OS_Lib;

   function Image (P : Profile_Kind) return String is
   begin
      return Characters.Handling.To_Lower (Profile_Kind'Image (P));
   end Image;

   function Image (P : Package_Kind) return String is
   begin
      case P is
         when Exe =>
            return "executable";
         when Lib =>
            return "library";
      end case;
   end Image;

   function Parse_Args (Arguments : CL_Arguments.Argument_List.Vector)
     return CL_Arguments.Argument_List.Vector
   is
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
         raise Parse_Error with "missing args";
      elsif Args_Separator /= 0 then
         for I in Args_Separator + 1 .. Arguments_Count loop
            Args.Append (Arguments (I));
         end loop;
      end if;

      return Args;
   end Parse_Args;

   function Parse_Package_Name (Arguments : CL_Arguments.Argument_List.Vector)
     return String
   is
      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 1 then
         raise Parse_Error with "missing package name";
      end if;

      declare
         Package_Name : constant String := Characters.Handling.To_Lower (Arguments (2));
      begin
         if not Config.Valid_Package_Name (Package_Name) then
            raise Parse_Error with "invalid package name '" & Arguments (2) & "'";
         end if;

         return Package_Name;
      end;
   end Parse_Package_Name;

   function Parse_Package_Type (Arguments : CL_Arguments.Argument_List.Vector)
     return Package_Kind
   is
      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count < 3 then
         return Exe;
      elsif Arguments (3) = "--exe" then
         return Exe;
      elsif Arguments (3) = "--lib" then
         return Lib;
      else
         raise Parse_Error with "invalid package type '" & Arguments (3) & "'";
      end if;
   end Parse_Package_Type;

   function Parse_Profile (Arguments : CL_Arguments.Argument_List.Vector)
     return Profile_Kind
   is
      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count < 2 then
         return Debug;
      elsif Arguments (2) = "--" then
         return Debug;
      elsif Arguments_Count = 2 and then
            Arguments (2) = "--profile"
      then
         raise Parse_Error with "missing profile";
      else
         if Arguments (2) /= "--profile" then
            raise Parse_Error with "unexpected option '" & Arguments (2) & "'";
         end if;

         begin
            return Profile_Kind'Value (Arguments (3));
         exception
            when Constraint_Error =>
               raise Parse_Error with "invalid profile '" & Arguments (3) & "'";
         end;
      end if;
   end Parse_Profile;

   function Parse (Arguments : CL_Arguments.Argument_List.Vector) return Command is
      Arguments_Count : constant Natural := Natural (Arguments.Length);
   begin
      if Arguments_Count = 0 then
         return (Kind => Help);
      end if;

      declare
         Kind : Command_Kind;
      begin
         begin
            Kind := Command_Kind'Value (Arguments (1));
         exception
            when Constraint_Error =>
               raise Parse_Error with "unknown command '" & Arguments (1) & "'";
         end;

         case Kind is
            when Build =>
               return (Kind => Build,
                       Build_Profile => Parse_Profile (Arguments));
            when Cache =>
               return (Kind => Cache);
            when Clean =>
               return (Kind => Clean);
            when Help =>
               return (Kind => Help);
            when Init =>
               return (Kind => Init,
                       Package_Name => To_Unbounded_String (Parse_Package_Name (Arguments)),
                       Package_Type => Parse_Package_Type (Arguments));
            when Run =>
               return (Kind => Run,
                       Run_Profile => Parse_Profile (Arguments),
                       Args => Parse_Args (Arguments));
            when Test =>
               return (Kind => Test,
                       Test_Profile => Parse_Profile (Arguments));
            when Version =>
               return (Kind => Version);
         end case;
      end;
   end Parse;

   function In_Package_Root return Boolean is
      use type Ada.Directories.File_Kind;

      Package_File : constant String := "tada.toml";
   begin
      return Directories.Exists (Package_File) and then
             Directories.Kind (Package_File) = Directories.Ordinary_File;
   end In_Package_Root;

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

   function Get_Exe_Suffix return String is
      Suffix : OS.String_Access := OS.Get_Target_Executable_Suffix;
      Result : constant String := Suffix.all;
   begin
      OS.Free (Suffix);
      return Result;
   end Get_Exe_Suffix;

   function Target_Bin_Path (Profile : String; Name : String) return String is
   begin
      return Directories.Compose
        (Containing_Directory =>
           Directories.Compose
             (Directories.Compose ("target", Profile), "bin"),
         Name => Name) & Get_Exe_Suffix;
   end Target_Bin_Path;

   function Tada_Cache_Path return String is
      use Directories;

      Is_Windows : constant Boolean := OS.Directory_Separator = '\';
   begin
      if Is_Windows then
         return Compose (Compose (Environment_Variables.Value ("LOCALAPPDATA"), "tada"), "package");
      else
         return Compose (Compose (Compose (Environment_Variables.Value ("HOME"), ".cache"), "tada"), "package");
      end if;
   end Tada_Cache_Path;

   procedure Execute_Build (Cmd : Command) is
      Package_Name : constant String := Config.Read ("tada.toml").Sections ("package") ("name");
   begin
      if not Execute_Build (Package_Name, Image (Cmd.Build_Profile)) then
         raise Execute_Error with "build failed";
      end if;
   end Execute_Build;

   procedure Execute_Cache is
      use Directories;

      Tada_Manifest : constant Config.Manifest := Config.Read ("tada.toml");
      Package_Name : constant String := Tada_Manifest.Sections ("package") ("name");
      Package_Version : constant String := Tada_Manifest.Sections ("package") ("version");
      Package_Name_Cache_Path : constant String := Compose (Tada_Cache_Path, Package_Name);
      Package_Full_Cache_Path : constant String := Compose (Package_Name_Cache_Path, Package_Version);
   begin
      if Exists (Package_Full_Cache_Path) then
         raise Execute_Error with "package '" & Package_Name & "', version '" & Package_Version &
                                  "' already cached at '" & Package_Full_Cache_Path & "'";
      end if;

      begin
         Create_Path (Package_Full_Cache_Path);
      exception
         when Use_Error =>
            raise Execute_Error with "unable to create '" & Package_Full_Cache_Path & "'";
      end;

      Copy_File ("tada.toml", Compose (Package_Full_Cache_Path, "tada.toml"));
      Copy_File (Package_Name & ".gpr", Compose (Package_Full_Cache_Path, Package_Name & ".gpr"));
      Copy_File (Package_Name & "_config.gpr", Compose (Package_Full_Cache_Path, Package_Name & "_config.gpr"));
      --  TODO: Copy src/

      Text_IO.Put_Line ("Cached package '" & Package_Name & "', version '" & Package_Version &
                        "' at '" & Package_Full_Cache_Path & "'");
   exception
      when E : others =>
         Delete_Tree (Package_Name_Cache_Path);

         raise Execute_Error with Exceptions.Exception_Message (E);
   end Execute_Cache;

   procedure Execute_Clean is
   begin
      if Directories.Exists ("target") then
         Text_IO.Put_Line ("Removing target/");
         Directories.Delete_Tree ("target");
      end if;
   end Execute_Clean;

   procedure Execute_Help is
   begin
      Text_IO.Put_Line ("Usage: tada [command] [options]");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Commands:");
      Text_IO.Put_Line ("    init <name> [--exe|--lib]           Create a new package");
      Text_IO.Put_Line ("    build [--profile <p>]               Compile the package");
      Text_IO.Put_Line ("    run [--profile <p>] [-- <args>...]  Build and run the executable");
      Text_IO.Put_Line ("    test [--profile <p>]                Build and run the test suite");
      Text_IO.Put_Line ("    clean                               Remove build artifacts");
      Text_IO.Put_Line ("    cache                               Add package to the local cache");
      Text_IO.Put_Line ("    help                                Show this message");
      Text_IO.Put_Line ("    version                             Display version");
   end Execute_Help;

   procedure Execute_Init (Cmd : Command) is
      use Directories;

      New_Package_Name : constant String := To_String (Cmd.Package_Name);
      Root : constant String := Full_Name (New_Package_Name);
   begin
      Text_IO.Put_Line ("Creating new package (" & Image (Cmd.Package_Type) & ")");

      if Exists (Root) then
         raise Execute_Error with "package '" & New_Package_Name & "' exists. Aborting.";
      end if;

      Create_Directory (Root);
      Create_Directory (Compose (Root, "src"));
      Create_Directory (Compose (Root, "tests"));

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, "README.md"));
         Templates.Write_Readme (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, "tada.toml"));
         Templates.Write_Manifest (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, ".gitignore"));
         Templates.Write_Gitignore (F);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package_Name & "_config.gpr"));
         Templates.Write_GPR_Config (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, "deps.gpr"));
         Templates.Write_GPR_Deps (F);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package_Name & ".gpr"));
         Templates.Write_GPR_Main (F, New_Package_Name, Cmd.Package_Type);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package_Name & "_tests.gpr"));
         Templates.Write_GPR_Tests (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), "run_tests.adb"));
         Templates.Write_Test_Runner (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package_Name &  "_suite.ads"));
         Templates.Write_Test_Suite_Spec (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package_Name &  "_suite.adb"));
         Templates.Write_Test_Suite_Body (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package_Name &  "_test.ads"));
         Templates.Write_Test_Spec (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package_Name &  "_test.adb"));
         Templates.Write_Test_Body (F, New_Package_Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package_Name & ".ads"));
         Templates.Write_Root_Package_Spec (F, New_Package_Name, Cmd.Package_Type);
         Text_IO.Close (F);
      end;

      case Cmd.Package_Type is
         when Exe =>
            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package_Name & "-main.ads"));
               Templates.Write_Main_Spec (F, New_Package_Name);
               Text_IO.Close (F);
            end;

            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package_Name & "-main.adb"));
               Templates.Write_Main_Body (F, New_Package_Name);
               Text_IO.Close (F);
            end;
         when Lib =>
            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package_Name & ".adb"));
               Templates.Write_Root_Package_Body (F, New_Package_Name);
               Text_IO.Close (F);
            end;
      end case;
   exception
      when E : others =>
         Delete_Tree (Root);

         raise Execute_Error with Exceptions.Exception_Message (E);
   end Execute_Init;

   procedure Execute_Run (Cmd : Command) is
      Package_Name : constant String := Config.Read ("tada.toml").Sections ("package") ("name");
   begin
      if not Execute_Build (Package_Name, Image (Cmd.Run_Profile)) then
         raise Execute_Error with "run build failed";
      end if;

      declare
         Exec_Name : constant String := Target_Bin_Path (Image (Cmd.Run_Profile), Package_Name);
      begin
         if not Execute_Target (Exec_Name, Cmd.Args) then
            raise Execute_Error with "run failed";
         end if;
      end;
   end Execute_Run;

   procedure Execute_Version is
   begin
      Text_IO.Put_Line (Tada.Version);
   end Execute_Version;

   procedure Execute_Test (Cmd : Command) is
      Package_Name : constant String := Config.Read ("tada.toml").Sections ("package") ("name");
      Exec_Name : constant String := Target_Bin_Path (Image (Cmd.Test_Profile), "run_tests");
   begin
      if not Execute_Build (Package_Name & "_tests", Image (Cmd.Test_Profile)) then
         raise Execute_Error with "test build failed";
      end if;

      if not Execute_Target (Exec_Name, CL_Arguments.Argument_List.Empty_Vector)
      then
         raise Execute_Error with "tests failed";
      end if;
   end Execute_Test;

   procedure Execute (Cmd : Command) is
   begin
      case Cmd.Kind is
         when Help | Init | Version =>
            null;
         when Build | Cache | Clean | Run | Test =>
            if not In_Package_Root then
               raise Execute_Error with "could not find 'tada.toml' in current directory";
            end if;
      end case;

      case Cmd.Kind is
         when Cache | Clean | Help | Init | Version =>
            null;
         when Build | Run | Test =>
            if not Exec_On_Path ("gprbuild") then
               raise Execute_Error with "could not find executable 'gprbuild' in PATH";
            end if;
      end case;

      case Cmd.Kind is
         when Build => Execute_Build (Cmd);
         when Cache => Execute_Cache;
         when Clean => Execute_Clean;
         when Help => Execute_Help;
         when Init => Execute_Init (Cmd);
         when Run => Execute_Run (Cmd);
         when Test => Execute_Test (Cmd);
         when Version => Execute_Version;
      end case;
   end Execute;
end Tada.Commands;
