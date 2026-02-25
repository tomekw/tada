with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

with Tada.Config;
with Tada.Package_Cache;
with Tada.Packages;
with Tada.Packages.Containers;
with Tada.Templates;

package body Tada.Commands is
   use Ada;
   use Packages.Containers;

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
         if not Packages.Is_Valid_Name (Package_Name) then
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
   begin
      return Directories.Exists (Packages.Manifest_Name);
   end In_Package_Root;

   function Exec_On_Path (Exec_Name : String) return Boolean is
      use type GNAT.Strings.String_Access;

      Path : OS.String_Access := OS.Locate_Exec_On_Path (Exec_Name);
      Result : constant Boolean := Path /= null;
   begin
      OS.Free (Path);

      return Result;
   end Exec_On_Path;

   function Run_GPRBuild (Project : String; Profile : String) return Boolean is
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
   end Run_GPRBuild;

   function Run_Target (Name : String; Arguments : CL_Arguments.Argument_List.Vector)
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
   end Run_Target;

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

   procedure Generate_Deps is
      use Packages;

      Tada_Manifest : constant Config.Manifest := Config.Read (Packages.Manifest_Name);

      Deps_Queue : Package_Info_Queues.List := Package_Info_Queues.Empty_List;
      Missing_Deps : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
      Visited_Deps : Package_Info_Maps.Map := Package_Info_Maps.Empty_Map;

      procedure Enqueue_Deps (Package_Manifest : Config.Manifest) is
      begin
         if Package_Manifest.Sections.Contains ("dependencies") then
            for C in Package_Manifest.Sections ("dependencies").Iterate loop
               declare
                  Name : constant String := Config.String_Maps.Key (C);
                  Version : constant String := Config.String_Maps.Element (C);
               begin
                  Deps_Queue.Append (Packages.Create (Name, Version));
               end;
            end loop;
         end if;
      end Enqueue_Deps;
   begin
      Enqueue_Deps (Tada_Manifest);

      while not Deps_Queue.Is_Empty loop
         declare
            Dep : constant Package_Info := Deps_Queue.First_Element;
         begin
            Deps_Queue.Delete_First;

            if Visited_Deps.Contains (Dep.Name) then
               if Visited_Deps (Dep.Name).Version /= Dep.Version then
                  raise Execute_Error with "version conflict for '" & Dep.Name & "': " &
                                           Visited_Deps (Dep.Name).Version & " vs " & Dep.Version;
               end if;
            else
               Visited_Deps.Insert (Dep.Name, Dep);

               if not Package_Cache.Is_Cached (Dep) then
                  Missing_Deps.Append (Dep);
               else
                  declare
                     Dep_Manifest : constant Config.Manifest := Config.Read (Package_Cache.Manifest_Path (Dep));
                  begin
                     Enqueue_Deps (Dep_Manifest);
                  end;
               end if;
            end if;
         end;
      end loop;

      if not Missing_Deps.Is_Empty then
         declare
            Error_Message_Builder : Unbounded_String := To_Unbounded_String ("missing dependencies:" & Characters.Latin_1.LF);
         begin
            for C in Missing_Deps.Iterate loop
               declare
                  Dep : constant Package_Info := Package_Info_Vectors.Element (C);
               begin
                  Append (Error_Message_Builder, "    - " & Dep.Name & " " & Dep.Version & Characters.Latin_1.LF);
               end;
            end loop;
            Append (Error_Message_Builder, "cache each package manually with 'tada cache'");

            raise Execute_Error with To_String (Error_Message_Builder);
         end;
      end if;

      for C in Visited_Deps.Iterate loop
         declare
            Dep : constant Package_Info := Package_Info_Maps.Element (C);
            Dep_Manifest : constant Config.Manifest := Config.Read (Package_Cache.Manifest_Path (Dep));
            Deps_To_Write : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
            F : Text_IO.File_Type;
         begin
            if Dep_Manifest.Sections.Contains ("dependencies") then
               for D in Dep_Manifest.Sections ("dependencies").Iterate loop
                  declare
                     Name : constant String := Config.String_Maps.Key (D);
                  begin
                     Deps_To_Write.Append (Visited_Deps (Name));
                  end;
               end loop;
            end if;

            Text_IO.Create (F, Text_IO.Out_File, Package_Cache.GPR_Deps_Path (Dep));
            Templates.Write_GPR_Deps (F, Dep.Name, Deps_To_Write);
            Text_IO.Close (F);
         end;
      end loop;

      declare
         Tada_Package : constant Package_Info := Packages.Create
           (Tada_Manifest.Sections ("package") ("name"),
            Tada_Manifest.Sections ("package") ("version"));

         Deps_To_Write : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
         F : Text_IO.File_Type;
      begin
         if Tada_Manifest.Sections.Contains ("dependencies") then
            for C in Tada_Manifest.Sections ("dependencies").Iterate loop
               declare
                  Name : constant String := Config.String_Maps.Key (C);
               begin
                  Deps_To_Write.Append (Visited_Deps (Name));
               end;
            end loop;
         end if;

         Text_IO.Create (F, Text_IO.Out_File, Tada_Package.GPR_Deps_Name);
         Templates.Write_GPR_Deps (F, Tada_Package.Name, Deps_To_Write);
         Text_IO.Close (F);
      end;
   end Generate_Deps;

   procedure Execute_Build (Cmd : Command) is
      Package_Name : constant String := Config.Read (Packages.Manifest_Name).Sections ("package") ("name");
   begin
      Generate_Deps;

      if not Run_GPRBuild (Package_Name, Image (Cmd.Build_Profile)) then
         raise Execute_Error with "build failed";
      end if;
   end Execute_Build;

   procedure Copy_Tree (Source_Path : String; Target_Path : String) is
      use Directories;

      Tree_Search : Search_Type;
      Search_Item : Directory_Entry_Type;
      Search_Filter : constant Filter_Type := [Ordinary_File => True,
                                               Directory => True,
                                               Special_File => False];
   begin
      Create_Directory (Target_Path);
      Start_Search (Tree_Search, Source_Path, "", Search_Filter);

      while More_Entries (Tree_Search) loop
         Get_Next_Entry (Tree_Search, Search_Item);

         declare
            Entry_Name : constant String := Simple_Name (Search_Item);
         begin
            if Kind (Search_Item) = Ordinary_File then
               Copy_File (Full_Name (Search_Item), Compose (Target_Path, Entry_Name));
            elsif Kind (Search_Item) = Directory and then
                  Entry_Name /= "." and then
                  Entry_Name /= ".."
            then
               Copy_Tree (Compose (Source_Path, Entry_Name), Compose (Target_Path, Entry_Name));
            end if;
         end;
      end loop;
      Text_IO.Put_Line ("Copied '" & Source_Path & "' to '" & Target_Path & "'");

      End_Search (Tree_Search);
   end Copy_Tree;

   procedure Execute_Cache is
      use Directories;

      Tada_Manifest : constant Config.Manifest := Config.Read (Packages.Manifest_Name);
      Tada_Package : constant Packages.Package_Info :=
        Packages.Create (Tada_Manifest.Sections ("package") ("name"),
                         Tada_Manifest.Sections ("package") ("version"));
      Tada_Package_Cache_Path : constant String := Package_Cache.Package_Path (Tada_Package);
   begin
      if Package_Cache.Is_Cached (Tada_Package) then
         raise Execute_Error with "package '" & Tada_Package.Name & " " & Tada_Package.Version &
                                  "' already cached at '" & Tada_Package_Cache_Path & "'";
      end if;

      begin
         begin
            Create_Path (Tada_Package_Cache_Path);
         exception
            when Use_Error =>
               raise Execute_Error with "unable to create '" & Tada_Package_Cache_Path & "'";
         end;

         Copy_File (Packages.Manifest_Name, Package_Cache.Manifest_Path (Tada_Package));
         Text_IO.Put_Line ("Copied '" & Packages.Manifest_Name & "' to '" & Tada_Package_Cache_Path & "'");

         Copy_File (Tada_Package.GPR_Name, Package_Cache.GPR_Path (Tada_Package));
         Text_IO.Put_Line ("Copied '" & Tada_Package.GPR_Name & "' to '" & Tada_Package_Cache_Path & "'");

         Copy_File (Tada_Package.GPR_Config_Name, Package_Cache.GPR_Config_Path (Tada_Package));
         Text_IO.Put_Line ("Copied '" & Tada_Package.GPR_Config_Name & "' to '" & Tada_Package_Cache_Path & "'");

         Copy_Tree ("src", Compose (Tada_Package_Cache_Path, "src"));

         Text_IO.New_Line;
         Text_IO.Put_Line ("Cached package '" & Tada_Package.Name & " " & Tada_Package.Version &
                           "' at '" & Tada_Package_Cache_Path & "'");
      exception
         when E : others =>
            Delete_Tree (Tada_Package_Cache_Path);

            raise Execute_Error with Exceptions.Exception_Message (E);
      end;
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

      New_Package : constant Packages.Package_Info := Packages.Create (To_String (Cmd.Package_Name), "0.1.0");
      Root : constant String := Full_Name (New_Package.Name);
   begin
      Text_IO.Put_Line ("Creating new package (" & Image (Cmd.Package_Type) & ")");

      if Exists (Root) then
         raise Execute_Error with "package '" & New_Package.Name & "' exists. Aborting.";
      end if;

      Create_Directory (Root);
      Create_Directory (Compose (Root, "src"));
      Create_Directory (Compose (Root, "tests"));

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, "README.md"));
         Templates.Write_Readme (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, Packages.Manifest_Name));
         Templates.Write_Manifest (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, ".gitignore"));
         Templates.Write_Gitignore (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package.GPR_Name));
         Templates.Write_GPR_Main (F, New_Package.Name, Cmd.Package_Type);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package.GPR_Config_Name));
         Templates.Write_GPR_Config (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package.GPR_Deps_Name));
         Templates.Write_GPR_Deps (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Root, New_Package.GPR_Tests_Name));
         Templates.Write_GPR_Tests (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), "run_tests.adb"));
         Templates.Write_Test_Runner (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package.Name &  "_suite.ads"));
         Templates.Write_Test_Suite_Spec (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package.Name &  "_suite.adb"));
         Templates.Write_Test_Suite_Body (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package.Name &  "_test.ads"));
         Templates.Write_Test_Spec (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "tests"), New_Package.Name &  "_test.adb"));
         Templates.Write_Test_Body (F, New_Package.Name);
         Text_IO.Close (F);
      end;

      declare
         F : Text_IO.File_Type;
      begin
         Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package.Name & ".ads"));
         Templates.Write_Root_Package_Spec (F, New_Package.Name, Cmd.Package_Type);
         Text_IO.Close (F);
      end;

      case Cmd.Package_Type is
         when Exe =>
            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package.Name & "-main.ads"));
               Templates.Write_Main_Spec (F, New_Package.Name);
               Text_IO.Close (F);
            end;

            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package.Name & "-main.adb"));
               Templates.Write_Main_Body (F, New_Package.Name);
               Text_IO.Close (F);
            end;
         when Lib =>
            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, Compose (Compose (Root, "src"), New_Package.Name & ".adb"));
               Templates.Write_Root_Package_Body (F, New_Package.Name);
               Text_IO.Close (F);
            end;
      end case;
   exception
      when E : others =>
         Delete_Tree (Root);

         raise Execute_Error with Exceptions.Exception_Message (E);
   end Execute_Init;

   procedure Execute_Run (Cmd : Command) is
      Package_Name : constant String := Config.Read (Packages.Manifest_Name).Sections ("package") ("name");
      Build_Cmd : constant Command := (Kind => Build, Build_Profile => Cmd.Run_Profile);
   begin
      Execute_Build (Build_Cmd);

      declare
         Exec_Name : constant String := Target_Bin_Path (Image (Cmd.Run_Profile), Package_Name);
      begin
         if not Run_Target (Exec_Name, Cmd.Args) then
            raise Execute_Error with "run failed";
         end if;
      end;
   end Execute_Run;

   procedure Execute_Version is
   begin
      Text_IO.Put_Line (Tada.Version);
   end Execute_Version;

   procedure Execute_Test (Cmd : Command) is
      Package_Name : constant String := Config.Read (Packages.Manifest_Name).Sections ("package") ("name");
      Exec_Name : constant String := Target_Bin_Path (Image (Cmd.Test_Profile), "run_tests");
   begin
      Generate_Deps;

      if not Run_GPRBuild (Package_Name & "_tests", Image (Cmd.Test_Profile)) then
         raise Execute_Error with "test build failed";
      end if;

      if not Run_Target (Exec_Name, CL_Arguments.Argument_List.Empty_Vector)
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
               raise Execute_Error with "could not find '" & Packages.Manifest_Name & "' in current directory";
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
