with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

with Tada.Environments;
with Tada.Manifests;
with Tada.Package_Cache;
with Tada.Package_Indexes;
with Tada.Packages;
with Tada.Packages.Containers;
with Tada.Runners;
with Tada.Templates;

package body Tada.Commands is
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

   function Parse (Result : Opts.Result) return Command is
      Kind : Command_Kind;
   begin
      begin
         Kind := Command_Kind'Value (Result.Cmd);
      exception
         when Constraint_Error =>
            raise Parse_Error with "unknown command '" & Result.Cmd & "'";
      end;

      case Kind is
         when Build =>
            begin
               return (Kind => Build,
                       Build_Profile => Profile_Kind'Value (Result.Arg ("profile", "debug")));
            exception
               when Constraint_Error =>
                  raise Parse_Error with "invalid profile '" & Result.Arg ("profile", "debug") & "'";
            end;
         when Cache =>
            return (Kind => Cache,
                    Force => Result.Has_Flag ("force"));
         when Clean =>
            return (Kind => Clean);
         when Config =>
            return (Kind => Config);
         when Doc =>
            return (Kind => Doc);
         when Init =>
            declare
               Package_Name : constant String := Characters.Handling.To_Lower (Result.Arg ("name", ""));
               Package_Type_Arg : constant String := Result.Arg ("type", "exe");
               Package_Type : Package_Kind;
            begin
               if not Packages.Is_Valid_Name (Package_Name) then
                  raise Parse_Error with "invalid package name '" & Result.Arg ("name", "") & "'";
               end if;

               if Package_Type_Arg = "exe" then
                  Package_Type := Exe;
               elsif Package_Type_Arg = "lib" then
                  Package_Type := Lib;
               else
                  raise Parse_Error with "invalid package type '" & Package_Type_Arg & "'";
               end if;

               return (Kind => Init,
                       Package_Name => String_Holders.To_Holder (Package_Name),
                       Package_Type => Package_Type);
            end;
         when Install =>
            return (Kind => Install);
         when Run =>
            begin
               return (Kind => Run,
                       Run_Profile => Profile_Kind'Value (Result.Arg ("profile", "debug")),
                       Args => Result.Passthrough_Args);
            exception
               when Constraint_Error =>
                  raise Parse_Error with "invalid profile '" & Result.Arg ("profile", "debug") & "'";
            end;
         when Test =>
            begin
               return (Kind => Test,
                       Test_Profile => Profile_Kind'Value (Result.Arg ("profile", "debug")),
                       Seed => String_Holders.To_Holder (Result.Arg ("seed", "")));
            exception
               when Constraint_Error =>
                  raise Parse_Error with "invalid profile '" & Result.Arg ("profile", "debug") & "'";
            end;
         when Version =>
            return (Kind => Version);
      end case;
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

   function Run_Target (Name : String; Arguments : Opts.Argument_List)
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

   function Target_Bin_Path (Profile : String; Name : String) return String is
   begin
      return Directories.Compose
        (Containing_Directory =>
           Directories.Compose
             (Directories.Compose ("target", Profile), "bin"),
         Name => Name) & Environments.Get_Exe_Suffix;
   end Target_Bin_Path;

   procedure Enqueue_Deps (Deps_Queue : in out Package_Info_Queues.List;
                           Package_Manifest : Manifests.Manifest;
                           Section_Name : String)
   is
   begin
      if Package_Manifest.Sections.Contains (Section_Name) then
         for C in Package_Manifest.Sections (Section_Name).Iterate loop
            declare
               Name : constant String := String_Maps.Key (C);
               Version : constant String := String_Maps.Element (C);
            begin
               Deps_Queue.Append (Packages.Create (Name, Version));
            end;
         end loop;
      end if;
   end Enqueue_Deps;

   procedure Generate_Deps (Tada_Manifest : Manifests.Manifest; Include_Dev_Deps : Boolean) is
      use Packages;
      use Templates;

      Deps_Queue : Package_Info_Queues.List := Package_Info_Queues.Empty_List;
      Missing_Deps : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
      Visited_Deps : Package_Info_Maps.Map := Package_Info_Maps.Empty_Map;
   begin
      Enqueue_Deps (Deps_Queue, Tada_Manifest, "dependencies");

      if Include_Dev_Deps then
         Enqueue_Deps (Deps_Queue, Tada_Manifest, "dev-dependencies");
      end if;

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
                     Dep_Manifest : constant Manifests.Manifest := Manifests.Read (Package_Cache.Manifest_Path (Dep));
                  begin
                     Enqueue_Deps (Deps_Queue, Dep_Manifest, "dependencies");
                  end;
               end if;
            end if;
         end;
      end loop;

      if not Missing_Deps.Is_Empty then
         declare
            use Ada.Strings.Unbounded;

            Error_Message_Builder : Unbounded_String := To_Unbounded_String ("missing dependencies:" & Characters.Latin_1.LF);
         begin
            for C in Missing_Deps.Iterate loop
               declare
                  Dep : constant Package_Info := Package_Info_Vectors.Element (C);
               begin
                  Append (Error_Message_Builder, "    - " & Dep.Name & " " & Dep.Version & Characters.Latin_1.LF);
               end;
            end loop;
            Append (Error_Message_Builder, "install missing packages with 'tada install'");

            raise Execute_Error with To_String (Error_Message_Builder);
         end;
      end if;

      for C in Visited_Deps.Iterate loop
         declare
            Dep : constant Package_Info := Package_Info_Maps.Element (C);
            Dep_Manifest : constant Manifests.Manifest := Manifests.Read (Package_Cache.Manifest_Path (Dep));
            Deps_To_Write : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
         begin
            if Dep_Manifest.Sections.Contains ("dependencies") then
               for D in Dep_Manifest.Sections ("dependencies").Iterate loop
                  declare
                     Name : constant String := String_Maps.Key (D);
                  begin
                     Deps_To_Write.Append (Visited_Deps (Name));
                  end;
               end loop;
            end if;

            Emit (Package_Cache.GPR_Deps_Path (Dep), Write_GPR_Deps'Access, Dep.Name, Deps_To_Write);
         end;
      end loop;

      declare
         Tada_Package : constant Package_Info := Packages.Create
           (Tada_Manifest.Sections ("package") ("name"),
            Tada_Manifest.Sections ("package") ("version"));
      begin
         declare
            Deps_To_Write : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
         begin
            if Tada_Manifest.Sections.Contains ("dependencies") then
               for C in Tada_Manifest.Sections ("dependencies").Iterate loop
                  declare
                     Name : constant String := String_Maps.Key (C);
                  begin
                     Deps_To_Write.Append (Visited_Deps (Name));
                  end;
               end loop;
            end if;

            Emit (Tada_Package.GPR_Deps_Name, Write_GPR_Deps'Access, Tada_Package.Name, Deps_To_Write);
         end;

         if Include_Dev_Deps then
            declare
               Deps_To_Write : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector;
            begin
               if Tada_Manifest.Sections.Contains ("dev-dependencies") then
                  for C in Tada_Manifest.Sections ("dev-dependencies").Iterate loop
                     declare
                        Name : constant String := String_Maps.Key (C);
                     begin
                        Deps_To_Write.Append (Visited_Deps (Name));
                     end;
                  end loop;
               end if;

               Emit (Tada_Package.GPR_Tests_Deps_Name, Write_GPR_Deps'Access, Tada_Package.Name & "_tests", Deps_To_Write);
            end;
         end if;
      end;
   end Generate_Deps;

   procedure Execute_Build (Cmd : Command) is
      Tada_Manifest : constant Manifests.Manifest := Manifests.Read (Packages.Manifest_Name);
      Package_Name : constant String := Tada_Manifest.Sections ("package") ("name");
   begin
      Generate_Deps (Tada_Manifest, Include_Dev_Deps => False);

      if not Runners.Run_GPRBuild (Package_Name, Image (Cmd.Build_Profile)) then
         raise Execute_Error with "build failed";
      end if;
   end Execute_Build;

   procedure Execute_Clean is
   begin
      if Directories.Exists ("target") then
         Text_IO.Put_Line ("Removing target/");
         Directories.Delete_Tree ("target");
      end if;
   end Execute_Clean;

   procedure Execute_Config is
      Env : constant Environments.Environment := Environments.Init;
   begin
      Text_IO.Put_Line ("Toolchain:");
      Text_IO.Put_Line ("    gnat: " & Env.GNAT_Path & " (" & Env.Config_Source & ")");
      Text_IO.Put_Line ("    gprbuild: " & Env.GPRBuild_Path & " (" & Env.Config_Source & ")");
   end Execute_Config;

   procedure Execute_Cache (Cmd : Command) is
      Tada_Manifest : constant Manifests.Manifest := Manifests.Read (Packages.Manifest_Name);
      Tada_Package : constant Packages.Package_Info := Packages.Create
        (Tada_Manifest.Sections ("package") ("name"),
         Tada_Manifest.Sections ("package") ("version"));
   begin
      if Cmd.Force and then
         Package_Cache.Is_Cached (Tada_Package)
      then
         Directories.Delete_Tree (Package_Cache.Package_Path (Tada_Package));
      end if;

      Package_Cache.Cache_Package (Directories.Current_Directory);
   end Execute_Cache;

   procedure Execute_Doc is
      Package_Name : constant String := Manifests.Read (Packages.Manifest_Name).Sections ("package") ("name");
   begin
      if not Runners.Run_GNATdoc (Package_Name) then
         raise Execute_Error with "doc failed";
      end if;
   end Execute_Doc;

   procedure Execute_Init (Cmd : Command) is
      use Directories;
      use Templates;

      New_Package : constant Packages.Package_Info := Packages.Create (Cmd.Package_Name.Element, "0.1.0-dev");
      Root : constant String := Full_Name (New_Package.Name);
   begin
      Text_IO.Put_Line ("Creating new package (" & Image (Cmd.Package_Type) & ")");

      if Exists (Root) then
         raise Execute_Error with "package '" & New_Package.Name & "' exists. Aborting.";
      end if;

      Create_Directory (Root);
      Create_Directory (Compose (Root, "src"));
      Create_Directory (Compose (Root, "tests"));

      Emit (Compose (Root, "README.md"), Write_Readme'Access, New_Package.Name);
      Emit (Compose (Root, Packages.Manifest_Name), Write_Manifest'Access, New_Package.Name);
      Emit (Compose (Root, ".gitignore"), Write_Gitignore'Access, New_Package.Name);
      Emit (Compose (Root, ".gitattributes"), Write_Gitattributes'Access);
      Emit (Compose (Root, New_Package.GPR_Name), Write_GPR_Main'Access, New_Package.Name, Cmd.Package_Type);
      Emit (Compose (Root, New_Package.GPR_Config_Name), Write_GPR_Config'Access, New_Package.Name);
      Emit (Compose (Root, New_Package.GPR_Deps_Name), Write_GPR_Deps'Access, New_Package.Name, Package_Info_Vectors.Empty_Vector);
      Emit (Compose (Root, New_Package.GPR_Tests_Deps_Name), Write_GPR_Deps'Access, New_Package.Name & "_tests", Package_Info_Vectors.Empty_Vector);
      Emit (Compose (Root, New_Package.GPR_Tests_Name), Write_GPR_Tests'Access, New_Package.Name);
      Emit (Compose (Compose (Root, "tests"), "tests_main.adb"), Write_Tests_Runner'Access, New_Package.Name);
      Emit (Compose (Compose (Root, "tests"), New_Package.Name &  "_tests.ads"), Write_Tests_Spec'Access, New_Package.Name);
      Emit (Compose (Compose (Root, "tests"), New_Package.Name &  "_tests.adb"), Write_Tests_Body'Access, New_Package.Name);
      Emit (Compose (Compose (Root, "src"), New_Package.Name &  ".ads"), Write_Root_Package_Spec'Access, New_Package.Name, Cmd.Package_Type);

      case Cmd.Package_Type is
         when Exe =>
            Emit (Compose (Compose (Root, "src"), New_Package.Name &  "-main.ads"), Write_Main_Spec'Access, New_Package.Name);
            Emit (Compose (Compose (Root, "src"), New_Package.Name &  "-main.adb"), Write_Main_Body'Access, New_Package.Name);
         when Lib =>
            Emit (Compose (Compose (Root, "src"), New_Package.Name &  ".adb"), Write_Root_Package_Body'Access, New_Package.Name);
      end case;
   exception
      when E : others =>
         Delete_Tree (Root);

         raise Execute_Error with Exceptions.Exception_Message (E);
   end Execute_Init;

   procedure Execute_Install is
      Index : constant Package_Indexes.Package_Index := Package_Indexes.Read (Package_Index_Url);
      Tada_Manifest : constant Manifests.Manifest := Manifests.Read (Packages.Manifest_Name);
      Deps_Queue : Package_Info_Queues.List := Package_Info_Queues.Empty_List;
      Visited_Deps : Package_Info_Maps.Map := Package_Info_Maps.Empty_Map;
   begin
      Enqueue_Deps (Deps_Queue, Tada_Manifest, "dependencies");
      Enqueue_Deps (Deps_Queue, Tada_Manifest, "dev-dependencies");

      while not Deps_Queue.Is_Empty loop
         declare
            Dep : constant Packages.Package_Info := Deps_Queue.First_Element;
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
                  declare
                     Package_Entry : constant Package_Indexes.Package_Index_Entry := Package_Indexes.Find (Index, Dep);
                  begin
                     Package_Indexes.Download (Package_Entry);
                     Package_Indexes.Verify_Checksum (Package_Entry);
                     Package_Indexes.Extract (Package_Entry);
                     Package_Cache.Cache_Package (Package_Indexes.Package_Tmp_Path (Package_Entry));
                  end;
               end if;

               declare
                  Dep_Manifest : constant Manifests.Manifest := Manifests.Read (Package_Cache.Manifest_Path (Dep));
               begin
                  Enqueue_Deps (Deps_Queue, Dep_Manifest, "dependencies");
               end;
            end if;
         end;
      end loop;

      Package_Indexes.Cleanup;
   end Execute_Install;

   procedure Execute_Run (Cmd : Command) is
      Package_Name : constant String := Manifests.Read (Packages.Manifest_Name).Sections ("package") ("name");
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
      Tada_Manifest : constant Manifests.Manifest := Manifests.Read (Packages.Manifest_Name);
      Package_Name : constant String := Tada_Manifest.Sections ("package") ("name");
      Exec_Name : constant String := Target_Bin_Path (Image (Cmd.Test_Profile), "tests");
   begin
      Generate_Deps (Tada_Manifest, Include_Dev_Deps => True);

      if not Runners.Run_GPRBuild (Package_Name & "_tests", Image (Cmd.Test_Profile)) then
         raise Execute_Error with "test build failed";
      end if;

      if not Run_Target (Exec_Name, [Cmd.Seed.Element])
      then
         raise Execute_Error with "tests failed";
      end if;
   end Execute_Test;

   procedure Execute (Cmd : Command) is
   begin
      case Cmd.Kind is
         when Config | Init | Version =>
            null;
         when Build | Cache | Clean | Doc | Install | Run | Test =>
            if not In_Package_Root then
               raise Execute_Error with "could not find '" & Packages.Manifest_Name & "' in current directory";
            end if;
      end case;

      case Cmd.Kind is
         when Doc =>
            if not Exec_On_Path ("gnatdoc") then
               raise Execute_Error with "could not find executable 'gnatdoc' in PATH";
            end if;
         when Install =>
            if not Exec_On_Path ("curl") then
               raise Execute_Error with "could not find executable 'curl' in PATH";
            end if;

            if not Exec_On_Path ("tar") then
               raise Execute_Error with "could not find executable 'tar' in PATH";
            end if;
         when others =>
            null;
      end case;

      case Cmd.Kind is
         when Build => Execute_Build (Cmd);
         when Cache => Execute_Cache (Cmd);
         when Clean => Execute_Clean;
         when Config => Execute_Config;
         when Doc => Execute_Doc;
         when Init => Execute_Init (Cmd);
         when Install => Execute_Install;
         when Run => Execute_Run (Cmd);
         when Test => Execute_Test (Cmd);
         when Version => Execute_Version;
      end case;
   end Execute;
end Tada.Commands;
