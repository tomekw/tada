with Ada.Characters.Handling;

package body Tada.Templates is

   use Ada.Text_IO;

   function Mixed_Case (Name : String) return String is
      Result  : String := Name;
      Cap_Next : Boolean := True;
   begin
      for I in Result'Range loop
         if Result (I) = '_' then
            Cap_Next := True;
         elsif Cap_Next then
            Result (I) :=
              Ada.Characters.Handling.To_Upper (Result (I));
            Cap_Next := False;
         end if;
      end loop;
      return Result;
   end Mixed_Case;

   --------------------
   -- Write_Readme --
   --------------------

   procedure Write_Readme
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
   begin
      Put_Line (File, "# " & Mixed_Case (Name));
   end Write_Readme;

   ----------------------
   -- Write_Manifest --
   ----------------------

   procedure Write_Manifest
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
   begin
      Put_Line (File, "name = """ & Name & """");
   end Write_Manifest;

   ----------------------
   -- Write_Gitignore --
   ----------------------

   procedure Write_Gitignore
     (File : Ada.Text_IO.File_Type)
   is
   begin
      Put_Line (File, "target/");
   end Write_Gitignore;

   ------------------------
   -- Write_GPR_Config --
   ------------------------

   procedure Write_GPR_Config
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "abstract project " & MC & "_Config is");
      Put_Line (File, "");
      Put_Line (File,
        "   type Build_Profile_Kind is " &
        "(""debug"", ""release"");");
      Put_Line (File,
        "   Build_Profile : Build_Profile_Kind :=");
      Put_Line (File,
        "     external (""BUILD_PROFILE"", ""debug"");");
      Put_Line (File, "");
      Put_Line (File,
        "   Common_Switches := " &
        "(""-gnat2022"", ""-gnatwa"", ""-gnata"");");
      Put_Line (File,
        "   Style_Switches := " &
        "(""-gnatyaAbCdefhiklnOprStux"");");
      Put_Line (File, "");
      Put_Line (File,
        "   Debug_Switches := Common_Switches &");
      Put_Line (File,
        "     (""-g"",       " &
        "--  Debug info");
      Put_Line (File,
        "      ""-O0"",      " &
        "--  No optimization");
      Put_Line (File,
        "      ""-gnateE"",  " &
        "--  Extra exception info");
      Put_Line (File,
        "      ""-gnatVa""); " &
        "--  All validity checks");
      Put_Line (File, "");
      Put_Line (File,
        "   Release_Switches := Common_Switches &");
      Put_Line (File,
        "     (""-O2"",          " &
        "--  Optimization");
      Put_Line (File,
        "      ""-gnatn"",       " &
        "--  Inlining");
      Put_Line (File,
        "      ""-fdata-sections"",");
      Put_Line (File,
        "      ""-ffunction-sections"");");
      Put_Line (File, "");
      Put_Line (File,
        "   Ada_Switches := ();");
      Put_Line (File,
        "   case Build_Profile is");
      Put_Line (File,
        "      when ""debug""   => " &
        "Ada_Switches := Debug_Switches;");
      Put_Line (File,
        "      when ""release"" => " &
        "Ada_Switches := Release_Switches;");
      Put_Line (File,
        "   end case;");
      Put_Line (File, "");
      Put_Line (File,
        "   Linker_Switches := ();");
      Put_Line (File,
        "   case Build_Profile is");
      Put_Line (File,
        "      when ""debug""   => " &
        "Linker_Switches := ();");
      Put_Line (File,
        "      when ""release"" => " &
        "Linker_Switches := ();");
      Put_Line (File,
        "   end case;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Config;");
   end Write_GPR_Config;

   ----------------------
   -- Write_GPR_Main --
   ----------------------

   procedure Write_GPR_Main
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "with """ & Name & "_config.gpr"";");
      Put_Line (File, "");
      Put_Line (File,
        "project " & MC & " is");
      Put_Line (File, "");
      Put_Line (File,
        "   for Source_Dirs use (""src"");");
      Put_Line (File,
        "   for Object_Dir use ""target/"" & " &
        MC & "_Config.Build_Profile & ""/obj"";");

      case Kind is
         when Commands.Exe =>
            Put_Line (File,
              "   for Exec_Dir use ""target/"" & " &
              MC & "_Config.Build_Profile & ""/bin"";");
            Put_Line (File,
              "   for Main use (""" &
              Name & "-main.adb"");");
            Put_Line (File, "");
            Put_Line (File,
              "   package Builder is");
            Put_Line (File,
              "      for Executable (""" &
              Name & "-main.adb"") use """ &
              Name & """;");
            Put_Line (File,
              "   end Builder;");
            Put_Line (File, "");
            Put_Line (File,
              "   package Compiler is");
            Put_Line (File,
              "      for Default_Switches (""Ada"") use");
            Put_Line (File,
              "        " & MC &
              "_Config.Ada_Switches &");
            Put_Line (File,
              "        " & MC &
              "_Config.Style_Switches;");
            Put_Line (File,
              "   end Compiler;");
            Put_Line (File, "");
            Put_Line (File,
              "   package Binder is");
            Put_Line (File,
              "      for Default_Switches " &
              "(""Ada"") use (""-Es"");");
            Put_Line (File,
              "   end Binder;");
            Put_Line (File, "");
            Put_Line (File,
              "   package Linker is");
            Put_Line (File,
              "      for Default_Switches (""Ada"") use");
            Put_Line (File,
              "        " & MC &
              "_Config.Linker_Switches;");
            Put_Line (File,
              "   end Linker;");

         when Commands.Lib =>
            Put_Line (File,
              "   for Library_Name use """ & MC & """;");
            Put_Line (File,
              "   for Library_Dir use ""lib"";");
            Put_Line (File,
              "   for Library_Kind use ""static"";");
            Put_Line (File, "");
            Put_Line (File,
              "   package Compiler is");
            Put_Line (File,
              "      for Default_Switches (""Ada"") use");
            Put_Line (File,
              "        " & MC &
              "_Config.Ada_Switches &");
            Put_Line (File,
              "        " & MC &
              "_Config.Style_Switches;");
            Put_Line (File,
              "   end Compiler;");
      end case;

      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & ";");
   end Write_GPR_Main;

   -----------------------
   -- Write_GPR_Tests --
   -----------------------

   procedure Write_GPR_Tests
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "with """ & Name & "_config.gpr"";");
      Put_Line (File,
        "with """ & Name & ".gpr"";");
      Put_Line (File,
        "with ""aunit.gpr"";");
      Put_Line (File, "");
      Put_Line (File,
        "project " & MC & "_Tests is");
      Put_Line (File, "");
      Put_Line (File,
        "   for Source_Dirs use (""tests"");");
      Put_Line (File,
        "   for Object_Dir use ""target/"" & " &
        MC & "_Config.Build_Profile & ""/obj-tests"";");
      Put_Line (File,
        "   for Exec_Dir use ""target/"" & " &
        MC & "_Config.Build_Profile & ""/bin"";");
      Put_Line (File,
        "   for Main use (""run_tests.adb"");");
      Put_Line (File, "");
      Put_Line (File,
        "   package Compiler is");
      Put_Line (File,
        "      for Default_Switches (""Ada"") use");
      Put_Line (File,
        "        " & MC &
        "_Config.Ada_Switches &");
      Put_Line (File,
        "        " & MC &
        "_Config.Style_Switches;");
      Put_Line (File,
        "   end Compiler;");
      Put_Line (File, "");
      Put_Line (File,
        "   package Binder is");
      Put_Line (File,
        "      for Default_Switches " &
        "(""Ada"") use (""-Es"");");
      Put_Line (File,
        "   end Binder;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Tests;");
   end Write_GPR_Tests;

   -------------------------
   -- Write_Test_Runner --
   -------------------------

   procedure Write_Test_Runner
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File, "with AUnit.Run;");
      Put_Line (File, "with AUnit.Reporter.Text;");
      Put_Line (File, "with " & MC & "_Suite;");
      Put_Line (File, "");
      Put_Line (File, "procedure Run_Tests is");
      Put_Line (File, "");
      Put_Line (File,
        "   procedure Runner is new AUnit.Run.Test_Runner");
      Put_Line (File,
        "     (" & MC & "_Suite.Suite);");
      Put_Line (File, "");
      Put_Line (File,
        "   Reporter : AUnit.Reporter.Text.Text_Reporter;");
      Put_Line (File, "");
      Put_Line (File, "begin");
      Put_Line (File, "   Runner (Reporter);");
      Put_Line (File, "end Run_Tests;");
   end Write_Test_Runner;

   -----------------------------
   -- Write_Test_Suite_Spec --
   -----------------------------

   procedure Write_Test_Suite_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File, "with AUnit.Test_Suites;");
      Put_Line (File, "");
      Put_Line (File,
        "package " & MC & "_Suite is");
      Put_Line (File, "");
      Put_Line (File,
        "   function Suite return " &
        "AUnit.Test_Suites.Access_Test_Suite;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Suite;");
   end Write_Test_Suite_Spec;

   -----------------------------
   -- Write_Test_Suite_Body --
   -----------------------------

   procedure Write_Test_Suite_Body
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "with " & MC & "_Test;");
      Put_Line (File, "");
      Put_Line (File,
        "package body " & MC & "_Suite is");
      Put_Line (File, "");
      Put_Line (File,
        "   Result : aliased " &
        "AUnit.Test_Suites.Test_Suite;");
      Put_Line (File,
        "   Test_1 : aliased " &
        MC & "_Test.Test_Case;");
      Put_Line (File, "");
      Put_Line (File,
        "   function Suite return " &
        "AUnit.Test_Suites.Access_Test_Suite");
      Put_Line (File, "   is");
      Put_Line (File, "   begin");
      Put_Line (File,
        "      AUnit.Test_Suites.Add_Test");
      Put_Line (File,
        "        (Result'Access, Test_1'Access);");
      Put_Line (File,
        "      return Result'Access;");
      Put_Line (File, "   end Suite;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Suite;");
   end Write_Test_Suite_Body;

   -----------------------
   -- Write_Test_Spec --
   -----------------------

   procedure Write_Test_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File, "with AUnit.Test_Cases;");
      Put_Line (File, "");
      Put_Line (File,
        "package " & MC & "_Test is");
      Put_Line (File, "");
      Put_Line (File,
        "   type Test_Case is new " &
        "AUnit.Test_Cases.Test_Case");
      Put_Line (File,
        "     with null record;");
      Put_Line (File, "");
      Put_Line (File, "   overriding");
      Put_Line (File, "   function Name");
      Put_Line (File,
        "     (Unused_T : Test_Case)");
      Put_Line (File,
        "      return AUnit.Message_String;");
      Put_Line (File, "");
      Put_Line (File, "   overriding");
      Put_Line (File,
        "   procedure Register_Tests " &
        "(T : in out Test_Case);");
      Put_Line (File, "");
      Put_Line (File, "private");
      Put_Line (File, "");
      Put_Line (File,
        "   procedure Test_True");
      Put_Line (File,
        "     (Unused_T : in out " &
        "AUnit.Test_Cases.Test_Case'Class);");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Test;");
   end Write_Test_Spec;

   -----------------------
   -- Write_Test_Body --
   -----------------------

   procedure Write_Test_Body
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "with AUnit.Assertions; use AUnit.Assertions;");
      Put_Line (File,
        "with AUnit.Test_Cases; use AUnit.Test_Cases;");
      Put_Line (File, "");
      Put_Line (File,
        "package body " & MC & "_Test is");
      Put_Line (File, "");
      Put_Line (File, "   overriding");
      Put_Line (File, "   function Name");
      Put_Line (File,
        "     (Unused_T : Test_Case)");
      Put_Line (File,
        "      return AUnit.Message_String");
      Put_Line (File, "   is");
      Put_Line (File, "   begin");
      Put_Line (File,
        "      return AUnit.Format (""" &
        MC & """);");
      Put_Line (File, "   end Name;");
      Put_Line (File, "");
      Put_Line (File, "   overriding");
      Put_Line (File,
        "   procedure Register_Tests " &
        "(T : in out Test_Case) is");
      Put_Line (File, "   begin");
      Put_Line (File,
        "      Registration.Register_Routine");
      Put_Line (File,
        "        (T, Test_True'Access,");
      Put_Line (File,
        "         ""True is True"");");
      Put_Line (File,
        "   end Register_Tests;");
      Put_Line (File, "");
      Put_Line (File, "   procedure Test_True");
      Put_Line (File,
        "     (Unused_T : in out " &
        "AUnit.Test_Cases.Test_Case'Class)");
      Put_Line (File, "   is");
      Put_Line (File, "   begin");
      Put_Line (File,
        "      Assert (True, ""True is True"");");
      Put_Line (File, "   end Test_True;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & "_Test;");
   end Write_Test_Body;

   ------------------------------
   -- Write_Root_Package_Spec --
   ------------------------------

   procedure Write_Root_Package_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      case Kind is
         when Commands.Exe =>
            Put_Line (File,
              "package " & MC & " is");
            Put_Line (File,
              "   pragma Pure;");
            Put_Line (File,
              "end " & MC & ";");

         when Commands.Lib =>
            Put_Line (File,
              "package " & MC & " is");
            Put_Line (File, "");
            Put_Line (File,
              "   procedure Hello;");
            Put_Line (File, "");
            Put_Line (File,
              "end " & MC & ";");
      end case;
   end Write_Root_Package_Spec;

   ----------------------
   -- Write_Main_Spec --
   ----------------------

   procedure Write_Main_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File,
        "procedure " & MC & ".Main;");
   end Write_Main_Spec;

   ----------------------
   -- Write_Main_Body --
   ----------------------

   procedure Write_Main_Body
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File, "with Ada.Text_IO;");
      Put_Line (File, "");
      Put_Line (File,
        "procedure " & MC & ".Main is");
      Put_Line (File, "begin");
      Put_Line (File,
        "   Ada.Text_IO.Put_Line " &
        "(""Hello from " & MC & "!"");");
      Put_Line (File,
        "end " & MC & ".Main;");
   end Write_Main_Body;

   ------------------------------
   -- Write_Root_Package_Body --
   ------------------------------

   procedure Write_Root_Package_Body
     (File : Ada.Text_IO.File_Type;
      Name : String)
   is
      MC : constant String := Mixed_Case (Name);
   begin
      Put_Line (File, "with Ada.Text_IO;");
      Put_Line (File, "");
      Put_Line (File,
        "package body " & MC & " is");
      Put_Line (File, "");
      Put_Line (File,
        "   procedure Hello is");
      Put_Line (File, "   begin");
      Put_Line (File,
        "      Ada.Text_IO.Put_Line " &
        "(""Hello from " & MC & "!"");");
      Put_Line (File, "   end Hello;");
      Put_Line (File, "");
      Put_Line (File,
        "end " & MC & ";");
   end Write_Root_Package_Body;

end Tada.Templates;
