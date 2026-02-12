with Ada.Text_IO;

with Tada.Commands;

--  Project scaffolding templates for the init command.
--  Each procedure writes the contents of one generated file
--  to an already-open File_Type. Name is the lowercase
--  project name (a valid Ada identifier).
package Tada.Templates is

   --  Write README.md with the project name as heading.
   procedure Write_Readme
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tada.toml project manifest.
   procedure Write_Manifest
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write .gitignore
   procedure Write_Gitignore
     (File : Ada.Text_IO.File_Type);

   --  Write the abstract config GPR with build profiles
   --  and compiler switches.
   procedure Write_GPR_Config
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write the main GPR project file.
   --  Kind determines executable vs library layout.
   procedure Write_GPR_Main
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind);

   --  Write the test GPR project file.
   procedure Write_GPR_Tests
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tests/run_tests.adb AUnit test runner.
   procedure Write_Test_Runner
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tests/NAME_suite.ads test suite spec.
   procedure Write_Test_Suite_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tests/NAME_suite.adb test suite body.
   procedure Write_Test_Suite_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tests/NAME_test.ads test case spec.
   procedure Write_Test_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write tests/NAME_test.adb test case body
   --  with a placeholder assertion.
   procedure Write_Test_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write src/NAME.ads root package spec.
   --  Exe: pragma Pure. Lib: declares Hello procedure.
   procedure Write_Root_Package_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind);

   --  Write src/NAME-main.ads procedure spec.
   --  Exe projects only.
   procedure Write_Main_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write src/NAME-main.adb procedure body.
   --  Exe projects only.
   procedure Write_Main_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   --  Write src/NAME.adb root package body.
   --  Lib projects only.
   procedure Write_Root_Package_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

end Tada.Templates;
