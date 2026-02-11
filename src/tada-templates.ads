with Ada.Text_IO;

with Tada.Commands;

package Tada.Templates is

   procedure Write_Readme
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Manifest
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Gitignore
     (File : Ada.Text_IO.File_Type);

   procedure Write_GPR_Config
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_GPR_Main
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind);

   procedure Write_GPR_Tests
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Test_Runner
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Test_Suite_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Test_Suite_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Test_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Test_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Root_Package_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String;
      Kind : Commands.Project_Kind);

   procedure Write_Main_Spec
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Main_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

   procedure Write_Root_Package_Body
     (File : Ada.Text_IO.File_Type;
      Name : String);

end Tada.Templates;
