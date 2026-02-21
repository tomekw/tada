with Ada.Text_IO;

package Tada.Templates is
   use Ada;
   use Ada.Text_IO;

   procedure Write_Readme (File : File_Type; Name : String);

   procedure Write_Manifest (File : File_Type; Name : String);

   procedure Write_Gitignore (File : File_Type);

   procedure Write_GPR_Config (File : File_Type; Name : String);

   procedure Write_GPR_Deps (File : File_Type);

   procedure Write_GPR_Main (File : File_Type; Name : String; Kind : Package_Kind);

   procedure Write_GPR_Tests (File : File_Type; Name : String);

   procedure Write_Test_Runner (File : File_Type; Name : String);

   procedure Write_Test_Suite_Spec (File : File_Type; Name : String);

   procedure Write_Test_Suite_Body (File : File_Type; Name : String);

   procedure Write_Test_Spec (File : File_Type; Name : String);

   procedure Write_Test_Body (File : File_Type; Name : String);

   procedure Write_Root_Package_Spec (File : File_Type; Name : String; Kind : Package_Kind);

   procedure Write_Main_Spec (File : File_Type; Name : String);

   procedure Write_Main_Body (File : File_Type; Name : String);

   procedure Write_Root_Package_Body (File : File_Type; Name : String);
end Tada.Templates;
