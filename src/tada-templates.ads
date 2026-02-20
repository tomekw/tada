with Ada.Text_IO;

package Tada.Templates is
   use Ada;

   procedure Write_Readme (File : Text_IO.File_Type; Name : String);

   procedure Write_Manifest (File : Text_IO.File_Type; Name : String);

   procedure Write_Gitignore (File : Text_IO.File_Type);

   procedure Write_GPR_Config (File : Text_IO.File_Type; Name : String);

   procedure Write_GPR_Main (File : Text_IO.File_Type; Name : String; Kind : Package_Kind);

   procedure Write_GPR_Tests (File : Text_IO.File_Type; Name : String);

   procedure Write_Test_Runner (File : Text_IO.File_Type; Name : String);

   procedure Write_Test_Suite_Spec (File : Text_IO.File_Type; Name : String);

   procedure Write_Test_Suite_Body (File : Text_IO.File_Type; Name : String);

   procedure Write_Test_Spec (File : Text_IO.File_Type; Name : String);

   procedure Write_Test_Body (File : Text_IO.File_Type; Name : String);

   procedure Write_Root_Package_Spec (File : Text_IO.File_Type; Name : String; Kind : Package_Kind);

   procedure Write_Main_Spec (File : Text_IO.File_Type; Name : String);

   procedure Write_Main_Body (File : Text_IO.File_Type; Name : String);

   procedure Write_Root_Package_Body (File : Text_IO.File_Type; Name : String);
end Tada.Templates;
