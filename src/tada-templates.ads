with Ada.Text_IO;

with Tada.Packages.Containers;

package Tada.Templates is
   use Ada.Text_IO;
   use Tada.Packages.Containers;

   procedure Emit
     (Path : String;
      Write : access procedure (File : File_Type));

   procedure Emit
     (Path : String;
      Write : access procedure (File : File_Type;
                                Name : String);
      Name : String);

   procedure Emit
     (Path : String;
      Write : access procedure (File : File_Type;
                                Name : String;
                                Kind : Package_Kind);
      Name : String;
      Kind : Package_Kind);

   procedure Emit
     (Path : String;
      Write : access procedure (File : File_Type;
                                Name : String;
                                Deps : Package_Info_Vectors.Vector);
      Name : String;
      Deps : Package_Info_Vectors.Vector);

   procedure Write_Readme (File : File_Type; Name : String);

   procedure Write_Manifest (File : File_Type; Name : String);

   procedure Write_Gitignore (File : File_Type; Name : String);

   procedure Write_Gitattributes (File : File_Type);

   procedure Write_GPR_Config (File : File_Type; Name : String);

   procedure Write_GPR_Deps (File : File_Type; Name : String; Deps : Package_Info_Vectors.Vector := Package_Info_Vectors.Empty_Vector);

   procedure Write_GPR_Main (File : File_Type; Name : String; Kind : Package_Kind);

   procedure Write_GPR_Tests (File : File_Type; Name : String);

   procedure Write_Tests_Runner (File : File_Type; Name : String);

   procedure Write_Tests_Spec (File : File_Type; Name : String);

   procedure Write_Tests_Body (File : File_Type; Name : String);

   procedure Write_Root_Package_Spec (File : File_Type; Name : String; Kind : Package_Kind);

   procedure Write_Main_Spec (File : File_Type; Name : String);

   procedure Write_Main_Body (File : File_Type; Name : String);

   procedure Write_Root_Package_Body (File : File_Type; Name : String);
end Tada.Templates;
