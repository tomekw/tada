with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tada.Config;

package body Parse_Manifest_Tests is

   Test_Path : constant String := "test_manifest.toml";

   package MR renames Tada.Config.Manifest_Results;
   package NR renames Tada.Config.Project_Name_Results;
   package VR renames Tada.Config.Project_Version_Results;

   use MR;
   use NR;
   use VR;

   --  Write a single-line manifest test file.
   procedure Write_Manifest (Line_1 : String) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Test_Path);
      Ada.Text_IO.Put_Line (F, Line_1);
      Ada.Text_IO.Close (F);
   end Write_Manifest;

   --  Write a two-line manifest test file.
   procedure Write_Manifest
     (Line_1 : String;
      Line_2 : String)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Test_Path);
      Ada.Text_IO.Put_Line (F, Line_1);
      Ada.Text_IO.Put_Line (F, Line_2);
      Ada.Text_IO.Close (F);
   end Write_Manifest;

   --  Parse Test_Path, assert Ok, return the Manifest.
   function Parse_Ok (Label : String) return Tada.Config.Manifest is
      R : constant MR.Result := Tada.Config.Parse (Test_Path);
   begin
      Assert (R.Status = MR.Ok,
              Label & ": expected Ok");
      return R.Value;
   end Parse_Ok;

   --  Parse Test_Path and assert the result is Error.
   procedure Assert_Parse_Error (Label : String) is
      R : constant MR.Result := Tada.Config.Parse (Test_Path);
   begin
      Assert (R.Status = MR.Error,
              Label & ": expected Error");
   end Assert_Parse_Error;

   procedure Assert_Name
     (M        : Tada.Config.Manifest;
      Expected : String;
      Label    : String)
   is
   begin
      Assert (M.Name.Status = NR.Ok,
              Label & ": name expected Ok");
      Assert (To_String (M.Name.Value) = Expected,
              Label & ": name expected '" & Expected & "'");
   end Assert_Name;

   procedure Assert_Version
     (M        : Tada.Config.Manifest;
      Expected : String;
      Label    : String)
   is
   begin
      Assert (M.Version.Status = VR.Ok,
              Label & ": version expected Ok");
      Assert (To_String (M.Version.Value) = Expected,
              Label & ": version expected '" & Expected & "'");
   end Assert_Version;

   procedure Assert_No_Name
     (M     : Tada.Config.Manifest;
      Label : String)
   is
   begin
      Assert (M.Name.Status /= NR.Ok,
              Label & ": name expected absent");
   end Assert_No_Name;

   procedure Assert_No_Version
     (M     : Tada.Config.Manifest;
      Label : String)
   is
   begin
      Assert (M.Version.Status /= VR.Ok,
              Label & ": version expected absent");
   end Assert_No_Version;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Manifest");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Both_Keys'Access,
         "Both keys");
      Registration.Register_Routine
        (T, Test_Name_Only'Access,
         "Name only");
      Registration.Register_Routine
        (T, Test_Version_Only'Access,
         "Version only");
      Registration.Register_Routine
        (T, Test_Reversed_Order'Access,
         "Reversed order");
      Registration.Register_Routine
        (T, Test_Extra_Whitespace'Access,
         "Extra whitespace");
      Registration.Register_Routine
        (T, Test_Blank_Lines'Access,
         "Blank lines");
      Registration.Register_Routine
        (T, Test_Empty_File'Access,
         "Empty file");
      Registration.Register_Routine
        (T, Test_Key_Casing'Access,
         "Key casing");
      Registration.Register_Routine
        (T, Test_File_Not_Found'Access,
         "File not found");
      Registration.Register_Routine
        (T, Test_Unquoted_Value'Access,
         "Unquoted value");
      Registration.Register_Routine
        (T, Test_Missing_Closing_Quote'Access,
         "Missing closing quote");
      Registration.Register_Routine
        (T, Test_Unknown_Key'Access,
         "Unknown key");
      Registration.Register_Routine
        (T, Test_Invalid_Project_Name'Access,
         "Invalid project name");
      Registration.Register_Routine
        (T, Test_Empty_Value'Access,
         "Empty value");
   end Register_Tests;

   overriding
   procedure Tear_Down (Unused_T : in out Test_Case) is
   begin
      if Ada.Directories.Exists (Test_Path) then
         Ada.Directories.Delete_File (Test_Path);
      end if;
   end Tear_Down;

   procedure Test_Both_Keys
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest
        ("name = ""foo""",
         "version = ""1.0""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("both keys");
      begin
         Assert_Name (M, "foo", "both keys");
         Assert_Version (M, "1.0", "both keys");
      end;
   end Test_Both_Keys;

   procedure Test_Name_Only
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name = ""foo""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("name only");
      begin
         Assert_Name (M, "foo", "name only");
         Assert_No_Version (M, "name only");
      end;
   end Test_Name_Only;

   procedure Test_Version_Only
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("version = ""2.0""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("version only");
      begin
         Assert_No_Name (M, "version only");
         Assert_Version (M, "2.0", "version only");
      end;
   end Test_Version_Only;

   procedure Test_Reversed_Order
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest
        ("version = ""1.0""",
         "name = ""bar""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("reversed");
      begin
         Assert_Name (M, "bar", "reversed");
         Assert_Version (M, "1.0", "reversed");
      end;
   end Test_Reversed_Order;

   procedure Test_Extra_Whitespace
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name   =   ""foo""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("whitespace");
      begin
         Assert_Name (M, "foo", "whitespace");
      end;
   end Test_Extra_Whitespace;

   procedure Test_Blank_Lines
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Test_Path);
      Ada.Text_IO.New_Line (F);
      Ada.Text_IO.Put_Line (F, "name = ""foo""");
      Ada.Text_IO.New_Line (F);
      Ada.Text_IO.Close (F);

      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("blank lines");
      begin
         Assert_Name (M, "foo", "blank lines");
      end;
   end Test_Blank_Lines;

   procedure Test_Empty_File
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Test_Path);
      Ada.Text_IO.Close (F);

      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("empty file");
      begin
         Assert_No_Name (M, "empty file");
         Assert_No_Version (M, "empty file");
      end;
   end Test_Empty_File;

   procedure Test_Key_Casing
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("NAME = ""foo""");
      declare
         M : constant Tada.Config.Manifest :=
           Parse_Ok ("key casing");
      begin
         Assert_Name (M, "foo", "key casing");
      end;
   end Test_Key_Casing;

   procedure Test_File_Not_Found
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      R : constant MR.Result :=
        Tada.Config.Parse ("nonexistent_file.toml");
   begin
      Assert (R.Status = MR.Error,
              "not found: expected Error");
   end Test_File_Not_Found;

   procedure Test_Unquoted_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name = foo");
      Assert_Parse_Error ("unquoted");
   end Test_Unquoted_Value;

   procedure Test_Missing_Closing_Quote
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name = ""foo");
      Assert_Parse_Error ("missing close quote");
   end Test_Missing_Closing_Quote;

   procedure Test_Unknown_Key
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("author = ""me""");
      Assert_Parse_Error ("unknown key");
   end Test_Unknown_Key;

   procedure Test_Invalid_Project_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name = ""123""");
      Assert_Parse_Error ("invalid name");
   end Test_Invalid_Project_Name;

   procedure Test_Empty_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Write_Manifest ("name =");
      Assert_Parse_Error ("empty value");
   end Test_Empty_Value;

end Parse_Manifest_Tests;
