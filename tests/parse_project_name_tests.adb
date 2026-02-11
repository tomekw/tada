with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tada.Commands; use Tada.Commands;
with Tada.CL_Arguments;

package body Parse_Project_Name_Tests is

   use Command_Results;

   package CL renames Tada.CL_Arguments;

   procedure Assert_Parses_Ok
     (Input    : String;
      Expected : String;
      Label    : String)
   is
      Arguments : constant CL.Argument_List.Vector :=
        ["init", Input];
      Cmd : constant Command_Results.Result :=
        Parse (Arguments);
   begin
      Assert (Cmd.Status = Ok,
              Label & ": expected Ok");
      Assert (Cmd.Value.Kind = Init,
              Label & ": expected Init");
      Assert
        (To_String (Cmd.Value.Project_Name) = Expected,
         Label & ": expected '" & Expected & "'");
   end Assert_Parses_Ok;

   procedure Assert_Parses_Error
     (Input : String;
      Label : String)
   is
      Arguments : constant CL.Argument_List.Vector :=
        ["init", Input];
      Cmd : constant Command_Results.Result :=
        Parse (Arguments);
   begin
      Assert (Cmd.Status = Error,
              Label & ": expected Error");
   end Assert_Parses_Error;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Project_Name");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Valid_Simple_Name'Access,
         "Valid simple name");
      Registration.Register_Routine
        (T, Test_Valid_Name_With_Underscore'Access,
         "Valid name with underscore");
      Registration.Register_Routine
        (T, Test_Valid_Single_Letter'Access,
         "Valid single letter");
      Registration.Register_Routine
        (T, Test_Valid_Name_Is_Lowercased'Access,
         "Valid name is lowercased");
      Registration.Register_Routine
        (T, Test_Valid_Name_With_Digits'Access,
         "Valid name with digits");
      Registration.Register_Routine
        (T, Test_Valid_Multiple_Underscores'Access,
         "Valid multiple underscores");
      Registration.Register_Routine
        (T, Test_Missing_Name'Access,
         "Missing name");
      Registration.Register_Routine
        (T, Test_Empty_Name'Access,
         "Empty name");
      Registration.Register_Routine
        (T, Test_Starts_With_Digit'Access,
         "Starts with digit");
      Registration.Register_Routine
        (T, Test_Starts_With_Underscore'Access,
         "Starts with underscore");
      Registration.Register_Routine
        (T, Test_Ends_With_Underscore'Access,
         "Ends with underscore");
      Registration.Register_Routine
        (T, Test_Double_Underscore'Access,
         "Double underscore");
      Registration.Register_Routine
        (T, Test_Reserved_Word'Access,
         "Reserved word");
      Registration.Register_Routine
        (T, Test_Reserved_Word_Mixed_Case'Access,
         "Reserved word mixed case");
      Registration.Register_Routine
        (T, Test_Special_Character'Access,
         "Special character");
   end Register_Tests;

   procedure Test_Valid_Simple_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok
        ("my_project", "my_project", "simple name");
   end Test_Valid_Simple_Name;

   procedure Test_Valid_Name_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok
        ("hello_world", "hello_world", "underscore name");
   end Test_Valid_Name_With_Underscore;

   procedure Test_Valid_Single_Letter
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok ("a", "a", "single letter");
   end Test_Valid_Single_Letter;

   procedure Test_Valid_Name_Is_Lowercased
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok
        ("MyProject", "myproject", "lowercased");
   end Test_Valid_Name_Is_Lowercased;

   procedure Test_Valid_Name_With_Digits
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok
        ("project1", "project1", "name with digits");
   end Test_Valid_Name_With_Digits;

   procedure Test_Valid_Multiple_Underscores
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Ok
        ("a_b_c", "a_b_c", "multiple underscores");
   end Test_Valid_Multiple_Underscores;

   procedure Test_Missing_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Arguments : constant CL.Argument_List.Vector :=
        ["init"];
      Cmd : constant Command_Results.Result :=
        Parse (Arguments);
   begin
      Assert (Cmd.Status = Error,
              "missing name: expected Error");
   end Test_Missing_Name;

   procedure Test_Empty_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error ("", "empty name");
   end Test_Empty_Name;

   procedure Test_Starts_With_Digit
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error ("123bad", "starts with digit");
   end Test_Starts_With_Digit;

   procedure Test_Starts_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("_bad", "starts with underscore");
   end Test_Starts_With_Underscore;

   procedure Test_Ends_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("bad_", "ends with underscore");
   end Test_Ends_With_Underscore;

   procedure Test_Double_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("bad__name", "double underscore");
   end Test_Double_Underscore;

   procedure Test_Reserved_Word
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("procedure", "reserved word");
   end Test_Reserved_Word;

   procedure Test_Reserved_Word_Mixed_Case
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("Procedure", "reserved word mixed case");
   end Test_Reserved_Word_Mixed_Case;

   procedure Test_Special_Character
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        ("my-project", "special character (hyphen)");
   end Test_Special_Character;

end Parse_Project_Name_Tests;
