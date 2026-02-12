with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Tada.Config;

package body Valid_Project_Name_Tests is

   procedure Assert_Valid
     (Name  : String;
      Label : String)
   is
   begin
      Assert (Tada.Config.Valid_Project_Name (Name),
              Label & ": expected valid");
   end Assert_Valid;

   procedure Assert_Invalid
     (Name  : String;
      Label : String)
   is
   begin
      Assert (not Tada.Config.Valid_Project_Name (Name),
              Label & ": expected invalid");
   end Assert_Invalid;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Valid_Project_Name");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Simple_Name'Access,
         "Simple name");
      Registration.Register_Routine
        (T, Test_With_Underscore'Access,
         "With underscore");
      Registration.Register_Routine
        (T, Test_Single_Letter'Access,
         "Single letter");
      Registration.Register_Routine
        (T, Test_With_Digits'Access,
         "With digits");
      Registration.Register_Routine
        (T, Test_Multiple_Segments'Access,
         "Multiple segments");
      Registration.Register_Routine
        (T, Test_Empty_String'Access,
         "Empty string");
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
        (T, Test_Hyphen'Access,
         "Hyphen");
   end Register_Tests;

   procedure Test_Simple_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Valid ("hello", "simple");
   end Test_Simple_Name;

   procedure Test_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Valid ("hello_world", "underscore");
   end Test_With_Underscore;

   procedure Test_Single_Letter
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Valid ("a", "single letter");
   end Test_Single_Letter;

   procedure Test_With_Digits
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Valid ("project1", "digits");
   end Test_With_Digits;

   procedure Test_Multiple_Segments
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Valid ("a_b_c", "segments");
   end Test_Multiple_Segments;

   procedure Test_Empty_String
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("", "empty");
   end Test_Empty_String;

   procedure Test_Starts_With_Digit
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("1bad", "digit start");
   end Test_Starts_With_Digit;

   procedure Test_Starts_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("_bad", "underscore start");
   end Test_Starts_With_Underscore;

   procedure Test_Ends_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("bad_", "underscore end");
   end Test_Ends_With_Underscore;

   procedure Test_Double_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("bad__name", "double underscore");
   end Test_Double_Underscore;

   procedure Test_Reserved_Word
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("procedure", "reserved");
   end Test_Reserved_Word;

   procedure Test_Hyphen
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Invalid ("my-project", "hyphen");
   end Test_Hyphen;

end Valid_Project_Name_Tests;
