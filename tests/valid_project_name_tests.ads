with AUnit.Test_Cases;

package Valid_Project_Name_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Simple_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Single_Letter
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_With_Digits
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Multiple_Segments
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Empty_String
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Starts_With_Digit
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Starts_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Ends_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Double_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Reserved_Word
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Hyphen
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Valid_Project_Name_Tests;
