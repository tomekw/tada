with AUnit.Test_Cases;

package Parse_Project_Name_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Valid_Simple_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Valid_Name_With_Underscore
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Valid_Single_Letter
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Valid_Name_Is_Lowercased
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Valid_Name_With_Digits
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Valid_Multiple_Underscores
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Missing_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Empty_Name
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

   procedure Test_Reserved_Word_Mixed_Case
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Special_Character
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Project_Name_Tests;
