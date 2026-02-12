with AUnit.Test_Cases;

package Parse_Manifest_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

   overriding
   procedure Tear_Down (Unused_T : in out Test_Case);

private

   procedure Test_Both_Keys
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Name_Only
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Version_Only
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Reversed_Order
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Extra_Whitespace
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Blank_Lines
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Empty_File
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Key_Casing
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_File_Not_Found
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Unquoted_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Missing_Closing_Quote
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Unknown_Key
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Invalid_Project_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Empty_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Manifest_Tests;
