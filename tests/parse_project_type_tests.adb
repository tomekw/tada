with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Tada.Commands; use Tada.Commands;
with Test_Helpers; use Test_Helpers;

package body Parse_Project_Type_Tests is

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Project_Type");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Default_Type'Access,
         "Default type");
      Registration.Register_Routine
        (T, Test_Explicit_Exe'Access,
         "Explicit exe");
      Registration.Register_Routine
        (T, Test_Explicit_Lib'Access,
         "Explicit lib");
      Registration.Register_Routine
        (T, Test_Invalid_Type'Access,
         "Invalid type");
   end Register_Tests;

   procedure Test_Default_Type
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["init", "foo"], "default");
   begin
      Assert (Cmd.Kind = Init,
              "default: expected Init");
      Assert (Cmd.Project_Type = Exe,
              "default: expected Exe");
   end Test_Default_Type;

   procedure Test_Explicit_Exe
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["init", "foo", "--exe"],
           "explicit exe");
   begin
      Assert (Cmd.Kind = Init,
              "explicit exe: expected Init");
      Assert (Cmd.Project_Type = Exe,
              "explicit exe: expected Exe");
   end Test_Explicit_Exe;

   procedure Test_Explicit_Lib
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["init", "foo", "--lib"],
           "explicit lib");
   begin
      Assert (Cmd.Kind = Init,
              "explicit lib: expected Init");
      Assert (Cmd.Project_Type = Lib,
              "explicit lib: expected Lib");
   end Test_Explicit_Lib;

   procedure Test_Invalid_Type
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["init", "foo", "--bad"],
         "invalid type");
   end Test_Invalid_Type;

end Parse_Project_Type_Tests;
