with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Tada.Commands; use Tada.Commands;
with Test_Helpers; use Test_Helpers;

package body Parse_Profile_Tests is

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Profile");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Build_Default_Profile'Access,
         "Build default profile");
      Registration.Register_Routine
        (T, Test_Build_Explicit_Debug'Access,
         "Build explicit debug");
      Registration.Register_Routine
        (T, Test_Build_Release'Access,
         "Build release");
      Registration.Register_Routine
        (T, Test_Build_Profile_Case_Insensitive'Access,
         "Build profile case insensitive");
      Registration.Register_Routine
        (T, Test_Build_Missing_Profile_Value'Access,
         "Build missing profile value");
      Registration.Register_Routine
        (T, Test_Build_Invalid_Profile'Access,
         "Build invalid profile");
      Registration.Register_Routine
        (T, Test_Build_Unexpected_Option'Access,
         "Build unexpected option");
      Registration.Register_Routine
        (T, Test_Test_Default_Profile'Access,
         "Test default profile");
      Registration.Register_Routine
        (T, Test_Test_Release'Access,
         "Test release");
      Registration.Register_Routine
        (T, Test_Test_Missing_Profile_Value'Access,
         "Test missing profile value");
      Registration.Register_Routine
        (T, Test_Test_Unexpected_Option'Access,
         "Test unexpected option");
   end Register_Tests;

   procedure Test_Build_Default_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["build"], "default");
   begin
      Assert (Cmd.Kind = Build,
              "default: expected Build");
      Assert (Cmd.Profile = Debug,
              "default: expected Debug");
   end Test_Build_Default_Profile;

   procedure Test_Build_Explicit_Debug
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["build", "--profile", "debug"],
           "explicit debug");
   begin
      Assert (Cmd.Kind = Build,
              "explicit debug: expected Build");
      Assert (Cmd.Profile = Debug,
              "explicit debug: expected Debug");
   end Test_Build_Explicit_Debug;

   procedure Test_Build_Release
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["build", "--profile", "release"],
           "release");
   begin
      Assert (Cmd.Kind = Build,
              "release: expected Build");
      Assert (Cmd.Profile = Release,
              "release: expected Release");
   end Test_Build_Release;

   procedure Test_Build_Profile_Case_Insensitive
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["build", "--profile", "DEBUG"],
           "case insensitive");
   begin
      Assert (Cmd.Kind = Build,
              "case insensitive: expected Build");
      Assert (Cmd.Profile = Debug,
              "case insensitive: expected Debug");
   end Test_Build_Profile_Case_Insensitive;

   procedure Test_Build_Missing_Profile_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["build", "--profile"],
         "missing value");
   end Test_Build_Missing_Profile_Value;

   procedure Test_Build_Invalid_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["build", "--profile", "fast"],
         "invalid profile");
   end Test_Build_Invalid_Profile;

   procedure Test_Build_Unexpected_Option
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["build", "--fast"],
         "unexpected option");
   end Test_Build_Unexpected_Option;

   procedure Test_Test_Default_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["test"], "default");
   begin
      Assert
        (Cmd.Kind = Tada.Commands.Test,
         "default: expected Test");
      Assert (Cmd.Profile = Debug,
              "default: expected Debug");
   end Test_Test_Default_Profile;

   procedure Test_Test_Release
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["test", "--profile", "release"],
           "release");
   begin
      Assert
        (Cmd.Kind = Tada.Commands.Test,
         "release: expected Test");
      Assert (Cmd.Profile = Release,
              "release: expected Release");
   end Test_Test_Release;

   procedure Test_Test_Missing_Profile_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["test", "--profile"],
         "missing value");
   end Test_Test_Missing_Profile_Value;

   procedure Test_Test_Unexpected_Option
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["test", "--verbose"],
         "unexpected option");
   end Test_Test_Unexpected_Option;

end Parse_Profile_Tests;
