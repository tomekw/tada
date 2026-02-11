with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Tada.Commands; use Tada.Commands;
with Test_Helpers; use Test_Helpers;

package body Parse_Args_Tests is

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Parse_Args");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_Run_No_Args'Access,
         "Run no args");
      Registration.Register_Routine
        (T, Test_Run_With_Args'Access,
         "Run with args");
      Registration.Register_Routine
        (T, Test_Run_Separator_No_Args'Access,
         "Run separator no args");
      Registration.Register_Routine
        (T, Test_Run_Single_Arg'Access,
         "Run single arg");
      Registration.Register_Routine
        (T, Test_Run_Profile_And_Args'Access,
         "Run profile and args");
      Registration.Register_Routine
        (T, Test_Run_Profile_Release_And_Args'Access,
         "Run profile release and args");
      Registration.Register_Routine
        (T, Test_Run_Profile_No_Separator'Access,
         "Run profile no separator");
      Registration.Register_Routine
        (T, Test_Run_Profile_Separator_No_Args'Access,
         "Run profile separator no args");
   end Register_Tests;

   procedure Test_Run_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok (["run"], "no args");
   begin
      Assert (Cmd.Kind = Run,
              "no args: expected Run");
      Assert (Cmd.Run_Profile = Debug,
              "no args: expected Debug");
      Assert (Cmd.Args.Is_Empty,
              "no args: expected empty args");
   end Test_Run_No_Args;

   procedure Test_Run_With_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["run", "--", "a", "b"],
           "with args");
   begin
      Assert (Cmd.Kind = Run,
              "with args: expected Run");
      Assert
        (Natural (Cmd.Args.Length) = 2,
         "with args: expected 2 args");
      Assert (Cmd.Args (1) = "a",
              "with args: first arg");
      Assert (Cmd.Args (2) = "b",
              "with args: second arg");
   end Test_Run_With_Args;

   procedure Test_Run_Separator_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["run", "--"], "separator no args");
   end Test_Run_Separator_No_Args;

   procedure Test_Run_Single_Arg
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["run", "--", "x"], "single arg");
   begin
      Assert (Cmd.Kind = Run,
              "single arg: expected Run");
      Assert
        (Natural (Cmd.Args.Length) = 1,
         "single arg: expected 1 arg");
      Assert (Cmd.Args (1) = "x",
              "single arg: expected 'x'");
   end Test_Run_Single_Arg;

   procedure Test_Run_Profile_And_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["run", "--profile", "debug",
            "--", "y"],
           "profile+args");
   begin
      Assert (Cmd.Kind = Run,
              "profile+args: expected Run");
      Assert (Cmd.Run_Profile = Debug,
              "profile+args: expected Debug");
      Assert
        (Natural (Cmd.Args.Length) = 1,
         "profile+args: expected 1 arg");
      Assert (Cmd.Args (1) = "y",
              "profile+args: expected 'y'");
   end Test_Run_Profile_And_Args;

   procedure Test_Run_Profile_Release_And_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["run", "--profile", "release",
            "--", "a", "b", "c"],
           "release+args");
   begin
      Assert (Cmd.Kind = Run,
              "release+args: expected Run");
      Assert (Cmd.Run_Profile = Release,
              "release+args: expected Release");
      Assert
        (Natural (Cmd.Args.Length) = 3,
         "release+args: expected 3 args");
      Assert (Cmd.Args (1) = "a",
              "release+args: first arg");
      Assert (Cmd.Args (2) = "b",
              "release+args: second arg");
      Assert (Cmd.Args (3) = "c",
              "release+args: third arg");
   end Test_Run_Profile_Release_And_Args;

   procedure Test_Run_Profile_No_Separator
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Cmd : constant Command :=
        Parse_Ok
          (["run", "--profile", "release"],
           "no separator");
   begin
      Assert (Cmd.Kind = Run,
              "no separator: expected Run");
      Assert (Cmd.Run_Profile = Release,
              "no separator: expected Release");
      Assert (Cmd.Args.Is_Empty,
              "no separator: expected empty args");
   end Test_Run_Profile_No_Separator;

   procedure Test_Run_Profile_Separator_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert_Parses_Error
        (["run", "--profile", "debug", "--"],
         "profile+separator no args");
   end Test_Run_Profile_Separator_No_Args;

end Parse_Args_Tests;
