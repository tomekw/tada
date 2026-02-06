with AUnit.Run;
with AUnit.Reporter.Text;
with Tada_Suite;

procedure Run_Tests is

   procedure Runner is new AUnit.Run.Test_Runner
     (Tada_Suite.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Runner (Reporter);
end Run_Tests;
