with Ada.Strings.Unbounded;

--  Generic discriminated result type for operations that
--  can succeed with a value or fail with an error message.
generic
   type Value_Type is private;
package Tada.Results is
   use Ada.Strings.Unbounded;

   --  Ok or Error discriminant.
   type Result_Status is (Ok, Error);

   --  Discriminated result: Ok holds a Value,
   --  Error holds a Message.
   type Result (Status : Result_Status := Error) is
   record
      case Status is
         when Ok =>
            Value : Value_Type;
         when Error =>
            Message : Unbounded_String;
      end case;
   end record;
end Tada.Results;
