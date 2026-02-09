with Ada.Strings.Unbounded;

generic
   type Value_Type is private;
package Tada.Results is
   use Ada.Strings.Unbounded;

   type Result_Status is (Ok, Error);

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
