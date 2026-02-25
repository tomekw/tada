with Ada.Containers.Indefinite_Holders;

package Tada is
   Version : constant String := "0.3.0";

   type Package_Kind is (Exe,
                         Lib);

   package String_Holders is new Ada.Containers.Indefinite_Holders (String);
end Tada;
