with Ada.Containers.Indefinite_Holders;

package Tada is
   Version : constant String := "0.5.0";

   Package_Index_Url : constant String :=
     "https://raw.githubusercontent.com/tadapm/tada-packages/refs/heads/main/index";

   type Package_Kind is (Exe,
                         Lib);

   package String_Holders is new Ada.Containers.Indefinite_Holders (String);
end Tada;
