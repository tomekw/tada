--  Root package for the Tada project management tool.
package Tada is
   pragma Pure;

   Version : constant String := "0.2.0";

   type Package_Kind is (Exe,
                         Lib);
end Tada;
