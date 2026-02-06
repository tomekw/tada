with Ada.Command_Line;
with Ada.Text_IO;

procedure Tada.Main is
   use Ada;

   Argument_Count : constant Natural := Command_Line.Argument_Count;
begin
   Text_IO.Put_Line ("Arguments count: " & Argument_Count'Image);

   if Argument_Count > 0 then
      for I in 1 .. Argument_Count loop
         Text_IO.Put_Line (Command_Line.Argument (I));
      end loop;
   end if;
end Tada.Main;
