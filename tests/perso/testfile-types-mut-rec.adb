with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type A;
   type B is access A;
   type A is access B;
begin
   New_Line;
end;
