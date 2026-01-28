with Ada.Text_IO; use Ada.Text_IO;

procedure Primes is
   function Is_Prime(N : Integer) return Boolean is
   begin
      if N < 2 then
         return False;
      end if;
      if N = 2 then
         return True;
      end if;
      if N mod 2 = 0 then
         return False;
      end if;
      
      declare
         I : Integer := 3;
      begin
         while I * I <= N loop
            if N mod I = 0 then
               return False;
            end if;
            I := I + 2;
         end loop;
      end;
      
      return True;
   end Is_Prime;
   
   Count : Integer := 0;
   Num : Integer := 2;
   Last_Prime : Integer := 0;
begin
   while Count < 1000 loop
      if Is_Prime(Num) then
         Last_Prime := Num;
         Count := Count + 1;
      end if;
      Num := Num + 1;
   end loop;
   
   Put_Line(Integer'Image(Last_Prime));
end Primes;
