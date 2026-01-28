program Primes;

function IsPrime(n: Integer): Boolean;
var
    i: Integer;
begin
    if n < 2 then
        Exit(False);
    if n = 2 then
        Exit(True);
    if (n mod 2) = 0 then
        Exit(False);
    
    i := 3;
    while i * i <= n do
    begin
        if (n mod i) = 0 then
            Exit(False);
        i := i + 2;
    end;
    
    Exit(True);
end;

var
    count, num, lastPrime: Integer;
begin
    count := 0;
    num := 2;
    lastPrime := 0;
    
    while count < 1000 do
    begin
        if IsPrime(num) then
        begin
            lastPrime := num;
            count := count + 1;
        end;
        num := num + 1;
    end;
    
    writeln(lastPrime);
end.
