1000 REM ARKABLE BUBBLESORT PROGRAM
1100 LET N = 10
1110 DIM A(N)
1200 FOR I = 1 TO N
1210 LET A(I) = RND(1)*10
1220 NEXT I
1120 PRINT "RANDOM NUMBERS:"
1230 GOSUB 3000
1240 GOSUB 2000
1250 PRINT "AFTER SORTING: "
1260 GOSUB 3000
1270 END
2000 REM BUBBLESORT SUBROUTINE
2010 FOR I = 1 TO N - 1
2020 FOR J = 1 TO N - I
2030 IF A(J) <= A(J+1) THEN 2070
2040 LET X = A(J)
2050 LET A(J) = A(J+1)
2060 LET A(J+1) = X
2070 NEXT J, I
2080 RETURN
3000 REM PRINT SUBROUTINE
3010 FOR I = 1 TO N-1
3020 PRINT A(I); ", ";
3030 NEXT I
3040 PRINT A(N)
3050 RETURN
