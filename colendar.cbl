      *    colendar
      *    a calendar cli for IBM COBOL
      *    why did i do this
       IDENTIFICATION DIVISION.
       PROGRAM-ID. colendar.
       AUTHOR. Hannah Liberty.
       DATE-WRITTEN. 18th August 2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EVENTS ASSIGN TO "data/events.txt"
           ORGANIZATION IS LINE SEQUENTIAL ACCESS MODE IS SEQUENTIAL.

           SELECT SRT-EVENTS ASSIGN TO "colendar.sort".
       DATA DIVISION.
       FILE SECTION.
       FD  EVENTS.
       01  EVENT-ENTRY.
           05 EVENT-DATE.
             10 EVENT-YEAR PIC 9(4).
             10 EVENT-MONTH PIC 9(2).
             10 EVENT-DAY PIC 9(2).
           05 EVENT-SPACE1 PIC X.
           05 EVENT-ID PIC 9(5).
           05 EVENT-SPACE2 PIC X.
           05 EVENT-DESC PIC X(120).

       SD  SRT-EVENTS.
       01  SRT-EVENT-ENTRY.
           05 SRT-EVENT-DATE.
             10 SRT-EVENT-YEAR PIC 9(4).
             10 SRT-EVENT-MONTH PIC 9(2).
             10 SRT-EVENT-DAY PIC 9(2).
           05 SRT-EVENT-SPACE1 PIC X.
           05 SRT-EVENT-DESC PIC X(120).
           05 SRT-EOL PIC X.

       WORKING-STORAGE SECTION.
       01  WS-EVENT-ENTRY.
           05 WS-EVENT-DATE.
             10 WS-EVENT-YEAR PIC 9(4).
             10 WS-EVENT-MONTH PIC 9(2).
             10 WS-EVENT-DAY PIC 9(2).
           05 WS-EVENT-SPACE PIC X.
           05 WS-EVENT-DESC PIC X(120).
       01  WS-EOF PIC A.

       01  IN-DATE PIC 9(8).
       01  IN-DESC PIC X(120).

       01  ARG PIC X(50) VALUE SPACES.
       01  ADDFLAG PIC 9 VALUE 1.
       01  HELPFLAG PIC 9 VALUE 1.

       01  FMTDATE.
           02 FMTYEAR PIC 9(4).
           02 FMTMONTH PIC 9(2).
           02 FMTDAY PIC 9(2).
       01  FMTREG PIC 9(8).
       01  DAYIDX PIC 9(2) VALUE 01.
       01  ZDAY PIC Z9.

       01  MONTH-LENGTH PIC 99.
       01  LEAP-YEAR PIC 9.
       01  YEAR-REM PIC 9.
       01  FIRST-OF-MONTH PIC 9(8).

       01  REFERENCEDAY.
           02 REFERENCEDATE PIC 9(8) VALUE 19940115.
           02 REFERENCEDAYOFWEEK PIC 9 VALUE 6.
       01  DELTA PIC 9(10).

       01  PRETTY-MONTH PIC A(3).

       PROCEDURE DIVISION.
      *
           ACCEPT ARG FROM ARGUMENT-VALUE
           PERFORM UNTIL ARG = SPACES
               IF FUNCTION UPPER-CASE(ARG) EQUAL TO 'ADD' THEN
                   MOVE 0 TO ADDFLAG END-IF
               IF FUNCTION UPPER-CASE(ARG) EQUAL TO 'HELP' THEN
                   MOVE 0 TO HELPFLAG END-IF
               MOVE SPACES TO ARG
               ACCEPT ARG FROM ARGUMENT-VALUE
           END-PERFORM

           IF ADDFLAG EQUAL TO 0 THEN
               PERFORM GET-EVENT
               PERFORM WRITE-EVENT
               STOP RUN
           END-IF

           IF HELPFLAG EQUAL TO 0 THEN
               PERFORM PRINT-HELP
               STOP RUN
           END-IF



      *    Pretty print a calendar and print future events
           PERFORM PRETTY-PRINT.
           DISPLAY " "
           PERFORM PRINT-EVENTS

           STOP RUN .

       GET-EVENT.
           DISPLAY "date (" FUNCTION CURRENT-DATE"): " WITH NO ADVANCING
           ACCEPT IN-DATE
           IF IN-DATE = 0 THEN
               MOVE FUNCTION CURRENT-DATE TO IN-DATE
           END-IF
           DISPLAY "description (max 120 chars): " WITH NO ADVANCING
           ACCEPT IN-DESC.

       WRITE-EVENT.
           OPEN EXTEND EVENTS
               MOVE IN-DATE TO WS-EVENT-DATE
               MOVE IN-DESC TO WS-EVENT-DESC
               MOVE SPACE TO WS-EVENT-SPACE
               WRITE EVENT-ENTRY FROM WS-EVENT-ENTRY
           CLOSE EVENTS

           PERFORM SORT-EVENTS.

       PRINT-EVENT.
           IF WS-EVENT-DATE > FUNCTION CURRENT-DATE THEN
           DISPLAY WS-EVENT-DATE " " WITH NO ADVANCING

           DISPLAY WS-EVENT-DESC
           END-IF.

       PRINT-EVENTS.
           PERFORM SORT-EVENTS

      *    Open the events file and print each one
           OPEN INPUT EVENTS.
           PERFORM UNTIL WS-EOF = 'Y'
           READ EVENTS NEXT RECORD INTO WS-EVENT-ENTRY
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END PERFORM PRINT-EVENT
           END-READ
           END-PERFORM
           CLOSE EVENTS.

       PRINT-HELP.
           DISPLAY "colendar - a COBOL calendar"
           DISPLAY "usage:"
           DISPLAY "  colendar          show future events"
           DISPLAY "  colendar add      add event to file"
           DISPLAY "  colendar help     show this message"
           .

       SORT-EVENTS.
           SORT SRT-EVENTS ON ASCENDING WS-EVENT-DATE WS-EVENT-DESC
           USING EVENTS GIVING EVENTS.

       GET-PRETTY-MONTH.
           EVALUATE FMTMONTH
               WHEN 01 MOVE "Jan" TO PRETTY-MONTH
               WHEN 02 MOVE "Feb" TO PRETTY-MONTH
               WHEN 03 MOVE "Mar" TO PRETTY-MONTH
               WHEN 04 MOVE "Apr" TO PRETTY-MONTH
               WHEN 05 MOVE "May" TO PRETTY-MONTH
               WHEN 06 MOVE "Jun" TO PRETTY-MONTH
               WHEN 07 MOVE "Jul" TO PRETTY-MONTH
               WHEN 08 MOVE "Aug" TO PRETTY-MONTH
               WHEN 09 MOVE "Sep" TO PRETTY-MONTH
               WHEN 10 MOVE "Oct" TO PRETTY-MONTH
               WHEN 11 MOVE "Nov" TO PRETTY-MONTH
               WHEN 12 MOVE "Dec" TO PRETTY-MONTH
           END-EVALUATE.

       GET-MONTH-LENGTH.
           EVALUATE FMTMONTH
               WHEN 01 MOVE 31 TO MONTH-LENGTH
               WHEN 02 PERFORM GET-FEB-LENGTH
               WHEN 03 MOVE 31 TO MONTH-LENGTH
               WHEN 04 MOVE 30 TO MONTH-LENGTH
               WHEN 06 MOVE 30 TO MONTH-LENGTH
               WHEN 09 MOVE 30 TO MONTH-LENGTH
               WHEN 11 MOVE 30 TO MONTH-LENGTH
               WHEN OTHER MOVE 31 TO MONTH-LENGTH
           END-EVALUATE.

       GET-FEB-LENGTH.
           PERFORM IS-LEAP-YEAR.
           IF LEAP-YEAR EQUAL TO 0 THEN
               MOVE 29 TO MONTH-LENGTH
           ELSE
               MOVE 28 TO MONTH-LENGTH
           END-IF.

       IS-LEAP-YEAR.
           IF FUNCTION MOD(FMTYEAR, 4) EQUAL TO 0 AND (
                   FUNCTION MOD(FMTYEAR, 100) NOT EQUAL TO 0 OR
                   FUNCTION MOD(FMTYEAR, 400) EQUAL TO 0
               )
           THEN MOVE 0 TO LEAP-YEAR
           ELSE MOVE 1 TO LEAP-YEAR
           END-IF.

       CALCULATE-DAY-OF-WEEK.
           COMPUTE DELTA = FUNCTION MOD(
               FUNCTION INTEGER-OF-DATE(FIRST-OF-MONTH)
               - FUNCTION INTEGER-OF-DATE(REFERENCEDATE)
               + 6, 7)
           IF DELTA = 0 THEN MOVE 7 TO DELTA END-IF
           COMPUTE DELTA = DELTA - 1
           .

       PRETTY-PRINT.
      *    Assorted date nonsense
           MOVE FUNCTION CURRENT-DATE TO FMTDATE.
           MOVE FMTDATE TO FMTREG.
           PERFORM GET-MONTH-LENGTH.
           PERFORM IS-LEAP-YEAR.
           PERFORM CALCULATE-DAY-OF-WEEK.
           PERFORM GET-PRETTY-MONTH

           DISPLAY PRETTY-MONTH " " FMTYEAR
           DISPLAY "MON   TUE   WED   THU   FRI   SAT   SUN".

      *    First, get the buffer to the right place.
           COMPUTE FIRST-OF-MONTH =
               FMTYEAR * 10000 + FMTMONTH * 100 + DAYIDX
           PERFORM CALCULATE-DAY-OF-WEEK
           PERFORM DELTA TIMES
               DISPLAY "      " WITH NO ADVANCING
           END-PERFORM
      *    Then loop through, printing date numbers.
           PERFORM UNTIL DAYIDX > MONTH-LENGTH
               PERFORM PRETTY-PRINT-DATE
               COMPUTE DAYIDX = DAYIDX + 1
      *        If it's sunday then start a new line
               COMPUTE DELTA = FUNCTION MOD(DELTA + 1, 7)
               IF DELTA = 0 THEN
               DISPLAY " "
               END-IF
           END-PERFORM
           DISPLAY " ".

       PRETTY-PRINT-DATE.
           MOVE DAYIDX TO ZDAY
           IF DAYIDX = FMTDAY THEN
           DISPLAY "<" ZDAY ">  " WITH NO ADVANCING
           ELSE
               DISPLAY " " ZDAY "   " WITH NO ADVANCING
           END-IF.
