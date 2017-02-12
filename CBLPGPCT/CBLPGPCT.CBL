       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 CBLPGPCT.
       AUTHOR.                     PHILIP GINGERICH.
       DATE-WRITTEN.               01/15/2017.
       DATE-COMPILED.   
       
      *****************************************************************
      * 
      *    THIS PROGRAM WILL CREATE A REPORT SHOWING THE COST TO
      *    CREATE EACH MACHINE.    
      * 
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT MACHINE-MASTER
               ASSIGN TO "MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT INVENTORY-DATA
               ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT ORDER-DATA
               ASSIGN TO "ORDER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT MACH-SHT-COST-DATA
               ASSIGN TO "MACHSHTCOST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT MACH-COST-DATA
               ASSIGN TO "MACHINECOST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT PRTOUT
               ASSIGN TO "MACHSHTPCT.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  MACHINE-MASTER
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 13 CHARACTERS
           DATA RECORD IS MACHINE-RECORD.
           
       01  MACHINE-RECORD.
           05  I-MACHINE           PIC X(5).
           05  I-MACHINE-PART      PIC X(5).
           05  I-PART-QTY          PIC 999.
           
       FD  INVENTORY-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS INVENTORY-RECORD.
           
       01  INVENTORY-RECORD.
           05  I-INV-PART          PIC X(5).
           05  I-INV-QTY           PIC 9(5).
           05  I-INV-COST          PIC 9(4)V99.
           
       FD  ORDER-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 8 CHARACTERS
           DATA RECORD IS ORDER-RECORD.
           
       01  ORDER-RECORD.
           05  I-ORD-MACHINE       PIC X(5).
           05  I-ORD-NUM           PIC 9(3).
           
       FD  MACH-SHT-COST-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 15 CHARACTERS
           DATA RECORD IS MACH-SHT-COST-REC.
           
       01  MACH-SHT-COST-REC.
           05  I-SHT-MACHINE       PIC X(5).
           05  I-SHT-COST          PIC 9(8)V99.
           
       FD  MACH-COST-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 15 CHARACTERS
           DATA RECORD IS MACH-COST-REC.
           
       01  MACH-COST-REC.
           05  I-CST-MACHINE       PIC X(5).
           05  I-CST-COST          PIC 9(8)V99.
       
       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 80 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS PRTLINE.
           
       01  PRTLINE                 PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       01  WORK-AREA.
           05  MORE-RECS           PIC X           VALUE 'Y'.
               88  NO-MORE-RECS                    VALUE 'N'.
           05  C-PCTR              PIC 99          VALUE ZEROS.
           05  WK-HOLD             PIC X           VALUE SPACE.
           
       01  CURRENT-DATE.
           05  I-DATE.
               10  I-YEAR    PIC 9(4).
               10  I-MONTH   PIC 99.
               10  I-DAY     PIC 99.
           
       01  CO-TITLE-LINE.
           05  FILLER              PIC X(6)    VALUE "DATE: ".
           05  O-MONTH             PIC 99.
           05  FILLER              PIC X       VALUE "/".
           05  O-DAY               PIC 99.
           05  FILLER              PIC X       VALUE "/".
           05  O-YEAR              PIC 9(4).
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE 
                                           "GINGERICH'S MANUFACTURING".
           05  FILLER              PIC X(6)    VALUE 'PAGE:'.
           05  O-PCTR              PIC Z9.
           
       01  RPT-TITLE-LINE.
           05  FILLER              PIC X(25)   VALUE SPACES.
           05  FILLER              PIC X(30)   VALUE 
                                   'MACHINE COST PERCENTAGE REPORT'.
                                   
       01  COL-HDGS-LINE.
           05  FILLER              PIC X(22)   VALUE SPACES.
           05  FILLER              PIC X(7)    VALUE 'MACHINE'.
           05  FILLER              PIC X(21)   VALUE SPACES.
           05  FILLER              PIC X(10)    VALUE 'PERCENTAGE'.
           
       01  D1.
           05  FILLER              PIC X(23)   VALUE SPACES.
           05  O-MACHINE           PIC X(5).
           05  FILLER              PIC X(24)   VALUE SPACES.
           05  O-PERCENT           PIC Z99.99.
           
       01 BLANK-LINE               PIC X       VALUE SPACE.
           
       PROCEDURE DIVISION.
       
       L1-CBLPGPCT.
           PERFORM L2-INIT THRU L2-INIT-EXIT.
           PERFORM L2-MAINLINE THRU L2-MAINLINE-EXIT
               UNTIL NO-MORE-RECS.
           PERFORM L2-CLOSING THRU L2-CLOSING-EXIT.
           STOP RUN.
           
       L2-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-DAY TO O-DAY.
           MOVE I-YEAR TO O-YEAR.
           OPEN INPUT ORDER-DATA.
           OPEN INPUT MACH-SHT-COST-DATA.
           OPEN INPUT MACH-COST-DATA.
           OPEN OUTPUT PRTOUT.
           PERFORM L9-HDGS THRU L9-HDGS-EXIT.
       L2-INIT-EXIT.
           EXIT.
           
       L2-MAINLINE.
           PERFORM L3-DETAIL THRU L3-DETAIL-EXIT.
           MOVE 'N' TO MORE-RECS.
       L2-MAINLINE-EXIT.
           EXIT.
           
       L2-CLOSING.
           CLOSE ORDER-DATA.
           CLOSE MACH-SHT-COST-DATA.
           CLOSE MACH-COST-DATA.
           CLOSE PRTOUT.
       L2-CLOSING-EXIT.
           EXIT.
           
       L3-DETAIL.
           WRITE PRTLINE FROM D1
               AFTER ADVANCING 1 LINE.
       L3-DETAIL-EXIT.
           EXIT.
           
       L9-HDGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM CO-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM RPT-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COL-HDGS-LINE
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
       L9-HDGS-EXIT.
           EXIT.