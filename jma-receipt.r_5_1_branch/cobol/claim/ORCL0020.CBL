      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/05/09  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  LF02-FILE       ASSIGN  FILE-NAME
                                   FILE    STATUS  IS  STS-LF02.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  LF02-FILE.
      *01  OUT-LF02-REC            PIC X(30000).
       01  LF02-REC.        
           COPY    "CPRLF002.INC".
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-NAME.
           03                      PIC X(14)  VALUE  "/var/tmp/claim". 
           03  FILE-HOSPNUM        PIC 9(02). 
           03  FILE-PTID           PIC 9(10). 
           03                      PIC X(06)  VALUE  ".txt".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      *
      *01  LF02-REC.        
      ***  COPY    "CPRLF002.INC".
      *     
018000*    フラグ領域
       01  STS-AREA.
           03  STS-LF02            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-KBN     PIC  X(01).
               05  WRK-PARA-PTID    PIC  9(10).
               05  WRK-PARA-SRYKA   PIC  X(02).
               05  WRK-PARA-SRYYMD  PIC  X(08).
               05  WRK-PARA-HKNCOMBINUM
                                    PIC  X(04). 
               05  WRK-PARA-HOSPNUM PIC  9(02).
               05  WRK-PARA-DRCD    PIC  X(05).
               05  WRK-PARA-DENPNUM
                                    PIC  X(07).
               05  WRK-PARA-SYORIFLG
                                    PIC  X(01).
      *
           03  WRK-IN               PIC X(01).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
       01  WRK-LF02-REC. 
           COPY    "CPRLF002.INC"    REPLACING  //LF02//
                                     BY         //WRK-LF02//.
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *PROCEDURE                   DIVISION.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    MOVE    COMMAND-PARAM       TO  WRK-PARA
           UNSTRING COMMAND-PARAM      DELIMITED BY ","
                                       INTO WRK-PARA-KBN
                                            WRK-PARA-PTID
                                            WRK-PARA-SRYKA
                                            WRK-PARA-SRYYMD
                                            WRK-PARA-HKNCOMBINUM
                                            WRK-PARA-HOSPNUM
                                            WRK-PARA-DRCD
                                            WRK-PARA-DENPNUM
                                            WRK-PARA-SYORIFLG
           END-UNSTRING
      *
      *
      *???
           DISPLAY "L0020 DRCD= " WRK-PARA-DRCD "##"
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM 
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM 
           MOVE    "2"                 TO  SPA-NYUGAIKBN 
           DISPLAY "WRK-PARA-HKNCOMBINUM = " 
                                WRK-PARA-HKNCOMBINUM "##"
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY "WRK-PARA-DENPNUM = " WRK-PARA-DENPNUM
           CALL    "ORCL0021"          USING   WRK-PARA-KBN
                                               WRK-PARA-PTID
                                               WRK-PARA-SRYKA
                                               SPA-HOSPNUM
                                               SPA-SYSYMD
                                               SPA-SRYYMD
                                               SPA-NYUGAIKBN
                                               SPA-HKNCOMBINUM
                                               WRK-PARA-DRCD
                                               WRK-PARA-DENPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-LF02-REC
      *
           MOVE    WRK-LF02-REC        TO  LF02-REC
      *     
           MOVE    WRK-PARA-HOSPNUM    TO  FILE-HOSPNUM
           MOVE    WRK-PARA-PTID       TO  FILE-PTID
           OPEN    OUTPUT    LF02-FILE
      *     
           MOVE    WRK-LF02-REC        TO  LF02-REC
           WRITE                           LF02-REC
           IF      STS-LF02        NOT =   ZERO
               DISPLAY "*** ORCL0020  ERR="   STS-LF02
           END-IF
      **** MOVE    LF02-REC            TO  WRK-DATA-REC 
      *    PERFORM VARYING     IDX     FROM   1   BY  1
      *            UNTIL       IDX     >      7
      *
      *        MOVE    WRK-DATA (IDX)      TO  OUT-LF02-REC
      *        WRITE                       OUT-LF02-REC
      *        IF      STS-LF02        NOT =   ZERO
      *            DISPLAY "*** ORCL0020  ERR="   STS-LF02
      *            MOVE    10             TO  IDX
      *        END-IF
      *****END-PERFORM
           CLOSE   LF02-FILE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCL0020 END ***"
      *    ACCEPT  WRK-IN
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
