      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0050.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為PUSH_APIデータ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/05167    NACL−竹田 　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  LF01-FILE       ASSIGN  FILE-NAME
                                   FILE    STATUS  IS  STS-LF01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  LF01-FILE.
       01  LF01-REC. 
           03                      PIC X(500).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-NAME.
           03                      PIC X(500).
018000*    フラグ領域
       01  STS-AREA.
           03  STS-LF01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-KBN     PIC  X(01).
               05  WRK-PARA-PTID    PIC  9(10).
               05  WRK-PARA-SRYKA   PIC  X(02).
               05  WRK-PARA-SRYYMD  PIC  X(08).
               05  WRK-PARA-HOSPNUM PIC  9(02).
               05  WRK-PARA-DRCD    PIC  X(05).
               05  WRK-PARA-HKNCOMBI
                                    PIC  X(04).
               05  WRK-PARA-SINRYONAIYO
                                    PIC  X(16).
               05  WRK-PARA-DENPNUM
                                    PIC  X(07).
               05  WRK-PARA-SYSYMD
                                    PIC  X(08).
               05  WRK-PARA-SYOFLG
                                    PIC  X(01).
               05  WRK-PARA-PTNUM   PIC  X(20).
               05  WRK-PARA-UID 
                                    PIC  X(36).
      *
           03  WRK-IN               PIC X(01).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
       01  WRK-LF01-REC. 
           03  WRK-EVENT            PIC X(50).
           03  WRK-KBN              PIC X(01).
           03  WRK-PTNUM            PIC X(20).
           03  WRK-PDATE            PIC X(10).
           03  WRK-SRYKA            PIC X(02).
           03  WRK-DRCD             PIC X(05).
           03  WRK-HKNCOMBI         PIC X(04).
           03  WRK-DENPNUM          PIC X(07).
           03  WRK-MODE             PIC X(01).
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
           COPY    "CPORCSGETTEMP.INC".
      *
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(512).
      *PROCEDURE                   DIVISION.
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
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
                                            WRK-PARA-HKNCOMBI
                                            WRK-PARA-HOSPNUM
                                            WRK-PARA-DRCD
                                            WRK-PARA-DENPNUM
                                            WRK-PARA-SYOFLG
                                            WRK-PARA-PTNUM
                                            WRK-PARA-UID 
           END-UNSTRING
      *
      *???
           DISPLAY "WRK-PARA-UID " WRK-PARA-UID
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           IF      WRK-PARA-SYSYMD     NOT =   SPACE
               MOVE    WRK-PARA-SYSYMD     TO  SPA-SYSYMD
           END-IF
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    WRK-PARA-UID        TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  FILE-NAME
           DISPLAY "FILE-NAME = " FILE-NAME
      * FILE OPEN
           OPEN    OUTPUT    LF01-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *???
           DISPLAY "HOSPNUM " SPA-HOSPNUM "##"
      *???
           MOVE    "account"           TO  WRK-EVENT
           MOVE    WRK-PARA-KBN        TO  WRK-KBN  
           MOVE    WRK-PARA-PTNUM      TO  WRK-PTNUM
           MOVE    WRK-PARA-SRYYMD(1:4)
                                       TO  WRK-PDATE(1:4)
           MOVE    "-"                 TO  WRK-PDATE(5:1)
           MOVE    WRK-PARA-SRYYMD(5:2)
                                       TO  WRK-PDATE(6:2)
           MOVE    "-"                 TO  WRK-PDATE(8:1)
           MOVE    WRK-PARA-SRYYMD(7:2)
                                       TO  WRK-PDATE(9:2)
           MOVE    WRK-PARA-SRYKA      TO  WRK-SRYKA
           MOVE    WRK-PARA-DRCD       TO  WRK-DRCD
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-HKNCOMBI   TO  WRK-HKNCOMBI
           MOVE    WRK-PARA-DENPNUM    TO  WRK-DENPNUM
           MOVE    WRK-PARA-SYOFLG     TO  WRK-MODE
      *
           MOVE    WRK-LF01-REC        TO  LF01-REC
           WRITE   LF01-REC
           CLOSE   LF01-FILE
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
           DISPLAY "*** ORCL0050 END ***"
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
       900-DBCLOSE-EXT.
           EXIT.
      *
