      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBGS120.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別請求チェック明細表
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/01/13    NACL-太田     新規作成
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
      *    エラーファイル
           SELECT  BGS100ERR-FILE  ASSIGN  BGS100P-ERRFNAME
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
       01  STS-AREA.
           03  STS-BGS100ERR       PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-PARA-NAME       PIC X(25).
           03  WRK-PARA            PIC X(100).
           03  WRK-SUBTITLE        PIC X(20).
      *
       01  CONST-AREA.
           03  CONST-H2204         PIC X(06)   VALUE "201004".
           03  CONST-SYAHO         PIC 9(02)   VALUE 1.
           03  CONST-KOKUHO        PIC 9(02)   VALUE 2.
           03  CONST-KOUKI         PIC 9(02)   VALUE 3.
           03  CONST-TOKUYO        PIC 9(02)   VALUE 4.
           03  CONST-JIHI          PIC 9(02)   VALUE 5.
           03  CONST-CHIKEN        PIC 9(02)   VALUE 6.
           03  CONST-ROUSAI        PIC 9(02)   VALUE 7.
           03  CONST-JIBAI         PIC 9(02)   VALUE 8.
           03  CONST-DAISAN        PIC 9(02)   VALUE 9.
           03  CONST-KOUGAI        PIC 9(02)   VALUE 10.
      *
      *    パラメタ領域
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ****************************************************************
       PROCEDURE               DIVISION
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
           IF    ( FLG-ERR         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP   RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              BGS100P-AREA
                                   SPA-AREA
                                   STS-AREA
                                   WRK-AREA
                                   FLG-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               BGS100P-JOBID
                                               BGS100P-SHELLID
                                               BGS100P-HOSPNUM
                                               BGS100P-ERRFNAME
                                               BGS100P-FORMKBN
                                               BGS100P-SRYYM
                                               BGS100P-NYUGAIKBN
                                               BGS100P-EDITKBN
                                               BGS100P-HKNKBN-G
                                               BGS100P-HKNJANUM
           END-UNSTRING
      *
           MOVE    BGS100P-HOSPNUM     TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SYSYMD
      *
           MOVE    SPA-HOSPNUM         TO  TMPFLPARA-HOSPNUM
           MOVE    SPA-TERMID          TO  TMPFLPARA-TERMID
           MOVE    "BGS100F"           TO  TMPFLPARA-FILE-ID
      *
           MOVE    1                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-SKYFNAME
      *
           MOVE    2                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-KOHFNAME
      *
           MOVE    3                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-HKNJAFNAME
      *
           MOVE    4                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-ETCFNAME
      *
           MOVE    5                   TO  TMPFLPARA-SYOKBN2
           MOVE    TMPFLPARA           TO  BGS100P-MEIFNAME
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BGS100P-SKYFNAME    TO  SGETTEMP-BASENAMES (1)
           MOVE    BGS100P-KOHFNAME    TO  SGETTEMP-BASENAMES (2)
           MOVE    BGS100P-HKNJAFNAME  TO  SGETTEMP-BASENAMES (3)
           MOVE    BGS100P-ETCFNAME    TO  SGETTEMP-BASENAMES (4)
           MOVE    BGS100P-MEIFNAME    TO  SGETTEMP-BASENAMES (5)
           MOVE    BGS100P-ERRFNAME    TO  SGETTEMP-BASENAMES (6)
           CALL    "ORCSGETTEMP"     USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BGS100P-SKYFNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  BGS100P-KOHFNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  BGS100P-HKNJAFNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  BGS100P-ETCFNAME
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  BGS100P-MEIFNAME
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  BGS100P-ERRFNAME
      *
           MOVE    SPACE               TO  WRK-SUBTITLE
           EVALUATE    BGS100P-FORMKBN
           WHEN    "1"
               MOVE    "（社保）"      TO  BGS100P-SHELLMSG
           WHEN    "2"
               MOVE    "（国保）"      TO  BGS100P-SHELLMSG
           WHEN    "3"
               MOVE    "（後期高齢者）"
                                       TO  BGS100P-SHELLMSG
           WHEN    "5"
               MOVE    "（特別療養費）"
                                       TO  BGS100P-SHELLMSG
           WHEN    "7"
               MOVE    "（労災）"
                                       TO  BGS100P-SHELLMSG
           WHEN    "8"
               MOVE    "（自賠責）"
                                       TO  BGS100P-SHELLMSG
           WHEN    "9"
               MOVE    "（公害）"
                                       TO  BGS100P-SHELLMSG
           WHEN    OTHER
               MOVE    "（標準帳票）"
                                       TO  BGS100P-SHELLMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  BGS100P-SHELLMSG
           STRING  "保険別請求チェック明細表"
                                       DELIMITED BY SIZE
                   WRK-SUBTITLE        DELIMITED BY SPACE
           INTO    BGS100P-SHELLMSG
           END-STRING
      *
           MOVE    "/var/tmp/"         TO  BGS100P-CSVDIR
      *
           MOVE    SPACE               TO  BGS100P-CSVFNAME
           STRING  SPA-HOSPNUM     DELIMITED   BY  SIZE
                   "hknseikyuchk_meisai_"
                                   DELIMITED   BY  SIZE
                   BGS100P-SRYYM   DELIMITED   BY  SIZE
                   ".csv"          DELIMITED   BY  SIZE
           INTO    BGS100P-CSVFNAME
           END-STRING
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 1001-PARA-CHK-SEC
      *
           IF    ( FLG-ERR             =   ZERO )
               PERFORM 1001-PRGOPTION-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ　処理
      *****************************************************************
       1001-PARA-CHK-SEC               SECTION.
      *
           IF    ( BGS100P-FORMKBN = "0" OR "1" OR "2" OR "3"
                                  OR "5"
                                  OR "7" OR "8" OR "9"
                                  OR "a" OR "A"
                                  OR "b" OR "B"  )
               CONTINUE
           ELSE
               MOVE    "0"         TO  BGS100P-FORMKBN
           END-IF
      *
           IF    ( BGS100P-NYUGAIKBN   = " " OR "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    " "         TO  BGS100P-NYUGAIKBN
           END-IF
      *
           IF    ( BGS100P-EDITKBN =   "1" OR "0" )
               CONTINUE
           ELSE
               MOVE    "0"         TO  BGS100P-EDITKBN
           END-IF
      *
           MOVE    BGS100P-SRYYM   TO  BGS100P-SRYYMD
           MOVE    "01"            TO  BGS100P-SRYYMD(7:2)
      *
           EVALUATE    BGS100P-FORMKBN
           WHEN    "0"
               MOVE    1           TO  BGS100P-SYAHO
                                       BGS100P-KOKUHO
                                       BGS100P-KOUKI
                                       BGS100P-TOKUYO
                                       BGS100P-ROUSAI
                                       BGS100P-JIBAI
                                       BGS100P-KOUGAI
                                       BGS100P-DAISAN
           WHEN    "a"
           WHEN    "A"
               MOVE    1           TO  BGS100P-SYAHO
                                       BGS100P-KOKUHO
                                       BGS100P-KOUKI
           WHEN    "b"
           WHEN    "B"
               MOVE    1           TO  BGS100P-TOKUYO
                                       BGS100P-ROUSAI
                                       BGS100P-JIBAI
                                       BGS100P-KOUGAI
                                       BGS100P-DAISAN
           WHEN    "1"
               MOVE    1           TO  BGS100P-SYAHO
           WHEN    "2"
               MOVE    1           TO  BGS100P-KOKUHO
           WHEN    "3"
               MOVE    1           TO  BGS100P-KOUKI
           WHEN    "5"
               MOVE    1           TO  BGS100P-TOKUYO
           WHEN    "7"
               MOVE    1           TO  BGS100P-ROUSAI
           WHEN    "8"
               MOVE    1           TO  BGS100P-JIBAI
               MOVE    1           TO  BGS100P-DAISAN
           WHEN    "9"
               MOVE    1           TO  BGS100P-KOUGAI
           END-EVALUATE
      *
           MOVE    1               TO  BGS100P-MEISAI
      *
           IF    ( BGS100P-SRYYM   >=  CONST-H2204 )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  WRK-ERRMSG
               STRING  "対象年月は平成２２年４月以降"
                                       DELIMITED BY SIZE
                       "としてください"
                                       DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       1001-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプション処理
      *****************************************************************
       1001-PRGOPTION-SEC              SECTION.
      *
           MOVE    ZERO            TO  BGS100P-SKYGET-PARA
      *
      *    （第三者行為）医療費一部負担額・食事標準負担額の集計先
           MOVE    "DAISAN_SAKI"   TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  BGS100P-SKYGET-PARA
           END-IF
      *
           .
       1001-PRGOPTION-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCBGS120"         TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    作表用データ作成
           IF    ( BGS100P-RC      =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCBGS110"
                   CALL    "ORCBGS110"  USING
                                       BGS100P-AREA
                                       SPA-AREA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCBGS110"
           END-IF
      *
      *    社保
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-SYAHO   =   1 )
                   MOVE    CONST-SYAHO     TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    国保
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-KOKUHO  =   1 )
                   MOVE    CONST-KOKUHO    TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    後期高齢者
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-KOUKI   =   1 )
                   MOVE    CONST-KOUKI     TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    特別療養費
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-TOKUYO  =   1 )
                   MOVE    CONST-TOKUYO    TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    労災
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-ROUSAI   =   1 )
                   MOVE    CONST-ROUSAI    TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    自賠責
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-JIBAI   =   1 )
                   MOVE    CONST-JIBAI     TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    第三者行為
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-DAISAN  =   1 )
                   MOVE    CONST-DAISAN    TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
      *    公害
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-KOUGAI  =   1 )
                   MOVE    CONST-KOUGAI    TO  BGS100P-HOKEN
                   PERFORM 2001-ORCBGS121-SEC
               END-IF
           END-IF
      *
           EVALUATE    BGS100P-RC
           WHEN    2
               MOVE    SPACE   TO  WRK-ERRMSG
               STRING    "０点公費記載の対象レセプトがあるため"
                                           DELIMITED BY SIZE
                         "処理できません"  DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       2001-ORCBGS121-SEC                  SECTION.
      *
           PERFORM 800-MCNDATE-SEC
           DISPLAY SMCNDATE-HMS "ST:ORCBGS121:" BGS100P-HOKEN
           CALL    "ORCBGS121" USING
                               BGS100P-AREA
                               SPA-AREA
           PERFORM 800-MCNDATE-SEC
           DISPLAY SMCNDATE-HMS "ED:ORCBGS121:" BGS100P-HOKEN
      *
           .
       2001-ORCBGS121-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-SKYFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-KOHFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-HKNJAFNAME  TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-ETCFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    BGS100P-MEIFNAME    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           IF    ( BGS100P-PAGE        =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-ERR             =   ZERO )
               MOVE    "STM"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    BGS100P-PAGE    TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
