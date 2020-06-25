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
       PROGRAM-ID.                 ORCBM570.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : 労災レセ電データチェックリスト
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/07    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    チェック結果ファイル
           SELECT  IN01-FILE       ASSIGN  CHKERRFILE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    チェック結果ソートファイル
           SELECT  IN02-FILE       ASSIGN  WRK-ASS02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN02-KEY
                                   FILE    STATUS  IS  STS-IN02.
      *    エラーファイル
           SELECT  CHECKERR-FILE    ASSIGN  CHECKERR
                                   FILE    STATUS  IS  STS-CHECKERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    チェック結果ファイル
       FD  IN01-FILE.
       01  IN01-REC                PIC X(20000).
      *
      *    チェック結果ソートファイル
       FD  IN02-FILE.
       01  IN02-REC.
           03  IN02-KEY.
               05  IN02-FILENAME           PIC X(20).
               05  IN02-SRT-RECENUM        PIC X(06).
               05  IN02-RECELINE           PIC 9(04).
               05  IN02-ROWNUM             PIC 9(03).
               05  IN02-ERRCD              PIC X(05).
               05  IN02-SEQ                PIC 9(05).
           05  IN02-RECENUM                PIC X(06).
           03  IN02-RECID                  PIC X(02).
           03  IN02-PTNUM                  PIC X(20).
           03  IN02-NAME                   PIC X(100).
           03  IN02-BIRTHDAY               PIC X(08).
           03  IN02-SRYYM                  PIC X(06).
           03  IN02-NYUGAIKBN              PIC X(01).
           03  IN02-TANKINUM               PIC X(14).
           03  IN02-CHOKINUM               PIC X(9).
           03  IN02-SHOBYOYMD              PIC X(8).
           03  IN02-RYOSTYMD               PIC X(8).
           03  IN02-RYOEDYMD               PIC X(8).
           03  IN02-LABEL                  PIC X(200).
           03  IN02-VALUE                  PIC X(200).
           03  IN02-VALUE-LEN              PIC 9(05).
           03  IN02-HEADER                 PIC X(300).
           03  IN02-HEADER-LEN             PIC 9(05).
           03  IN02-FOOTER                 PIC X(300).
           03  IN02-FOOTER-LEN             PIC 9(05).
           03  IN02-MSG                    PIC X(300).
           03  IN02-LINE                   PIC 9(07).
           03  IN02-DATA                   PIC X(10000).
      *
      *    エラーファイル
       FD  CHECKERR-FILE.
       01  CHECKERR-REC                    PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    チェック結果ファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //CHKERRFILE//.
      *    チェック結果ソートファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ASS02//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC"  REPLACING  //RECEERR//
                                    BY         //CHECKERR//.
      *
      *    ファイルステータス
       01  STS-AREA.
           03  STS-IN01                    PIC X(02).
           03  STS-IN02                    PIC X(02).
           03  STS-CHECKERR                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-IN01                    PIC 9(01).
           03  FLG-IN02                    PIC 9(01).
      *
           03  FLG-HKNJAINF                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXP                        PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-SEQ                     PIC 9(05).
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-IN                      PIC 9(05).
      *
       01  KEY-AREA.
           03  KEY-FILENAME                PIC X(20).
           03  KEY-RECENUM                 PIC X(06).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI       PIC X(01).
           05  WRK-PARA-DATAKBN            PIC X(01).
           05  WRK-PARA-SYORIKBN           PIC X(01).
           05  WRK-PARA-JOBID              PIC 9(07).
           05  WRK-PARA-SHELLID            PIC X(08).
           05  WRK-PARA-HOSPNUM            PIC 9(02).
           05  WRK-PARA-HKNJANUM           PIC X(08).
           03  LNK-PARA-CHKERRFILE         PIC X(100).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KANACHK-STR             PIC X(5000).
           03  WRK-KANACHK-LEN             PIC 9(05).
           03  WRK-STR                     PIC X(500).
           03  WRK-STR-Z   REDEFINES   WRK-STR.
               05  WRK-STR-ZX99            PIC Z(9)9.
               05  WRK-STR-FL              PIC X(490).
           03  WRK-STR-TMP                 PIC X(500).
           03  WRK-SKYYM-PRT               PIC X(16).
           03  WRK-SAKYMD-PRT-G.
               05  WRK-SAKYMD-PRT          PIC X(22).
               05  WRK-SAKYMD-FL1          PIC X(02).
               05  WRK-SAKYMD-HH           PIC X(04).
               05  WRK-SAKYMD-HH-LBL       PIC X(02).
               05  WRK-SAKYMD-MM           PIC X(04).
               05  WRK-SAKYMD-MM-LBL       PIC X(02).
           03  WRK-PAGE                    PIC ZZZZZ9.
           03  WRK-TAGFL1                  PIC X.
           03  WRK-TAGFL2                  PIC X.
      *****03  WRK-ASS02                   PIC X(100).
           03  WRK-RENBAN                  PIC ZZZZ9.
           03  WRK-RECENUM                 PIC X(06).
           03  WRK-LINE                    PIC Z(6)9.
           03  WRK-ERRMSG-G.
               05  WRK-ERRMSG              PIC X(102)
                                           OCCURS  4.
           03  WRK-ERRMSG2                 PIC X(1000).
           03  WRK-ENDVALUE-X              PIC X(50).
           03  WRK-ENDVALUE-N              PIC X(50).
           03  WRK-LEN                     PIC 9(05).
      *
      *    エラーメッセージ
       01  WRK-ERR-AREA.
           03  WRK-CHECKERR                 PIC X(200).
           03  WRK-CHECKERR-G               PIC X(300).
           03  WRK-MCP-RC                  PIC 9(9).
           03  WRK-ERR-FILE-STS            PIC X(02).
           03  WRK-ERR-FILE-MSG            PIC X(100).
           03  WRK-ERR-FILE-MSG1           PIC X(17).
           03  WRK-ERR-FLG                 PIC 9(01).
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    日付編集用ワーク
       01  WRK-HEN-AREA.
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG.
               05  WRK-HENYMD-G            PIC X(04).
               05  WRK-HENYMD-Y            PIC X(04).
               05  WRK-HENYMD-FIL1         PIC X(02).
               05  WRK-HENYMD-M            PIC X(04).
               05  WRK-HENYMD-FIL2         PIC X(02).
               05  WRK-HENYMD-D            PIC X(04).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HMS.
               05  WRK-HH                  PIC 9(02).
               05  WRK-MM                  PIC 9(02).
               05  WRK-DD                  PIC 9(02).
           03  WRK-ZHMS.
               05  WRK-ZHH                 PIC Z9.
               05  WRK-ZMM                 PIC Z9.
               05  WRK-ZDD                 PIC Z9.
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX              PIC 9(05) VALUE 8.
           03  CONST-ERRMSG-LINE-LEN       PIC 9(05) VALUE 102.
      *
           COPY    "HCM75.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(400).
      *
      ******************************************************************
      *
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM570 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
                                       SPA-AREA
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
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               LNK-PARA-CHKERRFILE
                                               WRK-PARA-HOSPNUM
                                               CHECKERR
           END-UNSTRING
      *
           DISPLAY "LNK-PARA-CHKERRFILE=" LNK-PARA-CHKERRFILE
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    CHECKERR        TO  SGETTEMP-BASENAMES (1)
           MOVE    LNK-PARA-CHKERRFILE
                                   TO  SGETTEMP-BASENAMES (2)
           STRING  LNK-PARA-CHKERRFILE         DELIMITED BY SPACE
                   ".SRT"                      DELIMITED BY SIZE
                                       INTO    SGETTEMP-BASENAMES (3)
           END-STRING
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  CHECKERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  CHECKERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  CHKERRFILE
           MOVE    SGETTEMP-FULLNAMES (3)
                                   TO  WRK-ASS02
      *
           MOVE   SGETTEMP-ST      TO  WRK-SGETTEMP-ST
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBM570"          TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "レセ電データチェックリスト（労災）"
                                           TO  JOB-SHELLMSG
               WHEN    OTHER
                   MOVE    "レセ電データチェックリスト（労災）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  WRK-SKYYM-PRT
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD-PRT
           MOVE    "　"                TO  WRK-SAKYMD-FL1
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           MOVE    WRK-HH              TO  WRK-ZHH
           MOVE    WRK-MM              TO  WRK-ZMM
           MOVE    WRK-DD              TO  WRK-ZDD
           MOVE    WRK-ZHMS            TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-KANACHK-STR(1:4) TO WRK-SAKYMD-HH
           MOVE    "時"                 TO WRK-SAKYMD-HH-LBL
           MOVE    WRK-KANACHK-STR(5:4) TO WRK-SAKYMD-MM
           MOVE    "分"                 TO WRK-SAKYMD-MM-LBL
      *
           MOVE    "<END_VALUE>"       TO  WRK-ENDVALUE-X
           MOVE    WRK-ENDVALUE-X      TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-KANACHK-STR     TO  WRK-ENDVALUE-N
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    チェック結果ソートファイル編集処理
           PERFORM 2001-IN02-EDIT-SEC
      *
      *    エラーリスト編集処理
           IF    ( CNT-SEQ         >   ZERO )
               PERFORM 2002-ERRLIST-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック結果ソートファイル編集処理
      *****************************************************************
       2001-IN02-EDIT-SEC          SECTION.
      *
           OPEN    INPUT   IN01-FILE
           IF      STS-IN01            =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    CHKERRFILE          TO  WRK-ERR-FILE-NM
               MOVE    STS-IN01            TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT  IN02-FILE
           IF      STS-IN02            =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    WRK-ASS02           TO  WRK-ERR-FILE-NM
               MOVE    STS-IN02            TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           MOVE    ZERO            TO  CNT-SEQ
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
      *
               PERFORM 20011-IN01-EDIT-SEC
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE           IN01-FILE
           CLOSE           IN02-FILE
      *
           .
       2001-IN02-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック結果ソートファイル編集処理
      *****************************************************************
       20011-IN01-EDIT-SEC     SECTION.
      *
           COMPUTE CNT-SEQ =   CNT-SEQ +   1
      *
           INITIALIZE                  IN02-REC
      *
           MOVE    CNT-SEQ TO  IN02-SEQ
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_RECENUM>"
                           OR     "</ERR_RECENUM>"
           INTO            WRK-TAGFL1
                           IN02-RECENUM
                           COUNT IN WRK-LEN
           END-UNSTRING
      *
           IF    ( WRK-LEN         >=   6 )
               MOVE    IN02-RECENUM    TO  IN02-SRT-RECENUM
           ELSE
               MOVE    ALL "0"         TO  IN02-SRT-RECENUM
               MOVE    IN02-RECENUM    TO  IN02-SRT-RECENUM
                                              (6 - WRK-LEN :)
           END-IF
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_FILENAME>"
                           OR     "</ERR_FILENAME>"
           INTO            WRK-TAGFL1
                           IN02-FILENAME
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_PTNUM>"
                           OR     "</ERR_PTNUM>"
           INTO            WRK-TAGFL1
                           IN02-PTNUM
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_NAME>"
                           OR     "</ERR_NAME>"
           INTO            WRK-TAGFL1
                           IN02-NAME
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_BIRTHDAY>"
                           OR     "</ERR_BIRTHDAY>"
           INTO            WRK-TAGFL1
                           IN02-BIRTHDAY
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_SRYYM>"
                           OR     "</ERR_SRYYM>"
           INTO            WRK-TAGFL1
                           IN02-SRYYM
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_NYUGAIKBN>"
                           OR     "</ERR_NYUGAIKBN>"
           INTO            WRK-TAGFL1
                           IN02-NYUGAIKBN
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_TANKINUM>"
                           OR     "</ERR_TANKINUM>"
           INTO            WRK-TAGFL1
                           IN02-TANKINUM
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_CHOKINUM>"
                           OR     "</ERR_CHOKINUM>"
           INTO            WRK-TAGFL1
                           IN02-CHOKINUM
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_SHOBYOYMD>"
                           OR     "</ERR_SHOBYOYMD>"
           INTO            WRK-TAGFL1
                           IN02-SHOBYOYMD
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_RYOSTYMD>"
                           OR     "</ERR_RYOSTYMD>"
           INTO            WRK-TAGFL1
                           IN02-RYOSTYMD
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_RYOEDYMD>"
                           OR     "</ERR_RYOEDYMD>"
           INTO            WRK-TAGFL1
                           IN02-RYOEDYMD
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_ERRCD>"
                           OR     "</ERR_ERRCD>"
           INTO            WRK-TAGFL1
                           IN02-ERRCD
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_RECID>"
                           OR     "</ERR_RECID>"
           INTO            WRK-TAGFL1
                           IN02-RECID
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_RECELINE>" 
                           OR     "</ERR_RECELINE>"
           INTO            WRK-TAGFL1
                           IN02-RECELINE
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_ROWNUM>" 
                           OR     "</ERR_ROWNUM>"
           INTO            WRK-TAGFL1
                           IN02-ROWNUM
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_LABEL>"
                           OR     "</ERR_LABEL>"
           INTO            WRK-TAGFL1
                           IN02-LABEL
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_VALUE>" 
                           OR     "</ERR_VALUE>"
           INTO            WRK-TAGFL1
                           IN02-VALUE
                           COUNT IN IN02-VALUE-LEN
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_HEADER>" 
                           OR     "</ERR_HEADER>"
           INTO            WRK-TAGFL1
                           IN02-HEADER
                           COUNT IN IN02-HEADER-LEN
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_FOOTER>" 
                           OR     "</ERR_FOOTER>"
           INTO            WRK-TAGFL1
                           IN02-FOOTER
                           COUNT IN IN02-FOOTER-LEN
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_MSG>"
                           OR     "</ERR_MSG>"
           INTO            WRK-TAGFL1
                           IN02-MSG
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_LINE>"  
                           OR     "</ERR_LINE>"
           INTO            WRK-TAGFL1
                           IN02-LINE
           END-UNSTRING
      *
           UNSTRING        IN01-REC
           DELIMITED       BY      "<ERR_DATA>"  
                           OR     "</ERR_DATA>"
           INTO            WRK-TAGFL1
                           IN02-DATA
           END-UNSTRING
      *
      *****PERFORM 20021-DISPLAY-SEC
           DISPLAY "PTNUM= " IN02-PTNUM
      *
           WRITE   IN02-REC
           IF      STS-IN02            =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    WRK-ASS02           TO  WRK-ERR-FILE-NM
               MOVE    STS-IN02            TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           .
       20011-IN01-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       2002-ERRLIST-EDIT-SEC       SECTION.
      *
           OPEN    INPUT           IN02-FILE
           IF      STS-IN02            =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    WRK-ASS02           TO  WRK-ERR-FILE-NM
               MOVE    STS-IN02            TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           PERFORM 900-IN02-READ-SEC
      *
           MOVE    LOW-VALUE           TO  KEY-FILENAME
                                           KEY-RECENUM
           PERFORM UNTIL ( FLG-IN02    NOT =   ZERO )
      *
               PERFORM 210-HEAD-SEC
      *
               PERFORM UNTIL ( FLG-IN02    NOT =   ZERO )
                        OR   ( CNT-LINE       >=   CONST-LINE-MAX )
      *
                   PERFORM 220-BODY-SEC
      *
                   PERFORM 900-IN02-READ-SEC
      *
               END-PERFORM
      *
               PERFORM 230-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE   IN02-FILE
      *
           .
       2002-ERRLIST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY処理
      *****************************************************************
       20021-DISPLAY-SEC           SECTION.
      *
      *
           DISPLAY "FILENAME["   IN02-FILENAME "]"
                   "RECENUM["   IN02-RECENUM "]"
                   "PTNUM["     IN02-PTNUM (1:10) "]"
                   "NAME["      IN02-NAME  (1:64) "]"
                   "BIRTHDAY["  IN02-BIRTHDAY "]"
                   "SRYYM["     IN02-SRYYM "]"
                   "NYUGAIKBN[" IN02-NYUGAIKBN "]"
                   "TANKINUM["   IN02-TANKINUM "]"
                   "CHOKINUM["   IN02-CHOKINUM "]"
                   "SHOBYOYMD["   IN02-SHOBYOYMD "]"
                   "SHOBYOYMD["   IN02-RYOSTYMD "]"
                   "SHOBYOYMD["   IN02-RYOEDYMD "]"
                   "ERRCD["     IN02-ERRCD "]"
                   "RECID["     IN02-RECID "]"
                   "RECELINE["     IN02-RECELINE "]"
                   "ROWNUM["    IN02-ROWNUM "]"
                   "LABEL["     IN02-LABEL (1:20) "]"
                   "VALUE["     IN02-VALUE (1:20) "]"
                   "MSG["       IN02-MSG   (1:20) "]"
                   "HEADER["       IN02-HEADER   (1:20) "]"
                   "FOOTER["       IN02-FOOTER   (1:20) "]"
                   "LINE["      IN02-LINE "]"
                   "DATA["      IN02-DATA   (1:20) "]"
      *
           .
       20021-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       210-HEAD-SEC                    SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    SPACE               TO  HCM75
           INITIALIZE                      HCM75
      *
      *    診療年月
           MOVE    WRK-SKYYM-PRT       TO  HCM75-SKYYM
      *
      *    提出先
           EVALUATE    WRK-PARA-TEISYUTUSAKI
           WHEN    "1"
                   MOVE    "労災"          TO  HCM75-SSKNAME
           WHEN    OTHER
                   MOVE    "労災"          TO  HCM75-SSKNAME
           END-EVALUATE
      *
      *    作成日
           MOVE    WRK-SAKYMD-PRT-G    TO  HCM75-SAKYMD
      *
           EVALUATE    WRK-PARA-DATAKBN
               WHEN    "1"
                   MOVE    "【提出用レセプト】"    TO  HCM75-MEMO1
               WHEN    "2"
                   MOVE    "【点検用レセプト】"    TO  HCM75-MEMO1
           END-EVALUATE
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
               MOVE    "個別作成"          TO  HCM75-MEMO1 (20:)
           END-IF      
      *
           EVALUATE    WRK-PARA-HKNJANUM
               WHEN    "99999999"
                   INITIALIZE                      HKNJAINF-REC
               WHEN    "00000000"
                   INITIALIZE                      HKNJAINF-REC
                   MOVE    "直接請求する保険者以外"
                                                TO  HKNJA-HKNJANAME
               WHEN    OTHER
                   MOVE    SPACE               TO  HKNJAINF-REC
                   INITIALIZE                      HKNJAINF-REC
                   MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                   MOVE    WRK-PARA-HKNJANUM   TO  HKNJA-HKNJANUM
                   MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                   PERFORM 900-HKNJAINF-READ-SEC
           END-EVALUATE
           MOVE    HKNJA-HKNJANAME     TO  HCM75-MEMO2
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCM75-PAGE
      *
           .
       210-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       220-BODY-SEC                    SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           IF    ( CNT-LINE            =   1 )
            OR   ( IN02-FILENAME   NOT =   KEY-FILENAME )
            OR   ( IN02-RECENUM    NOT =   KEY-RECENUM )
      *
               PERFORM 2201-PTINF-EDIT-SEC
      *
               MOVE    IN02-FILENAME   TO  KEY-FILENAME
               MOVE    IN02-RECENUM    TO  KEY-RECENUM
      *
           END-IF
      *
           MOVE    IN02-FILENAME       TO  HCM75-FILENAME (CNT-LINE)
      *
           MOVE    IN02-RECENUM        TO  WRK-RECENUM
           MOVE    WRK-RECENUM         TO  HCM75-RECENUM (CNT-LINE)
      *
           IF    ( IN02-RECELINE   NOT =   ZERO )
               MOVE    "-"             TO  HCM75-RECENUMFL (CNT-LINE)
               MOVE    SPACE           TO  WRK-STR
               MOVE    IN02-RECELINE   TO  WRK-STR-ZX99
               PERFORM 800-LTRIM-SEC
               MOVE    WRK-STR         TO  HCM75-RECELINE  (CNT-LINE)
           END-IF
      *
           MOVE    IN02-ERRCD          TO  HCM75-ERRCD (CNT-LINE)
      *
           MOVE    1                   TO  IDXP
           MOVE    SPACE               TO  WRK-ERRMSG2
      *
           IF    ( IN02-HEADER     NOT =   SPACE )
               MOVE    IN02-HEADER     TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-SEC
               STRING  WRK-KANACHK-STR DELIMITED BY SPACE
                       "　"            DELIMITED BY SIZE
               INTO    WRK-ERRMSG2
               WITH    POINTER IDXP
               END-STRING
           END-IF
      *
           IF    ( IN02-LABEL      NOT =   SPACE )
               PERFORM 2201-LABEL-EDIT-SEC
               PERFORM 2201-VALUE-EDIT-SEC
               PERFORM 2201-ROWNUM-EDIT-SEC
           END-IF
      *
           IF    ( IN02-FOOTER     NOT =   SPACE )
               MOVE    IN02-FOOTER     TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-SEC
               STRING  "（"            DELIMITED BY SIZE
                       WRK-KANACHK-STR DELIMITED BY SPACE
                       "）"            DELIMITED BY SIZE
               INTO    WRK-ERRMSG2
               WITH    POINTER IDXP
               END-STRING
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRMSG-G
           MOVE    IN02-MSG            TO  WRK-ERRMSG-G
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   4 )
                    OR   ( WRK-ERRMSG (IDXE)   =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           COMPUTE IDXE
             =   ( IDXE - 1 )
             *     CONST-ERRMSG-LINE-LEN
             +     1
      *
           MOVE    WRK-ERRMSG2         TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-KANACHK-STR     TO  WRK-ERRMSG-G(IDXE:)
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   4 )
                    OR   ( WRK-ERRMSG (IDXE)   =   SPACE )
               MOVE    WRK-ERRMSG (IDXE)
                                      TO   HCM75-ERRMSG (CNT-LINE IDXE)
           END-PERFORM
      *
           IF    ( IN02-LINE       NOT =   ZERO )
               MOVE    IN02-LINE       TO  WRK-LINE
               MOVE    WRK-LINE        TO  HCM75-LINE (CNT-LINE)
           END-IF
      *
           IF    ( IN02-DATA       NOT =   SPACE )
               MOVE    IN02-DATA(1:180)    TO  WRK-KANACHK-STR
               PERFORM 800-KANACHK-SEC
               IF    ( IN02-DATA(181:) NOT =   SPACE )
                   STRING  WRK-KANACHK-STR(1:170)  DELIMITED   BY  SIZE
                           "…"                    DELIMITED   BY  SIZE
                   INTO    HCM75-CSVDATA (CNT-LINE)
                   END-STRING
               ELSE
                   MOVE    WRK-KANACHK-STR TO  HCM75-CSVDATA (CNT-LINE)
               END-IF
           END-IF
      *
           .
       220-BODY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2201-PTINF-EDIT-SEC             SECTION.
      *
           IF    ( IN02-PTNUM (11:)    =   SPACE )
               MOVE    IN02-PTNUM          TO  HCM75-PTNUM-BOT(CNT-LINE)
           ELSE
               MOVE    IN02-PTNUM(1:10)    TO  HCM75-PTNUM-TOP(CNT-LINE)
               MOVE    IN02-PTNUM(11:)     TO  HCM75-PTNUM-BOT(CNT-LINE)
           END-IF
      *
           IF    ( IN02-NAME (33:)     =   SPACE )
               MOVE    IN02-NAME           TO  HCM75-NAME-BOT(CNT-LINE)
           ELSE
               MOVE    IN02-NAME(1:32)     TO  HCM75-NAME-TOP(CNT-LINE)
               MOVE    IN02-NAME(33:)      TO  HCM75-NAME-BOT(CNT-LINE)
           END-IF
      *
           MOVE    IN02-BIRTHDAY       TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD1         TO  HCM75-BIRTHDAY (CNT-LINE)
      *
           EVALUATE    IN02-NYUGAIKBN
           WHEN    "1"
               MOVE    "入院"          TO  HCM75-NYUGAI (CNT-LINE)
           WHEN    "2"
               MOVE    "入院外"        TO  HCM75-NYUGAI (CNT-LINE)
           END-EVALUATE
      *
           MOVE    IN02-SRYYM          TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD1(1:6)    TO  HCM75-SRYYM (CNT-LINE)
      *
           MOVE    IN02-TANKINUM       TO  HCM75-TANKINUM (CNT-LINE)
      *
           MOVE    IN02-CHOKINUM       TO  HCM75-CHOKINUM (CNT-LINE)
      *
           MOVE    IN02-SHOBYOYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD1         TO  HCM75-SHOBYOYMD (CNT-LINE)
      *
           MOVE    IN02-RYOSTYMD       TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD1         TO  HCM75-RYOSTYMD (CNT-LINE)
      *
           MOVE    IN02-RYOEDYMD       TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD1         TO  HCM75-RYOEDYMD (CNT-LINE)
      *
           .
       2201-PTINF-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目名編集処理
      *****************************************************************
       2201-LABEL-EDIT-SEC             SECTION.
      *
           STRING  "項目名［"              DELIMITED   BY  SIZE
                   IN02-LABEL              DELIMITED   BY  SPACE
                   "］"                    DELIMITED   BY  SIZE
           INTO    WRK-ERRMSG2
           WITH    POINTER IDXP
           END-STRING
      *
           .
       2201-LABEL-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レコード内項目位置編集処理
      *****************************************************************
       2201-ROWNUM-EDIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-STR
           MOVE    IN02-ROWNUM         TO  WRK-STR-ZX99
           PERFORM 800-LTRIM-SEC
           STRING  "レコード内項目位置［"
                                           DELIMITED   BY  SIZE
                   WRK-STR                 DELIMITED   BY  SPACE
                   "］"                    DELIMITED   BY  SIZE
           INTO    WRK-ERRMSG2
           WITH    POINTER IDXP
           END-STRING
      *
           .
       2201-ROWNUM-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目内容編集処理
      *****************************************************************
       2201-VALUE-EDIT-SEC             SECTION.
      *
           IF    ( IN02-VALUE-LEN      >   ZERO )
               MOVE    SPACE           TO  WRK-KANACHK-STR
               STRING  IN02-VALUE(1: IN02-VALUE-LEN)
                                           DELIMITED BY SIZE
                       WRK-ENDVALUE-X      DELIMITED BY SIZE
               INTO    WRK-KANACHK-STR
               END-STRING
               PERFORM 800-ZENKAKU-SEC
               MOVE    SPACE           TO  WRK-STR
               UNSTRING    WRK-KANACHK-STR DELIMITED BY WRK-ENDVALUE-N
               INTO    WRK-STR
               END-UNSTRING
           ELSE
               MOVE    SPACE           TO  WRK-STR
           END-IF
      *
           MOVE    WRK-STR(1:100)      TO  WRK-KANACHK-STR
           PERFORM 800-ZENKAKU-SEC
           STRING  "内容［"            DELIMITED   BY  SIZE
                   WRK-KANACHK-STR     DELIMITED   BY  SPACE
           INTO    WRK-ERRMSG2
           WITH    POINTER IDXP
           END-STRING
      *
           IF    ( WRK-STR(101:)       NOT =   SPACE )
               STRING  "…"                DELIMITED   BY  SIZE
               INTO    WRK-ERRMSG2
               WITH    POINTER IDXP
               END-STRING
           END-IF
      *
           STRING  "］"                    DELIMITED   BY  SIZE
           INTO    WRK-ERRMSG2
           WITH    POINTER IDXP
           END-STRING
      *
           .
       2201-VALUE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-PRINT-SEC                   SECTION.
      *
            INITIALIZE                     ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCM75.red"          TO  SPRT-PRTID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "レセ電データチェックリスト（労災）"
                                           TO  SPRT-TITLE
               WHEN    OTHER
                   MOVE    "レセ電データチェックリスト（労災）"
                                           TO  SPRT-TITLE
           END-EVALUATE
           MOVE    HCM75               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA 
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                           TO  WRK-CHECKERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    SPACE               TO  WRK-CHECKERR-G
               STRING  "ORCBM570 "         DELIMITED   BY  SIZE
                       WRK-CHECKERR        DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-CHECKERR-G
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   CHECKERR-FILE
           IF      STS-CHECKERR        =   ZERO
               CLOSE   CHECKERR-FILE
           ELSE
               OPEN    OUTPUT              CHECKERR-FILE
      *
               MOVE    WRK-CHECKERR        TO  CHECKERR-REC
               WRITE   CHECKERR-REC
               CLOSE   CHECKERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CHECKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-CHECKERR-G
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-CHECKERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCBM570 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-CHECKERR-G
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
                                       JOB-UPDCNT1
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM570 IN   "  CNT-IN
           DISPLAY "***          PAGE "  CNT-PAGE
           DISPLAY "*** ORCBM570 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF    ( STS-DAY-RC1         =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               END-IF
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           .
       800-SEIWA-EXT.
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
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    500             TO  KANACHK-MAX-CNT
           MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
           MOVE    KANACHK-MAX     TO  WRK-KANACHK-LEN
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    文字列チェック処理
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    500             TO  KANACHK-MAX-CNT
           MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
           MOVE    KANACHK-MAX     TO  WRK-KANACHK-LEN
      *
           .
       800-KANACHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-LTRIM-SEC               SECTION.
      *
           IF    ( WRK-STR         =   SPACE )
               CONTINUE
           ELSE
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >       500 )
      *
                   IF    ( WRK-STR (IDXL:1)    NOT =   SPACE )
                       MOVE    WRK-STR (IDXL:)
                                           TO  WRK-STR-TMP
                       MOVE    WRK-STR-TMP TO  WRK-STR
                       MOVE    500         TO  IDXL
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       800-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ファイル読み込み処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
               AT  END
                   MOVE    1           TO  FLG-IN01
               NOT AT  END
                   IF      STS-IN01          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    CHKERRFILE          TO  WRK-ERR-FILE-NM
                       MOVE    STS-IN01            TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1           TO  CNT-IN
           END-READ
      *
           .
       900-IN01-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ソートファイル読み込み処理
      *****************************************************************
       900-IN02-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-IN02
      *
           READ    IN02-FILE       NEXT
               AT  END
                   MOVE    1           TO  FLG-IN02
               NOT AT  END
                   IF      STS-IN02          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    WRK-ASS02           TO  WRK-ERR-FILE-NM
                       MOVE    STS-IN02            TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
           END-READ
      *
           .
       900-IN02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスタ読込
      *****************************************************************
       900-HKNJAINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
                   INITIALIZE                      HKNJAINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
               INITIALIZE                      HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
