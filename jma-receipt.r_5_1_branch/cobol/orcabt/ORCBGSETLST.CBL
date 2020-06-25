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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBGSETLST.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : セット一覧表印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/04/17    NACL−森脇   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
      *    ＣＳＶファイル
           SELECT  RECECSV-FILE        ASSIGN  RECECSV
                                       ORGANIZATION    IS  LINE
                                                           SEQUENTIAL
                                       FILE    STATUS  IS  STS-RECECSV.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *    ＣＳＶファイル
       FD  RECECSV-FILE.
       01  RECECSV-REC                 PIC X(400).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //RECECSV//.
      *
      *    帳票
           COPY    "HCPOM2.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-RECECSV             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECECSV         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
      *
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KIKAN           PIC 9(01).
           03  FLG-ZENKAKU          PIC 9(01).
           03  FLG-HANKAKU          PIC 9(01).
      *
           03  FLG-KEIKA-GAI        PIC 9(01).
           03  FLG-KEIKA            PIC 9(01).
           03  FLG-OK-KEIKA         PIC 9(01).
      *
           03  FLG-KIZAI            PIC 9(01).
           03  FLG-OVER             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-P               PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX3                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  WRK-HEN-MAX         PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-STRCD          PIC X(06).
           03  WRK-PARA-ENDCD          PIC X(06).
           03  WRK-PARA-KEIKAYMD       PIC X(08).
           03  WRK-PARA-CSVKBN         PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
      *
           03  WRK-PAGE            PIC 9(03).
           03  PCNT                PIC 9(03).
      *
           03  WRK-KEY-SETCD       PIC  X(06).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-TANINAME        PIC X(04).
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-KAISU           PIC 9(05).
      *
           03  WRK-TNS-NAME        PIC X(200).
      *
           03  WRK-KEY-KIKAN.
               05  WRK-YUKOSTYMD       PIC X(08).
               05  WRK-YUKOEDYMD       PIC X(08).
      *
           03  WRK-INPUTCD-G.
               05  WRK-INPUTCD         PIC X(20)   OCCURS  5.
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-FTNKBN              PIC X(20).
      *
           03  WRK-TNS-KEIKASOCHIYMD   PIC X(08).
           03  WRK-TNS-KZMCOMPSIKIBETU PIC X(01).
           03  WRK-TNS-TANICD          PIC X(02).
           03  WRK-TNS-TANINAME        PIC X(20).
           03  WRK-TNS-DATAKBN         PIC X(01).
      *
           03  WRK-KEIKAYMD-G.
               05  WRK-KEIKA-H         PIC X(14).
               05  WRK-KEIKAYMD        PIC X(10).
               05  WRK-KEIKA-H2        PIC X(14).
               05  WRK-CHNGCD          PIC X(09).
      *    換算値数量
           03  WRK-SURYO               PIC 9(05)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(05).
               05  WRK-SURYO-S2        PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZ.
               05  WRK-SURYO-Z1-9      REDEFINES   WRK-SURYO-Z1
                                       PIC ZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HEN           PIC X(11).
           03  IDM                     PIC 9(04).
           03  IDM2                    PIC 9(04).
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *
      *    印刷内容
       01  HEAD-01.
           03  FILLER                  PIC X(10)   VALUE   SPACE.
           03  H01-MIDASI              PIC X(50).
           03  FILLER                  PIC X(05)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-YY                  PIC 9(04).
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-MM                  PIC Z9.
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-DD                  PIC Z9.
           03  FILLER                  PIC X(04)   VALUE   "  P-".
           03  H01-PCNT                PIC ZZ9.
       01  HEAD-010.
           03  FILLER                  PIC X(30)   VALUE   SPACE.
           03  H01-KEIKA-YMD           PIC X(30).
       01  HEAD-02.
           03  FILLER                  PIC X(02).
           03  H02-SETCD               PIC X(06).
           03  FILLER                  PIC X(02).
           03  H02-DSPNAME             PIC X(100).
       01  HEAD-03.
           03  FILLER                  PIC X(01).
           03  H03-MIDASI              PIC X(12).
           03  FILLER                  PIC X(01).
           03  H03-INPUTCD-G           PIC X(94).
      ***  03  H03-INPUTCD-G           OCCURS  5.
      *        05  H03-INPUTCD         PIC X(14).
      ***      05  H03-YOBI1           PIC X(01).
       01  HEAD-04.
           03  FILLER                  PIC X(01).
           03  H04-MIDASI              PIC X(14).
           03  FILLER                  PIC X(01).
           03  H04-YUKOSTYMD           PIC X(12).
           03  H04-KARA                PIC X(02).
           03  H04-YUKOEDYMD           PIC X(12).
       01  BODY-01N.
           03  B01N-YOBI1              PIC X(03).
           03  B01N-ZUMIFLG            PIC X(02).
           03  B01N-YOBI2              PIC X(11).
           03  B01N-NAME-G.
               05  B01N-NAME               PIC X(80).
               05  B01N-YOBI11             PIC X(12).
           03  B01N-NAME-G2            REDEFINES   B01N-NAME-G.
               05  B01N-NAME2              PIC X(66).
               05  B01N-YOBI3              PIC X(12).
               05  B01N-TANINAME           PIC X(04).
               05  B01N-YOBI4              PIC X(10).
           03  B01N-NAME-G3            REDEFINES   B01N-NAME-G.
               05  B01N-NAME3              PIC X(66).
               05  B01N-YOBI33             PIC X(12).
               05  B01N-TANINAME3          PIC X(04).
               05  B01N-YOBI34             PIC X(10).
      *
       01  BODY-01.
           03  B01-YOBI1               PIC X(06).
           03  B01-SRYCD               PIC X(09).
           03  B01-YOBI2               PIC X(01).
           03  B01-HENSYU.
               05  B01-NAME-FIL1           PIC X(66).
               05  B01-HENSYU-I.
                   07  B01-SURYO-HEN           PIC X(11).
                   07  B01-KAISU-HEN           PIC X(05).
                   07  B01-INPUT-HEN           PIC X(10).
           03  B01-HENSYU-2            REDEFINES   B01-HENSYU.
               05  B01-NAME-FIL2           PIC X(77).
               05  B01-KANSURYO-HEN        PIC X(15).
           03  B01-HENSYU-3            REDEFINES   B01-HENSYU.
               05  B01-YOBI131             PIC X(36).
               05  B01-HENSYU-4            PIC X(56).
           03  B01-HENSYU-5G           REDEFINES   B01-HENSYU.
               05  B01-YOBI141             PIC X(47).
               05  B01-HENSYU-5            PIC X(45).
      *
       01  WRK-B01-HENSYU.
           03  WRK-B01-SURYO-HEN           PIC X(15).
           03  WRK-B01-KAISU-HEN           PIC X(05).
      *
       01  WRK-HEN-NAME.
           03  WRK-NAME                PIC X(140).
      *
       01  WRK-HEN-SURYO.
           03  B01-SURYO1              PIC ZZZZ9.99999.
      *
       01  WRK-HEN-SURYO-3.
           03  B01-SURYO3              PIC ZZZZ9.
      *
       01  WRK-HEN-KAISU.
           03  B01-KAKERU              PIC X(02).
           03  B01-KAISU               PIC ZZZ.
      *
       01  WRK-HEN-SURYO-2.
           03  B01-SURYO12             PIC ZZZ9.
           03  B01-YOBI6               PIC X(02).
           03  B01-SURYO22             PIC ZZZZ.
      *
       01  WRK-HEN-ATAI.
           03  B01-ATAI1               PIC X(08).
           03  B01-ATAI1F              PIC X(01).
           03  B01-ATAI2               PIC X(08).
           03  B01-ATAI2F              PIC X(01).
           03  B01-ATAI3               PIC X(08).
           03  B01-ATAI3F              PIC X(01).
           03  B01-ATAI4               PIC X(08).
           03  B01-ATAI4F              PIC X(01).
           03  B01-ATAI5               PIC X(08).
      *
       01  WRK-HEN-INPUTHEN.
           03  B01-INPUTKBN            PIC X(03).
           03  B01-KANSURYO            PIC X(12).
      *
      *    内容
       01  WRK-HEN-AREA.
      *    半角
           03  WRK-TBL-REC             PIC X(108)  OCCURS   500.
      *    全角
           03  WRK-TBL-REC2            PIC X(108)  OCCURS   500.
      *
           03  WRK-PAGE-MAX            PIC 9(03).
      *
       01  WRK-HENYMD-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(08)   VALUE   "setlist-".
               05  WRK-TO-DATA-PRTYMD  PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *
       01  CSV-HEAD-AREA.
           03  CSV-SETCD               PIC X(06).
           03  CSV-DSPNAME             PIC X(100).
           03  CSV-YUKOSTYMD           PIC X(08).
           03  CSV-YUKOEDYMD           PIC X(08).
       01  CSV-BODY-AREA.
           03  CSV-ZUMIFLG             PIC X(02).
           03  CSV-SRYCD               PIC X(09).
           03  CSV-NAME                PIC X(100).
           03  CSV-SURYO1              PIC 9999999.99999.
           03  CSV-SURYO2              PIC 9999999.99999.
           03  CSV-TANINAME            PIC X(04).
           03  CSV-KAISU               PIC 999.
           03  CSV-BIKOU               PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
       PROCEDURE                   DIVISION
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
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      STS-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           UNSTRING    COMMAND-PARAM       DELIMITED  BY  ","
                       INTO                LNK-PRTKANRI-RENNUM
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
                                           WRK-PARA-JOBID
                                           WRK-PARA-SHELLID
                                           WRK-PARA-HOSPNUM
                                           RECEERR
                                           WRK-PARA-STRCD
                                           WRK-PARA-ENDCD
                                           WRK-PARA-KEIKAYMD
                                           WRK-PARA-CSVKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPA-SRYYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBGSETLST"       TO  JOB-PGID
           MOVE    "セット登録一覧"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    ファイル設定
           MOVE    "SETLST1"           TO  RECECSV-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECECSV-TERMID
           MOVE    SPA-HOSPNUM         TO  RECECSV-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECECSV-BASENAME    TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  RECECSV-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           IF      WRK-PARA-STRCD      =   SPACE
               MOVE    "P     "            TO  WRK-PARA-STRCD
           END-IF
           IF    ( WRK-PARA-ENDCD      =   ALL "9" )   OR
                 ( WRK-PARA-ENDCD      =   SPACE )
               MOVE    "SXXXXX"            TO  WRK-PARA-ENDCD
           END-IF
      *
      *    初期クリア
           MOVE    ZERO                TO  IDX-P
           MOVE    SPACE               TO  HCPOM2
      *
           MOVE    "セット登録一覧表"  TO  H01-MIDASI
      *
           IF      WRK-PARA-KEIKAYMD   NOT =   SPACE
               MOVE    WRK-PARA-KEIKAYMD   TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               STRING  "《経過措置日："        DELIMITED  BY  SIZE
                       WRK-HENYMDG             DELIMITED  BY  SIZE
                       "》"                    DELIMITED  BY  SIZE
                                               INTO    H01-KEIKA-YMD
               END-STRING
           ELSE
               MOVE    SPACE               TO  H01-KEIKA-YMD
           END-IF
      *
           MOVE    SPA-SRYYMD(1:4)     TO  H01-YY
           MOVE    SPA-SRYYMD(5:2)     TO  H01-MM
           MOVE    SPA-SRYYMD(7:2)     TO  H01-DD
      *
           IF      WRK-PARA-CSVKBN     NOT =   "1"
               MOVE    ZERO                TO  WRK-PARA-CSVKBN
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           MOVE    SPA-SRYYMD          TO  WRK-TO-DATA-PRTYMD
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    入力セット検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-PARA-STRCD      TO  ISET-SETCD
           MOVE    WRK-PARA-ENDCD      TO  ISET-UPHMS
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
               MOVE    1                   TO  FLG-END
           END-IF
      *
           OPEN    OUTPUT              RECECSV-FILE
      *
           PERFORM 410-CSV-HEAD-SEC
      *
      *    編集印刷処理
           PERFORM 200-MEISAI-SYORI-SEC
                   UNTIL       FLG-END     =   1
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    印刷処理
           IF      IDX-P               >   ZERO
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           CLOSE                       RECECSV-FILE
      *
           IF    ( PCNT                >   ZERO )  AND
                 ( WRK-PARA-CSVKBN     =   "1" )
               PERFORM 430-CSV-OUTSET-SEC
           END-IF
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    RECECSV         TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    編集印刷処理
      *****************************************************************
       200-MEISAI-SYORI-SEC            SECTION.
      *
           INITIALIZE                  WRK-HEN-AREA
           MOVE    ZERO                TO  FLG-KIKAN
           MOVE    ZERO                TO  FLG-KEIKA
           MOVE    ZERO                TO  IDX
           MOVE    ISET-SETCD          TO  WRK-KEY-SETCD
      *
      *    見出し編集
           MOVE    ZERO                TO  WRK-HEN-MAX
           PERFORM 2001-HEAD-HEN-SEC
           MOVE    SPACE               TO  WRK-YUKOSTYMD
           MOVE    SPACE               TO  WRK-YUKOEDYMD
      *
           PERFORM
                   UNTIL    (FLG-END       =    1    )  OR
                            (ISET-SETCD    NOT =   WRK-KEY-SETCD)
               PERFORM 2002-MEISAI-HEN-SEC
           END-PERFORM
      *
      *        経過措置対象なし
           IF     (WRK-PARA-KEIKAYMD   NOT =   SPACE )  AND
                  (FLG-KEIKA           =   ZERO  )
               INITIALIZE                  WRK-HEN-AREA
               MOVE    ZERO                TO  WRK-HEN-MAX
               MOVE    ZERO                TO  FLG-KEIKA
               IF      IDX-P               =   ZERO
                   COMPUTE PCNT                =   PCNT    -   1
               END-IF
           ELSE
      *        印刷編集処理
               PERFORM 2100-PRINT-HENSYU-SEC
      *
               ADD     1                   TO  IDX-P
               IF      IDX-P               <=  69
                   MOVE    ALL "-"             TO  HCPOM2-MOJI  (IDX-P)
               END-IF
           END-IF
      *
           .
       200-MEISAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2100-PRINT-HENSYU-SEC          SECTION.
      *
      *    最大行数のセット
           MOVE    69                  TO  IDX-MAX
      *    編集処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-HEN-MAX
               IF      IDX-P               >=  IDX-MAX
      *            改ページ
                   PERFORM 400-PRINT-SYORI-SEC
                   MOVE    ZERO                TO  IDX-P
      *            見出し編集
                   ADD     1                   TO  PCNT
                   MOVE    PCNT                TO  H01-PCNT
                   ADD     1                   TO  IDX-P
                   MOVE    HEAD-01             TO  HCPOM2-MOJI (IDX-P)
                   ADD     1                   TO  IDX-P
                   IF      WRK-PARA-KEIKAYMD   NOT =   SPACE
                       MOVE    HEAD-010        TO  HCPOM2-MOJI (IDX-P)
                       ADD     1                   TO  IDX-P
                   END-IF
               END-IF
               ADD     1                   TO  IDX-P
               MOVE    WRK-TBL-REC (IDX)   TO  HCPOM2-MOJI  (IDX-P)
               MOVE    WRK-TBL-REC2 (IDX)  TO  HCPOM2-MOJI2 (IDX-P)
           END-PERFORM
           INITIALIZE                  WRK-HEN-AREA
           MOVE    ZERO                TO  WRK-HEN-MAX
           .
       2100-PRINT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2001-HEAD-HEN-SEC          SECTION.
      *
      *    見出し編集
           IF      IDX-P               =   ZERO
               ADD     1                   TO  PCNT
               MOVE    PCNT                TO  H01-PCNT
               ADD     1                   TO  WRK-HEN-MAX
               MOVE    HEAD-01             TO  WRK-TBL-REC (WRK-HEN-MAX)
               ADD     1                   TO  WRK-HEN-MAX
               IF      WRK-PARA-KEIKAYMD   NOT =   SPACE
                   MOVE    HEAD-010        TO  WRK-TBL-REC (WRK-HEN-MAX)
                   ADD     1               TO  WRK-HEN-MAX
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  HEAD-02
           MOVE    SPACE               TO  HEAD-03
           MOVE    SPACE               TO  CSV-HEAD-AREA
           MOVE    SPACE               TO  WRK-INPUTCD-G
      *    入力ＣＤ
           MOVE    ISET-SETCD          TO  H02-SETCD
                                           CSV-SETCD
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    ISET-SETCD          TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM
               UNTIL     FLG-INPUTCD       =   1
               IF      ICD-INPUTCD         =   ISET-SETCD
      *            表示名
      ****         MOVE    ICD-DSPNAME         TO  H02-DSPNAME
      *            セット名称　編集
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    ICD-DSPNAME         TO  KANACHK-MAE-INPUT
                   MOVE    100                 TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  H02-DSPNAME
                                                   CSV-DSPNAME
      *
               ELSE
                   ADD     1                   TO  IDY
                   IF      IDY                 <=  5
                       MOVE    ICD-INPUTCD     TO  WRK-INPUTCD (IDY)
      *****************IF      IDY             >   1
      *                    MOVE    ","         TO  H03-YOBI1 (IDY - 1)
      *****************END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           ADD     1                   TO  WRK-HEN-MAX
           MOVE    HEAD-02             TO  WRK-TBL-REC (WRK-HEN-MAX)
           IF      IDY                 >   ZERO
               ADD     1                   TO  WRK-HEN-MAX
               MOVE    "【入力ＣＤ】"      TO  H03-MIDASI
               PERFORM 20011-INPUTCD-HEN-SEC
               MOVE    HEAD-03             TO  WRK-TBL-REC (WRK-HEN-MAX)
           END-IF
      *
           .
       2001-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ間編集処理
      *****************************************************************
       20011-INPUTCD-HEN-SEC          SECTION.
      *
           MOVE    1                    TO  IDX3
           MOVE    ZERO                 TO  FLG-ZENKAKU
           MOVE    ZERO                 TO  FLG-HANKAKU
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >  IDY
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-INPUTCD (IDX2)  TO  KANACHK-MAE-INPUT
               MOVE    20                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      IDX2                >   1
                  AND (H03-INPUTCD-G   NOT =   SPACE)
      *VER.4.7
                   MOVE    ","             TO  H03-INPUTCD-G
                                                          (IDX3:)
                   ADD     1               TO  IDX3
      *            全角
      *            IF      KANACHK-SYUBETU     =   2
      *                IF      FLG-HANKAKU     =   1
      *                    前に半角がある時、行を変える
      *                    MOVE    ","            TO  H03-INPUTCD-G
      *                                                   (IDX3:)
      *                    MOVE    HEAD-03     TO  WRK-TBL-REC
      *                                                (WRK-HEN-MAX)
      *                    ADD     1               TO  WRK-HEN-MAX
      *                    MOVE    SPACE           TO  HEAD-03
      *                    MOVE    1               TO  IDX3
      *                    MOVE    ZERO            TO  FLG-HANKAKU
      *                ELSE
      *                    MOVE    "　　，"        TO  H03-INPUTCD-G
      *                                                   (IDX3:)
      *                    ADD     6               TO  IDX3
      *                END-IF
      *            ELSE
      *                MOVE    ","            TO  H03-INPUTCD-G
      *                                                   (IDX3:)
      *                ADD     1               TO  IDX3
      *            END-IF
               END-IF
      ****     IF     (IDX3  +   KANACHK-MAX)  >   76
               IF     (IDX3  +   KANACHK-MAX)  >   94
                   MOVE    HEAD-03         TO  WRK-TBL-REC (WRK-HEN-MAX)
                   ADD     1                   TO  WRK-HEN-MAX
                   MOVE    SPACE               TO  HEAD-03
                   MOVE    1                   TO  IDX3
               END-IF
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-ZENKAKU
               ELSE
                   MOVE    1               TO  FLG-HANKAKU
               END-IF
               MOVE    WRK-INPUTCD (IDX2)(1:KANACHK-MAX)
                                           TO  H03-INPUTCD-G(IDX3:)
               ADD     KANACHK-MAX         TO  IDX3
           END-PERFORM
      *
           .
       20011-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    期間編集処理
      *****************************************************************
       2009-HEAD04-PRINT-SEC          SECTION.
      *
           IF      FLG-KIKAN           =   1
      *        経過措置対象なし
               IF     (WRK-PARA-KEIKAYMD   NOT =   SPACE )  AND
                      (FLG-KEIKA           =   ZERO  )
                   INITIALIZE                  WRK-HEN-AREA
                   MOVE    ZERO                TO  WRK-HEN-MAX
                   MOVE    ZERO                TO  FLG-KEIKA
               ELSE
      *            印刷編集処理
                   PERFORM 2100-PRINT-HENSYU-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-KIKAN
           MOVE    SPACE               TO  HEAD-04
           MOVE    "【有効期間】"      TO  H04-MIDASI
      *
           MOVE    ISET-YUKOSTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  H04-YUKOSTYMD
           MOVE    ISET-YUKOEDYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  H04-YUKOEDYMD
           MOVE    "〜"                TO  H04-KARA
      *
      *    印刷編集処理
           IF      WRK-HEN-MAX         >=  500
               PERFORM 2100-PRINT-HENSYU-SEC
           END-IF
      *
           ADD     1                   TO  WRK-HEN-MAX
           MOVE    HEAD-04             TO  WRK-TBL-REC (WRK-HEN-MAX)
           .
       2009-HEAD04-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       2002-MEISAI-HEN-SEC          SECTION.
      *
      *    経過措置日設定判定
           MOVE    ZERO                TO  FLG-KEIKA-GAI
           IF     (WRK-PARA-KEIKAYMD   NOT =   SPACE )
      *        有効終了日＜経過措置日 は対象外
               IF      (ISET-YUKOEDYMD     <=  WRK-PARA-KEIKAYMD)
                   MOVE    1               TO  FLG-KEIKA-GAI
               END-IF
           END-IF
      *
           IF      FLG-KEIKA-GAI       =   ZERO
      *    期間編集
               IF     (ISET-YUKOSTYMD      =   "00000000" )  AND
                      (ISET-YUKOEDYMD      =   "99999999" )
                   MOVE    ISET-YUKOSTYMD      TO  WRK-YUKOSTYMD
                   MOVE    ISET-YUKOEDYMD      TO  WRK-YUKOEDYMD
               ELSE
                   IF     (ISET-YUKOSTYMD      =   WRK-YUKOSTYMD ) AND
                          (ISET-YUKOEDYMD      =   WRK-YUKOEDYMD )
                       CONTINUE
                   ELSE
      *                期間印刷
                       PERFORM 2009-HEAD04-PRINT-SEC
                       MOVE    ISET-YUKOSTYMD      TO  WRK-YUKOSTYMD
                       MOVE    ISET-YUKOEDYMD      TO  WRK-YUKOEDYMD
                   END-IF
               END-IF
      *
               MOVE    ISET-YUKOSTYMD      TO  CSV-YUKOSTYMD
               MOVE    ISET-YUKOEDYMD      TO  CSV-YUKOEDYMD
               PERFORM 20020-MEI-HENSYU-SEC
           END-IF
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-INPUTSET-READ-SEC
      *
           .
       2002-MEISAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20020-MEI-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE               TO  BODY-01
           MOVE    SPACE               TO  BODY-01N
           MOVE    SPACE               TO  CSV-BODY-AREA
      *
           MOVE    SPACE               TO  WRK-NAME
           MOVE    SPACE               TO  WRK-TNS-KEIKASOCHIYMD
           MOVE    SPACE               TO  WRK-KEIKAYMD-G
           MOVE    ISET-INPUTCD        TO  B01-SRYCD
                                           CSV-SRYCD
           EVALUATE    ISET-INPUTCD(1:1)
               WHEN    "S"
      *            約束セット
                   PERFORM 20021-YAKUSOKU-HEN-SEC
               WHEN    "."
      *            診療種別
                   PERFORM 20022-SRYSYUKBN-HEN-SEC
               WHEN    OTHER
      *            点数マスタ
                   PERFORM 20023-SRYCD-HEN-SEC
           END-EVALUATE
      *    器材商品コード
           IF      ISET-INPUTCD(1:3)   =   "058"
               MOVE    1                   TO  FLG-KIZAI
      *ver.4.7 単位は器材コードで表示
               MOVE    WRK-TNS-TANINAME    TO  WRK-SYO-TANINAME
           ELSE
               MOVE    ZERO                TO  FLG-KIZAI
           END-IF
      *
           .
       20020-MEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    約束セット処理
      *****************************************************************
       20021-YAKUSOKU-HEN-SEC          SECTION.
      *
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
      **** MOVE    ISET-SETCD          TO  ICD-INPUTCD
           MOVE    ISET-INPUTCD        TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
               INITIALIZE                      INPUTCD-REC
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTCD         =   ZERO
      *        表示名
      *****    MOVE    ICD-DSPNAME         TO  B01N-NAME
      *        セット名称　編集
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    ICD-DSPNAME         TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  B01N-NAME
                                               CSV-NAME
      *
           END-IF
      *    回数
           IF     (ISET-KAISU              >   1     )
               MOVE    ISET-KAISU          TO  B01-KAISU
                                               CSV-KAISU
               MOVE    "×"                TO  B01-KAKERU
               MOVE    WRK-HEN-KAISU       TO  B01-KAISU-HEN
           END-IF
      *
           PERFORM 20024-TBL-HEN01-SEC
      *
           PERFORM 420-CSV-BODYSET-SEC
           .
       20021-YAKUSOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別処理
      *****************************************************************
       20022-SRYSYUKBN-HEN-SEC          SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-NAME
                   WHEN    ISET-INPUTCD(2:3)   =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-NAME
           END-SEARCH 
      *
           MOVE    WRK-NAME            TO  B01N-NAME
                                           CSV-NAME
      *
           PERFORM 20024-TBL-HEN01-SEC
      *
           PERFORM 420-CSV-BODYSET-SEC
           .
       20022-SRYSYUKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   １行編集処理
      *****************************************************************
       20024-TBL-HEN01-SEC          SECTION.
      *
           INSPECT B01N-YOBI1          REPLACING  ALL "  "  BY  "　"
           INSPECT B01N-YOBI2          REPLACING  ALL "  "  BY  "　"
           INSPECT B01N-ZUMIFLG        REPLACING  ALL "  "  BY  "　"
           INSPECT B01N-NAME           REPLACING  ALL "  "  BY  "　"
      *
           IF      WRK-HEN-MAX         >=  500
      *        印刷編集処理
               PERFORM 2100-PRINT-HENSYU-SEC
           END-IF
      *
           ADD     1                   TO  WRK-HEN-MAX
           MOVE    BODY-01             TO  WRK-TBL-REC  (WRK-HEN-MAX)
           MOVE    BODY-01N            TO  WRK-TBL-REC2 (WRK-HEN-MAX)
           MOVE    SPACE               TO  BODY-01
           MOVE    SPACE               TO  BODY-01N
           .
       20024-TBL-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療コード処理
      *****************************************************************
       20023-SRYCD-HEN-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    ISET-INPUTCD        TO  TNS-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK-KEIKA
           PERFORM UNTIL    (FLG-TENSU     =   1  )
      ****              OR  (FLG-OK        =   1  )
      *H26.4
            IF     FLG-OK          =   1
      *        経過措置日は最後の履歴内容
               MOVE    TNS-KEIKASOCHIYMD   TO  WRK-TNS-KEIKASOCHIYMD
               IF     (WRK-PARA-KEIKAYMD   NOT =   SPACE )   AND
                      (TNS-SRYCD (1:1)         =   "6"   )
                   IF     (WRK-PARA-KEIKAYMD   <   TNS-KEIKASOCHIYMD)
                      OR  (TNS-KEIKASOCHIYMD   =   ZERO
                                               OR  SPACE    )
                       MOVE    ZERO                TO  FLG-OK-KEIKA
                   END-IF
      *            経過措置日に有効
                   IF    ((TNS-YUKOSTYMD       <=  WRK-PARA-KEIKAYMD )
                     AND  (TNS-YUKOEDYMD       >=  WRK-PARA-KEIKAYMD ))
                     AND  (WRK-PARA-KEIKAYMD   =   TNS-KEIKASOCHIYMD)
                       MOVE    1               TO  FLG-OK-KEIKA
                   END-IF
               END-IF
            ELSE
      *
               MOVE    TNS-NAME            TO  WRK-NAME
      *!
      *        検査正式名称
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60")  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    SPACE               TO  WRK-NAME
                   STRING  TNS-NAME        DELIMITED  BY  SPACE
                           "　【"          DELIMITED  BY  SIZE
                           TNS-FORMALNAME  DELIMITED  BY  SPACE
                           "】"            DELIMITED  BY  SIZE
                                           INTO  WRK-NAME
                   END-STRING
               END-IF
      *!
      *        器材商品名コードの器材コード
               IF     (FLG-KIZAI           =   1   )  AND
                      (TNS-SRYCD(1:1)      =   "7" )
                   MOVE    SPACE               TO  WRK-NAME
                   STRING  "〔"            DELIMITED  BY  SIZE
                           TNS-NAME        DELIMITED  BY  SPACE
                           "〕"            DELIMITED  BY  SIZE
                                           INTO  WRK-NAME
                   END-STRING
      *ver.4.7
      *            商品名コードの単位へ
                   IF      WRK-SYO-TANINAME    NOT =   SPACE
                       MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                       MOVE    SPACE               TO  WRK-SYO-TANINAME
                   END-IF
               END-IF
      *
               IF     (TNS-YUKOSTYMD       <=  SPA-SRYYMD )  AND
                      (TNS-YUKOEDYMD       >=  SPA-SRYYMD )
      *            診療日対象分
                   MOVE    1               TO  FLG-OK
               END-IF
      *        経過措置日設定あり
               IF     (WRK-PARA-KEIKAYMD   NOT =   SPACE )   AND
                      (TNS-SRYCD (1:1)         =   "6"   )   AND
                      (TNS-KEIKASOCHIYMD   NOT =   SPACE
                                               AND ZERO  )
                   IF     (TNS-YUKOSTYMD   <=  WRK-PARA-KEIKAYMD ) AND
                          (TNS-YUKOEDYMD   >=  WRK-PARA-KEIKAYMD )
      *                経過措置日に有効
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            経過措置日判定
                   IF      WRK-PARA-KEIKAYMD   =   TNS-KEIKASOCHIYMD
      *************    MOVE    1               TO  FLG-KEIKA
                       MOVE    1               TO  FLG-OK-KEIKA
                   END-IF
               END-IF
      *        経過措置日
      *        IF      TNS-KEIKASOCHIYMD   =   SPACE  OR  ZERO
      *            CONTINUE
      *        ELSE
                   MOVE    TNS-KEIKASOCHIYMD   TO  WRK-TNS-KEIKASOCHIYMD
      *        END-IF
      *
               MOVE    TNS-KZMCOMPSIKIBETU TO  WRK-TNS-KZMCOMPSIKIBETU
               MOVE    TNS-TANICD          TO  WRK-TNS-TANICD
               MOVE    TNS-TANINAME        TO  WRK-TNS-TANINAME
               MOVE    TNS-DATAKBN         TO  WRK-TNS-DATAKBN
      *
            END-IF
      *
      *
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *H26.4
      *    経過措置日が設定日と一致
           IF      FLG-OK-KEIKA        =   1
               MOVE    1               TO  FLG-KEIKA
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "削"            TO  B01N-ZUMIFLG
                                           CSV-ZUMIFLG
           ELSE
      *        経過措置日判定
               IF     FLG-OK-KEIKA         =   1
                   MOVE    "経"            TO  B01N-ZUMIFLG
                                               CSV-ZUMIFLG
               END-IF
           END-IF
      *
           IF      ISET-COMMENT    NOT =   SPACE
               MOVE    ISET-COMMENT        TO  WRK-NAME
           END-IF
      *    名称編集
           MOVE    WRK-NAME            TO  B01N-NAME
                                           CSV-NAME
           IF      WRK-NAME(61:)       =   SPACE
               CONTINUE
           ELSE
      *        １行編集
               PERFORM 20024-TBL-HEN01-SEC
               IF      WRK-NAME(81:)   NOT =   SPACE
                   MOVE    WRK-NAME(81:)       TO  B01N-NAME
               END-IF
           END-IF
      *
      *    経過措置日
           IF      WRK-TNS-KEIKASOCHIYMD   =   SPACE
                                           OR  ZERO
               MOVE    SPACE               TO  WRK-KEIKAYMD-G
           ELSE
               MOVE    SPACE               TO  WRK-KEIKAYMD-G
               MOVE    WRK-TNS-KEIKASOCHIYMD   TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-KEIKAYMD
               MOVE    "【経過措置日】"    TO  WRK-KEIKA-H
               PERFORM 200231-SRYCDCHG-CHK-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HEN-SURYO
           MOVE    SPACE               TO  WRK-HEN-SURYO-2
           MOVE    SPACE               TO  WRK-HEN-SURYO-3
      *    数量（薬剤・器材）
           IF     (ISET-INPUTCD(1:1)   =   "6"  OR  "7" )  OR
                 ((ISET-INPUTCD(1:1)   NOT =   "1"   )  AND
                  (ISET-SURYO1         NOT =   1     )  )
              OR  (ISET-INPUTCD(1:3)   =   "059"     )
               MOVE    ISET-SURYO1         TO  B01-SURYO1
                                               CSV-SURYO1
               MOVE    WRK-HEN-SURYO       TO  B01-SURYO-HEN
               MOVE    WRK-TNS-TANINAME    TO  B01N-TANINAME
                                               CSV-TANINAME
           ELSE
      *        診療コード
               IF      (ISET-INPUTCD(1:1)      =   "1"  ) AND
                       (ISET-SURYO1        NOT =   1    ) AND
                       (WRK-TNS-KZMCOMPSIKIBETU
                                           NOT =  ZERO )
                   MOVE    ISET-SURYO1         TO  B01-SURYO3
                                                   CSV-SURYO1
                   MOVE    WRK-HEN-SURYO-3     TO  B01-SURYO-HEN
                   MOVE    WRK-TNS-TANINAME    TO  B01N-TANINAME3
                                                   CSV-TANINAME
               END-IF
           END-IF
      *
      *    分画数（フィルムのみ）
           IF     (ISET-INPUTCD(1:1)   =   "7" )  AND
                  (ISET-SURYO2         >   ZERO)  AND
                  (WRK-TNS-DATAKBN     =   "3" )
               MOVE    ISET-SURYO1         TO  B01-SURYO12
                                               CSV-SURYO1
               MOVE    ISET-SURYO2         TO  B01-SURYO22
                                               CSV-SURYO2
               MOVE    WRK-HEN-SURYO-2     TO  B01-SURYO-HEN
               MOVE    SPACE               TO  B01N-TANINAME
                                               CSV-TANINAME
           END-IF
      *    乳幼児加算・酸素補正率は数量記載なし
           IF     (ISET-INPUTCD        =   "799990070" 
                                       OR  "770020070" )
               MOVE    SPACE               TO  B01-HENSYU
               MOVE    SPACE               TO  B01N-TANINAME
           END-IF
      *    その他
           IF      ISET-ATAI-AREA      NOT =   SPACE
      *        用法コメント
               IF      ISET-INPUTCD(1:3)   =   "001" 
                   MOVE    ZERO            TO  FLG-OVER
                   MOVE    SPACE           TO  B01-HENSYU-I
                   STRING  ISET-ATAI1      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI2      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI3      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI4      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI5      DELIMITED  BY  SPACE
                           INTO                    B01-HENSYU-I
                           ON  OVERFLOW
                               MOVE   1            TO  FLG-OVER
                   END-STRING
                   IF      FLG-OVER        =   1
                       MOVE    SPACE           TO  B01-HENSYU-I
                       MOVE    SPACE           TO  B01-HENSYU-5
                       STRING
                           ISET-ATAI1      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI2      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI3      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI4      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI5      DELIMITED  BY  SPACE
                           INTO                    B01-HENSYU-5
                       END-STRING
                   END-IF
               ELSE
                   MOVE    SPACE           TO  B01-HENSYU-I
                   STRING  ISET-ATAI1      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI2      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI3      DELIMITED  BY  SPACE
                           ","             DELIMITED  BY  SIZE
                           ISET-ATAI4      DELIMITED  BY  SPACE
                           INTO                    B01-HENSYU-I
                   END-STRING
               END-IF
               MOVE    SPACE           TO  CSV-BIKOU
               STRING  ISET-ATAI1      DELIMITED  BY  SPACE
                       "  "            DELIMITED  BY  SIZE
                       ISET-ATAI2      DELIMITED  BY  SPACE
                       "  "            DELIMITED  BY  SIZE
                       ISET-ATAI3      DELIMITED  BY  SPACE
                       "  "            DELIMITED  BY  SIZE
                       ISET-ATAI4      DELIMITED  BY  SPACE
                       "  "            DELIMITED  BY  SIZE
                       ISET-ATAI5      DELIMITED  BY  SPACE
                                       INTO    CSV-BIKOU
               END-STRING
           END-IF
      *
      *    換算値
           MOVE    SPACE               TO  WRK-HEN-INPUTHEN
           IF      ISET-KANSURYO       >   ZERO
               MOVE    ISET-KANSURYO       TO  WRK-SURYO
               PERFORM 810-SURYO-HEN-SEC
               STRING  "("
                       WRK-SURYO-HEN        DELIMITED  BY  SPACE
                       " A)"                DELIMITED  BY  SIZE
                                            INTO    WRK-HEN-INPUTHEN
               END-STRING
           END-IF
           IF      ISET-INPUTKBN       =   "1"
               MOVE    "(C)"                TO  B01-INPUTKBN
           END-IF
           IF      ISET-INPUTKBN       =   "2"
               MOVE    "(I)"                TO  B01-INPUTKBN
           END-IF
           IF      WRK-HEN-INPUTHEN (10:)   =   SPACE
               IF      ISET-ATAI-AREA           =   SPACE
                   MOVE    WRK-HEN-INPUTHEN     TO  B01-INPUT-HEN
               END-IF
           ELSE
      *        １行編集
               PERFORM 20024-TBL-HEN01-SEC
               MOVE    WRK-HEN-INPUTHEN     TO  B01-KANSURYO-HEN
           END-IF
      *    経過措置日
           IF      WRK-KEIKAYMD-G  NOT =   SPACE
      *        １行編集
               PERFORM 20024-TBL-HEN01-SEC
      *        １行編集
               MOVE    WRK-KEIKAYMD-G      TO  B01-HENSYU-4
                                               CSV-BIKOU
               PERFORM 20024-TBL-HEN01-SEC
               MOVE    SPACE           TO  WRK-KEIKAYMD-G
           END-IF
      *    回数
           IF     (ISET-KAISU              >   1     )
             OR  ((ISET-INPUTCD(1:3)       =   "001" )  AND
                  (ISET-KAISU              >   ZERO  ))
               MOVE    ISET-KAISU          TO  B01-KAISU
                                               CSV-KAISU
               MOVE    "×"                TO  B01-KAKERU
               IF      B01N-TANINAME       =   SPACE
                   MOVE    WRK-HEN-KAISU       TO  B01-KAISU-HEN
               ELSE
      *           １行編集
                   PERFORM 20024-TBL-HEN01-SEC
                   MOVE    WRK-HEN-KAISU       TO  B01-KAISU-HEN
               END-IF
           END-IF
      *    １行編集
           IF     (BODY-01             =   SPACE )  AND
                  (BODY-01N            =   SPACE )
               CONTINUE
           ELSE
               PERFORM 20024-TBL-HEN01-SEC
           END-IF
      *
           PERFORM 420-CSV-BODYSET-SEC
           .
       20023-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    経過措置コード判定処理
      *****************************************************************
       200231-SRYCDCHG-CHK-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    ISET-INPUTCD        TO  CHG-IPNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYCDCHG        =   ZERO
               MOVE    CHG-RJNSRYCD          TO  WRK-CHNGCD
               MOVE    "　移行コード："      TO  WRK-KEIKA-H2
           END-IF
      *
           .
       200231-SRYCDCHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    数量変換編集処理
      *****************************************************************
       810-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    SPACE               TO  WRK-SURYO-HEN
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               IF      WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1  BY  1
                   UNTIL       IDM     >   11
               IF      WRK-SURYO-X(IDM:1)  NOT =   SPACE
                   ADD     1               TO  IDM2
                   MOVE    WRK-SURYO-X(IDM:1)  TO  WRK-SURYO-HEN(IDM2:1)
               END-IF
           END-PERFORM
           .
       810-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
                                                    OR   ZERO
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       900-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
               MOVE    ZERO                TO  FLG-END
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-INPUTSET-READ-EXT.
           EXIT.
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC             SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
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
           MOVE   "HCPOM2.red"         TO  SPRT-PRTID
           MOVE    HCPOM2              TO  SPRT-PRTDATA
           MOVE    "セット登録一覧"    TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX-P
           MOVE    SPACE               TO  HCPOM2
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       410-CSV-HEAD-SEC            SECTION.
      *
           MOVE    SPACE               TO  RECECSV-REC
           STRING  "セットコード"          DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "セット名"              DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "有効開始日"            DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "有効終了日"            DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "状態"                  DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "入力コード"            DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "名称"                  DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "数量"                  DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "回数（分画等）"        DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "単位"                  DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "日数・回数"            DELIMITED BY SIZE
                   ","                     DELIMITED BY SIZE
                   "備考"                  DELIMITED BY SIZE
                                           INTO    RECECSV-REC
           END-STRING
           WRITE   RECECSV-REC
           .
       410-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       420-CSV-BODYSET-SEC       SECTION.
      *
           MOVE    SPACE               TO  RECECSV-REC
           STRING  CSV-SETCD               DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-DSPNAME             DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-YUKOSTYMD           DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-YUKOEDYMD           DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-ZUMIFLG             DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-SRYCD               DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-NAME                DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-SURYO1              DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-SURYO2              DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-TANINAME            DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-KAISU               DELIMITED BY SPACE
                   ","                     DELIMITED BY SIZE
                   CSV-BIKOU               DELIMITED BY SIZE
                                           INTO    RECECSV-REC
           END-STRING
           WRITE   RECECSV-REC
           .
       420-CSV-BODYSET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       430-CSV-OUTSET-SEC       SECTION.
      *
           OPEN    INPUT               RECECSV-FILE
      *
           INITIALIZE                  RECECSV-REC
           PERFORM 900-RECECSV-NEXT-SEC
      *
           PERFORM UNTIL FLG-RECECSV   =   1
               PERFORM 490-CSV-OUT-SEC
               PERFORM 900-RECECSV-NEXT-SEC
           END-PERFORM
      *
           CLOSE                       RECECSV-FILE
           .
       430-CSV-OUTSET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       490-CSV-OUT-SEC             SECTION.
      *
           INITIALIZE                      ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    SPACE               TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    RECECSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "セット登録一覧ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"         USING
                                           ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       490-CSV-OUT-EXT.
           EXIT.
      *****************************************************************
      *    CSVファイル順読込
      *****************************************************************
       900-RECECSV-NEXT-SEC            SECTION.
      *
           READ    RECECSV-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-RECECSV
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECECSV
           END-READ
           .
       900-RECECSV-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      PCNT            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
      *
           END-IF
      *
           DISPLAY "*** ORCBGSETLST END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    PCNT                TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT                   RECEERR-FILE
      *
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT                  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBGSETLST "          DELIMITED  BY  SIZE
                   WRK-RECEERR             DELIMITED  BY  SIZE
                   LOW-VALUE               DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
      *
           CALL    "cobabort"              USING
                                           WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.