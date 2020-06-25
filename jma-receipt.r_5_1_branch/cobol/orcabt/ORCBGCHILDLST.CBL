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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBGCHILDLST.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 出産育児一時金チェックリスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/09/9     NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-太田    10/01/29  拡張漢字対応
      *  04.05.01    NACL-藤原    10/07/07  ２５日請求対応
      *
      *  04.06.01    NACL-藤原    13/06/05  退職国保の保険者番号を記載する
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  BUNBENERR-FILE  ASSIGN  BUNBENERR
                                   FILE    STATUS  IS  STS-BUNBENERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  BUNBENERR-FILE.
       01  BUNBENERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //BUNBENERR//.
      *
           COPY    "HCMSL55.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BUNBENERR       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-TEISYUTUSAKI
                                   PIC X(01).
               05  KEY-N-SKYKBN    PIC X(01).
               05  KEY-N-HKNJANUM  PIC X(08).
               05  KEY-N-BUNBENKBN PIC X(01).
           03  KEY-OLD.
               05  KEY-O-TEISYUTUSAKI
                                   PIC X(01).
               05  KEY-O-SKYKBN    PIC X(01).
               05  KEY-O-HKNJANUM  PIC X(08).
               05  KEY-O-BUNBENKBN PIC X(01).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-OCC             OCCURS  3.
               05  SUM-CNT         PIC 9(03).
               05  SUM-MONEY       PIC 9(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SKYYM      PIC X(06).
           03  WRK-PARA-SKYKBN     PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-BUNBENERR       PIC X(200).
      *
           03  WRK-TEISYUTUSAKI    PIC X(01).
      *
           03  WRK-SRYYM           PIC X(06).
           03  WRK-SKYYM           PIC X(16).
           03  WRK-SKYYMH          PIC X(05).
           03  WRK-BIRTHDAYH       PIC X(07).
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZZZZZ9        PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    印刷内容
       01  HEAD-01.
           03  FILLER                  PIC X(20)   VALUE   SPACE.
           03  H01-SRYYM               PIC X(16).
           03  H01-SRYYM-TITLE         PIC X(08).
           03  FILLER                  PIC X(28)   VALUE
               "出産育児一時金チェックリスト".
           03  H01-TEISYUTUSAKI        PIC X(10).
           03  H01-SKYKBN              PIC X(16).
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC ZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE
               "保険者番号　".
           03  FILLER                  PIC X(10)   VALUE
               "分娩区分　".
           03  FILLER                  PIC X(22)   VALUE
               "患者番号　　　　　　　".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(36)   VALUE
               "氏名".
           03  FILLER                  PIC X(12)   VALUE
               "出産年月日".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE
               "請求日　". 
           03  FILLER                  PIC X(09)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "代理受取額".
      *
       01  BODY-01.
           03  B01-FIL1                PIC X(2).
           03  B01-HKNJANUM            PIC X(10).
           03  B01-BUNBENKBN           PIC X(12).
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(40).
           03  B01-FIL2                PIC X(01).
           03  B01-BIRTHDAY            PIC X(09).
           03  B01-FIL3                PIC X(03).
           03  B01-TEISYUTUBIKBN       PIC X(08).
           03  B01-FIL4                PIC X(09).
           03  B01-DAIRI-MONEY         PIC X(10).
      *
       01  BODY-02.
           03  FILLER                  PIC X(28)   VALUE   SPACE.
           03  B02-TITLE               PIC X(20).
           03  FILLER                  PIC X(06)   VALUE   "件数：".
           03  B02-KENSU               PIC X(03).
           03  FILLER                  PIC X(18)   VALUE
               "　代理受取額合計：".
           03  B02-GOKEI               PIC X(10).
      *
       01  WRK-CONST-AREA.
            03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   55.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           MOVE    LOW-VALUE       TO  KEY-AREA
      *
           INITIALIZE                  BUNBENERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       BUNBENERR
                                       WRK-PARA-SKYYM
                                       WRK-PARA-SKYKBN
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-SKYYM =" WRK-PARA-SKYYM
           DISPLAY "WRK-PARA-SKYKBN=" WRK-PARA-SKYKBN
      *
           IF      WRK-PARA-SKYKBN =   SPACE
               MOVE    "0"             TO  WRK-PARA-SKYKBN
           END-IF
           IF      WRK-PARA-SKYYM  =   SPACE
               MOVE    "000000"        TO  WRK-PARA-SKYYM
               MOVE    "0"             TO  WRK-PARA-SKYKBN
           END-IF
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBGCHILDLST.CBL"
                                   TO  JOB-PGID
           MOVE    "出産育児一時金チェックリスト"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BUNBENERR       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BUNBENERR
      *
      *    請求年月編集
           IF      WRK-PARA-SKYYM  =   "000000"
               MOVE    "請求年月設定なし"
                                       TO  H01-SRYYM
               MOVE    "　　　"        TO  H01-SRYYM-TITLE
           ELSE     
               MOVE    WRK-PARA-SKYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3(1:16)
                                       TO  H01-SRYYM
               MOVE    "請求分"        TO  H01-SRYYM-TITLE
           END-IF
      *
      *    作成日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
           IF      WRK-PARA-SKYKBN =   "0" OR  "1" OR  "2" OR  "3"
               CONTINUE
           ELSE
               MOVE    "請求区分の設定が誤っています" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    分娩管理読込
           MOVE    "%"             TO  WRK-TEISYUTUSAKI
           PERFORM 900-BUNBEN-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  SUM-AREA
           ADD     1               TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    ZERO            TO  IDX
           MOVE    KEY-NEW         TO  KEY-OLD
      *
           EVALUATE    KEY-N-TEISYUTUSAKI
               WHEN    SPACE
                   MOVE    "　　　　"          TO  H01-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "（社保）"          TO  H01-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    "（国保）"          TO  H01-TEISYUTUSAKI
           END-EVALUATE
      *
      *    支払機関別編集処理
           PERFORM          UNTIL   FLG-END        =   1
                            OR      KEY-N-TEISYUTUSAKI
                                               NOT =
                                                     KEY-O-TEISYUTUSAKI
      *        請求区分別編集処理
               MOVE    KEY-NEW         TO  KEY-OLD
               INITIALIZE                  SUM-OCC (2)
      *
               EVALUATE    KEY-N-SKYKBN
                   WHEN    SPACE
                       MOVE    "　　　　　　　"    TO  H01-SKYKBN
                   WHEN    "0"
                       MOVE    "【請求しない】"    TO  H01-SKYKBN
                   WHEN    "1"
                       MOVE    "【未請求】　　"    TO  H01-SKYKBN
                   WHEN    "2"
                       MOVE    "【請求済】　　"    TO  H01-SKYKBN
               END-EVALUATE
               PERFORM 2001-HEAD-PRINT-SEC
               PERFORM             UNTIL   FLG-END        =   1
                                   OR      KEY-N-TEISYUTUSAKI
                                                      NOT =
                                                   KEY-O-TEISYUTUSAKI
                                   OR      KEY-N-SKYKBN
                                                      NOT =
                                                   KEY-O-SKYKBN
      *            分娩区分・保険者別編集処理
                   INITIALIZE                  SUM-OCC (1)
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM          UNTIL   FLG-END        =  1
                                    OR      KEY-NEW    NOT = KEY-OLD
      *                明細編集処理
                       PERFORM 2002-BODY-PRINT-SEC
                   END-PERFORM
      *
      *            分娩区分・保険者別合計編集処理
                   MOVE    1                TO  IDX
                   PERFORM 2003-TERM-PRINT-SEC
               END-PERFORM
      *
      *        請求区分別合計編集処理
               MOVE    2                TO  IDX
               PERFORM 2003-TERM-PRINT-SEC
           END-PERFORM
      *
      *    支払機関別合計編集処理
           MOVE    3                TO  IDX
           PERFORM 2003-TERM-PRINT-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2001-HEAD-PRINT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL55
           MOVE    HEAD-01             TO  HCMSL55-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL55-MOJI (3)
      *
           MOVE    4                   TO  CNT-LINE
           .
       2001-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧作成処理
      *****************************************************************
       2002-BODY-PRINT-SEC          SECTION.
      *
           MOVE    SPACE               TO  BODY-01
      *    保険者番号
           MOVE    BUNBEN-HKNJANUM     TO  B01-HKNJANUM
      *    分娩区分
           EVALUATE    BUNBEN-BUNBENKBN
               WHEN    "1"
                   MOVE    "　正常　"      TO  B01-BUNBENKBN
               WHEN    "2"
                   MOVE    "　異常　"      TO  B01-BUNBENKBN
           END-EVALUATE
      *
           PERFORM          UNTIL   FLG-END        =   1
                            OR      KEY-NEW    NOT =   KEY-OLD
      *
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
      *            帳票印刷処理
                   PERFORM 2004-PRINT-OUT-SEC
      *            見出し編集処理
                   PERFORM 2001-HEAD-PRINT-SEC
               END-IF
      *
      *        患者番号
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    BUNBEN-PTID         TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  B01-PTNUM
      *
      *****    MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *****    MOVE    BUNBEN-PTID         TO  PTINF-PTID
      *****    MOVE    PTINF-REC           TO  MCPDATA-REC
      *****    PERFORM 900-PTINF-READ-SEC
               MOVE    BUNBEN-NAME         TO  B01-NAME
               INSPECT B01-NAME            REPLACING  ALL "  "  BY  "　"
               IF    ( SPA-ENCODING    =   2 )
                   INITIALIZE              ORCSKANACHK2AREA
                   MOVE    "1"         TO  KANACHK2-SYORI
                   MOVE    B01-NAME    TO  KANACHK2-MAE-INPUT
                   CALL    "ORCSKANACHK2"  USING
                                           ORCSKANACHK2AREA
                   MOVE    KANACHK2-MAE-INPUT
                                       TO  B01-NAME
               END-IF
      *
               MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1    TO  B01-BIRTHDAY
      *
           EVALUATE    BUNBEN-TEISYUTUBIKBN
               WHEN    "1"
                   MOVE    "１０日　"      TO  B01-TEISYUTUBIKBN
               WHEN    "2"
                   MOVE    "２５日　"      TO  B01-TEISYUTUBIKBN
           END-EVALUATE
      *
               MOVE    BUNBEN-MONEY (12)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  B01-DAIRI-MONEY
      *
               ADD     1               TO  CNT-LINE
               MOVE    BODY-01         TO  HCMSL55-MOJI (CNT-LINE)
      *
               MOVE    SPACE           TO  BODY-01
               MOVE    "　　　　"      TO  B01-BUNBENKBN
      *
               ADD     1               TO  CNT-OUT
      *
      *        合計集計処理
               ADD     1               TO  SUM-CNT (1)
                                           SUM-CNT (2)
                                           SUM-CNT (3)
               ADD     BUNBEN-MONEY (12)
                                       TO  SUM-MONEY (1)
                                           SUM-MONEY (2)
                                           SUM-MONEY (3) 
      *
      *        分娩管理読込   
               PERFORM 900-BUNBEN-READ-N-SEC
           END-PERFORM
           .
       2002-BODY-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       2003-TERM-PRINT-SEC                SECTION.
      *
      *    請求年月指定なしのとき総合計以外は編集しない 
           IF    ( IDX                <   3        )
           AND   ( WRK-PARA-SKYYM     =   "000000" ) 
               GO  TO  2003-TERM-PRINT-EXT
           END-IF
      *    
           EVALUATE    IDX
               WHEN    1
                   MOVE    "　　　　　　　　　　"
                                           TO  B02-TITLE
                   ADD     1               TO  CNT-LINE
               WHEN    2
                   MOVE    "【請求区分別合計】　"
                                           TO  B02-TITLE
               WHEN    3
                   IF      WRK-PARA-SKYYM      =   "000000" 
                       MOVE    "【合　　　　計】　　"
                                                   TO  B02-TITLE
                       ADD     1                   TO  CNT-LINE
                   ELSE
                       MOVE    "【提出先別合計】　　"
                                                   TO  B02-TITLE
                   END-IF
           END-EVALUATE
      *
           MOVE    SUM-CNT   (IDX) TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  B02-KENSU
           MOVE    SUM-MONEY (IDX) TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  B02-GOKEI
      *
           IF      CNT-LINE            >=  WRK-CONS-LINE-MAX
      *        帳票印刷処理
               PERFORM 2004-PRINT-OUT-SEC
      *        見出し編集処理
               PERFORM 2001-HEAD-PRINT-SEC
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-02         TO  HCMSL55-MOJI (CNT-LINE)
      *
           EVALUATE    IDX
               WHEN    1
      *            保険者番号・分娩区分別合計の後に１行空ける
                   IF      KEY-N-SKYKBN     =   KEY-O-SKYKBN
                       ADD     1                TO  CNT-LINE
                   END-IF
               WHEN    2
                   IF      KEY-N-TEISYUTUSAKI
                                            =   KEY-O-TEISYUTUSAKI
      *                帳票印刷処理
                       PERFORM 2004-PRINT-OUT-SEC
                   END-IF
               WHEN    3
      *            帳票印刷処理
                   PERFORM 2004-PRINT-OUT-SEC
           END-EVALUATE
           .
      *
       2003-TERM-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2004-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
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
           MOVE   "HCMSL55.red"        TO  SPRT-PRTID
           MOVE    HCMSL55             TO  SPRT-PRTDATA
           EVALUATE    KEY-O-TEISYUTUSAKI
               WHEN    SPACE
                   MOVE
                       "（請求年月なし）出産育児一時金チェックリスト"
                                           TO  SPRT-TITLE
               WHEN    "1"
                   MOVE
                       "（社保）出産育児一時金チェックリスト"
                                           TO  SPRT-TITLE
               WHEN    "2"
                   MOVE
                       "（国保）出産育児一時金チェックリスト"
                                           TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2004-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               BUNBENERR-FILE
           IF        ( STS-BUNBENERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT            BUNBENERR-FILE
      *
               MOVE    WRK-BUNBENERR     TO  BUNBENERR-REC
               WRITE   BUNBENERR-REC
               CLOSE   BUNBENERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-BUNBENERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBGCHILDLST.CBL "
                                   DELIMITED  BY  SIZE
                   WRK-BUNBENERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           DISPLAY "*** ORCBGCHILDLST IN   "  CNT-IN
           DISPLAY "*** ORCBGCHILDLST OUT  "  CNT-OUT
           DISPLAY "*** ORCBGCHILDLST PAGE "  CNT-PAGE
           DISPLAY "*** ORCBGCHILDLST END ***"
      *
           IF      CNT-IN          =   ZERO
               MOVE    "対象患者がありませんでした"
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩管理読込
      *****************************************************************
       900-BUNBEN-READ-SEC             SECTION.
      * 
           INITIALIZE                  BUNBEN-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  BUNBEN-HOSPNUM
           MOVE    WRK-PARA-SKYYM  TO  BUNBEN-SKYYM
           MOVE    WRK-TEISYUTUSAKI
                                   TO  BUNBEN-TEISYUTUSAKI
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-BUNBEN-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-BUNBEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩管理読込
      *****************************************************************
       900-BUNBEN-READ-N-SEC           SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  BUNBEN-REC
               IF      BUNBEN-SKYKBN   =   SPACE
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    BUNBEN-PTID     TO  PTINF-PTID
               MOVE    PTINF-REC       TO  MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      PTINF-TSTPTNUMKBN   =   "1"
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               EVALUATE    WRK-PARA-SKYKBN
                   WHEN    "1"
                       IF      BUNBEN-SKYKBN   =   "2"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
                   WHEN    "2"
                       IF      BUNBEN-SKYKBN   =   "1"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
                   WHEN    "3"
                       IF      BUNBEN-SKYKBN   =   "0"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
               END-EVALUATE
      *
               IF      WRK-PARA-SKYYM  =   "000000"
                   MOVE    SPACE           TO  KEY-N-SKYKBN
                                               KEY-N-TEISYUTUSAKI
                                               KEY-N-BUNBENKBN
                                               KEY-N-HKNJANUM
               ELSE
                   MOVE    BUNBEN-SKYKBN   TO  KEY-N-SKYKBN
                   MOVE    BUNBEN-TEISYUTUSAKI
                                           TO  KEY-N-TEISYUTUSAKI
                   MOVE    BUNBEN-BUNBENKBN
                                           TO  KEY-N-BUNBENKBN
      *********    MOVE    BUNBEN-HKNJANUM-S
                   MOVE    BUNBEN-HKNJANUM TO  KEY-N-HKNJANUM    
               END-IF
               ADD     1               TO  CNT-IN
      *  
               DISPLAY "PTNUM="         BUNBEN-PTNUM
                       " TEISYUTUSAKI=" BUNBEN-TEISYUTUSAKI
                       " SKYKBN="       BUNBEN-SKYKBN
                       " BUNBENKBN="    BUNBEN-BUNBENKBN
                       " HKNJANUM="     BUNBEN-HKNJANUM-S
           ELSE
               MOVE    1               TO  FLG-END
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-BUNBEN-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
      *
