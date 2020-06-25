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
       PROGRAM-ID.             ORCBG021.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 未コード化病名一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/01/19    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/04/01  診療科を名称で表示する
      *  04.05.00    NACL-太田    10/01/29  拡張漢字対応
      *
      *  04.06.01    NACL-藤原    12/12/14  コード化可能病名のみ出力対応
      *                                     自院病名も対応
      *                                     診療年月までに該当する病名以
      *                                     外も対象
      *                                     補足チェック対応
      *                                     同義語変換対応
      *  04.06.02    NACL-藤原    13/02/13  自院病名の廃止病名のチェック
      *                                     条件修正
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
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMSL55.INC".
      *
           COPY    "HCMSL80.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-PAGE1           PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-OUT1            PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(02).
           03  IDY                 PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SRYYM      PIC X(06).
           03  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-CDKBN      PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-YM              PIC X(06).
           03  WRK-PTID            PIC 9(10).
           03  WRK-PTBYO-PTID      PIC 9(10).
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
      *    病名
       01  WRK-BYOMEI-REC.
           COPY    "CPBYOMEI.INC"  REPLACING  //BYO-//
                                   BY         //WRK-BYO-//.
      *
      *    患者病名一覧
       01  HEAD-01.
           03  FILLER                  PIC X(23)   VALUE   SPACE.
           03  H01-TITLE               PIC X(22).
           03  H01-SRYYM               PIC X(16).
           03  H01-SRYYM-TITLE         PIC X(32)   VALUE   SPACE.
           03  H01-PRTKBN-TITLE        PIC X(22)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC ZZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(20)   VALUE
               "患者番号　　　　　　".
      **     03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(14)   VALUE
               "氏名　　　　".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE
               "診療科　".
           03  FILLER                  PIC X(10)   VALUE
               "開始日　".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(04)   VALUE
               "入外".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "病名".
      *
       01  HEAD-03.
           03  FILLER                  PIC X(59)   VALUE   SPACE.
           03  FILLER                  PIC X(100)  VALUE
               "理由".
      *
       01  HEAD-04.
           03  FILLER                  PIC X(59)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE
               "入力コード".
      *
       01  HEAD-05.
           03  FILLER                  PIC X(59)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE
               "補足コメント".
      *
       01  BODY-01.
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(14).
           03  B01-FIL1                PIC X(01).
           03  B01-SRYKA               PIC X(06).
           03  B01-FIL2                PIC X(02).
           03  B01-SRYYMD              PIC X(10).
           03  B01-FIL3                PIC X(01).
           03  B01-NYUGAIKBN           PIC X(04).
           03  B01-FIL4                PIC X(01).
           03  B01-BYOMEI              PIC X(80).
      *
       01  BODY-02.
           03  B02-FIL1                PIC X(59).
           03  B02-RIYUU               PIC X(104).
           03  B02-RIYUU-R             REDEFINES   B02-RIYUU.
               05  B02-RIYUU1          PIC X(24).
               05  B02-FIL2            PIC X(01).
               05  B02-HAISIYMD        PIC X(09).
               05  B02-FIL3            PIC X(01).
               05  B02-BYOMEICD        PIC X(07).
               05  B02-FIL4            PIC X(01).
               05  B02-BYOMEI          PIC X(60).
               05  B02-FIL5            PIC X(01).
      *
       01  BODY-03.
           03  B03-FIL1                PIC X(59).
           03  B03-RIYUU               PIC X(24).
           03  B03-FIL2                PIC X(01).
           03  B03-HAISIYMD            PIC X(09).
           03  B03-FIL3                PIC X(01).
           03  B03-BYOMEICD            PIC X(07).
           03  B03-FIL4                PIC X(01).
           03  B03-BYOMEI              PIC X(60).
      *
       01  BODY-04.
           03  B04-FIL1                PIC X(59).
           03  B04-BYOMEIINPUTCD       PIC X(22)   OCCURS  4.
      *
       01  BODY-05.
           03  B05-FIL1                PIC X(59).
           03  B05-HOSOKU              PIC X(40).
      *
      *    自院病名一覧
       01  HEAD-011.
           03  FILLER                  PIC X(05)   VALUE   SPACE.
           03  H011-TITLE              PIC X(32).
           03  H011-TITLE1             PIC X(18).
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H011-SYSYMD             PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H011-PAGE               PIC ZZZ9.
      *
       01  HEAD-012.
           03  FILLER                  PIC X(10)   VALUE
               "入力コード".
           03  FILLER                  PIC X(04)   VALUE   SPACE.
           03  FILLER                  PIC X(06)   VALUE   "病名".
      *
       01  HEAD-013.
           03  FILLER                  PIC X(100)  VALUE   "理由".
      *
       01  HEAD-014.
           03  FILLER                  PIC X(10)   VALUE
               "　　　　　".
           03  FILLER                  PIC X(04)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE
               "補足コメント".
      *
       01  BODY-011.
           03  B011-BYOMEIINPUTCD      PIC X(10).
           03  B011-FILL               PIC X(02).
           03  B011-BYOMEI             PIC X(82).
           03  B011-BYOMEI-R           REDEFINES   B011-BYOMEI.
               05  B011-FILL           PIC X(02).
               05  B011-BYOMEI1        PIC X(80).
      *
       01  BODY-013.
           03  B013-RIYUU              PIC X(104).
           03  B013-RIYUU-R            REDEFINES   B013-RIYUU.
               05  B013-RIYUU1         PIC X(24).
               05  B013-FIL2           PIC X(01).
               05  B013-HAISIYMD       PIC X(09).
               05  B013-FIL3           PIC X(01).
               05  B013-BYOMEICD       PIC X(07).
               05  B013-FIL4           PIC X(01).
               05  B013-BYOMEI         PIC X(60).
               05  B013-FIL5           PIC X(01).
      *
       01  BODY-014.
           03  B014-RIYUU              PIC X(24).
           03  B014-FIL2               PIC X(01).
           03  B014-HAISIYMD           PIC X(09).
           03  B014-FIL3               PIC X(01).
           03  B014-BYOMEICD           PIC X(07).
           03  B014-FIL4               PIC X(01).
           03  B014-BYOMEI             PIC X(60).
      *
       01  BODY-015.
           03  FILLER                  PIC X(12).
           03  B015-HOSOKU             PIC X(24).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   55.
           03  WRK-CONS-LINE-MAX1      PIC 9(02)   VALUE   80.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    
           COPY    "CPSK1005.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    同義語マスタ
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
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
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           PERFORM 200-MAIN-SEC
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
      *
           INITIALIZE                  RECEERR
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
                                       RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-PRTKBN
                                       WRK-PARA-SYOKBN
                                       WRK-PARA-CDKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO SPA-HOSPNUM
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBG021"      TO  JOB-PGID
           MOVE    "未コード化病名一覧"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    診療年月編集
           IF      WRK-PARA-SRYYM  =   "999999" 
      *        全病名対象時
               MOVE    "1"             TO  WRK-PARA-SYOKBN
               MOVE    "0"             TO  WRK-PARA-PRTKBN
               MOVE    "99999999"      TO  WRK-SRYYMD
      *
      **         IF      WRK-PARA-CDKBN  =   "0"
      **             MOVE    "1"             TO  WRK-PARA-CDKBN
      **         END-IF
               MOVE    "全病名対象）"  TO  H01-SRYYM
               IF      WRK-PARA-CDKBN  =   "2"
                   MOVE    "【補足コメント】"
                                           TO  H01-SRYYM-TITLE
               ELSE
                   MOVE    "　　　　　　　"
                                           TO  H01-SRYYM-TITLE
               END-IF
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-PARA-SRYYM      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-SDAY (1:6)    TO  WRK-PARA-SRYYM
               ELSE
                   MOVE    "診療年月がエラーです"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
      *
               MOVE    LNK-DAY2-EDTYMD3(1:16)
                                       TO  H01-SRYYM
               IF      WRK-PARA-SYOKBN     =   "0"
                   IF      WRK-PARA-CDKBN      =   "2"
                       MOVE    "有効・診療あり）【補足コメント】"
                                               TO  H01-SRYYM-TITLE
                   ELSE
                       MOVE    "有効・診療あり）"
                                               TO  H01-SRYYM-TITLE
                   END-IF
               ELSE
                   IF      WRK-PARA-CDKBN      =   "2"
                       MOVE    "有効）【補足コメント】"
                                               TO  H01-SRYYM-TITLE
                   ELSE
                       MOVE    "有効）"        TO  H01-SRYYM-TITLE
                   END-IF
               END-IF
               MOVE    WRK-PARA-SRYYM  TO  WRK-SRYYMD (1:6)
               MOVE    "31"            TO  WRK-SRYYMD (7:2)
           END-IF
      *
           DISPLAY "SRYYM="  WRK-PARA-SRYYM
                   " PRTKBN="  WRK-PARA-PRTKBN
                   " SYOKBN=" WRK-PARA-SYOKBN 
                   " CDKBN="  WRK-PARA-CDKBN 
      *
           IF      WRK-PARA-PRTKBN =   "1"
               MOVE    "（病名ＣＤあり）"
                                       TO  H01-PRTKBN-TITLE
           END-IF
      *
           IF      WRK-PARA-CDKBN  =   "0"
               MOVE    "　未コード化病名一覧（"
                                       TO  H01-TITLE
               MOVE    "　未コード化病名一覧（自院病名）"
                                       TO  H011-TITLE
           ELSE
               MOVE    "コード化可能病名一覧（"
                                       TO  H01-TITLE
               MOVE    "コード化可能病名一覧（自院病名）"
                                       TO  H011-TITLE
               IF      WRK-PARA-CDKBN  =   "2"
                   MOVE    "【補足コメント】"
                                           TO  H011-TITLE1
               ELSE
                   MOVE    SPACE           TO  H011-TITLE1
               END-IF
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
                                       H011-SYSYMD
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
      *    患者病名の一覧作成処理
           MOVE    1               TO  FLG-PRTKBN
           COMPUTE LNK-PRTKANRI-SHORI-RENNUM
                                   =   LNK-PRTKANRI-SHORI-RENNUM
                                   +   1
      *
      *    初期値設定
           MOVE    WRK-CONS-LINE-MAX
                                   TO  CNT-LINE
           MOVE    ZERO            TO  CNT-PAGE
                                       CNT-OUT
                                       WRK-PTID
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  PTBYO-HOSPNUM
           MOVE    WRK-SRYYMD      TO  PTBYO-SRYYMD
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key33"         TO  MCP-PATHNAME
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 200-PTBYOMEI-SYORI-SEC
                                   UNTIL   FLG-PTBYOMEI =   1
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key33"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           DISPLAY "*** ORCBG021 PTBYOMEI   OUT  "  CNT-OUT
           DISPLAY "*** ORCBG021            PAGE "  CNT-PAGE
      *
      *    自院病名の一覧作成処理
           MOVE    2               TO  FLG-PRTKBN
           COMPUTE LNK-PRTKANRI-SHORI-RENNUM
                                   =   LNK-PRTKANRI-SHORI-RENNUM
                                   +   1
      *
      *    初期値設定
           MOVE    WRK-CONS-LINE-MAX1
                                   TO  CNT-LINE
           MOVE    ZERO            TO  CNT-PAGE
                                       CNT-OUT
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-USERBYOMEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 200-USERTBYOMEI-SYORI-SEC
                                   UNTIL   FLG-USERBYOMEI  =   1
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           DISPLAY "*** ORCBG021 USERBYOMEI OUT  "  CNT-OUT
           DISPLAY "*** ORCBG021            PAGE "  CNT-PAGE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名の一覧作成処理
      *****************************************************************
       200-PTBYOMEI-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
                                           FLG-NYUINACCT
           MOVE    PTBYO-PTID          TO  WRK-PTBYO-PTID
      *
           IF      WRK-PARA-SYOKBN     =   "0"
               PERFORM 2001-ACCT-CHK-SEC
           END-IF
      *
           DISPLAY "PTID=" PTBYO-PTID "#"  FLG-SRYACCT "#" FLG-NYUINACCT
      *
           IF    ( FLG-SRYACCT         =   ZERO )
           OR    ( FLG-NYUINACCT       =   ZERO )
               PERFORM         UNTIL   FLG-PTBYOMEI =   1
                               OR      PTBYO-PTID
                                           NOT =   WRK-PTBYO-PTID
      *            患者病名チェック処理
                   PERFORM 2001-PTBYO-CHK-SEC
                   IF      FLG-CHK             =   1
      *                一覧作成処理
                       PERFORM 2002-PRINT-SEC
                   END-IF
      *
                   PERFORM 900-PTBYOMEI-READ-SEC
               END-PERFORM
           ELSE
               PERFORM         UNTIL   FLG-PTBYOMEI =   1
                               OR      PTBYO-PTID
                                           NOT =   WRK-PTBYO-PTID
                   PERFORM 900-PTBYOMEI-READ-SEC
               END-PERFORM
           END-IF
           .
       200-PTBYOMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象月会計チェック処理
      *****************************************************************
       2001-ACCT-CHK-SEC          SECTION.
      *
      *    診療会計チェック
           INITIALIZE                  SRYACCT-REC 
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    PTBYO-PTID      TO  ACCT-PTID
           MOVE    WRK-PARA-SRYYM  TO  ACCT-SRYYM
           MOVE    "2"             TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key37"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key37"         TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key37"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-SRYACCT     =   ZERO
               GO  TO  2001-ACCT-CHK-EXT
           END-IF
      *
           INITIALIZE                  SRYACCT-REC 
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    PTBYO-PTID      TO  ACCT-PTID
           MOVE    WRK-PARA-SRYYM  TO  ACCT-SRYYM
           MOVE    "1"             TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key37"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key37"         TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key37"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-SRYACCT     =   ZERO
               GO  TO  2001-ACCT-CHK-EXT
           END-IF
      *
      *    入院会計チェック
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    PTBYO-PTID          TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-ACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名チェック処理
      *****************************************************************
       2001-PTBYO-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
                                       FLG-ERR
           INITIALIZE                  WRK-BYOMEI-REC
      *
      *    入力コードがある病名のみ対象のとき
           IF      WRK-PARA-PRTKBN =   "1"
               IF    ( PTBYO-BYOMEIINPUTCD (1)
                                           =   SPACE              )
               OR    ( PTBYO-BYOMEIINPUTCD (1)
                                           =   PTBYO-BYOMEICD (1) AND
                       PTBYO-BYOMEIINPUTCD (1)
                                       NOT =   "0000999"          )    
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    対象診療年月に有効な病名か
      *    全病名対象のときは判定なし
           IF      WRK-SRYYMD      =   "99999999"
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)
                                       =   WRK-SRYYMD (1:6)
                   CONTINUE
               ELSE
                   IF      PTBYO-TENKIKBN  =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)
                                               >=  WRK-SRYYMD (1:6)
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF
               END-IF
      *
               IF      PTBYO-REZEMM        >   ZERO
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN
                   MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  WRK-YM
                   IF      WRK-YM              >=  WRK-SRYYMD (1:6)
                       CONTINUE
                   ELSE
                       GO  TO  2001-PTBYO-CHK-EXT
                   END-IF
               END-IF
           END-IF    
      *
      *    コード化可能病名のみのとき
           IF     WRK-PARA-CDKBN   =   "1" OR  "2"
               IF      PTBYO-BYOMEIHENFLG
                                       =   "1"
      *            病名からコード検索処理
                   INITIALIZE              ORCSBYOMEIAREA
                   EVALUATE    WRK-PARA-CDKBN
                       WHEN    "1"
                           MOVE    "1"             TO  LNK-BYO-SYORI
                       WHEN    "2"
                           MOVE    "3"             TO  LNK-BYO-SYORI
                   END-EVALUATE
                   MOVE    PTBYO-BYOMEI    TO  LNK-BYO-BYOMEI
                   CALL    "ORCSBYOMEI"    USING    ORCSBYOMEIAREA
                                                     SPA-AREA
                   IF      LNK-BYO-BYOMEIHENFLG
                                           =   "1"
                       GO  TO  2001-PTBYO-CHK-EXT
                   ELSE
                     IF   ( WRK-PARA-CDKBN   =   "2"   )
                     AND  ( LNK-BYO-HOSOKU-BYOMEI
                                             =   SPACE )  
                       GO  TO  2001-PTBYO-CHK-EXT
                     ELSE
                       MOVE    6               TO  FLG-ERR
      *                廃止年月日が設定されているとき診療開始日以前か
      *                なら未傷病名コードにする
                       IF    ( LNK-BYO-IKOSAKICD   =   SPACE )
                       AND   ( LNK-BYO-HAISIYMD-ERR
                                                   =   ZERO  )
                           CONTINUE
                       ELSE
                           MOVE    LNK-BYO-KHNBYOMEICD TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           MOVE    BYOMEI-REC          TO
                                                       WRK-BYOMEI-REC
                           IF      WRK-BYO-HAISIYMD    NOT =   SPACE
                               IF      WRK-BYO-HAISIYMD
                                                       <   PTBYO-SRYYMD
                                   IF      WRK-BYO-IKOSAKICD
                                                           =   SPACE
                                       MOVE    7           TO  FLG-ERR
      *                                推奨病名チェック
                                       INITIALIZE          SYNONYM-REC
                                       MOVE    WRK-BYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
                                       PERFORM 5001-SYNONYM-CHK-SEC
                                       IF      FLG-SYNONYM =   ZERO
                                           MOVE    5       TO  FLG-ERR
                                       END-IF                                       
                                   ELSE
                                       MOVE    8           TO  FLG-ERR
                                       MOVE    WRK-BYO-IKOSAKICD
                                                           TO
                                                           BYO-BYOMEICD
                                       PERFORM 900-BYOMEI-READ-SEC
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                     END-IF
                   END-IF
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           ELSE   
      *      編集病名のとき再度チェックを行なう
             IF      PTBYO-BYOMEIHENFLG
                                   =   "1"
               IF      PTBYO-BYOMEICD (1)  =   "0000999"
                   IF      PTBYO-BYOMEIINPUTCD (1)
                                               =   PTBYO-BYOMEICD (1)
                       MOVE    9                   TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-ERR
               END-IF
             ELSE     
      *        廃止年月日が設定されているとき診療開始日以前か
      *        なら未傷病名コードにする
               MOVE    PTBYO-KHNBYOMEICD   TO  BYO-BYOMEICD
               PERFORM 900-BYOMEI-READ-SEC
               MOVE    BYOMEI-REC          TO  WRK-BYOMEI-REC
               IF      WRK-BYO-HAISIYMD    NOT =   SPACE
                   IF      WRK-BYO-HAISIYMD    <   PTBYO-SRYYMD
                       IF      WRK-BYO-IKOSAKICD   =   SPACE
                           MOVE    3                   TO  FLG-ERR
      *                    推奨病名チェック
                           INITIALIZE                  SYNONYM-REC
                           MOVE    WRK-BYO-BYOMEI      TO SYNONYM-BYOMEI
                           PERFORM 5001-SYNONYM-CHK-SEC
                           IF      FLG-SYNONYM         =   ZERO
                               MOVE    5                   TO  FLG-ERR
                           END-IF
                       ELSE
                           MOVE    4                   TO  FLG-ERR
                           MOVE    WRK-BYO-IKOSAKICD   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *    
           IF      FLG-ERR         =   ZERO
               GO  TO  2001-PTBYO-CHK-EXT
           END-IF
      *
           MOVE    1               TO  FLG-CHK    
           .
       2001-PTBYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧作成処理（患者病名）
      *****************************************************************
       2002-PRINT-SEC          SECTION.
      *
           IF      CNT-LINE             >=  54
               PERFORM 20021-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPACE               TO  BODY-01
                                           BODY-02
                                           BODY-03
                                           BODY-04
                                           BODY-05
      *
           IF      WRK-PTID            =   PTBYO-PTID
               CONTINUE
           ELSE
               MOVE    PTBYO-PTID          TO  WRK-PTID
      *        患者番号
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    PTBYO-PTID          TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  B01-PTNUM
      *
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    PTBYO-PTID          TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME          TO  B01-NAME
           END-IF
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
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1    TO  B01-SRYYMD
      *****MOVE    PTBYO-SRYKA         TO  B01-SRYKA
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    PTBYO-SRYKA         TO  SYS-1005-KBNCD
           MOVE    PTBYO-SRYYMD (1:6)  TO  SYS-1005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD (7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI         =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 (1:6)
                                           TO  B01-SRYKA
           ELSE
                MOVE    PTBYO-SRYKA        TO  B01-SRYKA
           END-IF
           EVALUATE    PTBYO-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入"            TO  B01-NYUGAIKBN
               WHEN    "2"
                    MOVE   "外"            TO  B01-NYUGAIKBN
               WHEN    OTHER
                    MOVE   "　"            TO  B01-NYUGAIKBN
           END-EVALUATE
           INSPECT B01-NYUGAIKBN       REPLACING  ALL "  "  BY  "　"
           MOVE    PTBYO-BYOMEI    TO  B01-BYOMEI
           INSPECT B01-BYOMEI          REPLACING  ALL "  "  BY  "　"
      *
           EVALUATE    FLG-ERR
               WHEN    3
               WHEN    4
                   MOVE    WRK-BYO-HAISIYMD
                                       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
           END-EVALUATE
      *
           EVALUATE    FLG-ERR
               WHEN    1
                   MOVE    "◎未コード化傷病名コードで登録"
                                               TO  B02-RIYUU
               WHEN    2
                   STRING  "◎傷病名コードで組み立てができないか、"
                                               DELIMITED  BY  SIZE
                           "入力されたコードと異る病名です"
                                               DELIMITED  BY  SIZE
                                               INTO  B02-RIYUU
                   END-STRING
               WHEN    3
               WHEN    7
                   MOVE    "◎廃止病名あり【廃止日："
                                               TO  B02-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B02-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B02-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B02-BYOMEI
               WHEN    4
               WHEN    8
                   MOVE    "◎移行病名あり【廃止日："
                                               TO  B02-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B02-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B02-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B02-BYOMEI
                   MOVE    "　　　　　　　　　　　　"
                                               TO  B03-RIYUU
                   MOVE    BYO-BYOMEICD        TO  B03-BYOMEICD
                   MOVE    BYO-BYOMEI          TO  B03-BYOMEI
               WHEN    5
                   MOVE    "◎推奨病名あり【廃止日："
                                               TO  B02-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B02-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B02-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B02-BYOMEI
                   MOVE    "　　　　　　　　　　　　"
                                               TO  B03-RIYUU
                   MOVE    BYO-BYOMEICD        TO  B03-BYOMEICD
                   MOVE    BYO-BYOMEI          TO  B03-BYOMEI
               WHEN    9
                   MOVE    "●未コード化傷病名コードで登録"
                                           TO  B02-RIYUU
           END-EVALUATE
      *
      *
           IF    ( PTBYO-BYOMEIINPUTCD (1)
                                       =   SPACE              )
           OR    ( PTBYO-BYOMEIINPUTCD (1)
                                       =   PTBYO-BYOMEICD (1) )
               CONTINUE
           ELSE 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      PTBYO-BYOMEIINPUTCD (IDX)
                                   =   SPACE
                   MOVE    PTBYO-BYOMEIINPUTCD (IDX)
                                           TO  B04-BYOMEIINPUTCD (IDX)
               END-PERFORM
           END-IF
      *
           IF   ( WRK-PARA-CDKBN   =   "2"   )
           AND  ( LNK-BYO-HOSOKU-BYOMEI
                               NOT =   SPACE )
               MOVE    LNK-BYO-HOSOKU-BYOMEI
                                           TO  B05-HOSOKU
           END-IF    
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-01         TO  HCMSL55-MOJI (CNT-LINE)
      *
           IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
               PERFORM 20021-HEAD-PRINT-SEC
           END-IF
      *
           IF      BODY-02         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20021-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-02         TO  HCMSL55-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-03         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20021-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-03         TO  HCMSL55-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-05         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20021-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-05         TO  HCMSL55-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-04         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20021-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-04         TO  HCMSL55-MOJI (CNT-LINE)
           END-IF
      *
           ADD     1               TO  CNT-LINE
      *
           ADD     1               TO  CNT-OUT
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       20021-HEAD-PRINT-SEC                SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
                                           CNT-PAGE1
           MOVE    CNT-PAGE            TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL55
           MOVE    HEAD-01             TO  HCMSL55-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL55-MOJI (3)
           EVALUATE    WRK-PARA-CDKBN
               WHEN    "0"
                   MOVE    HEAD-03             TO  HCMSL55-MOJI (4)
                   MOVE    HEAD-04             TO  HCMSL55-MOJI (5)
                   MOVE    6                   TO  CNT-LINE
               WHEN    "1"
                   MOVE    HEAD-04             TO  HCMSL55-MOJI (4)
                   MOVE    5                   TO  CNT-LINE
               WHEN    "2"
                   MOVE    HEAD-05             TO  HCMSL55-MOJI (4)
                   MOVE    HEAD-04             TO  HCMSL55-MOJI (5)
                   MOVE    6                   TO  CNT-LINE
           END-EVALUATE
           .
       20021-HEAD-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自院病名の一覧作成処理
      *****************************************************************
       200-USERTBYOMEI-SYORI-SEC       SECTION.
      *
      *    自院病名チェック処理
           PERFORM 2001-USERBYOMEI-CHK-SEC
           IF      FLG-CHK             =   1
      *        一覧作成処理（自院病名）
               PERFORM 2002-USERBYOMEI-PRINT-SEC
           END-IF
      *
           PERFORM 900-USERBYOMEI-READ-SEC
           .
       200-USERTBYOMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名チェック処理
      *****************************************************************
       2001-USERBYOMEI-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
                                       FLG-ERR
           INITIALIZE                  WRK-BYOMEI-REC
      *
      *    コード化可能病名のとき
           IF     WRK-PARA-CDKBN   =   "1" OR  "2"
               IF      USBYO-BYOMEIHENFLG
                                       =   "1"
      *            病名からコード検索処理
                   INITIALIZE              ORCSBYOMEIAREA
                   EVALUATE    WRK-PARA-CDKBN
                       WHEN    "1"
                           MOVE    "1"             TO  LNK-BYO-SYORI
                       WHEN    "2"
                           MOVE    "3"             TO  LNK-BYO-SYORI
                   END-EVALUATE
                   MOVE    USBYO-BYOMEI    TO  LNK-BYO-BYOMEI
                   CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                                   SPA-AREA
                   IF      LNK-BYO-BYOMEIHENFLG
                                           =   "1"
                       GO  TO  2001-USERBYOMEI-CHK-EXT
                   ELSE
                     IF   ( WRK-PARA-CDKBN   =   "2"   )
                     AND  ( LNK-BYO-HOSOKU-BYOMEI
                                             =   SPACE )  
                       GO  TO  2001-USERBYOMEI-CHK-EXT
                     ELSE
                       MOVE    6               TO  FLG-ERR
      *                廃止年月日が設定されているとき
                       IF    ( LNK-BYO-IKOSAKICD   =   SPACE )
                       AND   ( LNK-BYO-HAISIYMD-ERR
                                                   =   ZERO  )
                           CONTINUE
                       ELSE
                           MOVE    LNK-BYO-KHNBYOMEICD TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           MOVE    BYOMEI-REC          TO
                                                       WRK-BYOMEI-REC
                           IF      WRK-BYO-HAISIYMD
                                                   NOT =   SPACE
                               IF      WRK-BYO-IKOSAKICD   =   SPACE
                                   MOVE    7               TO  FLG-ERR
      *                            推奨病名チェック
                                   INITIALIZE          SYNONYM-REC
                                   MOVE    WRK-BYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
                                   PERFORM 5001-SYNONYM-CHK-SEC
                                   IF      FLG-SYNONYM =   ZERO
                                       MOVE    5           TO  FLG-ERR
                                   END-IF
                               ELSE
                                   MOVE    8               TO  FLG-ERR
                                   MOVE    WRK-BYO-IKOSAKICD
                                                           TO
                                                           BYO-BYOMEICD
                                   PERFORM 900-BYOMEI-READ-SEC
                               END-IF
                           END-IF
                       END-IF
                     END-IF
                   END-IF
               ELSE
                   GO  TO  2001-USERBYOMEI-CHK-EXT
               END-IF
           ELSE   
      *      編集病名のとき再度チェックを行なう
             IF      USBYO-BYOMEIHENFLG
                                   =   "1"
               IF      USBYO-BYOMEICD (1)  =   "0000999"
                   IF      USBYO-BYOMEIINPUTCD =   USBYO-BYOMEICD (1)
                       MOVE    9                   TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               ELSE
                   MOVE    2                   TO  FLG-ERR
               END-IF
             ELSE     
      *        廃止年月日が設定されているとき
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       21
                       OR      USBYO-BYOMEICD (IDY)
                                           =   SPACE   OR  ALL "9"
                   IF      USBYO-BYOMEICD (IDY) (1:1)
                                               NOT =   "Z"
                       MOVE    USBYO-BYOMEICD (IDY)
                                                   TO  USBYO-KHNBYOMEICD
                       MOVE    21                  TO  IDY
                   END-IF
               END-PERFORM         
               MOVE    USBYO-KHNBYOMEICD   TO  BYO-BYOMEICD
               PERFORM 900-BYOMEI-READ-SEC
               MOVE    BYOMEI-REC          TO  WRK-BYOMEI-REC
      *********IF      WRK-BYO-HAISIYMD    NOT =   SPACE
               IF      WRK-BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                   CONTINUE
               ELSE
                   IF      WRK-BYO-IKOSAKICD   =   SPACE
                       MOVE    3                   TO  FLG-ERR
      *                推奨病名チェック
                       INITIALIZE                  SYNONYM-REC
                       MOVE    WRK-BYO-BYOMEI      TO  SYNONYM-BYOMEI
                       PERFORM 5001-SYNONYM-CHK-SEC
                       IF      FLG-SYNONYM         =   ZERO
                           MOVE    5                   TO  FLG-ERR
                       END-IF
                   ELSE
                       MOVE    4                   TO  FLG-ERR
                       MOVE    WRK-BYO-IKOSAKICD   TO  BYO-BYOMEICD
                       PERFORM 900-BYOMEI-READ-SEC
                   END-IF
               END-IF
             END-IF
           END-IF
      *    
           IF      FLG-ERR         =   ZERO
               GO  TO  2001-USERBYOMEI-CHK-EXT
           END-IF
      *
           MOVE    1               TO  FLG-CHK    
           .
       2001-USERBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧作成処理（自院病名）
      *****************************************************************
       2002-USERBYOMEI-PRINT-SEC          SECTION.
      *
           IF      CNT-LINE             >=  79
               PERFORM 20021-USERBYOMEI-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPACE               TO  BODY-011
                                           BODY-013
                                           BODY-014
                                           BODY-015
      *
           MOVE    USBYO-BYOMEIINPUTCD
                                       TO  B011-BYOMEIINPUTCD
           IF     USBYO-CDSYU          =   "J"
               INSPECT B011-BYOMEIINPUTCD  REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           IF     USBYO-CDSYU          =   "J"
               MOVE    USBYO-BYOMEI        TO  B011-BYOMEI1
               INSPECT B011-BYOMEI1        REPLACING  ALL "  "  BY  "　"
           ELSE
               MOVE    USBYO-BYOMEI        TO  B011-BYOMEI
           END-IF
      *
           EVALUATE    FLG-ERR
               WHEN    3
               WHEN    4
                   MOVE    WRK-BYO-HAISIYMD    TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
           END-EVALUATE
      *
           EVALUATE    FLG-ERR
               WHEN    1
                   MOVE    "◎未コード化傷病名コードで登録"
                                               TO  B013-RIYUU
               WHEN    2
                   STRING  "◎傷病名コードで組み立てができないか、"
                                               DELIMITED  BY  SIZE
                           "入力されたコードと異る病名です"
                                               DELIMITED  BY  SIZE
                                               INTO  B013-RIYUU
                   END-STRING
               WHEN    3
               WHEN    7
                   MOVE    "◎廃止病名あり【廃止日："
                                               TO  B013-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B013-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B013-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B013-BYOMEI
               WHEN    4
               WHEN    8
                   MOVE    "◎移行病名あり【廃止日："
                                               TO  B013-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B013-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B013-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B013-BYOMEI
                   MOVE    "　　　　　　　　　　　　"
                                               TO  B014-RIYUU
                   MOVE    BYO-BYOMEICD        TO  B014-BYOMEICD
                   MOVE    BYO-BYOMEI          TO  B014-BYOMEI
               WHEN    5
                   MOVE    "◎推奨病名あり【廃止日："
                                               TO  B013-RIYUU1
                   MOVE    LNK-DAY2-EDTYMD1    TO  B013-HAISIYMD
                   MOVE    WRK-BYO-BYOMEICD    TO  B013-BYOMEICD
                   MOVE    WRK-BYO-BYOMEI      TO  B013-BYOMEI
                   MOVE    "　　　　　　　　　　　　"
                                               TO  B014-RIYUU
                   MOVE    BYO-BYOMEICD        TO  B014-BYOMEICD
                   MOVE    BYO-BYOMEI          TO  B014-BYOMEI
               WHEN    9
                   MOVE    "●未コード化傷病名コードで登録"
                                               TO  B013-RIYUU
           END-EVALUATE
      *
           IF   ( WRK-PARA-CDKBN   =   "2"   )
           AND  ( LNK-BYO-HOSOKU-BYOMEI
                               NOT =   SPACE )
               MOVE    LNK-BYO-HOSOKU-BYOMEI
                                           TO  B015-HOSOKU
           END-IF    
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-011        TO  HCMSL80-MOJI (CNT-LINE)
      *
           IF      CNT-LINE             >=  WRK-CONS-LINE-MAX1
               PERFORM 20021-USERBYOMEI-HEAD-PRINT-SEC
           END-IF
      *
           IF      BODY-013        NOT =   SPACE
               IF      CNT-LINE            >=  WRK-CONS-LINE-MAX1
                   PERFORM 20021-USERBYOMEI-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-013        TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-014        NOT =   SPACE
               IF      CNT-LINE            >=  WRK-CONS-LINE-MAX1
                   PERFORM 20021-USERBYOMEI-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-014        TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-015        NOT =   SPACE
               IF      CNT-LINE            >=  WRK-CONS-LINE-MAX1
                   PERFORM 20021-USERBYOMEI-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-015        TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
           ADD     1               TO  CNT-LINE
      *
           ADD     1               TO  CNT-OUT
           .
       2002-USERBYOMEI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       20021-USERBYOMEI-HEAD-PRINT-SEC SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
                                           CNT-PAGE1
           MOVE    CNT-PAGE            TO  H011-PAGE
      *
           MOVE    SPACE               TO  HCMSL80
           MOVE    HEAD-011            TO  HCMSL80-MOJI (1)
           MOVE    HEAD-012            TO  HCMSL80-MOJI (3)
           MOVE    4                   TO  CNT-LINE
           IF      WRK-PARA-CDKBN  =   "0"
               MOVE    HEAD-013            TO  HCMSL80-MOJI (4)
               MOVE    5                   TO  CNT-LINE
           ELSE
               IF      WRK-PARA-CDKBN  =   "2"
                   MOVE    HEAD-014            TO  HCMSL80-MOJI (4)
                   MOVE    5                   TO  CNT-LINE
               END-IF
           END-IF
           .
       20021-USERBYOMEI-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       200111-PRINT-OUT-SEC                SECTION.
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
           EVALUATE    FLG-PRTKBN
               WHEN    1
                   MOVE   "HCMSL55.red"        TO  SPRT-PRTID
                   MOVE    HCMSL55             TO  SPRT-PRTDATA
                   IF      WRK-PARA-CDKBN      =   "0"
                       MOVE    "未コード化病名一覧"
                                                   TO  SPRT-TITLE
                   ELSE
                       IF      WRK-PARA-CDKBN      =   "1"
                           MOVE    "コード化可能病名一覧"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE
                               "コード化可能病名一覧（補足コメント）"
                                                       TO  SPRT-TITLE
                       END-IF
                   END-IF
               WHEN    2
          
                   MOVE   "HCMSL80.red"        TO  SPRT-PRTID
                   MOVE    HCMSL80             TO  SPRT-PRTDATA
                   IF      WRK-PARA-CDKBN      =   "0"
                       MOVE    "（自院）未コード化病名一覧"
                                                   TO  SPRT-TITLE
                   ELSE
                       IF      WRK-PARA-CDKBN      =   "1"
                           MOVE    "（自院）コード化可能病名一覧"
                                                       TO  SPRT-TITLE
                       ELSE
                           MOVE
                          "（自院）コード化可能病名一覧（補足コメント）"
                                                       TO  SPRT-TITLE
                       END-IF
                   END-IF
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
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       200111-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBG021 " DELIMITED  BY  SIZE
                   WRK-RECEERR     DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      *
           PERFORM 900-SYNONYM-READ-SEC
      *
           IF      FLG-SYNONYM         =   ZERO
               MOVE    SYNONYM-BYOMEICD    TO  BYO-BYOMEICD
               MOVE    ZERO                TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   1
                   MOVE    1                   TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBG021 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE1       TO  JOB-UPDCNT
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
      *    患者病名読込
      *****************************************************************
       900-PTBYOMEI-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key33"         TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名読込
      *****************************************************************
       900-USERBYOMEI-READ-SEC           SECTION.
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
      *        分類コードは読み飛ばし
               IF      USBYO-CDSYU     =   "C"
                   GO  TO  900-USERBYOMEI-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USERBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読み込み
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
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタ読込
      *****************************************************************
       900-SYNONYM-READ-SEC         SECTION.
      *
           MOVE    SYNONYM-REC         TO  MCPDATA-REC
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_synonym"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYNONYM-REC
                   MOVE    ZERO            TO  FLG-SYNONYM
               ELSE
                   INITIALIZE                  SYNONYM-REC
                   MOVE    1               TO  FLG-SYNONYM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYNONYM
               INITIALIZE                      SYNONYM-REC
           END-IF
      *
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYNONYM-READ-EXT.
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
