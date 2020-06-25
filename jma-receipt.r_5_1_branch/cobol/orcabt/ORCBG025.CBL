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
       PROGRAM-ID.             ORCBG025.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 傷病名マスタ一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/09/28    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE    ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
           COPY    "HCMSL80.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-TOUKEIERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-PAGE-ALL        PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-OUT-ALL         PIC 9(05).
           03  CNT-CSV             PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-PRTKBN     PIC X(01).
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    ＣＳＶデータ編集用
       01  WRK-CSV-AREA.
           03  WRK-IN-DATA         PIC X(2000).
           03  WRK-OUT-REC         PIC X(2000).
           03  WRK-CSV-REC         PIC X(2000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-IN-MAX          PIC 9(04).
           03  WRK-END             PIC 9(01).
      *
      *    ＣＳＶデータ日付編集用
       01  WRK-CSVHENYMD.
           03  WRK-CYY             PIC 9(04).
           03  FILLER              PIC X(01)   VALUE   "/".
           03  WRK-CMM             PIC 9(02).
           03  FILLER              PIC X(01)   VALUE   "/".
           03  WRK-CDD             PIC 9(02).
      *
      *    ＣＳＶ出力先
       01  WRK-TO-DATA-AREA.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(16)   VALUE
                                       "shobyomeimasuta_".
               05  WRK-TO-DATA-SYOKBN  PIC X(01).
               05  WRK-TO-DATA-PRTKBN  PIC X(01).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    移行先病名
       01  WRK-BYOMEI-REC.
           COPY    "CPBYOMEI.INC"  REPLACING  //BYO-//
                                   BY         //WRK-BYO-//.
      *
       01  WRK-ERR-AREA.
           03  WRK-TOUKEIERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *
       01  CSV-HEAD-01.
           03  FILLER                  PIC X(12)   VALUE
               "傷病名コード".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(08)   VALUE
               "傷病名称".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(14)  VALUE
               "傷病名カナ名称".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(16)   VALUE
               "ＩＣＤ−１０−１".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(16)   VALUE
               "ＩＣＤ−１０−２".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(12)   VALUE
               "病名管理番号".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(08)   VALUE   "採択区分".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(12)   VALUE
               "交換用コード".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(18)   VALUE
               "特定疾患等対象区分".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(16)   VALUE
               "難病外来対象区分".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(16)   VALUE
               "単独使用禁止区分".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(14)   VALUE
               "保険請求外区分".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(10)   VALUE
               "廃止年月日".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(12)   VALUE
               "移行先コード".
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  FILLER                  PIC X(12)   VALUE
               "移行先傷病名".
      *
       01  HEAD-01.
           03  FILLER                  PIC X(40)   VALUE   SPACE.
           03  FILLER                  PIC X(16)   VALUE
               "傷病名マスタ一覧".
           03  H01-TITLE               PIC X(26)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(05)   VALUE   "   P.".
           03  H01-PAGE                PIC ZZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(14)   VALUE
               "傷病名コード　".
           03  FILLER                  PIC X(08)   VALUE
               "傷病名称".
      *
       01  HEAD-03.
           03  FILLER                  PIC X(14)   VALUE
               "　　　　　　　".
           03  FILLER                  PIC X(14)  VALUE
               "傷病名カナ名称".
      *
       01  HEAD-04.
           03  FILLER                  PIC X(14)   VALUE
               "　　　　　　　".
           03  FILLER                  PIC X(12)   VALUE
               "ＩＣＤ−１０".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE
               "管理番号".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(04)   VALUE
               "採択".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(06)   VALUE
               "交換用".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(14)   VALUE
               "特定疾患等".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE
               "難病外来".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(18)   VALUE
               "単独使用禁止".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "保険請求外".
      *
       01  BODY-01.
           03  B01-BYOMEICD            PIC X(14).
           03  B01-BYOMEI              PIC X(60).
      *
       01  BODY-02.
           03  B02-FIL1                PIC X(14).
           03  B02-BYOMEIKANA          PIC X(100).
      *
       01  BODY-03.
           03  B03-FIL1                PIC X(14).
           03  B03-ICD10-1             PIC X(05).
           03  B03-FIL2                PIC X(01).
           03  B03-ICD10-2             PIC X(05).
           03  B03-FIL3                PIC X(02).
           03  B03-HYOJUNCD            PIC X(08).
           03  B03-FIL4                PIC X(01).
           03  B03-SAITAKUKBN          PIC X(01).
           03  B03-FIL5                PIC X(04).
           03  B03-BYOMEICHGCD         PIC X(04).
           03  B03-FIL6                PIC X(04).
           03  B03-TOKSKNCD            PIC X(02).
           03  B03-FIL7                PIC X(01).
           03  B03-TOKSKNCD-NM         PIC X(10).
           03  B03-FIL8                PIC X(02).
           03  B03-NANBYOCD            PIC 9(02).
           03  B03-FIL9                PIC X(01).
           03  B03-NANBYOCD-NM         PIC X(08).
           03  B03-FIL10               PIC X(02).
           03  B03-TANDOKUKBN          PIC 9(02).
           03  B03-FIL11               PIC X(01).
           03  B03-TANDOKUKBN-NM       PIC X(14).
           03  B03-FIL12               PIC X(02).
           03  B03-HKNSKYKBN           PIC 9(01).
           03  B03-FIL13               PIC X(01).
           03  B03-HKNSKYKBN-NM        PIC X(10).
      *
       01  BODY-04.
           03  B04-FIL1                PIC X(14).
           03  B04-HAISI-TITLE         PIC X(08).
           03  B04-HAISIYMD            PIC X(09).
           03  B04-FIL2                PIC X(02).
           03  B04-IKOSAKI-TITLE       PIC X(08).
           03  B04-IKOSAKICD           PIC X(07).
           03  B04-FIL3                PIC X(01).
           03  B04-IKOSAKI-BYOMEI      PIC X(60).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   80.
      *    カンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
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
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
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
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE                   DIVISION
                                       USING   COMMAND-PARAM.
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
           INITIALIZE                  TOUKEIERR
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
                                       TOUKEIERR
                                       WRK-PARA-SYOKBN
                                       WRK-PARA-PRTKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-SYOKBN="  WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-PRTKBN="  WRK-PARA-PRTKBN
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBG025"      TO  JOB-PGID
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "0"
                   MOVE    "傷病名マスタ一覧"  TO  JOB-SHELLMSG
               WHEN    "1"
                   MOVE     "傷病名マスタ一覧（傷病名）"
                                               TO  JOB-SHELLMSG
               WHEN    "2"
                   MOVE    "傷病名マスタ一覧（廃止病名のみ）"
                                               TO  JOB-SHELLMSG
               WHEN    "3"
                   MOVE    "傷病名マスタ一覧（移行病名のみ）"
                                               TO  JOB-SHELLMSG
               WHEN    "9"
                   MOVE    "傷病名マスタ一覧（修飾語）"
                                               TO  JOB-SHELLMSG
               WHEN    OTHER
                   MOVE    "傷病名マスタ一覧"  TO  JOB-SHELLMSG
           END-EVALUATE
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TOUKEIERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  TOUKEIERR
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "0"
                   MOVE    "　　　　　　"      TO  H01-TITLE
               WHEN    "1"
                   MOVE    "（傷病名）　"      TO  H01-TITLE
               WHEN    "2"
                   MOVE    "（廃止病名のみ）"  TO  H01-TITLE
               WHEN    "3"
                   MOVE    "（移行病名のみ）"  TO  H01-TITLE
               WHEN    "9"
                   MOVE    "（修飾語）　"      TO  H01-TITLE
               WHEN    OTHER
                   MOVE    "指定したパラメタが正しくありません"
                                               TO  WRK-TOUKEIERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-EVALUATE
      *
           IF      WRK-PARA-PRTKBN     =   "0" OR  "1"
               CONTINUE
           ELSE
               MOVE    "指定したパラメタが正しくありません"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
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
      *    ＣＳＶ
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-PARA-SYOKBN     TO  WRK-TO-DATA-SYOKBN
           MOVE    WRK-PARA-PRTKBN     TO  WRK-TO-DATA-PRTKBN
           DISPLAY "CSVFILE=" WRK-TO-DATA
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF      WRK-PARA-PRTKBN =   "0"
               MOVE    "key9"          TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key8"          TO  WRK-MCP-PATHNAME
           END-IF
      *
      *    傷病名
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
                   MOVE    1               TO  FLG-SYORI
                   PERFORM 2001-PRINT-SEC
           END-EVALUATE
      *
      *    修飾語
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "0"
               WHEN    "9"
                   MOVE    2               TO  FLG-SYORI
                   PERFORM 2001-PRINT-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2001-PRINT-SEC                SECTION.
      *
           MOVE    WRK-CONS-LINE-MAX
                                   TO  CNT-LINE
           MOVE    ZERO            TO  CNT-PAGE
                                       CNT-OUT
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "0"
                   IF      FLG-SYORI       =   1
                       MOVE    "（傷病名）　"      TO  H01-TITLE
                   ELSE
                       MOVE    "（修飾語）　"      TO  H01-TITLE
                   END-IF
               WHEN    "1"
                   MOVE    "（傷病名）　"      TO  H01-TITLE
               WHEN    "2"
                   MOVE    "（廃止病名のみ）"  TO  H01-TITLE
               WHEN    "3"
                   MOVE    "（移行病名のみ）"  TO  H01-TITLE
               WHEN    "9"
                   MOVE    "（修飾語）　"      TO  H01-TITLE
           END-EVALUATE
      *
           INITIALIZE                  BYOMEI-REC
           IF      FLG-SYORI       =   1
               MOVE    "0000000"       TO  BYO-BYOMEICD
               MOVE    "ZZZ0000"       TO  BYO-IKOSAKICD
           ELSE
               MOVE    "ZZZ0000"       TO  BYO-BYOMEICD
               MOVE    "ZZZZZZZ"       TO  BYO-IKOSAKICD
           END-IF 
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
      *    明細編集処理
           PERFORM 20011-BODY-PRINT-SEC
                           UNTIL   FLG-END    =   1 
      *
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   ZERO
               PERFORM 2002-PRINT-OUT-SEC
           END-IF
      *
           IF      FLG-SYORI       =   1
               DISPLAY "*** ORCBG025 BYOMEI   OUT  "  CNT-OUT
               DISPLAY "*** ORCBG025          PAGE "  CNT-PAGE
           ELSE
               DISPLAY "*** ORCBG025 SHUSHOKU OUT  "  CNT-OUT
               DISPLAY "*** ORCBG025          PAGE "  CNT-PAGE
           END-IF
           .
       2001-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20011-BODY-PRINT-SEC         SECTION.
      *
      *
           IF      CNT-LINE             >=  79
               PERFORM 20012-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPACE               TO  BODY-01
                                           BODY-04
                                           BODY-03
                                           BODY-02
      *
           MOVE    BYO-BYOMEICD        TO  B01-BYOMEICD
           MOVE    BYO-BYOMEI          TO  B01-BYOMEI
           INSPECT B01-BYOMEI          REPLACING  ALL "  "  BY  "　"
      *
           MOVE    BYO-BYOMEIKANA      TO  B02-BYOMEIKANA
           INSPECT B02-BYOMEIKANA      REPLACING  ALL "  "  BY  "　"
      *
           MOVE    BYO-ICD10-1         TO  B03-ICD10-1
           MOVE    BYO-ICD10-2         TO  B03-ICD10-2
           MOVE    BYO-HYOJUNCD        TO  B03-HYOJUNCD
           MOVE    BYO-SAITAKUKBN      TO  B03-SAITAKUKBN
           MOVE    BYO-BYOMEICHGCD     TO  B03-BYOMEICHGCD
      *
           IF      BYO-BYOMEICD (1:3)  =   "ZZZ"
               CONTINUE
           ELSE
               MOVE    BYO-TOKSKNCD        TO  B03-TOKSKNCD
               EVALUATE    BYO-TOKSKNCD
                   WHEN    "00"
                       MOVE    "対象外　　"        TO  B03-TOKSKNCD-NM
                   WHEN    "03"
                       MOVE    "皮膚（１）"        TO  B03-TOKSKNCD-NM
                   WHEN    "04"
                       MOVE    "皮膚（２）"        TO  B03-TOKSKNCD-NM
                   WHEN    "05"
                       MOVE    "特定疾患　"        TO  B03-TOKSKNCD-NM
                   WHEN    "07"
                       MOVE    "てんかん　"        TO  B03-TOKSKNCD-NM
                   WHEN    "08"
                       MOVE    "特定・てん"        TO  B03-TOKSKNCD-NM
               END-EVALUATE
               MOVE    BYO-NANBYOCD        TO  B03-NANBYOCD
               EVALUATE    BYO-NANBYOCD
                   WHEN    "00"
                       MOVE    "対象外　"          TO  B03-NANBYOCD-NM
                   WHEN    "09"
                       MOVE    "難病外来"          TO  B03-NANBYOCD-NM
               END-EVALUATE
               MOVE    BYO-TANDOKUKBN      TO  B03-TANDOKUKBN
               EVALUATE    BYO-TANDOKUKBN
                   WHEN    0
                       MOVE    "組合せ必須以外"    TO  B03-TANDOKUKBN-NM
                   WHEN    1
                       MOVE    "組合せ必須　　"    TO  B03-TANDOKUKBN-NM
               END-EVALUATE
               MOVE    BYO-HKNSKYKBN       TO  B03-HKNSKYKBN
               EVALUATE    BYO-HKNSKYKBN
                   WHEN    "0"
                       MOVE    "請求対象　"        TO  B03-HKNSKYKBN-NM
                   WHEN    "1"
                       MOVE    "請求対象外"        TO  B03-HKNSKYKBN-NM
               END-EVALUATE
           END-IF
      *
           IF      BYO-HAISIYMD        =   "99999999"
               CONTINUE
           ELSE
               MOVE    "廃止日："          TO  B04-HAISI-TITLE
      *
               MOVE    BYO-HAISIYMD        TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1    TO  B04-HAISIYMD
           END-IF
      *
           INITIALIZE                      WRK-BYOMEI-REC
           IF      BYO-IKOSAKICD       =   SPACE
               CONTINUE
           ELSE
               MOVE    "移行先："          TO  B04-IKOSAKI-TITLE
      *
               MOVE    BYO-IKOSAKICD       TO  B04-IKOSAKICD
               INITIALIZE                      WRK-BYOMEI-REC
               MOVE    BYO-IKOSAKICD       TO  WRK-BYO-BYOMEICD
               PERFORM 900-BYOMEI-READ-SEC
               MOVE    WRK-BYO-BYOMEI      TO  B04-IKOSAKI-BYOMEI
               INSPECT B04-IKOSAKI-BYOMEI  REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-01         TO  HCMSL80-MOJI (CNT-LINE)
      *
           IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
               PERFORM 20012-HEAD-PRINT-SEC
           END-IF
           ADD     1               TO  CNT-LINE
           MOVE    BODY-02         TO  HCMSL80-MOJI (CNT-LINE)
      *
           IF      BODY-03         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20012-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-03         TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
           IF      BODY-04         NOT =   SPACE
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
                   PERFORM 20012-HEAD-PRINT-SEC
               END-IF
               ADD     1               TO  CNT-LINE
               MOVE    BODY-04         TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
      *    ＣＳＶデータ出力
           PERFORM 500-CSV-SYORI-SEC
      *
           ADD     1               TO  CNT-LINE
      *
           ADD     1               TO  CNT-OUT
                                       CNT-OUT-ALL
      *
           PERFORM 900-BYOMEI-READ-N-SEC
           .
       20011-BODY-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       20012-HEAD-PRINT-SEC                SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 2002-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
                                           CNT-PAGE-ALL
           MOVE    CNT-PAGE-ALL        TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL80
           MOVE    HEAD-01             TO  HCMSL80-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL80-MOJI (3)
           MOVE    HEAD-03             TO  HCMSL80-MOJI (4)
           MOVE    HEAD-04             TO  HCMSL80-MOJI (5)
           MOVE    6                   TO  CNT-LINE
           .
       20012-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2002-PRINT-OUT-SEC                SECTION.
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
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "2"
                   MOVE    "傷病名一覧（廃止病名）"
                                               TO  SPRT-TITLE
               WHEN    "3"
                   MOVE    "傷病名一覧（移行病名）"
                                               TO  SPRT-TITLE
               WHEN    OTHER
                   MOVE    "傷病名一覧"        TO  SPRT-TITLE
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
                                          TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2002-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ処理
      *****************************************************************
       500-CSV-SYORI-SEC             SECTION.
      *
           IF      CNT-CSV             =   ZERO
               PERFORM 5001-CSV-HEAD-SEC
           END-IF
      *
           INITIALIZE                      WRK-CSV-AREA
      *    傷病名コード
           MOVE    BYO-BYOMEICD        TO  WRK-IN-DATA
           MOVE    7                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    傷病名称
           MOVE    BYO-BYOMEI          TO  WRK-IN-DATA
           MOVE    200                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    傷病名カナ名称
           MOVE    BYO-BYOMEIKANA      TO  WRK-IN-DATA
           MOVE    200                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    ＩＤＣ−１０−１
           MOVE    BYO-ICD10-1         TO  WRK-IN-DATA
           MOVE    5                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    ＩＤＣ−１０−２
           MOVE    BYO-ICD10-2         TO  WRK-IN-DATA
           MOVE    5                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    病名管理区分
           MOVE    BYO-HYOJUNCD        TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    採択区分
           MOVE    BYO-SAITAKUKBN      TO  WRK-IN-DATA
           MOVE    1                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    病名交換用コード
           MOVE    BYO-BYOMEICHGCD     TO  WRK-IN-DATA
           MOVE    4                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    特定疾患コード
           MOVE    BYO-TOKSKNCD        TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    難病外来コード
           MOVE    BYO-TOKSKNCD        TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    単独使用禁止区分
           MOVE    BYO-TANDOKUKBN      TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    保険請求外区分
           MOVE    BYO-HKNSKYKBN       TO  WRK-IN-DATA
           MOVE    1                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    廃止年月日
           MOVE    BYO-HAISIYMD        TO  WRK-SYMD
           MOVE    WRK-SYY             TO  WRK-CYY
           MOVE    WRK-SMM             TO  WRK-CMM
           MOVE    WRK-SDD             TO  WRK-CDD
           MOVE    WRK-CSVHENYMD       TO  WRK-IN-DATA
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    移行先コード
           MOVE    BYO-IKOSAKICD       TO  WRK-IN-DATA
           MOVE    7                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    移行先傷病名
           MOVE    WRK-BYO-BYOMEI      TO  WRK-IN-DATA
           MOVE    200                 TO  WRK-IN-MAX
           MOVE    1                   TO  WRK-END
           PERFORM 5002-MOJI-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   WRK-KAIGYO              DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       500-CSV-SYORI-EXIT.
           EXIT.
      *
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       5001-CSV-HEAD-SEC           SECTION.
      *
           MOVE    CSV-HEAD-01         TO  WRK-OUT-REC
           MOVE    SPACE               TO  WRK-CSV-REC
           STRING  WRK-OUT-REC         DELIMITED  BY  SPACE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                                       INTO  WRK-CSV-REC
           END-STRING
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       5001-CSV-HEAD-EXIT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5002-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-IN-DATA         =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW     FROM    WRK-IN-MAX  BY  -1
                       UNTIL   IDW     <       1
                   IF      WRK-IN-DATA (IDW:1) NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-IN-DATA (1:IDW)
                                                   TO  WRK-CSV-REC
                       ELSE
                           MOVE    SPACE           TO  WRK-OUT-REC
                           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-IN-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-OUT-REC
                           END-STRING
                           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               MOVE    SPACE               TO  WRK-OUT-REC
               STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                       WRK-KUGIRI              DELIMITED  BY  SIZE
                       INTO        WRK-OUT-REC
               END-STRING
               MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           .
       5002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       600-TOUKEICSV-SEC           SECTION.
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
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "傷病名マスタ一覧ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1               TO  CNT-CSV
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       600-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               TOUKEIERR-FILE
           IF        ( STS-TOUKEIERR   =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR   TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBG025 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DISPLAY "*** ORCBG025          OUT  "  CNT-OUT-ALL
      *    DISPLAY "*** ORCBG025          PAGE "  CNT-PAGE-ALL
           DISPLAY "*** ORCBG025 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE-ALL    TO  JOB-UPDCNT
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
      *    病名読込
      *****************************************************************
       900-BYOMEI-READ-N-SEC           SECTION.
      *
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
               EVALUATE    WRK-PARA-SYOKBN
                   WHEN    "2"
                       IF      BYO-HAISIYMD   =   "99999999"
                           GO  TO  900-BYOMEI-READ-N-SEC
                       END-IF
                       IF      BYO-IKOSAKICD
                                          NOT =   SPACE
                           GO  TO  900-BYOMEI-READ-N-SEC
                       END-IF
                   WHEN    "3"
                       IF      BYO-IKOSAKICD  =   SPACE
                           GO  TO  900-BYOMEI-READ-N-SEC
                       END-IF
               END-EVALUATE
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-BYOMEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    WRK-BYOMEI-REC  TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_byomei"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  WRK-BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
                   INITIALIZE                  WRK-BYOMEI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  WRK-BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-BYOMEI-READ-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-TOUKEIERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCBG025 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
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
      *
