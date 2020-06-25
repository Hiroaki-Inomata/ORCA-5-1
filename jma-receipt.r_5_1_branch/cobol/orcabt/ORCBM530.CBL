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
       PROGRAM-ID.             ORCBM530.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 症状詳記（個別作成）
      *  管理者            :
      *  作成日付    作業者        記述
      *  08/10/31    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *
      *  04.07.01    NACL-藤原    14/12/03  平成２６年１１月改正対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  05.00.00    NACL-森脇    19/04/19  新元号対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *----(05.00.00)--UPD-START---
      *     COPY    "HCM37V01.INC"  REPLACING  //HCM37V01//
           COPY    "HCM37V02.INC"  REPLACING  //HCM37V02//
                                   BY         //HCM37//.
      *----(05.00.00)--UPD-END-----
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
           03  FLG-ONRECE-B            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOH               PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
           03  FLG-RECEDENOK           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
           03  CNT-RECEDEN             PIC 9(06).
           03  CNT-RECE10              PIC 9(06).
           03  CNT-GPAGE               PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           05  WRK-PARA-RENNUM         PIC 9(3).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-PATH                PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-SJKBN               PIC X(02).
           03  WRK-KEY-SJKBN           PIC X(02).
           03  WRK-MOJI                PIC X(100).
           03  WRK-KOHID               PIC 9(10).
           03  WRK-KOHNUM              PIC X(03).
           03  WRK-JKYSNUM             PIC X(20).
      *
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-SRYYMWH             PIC X(16).
           03  WRK-SYM                 PIC 9(06).
      *
           03  WRK-ZZ9                 PIC ZZ9.
      *
       01  WRK-MOJI-AREA.
           03  WRK-MOJI1.
               05  FILLER11            PIC X(50)   VALUE
                   "【患者の主たる疾患（合併症を含む。）の診断根拠とな".
               05  FILLER12            PIC X(50)   VALUE
                   "った臨床症状】".
           03  WRK-MOJI2.
               05  FILLER21            PIC X(50)   VALUE
                   "【患者の主たる疾患（合併症を含む。）の診断根拠とな".
               05  FILLER22            PIC X(50)   VALUE
                   "った臨床症状の診察・検査所見】".
           03  WRK-MOJI3.
               05  FILLER31            PIC X(50)   VALUE
                   "【主な治療行為（手術、処置、薬物治療等）の必要性】".
           03  WRK-MOJI4.
               05  FILLER41            PIC X(50)   VALUE
                   "【主な治療行為（手術、処置、薬物治療等）の経過】".
           03  WRK-MOJI5.
               05  FILLER51            PIC X(50)   VALUE
                   "【診療報酬明細書の合計点数が１００万点以上の場合に".
               05  FILLER52            PIC X(50)   VALUE
                   "おける薬剤に係る症状等】".
           03  WRK-MOJI6.
               05  FILLER61            PIC X(50)   VALUE
                   "【診療報酬明細書の合計点数が１００万点以上の場合に".
               05  FILLER62            PIC X(50)   VALUE
                   "おける処置に係る症状等】".
           03  WRK-MOJI7.
               05  FILLER71            PIC X(50)   VALUE   "【その他】".
           03  WRK-MOJI8.
               05  FILLER81            PIC X(50)   VALUE
                   "【診療報酬明細書摘要欄記載コメント】".
           03  WRK-MOJI9.
               05  FILLER91            PIC X(50)   VALUE
                   "【薬事法に規定する治験に係る治験概要】".
           03  WRK-MOJI10.
               05  FILLER101           PIC X(50)   VALUE
                   "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  WRK-MOJI11.
               05  FILLER111           PIC X(50)   VALUE
                   "【廃用症候群に係る評価表】".
           03  WRK-MOJI12.
               05  FILLER121           PIC X(50)   VALUE
                   "【上記以外の診療報酬明細書】".
           03  WRK-MOJI13.
               05  FILLER              PIC X(50)   VALUE
               "【医薬品医療機器等法に規定する治験に係る治験概要】".
      *
       01  CONST-AREA.
           03  CONST-H2004             PIC X(06)   VALUE    "200804".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科情報
      *     COPY    "CPSK1005.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算情報
           COPY    "CPRECEDEN.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOH-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
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
      *    個別レセプト対応
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
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
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
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
           DISPLAY "*** ORCBM530 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
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
                                               WRK-PARA-RENNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBM530"          TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "症状詳記（社保）"
                                           TO  JOB-SHELLMSG
               WHEN    "2"
                   MOVE    "症状詳記（国保）"
                                           TO  JOB-SHELLMSG
               WHEN    "3"
                   MOVE    "症状詳記（広域）"
                                           TO  JOB-SHELLMSG
               WHEN    "4"
                   MOVE    "症状詳記（国保、広域）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    システム管理基本情報読込
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYM
           PERFORM 900-SYS1001-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-SYS1900-READ-SEC
      *
      *    個別請求情報読み込み
           INITIALIZE                  ONRECE-B-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ONRECE-B-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  ONRECE-B-SRYYM
           MOVE    WRK-PARA-TEISYUTUSAKI
                                       TO  ONRECE-B-TEISYUTUSAKI
           MOVE    WRK-PARA-RENNUM     TO  ONRECE-B-RENNUM
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM UNTIL   FLG-END     =   1
      *        請求管理読み込み
               PERFORM 900-SEIKYU-READ-SEC
      *        レセプト電算情報読み込み
               PERFORM 900-RECEDEN-SELECT-SEC
      *
               MOVE    SPACE       TO  WRK-SJKBN
               MOVE    SPACE       TO  WRK-KEY-SJKBN
               MOVE    SPACE       TO  WRK-MOJI
               MOVE    ZERO        TO  CNT-PAGE
               MOVE    ZERO        TO  CNT-LINE
      *
               PERFORM UNTIL   FLG-RECEDEN =   1
      *
                   IF     WRK-MOJI     =   SPACE
                       MOVE    RECEDEN-RECEDATA    TO  WRK-MOJI
                       MOVE    RECEDEN-SJKBN       TO  WRK-SJKBN
      *                レセプト電算情報読み込み
                       PERFORM 900-RECEDEN-READ-SEC
                   END-IF
      *
      *            帳票編集<ヘッダー部>処理
                   IF    ( CNT-LINE    =   ZERO)
                       PERFORM 210-HEAD-HEN-SEC
                   END-IF
      *E
                   IF      WRK-SJKBN   NOT =   WRK-KEY-SJKBN
                       IF    ( WRK-MOJI    NOT =   SPACE )
                       AND   ( CNT-LINE    =   31 )
                           ADD     1       TO  CNT-LINE
                           MOVE   "次ページに続く"
                                           TO  HCM37-NAIYO(CNT-LINE)
                           PERFORM 400-PRINT-OUT-SEC
                           MOVE    ZERO        TO  CNT-LINE
                           PERFORM 210-HEAD-HEN-SEC
                       END-IF
      *
                       ADD     1           TO  CNT-LINE
                       MOVE    WRK-SJKBN   TO  WRK-KEY-SJKBN
                       EVALUATE    WRK-SJKBN
                           WHEN    "01"
                               MOVE    WRK-MOJI1
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "02"
                               MOVE    WRK-MOJI2
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "03"
                               MOVE    WRK-MOJI3
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "04"
                               MOVE    WRK-MOJI4
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "05"
                               MOVE    WRK-MOJI5
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "06"
                               MOVE    WRK-MOJI6
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "07"
                               MOVE    WRK-MOJI7
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "50"
                               IF      RECE10-SRYYM    <   201412
                                   MOVE    WRK-MOJI9
                                           TO  HCM37-NAIYO(CNT-LINE)
                               ELSE
                                   MOVE    WRK-MOJI13
                                           TO  HCM37-NAIYO(CNT-LINE)
                               END-IF
                           WHEN    "51"
                               MOVE    WRK-MOJI10
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "52"
                               MOVE    WRK-MOJI11
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "90"
                               MOVE    WRK-MOJI12
                                       TO  HCM37-NAIYO(CNT-LINE)
                           WHEN    "99"
                               MOVE    WRK-MOJI8
                                       TO  HCM37-NAIYO(CNT-LINE)
                       END-EVALUATE
                   END-IF
      *
                   IF      WRK-MOJI    NOT =   SPACE
                       IF    ( FLG-RECEDEN =   ZERO )
                       AND   ( CNT-LINE    =   31 )
                           ADD     1       TO  CNT-LINE
                           MOVE   "次ページに続く"
                                           TO  HCM37-NAIYO(CNT-LINE)
                           PERFORM 400-PRINT-OUT-SEC
                           MOVE    ZERO        TO  CNT-LINE
                           PERFORM 210-HEAD-HEN-SEC
                       END-IF
      *
                       ADD     1       TO  CNT-LINE
                       MOVE   WRK-MOJI TO  HCM37-NAIYO(CNT-LINE)
                       MOVE   SPACE    TO  WRK-MOJI
                   END-IF
      *
      *            帳票印刷処理
                   IF    ( FLG-RECEDEN =   1 )
                   OR    ( CNT-LINE    >=  32 )
                       PERFORM 400-PRINT-OUT-SEC
                       MOVE    ZERO        TO  CNT-LINE
                   END-IF

               END-PERFORM
      *
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    "tbl_onrece_body"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HCM37
      *
           ADD     1                   TO  CNT-PAGE
           ADD     1                   TO  CNT-GPAGE
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM37-PAGE
      *
      *    保険種別１
           EVALUATE    RECE10-RECESYUBETU (2:1)
           WHEN    "1"
               EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    1
                   MOVE    "１社"      TO  HCM37-HKNSBT1
               WHEN    2
                   MOVE    "１国"      TO  HCM37-HKNSBT1
               END-EVALUATE
           WHEN    "2"
               MOVE    "２公費"        TO  HCM37-HKNSBT1
           WHEN    "3"
               IF      RECE10-SRYYM    <   CONST-H2004
                   MOVE    "３老人"        TO  HCM37-HKNSBT1
               ELSE
                   MOVE    "３後期"        TO  HCM37-HKNSBT1
               END-IF
           WHEN    "4"
               MOVE    "４退職"        TO  HCM37-HKNSBT1
           END-EVALUATE
      *
      *    保険種別２
           EVALUATE    RECE10-RECESYUBETU (3:1)
           WHEN    "1"
               MOVE    "１単独"        TO  HCM37-HKNSBT2
           WHEN    "2"
               MOVE    "２２併"        TO  HCM37-HKNSBT2
           WHEN    "3"
               MOVE    "３３併"        TO  HCM37-HKNSBT2
           END-EVALUATE
      *
      *    本家族区分
           PERFORM 220-HONKZK-SET-SEC
      *
      *    診療年月
           MOVE    RECE10-SRYYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  HCM37-SRYM
      *
           IF      RECE10-SRYYM        NOT =   WRK-SYM
               MOVE    RECE10-SRYYM        TO  WRK-SYM
      *        システム管理基本情報読込
               PERFORM 900-SYS1001-READ-SEC
           END-IF
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  HCM37-HOSPCD
      *
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCM37-HOSPNAME
      *
      *    システム管理診療科読み込み
      *     PERFORM 900-SYS1005-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *        診療科
      *         MOVE    SYS-1005-SRYKANAME  TO  HCM37-SRYKA
      *     END-IF
      *
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
      *        患者氏名
               MOVE    PTINF-NAME          TO  HCM37-NAME
           END-IF
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *
      *----(05.00.00)--UPD-START---
      *     EVALUATE    WRK-HENYMDG(1:4)
      *     WHEN    "明治"
      *         MOVE    "○"                TO  HCM37-BIRTHG(1)
      *     WHEN    "大正"
      *         MOVE    "○"                TO  HCM37-BIRTHG(2)
      *     WHEN    "昭和"
      *         MOVE    "○"                TO  HCM37-BIRTHG(3)
      *     WHEN    "平成"
      *         MOVE    "○"                TO  HCM37-BIRTHG(4)
      *     END-EVALUATE
           MOVE    WRK-HENYMDG(1:4)       TO  HCM37-BIRTHGG
      *----(05.00.00)--UPD-END-----
      *
           MOVE    WRK-HENYMDG(5:4)       TO  HCM37-BIRTHY
           MOVE    WRK-HENYMDG(11:4)      TO  HCM37-BIRTHM
           MOVE    WRK-HENYMDG(17:4)      TO  HCM37-BIRTHD
      *
      *    入外区分
           EVALUATE    RECE10-NYUGAIKBN
           WHEN    "1"
               MOVE    "○"                TO  HCM37-NYUGAIKBN(1)
           WHEN    "2"
               MOVE    "○"                TO  HCM37-NYUGAIKBN(2)
           END-EVALUATE
      *
      *    保険者番号
           MOVE    RECE10-HKNJANUM     TO  HCM37-HKNJANUM
      *
      *    記号
           MOVE    RECE10-KIGO         TO  HCM37-KIGO
      *    番号
           MOVE    RECE10-NUM          TO  HCM37-NUM
      *
           IF      RECE10-SRYYM    <   CONST-H2004
               MOVE    "市町村番号："      TO  HCM37-RJNFTNJANAME
               MOVE    "老人受給者番号："  TO  HCM37-RJNJKYSNAME
      *        老人保険
               IF      RECE10-RJNHKNNUM    NOT =   SPACE
                   MOVE    SPACE               TO  WRK-JKYSNUM
                   IF      RECE10-RJNHKNID     NOT =   ZERO
                       MOVE    RECE10-RJNHKNID     TO  WRK-KOHID
                       PERFORM 900-PTKOH-READ-KOH-SEC
                   ELSE
                       MOVE    RECE10-RJNHKNNUM    TO  WRK-KOHNUM
                       PERFORM 900-PTKOH-READ-SEC
                   END-IF
                   IF      FLG-PTKOH           =   ZERO
                       MOVE    PTKOH-FTNJANUM      TO  HCM37-RJNFTNJANUM
                       MOVE    PTKOH-JKYSNUM       TO  WRK-JKYSNUM
                   END-IF
                   IF      WRK-JKYSNUM(8:)     NOT =   SPACE
                       MOVE    WRK-JKYSNUM         TO  HCM37-RJNJKYSNUM2
                   ELSE
                       MOVE    WRK-JKYSNUM(1:7)    TO  HCM37-RJNJKYSNUM
                   END-IF
               END-IF
           END-IF
      *
      *    保険公費
           PERFORM VARYING   IDX    FROM    1   BY   1
                   UNTIL    (IDX    >       4)
                   OR       (RECE10-KOHNUM(IDX) =   SPACE)
               IF      RECE10-KOHID (IDX)   NOT =   ZERO
                   MOVE    RECE10-KOHID (IDX)       TO  WRK-KOHID
                   PERFORM 900-PTKOH-READ-KOH-SEC
               ELSE
                   MOVE    RECE10-KOHNUM (IDX)      TO  WRK-KOHNUM
                   PERFORM 900-PTKOH-READ-SEC
               END-IF
               IF      FLG-PTKOH           =   ZERO
                   MOVE    SPACE               TO  WRK-JKYSNUM
      *        生保のとき月代わり受給者番号マスタより受給者番号をセット 
                   IF    ( PTKOH-KOHNUM    =   "012" )
      *                中国残留法人等
                       OR    ( PTKOH-KOHNUM    =   "025" )
                       INITIALIZE                  MONTHLYNUM-REC
                       MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
                       MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                       MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                       MOVE    LNK-PRTKANRI-SRYYM
                                               TO  MONTHLYNUM-SRYYM
                       MOVE    RECE10-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF      FLG-MONTHLYNUM  =   ZERO
                           MOVE    MONTHLYNUM-JKYSNUM  TO  WRK-JKYSNUM
                       ELSE
                           INITIALIZE                  MONTHLYNUM-REC
                           MOVE    SPA-HOSPNUM     TO
                                                   MONTHLYNUM-HOSPNUM
                           MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                           MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                           MOVE    LNK-PRTKANRI-SRYYM
                                                   TO  MONTHLYNUM-SRYYM
                           MOVE    RECE10-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF      FLG-MONTHLYNUM  =   ZERO
                               MOVE    MONTHLYNUM-JKYSNUM
                                                       TO  WRK-JKYSNUM
                           ELSE
                               MOVE    PTKOH-JKYSNUM   TO  WRK-JKYSNUM
                           END-IF
                       END-IF
                   ELSE
                       MOVE    PTKOH-JKYSNUM   TO  WRK-JKYSNUM
                   END-IF
                   IF      WRK-JKYSNUM(8:)     NOT =   SPACE
                       MOVE    WRK-JKYSNUM         TO  HCM37-KOHJKYSNUM2
                   ELSE
                       MOVE    WRK-JKYSNUM(1:7)    TO  HCM37-KOHJKYSNUM
                   END-IF
                   MOVE    PTKOH-FTNJANUM  TO  HCM37-KOHFTNJANUM
                   MOVE    4               TO  IDX
               END-IF
           END-PERFORM
      *
      *    カルテ番号
           MOVE    RECE10-PTNUM        TO  HCM37-DEMPNUM
      *
           .
       210-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <本人・家族>編集処理
      *****************************************************************
       220-HONKZK-SET-SEC          SECTION.
      *
      *    不明分・自費は編集しない
           IF      RECE10-RECESYUBETU  =   ZERO    OR  8888
               CONTINUE
           ELSE
               EVALUATE    RECE10-RECESYUBETU(4:1)
                   WHEN    "1"
                       MOVE    "１本入"        TO  HCM37-HONKZK
                   WHEN    "2"
                       MOVE    "２本外"        TO  HCM37-HONKZK
                   WHEN    "3"
                       IF      RECE10-SRYYM    <   CONST-H2004
                           MOVE    "３三入"        TO  HCM37-HONKZK
                       ELSE
                           MOVE    "３六入"        TO  HCM37-HONKZK
                       END-IF
                   WHEN    "4"
                       IF      RECE10-SRYYM    <   CONST-H2004
                           MOVE    "４三外"        TO  HCM37-HONKZK
                       ELSE
                           MOVE    "４六外"        TO  HCM37-HONKZK
                       END-IF
                   WHEN    "5"
                       MOVE    "５家入"        TO  HCM37-HONKZK
                   WHEN    "6"
                       MOVE    "６家外"        TO  HCM37-HONKZK
                   WHEN    "7"
                       IF      RECE10-SRYYM    <   CONST-H2004
                           MOVE    "７高入９"      TO  HCM37-HONKZK
                       ELSE
                           MOVE    "７高入一"      TO  HCM37-HONKZK
                       END-IF
                   WHEN    "8"
                       IF      RECE10-SRYYM    <   CONST-H2004
                           MOVE    "８高外９"      TO  HCM37-HONKZK
                       ELSE
                           MOVE    "８高外一"      TO  HCM37-HONKZK
                       END-IF
                   WHEN    "9"
                       IF      RECE10-SRYYM    <   "200610"
                           MOVE    "９高入８"      TO  HCM37-HONKZK
                       ELSE
                           MOVE    "９高入７"  TO  HCM37-HONKZK
                       END-IF
                   WHEN    "0"
                       IF      RECE10-SRYYM    <   "200610"
                           MOVE    "０高外８"      TO  HCM37-HONKZK
                       ELSE
                           MOVE    "０高外７"  TO  HCM37-HONKZK
                       END-IF
               END-EVALUATE
           END-IF    
      *
           .
       220-HONKZK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
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
      *----(05.00.00)--UPD-START---
      *     MOVE   "HCM37V01.red"       TO  SPRT-PRTID
           MOVE   "HCM37V02.red"       TO  SPRT-PRTID
      *----(05.00.00)--UPD-END-----
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "症状詳記（社保）"
                                           TO  SPRT-TITLE
               WHEN    "2"
                   MOVE    "症状詳記（国保）"
                                           TO  SPRT-TITLE
               WHEN    "3"
                   MOVE    "症状詳記（広域）"
                                           TO  SPRT-TITLE
               WHEN    "4"
                   MOVE    "症状詳記（国保、広域）"
                                           TO  SPRT-TITLE
           END-EVALUATE
           MOVE    HCM37               TO  SPRT-PRTDATA
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
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル 書き込みエラー 患者番号="
                                               DELIMITED  BY  SIZE
                      RECE10-PTNUM             DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
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
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
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
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-GPAGE       TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM530 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
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
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理基本情報読込
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           MOVE    WRK-SYM             TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報（基本）が取得できません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-SYS1900-READ-SEC        SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE               TO  SYS-1900-REC
           INITIALIZE                      SYS-1900-REC
           MOVE    "1900"              TO  SYS-1900-KANRICD
           MOVE    "*"                 TO  SYS-1900-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1900-STYUKYMD
                                           SYS-1900-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(20) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-SYSY1901-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-SYS1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-SYSY1901-READ-SEC       SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(20) TO  SYS-1901-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1901-STYUKYMD
                                           SYS-1901-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  SYS-1001-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-SYS1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルSELECT処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC  SECTION.
      *
           MOVE    ONRECE-B-REC    TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           ELSE
               INITIALIZE                  ONRECE-B-REC
               MOVE  1                 TO  FLG-END
           END-IF
           .
       900-ONRECE-BODY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルFETCH処理
      *****************************************************************
       900-ONRECE-BODY-FETCH-SEC   SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC       TO  ONRECE-B-REC
               MOVE  ZERO              TO  FLG-ONRECE-B
           ELSE
               INITIALIZE                  ONRECE-B-REC
               MOVE  1                 TO  FLG-END
           END-IF
           .
       900-ONRECE-BODY-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-SELECT-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN
      *     
           INITIALIZE                      RECEDEN-REC
           MOVE    ONRECE-B-HOSPNUM    TO  RECEDEN-HOSPNUM
           MOVE    ONRECE-B-SRYYM-B    TO  RECEDEN-SRYYM
           MOVE    ONRECE-B-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
           MOVE    ONRECE-B-PTID       TO  RECEDEN-PTID
           MOVE    ONRECE-B-RECEKA     TO  RECEDEN-RECEKA
           MOVE    ONRECE-B-TEISYUTUSAKI-B
                                       TO  RECEDEN-TEISYUTUSAKI
           MOVE    ONRECE-B-RECESYUBETU
                                       TO  RECEDEN-RECESYUBETU
           MOVE    ONRECE-B-HKNJANUM   TO  RECEDEN-HKNJANUM 
           MOVE    ONRECE-B-HOJOKBN    TO  RECEDEN-HOJOKBN-KEY
           MOVE    ONRECE-B-KOHID      TO  RECEDEN-KOHID-KEY
           MOVE    ONRECE-B-TEKSTYMD   TO  RECEDEN-TEKSTYMD
           MOVE    ONRECE-B-RECEIPT-KBN
                                       TO  RECEDEN-RECEIPT-KBN
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RECEDEN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECEDEN
           END-IF
           .
       900-RECEDEN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-RECEDENOK
           PERFORM UNTIL   FLG-RECEDENOK   =   1
                   OR      FLG-RECEDEN     =   1
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-RECEDEN
                   MOVE    MCPDATA-REC     TO  RECEDEN-REC
                   IF      RECEDEN-SJKBN   =   "01"
                                           OR  "02"
                                           OR  "03"
                                           OR  "04"
                                           OR  "05"
                                           OR  "06"
                                           OR  "07"
                                           OR  "50"
                                           OR  "51"
                                           OR  "52"
                                           OR  "90"
                                           OR  "99"
                       ADD     1               TO  CNT-RECEDEN
                       MOVE    1               TO  FLG-RECEDENOK
                   ELSE
                       MOVE    ZERO            TO  FLG-RECEDENOK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-RECEDENOK
                   MOVE    1               TO  FLG-RECEDEN
               END-IF
           END-PERFORM
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
      *
           INITIALIZE                      RECE10-REC
           MOVE    ONRECE-B-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    ONRECE-B-SRYYM-B    TO  RECE10-SRYYM
           MOVE    ONRECE-B-NYUGAIKBN  TO  RECE10-NYUGAIKBN
           MOVE    ONRECE-B-PTID       TO  RECE10-PTID
           MOVE    ONRECE-B-RECEKA     TO  RECE10-RECEKA
           MOVE    ONRECE-B-TEISYUTUSAKI-B
                                       TO  RECE10-TEISYUTUSAKI
           MOVE    ONRECE-B-RECESYUBETU
                                       TO  RECE10-RECESYUBETU
           MOVE    ONRECE-B-HKNJANUM   TO  RECE10-HKNJANUM 
           MOVE    ONRECE-B-HOJOKBN    TO  RECE10-HOJOKBN-KEY
           MOVE    ONRECE-B-KOHID      TO  RECE10-KOHID-KEY
           MOVE    ONRECE-B-TEKSTYMD   TO  RECE10-TEKSTYMD
           MOVE    ONRECE-B-RECEIPT-KBN
                                       TO  RECE10-RECEIPT-KBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                      RECE10-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-RECE10
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-RECE10-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター読込
      *****************************************************************
       900-PTKOH-READ-SEC          SECTION.
      *
           INITIALIZE                  PTKOH-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    RECE10-PTID     TO  PTKOH-PTID
           MOVE    WRK-KOHNUM      TO  PTKOH-KOHNUM
           MOVE    RECE10-TEKSTYMD TO  PTKOH-TEKSTYMD
           MOVE    RECE10-TEKSTYMD TO  PTKOH-TEKEDYMD
           MOVE    PTKOH-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-PTKOH-FETCH-SEC
           ELSE
               INITIALIZE                  PTKOH-REC
               MOVE    1               TO  FLG-PTKOH
           END-IF
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOH-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター読込（公費単独）
      *****************************************************************
       900-PTKOH-READ-KOH-SEC      SECTION.
      *
           INITIALIZE                  PTKOH-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    RECE10-PTID     TO  PTKOH-PTID
           MOVE    WRK-KOHID       TO  PTKOH-KOHID
           MOVE    PTKOH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-PTKOH-FETCH-SEC
           ELSE
               INITIALIZE                  PTKOH-REC
               MOVE    1               TO  FLG-PTKOH
           END-IF
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOH-READ-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター読込
      *****************************************************************
       900-PTKOH-FETCH-SEC             SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOH-REC
               MOVE    ZERO            TO  FLG-PTKOH
           ELSE
               INITIALIZE                  PTKOH-REC
               MOVE    1               TO  FLG-PTKOH
           END-IF
           .
       900-PTKOH-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC     SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_monthlynum"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC        SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
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
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
