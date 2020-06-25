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
       PROGRAM-ID.             ORCHC04SUB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来診療費明細書編集サブ（ＨＣＭ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/04/07    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  10/11/29  自賠責金額入力対応
      *  04.07.00    NACL-多々納  11/12/19  商品名コード単価対応
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      *  04.07.00    NACL-多々納  14/02/13  労災金額％加算対応
      *  04.07.00    NACL-多々納  14/10/14  コメント対象他追加
      *  05.00.00    NACL-太田    16/02/02  診療科名他追加
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０１年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PLUSKBN         PIC 9(01).
      *
           03  FLG-NEXT            PIC 9(01).
           03  FLG-MEISAI          PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDX-JYU             PIC 9(04).
           03  IDX-REN             PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-S               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-H-S             PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX-TBL2            PIC 9(04).
           03  IDX-HZN             PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDXM2               PIC 9(04).
           03  IDXD                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY             PIC X(03).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-SUM-TENSU           PIC 9(09)V99.
           03  WRK-SUM-KINGAKU         PIC 9(09).
      *
           03  WRK-KAISU               PIC 9(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-KINGAKU             PIC S9(11).
           03  WRK-ZAITENSU            PIC S9(11).
           03  WRK-SRYMEI              PIC X(10).
           03  WRK-SRYMEI-OLD          PIC X(10).
           03  WRK-TENSU-KEI           PIC S9(11).
      *
           03  WRK-Z119                PIC ZZZ,ZZZ,ZZ9.
           03  WRK-Z119-2              PIC ---,---,--9.
           03  WRK-Z1192               PIC ZZZZZZZZ9.
           03  WRK-Z1192-2             PIC --------9.
           03  WRK-Z7                  PIC ZZZZZZ9.
           03  WRK-Z7-2                PIC ------9.
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-NO                  PIC Z(7).
           03  WRK-WARI                PIC 9(02)V9.
           03  WRK-Z6                  PIC ZZZ.
           03  WRK-Z62                 PIC 9.9.
      *
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-X-R       REDEFINES   WRK-SURYO-X.
               05  WRK-SURYO-ZF    PIC X(06).
               05  WRK-SURYO-Z4    PIC ZZZZ9.
      *    文字変換
           03  WRK-ZENKAKU-G           PIC X(200).
           03  WRK-MOJI                PIC X(200).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-Z-SURYO             PIC X(11).
           03  WRK-K-SURYO             PIC X(22).
           03  WRK-ZENHEN-SURYO        PIC X(80).
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
           03  WRK-SYO-TEN             PIC 9(07)V99.
      *
           03  WRK-TNS-NAME            PIC X(200).
      *
       01  WRK-MEISAI-AREA.
           03  WRK-TBL-AREA            OCCURS   50.
               05  WRK-TBL-SRYCD        PIC X(09).
               05  WRK-TBL-NAME         PIC X(140).
               05  WRK-TBL-TANINAME     PIC X(10).
               05  WRK-TBL-SURYO        PIC 9(05)V9(5).
               05  WRK-TBL-JIHIMONEY    PIC 9(09).
               05  WRK-TBL-TEN          PIC 9(07).
               05  WRK-TBL-TENSIKIBETU  PIC 9(01).
      *
       01  TBL-DATA-AREA.
           03  TBL-DATA-TBL            OCCURS   200.
               05  TBL-SRYKBN          PIC X(02).
               05  TBL-SRYSYUKBN       PIC X(03).
               05  TBL-ZAITENSU        PIC S9(07).
               05  TBL-RSIKINGAKU      PIC S9(07).
               05  TBL-RSIKIN          PIC X(01).
               05  TBL-KAISU           PIC 9(03).
               05  TBL-ZAI-TBL         OCCURS   50.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-NAME            PIC X(140).
                   07  TBL-TANINAME        PIC X(10).
                   07  TBL-SURYO           PIC 9(05)V9(5).
                   07  TBL-JIHIMONEY       PIC S9(07).
                   07  TBL-TEN             PIC 9(07).
                   07  TBL-TENSIKIBETU     PIC 9(01).
      *
      *
       01  TBL-SORT-AREA.
           03  TBL-SORT-TBL            OCCURS   200.
               05  TBL-SORT-SRYKBN          PIC X(02).
               05  TBL-SORT-SRYSYUKBN       PIC X(03).
               05  TBL-SORT-ZAITENSU        PIC S9(07).
               05  TBL-SORT-RSIKINGAKU      PIC S9(07).
               05  TBL-SORT-RSIKIN          PIC X(01).
               05  TBL-SORT-KAISU           PIC 9(03).
               05  TBL-SORT-ZAI-TBL         OCCURS   50.
                   07  TBL-SORT-SRYCD           PIC X(09).
                   07  TBL-SORT-NAME            PIC X(140).
                   07  TBL-SORT-TANINAME        PIC X(10).
                   07  TBL-SORT-SURYO           PIC 9(05)V9(5).
                   07  TBL-SORT-JIHIMONEY       PIC S9(07).
                   07  TBL-SORT-TEN             PIC 9(07).
                   07  TBL-SORT-TENSIKIBETU     PIC 9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保存用収納ワーク
       01  TBL-SYUNOU-AREA.
         02  TBL-SYUNOU-REC        OCCURS   15.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //TBL-SYU-//.
      *    保存用収納ワーク
       01  HZN-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //HZN-SYU-//.
      *
      *    保存用受診履歴ワーク
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険組合せ名称編集サブ
           COPY    "CPORCSHKNMEI.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC04SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC04SUBAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCHC04SUB-INFO-AREA
           INITIALIZE                  ORCHC04SUB-LIST-AREA
           INITIALIZE                  HZN-JYURRK-REC
      *
      *    各ＤＢ読込
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *   主処理処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    伝票番号分編集処理
           PERFORM 2001-DENPNUM-SYORI-SEC
      *    診療区分毎にＳＯＲＴ
           PERFORM 2002-TBL-SORT-SEC
      *    伝票番号分テーブル編集
           PERFORM 2003-SUBTBL-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票番号毎印刷処理
      *****************************************************************
       2001-DENPNUM-SYORI-SEC          SECTION.
      *
           INITIALIZE                  TBL-DATA-AREA
           MOVE    ZERO                TO  IDX-TBL
      *    受診履歴検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    ORCHC04SUB-DENPNUM  TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
               MOVE    JYURRK-REC      TO  HZN-JYURRK-REC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM UNTIL    FLG-JYURRK     =   1
      *        受診履歴より編集
               PERFORM 2001-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( HZN-JYURRK-HOSPNUM  NOT =   ZERO )
               PERFORM 20011-INFO-HEN-SEC
           END-IF
      *
           .
       2012-DENPNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療科名等概要編集処理
      *****************************************************************
       20011-INFO-HEN-SEC              SECTION.
      *
           MOVE    HZN-JYURRK-SRYKA    TO  ORCHC04SUB-SRYKA
           MOVE    HZN-JYURRK-HKNCOMBINUM
                                       TO  ORCHC04SUB-HKNCOMBI
           MOVE    HZN-JYURRK-SRYYMD   TO  ORCHC04SUB-SRYYMD
      *
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    HZN-JYURRK-SRYKA    TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1
                                       TO  ORCHC04SUB-SRYKAMEI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           INITIALIZE                  ORCSHKNMEIAREA
           MOVE    HZN-JYURRK-HKNCOMBINUM
                                       TO  ORCSHKNMEI-HKNCOMBINUM
           MOVE    HZN-JYURRK-SRYYMD   TO  ORCSHKNMEI-SRYYMD
           MOVE    HZN-JYURRK-PTID     TO  ORCSHKNMEI-PTID
           CALL    "ORCSHKNMEI"        USING
                                       SPA-AREA
                                       ORCSHKNMEIAREA
           MOVE    ORCSHKNMEI-HKNCOMBIMEI
                                       TO  ORCHC04SUB-HKNCOMBIMEI
      *
           MOVE    HZN-JYURRK-SRYYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  ORCHC04SUB-SRYYMDMEI
      *
           .
       20011-INFO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴からの判定編集処理
      *****************************************************************
       2001-JYURRK-HEN-SEC          SECTION.
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU) =   ZERO )
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
      *ver.4.8
      ***      IF      IDX-REN         >   4
      *            MOVE    4               TO  IDX-REN
      ***      END-IF
               PERFORM 2002-SRYACCT-SYORI-SEC
           END-PERFORM
      *
           .
       2001-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACCT-SYORI-SEC      SECTION.
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-JYU)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *    回数ゼロ、点数ゼロ、コメントは対象外
           IF     (WRK-KAISU           =   ZERO )  OR
      *******     (ACCT-ZAITEN         =   ZERO )  OR
                  (ACCT-SRYKBN         =   "98"
                                       OR  "99" )
      *        包括分は対象外
               OR (ACCT-HKNCOMBI       =   "9999")
               OR (ACCT-JIHOKBN        =   "1"   )
               CONTINUE
           ELSE
               PERFORM 20021-SRYACT-SYORI-SEC
           END-IF
           .
       2002-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       20021-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
      *    自費金額・点数＝ゼロは対象外
           IF     (ACCT-ZAITEN         =   ZERO )  AND
                  (SRY-JIHIMONEYTOTAL  =   ZERO )  AND
                  (FLG-SRYACT          =   ZERO )
               MOVE    1                   TO  FLG-SRYACT
      *H26.10
      *        自費対象であれば、点数ゼロも対象とする
               IF      ORCHC04SUB-PARAJIHIKBN  =   "1"
                   IF     (ACCT-SRYKBN         =   "95"
                                               OR  "96" )
                       MOVE    ZERO            TO  FLG-SRYACT
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-MEISAI-AREA
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-SUM-TENSU
           MOVE    ZERO                TO  WRK-SUM-KINGAKU
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )   OR
                                  (SRY-SRYCD (IDXA)    =   SPACE)
      *            診療・薬剤・器材・部位・自費を対象とする
                   IF      (SRY-SRYCD (IDXA)(1:1)      =   "1"
                                                       OR  "6"
                                                       OR  "7" ) OR
                           (SRY-SRYCD (IDXA)(1:3)      =   "059"
                                                       OR  "002"
                                                       OR  "095"
                                                       OR  "096")
      *            器材商品名対象
                       OR  (SRY-SRYCD (IDXA)(1:3)      =   "058")
      *H26.10
      *            コメントコード対象
                       OR ((ORCHC04SUB-PARACOMKBN  =   "1"  )  AND
                           ((SRY-SRYCD (IDXA)(1:3)  =   "008") OR
                            (SRY-SRYCD (IDXA)(1:1)  =   "8"  )))
      *
                       PERFORM 200211-MEISE-HEN-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤ごとに、編集する
           IF      IDX-S               >   ZERO
               PERFORM 20023-TBL-HENSYU-SEC
           END-IF
           .
       20021-SRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *        明細編集処理
      *****************************************************************
       200211-MEISE-HEN-SEC      SECTION.
      *
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           ADD     1                   TO  IDX-S
           MOVE    SRY-SRYCD (IDXA)    TO  WRK-TBL-SRYCD (IDX-S)
      *
           MOVE    TNS-NAME            TO  WRK-TNS-NAME
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6" )
              OR  (TNSP-SHONAME        =   SPACE )
               CONTINUE
           ELSE
      *        薬剤以外で帳票名があれば帳票名称
               MOVE    TNSP-SHONAME        TO  WRK-TNS-NAME
           END-IF
      *
      *    コメント対象
           IF    ((SRY-SRYCD (IDXA)(1:1)   =   "8" )  OR
                  (SRY-SRYCD (IDXA)(1:3)   =   "008"))
              AND (SRY-INPUTNUM (IDXA) NOT =   ZERO )
               PERFORM 2002110-PTCOM-SET-SEC
           END-IF
      *
      *??  MOVE    TNS-NAME            TO  WRK-TBL-NAME  (IDX-S)
           MOVE    WRK-TNS-NAME        TO  WRK-TBL-NAME  (IDX-S)
      *
      *    器材商品名コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "058"
               MOVE    TNS-FORMALNAME      TO  WRK-TBL-NAME  (IDX-S)
      *ver.4.7 単位は器材コードで表示
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
               MOVE    TNS-TEN             TO  WRK-SYO-TEN
               MOVE    SPACE               TO  TNS-TANINAME
               MOVE    ZERO                TO  TNS-TANICD
           END-IF
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" ) AND
                  (SRY-MEISKYFLG  (IDXA)   =   "1" )
               MOVE    SPACE               TO  WRK-TBL-NAME  (IDX-S)
               STRING     "〔"             DELIMITED   BY  SIZE
                          TNS-NAME         DELIMITED   BY  "    "
                          "〕"             DELIMITED   BY  SIZE
                          INTO             WRK-TBL-NAME  (IDX-S)
               END-STRING
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
               IF      WRK-SYO-TEN         >   ZERO
                   MOVE    WRK-SYO-TEN     TO  TNS-TEN
               END-IF
      *
           END-IF
      *    単位
           MOVE    TNS-TANINAME        TO  WRK-TBL-TANINAME(IDX-S)
      *    数量
           MOVE    SRY-SRYSURYO(IDXA)  TO  WRK-TBL-SURYO (IDX-S)
           IF     (TNS-TANICD          =   ZERO
                                       OR  SPACE )
      *        器材、薬剤は単位なしでも表示（％加算、減算等は対象外）
               IF    ( SRY-SRYCD (IDXA)(1:1)   =   "6"
                                               OR  "7"  )  AND
                     ( TNS-TEN                 >   ZERO )  AND
                     ( TNS-TENSIKIBETU         =   1
                                               OR  2    )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  WRK-TBL-SURYO (IDX-S)
               END-IF
           END-IF
      *    初診・再診料は明細毎の点数
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
               CONTINUE
           ELSE
               IF     (TNS-TENSIKIBETU     =   3
                                           OR  1         )
                  AND (SRY-SRYCD (IDXA)(1:1)    =   "1"  )
                   MOVE    TNS-TEN             TO  WRK-TBL-TEN (IDX-S)
                   MOVE    TNS-TENSIKIBETU     TO  WRK-TBL-TENSIKIBETU
                                                            (IDX-S)
                   IF      TNS-TENSIKIBETU     =   3
                       ADD     TNS-TEN         TO  WRK-SUM-TENSU
                   ELSE
                       ADD     TNS-TEN         TO  WRK-SUM-KINGAKU
                   END-IF
               END-IF
      *        自賠責のその他
               IF     (SRY-SRYCD (IDXA)(1:4)   =   "0959" )
      *            自賠責金額入力を点数とする
                   IF     (SRY-SRYSYUKBN       =   "809")
                       MOVE    SRY-JIHIMONEY(IDXA)     TO  TNS-TEN
                   END-IF
                   MOVE    TNS-TEN             TO  WRK-TBL-TEN (IDX-S)
                   MOVE    TNS-TENSIKIBETU     TO  WRK-TBL-TENSIKIBETU
                                                            (IDX-S)
                   ADD     TNS-TEN             TO  WRK-SUM-KINGAKU
               END-IF
           END-IF
      *
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
      *        自費は１行１明細とする
               COMPUTE WRK-TBL-JIHIMONEY(IDX-S)
                                       =   SRY-JIHIMONEY(IDXA)
      *****    MOVE    ZERO                TO  WRK-TBL-SURYO (IDX-S)
           END-IF
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           .
       200211-MEISE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       2002110-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-TNS-NAME
           END-IF
           .
       2002110-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *        テーブル編集処理
      *****************************************************************
       20023-TBL-HENSYU-SEC      SECTION.
      *
           ADD     1                   TO  IDX-TBL
      *
           IF      ACCT-SRYKBN         =   "95"
                                       OR  "96"
               CONTINUE
           ELSE
               IF      ACCT-ZAIREQFLG      =   "1"
      *            附加情報あり
                   PERFORM 200231-SRYACCTPLUS-CHK-SEC
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU (IDX-TBL)
               END-IF
           END-IF
           MOVE    ACCT-SRYKBN         TO  TBL-SRYKBN (IDX-TBL)
      *    診療区分 ８０ の時、変更
           IF      ACCT-SRYKBN         =   "80"
               EVALUATE    WRK-SRYSYUKBN
                   WHEN    "800"
                       MOVE    "80"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "809"
                       MOVE    "86"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "820"
                       MOVE    "29"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "830"
                       MOVE    "83"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "840"
                       MOVE    "84"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "850"
                       MOVE    "85"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "890"
                       MOVE    "89"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    OTHER
                       MOVE    "80"        TO  TBL-SRYKBN (IDX-TBL)
               END-EVALUATE
           END-IF
           MOVE    WRK-SRYSYUKBN       TO  TBL-SRYSYUKBN(IDX-TBL)
           MOVE    WRK-KAISU           TO  TBL-KAISU (IDX-TBL)
      *
           MOVE    ZERO                TO  FLG-MEISAI
           IF     (WRK-SUM-TENSU       NOT =   ZERO )  AND
                  (WRK-SUM-KINGAKU     NOT =   ZERO )
      *        金額・点数混在時は、明細
               MOVE    1               TO  FLG-MEISAI
               MOVE    ZERO            TO  TBL-ZAITENSU (IDX-TBL)
           END-IF
           IF     (WRK-SUM-KINGAKU     =   ZERO )  AND
                  (WRK-SUM-TENSU       =   TBL-ZAITENSU (IDX-TBL))
      *        点数合計の時、初診・再診のみ明細
               IF      ACCT-SRYKBN         =   "11"
                                           OR  "12"
                   MOVE    1               TO  FLG-MEISAI
                   MOVE    ZERO            TO  TBL-ZAITENSU (IDX-TBL)
               END-IF
           END-IF
      *
           IF     (WRK-SUM-KINGAKU NOT =   ZERO )  AND
                  (WRK-SUM-TENSU       =   ZERO )
      *        自賠責の金額分
               MOVE    1               TO  FLG-MEISAI
               MOVE    ZERO            TO  TBL-ZAITENSU (IDX-TBL)
           END-IF
      *!!!
      *    労災金額
           IF     (WRK-SUM-KINGAKU NOT =   ZERO )  AND
                  (WRK-SUM-TENSU       =   ZERO )  AND
                  (TBL-RSIKINGAKU (IDX-TBL)    NOT =   ZERO )
               IF      ACCT-SRYKBN         =   "11"
                                           OR  "12"
                   CONTINUE
               ELSE
      *            初診・再診以外は１剤毎の金額とする 
                   MOVE    "1"             TO  TBL-RSIKIN   (IDX-TBL)
                   MOVE    ZERO            TO  TBL-ZAITENSU (IDX-TBL)
                   MOVE    ZERO            TO  FLG-MEISAI
      *            その他とする
                   IF     (ACCT-SRYKBN         =   "80"  )
                     AND  (WRK-SRYSYUKBN       =   "800" )
                       MOVE    "86"            TO  TBL-SRYKBN (IDX-TBL)
                   END-IF
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-S
               MOVE    WRK-TBL-SRYCD (IDX) TO  TBL-SRYCD (IDX-TBL IDX)
               MOVE    WRK-TBL-NAME  (IDX) TO  TBL-NAME  (IDX-TBL IDX)
               MOVE    WRK-TBL-TANINAME(IDX)
                                           TO  TBL-TANINAME(IDX-TBL IDX)
               MOVE    WRK-TBL-SURYO (IDX) TO  TBL-SURYO (IDX-TBL IDX)
               MOVE    WRK-TBL-JIHIMONEY (IDX)
                                           TO  TBL-JIHIMONEY
                                                        (IDX-TBL IDX)
      *
               IF      FLG-MEISAI          =   1
                   MOVE    WRK-TBL-TEN (IDX)   TO  TBL-TEN (IDX-TBL IDX)
                   MOVE    WRK-TBL-TENSIKIBETU (IDX)
                                               TO  TBL-TENSIKIBETU
                                                        (IDX-TBL IDX)
               END-IF
      *
               IF     (WRK-TBL-SRYCD (IDX)(1:4)    =   "0959" )  AND
                      (WRK-SRYSYUKBN           =   "800"  )  AND
                      (TBL-SRYKBN (IDX-TBL)    =   "80"   )
      *            自賠責ユーザコードは、その他とする
                   MOVE    "86"            TO  TBL-SRYKBN (IDX-TBL)
               END-IF
           END-PERFORM
      *
           .
       20023-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    附加情報編集処理
      *****************************************************************
       200231-SRYACCTPLUS-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCTPLUS-READ-SEC
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-PLUSKBN
           IF      FLG-SRYACCTPLUS     =   ZERO
               IF      ACCTP-PLUSKBN       =   1
      *            マイナス点数
                   COMPUTE TBL-ZAITENSU (IDX-TBL)
                                               =   ACCT-ZAITEN
                                               *   -1
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU(IDX-TBL)
               END-IF
      *!
      *        労災金額
               MOVE    ACCTP-RSIKINGAKU    TO  TBL-RSIKINGAKU (IDX-TBL)
           ELSE
               MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU(IDX-TBL)
           END-IF
      *
           .
       200231-SRYACCTPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルＳＯＲＴ処理
      *****************************************************************
       2002-TBL-SORT-SEC             SECTION.
      *
           INITIALIZE                  TBL-SORT-AREA
           MOVE    TBL-DATA-AREA       TO  TBL-SORT-AREA
           INITIALIZE                  TBL-DATA-AREA
           MOVE    ZERO                TO  IDX-TBL2
           MOVE    SPACE               TO  WRK-SRYKBN
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL     IDX       >   IDX-TBL
               MOVE    "99"            TO  WRK-SRYKBN
               MOVE    ZERO            TO  IDX-HZN
               PERFORM VARYING   IDX1      FROM   1   BY  1
                       UNTIL     IDX1      >   IDX-TBL
                   IF      TBL-SORT-SRYKBN (IDX1)   NOT =   SPACE
                       IF      TBL-SORT-SRYKBN (IDX1)  <   WRK-SRYKBN
                           MOVE    IDX1            TO  IDX-HZN
                           MOVE    TBL-SORT-SRYKBN (IDX1)
                                                   TO  WRK-SRYKBN
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDX-HZN             >   ZERO
                   ADD     1               TO  IDX-TBL2
                   MOVE    TBL-SORT-TBL (IDX-HZN)
                                           TO  TBL-DATA-TBL (IDX-TBL2)
                   INITIALIZE              TBL-SORT-TBL (IDX-HZN)
               END-IF
           END-PERFORM
           MOVE    IDX-TBL2            TO  IDX-TBL
           .
       20051-TBL-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称処理
      *****************************************************************
       200521-SRYSYUMEI-SEC             SECTION.
      *
           EVALUATE    TBL-SRYKBN (IDX)
               WHEN    "11"
               WHEN    "12"
                   MOVE    "初・再診料"        TO  WRK-SRYMEI
               WHEN    "13"
                   MOVE    "医学管理等"        TO  WRK-SRYMEI
               WHEN    "14"
                   MOVE    "在宅医療"          TO  WRK-SRYMEI
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "24"
               WHEN    "25"
               WHEN    "26"
               WHEN    "27"
               WHEN    "29"
                   MOVE    "投　　薬"          TO  WRK-SRYMEI
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    "注　　射"          TO  WRK-SRYMEI
               WHEN    "40"
                   MOVE    "処　　置"          TO  WRK-SRYMEI
               WHEN    "50"
                   MOVE    "手　　術"          TO  WRK-SRYMEI
               WHEN    "54"
                   MOVE    "麻　　酔"          TO  WRK-SRYMEI
               WHEN    "60"
                   MOVE    "検　　査"          TO  WRK-SRYMEI
               WHEN    "64"
                   MOVE    "病理診断"          TO  WRK-SRYMEI
               WHEN    "70"
                   MOVE    "画像診断"          TO  WRK-SRYMEI
               WHEN    "80"
                   MOVE    "リハビリ"          TO  WRK-SRYMEI
               WHEN    "83"
                   MOVE    "精神科専門"        TO  WRK-SRYMEI
               WHEN    "84"
                   MOVE    "放射線治療"        TO  WRK-SRYMEI
               WHEN    "85"
                   MOVE    "療養担当"          TO  WRK-SRYMEI
               WHEN    "89"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "86"
                   MOVE    "その他"            TO  WRK-SRYMEI
               WHEN    "90"
               WHEN    "92"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "97"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "95"
               WHEN    "96"
                   MOVE    "給付外"            TO  WRK-SRYMEI
               WHEN    OTHER
                   MOVE    "その他"            TO  WRK-SRYMEI
           END-EVALUATE
           .
       200521-SRYSYUMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票番号分テーブル編集処理
      *****************************************************************
       2003-SUBTBL-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  ORCHC04SUB-TBL-MAX
           MOVE    ZERO                TO  IDX-H
           MOVE    SPACE               TO  WRK-SRYMEI-OLD
           MOVE    ZERO                TO  WRK-TENSU-KEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-TBL )
                           OR (IDX-H   >=  800     )
               MOVE    ZERO                TO  IDX-H-S
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   50  )
                               OR (TBL-SRYCD (IDX IDX2)  =   SPACE)
                               OR (IDX-H   >=  800  )
                   ADD     1               TO  IDX-H
      *            診療区分
                   IF      IDX2                =   1
                       PERFORM 200521-SRYSYUMEI-SEC
                       IF      WRK-SRYMEI     NOT  =   WRK-SRYMEI-OLD
      *                    小計
                           IF     (ORCHC04SUB-PARAKBN  =   "1"  )
                           AND    (WRK-TENSU-KEI       >   ZERO )
                               PERFORM 20039-SYOKEI-HEN-SEC
                               ADD     1                   TO  IDX-H
                           END-IF
                           MOVE    WRK-SRYMEI
                                               TO  ORCHC04SUB-H-SRYKBN
                                                               (IDX-H)
                           MOVE    WRK-SRYMEI      TO  WRK-SRYMEI-OLD
                           MOVE    ZERO            TO  WRK-TENSU-KEI
                       END-IF
                   END-IF
      *            剤の１行目
                   IF      IDX-H-S         =   ZERO
                       MOVE    IDX-H           TO  IDX-H-S
                   END-IF
      *
                   MOVE    TBL-SRYKBN (IDX)    TO  ORCHC04SUB-SRYKBN
                                                              (IDX-H)
                   MOVE    TBL-SRYCD(IDX IDX2) TO  ORCHC04SUB-SRYCD
                                                              (IDX-H)
                   MOVE    TBL-SURYO(IDX IDX2) TO  ORCHC04SUB-SURYO
                                                              (IDX-H)
      *            剤開始
                   IF      IDX2                =   1
                       MOVE    "＊"            TO  ORCHC04SUB-H-ZAIHEN
                                                              (IDX-H)
                   ELSE
                       MOVE    "　"            TO  ORCHC04SUB-H-ZAIHEN
                                                              (IDX-H)
                   END-IF
      *            名称
                   MOVE    TBL-NAME (IDX IDX2) TO  ORCHC04SUB-NAME
                                                              (IDX-H)
      *            単位名称
                   MOVE    TBL-TANINAME (IDX IDX2)
                                               TO  ORCHC04SUB-TANINAME
                                                              (IDX-H)
      *            数量
                   IF      TBL-SURYO (IDX IDX2)    >   ZERO
                       MOVE    TBL-SURYO (IDX IDX2)   TO  WRK-SURYO
                       PERFORM 200521-SURYO-HEN-SEC
                       MOVE    WRK-SURYO-X     TO  ORCHC04SUB-H-SURYO
                                                              (IDX-H)
      *                数量＋単位名称（全角）
                       PERFORM 20052-SURYO-ZENHEN-SEC
                   END-IF
      *            明細点数
                   IF      TBL-TEN  (IDX IDX2) NOT =   ZERO
                       PERFORM 30051-TENSU-HEN-SEC
                   END-IF
                   IF      TBL-JIHIMONEY (IDX IDX2)   >   ZERO
      *                金額
                       MOVE    TBL-JIHIMONEY(IDX IDX2) TO  WRK-Z1192
                       MOVE    WRK-Z1192       TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
      *                合計
                       COMPUTE WRK-KINGAKU     =  (TBL-JIHIMONEY
                                                           (IDX IDX2)
                                               *   TBL-KAISU    (IDX) )
                       MOVE    WRK-KINGAKU     TO  WRK-Z119
                       MOVE    WRK-Z119        TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                       IF     (ORCHC04SUB-H-KINGAKU1 (IDX-H)
                                                   NOT =   SPACE  )
                        AND   (ORCHC04SUB-GOKEITANI    =   "1"    )
                           MOVE    WRK-KINGAKU     TO  WRK-Z1192
                           MOVE    WRK-Z1192   TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                       END-IF
      *                円
                       MOVE    "1"             TO  ORCHC04SUB-H-ENKBN
                                                              (IDX-H)
                       MOVE    "◇"            TO  ORCHC04SUB-H-ZAIHEN
                                                              (IDX-H)
                   END-IF
               END-PERFORM
               IF     (TBL-SRYKBN (IDX)    =   "95"
                                           OR  "96" )
               OR     (TBL-ZAITENSU (IDX)  =   ZERO )
                   CONTINUE
               ELSE
                   IF      ORCHC04SUB-GOKEITANI    =   "1"
      *                点数表示
                       MOVE    TBL-ZAITENSU (IDX)  TO  WRK-ZAITENSU
                   ELSE
      *                金額表示
                       COMPUTE WRK-ZAITENSU    =   TBL-ZAITENSU (IDX)
                                               *   10
                   END-IF
      *            点数
                   IF      WRK-ZAITENSU        >   ZERO
                       MOVE    WRK-ZAITENSU    TO  WRK-Z1192
                       MOVE    WRK-Z1192       TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
                   ELSE
                       MOVE    WRK-ZAITENSU    TO  WRK-Z1192-2
                       MOVE    WRK-Z1192-2     TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
                   END-IF
      *            合計
                   COMPUTE WRK-KINGAKU         =  (WRK-ZAITENSU
                                               *   TBL-KAISU    (IDX) )
                   IF      WRK-KINGAKU         >   ZERO
                       MOVE    WRK-KINGAKU     TO  WRK-Z119
                       MOVE    WRK-Z119        TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                   ELSE
                       MOVE    WRK-KINGAKU     TO  WRK-Z119-2
                       MOVE    WRK-Z119-2      TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                   END-IF
      *            剤点数
                   MOVE    TBL-ZAITENSU (IDX) TO  ORCHC04SUB-ZAITENSU
                                                              (IDX-H)
      *            点数　集計
                   COMPUTE WRK-TENSU-KEI       =   WRK-TENSU-KEI
                                               +   WRK-KINGAKU
               END-IF
      *!!!
               IF     (TBL-RSIKIN (IDX)    =   "1"  )
      *            労災円
                   MOVE    TBL-RSIKINGAKU (IDX)  TO  WRK-ZAITENSU
                   IF      WRK-ZAITENSU        >   ZERO
                       MOVE    WRK-ZAITENSU    TO  WRK-Z1192
                       MOVE    WRK-Z1192       TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
                   ELSE
                       MOVE    WRK-ZAITENSU    TO  WRK-Z1192-2
                       MOVE    WRK-Z1192-2     TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
                   END-IF
      *            合計
                   COMPUTE WRK-KINGAKU         =  (WRK-ZAITENSU
                                               *   TBL-KAISU    (IDX) )
                   IF      WRK-KINGAKU         >   ZERO
                       MOVE    WRK-KINGAKU     TO  WRK-Z119
                       MOVE    WRK-Z119        TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                   ELSE
                       MOVE    WRK-KINGAKU     TO  WRK-Z119-2
                       MOVE    WRK-Z119-2      TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
                   END-IF
      *            剤点数
                   MOVE    TBL-RSIKINGAKU (IDX)
                                           TO  ORCHC04SUB-ZAITENSU
                                                              (IDX-H)
      *            円
                   MOVE    "1"                 TO  ORCHC04SUB-H-ENKBN
                                                              (IDX-H)
               END-IF
      *!!!
      *
      *        回数
               MOVE    TBL-KAISU (IDX)     TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  ORCHC04SUB-H-KAISU
                                                            (IDX-H)
      *        回数
               MOVE    TBL-KAISU (IDX)     TO  ORCHC04SUB-KAISU
                                                            (IDX-H)
      *        剤終了
               MOVE    "1"                 TO  ORCHC04SUB-ZAIEND
                                                            (IDX-H)
           END-PERFORM
      *    小計
           IF     (ORCHC04SUB-PARAKBN  =   "1"  )
           AND    (WRK-TENSU-KEI       >   ZERO )
               ADD     1               TO  IDX-H
               PERFORM 20039-SYOKEI-HEN-SEC
           END-IF
           MOVE    IDX-H               TO  ORCHC04SUB-TBL-MAX
      *
           .
       2003-SUBTBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       30051-TENSU-HEN-SEC       SECTION.
      *
           IF      TBL-TENSIKIBETU (IDX IDX2)  =   3
      *        点数
               IF      ORCHC04SUB-GOKEITANI    =   "1"
      *            点数表示
                   COMPUTE WRK-ZAITENSU    =   TBL-TEN   (IDX IDX2)
               ELSE
      *            金額表示
                   COMPUTE WRK-ZAITENSU    =   TBL-TEN   (IDX IDX2)
                                           *   10
               END-IF
      *        点数
               IF      WRK-ZAITENSU        >   ZERO
                   MOVE    WRK-ZAITENSU    TO  WRK-Z1192
                   MOVE    WRK-Z1192       TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
               ELSE
                   MOVE    WRK-ZAITENSU    TO  WRK-Z1192-2
                   MOVE    WRK-Z1192-2     TO  ORCHC04SUB-H-TENSU
                                                              (IDX-H)
               END-IF
      *        合計
               COMPUTE WRK-KINGAKU         =  (WRK-ZAITENSU
                                           *   TBL-KAISU    (IDX) )
               IF      WRK-KINGAKU         >   ZERO
                   MOVE    WRK-KINGAKU         TO  WRK-Z119
                   MOVE    WRK-Z119            TO  ORCHC04SUB-H-KINGAKU
                                                             (IDX-H)
               ELSE
                   MOVE    WRK-KINGAKU         TO  WRK-Z119-2
                   MOVE    WRK-Z119-2          TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
               END-IF
      *        点数　集計
               COMPUTE WRK-TENSU-KEI       =   WRK-TENSU-KEI
                                           +   WRK-KINGAKU
           ELSE
      *        金額
               MOVE    TBL-TEN (IDX IDX2)  TO  WRK-ZAITENSU
               MOVE    WRK-ZAITENSU        TO  WRK-Z1192
               MOVE    WRK-Z1192           TO  ORCHC04SUB-H-TENSU
                                                            (IDX-H)
      *        円
               MOVE    "1"                 TO  ORCHC04SUB-H-ENKBN
                                                            (IDX-H)
           END-IF
      *    合計
           COMPUTE WRK-KINGAKU         =  (WRK-ZAITENSU
                                       *   TBL-KAISU    (IDX) )
           IF      WRK-KINGAKU         >   ZERO
               MOVE    WRK-KINGAKU         TO  WRK-Z119
               MOVE    WRK-Z119            TO  ORCHC04SUB-H-KINGAKU
                                                             (IDX-H)
           ELSE
               MOVE    WRK-KINGAKU         TO  WRK-Z119-2
               MOVE    WRK-Z119-2          TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
           END-IF
      *    コード点数
           MOVE    TBL-TEN (IDX IDX2)  TO  ORCHC04SUB-TENSU
                                                            (IDX-H)
      *    回数
           MOVE    TBL-KAISU (IDX)     TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  ORCHC04SUB-H-KAISU
                                                            (IDX-H)
      *    点数表示行
           MOVE    "＊"                TO  ORCHC04SUB-H-ZAIHEN
                                                              (IDX-H)
           .
       30051-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数　小計編集処理
      *****************************************************************
       20039-SYOKEI-HEN-SEC                  SECTION.
      *
           IF      WRK-TENSU-KEI           >   ZERO
               MOVE    WRK-TENSU-KEI       TO  WRK-Z119
               MOVE    WRK-Z119            TO  ORCHC04SUB-H-KINGAKU
                                                         (IDX-H)
           ELSE
               MOVE    WRK-TENSU-KEI       TO  WRK-Z119-2
               MOVE    WRK-Z119-2          TO  ORCHC04SUB-H-KINGAKU
                                                              (IDX-H)
           END-IF
           MOVE    WRK-TENSU-KEI       TO  ORCHC04SUB-ZAITENSU (IDX-H)
           MOVE    "XX"                TO  ORCHC04SUB-SRYKBN   (IDX-H)
      *
      *    剤終了
           IF      ORCHC04SUB-APIKBN   NOT =   "1"
               MOVE    SPACE           TO  ORCHC04SUB-ZAIEND
                                                            (IDX-H - 1 )
           END-IF
           MOVE    "1"                 TO  ORCHC04SUB-ZAIEND
                                                            (IDX-H)
           .
       20039-SYOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       200521-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM   FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1) TO  WRK-SURYO-Z3(IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           ELSE
               MOVE    SPACE               TO  WRK-SURYO-X
               MOVE    WRK-SURYO           TO  WRK-SURYO-Z4
           END-IF
      *
           .
       200521-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量＋単位名称（全角）編集処理
      *****************************************************************
       20052-SURYO-ZENHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-Z-SURYO
           MOVE    ZERO                TO  IDXM2
           PERFORM VARYING     IDXM    FROM    1   BY  1
                   UNTIL       IDXM    >   11
               IF      WRK-SURYO-X (IDXM:1) NOT =   SPACE
                   MOVE    WRK-SURYO-X(IDXM:)   TO  WRK-Z-SURYO
                   MOVE    11                   TO  IDXM
               END-IF
           END-PERFORM
           MOVE    WRK-Z-SURYO         TO  WRK-MOJI
           PERFORM 2009-KANACHK-SEC
           MOVE    WRK-ZENKAKU-G       TO  WRK-K-SURYO
      *
           MOVE    1                   TO  IDXM2
           MOVE    SPACE               TO  WRK-ZENHEN-SURYO
           PERFORM VARYING     IDXM    FROM    1   BY  2
                   UNTIL       IDXM    >   22
               IF      WRK-K-SURYO (IDXM:2)     =   SPACE
                                                OR  "　"
                   CONTINUE
               ELSE
                   MOVE    WRK-K-SURYO(IDXM:2)  TO  WRK-ZENHEN-SURYO
                                                         (IDXM2:2)
                   ADD     2                    TO  IDXM2
               END-IF
           END-PERFORM 
      *    単位名称
           PERFORM VARYING     IDXM    FROM    1   BY  2
                   UNTIL       IDXM    >   10
               IF      TBL-TANINAME (IDX IDX2) (IDXM:2)
                                                =   SPACE
                                                OR  "　"
                   CONTINUE
               ELSE
                   MOVE    TBL-TANINAME (IDX IDX2)(IDXM:2)
                                                TO  WRK-ZENHEN-SURYO
                                                         (IDXM2:2)
                   ADD     2                    TO  IDXM2
               END-IF
           END-PERFORM
           MOVE    WRK-ZENHEN-SURYO    TO  ORCHC04SUB-H-ZEN-SURYO
                                                          (IDX-H)
           COMPUTE ORCHC04SUB-H-ZENMAX (IDX-H) =   IDXM2   -   1
      *
           .
       20052-SURYO-ZENHEN-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-KANACHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ZENKAKU-G
           ELSE
               MOVE    WRK-MOJI            TO  WRK-ZENKAKU-G
           END-IF
      *
           .
       2009-KANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCTPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCTPLUS-REC
               MOVE    ZERO            TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       930-SRYACCTPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF     (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *R02.2
      *    ユーザー単位設定
           IF     (FLG-TENSUPLUS       =   ZERO)
              AND (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
      *
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
