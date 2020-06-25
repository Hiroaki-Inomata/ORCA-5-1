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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCVTHKNCOMBI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 保険組合せ作成処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/04/11    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-伊藤     01/12/12  ＤＢコールインターフェイス
      *                                     の変更  
      *  01.00.02    NACL-多々納  02/09/19  パラメタＦをＤＢパラメタへ変更
      *                                     Ｈ１４．１０改正対応
      *  01.00.03    NACL-多々納  02/11/29  前期高齢者対応
      *  01.00.04    NACL-多々納  03/05/12  保険期間外の対応（暫定）
      *  01.00.05    NACL-多々納  03/05/16  保険期間外の対応
      *  01.00.06    NACL-多々納  05/09/01  前期高齢者コンバート対応
      *  01.00.07    NACL-森脇    05/09/01  エラーメッセージ、コミット
      *  01.00.08    NACL-森脇    05/09/28  構造変更
      *  01.00.09    NACL-多々納  06/01/12  補助区分判定、メッセージ修正
      *  01.00.10    NACL-多々納  06/06/30  県番号設定追加（北海道用）
      *  02.00.01    NACL-多々納  06/07/27  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/16  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/12  保険番号マスタ期間変更削除
      *  04.02.00    NACL-多々納  08/04/28  後期高齢者対応
      *                                     期間変更削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  PARM-FILE       ASSIGN
                                   ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARM.
      *    患者情報ファイル
           SELECT  PTINF-FILE      ASSIGN
                                   ASS-PTINF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PTINF.
      *    出力エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-LST.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  PARM-FILE.
       01  PARM-REC.
           03  PARM-RECKBN             PIC X(01).
           03  PARM-DATKBN             PIC X(04).
           03  FILLER                  PIC X(01).
           03  PARM-DATA               PIC X(100).
      *
      *    患者情報ファイル
       FD  PTINF-FILE.
       01  PTINFW-REC.
           COPY    "CPPTINF.INC"       REPLACING
                                       //PTINF-// BY //PTINFW-//.
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC.
           03  ERR-REC1                PIC X(100).
           03  ERR-REC1-R  REDEFINES   ERR-REC1.
               05  ERR-LINENO          PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR-NAIYO           PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR-ERRNO           PIC X(03).
               05  ERR-PTNUMMES        PIC X(09).
               05  ERR-PTNUM           PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR-PTIDMES         PIC X(09).
               05  ERR-PTID            PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR-HKNNUMMES       PIC X(09).
               05  ERR-HKNNUM          PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR-HKNIDMES        PIC X(05).
               05  ERR-HKNID           PIC X(02).
               05  FILLER              PIC X(01).
               05  ERR-TEKSTEDMES      PIC X(05).
               05  ERR-TEKSTYMD        PIC X(08).
               05  ERR-TEKBAR          PIC X(01).
               05  ERR-TEKEDYMD        PIC X(08).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PARA                PIC X(256).
           03  ASS-PTINF               PIC X(256).
           03  ASS-ERR                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARM                PIC X(02).
           03  STS-PTINF               PIC X(02).
           03  STS-PARA                PIC X(02).
           03  STS-LST                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARM                PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-HKNCOOMBI           PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
      *
           03  FLG-RJN                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HENKOU              PIC 9(01).
      *
           03  FLG-KEN-OK              PIC 9(01).
           03  FLG-KEN-ARI             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARM                PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTINF               PIC 9(06).
           03  CNT-PTHKNINF            PIC 9(06).
           03  CNT-PTKOHINF            PIC 9(06).
           03  CNT-HKNCOMBI            PIC 9(06).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-09-4               PIC X(06).
           03  PARA-11-4               PIC X(06).
           03  PARA-HN-HOSPNUM         PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDH                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-TEKEDYMD            PIC X(08).
           03  WRK-HKNKOHSBTKBN        PIC X(02).
           03  WRK-NUM1                PIC 9(01).
           03  WRK-NUM2                PIC 9(02).
      *
           03  WRK-BASEYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-BYMD-70.
               05  WRK-B70-YY          PIC 9(04).
               05  WRK-B70-MM          PIC 9(02).
               05  WRK-B70-DD          PIC 9(02).
      *
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-HKNNUM      PIC X(03).
               05  WRK-KEN-TEKSTYMD    PIC X(08).
               05  WRK-KEN-TEKEDYMD    PIC X(08).
               05  WRK-KEN-ERFLG       PIC X(01).
               05  WRK-KEN-SBTKBN      PIC X(01).
               05  WRK-KEN-HOJOKBN     PIC X(01).
               05  WRK-KEN-CONTKBN     PIC X(01).
               05  WRK-KEN-SEIDO       PIC X(10).
               05  WRK-KEN-HKNKOH      PIC 9(01).
               05  WRK-KEN-PAYKBN      PIC X(02).
               05  WRK-KEN-ERFLG2      PIC X(01).
      *
      *    新組合せパラメタデータ用レイアウト
       01  NC01PARA-REC.
           03  PARA-NC01-REC           PIC X(500).
           03  PARA-NC01-FLG           PIC 9(01).
      *
      *    スパ領域
       01  SPA-AREA2.
           03  SPA-HKNID               PIC 9(10).
           03  SPA-KOHID               PIC 9(10).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTHKNCOMBI.CBL】".
               05  MES-TITLE1-YY       PIC X(04).
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02).
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02).
               05  FILLER              PIC X(20) VALUE SPACE.
               05  FILLER              PIC X(07) VALUE "HOSPID:".
               05  MES-TITLE1-HOSPID   PIC X(10).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(08) VALUE "HOSPSBT:".
               05  MES-TITLE1-HOSPSBT  PIC X(01).
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE "ROUPAYKBN:".
               05  MES-TITLE1-ROUPAY   PIC X(01).
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(10) VALUE "LINENO ERR".
                   07  FILLER          PIC X(42) VALUE
                   " [1]保険番号取得失敗  [2]保険情報不正公費 ".
                   07  FILLER          PIC X(44) VALUE
                   " [3]公費情報不正保険  [4]補助区分不整合".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]処理件数オーバー  [6]患者保険更新失敗 ".
                   07  FILLER          PIC X(44) VALUE
                   " [7]患者公費更新失敗  [8]保険組合せ出力失敗".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(10) VALUE "       MSG".
                   07  FILLER          PIC X(42) VALUE
                   " [A]保険期間変更追加  [B]保険開始期間変更 ".
                   07  FILLER          PIC X(41) VALUE
                   " [C]公費期間変更追加  [D]公費開始期間変更".
               05  MES-TITLE2-L4.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報基本情報）
           COPY    "CPSK1001.INC".
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    患者公費情報せ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険番号
       01  HZN-HKNNUM-AREA.
           02  HZN-HKNNUM-REC          OCCURS   2.
           COPY    "CPHKNNUM.INC"      REPLACING
                                       //HKN-// BY //HZN-HKN-//.
      *    保険番号テーブル
       01  TBL-HKNNUM-AREA.
           02  TBL-HKNNUM-REC          OCCURS  300.
           COPY    "CPHKNNUM.INC"      REPLACING
                                       //HKN-// BY //TBL-HKN-//.
       01  IDX-HKN-MAX                 PIC 9(04).
      *
      *    患者保険情報
       01  HZN-PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC"    REPLACING
                                       //PTHKN-// BY //HZN-PTHKN-//.
      *    患者公費情報せ
       01  HZN-PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC"    REPLACING
                                       //PTKOH-// BY //HZN-PTKOH-//.
      *
      *    パラメタ
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    パラメタ
       01  WRK-PARA-REC.
           COPY    "CPPARA.INC"        REPLACING
                                       //PARA-// BY //WRK-PARA-//.
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-PTINF   =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
      *
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           MOVE    ZERO            TO  SPA-AREA2
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    ASS-PARA
           END-UNSTRING
      *
      *    パラメータ情報取得
           PERFORM 101-PARM-GET-SEC
           IF  FLG-PARM            =   2
               MOVE    1           TO  FLG-PTINF
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HN-HOSPNUM    NUMERIC
               MOVE    PARA-HN-HOSPNUM     TO  SPA-HOSPNUM
           ELSE
               MOVE    1                   TO  FLG-PTINF
               DISPLAY "*(ORCVTHKNCOMBI)* HOSPNUM ERR ["
                                                 PARA-HN-HOSPNUM"]"
               GO  TO  100-INIT-EXT
           END-IF
      *
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYSYMD (3:)
           ADD     2000            TO  WRK-SYSYY
           MOVE    WRK-SYSYMD      TO  SPA-SYSYMD
           MOVE    SPA-SYSYMD(1:4) TO  MES-TITLE1-YY
           MOVE    SPA-SYSYMD(5:2) TO  MES-TITLE1-MM
           MOVE    SPA-SYSYMD(7:2) TO  MES-TITLE1-DD
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 110-CP1001-KENSAKU-SEC
           IF  FLG-PTINF           =   1
               GO  TO 100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 120-LST-INIT-SEC
           IF      FLG-PTINF       =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-09-4       TO  WRK-COMMIT
           MOVE    PARA-11-4       TO  WRK-ERR
      *
      *    保険番号テーブル設定
           PERFORM 1009-HKNNUM-TBL-SEC
           IF      FLG-PTINF       =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    患者情報　順ファイル処理
           PERFORM 900-PTINF-SEQUENCE-SEC
      *
           OPEN    INPUT           PTINF-FILE
           PERFORM 900-PTINF-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号テーブル設定
      *****************************************************************
       1009-HKNNUM-TBL-SEC            SECTION.
      *
           INITIALIZE                 TBL-HKNNUM-AREA
           MOVE    ZERO               TO  IDX-HKN-MAX
      *
           MOVE    SPACE               TO  HKNNUM-REC
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 950-HKNNUM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           PERFORM UNTIL      (FLG-HKNNUM  =   1 )
                           OR (IDX-HKN-MAX >=  300 )
               ADD     1                   TO  IDX-HKN-MAX
               MOVE    HKNNUM-REC          TO  TBL-HKNNUM-REC
                                             (IDX-HKN-MAX)
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 950-HKNNUM-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-HKN-MAX         =   ZERO
               MOVE    1                   TO  FLG-PTINF
               DISPLAY "*(ORCVTHKNCOMBI)* TBL-HKNNUM NASI ERR"
           END-IF
           .
       1009-HKNNUM-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得
      *****************************************************************
       101-PARM-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-PARM
           MOVE    SPACE           TO  PARA-SAVE
           OPEN    INPUT               PARM-FILE
           IF      STS-PARM        NOT =   ZERO
               MOVE    2               TO  FLG-PARM
               DISPLAY "*(ORCVTHKNCOMBI)* PARM-FILE OPN STS["
                        STS-PARM "]"
               GO  TO  101-PARM-GET-EXT
           END-IF
      *
           PERFORM 900-PARM-READ-SEC
      *
           PERFORM UNTIL
                   FLG-PARM  =  1
               IF  PARM-RECKBN         =   "@"
                   PERFORM 101-PARM-GET1-SEC
               END-IF
               PERFORM  900-PARM-READ-SEC
           END-PERFORM
      *
           CLOSE   PARM-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-09-4           IS  NUMERIC )   AND
                 ( PARA-11-4           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-PTINF
               GO  TO  101-PARM-GET-EXT
           END-IF
      *
           .
       101-PARM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得（１）
      *****************************************************************
       101-PARM-GET1-SEC           SECTION.
      *
           EVALUATE                PARM-DATKBN
             WHEN  "02-4"
                   MOVE  PARM-DATA TO  ASS-ERR
             WHEN  "09-4"
                 MOVE  PARM-DATA   TO  PARA-09-4
             WHEN  "11-4"
                 MOVE  PARM-DATA   TO  PARA-11-4
             WHEN    "HN"
                 MOVE  PARM-DATA   TO  PARA-HN-HOSPNUM
           END-EVALUATE
      *
           .
       101-PARM-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       110-CP1001-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
           MOVE    WRK-SYSYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF  FLG-SYSKANRI            =   1
               MOVE    1               TO  FLG-PTINF
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *        県番号
               MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       110-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       120-LST-INIT-SEC            SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-PTINF
               DISPLAY "*(ORCVTHKNCOMBI)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  120-LST-INIT-EXT
           END-IF
      *
           MOVE    SPA-HOSPID(1:10)    TO  MES-TITLE1-HOSPID
           MOVE    SPA-HOSPSBT         TO  MES-TITLE1-HOSPSBT
           MOVE    SPA-ROUPAYKBN       TO  MES-TITLE1-ROUPAY
           MOVE    MES-TITLE1          TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L4   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       120-LST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報　順ファイル処理
      *****************************************************************
       900-PTINF-SEQUENCE-SEC      SECTION.
      *
           MOVE    "/tmp/PTINF.txt"
                                   TO  ASS-PTINF
           OPEN    OUTPUT          PTINF-FILE
      *
           MOVE    ZERO            TO  FLG-PTINF
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 910-PTINF-NEXT-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           PERFORM UNTIL   FLG-PTINF   =   1
               MOVE    PTINF-REC       TO  PTINFW-REC
               WRITE   PTINFW-REC
      *
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 910-PTINF-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           CLOSE   PTINF-FILE
           MOVE    ZERO            TO  FLG-PTINF
      *
           .
       900-PTINF-SEQUENCE-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF  CNT-PTINF (4:3)         =   ZERO
               DISPLAY "***(ORCVTHKNCOMBI)*** CNT-PTINF[" CNT-PTINF "]"
           END-IF
      *    指定した件数になったらコミット処理する
           IF  CNT-COMMIT              =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC 
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理抜ける
           IF  ( WRK-ERR           NOT =   ZERO )  AND
               ( CNT-ERR               =   WRK-ERR )
               MOVE    1               TO  FLG-PTINF
               GO  TO  200-MAIN-EXT
           END-IF
           IF   ( PTINF-PTID    NOT =   ALL "0"   AND
                  PTINF-PTID    NOT =   SPACE )
      *         MOVE        PTNUM-PTID  TO  SPA-PTNUM
      *        作成処理
               PERFORM 200-SAKUSEI-SYORI-SEC
           END-IF
      *
           PERFORM 900-PTINF-READ-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成処理
      *****************************************************************
       200-SAKUSEI-SYORI-SEC       SECTION.
      *
           MOVE    PTINF-PTID      TO  SPA-PTID
      *
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM       =   ZERO
               MOVE    PTNUM-PTNUM     TO  SPA-PTNUM
           END-IF
      *
      *    既に登録済みの患者は対象外
           IF      PTNUM-AUTOCOMBINUM  >   ZERO
               GO      TO   200-SAKUSEI-SYORI-EXT
           END-IF
      *
           MOVE    ZERO            TO  IDX-AREA
           MOVE    ZERO            TO  SPA-AREA2
      *    ７０歳到達日
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    PTINF-BIRTHDAY  TO  WRK-BASEYMD
           IF      WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
      *    ７０歳を迎える日
           MOVE    WRK-BASEYMD     TO  WRK-BYMD-70
           COMPUTE WRK-B70-YY      =   WRK-B70-YY  +   70
      *
      *    ファイルオープン処理
           PERFORM 210-PARA-OPEN-SEC
      *
      *    患者保険情報　検索処理
           PERFORM 220-PTHKNINF-KENSAKU-SEC
      *
      *    患者公費情報　検索処理
           PERFORM 230-PTKOHINF-KENSAKU-SEC
      *
      *    組合せ編集処理
           CALL    "ORCGP02W2" USING   SPA-AREA
           IF  SPA-ERRCD           NOT =   SPACE
      *        エラーリスト出力処理
               PERFORM 2220-ERRLST-OUT-SEC
               GO  TO  200-SAKUSEI-SYORI-EXT
           END-IF
      *
      *    保険組合せ　登録処理
           PERFORM 240-HKNCOMBI-INSERT-SEC
      *
      *    患者番号変換マスタ　更新処理
           IF  IDZ                     >   ZERO
               PERFORM 250-PTNUM-UPDATE-SEC
           END-IF
      *
           .
       200-SAKUSEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルオープン処理
      *****************************************************************
       210-PARA-OPEN-SEC           SECTION.
      *
      *    パラメタ削除
           INITIALIZE                  PARA-REC
           MOVE    "P02"           TO  PARA-GYOUMUID
           MOVE    SPA-TERMID      TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
           MOVE    PARA-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       210-PARA-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報　検索処理
      *****************************************************************
       220-PTHKNINF-KENSAKU-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTHKNINF
           MOVE    SPACE           TO  PTHKNINF-REC
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID        TO  PTHKN-PTID
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           PERFORM   UNTIL   FLG-PTHKNINF  =   1
               ADD     1               TO  SPA-HKNID
               IF      PTHKN-SAKJOKBN      =   SPACE
                   INITIALIZE                  WRK-KENSAKU-AREA
                   MOVE    PTHKN-HKNNUM    TO  WRK-KEN-HKNNUM
                   MOVE    PTHKN-TEKSTYMD  TO  WRK-KEN-TEKSTYMD
                   MOVE    PTHKN-TEKEDYMD  TO  WRK-KEN-TEKEDYMD
                   MOVE    1               TO  WRK-KEN-HKNKOH
                   MOVE    ZERO            TO  WRK-KEN-PAYKBN
      *            保険確認処理
                   PERFORM 2210-HKNKOH-KAKUNIN-SEC
                   IF     (WRK-KEN-ERFLG       =   SPACE )  AND
                          (WRK-KEN-ERFLG2      =   SPACE )
      *                補助区分を再編集する
                       PERFORM 2201-HOJOKBN-SET-SEC
                   END-IF
                   IF     (WRK-KEN-ERFLG       =   SPACE )  AND
                          (WRK-KEN-ERFLG2      =   SPACE )
                       ADD     1                   TO  IDX
                       MOVE    SPACE               TO  PARA-REC
                       INITIALIZE                      PARA-REC
                       MOVE    "PTHKNINF"          TO  PARA-FILEMEI
                       MOVE    IDX                 TO  PARA-EDANUM
                       MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
                       PERFORM 400-PARA-INSERT-SEC
                   ELSE
      *                エラーリスト出力処理
                       PERFORM 2220-ERRLST-OUT-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       220-PTHKNINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前期高齢者の時、補助区分を再編集する処理
      *****************************************************************
       2201-HOJOKBN-SET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-HENKOU
      *    コンバート時に前期高齢者の時、補助区分を再編集する
           IF     (PTINF-BIRTHDAY      >   "19320930" ) AND
                  (WRK-BYMD-70         <=  WRK-SYSYMD ) AND
                  (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
               IF     (WRK-BYMD-70         >   PTHKN-TEKSTYMD )
      *            適用開始日が前期高齢者年齢以前
                   PERFORM 22012-HOJOKBN-CHK2-SEC
               ELSE
                   PERFORM 22011-HOJOKBN-CHK-SEC
               END-IF
           ELSE
               PERFORM 22012-HOJOKBN-CHK2-SEC
           END-IF
      *
           IF      FLG-HENKOU          =   1
      *        保険情報更新
               MOVE    PTHKNINF-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "*(ORCVTHKNCOMBI)*  PTHKNINF UPD ERR:" MCP-RC
                           ",KEY:" PTHKN-KEY
               END-IF
           END-IF
      *
           .
       2201-HOJOKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    前期高齢者の時、補助区分を再編集する処理
      *****************************************************************
       22011-HOJOKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HENKOU
      *    労災・自費・治験
      **** IF      WRK-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9"
           IF      WRK-HKNKOHSBTKBN    =   "4"  OR  "9"
               GO  TO  22011-HOJOKBN-CHK-EXT
           END-IF
      *    補助区分の内容チェック
           EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
      *            船員
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    "060"
               WHEN    "068"
      *            国保
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "0"
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
      *            共済
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "0"
                       WHEN    "2"
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    "039"
               WHEN    "040"
      *            後期高齢者
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "1"
                       WHEN    "3"
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
      *        自費
               WHEN    "980"   THRU   "989"
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    OTHER
      *            その他
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
           END-EVALUATE
      *
      **** EVALUATE    PTHKN-HKNNUM
      *        WHEN    "002"
      *            船員
      *            EVALUATE    PTHKN-HOJOKBN
      *                WHEN    "1"
      *                    MOVE    "A"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *                WHEN    "2"
      *                    MOVE    "B"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *                WHEN    "3"
      *                    MOVE    "C"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *                WHEN    "0"
      *                WHEN    SPACE
      *                    MOVE    "9"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *            END-EVALUATE
      *        WHEN    "060"
      *            国保
      *            EVALUATE    PTHKN-HOJOKBN
      *                WHEN    "1"
      *                WHEN    "2"
      *                WHEN    "4"
      *                WHEN    "5"
      *                    CONTINUE
      *                WHEN    OTHER
      *                    MOVE    "1"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *            END-EVALUATE
      *        WHEN    OTHER
      *            その他
      *            EVALUATE    PTHKN-HOJOKBN
      *                WHEN    "9"
      *                WHEN    "8"
      *                    CONTINUE
      *                WHEN    OTHER
      *                    MOVE    "9"         TO  PTHKN-HOJOKBN
      *                    MOVE    1           TO  FLG-HENKOU
      *            END-EVALUATE
      **** END-EVALUATE
      *
           .
       22011-HOJOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前期高齢者以外の時、補助区分のチェック処理
      *****************************************************************
        22012-HOJOKBN-CHK2-SEC     SECTION.
      *
      *    老人年齢、労災・自費・治験
      ***  IF     (PTINF-BIRTHDAY      <=  "19320930" ) OR
      ****        (WRK-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9")
      *    労災・治験
           IF     (WRK-HKNKOHSBTKBN    =   "4"  OR  "9")
               GO  TO  22012-HOJOKBN-CHK2-EXT
           END-IF
      *
           EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
      *            船員
                   IF     (PTINF-BIRTHDAY      <=  "19320930" )
                       IF     (PTHKN-TEKEDYMD      >=  "20080401" )
                           EVALUATE    PTHKN-HOJOKBN
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    SPACE
                               MOVE    "5"             TO  WRK-KEN-ERFLG
                           END-EVALUATE
                       END-IF
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    SPACE
                               CONTINUE
                           WHEN    OTHER
                               MOVE    "5"             TO  WRK-KEN-ERFLG
                       END-EVALUATE
                   END-IF
               WHEN    "060"
               WHEN    "068"
      *            国保
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
      *            共済
                   IF     (PTINF-BIRTHDAY      <=  "19320930" )
                       CONTINUE
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "2"
                           WHEN    SPACE
                               CONTINUE
                           WHEN    OTHER
                               MOVE    "5"             TO  WRK-KEN-ERFLG
                       END-EVALUATE
                   END-IF
               WHEN    "039"
               WHEN    "040"
      *            後期高齢者
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "1"
                       WHEN    "3"
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
      *        自費
               WHEN    "980"   THRU   "989"
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    SPACE
                           MOVE    "5"             TO  WRK-KEN-ERFLG
                   END-EVALUATE
               WHEN    OTHER
      *            その他
                   IF     (PTINF-BIRTHDAY      <=  "19320930" )
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    SPACE
                           WHEN    "9"
                           WHEN    "8"
                           WHEN    "7"
                               CONTINUE
                           WHEN    OTHER
                               MOVE    "5"             TO  WRK-KEN-ERFLG
                       END-EVALUATE
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "9"
                           WHEN    "8"
                           WHEN    "7"
                               MOVE    "5"             TO  WRK-KEN-ERFLG
                       END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           .
        22012-HOJOKBN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報　検索処理
      *****************************************************************
       230-PTKOHINF-KENSAKU-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTKOHINF
           MOVE    SPACE           TO  PTKOHINF-REC
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID        TO  PTKOH-PTID
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 930-PTKOHINF-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           PERFORM   UNTIL   FLG-PTKOHINF  =   1
               ADD     1               TO  SPA-KOHID
               IF  PTKOH-SAKJOKBN      =   SPACE
                   INITIALIZE                  WRK-KENSAKU-AREA
                   MOVE    PTKOH-KOHNUM    TO  WRK-KEN-HKNNUM
                   MOVE    PTKOH-TEKSTYMD  TO  WRK-KEN-TEKSTYMD
                   MOVE    PTKOH-TEKEDYMD  TO  WRK-KEN-TEKEDYMD
                   MOVE    PTKOH-PAYKBN    TO  WRK-KEN-PAYKBN
                   MOVE    2               TO  WRK-KEN-HKNKOH
      *            保険確認処理
                   PERFORM 2210-HKNKOH-KAKUNIN-SEC
                   IF     (WRK-KEN-ERFLG       =   SPACE )  AND
                          (WRK-KEN-ERFLG2      =   SPACE )
                       ADD     1               TO  IDY
                       MOVE    SPACE           TO  PARA-REC
                       INITIALIZE                  PARA-REC
                       MOVE    "PTKOHINF"      TO  PARA-FILEMEI
                       MOVE    IDY             TO  PARA-EDANUM
                       MOVE    PTKOHINF-REC    TO  PARA-DATA-REC
                       PERFORM 400-PARA-INSERT-SEC
                   ELSE
      *                エラーリスト出力処理
                       PERFORM 2220-ERRLST-OUT-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 930-PTKOHINF-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       230-PTKOHINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費確認処理
      *****************************************************************
       2210-HKNKOH-KAKUNIN-SEC     SECTION.
      *
           MOVE    SPACE           TO  HZN-HKNNUM-AREA
           INITIALIZE                  HZN-HKNNUM-AREA
           INITIALIZE                  HKNNUM-REC
      *
           MOVE    ZERO            TO  FLG-HKNNUM
           IF      WRK-KEN-PAYKBN      =   SPACE
               MOVE    ZERO            TO  WRK-KEN-PAYKBN
           END-IF
      *    老人公費で支払区分が設定されていない時、システム管理より
           IF     (WRK-KEN-PAYKBN      =    ZERO )  AND
                  (WRK-KEN-HKNNUM      =    "027")
               MOVE    SPA-HOSPSBT         TO  WRK-KEN-PAYKBN (1:1) 
               MOVE    SPA-ROUPAYKBN       TO  WRK-KEN-PAYKBN (2:1)
           END-IF
           MOVE    ZERO                TO  FLG-KEN-OK
           MOVE    ZERO                TO  FLG-KEN-ARI
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   IDX-HKN-MAX )
                          OR  (FLG-HKNNUM  =   1       )
               IF     (WRK-KEN-HKNNUM      =   TBL-HKN-HKNNUM (IDH))
                 AND  (WRK-KEN-PAYKBN      =   TBL-HKN-PAYKBN (IDH))
                   MOVE    1               TO  FLG-KEN-ARI
      *
                   IF     (WRK-KEN-TEKSTYMD    >=  TBL-HKN-TEKSTYMD
                                                             (IDH)) AND
                          (WRK-KEN-TEKEDYMD    <=  TBL-HKN-TEKEDYMD
                                                             (IDH))
                       MOVE    1                   TO  FLG-KEN-OK
                       MOVE    TBL-HKNNUM-REC (IDH)
                                                   TO  HKNNUM-REC
                       MOVE    1                   TO  FLG-HKNNUM
                       IF      TBL-HKN-HKNKOHSBTKBN(IDH)   =   "3"
      *                老人保険
                           MOVE    1               TO  FLG-RJN
                       ELSE
                           MOVE    TBL-HKN-HKNKOHSBTKBN(IDH)
                                                   TO  WRK-HKNKOHSBTKBN
                       END-IF
                   ELSE
      *            終了日が対象の時、対象とする
                       IF      ( WRK-KEN-TEKEDYMD  <=  TBL-HKN-TEKEDYMD
                                                                (IDH) )
                           MOVE    TBL-HKNNUM-REC(IDH)
                                                   TO  HZN-HKNNUM-REC(1)
                       ELSE
                           MOVE    TBL-HKNNUM-REC(IDH)
                                                   TO  HZN-HKNNUM-REC(2)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    老人公費で支払区分＝00の期間内はそのまま
           IF     (WRK-KEN-HKNNUM      =   "027" )  AND
                  (FLG-KEN-OK          =   ZERO  )  AND
                  (FLG-KEN-ARI         =   1     )
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL      (IDH     >   IDX-HKN-MAX )
                              OR  (FLG-KEN-OK  =   1     )
                   IF     (WRK-KEN-HKNNUM      =   TBL-HKN-HKNNUM
                                                          (IDH)) AND
                          (WRK-KEN-TEKSTYMD    >=  TBL-HKN-TEKSTYMD
                                                          (IDH)) AND
                          (WRK-KEN-TEKEDYMD    <=  TBL-HKN-TEKEDYMD
                                                         (IDH))
                       IF      TBL-HKN-PAYKBN (IDH)    =   "00"
                           MOVE    1               TO  FLG-KEN-OK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-KEN-OK      =   1
      *        対象あり
               MOVE    SPACE           TO  WRK-KEN-ERFLG
           ELSE
               IF      FLG-KEN-ARI     =   1
      *            期間外あり
                   MOVE    "1"             TO  WRK-KEN-ERFLG
               ELSE
      *            対象無し
                   MOVE    "2"             TO  WRK-KEN-ERFLG
               END-IF
           END-IF
      *
      *    MOVE    ZERO            TO  FLG-HKNNUM
      *    MOVE    SPACE           TO  HKNNUM-REC
      *    INITIALIZE                  HKNNUM-REC
      *    MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
      *    MOVE    WRK-KEN-HKNNUM  TO  HKN-HKNNUM
      *    IF      WRK-KEN-PAYKBN      =   SPACE
      *        MOVE    ZERO            TO  WRK-KEN-PAYKBN
      *    END-IF
      *    老人公費で支払区分が設定されていない時、システム管理より
      *    IF     (WRK-KEN-PAYKBN      =    ZERO )  AND
      *           (WRK-KEN-HKNNUM      =    "027")
      *        MOVE    SPA-HOSPSBT         TO  HKN-PAYKBN (1:1) 
      *        MOVE    SPA-ROUPAYKBN       TO  HKN-PAYKBN (2:1)
      *    ELSE
      *        MOVE    WRK-KEN-PAYKBN      TO  HKN-PAYKBN
      *    END-IF
      *
      *    MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *    MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *    MOVE    "key7"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *        MOVE    "key7"              TO  MCP-PATHNAME
      *        PERFORM 950-HKNNUM-NEXT-SEC
      *        MOVE    "1"             TO  WRK-KEN-ERFLG
      *    ELSE
      *        INITIALIZE              HKNNUM-REC
      *        MOVE    1               TO  FLG-HKNNUM
      *        MOVE    "2"             TO  WRK-KEN-ERFLG
      *    END-IF
      *
      *    MOVE    ZERO                TO  FLG-RJN
      *    MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
      *    PERFORM   UNTIL   FLG-HKNNUM    =   1
      *      IF  ( WRK-KEN-TEKSTYMD        >=  HKN-TEKSTYMD ) AND
      *          ( WRK-KEN-TEKEDYMD        <=  HKN-TEKEDYMD )
      *        IF  HKN-HKNKOHSBTKBN        =   "3"
      *            MOVE    1               TO  FLG-RJN
      **********   IF  (HKN-PAYKBN (1:1)   =   SPA-HOSPSBT)  AND
      ***********      (HKN-PAYKBN (2:1)   =   SPA-ROUPAYKBN)
      *              MOVE    SPACE         TO  WRK-KEN-ERFLG
      *              MOVE    1             TO  FLG-HKNNUM
      *********    ELSE
      *              PERFORM 950-HKNNUM-NEXT-SEC
      **********   END-IF
      *        ELSE
      *            MOVE    SPACE               TO  WRK-KEN-ERFLG
      *            MOVE    1                   TO  FLG-HKNNUM
      *            MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
      *        END-IF
      *      ELSE
      *        終了日が対象の時、対象とする
      *        IF      ( WRK-KEN-TEKEDYMD      <=  HKN-TEKEDYMD )
      *            MOVE    HKNNUM-REC          TO  HZN-HKNNUM-REC (1)
      *        ELSE
      *            MOVE    HKNNUM-REC          TO  HZN-HKNNUM-REC (2)
      *        END-IF
      *
      *        MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *        MOVE    "key7"              TO  MCP-PATHNAME
      *        PERFORM 950-HKNNUM-NEXT-SEC
      *      END-IF
      *    END-PERFORM
      *    MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *    MOVE    "key7"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    対象支払区分の老人が存在しない場合
      *    IF  (WRK-KEN-ERFLG          =   "1")  AND
      *        (FLG-RJN                =   1  )
      *        MOVE    ZERO            TO  FLG-HKNNUM
      *        MOVE    SPACE           TO  HKNNUM-REC
      *        MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
      *        MOVE    WRK-KEN-HKNNUM  TO  HKN-HKNNUM
      *        MOVE    HKNNUM-REC      TO  MCPDATA-REC
      *        MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *        MOVE    "key2"              TO  MCP-PATHNAME
      *        PERFORM 910-DBSELECT-SEC
      *        IF      MCP-RC             =   ZERO
      *            MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *            MOVE    "key2"              TO  MCP-PATHNAME
      *            PERFORM 950-HKNNUM-NEXT-SEC
      *        ELSE
      *            INITIALIZE             HKNNUM-REC
      *            MOVE    1              TO  FLG-HKNNUM
      *        END-IF
      *
      *        PERFORM   UNTIL   FLG-HKNNUM   =   1
      *          IF  ( WRK-KEN-TEKSTYMD       >=  HKN-TEKSTYMD ) AND
      *              ( WRK-KEN-TEKEDYMD       <=  HKN-TEKEDYMD )
      *              IF  HKN-HKNKOHSBTKBN     =   "3"
      *                  IF  (HKN-PAYKBN      =   "00")
      *                      MOVE    SPACE    TO  WRK-KEN-ERFLG
      *                      MOVE    1              TO  FLG-HKNNUM
      *                  ELSE
      *                      PERFORM 950-HKNNUM-NEXT-SEC
      *                  END-IF
      *              ELSE
      *                  PERFORM 950-HKNNUM-NEXT-SEC
      *              END-IF
      *          ELSE
      *              PERFORM 950-HKNNUM-NEXT-SEC
      *          END-IF
      *        END-PERFORM
      *        MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *        MOVE    "key2"              TO  MCP-PATHNAME
      *        PERFORM 990-DBCLOSE-SEC
      **** END-IF
      *
      *!!!!!!!!!!!!
      *!!  ORCVTPTHKNINF.CBL  ORCVTPTHKNINF.CBL で行うので
      *!!  対象無しはエラーとする
      *    対象の保険がなかった時、終了日の対象分とする
      *    IF     (WRK-KEN-ERFLG       NOT =   SPACE )  AND
      *           (HZN-HKN-TEKSTYMD(1) NOT =   SPACE )
      *!       MOVE    HZN-HKNNUM-REC          TO  HKNNUM-REC
      *!       MOVE    HKN-TEKSTYMD            TO  WRK-KEN-TEKSTYMD
      *        MOVE    SPACE                   TO  WRK-KEN-ERFLG
      *        EVALUATE    WRK-KEN-HKNKOH
      *            WHEN    1
      *                保険更新
      *                PERFORM 22101-HKNINF-UPD-SEC
      *            WHEN    2
      *                公費更新
      *                PERFORM 22102-KOHINF-UPD-SEC
      *        END-EVALUATE
      *****END-IF
      *
      *    対象の保険番号マスタが見つかった場合
           IF     (WRK-KEN-ERFLG       =   SPACE )  AND
                  (WRK-KEN-ERFLG2      =   SPACE )
      *        対象の保険番号が保険のもの
               IF  HKN-HKNKOHSBTKBN       =   "1"  OR  "4"
                                          OR  "8"  OR  "9"
      *        　　患者公費情報のデータ
                   IF  WRK-KEN-HKNKOH     =   2
                       MOVE    "4"        TO  WRK-KEN-ERFLG
                   END-IF
               ELSE
      *        対象の保険番号が公費のもの
      *        　　患者情報情報のデータ
                   IF  WRK-KEN-HKNKOH     =   1
                       MOVE    "3"        TO  WRK-KEN-ERFLG
                   END-IF
               END-IF
           END-IF
      *
           .
       2210-HKNKOH-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報更新処理
      *****************************************************************
       22101-HKNINF-UPD-SEC        SECTION.
      *
      *    対象の保険番号が保険であること
           IF      HZN-HKN-HKNKOHSBTKBN (1)
                                       =   "1"  OR  "4"
                                       OR  "8"  OR  "9"
                CONTINUE
           ELSE
               MOVE    "3"        TO  WRK-KEN-ERFLG
               GO  TO  22101-HKNINF-UPD-EXT
           END-IF
      *    保険変更があった時、終了日を変更する
           IF      HZN-HKN-TEKSTYMD (2)    NOT =   SPACE
              AND (HZN-HKN-TEKEDYMD (2)        >=  PTHKN-TEKSTYMD)
               MOVE    PTHKN-TEKEDYMD          TO  WRK-TEKEDYMD
               MOVE    HZN-HKN-TEKEDYMD (2)    TO  PTHKN-TEKEDYMD
      *        開始日が保険より前の時、開始日を保険の開始日にする
               IF      HZN-HKN-TEKSTYMD (2)    >   PTHKN-TEKSTYMD
                   MOVE    HZN-HKN-TEKSTYMD(2) TO  PTHKN-TEKSTYMD
               END-IF
               MOVE    HZN-HKN-HKNKOHSBTKBN (2)
                                           TO  WRK-HKNKOHSBTKBN
               MOVE    "A"                 TO  WRK-KEN-ERFLG2
      *        保険を更新する
               PERFORM 221011-PTHKNINF-UPD-SEC
      *        新しい保険を追加する
               MOVE    HZN-HKN-TEKSTYMD (1)    TO  PTHKN-TEKSTYMD
               MOVE    WRK-TEKEDYMD      TO  PTHKN-TEKEDYMD
               MOVE    HZN-HKN-HKNKOHSBTKBN (1)
                                           TO  WRK-HKNKOHSBTKBN
               PERFORM 221012-PTHKNINF-INS-SEC
           ELSE
      *        保険の開始日を変更する
               MOVE    HZN-HKN-TEKSTYMD (1)    TO  PTHKN-TEKSTYMD
               MOVE    "B"                 TO  WRK-KEN-ERFLG2
               MOVE    HZN-HKN-HKNKOHSBTKBN (1)
                                           TO  WRK-HKNKOHSBTKBN
               PERFORM 221011-PTHKNINF-UPD-SEC
           END-IF
      *
           .
       22101-HKNINF-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報更新処理
      *****************************************************************
       221011-PTHKNINF-UPD-SEC     SECTION.
      *
      *    前期高齢者の時、補助区分を再編集する
           IF     (PTINF-BIRTHDAY      >   "19320930" ) AND
                  (WRK-BYMD-70         <=  WRK-SYSYMD ) AND
                  (PTHKN-TEKEDYMD      >=  WRK-SYSYMD ) AND
                  (PTHKN-TEKSTYMD      >   "20021001" )
              AND (PTHKN-TEKSTYMD      >=  WRK-BYMD-70 )
               PERFORM 22011-HOJOKBN-CHK-SEC
           END-IF
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "6"             TO  WRK-KEN-ERFLG
               DISPLAY "*(ORCVTHKNCOMBI)*  PTHKNINF UPD ERR:" MCP-RC
                       ",KEY:" PTHKN-KEY
               GO  TO  221011-PTHKNINF-UPD-EXT
           END-IF
      *
      *    パラメタＤＢへ追加
           ADD     1               TO  IDX
           MOVE    SPACE           TO  PARA-REC
           INITIALIZE                  PARA-REC
           MOVE    "PTHKNINF"      TO  PARA-FILEMEI
           MOVE    IDX             TO  PARA-EDANUM
           MOVE    PTHKNINF-REC    TO  PARA-DATA-REC
           PERFORM 400-PARA-INSERT-SEC
      *
           .
       221011-PTHKNINF-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報追加処理
      *****************************************************************
       221012-PTHKNINF-INS-SEC     SECTION.
      *
      *    前期高齢者の時、補助区分を再編集する
           IF     (PTINF-BIRTHDAY      >   "19320930" ) AND
                  (WRK-BYMD-70         <=  WRK-SYSYMD ) AND
                  (PTHKN-TEKEDYMD      >=  WRK-SYSYMD ) AND
                  (PTHKN-TEKSTYMD      >   "20021001" )
              AND (PTHKN-TEKSTYMD      >=  WRK-BYMD-70 )
               PERFORM 22011-HOJOKBN-CHK-SEC
           END-IF
      *    保険ＩＤ設定
           PERFORM 2210013-PTNUM-UPD-SEC
           ADD     1               TO  SPA-HKNID
      *
           MOVE    PTNUM-HKNID     TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC    TO  HZN-PTHKNINF-REC
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-PTHKNINF-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTHKNINF        =   1
               MOVE    HZN-PTHKNINF-REC    TO  PTHKNINF-REC
      *
               MOVE    PTHKNINF-REC    TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "6"             TO  WRK-KEN-ERFLG
                   DISPLAY "*(ORCVTHKNCOMBI)*  PTHKNINF INS ERR:" MCP-RC
                           ",KEY:" PTHKN-KEY
                   GO  TO  221012-PTHKNINF-INS-EXT
               END-IF
           ELSE
               MOVE    "6"             TO  WRK-KEN-ERFLG
               DISPLAY "*(ORCVTHKNCOMBI)*  PTHKNINF INS ERR:" MCP-RC
                       ",KEY:" PTHKN-KEY
               GO  TO  221012-PTHKNINF-INS-EXT
           END-IF
      *
      *    パラメタＤＢへ追加
           ADD     1               TO  IDX
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    "PTHKNINF"          TO  PARA-FILEMEI
           MOVE    IDX                 TO  PARA-EDANUM
           MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
           PERFORM 400-PARA-INSERT-SEC
      *
           .
       221012-PTHKNINF-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報更新処理
      *****************************************************************
       2210013-PTNUM-UPD-SEC       SECTION.
      *
           PERFORM 900-PTNUM-READ-SEC
      *
           IF      FLG-PTNUM       =   ZERO
      *********MOVE        PTNUM-PTID  TO  SPA-PTNUM
               EVALUATE    WRK-KEN-HKNKOH
                   WHEN    1
      *                保険更新
                       COMPUTE PTNUM-HKNID     =   PTNUM-HKNID
                                               +   1
                   WHEN    2
      *                公費更新
                       COMPUTE PTNUM-KOHID     =   PTNUM-KOHID
                                               +   1
               END-EVALUATE
      *
               MOVE    SPA-SYSYMD      TO  PTNUM-UPYMD
               MOVE    PTNUM-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "*(ORCVTHKNCOMBI)*  PTNUM UPD ERR:" MCP-RC
                           ",KEY:"  PTNUM-KEY
               END-IF
           END-IF
      *
           .
       2210013-PTNUM-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報更新処理
      *****************************************************************
       22102-KOHINF-UPD-SEC        SECTION.
      *
      *    対象の保険番号が保険であること
           IF      HZN-HKN-HKNKOHSBTKBN (1)
                                       =   "1"  OR  "4"
                                       OR  "8"  OR  "9"
               MOVE    "4"             TO  WRK-KEN-ERFLG
               GO  TO  22102-KOHINF-UPD-EXT
           END-IF
      *    保険変更があった時、終了日を変更する
           IF      HZN-HKN-TEKSTYMD (2)    NOT =   SPACE
              AND (HZN-HKN-TEKEDYMD (2)        >=  PTKOH-TEKSTYMD)
               MOVE    PTKOH-TEKEDYMD          TO  WRK-TEKEDYMD
               MOVE    HZN-HKN-TEKEDYMD (2)    TO  PTKOH-TEKEDYMD
      *        開始日が保険より前の時、開始日を保険の開始日にする
               IF      HZN-HKN-TEKSTYMD (2)    >   PTKOH-TEKSTYMD
                   MOVE    HZN-HKN-TEKSTYMD(2) TO  PTKOH-TEKSTYMD
               END-IF
               MOVE    HZN-HKN-HKNKOHSBTKBN (2)
                                           TO  WRK-HKNKOHSBTKBN
               MOVE    "C"                 TO  WRK-KEN-ERFLG2
      *        保険を更新する
               PERFORM 221021-PTKOHINF-UPD-SEC
      *        新しい保険を追加する
               MOVE    HZN-HKN-TEKSTYMD (1)    TO  PTKOH-TEKSTYMD
               MOVE    WRK-TEKEDYMD      TO  PTKOH-TEKEDYMD
               MOVE    HZN-HKN-HKNKOHSBTKBN (1)
                                           TO  WRK-HKNKOHSBTKBN
               PERFORM 221022-PTKOHINF-INS-SEC
           ELSE
      *        保険の開始日を変更する
               MOVE    HZN-HKN-TEKSTYMD (1)    TO  PTKOH-TEKSTYMD
               MOVE    "D"                 TO  WRK-KEN-ERFLG2
               MOVE    HZN-HKN-HKNKOHSBTKBN (1)
                                           TO  WRK-HKNKOHSBTKBN
      ****     PERFORM 221022-PTKOHINF-INS-SEC
               PERFORM 221021-PTKOHINF-UPD-SEC
           END-IF
      *
           .
       22102-KOHINF-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報更新処理
      *****************************************************************
       221021-PTKOHINF-UPD-SEC     SECTION.
      *
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "7"             TO  WRK-KEN-ERFLG
               DISPLAY "*(ORCVTHKNCOMBI)*  PTKOHINF UPD ERR:" MCP-RC
                       ",KEY:" PTKOH-KEY
               GO  TO  221021-PTKOHINF-UPD-EXT
           END-IF
      *
      *    パラメタＤＢへ追加
           ADD     1               TO  IDY
           MOVE    SPACE           TO  PARA-REC
           INITIALIZE                  PARA-REC
           MOVE    "PTKOHINF"      TO  PARA-FILEMEI
           MOVE    IDY             TO  PARA-EDANUM
           MOVE    PTKOHINF-REC    TO  PARA-DATA-REC
           PERFORM 400-PARA-INSERT-SEC
      *
           .
       221021-PTKOHINF-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報追加処理
      *****************************************************************
       221022-PTKOHINF-INS-SEC     SECTION.
      *
      *    保険ＩＤ設定
           PERFORM 2210013-PTNUM-UPD-SEC
           ADD     1               TO  SPA-KOHID
      *
           MOVE    PTNUM-KOHID     TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC    TO  HZN-PTKOHINF-REC
      *
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 930-PTKOHINF-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHINF        =   1
               MOVE    HZN-PTKOHINF-REC    TO  PTKOHINF-REC
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "7"             TO  WRK-KEN-ERFLG
                   DISPLAY "*(ORCVTHKNCOMBI)*  PTKOHINF INS ERR:" MCP-RC
                           ",KEY:" PTHKN-KEY
                   GO  TO  221022-PTKOHINF-INS-EXT
               END-IF
           ELSE
               MOVE    "7"             TO  WRK-KEN-ERFLG
               DISPLAY "*(ORCVTHKNCOMBI)*  PTKOHINF INS ERR:" MCP-RC
                       ",KEY:" PTHKN-KEY
               GO  TO  221022-PTKOHINF-INS-EXT
           END-IF
      *
      *    パラメタＤＢへ追加
           ADD     1                   TO  IDY
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    "PTKOHINF"          TO  PARA-FILEMEI
           MOVE    IDY                 TO  PARA-EDANUM
           MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
           PERFORM 400-PARA-INSERT-SEC
      *
      *
           .
       221022-PTKOHINF-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC         SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"           TO  PARA-GYOUMUID
           MOVE    SPA-TERMID      TO  PARA-TERMID
           MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
      *
           MOVE    PARA-REC        TO  WRK-PARA-REC
      *
           MOVE    PARA-REC        TO  MCPDATA-REC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PARA        =   1
               MOVE    WRK-PARA-REC    TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "*(ORCVTHKNCOMBI)*  PARA INSERT ERR:" MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    WRK-PARA-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "*(ORCVTHKNCOMBI)*  PARA UPDATE ERR:" MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       2220-ERRLST-OUT-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-ERR
      *
           IF  SPA-ERRCD           =   SPACE
               EVALUATE    WRK-KEN-ERFLG
      *        該当する保険番号なし
               WHEN    "1"
               WHEN    "2"
                   MOVE    1           TO  FLG-ERR
      *        患者保険マスタに公費が入っている場合
               WHEN    "3"
                   MOVE    2           TO  FLG-ERR
      *        患者公費マスタに保険が入っている場合
               WHEN    "4"
                   MOVE    3           TO  FLG-ERR
      ******** 前期高齢者の補助区分が入っている場合
      *        補助区分が違う場合
               WHEN    "5"
                   MOVE    4           TO  FLG-ERR
      *        患者保険マスタ更新エラー
               WHEN    "6"
                   MOVE    6           TO  FLG-ERR
      *        患者公費マスタ更新エラー
               WHEN    "7"
                   MOVE    7           TO  FLG-ERR
      *        保険組合せ出力エラー
               WHEN    "8"
                   MOVE    8           TO  FLG-ERR
               END-EVALUATE
           ELSE
      *        サブプロ（ORCGP02W2）内でエラー発生
               EVALUATE    SPA-ERRCD
      *        保険・公費テーブル１００件オーバー
               WHEN    "0015"
                   MOVE    5           TO  FLG-ERR
      *        保険・公費　該当なし
               WHEN    "0001"
               WHEN    "0014"
                   MOVE    1           TO  FLG-ERR
               END-EVALUATE
           END-IF
      *
           ADD     1               TO  CNT-ERR
           MOVE    SPACE           TO  ERR-REC
           MOVE    "000000"        TO  ERR-LINENO
      *
           MOVE    "患者番号:"         TO  ERR-PTNUMMES
           MOVE    "患者ＩＤ:"         TO  ERR-PTIDMES
           MOVE    "保険番号:"         TO  ERR-HKNNUMMES
           MOVE    "連番:"             TO  ERR-HKNIDMES
           MOVE    "期間:"             TO  ERR-TEKSTEDMES
      *
           MOVE    SPA-PTNUM           TO  ERR-PTNUM
           MOVE    SPA-PTID            TO  ERR-PTID
      *
           IF      WRK-KEN-HKNNUM      NOT =   SPACE
               MOVE    WRK-KEN-HKNNUM      TO  ERR-HKNNUM
               EVALUATE    WRK-KEN-HKNKOH
                   WHEN    1
                   IF      SPA-HKNID       <   10
                       MOVE    SPA-HKNID       TO  WRK-NUM1
                       MOVE    WRK-NUM1        TO  ERR-HKNID
                   ELSE
                       MOVE    SPA-HKNID       TO  WRK-NUM2
                       MOVE    WRK-NUM2        TO  ERR-HKNID
                   END-IF
                   WHEN    2
                   IF      SPA-KOHID       <   10
                       MOVE    SPA-KOHID       TO  WRK-NUM1
                       MOVE    WRK-NUM1        TO  ERR-HKNID
                   ELSE
                       MOVE    SPA-KOHID       TO  WRK-NUM2
                       MOVE    WRK-NUM2        TO  ERR-HKNID
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( WRK-KEN-TEKSTYMD    NOT =   SPACE ) AND
                 ( WRK-KEN-TEKEDYMD    NOT =   SPACE )
               MOVE    WRK-KEN-TEKSTYMD    TO  ERR-TEKSTYMD
               MOVE    WRK-KEN-TEKEDYMD    TO  ERR-TEKEDYMD
               MOVE    "-"                 TO  ERR-TEKBAR
           END-IF
      *
           IF      FLG-ERR         =   0
      *        期間変更
               IF      WRK-KEN-ERFLG2  NOT =   SPACE
                   MOVE    "["                 TO  ERR-ERRNO(1:1)
                   MOVE    WRK-KEN-ERFLG2(1:1) TO  ERR-ERRNO(2:1)
                   MOVE    "]"                 TO  ERR-ERRNO(3:1)
                   MOVE    "MSG"               TO  ERR-NAIYO
               END-IF
           ELSE
               MOVE    "["                 TO  ERR-ERRNO(1:1)
               MOVE    FLG-ERR (1:1)       TO  ERR-ERRNO(2:1)
               MOVE    "]"                 TO  ERR-ERRNO(3:1)
               MOVE    "ERR"               TO  ERR-NAIYO
           END-IF
      *
           WRITE   ERR-REC
      *
           MOVE    SPACE               TO  WRK-KEN-ERFLG
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-ERR
      *
           .
       2220-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ　登録処理
      *****************************************************************
       240-HKNCOMBI-INSERT-SEC     SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    "P02"           TO  PARA-GYOUMUID
           MOVE    SPA-TERMID      TO  PARA-TERMID
           MOVE    "NC01PARA"      TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
           MOVE    PARA-REC        TO  MCPDATA-REC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL   FLG-PARA        =  1
      *
               MOVE    PARA-DATA-REC       TO  NC01PARA-REC
      *        組合せ番号　設定処理
               PERFORM 2410-HKNCOMBINUM-SET-SEC
      *
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       240-HKNCOMBI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ番号　設定処理
      *****************************************************************
       2410-HKNCOMBINUM-SET-SEC    SECTION.
      *
           MOVE    SPACE           TO  HKNCOMBI-REC
           MOVE    PARA-NC01-REC   TO  HKNCOMBI-REC
           ADD     1               TO  IDZ
           MOVE    IDZ             TO  COMB-HKNCOMBINUM
      *
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "*(ORCVTHKNCOMBI)* HKNCOMBI INS-ERR[" MCP-RC "]"
               MOVE    "8"            TO  WRK-KEN-ERFLG
               PERFORM 2220-ERRLST-OUT-SEC
               GO  TO  2410-HKNCOMBINUM-SET-EXT
           END-IF
           ADD     1               TO  CNT-HKNCOMBI
      *
           .
       2410-HKNCOMBINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       250-PTNUM-UPDATE-SEC        SECTION.
      *
           PERFORM 900-PTNUM-READ-SEC
      *
           IF      FLG-PTNUM       =   1
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  WRK-KEN-ERFLG
           ELSE
               MOVE    IDZ             TO  PTNUM-AUTOCOMBINUM
      *!       IF  SPA-HKNID       NOT =   PTNUM-HKNID
               IF  SPA-HKNID           >   PTNUM-HKNID
                   MOVE    SPA-HKNID   TO  PTNUM-HKNID
               END-IF
      *!       IF  SPA-KOHID       NOT =   PTNUM-KOHID
               IF  SPA-KOHID           >   PTNUM-KOHID
                   MOVE    SPA-KOHID   TO  PTNUM-KOHID
               END-IF
               MOVE    SPA-SYSYMD      TO  PTNUM-UPYMD
               MOVE    PTNUM-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "*(ORCVTHKNCOMBI)* PTNUM UPD-ERR PTID["
                           SPA-PTID "]MCP-RC[" MCP-RC "]"
               END-IF
           END-IF
      *
           .
       250-PTNUM-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
           CLOSE                       PTINF-FILE
      *    帳票出力　終了処理
           CLOSE                       ERR-FILE
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS-PTINF       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           DISPLAY "*(ORCVTHKNCOMBI)* PTINF   /I CNT[" CNT-PTINF    "]"
           DISPLAY "*(ORCVTHKNCOMBI)* PTHKNINF/I CNT[" CNT-PTHKNINF "]"
           DISPLAY "*(ORCVTHKNCOMBI)* PTKOHINF/I CNT[" CNT-PTKOHINF "]"
           DISPLAY "*(ORCVTHKNCOMBI)* HKNCOMBI/O CNT[" CNT-HKNCOMBI "]"
           DISPLAY "*(ORCVTHKNCOMBI)* ERR     /O CNT[" CNT-ERR      "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報　読み込み処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           READ    PTINF-FILE
                   AT  END
                       MOVE  1     TO  FLG-PTINF
                   NOT AT  END
                       MOVE  PTINFW-REC
                                   TO  PTINF-REC
                       ADD   1     TO  CNT-PTINF
                       ADD   1     TO  CNT-COMMIT
           END-READ
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＦＥＴＣＨ処理
      *****************************************************************
       900-SYSTEM-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC             =   ZERO
               MOVE    ZERO           TO  FLG-SYSKANRI
           ELSE
               MOVE    1              TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタＦＥＴＣＨ処理
      *****************************************************************
       910-PTINF-NEXT-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           .
       910-PTINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスタＦＥＴＣＨ処理
      *****************************************************************
       920-PTHKNINF-NEXT-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
               ADD     1               TO  CNT-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       920-PTHKNINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費マスタＦＥＴＣＨ処理
      *****************************************************************
       930-PTKOHINF-NEXT-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
               ADD     1               TO  CNT-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       930-PTKOHINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       950-HKNNUM-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       950-HKNNUM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータファイル読み込み処理
      *****************************************************************
       900-PARM-READ-SEC           SECTION.
      *
          MOVE    SPACE            TO  PARM-REC
      *
          READ    PARM-FILE
              AT  END
                  MOVE    1            TO  FLG-PARM
              NOT AT  END
                  ADD     1            TO  CNT-PARM
          END-READ
      *
          .
       900-PARM-READ-EXT.
          EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
      *********INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBOPEN ERROR"
           END-IF
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBSTART ERROR"
           END-IF
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBCOMMIT ERROR"
           END-IF
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBDISCONNECT ERROR"
           END-IF
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
