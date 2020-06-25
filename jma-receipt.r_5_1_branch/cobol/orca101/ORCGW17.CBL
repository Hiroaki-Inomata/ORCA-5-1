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
       PROGRAM-ID.                 ORCGW17.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : データチェック設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/01    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-W17.
           03  SPA-GMN-AREA.
               05  SPA-GMN-CUR         PIC 9(03).
               05  SPA-GMN-GMN-AREA.
                   07  SPA-GMN-CHECKLIST   OCCURS  100.
                       09  SPA-GMN-NUMCL       PIC X(3).
                       09  SPA-GMN-CDCL        PIC X(03).
                       09  SPA-GMN-KOUHICL     PIC X(20).
                       09  SPA-GMN-FTNNUMCL    PIC X(14).
                       09  SPA-GMN-JKSNUMCL    PIC X(14).
                       09  SPA-GMN-FTSHANKAKUCL PIC X(1).
                       09  SPA-GMN-FTSZENKAKUCL PIC X(1).
                       09  SPA-GMN-FTSNUMERICCL PIC X(1).
                       09  SPA-GMN-FTSKETACL    PIC X(1).
                       09  SPA-GMN-FTSKETAMINCL PIC X(02).
                       09  SPA-GMN-JKSHANKAKUCL PIC X(1).
                       09  SPA-GMN-JKSZENKAKUCL PIC X(1).
                       09  SPA-GMN-JKSNUMERICCL PIC X(1).
                       09  SPA-GMN-JKSKETACL    PIC X(1).
                       09  SPA-GMN-JKSKETAMINCL PIC X(02).
                       09  SPA-GMN-STYMDCL     PIC X(9).
                       09  SPA-GMN-EDYMDCL     PIC X(9).
                   07  SPA-GMN-CLIST-MAX       PIC 9(03).
                   07  SPA-GMN-SELNUM          PIC 9(03).
                   07  SPA-GMN-KOUHI-G.
                       09  SPA-GMN-KOHCD       PIC X(03).
                       09  SPA-GMN-FL01        PIC X(01).
                       09  SPA-GMN-KOHMEI      PIC X(20).
                   07  SPA-GMN-FTSNUM-G.
                       09  SPA-GMN-FTSCD       PIC X(01).
                       09  SPA-GMN-FL02        PIC X(01).
                       09  SPA-GMN-FTSMEI      PIC X(14).
                   07  SPA-GMN-JKSNUM-G.
                       09  SPA-GMN-JKSCD       PIC X(01).
                       09  SPA-GMN-FL03        PIC X(01).
                       09  SPA-GMN-JKSMEI      PIC X(14).
                   07  SPA-GMN-FTSHANKAKU-G.
                       09  SPA-GMN-FTSHANKAKUCD PIC X(01).
                       09  SPA-GMN-FL04        PIC X(01).
                       09  SPA-GMN-FTSHANKAKUMEI
                                               PIC X(18).
                   07  SPA-GMN-FTSZENKAKU-G.
                       09  SPA-GMN-FTSZENKAKUCD PIC X(01).
                       09  SPA-GMN-FL05        PIC X(01).
                       09  SPA-GMN-FTSZENKAKUMEI
                                               PIC X(18).
                   07  SPA-GMN-FTSNUMERIC-G.
                       09  SPA-GMN-FTSNUMERICCD PIC X(01).
                       09  SPA-GMN-FL06        PIC X(01).
                       09  SPA-GMN-FTSNUMERICMEI
                                               PIC X(18).
                   07  SPA-GMN-FTSKETA-G.
                       09  SPA-GMN-FTSKETACD PIC X(01).
                       09  SPA-GMN-FL07        PIC X(01).
                       09  SPA-GMN-FTSKETAMEI
                                               PIC X(18).
                   07  SPA-GMN-FTSKETAMIN-G.
                       09  SPA-GMN-FTSKETAMINCD PIC X(02).
                       09  SPA-GMN-FL08        PIC  X(01).
                       09  SPA-GMN-FTSKETAMINMEI
                                               PIC  X(16).
                   07  SPA-GMN-JKSHANKAKU-G.
                       09  SPA-GMN-JKSHANKAKUCD PIC X(01).
                       09  SPA-GMN-FL09        PIC X(01).
                       09  SPA-GMN-JKSHANKAKUMEI
                                               PIC X(18).
                   07  SPA-GMN-JKSZENKAKU-G.
                       09  SPA-GMN-JKSZENKAKUCD PIC X(01).
                       09  SPA-GMN-FL10        PIC X(01).
                       09  SPA-GMN-JKSZENKAKUMEI
                                               PIC X(18).
                   07  SPA-GMN-JKSNUMERIC-G.
                       09  SPA-GMN-JKSNUMERICCD PIC X(01).
                       09  SPA-GMN-FL11        PIC X(01).
                       09  SPA-GMN-JKSNUMERICMEI
                                               PIC X(18).
                   07  SPA-GMN-JKSKETA-G.
                       09  SPA-GMN-JKSKETACD PIC X(01).
                       09  SPA-GMN-FL12        PIC X(01).
                       09  SPA-GMN-JKSKETAMEI
                                               PIC X(18).
                   07  SPA-GMN-JKSKETAMIN-G.
                       09  SPA-GMN-JKSKETAMINCD PIC X(02).
                       09  SPA-GMN-FL13        PIC  X(01).
                       09  SPA-GMN-JKSKETAMINMEI
                                               PIC  X(16).
                   07  SPA-GMN-YUKOSTYMD   PIC X(09).
                   07  SPA-GMN-YUKOEDYMD   PIC X(09).
      *
               05  SPA-GMN-KOUHICOMBO-AREA.
                   07  SPA-GMN-KOUHI-LIST  OCCURS 99.
                       09  SPA-GMN-KOHCDL      PIC X(03).
                       09  SPA-GMN-FL14        PIC X(01).
                       09  SPA-GMN-KOHMEIL     PIC X(20).
                   07  SPA-GMN-KOUHI-MAX   PIC 9(04).
      *
               05  SPA-GMN-FTSNUMCOMBO-AREA.
                   07  SPA-GMN-FTSNUM-LIST OCCURS 2.
                       09  SPA-GMN-FTSCDL      PIC X(01).
                       09  SPA-GMN-FL15        PIC X(01).
                       09  SPA-GMN-FTSMEIL     PIC X(14).
                   07  SPA-GMN-FTSNUM-MAX  PIC 9(04).
      *
               05  SPA-GMN-JKSNUMCOMBO-AREA.
                   07  SPA-GMN-JKSNUM-LIST OCCURS 2.
                       09  SPA-GMN-JKSCDL      PIC X(01).
                       09  SPA-GMN-FL16        PIC X(01).
                       09  SPA-GMN-JKSMEIL     PIC X(14).
                   07  SPA-GMN-JKSNUM-MAX  PIC 9(04).
      *
               05  SPA-GMN-FTSHANKAKUCOMBO-AREA.
                   07  SPA-GMN-FTSHANKAKU-LIST OCCURS 2.
                       09  SPA-GMN-FTSHANKAKUCDL    PIC X(01).
                       09  SPA-GMN-FL17             PIC X(01).
                       09  SPA-GMN-FTSHANKAKUMEIL   PIC X(18).
                   07  SPA-GMN-FTSHANKAKU-MAX       PIC 9(04).
               05  SPA-GMN-FTSZENKAKUCOMBO-AREA.
                   07  SPA-GMN-FTSZENKAKU-LIST OCCURS 2.
                       09  SPA-GMN-FTSZENKAKUCDL    PIC X(01).
                       09  SPA-GMN-FL18             PIC X(01).
                       09  SPA-GMN-FTSZENKAKUMEIL   PIC X(18).
                   07  SPA-GMN-FTSZENKAKU-MAX       PIC 9(04).
      *
               05  SPA-GMN-FTSNUMERICCOMBO-AREA.
                   07  SPA-GMN-FTSNUMERIC-LIST OCCURS 2.
                       09  SPA-GMN-FTSNUMERICCDL    PIC X(01).
                       09  SPA-GMN-FL19             PIC X(01).
                       09  SPA-GMN-FTSNUMERICMEIL   PIC X(18).
                   07  SPA-GMN-FTSNUMERIC-MAX       PIC 9(04).
      *
               05  SPA-GMN-FTSKETACOMBO-AREA.
                   07  SPA-GMN-FTSKETA-LIST OCCURS 2.
                       09  SPA-GMN-FTSKETACDL    PIC X(01).
                       09  SPA-GMN-FL20          PIC X(01).
                       09  SPA-GMN-FTSKETAMEIL   PIC X(18).
                   07  SPA-GMN-FTSKETA-MAX       PIC 9(04).
      *
               05  SPA-GMN-FTSKETAMINCOMBO-AREA.
                   07  SPA-GMN-FTSKETAMIN-LIST OCCURS 10.
                       09  SPA-GMN-FTSKETAMINCDL    PIC X(02).
                       09  SPA-GMN-FL21             PIC X(01).
                       09  SPA-GMN-FTSKETAMINMEIL   PIC X(16).
                   07  SPA-GMN-FTSKETAMIN-MAX       PIC 9(04).
      *
               05  SPA-GMN-JKSHANKAKUCOMBO-AREA.
                   07  SPA-GMN-JKSHANKAKU-LIST OCCURS 2.
                       09  SPA-GMN-JKSHANKAKUCDL    PIC X(01).
                       09  SPA-GMN-FL22             PIC X(01).
                       09  SPA-GMN-JKSHANKAKUMEIL   PIC X(18).
                   07  SPA-GMN-JKSHANKAKU-MAX       PIC 9(04).
               05  SPA-GMN-JKSZENKAKUCOMBO-AREA.
                   07  SPA-GMN-JKSZENKAKU-LIST OCCURS 2.
                       09  SPA-GMN-JKSZENKAKUCDL    PIC X(01).
                       09  SPA-GMN-FL23             PIC X(01).
                       09  SPA-GMN-JKSZENKAKUMEIL   PIC X(18).
                   07  SPA-GMN-JKSZENKAKU-MAX       PIC 9(04).
      *
               05  SPA-GMN-JKSNUMERICCOMBO-AREA.
                   07  SPA-GMN-JKSNUMERIC-LIST OCCURS 2.
                       09  SPA-GMN-JKSNUMERICCDL    PIC X(01).
                       09  SPA-GMN-FL24             PIC X(01).
                       09  SPA-GMN-JKSNUMERICMEIL   PIC X(18).
                   07  SPA-GMN-JKSNUMERIC-MAX       PIC 9(04).
      *
               05  SPA-GMN-JKSKETACOMBO-AREA.
                   07  SPA-GMN-JKSKETA-LIST OCCURS 2.
                       09  SPA-GMN-JKSKETACDL    PIC X(01).
                       09  SPA-GMN-FL25          PIC X(01).
                       09  SPA-GMN-JKSKETAMEIL   PIC X(18).
                   07  SPA-GMN-JKSKETA-MAX       PIC 9(04).
      *
               05  SPA-GMN-JKSKETAMINCOMBO-AREA.
                   07  SPA-GMN-JKSKETAMIN-LIST OCCURS 20.
                       09  SPA-GMN-JKSKETAMINCDL    PIC X(02).
                       09  SPA-GMN-FL26             PIC X(01).
                       09  SPA-GMN-JKSKETAMINMEIL   PIC X(16).
                   07  SPA-GMN-JKSKETAMIN-MAX       PIC 9(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-CHECKLIST   OCCURS  100.
                       09  SPA-NAI-STYMD       PIC X(08).
                       09  SPA-NAI-EDYMD       PIC X(08).
                   07  SPA-NAI-YUKOSTYMD   PIC X(08).
                   07  SPA-NAI-YUKOEDYMD   PIC X(08).
      *
               05  SPA-MOT-AREA.
                   07  SPA-MOT-KOHCD       PIC X(03).
                   07  SPA-MOT-STYMD       PIC X(08).
                   07  SPA-MOT-EDYMD       PIC X(08).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD             PIC X(08).
           03  SYS-TIME            PIC X(06).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-DBRC            PIC 9(01).
           03  FLG-DABURI          PIC 9(01).
           03  FLG-SKIP            PIC 9(01).
           03  FLG-CLIST-ROW       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-KOH             PIC 9(02).
           03  IDX-FTS             PIC 9(02).
           03  IDX-JKS             PIC 9(02).
           03  IDX-LST             PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH-KOH      PIC X(20).
           03  WRK-DBPATH-SYS      PIC X(20).
      *
           03  WRK-WIDMSG          PIC X(70).
      *
           03  WRK-YMD             PIC X(10).
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-YUKOSTYMD       PIC X(08).
           03  WRK-YUKOEDYMD       PIC X(08).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-UPDKBN          PIC X(03).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-ROW.
           05  WRK-CLIST-ROW       PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-CLIST-MAX     PIC 9(05)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    チェック排除情報
           COPY  "CPSK1040.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE               DIVISION    USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W17
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN    "PUTG"      ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W17         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END             =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
      *            エラーメッセージ表示処理
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "W17    "       TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
      *            確認画面ＯＫ処理
                   PERFORM 330-WID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN "3001"
      *                削除処理
                       PERFORM 481-SAKUJO-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W17
           INITIALIZE                  SPA-W17
      *
      *    有効開始日
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
      *    有効終了日
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
      *    負担者番号抜け
           MOVE    "0"             TO  SPA-GMN-FTSCDL(1)
           MOVE    "1"             TO  SPA-GMN-FTSCDL(2)
           MOVE    "チェックする"  TO  SPA-GMN-FTSMEIL(1)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-FTSMEIL(2)
           MOVE    2               TO  SPA-GMN-FTSNUM-MAX
           MOVE    SPA-GMN-FTSNUM-LIST(1)
                                   TO  SPA-GMN-FTSNUM-G
      *
      *    受給者番号抜け
           MOVE    "0"             TO  SPA-GMN-JKSCDL(1)
           MOVE    "1"             TO  SPA-GMN-JKSCDL(2)
           MOVE    "チェックする"  TO  SPA-GMN-JKSMEIL(1)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-JKSMEIL(2)
           MOVE    2               TO  SPA-GMN-JKSNUM-MAX
           MOVE    SPA-GMN-JKSNUM-LIST(1)
                                   TO  SPA-GMN-JKSNUM-G
      *
      *    負担者番号半角
           MOVE    "1"                 TO  SPA-GMN-FTSHANKAKUCDL(1)
           MOVE    "認める"            TO  SPA-GMN-FTSHANKAKUMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-FTSHANKAKUCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-FTSHANKAKUMEIL(2)
           MOVE    2                   TO  SPA-GMN-FTSHANKAKU-MAX
           MOVE    SPA-GMN-FTSHANKAKU-LIST(1)
                                       TO  SPA-GMN-FTSHANKAKU-G
      *
      *    負担者番号全角
           MOVE    "1"                 TO  SPA-GMN-FTSZENKAKUCDL(1)
           MOVE    "認める"            TO  SPA-GMN-FTSZENKAKUMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-FTSZENKAKUCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-FTSZENKAKUMEIL(2)
           MOVE    2                   TO  SPA-GMN-FTSZENKAKU-MAX
           MOVE    SPA-GMN-FTSZENKAKU-LIST(1)
                                       TO  SPA-GMN-FTSZENKAKU-G
      *
      *    負担者番号数字チェック
           MOVE    "1"                 TO  SPA-GMN-FTSNUMERICCDL(1)
           MOVE    "認める"            TO  SPA-GMN-FTSNUMERICMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-FTSNUMERICCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-FTSNUMERICMEIL(2)
           MOVE    2                   TO  SPA-GMN-FTSNUMERIC-MAX
           MOVE    SPA-GMN-FTSNUMERIC-LIST(1)
                                       TO  SPA-GMN-FTSNUMERIC-G
      *
      *    負担者番号桁数チェック
           MOVE    "0"                 TO  SPA-GMN-FTSKETACDL(1)
           MOVE    "チェックする"      TO  SPA-GMN-FTSKETAMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-FTSKETACDL(2)
           MOVE    "チェックしない"    TO  SPA-GMN-FTSKETAMEIL(2)
           MOVE    2                   TO  SPA-GMN-FTSKETA-MAX
           MOVE    SPA-GMN-FTSKETA-LIST(2)
                                       TO  SPA-GMN-FTSKETA-G
      *
      *    負担者番号桁数チェック
           MOVE    "01"                TO  SPA-GMN-FTSKETAMINCDL(1)
           MOVE    "１桁"              TO  SPA-GMN-FTSKETAMINMEIL(1)
           MOVE    "02"                TO  SPA-GMN-FTSKETAMINCDL(2)
           MOVE    "２桁"              TO  SPA-GMN-FTSKETAMINMEIL(2)
           MOVE    "03"                TO  SPA-GMN-FTSKETAMINCDL(3)
           MOVE    "３桁"              TO  SPA-GMN-FTSKETAMINMEIL(3)
           MOVE    "04"                TO  SPA-GMN-FTSKETAMINCDL(4)
           MOVE    "４桁"              TO  SPA-GMN-FTSKETAMINMEIL(4)
           MOVE    "05"                TO  SPA-GMN-FTSKETAMINCDL(5)
           MOVE    "５桁"              TO  SPA-GMN-FTSKETAMINMEIL(5)
           MOVE    "06"                TO  SPA-GMN-FTSKETAMINCDL(6)
           MOVE    "６桁"              TO  SPA-GMN-FTSKETAMINMEIL(6)
           MOVE    "07"                TO  SPA-GMN-FTSKETAMINCDL(7)
           MOVE    "７桁"              TO  SPA-GMN-FTSKETAMINMEIL(7)
           MOVE    "08"                TO  SPA-GMN-FTSKETAMINCDL(8)
           MOVE    "８桁"              TO  SPA-GMN-FTSKETAMINMEIL(8)
           MOVE    8                   TO  SPA-GMN-FTSKETAMIN-MAX
      *
      *    受給者番号半角
           MOVE    "1"                 TO  SPA-GMN-JKSHANKAKUCDL(1)
           MOVE    "認める"            TO  SPA-GMN-JKSHANKAKUMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-JKSHANKAKUCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-JKSHANKAKUMEIL(2)
           MOVE    2                   TO  SPA-GMN-JKSHANKAKU-MAX
           MOVE    SPA-GMN-JKSHANKAKU-LIST(1)
                                       TO  SPA-GMN-JKSHANKAKU-G
      *
      *    受給者番号全角
           MOVE    "1"                 TO  SPA-GMN-JKSZENKAKUCDL(1)
           MOVE    "認める"            TO  SPA-GMN-JKSZENKAKUMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-JKSZENKAKUCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-JKSZENKAKUMEIL(2)
           MOVE    2                   TO  SPA-GMN-JKSZENKAKU-MAX
           MOVE    SPA-GMN-JKSZENKAKU-LIST(1)
                                       TO  SPA-GMN-JKSZENKAKU-G
      *
      *    受給者番号数字チェック
           MOVE    "1"                 TO  SPA-GMN-JKSNUMERICCDL(1)
           MOVE    "認める"            TO  SPA-GMN-JKSNUMERICMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-JKSNUMERICCDL(2)
           MOVE    "エラーとする"      TO  SPA-GMN-JKSNUMERICMEIL(2)
           MOVE    2                   TO  SPA-GMN-JKSNUMERIC-MAX
           MOVE    SPA-GMN-JKSNUMERIC-LIST(1)
                                       TO  SPA-GMN-JKSNUMERIC-G
      *
      *    受給者番号桁数チェック
           MOVE    "0"                 TO  SPA-GMN-JKSKETACDL(1)
           MOVE    "チェックする"      TO  SPA-GMN-JKSKETAMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-JKSKETACDL(2)
           MOVE    "チェックしない"    TO  SPA-GMN-JKSKETAMEIL(2)
           MOVE    2                   TO  SPA-GMN-JKSKETA-MAX
           MOVE    SPA-GMN-JKSKETA-LIST(2)
                                       TO  SPA-GMN-JKSKETA-G
      *
      *    受給者番号桁数チェック
           MOVE    "01"                TO  SPA-GMN-JKSKETAMINCDL(1)
           MOVE    "１桁"              TO  SPA-GMN-JKSKETAMINMEIL(1)
           MOVE    "02"                TO  SPA-GMN-JKSKETAMINCDL(2)
           MOVE    "２桁"              TO  SPA-GMN-JKSKETAMINMEIL(2)
           MOVE    "03"                TO  SPA-GMN-JKSKETAMINCDL(3)
           MOVE    "３桁"              TO  SPA-GMN-JKSKETAMINMEIL(3)
           MOVE    "04"                TO  SPA-GMN-JKSKETAMINCDL(4)
           MOVE    "４桁"              TO  SPA-GMN-JKSKETAMINMEIL(4)
           MOVE    "05"                TO  SPA-GMN-JKSKETAMINCDL(5)
           MOVE    "５桁"              TO  SPA-GMN-JKSKETAMINMEIL(5)
           MOVE    "06"                TO  SPA-GMN-JKSKETAMINCDL(6)
           MOVE    "６桁"              TO  SPA-GMN-JKSKETAMINMEIL(6)
           MOVE    "07"                TO  SPA-GMN-JKSKETAMINCDL(7)
           MOVE    "７桁"              TO  SPA-GMN-JKSKETAMINMEIL(7)
           MOVE    "08"                TO  SPA-GMN-JKSKETAMINCDL(8)
           MOVE    "８桁"              TO  SPA-GMN-JKSKETAMINMEIL(8)
           MOVE    "09"                TO  SPA-GMN-JKSKETAMINCDL(9)
           MOVE    "９桁"              TO  SPA-GMN-JKSKETAMINMEIL(9)
           MOVE    "10"                TO  SPA-GMN-JKSKETAMINCDL(10)
           MOVE    "１０桁"            TO  SPA-GMN-JKSKETAMINMEIL(10)
           MOVE    "11"                TO  SPA-GMN-JKSKETAMINCDL(11)
           MOVE    "１１桁"            TO  SPA-GMN-JKSKETAMINMEIL(11)
           MOVE    "12"                TO  SPA-GMN-JKSKETAMINCDL(12)
           MOVE    "１２桁"            TO  SPA-GMN-JKSKETAMINMEIL(12)
           MOVE    "13"                TO  SPA-GMN-JKSKETAMINCDL(13)
           MOVE    "１３桁"            TO  SPA-GMN-JKSKETAMINMEIL(13)
           MOVE    "14"                TO  SPA-GMN-JKSKETAMINCDL(14)
           MOVE    "１４桁"            TO  SPA-GMN-JKSKETAMINMEIL(14)
           MOVE    "15"                TO  SPA-GMN-JKSKETAMINCDL(15)
           MOVE    "１５桁"            TO  SPA-GMN-JKSKETAMINMEIL(15)
           MOVE    "16"                TO  SPA-GMN-JKSKETAMINCDL(16)
           MOVE    "１６桁"            TO  SPA-GMN-JKSKETAMINMEIL(16)
           MOVE    "17"                TO  SPA-GMN-JKSKETAMINCDL(17)
           MOVE    "１７桁"            TO  SPA-GMN-JKSKETAMINMEIL(17)
           MOVE    "18"                TO  SPA-GMN-JKSKETAMINCDL(18)
           MOVE    "１８桁"            TO  SPA-GMN-JKSKETAMINMEIL(18)
           MOVE    "19"                TO  SPA-GMN-JKSKETAMINCDL(19)
           MOVE    "１９桁"            TO  SPA-GMN-JKSKETAMINMEIL(19)
           MOVE    "20"                TO  SPA-GMN-JKSKETAMINCDL(20)
           MOVE    "２０桁"            TO  SPA-GMN-JKSKETAMINMEIL(20)
           MOVE    20                  TO  SPA-GMN-JKSKETAMIN-MAX
      *
      *    コンボ項目設定処理
           PERFORM 3101-HKNNUM-COMBO-SET-SEC
      *
      *    チェック排除情報リスト編集処理
           PERFORM 3101-CLIST-SET-SEC
           MOVE    1               TO  FLG-CLIST-ROW
      *
      *    カーソル位置をセット
           IF      SPA-GMN-CLIST-MAX   >   ZERO
               MOVE    001             TO  SPA-GMN-CUR
           ELSE
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費コンボ項目設定処理
      *****************************************************************
       3101-HKNNUM-COMBO-SET-SEC   SECTION.
      *
      *    保険番号読み込み
           MOVE    ZERO            TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-NAI-YUKOEDYMD   TO  HKN-TEKSTYMD
           MOVE    SPA-NAI-YUKOSTYMD   TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "HKNNUM-KEY3"   TO  WRK-DBPATH-KOH
      *
           PERFORM 31011-HKNNUM-SELECT-SEC
      *
      *    公費の種類コンボセット
           MOVE    ZERO            TO  IDX-KOH
           PERFORM UNTIL ( FLG-HKNNUM  =   1  )
                   OR    ( IDX-KOH     >=  99 )
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR "4" OR
                                               "8" OR "9" )
                   CONTINUE
               ELSE
                   IF    ( IDX-KOH         =   ZERO )
                    OR   ( HKN-HKNNUM  NOT =   SPA-GMN-KOHCDL(IDX-KOH))
                       ADD     1           TO  IDX-KOH
                       MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-KOHMEIL(IDX-KOH)
                       MOVE    HKN-HKNNUM  TO  SPA-GMN-KOHCDL(IDX-KOH)
                   ELSE
                       MOVE    HKN-TANSEIDONAME
                                           TO  SPA-GMN-KOHMEIL(IDX-KOH)
                   END-IF
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
      *        保険番号読み込み
               PERFORM 31011-HKNNUM-FETCH-SEC
           END-PERFORM
      *
           MOVE    IDX-KOH         TO  SPA-GMN-KOUHI-MAX
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-HKNNUM-COMBO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費リスト項目設定処理
      *****************************************************************
       3101-HKNNUM-CLIST-SET-SEC   SECTION.
      *
      *    保険番号読み込み
           MOVE    ZERO            TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
      *
           MOVE    SPA-HOSPNUM      TO  HKN-HOSPNUM
           MOVE    SYS-1040-KBNCD  TO  HKN-HKNNUM
           MOVE    SYS-1040-EDYUKYMD
                                   TO  HKN-TEKSTYMD
           MOVE    SYS-1040-STYUKYMD
                                   TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
      **** MOVE    "HKNNUM-KEY5"   TO  WRK-DBPATH-KOH
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 31011-HKNNUM-SELECT-SEC
      *
      *    公費の種類セット
           PERFORM UNTIL ( FLG-HKNNUM  =   1  )
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR "4" OR
                                               "8" OR "9" )
                   CONTINUE
               ELSE
                   MOVE    HKN-TANSEIDONAME
                                       TO  SPA-GMN-KOUHICL(IDX-LST)
                   MOVE    HKN-HKNNUM  TO  SPA-GMN-CDCL(IDX-LST)
               END-IF
      *
      *        保険番号読み込み
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 31011-HKNNUM-FETCH-SEC
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-HKNNUM-CLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック排除情報リスト編集処理
      *****************************************************************
       3101-CLIST-SET-SEC          SECTION.
      *
      *    チェック排除読み込み
           MOVE    ZERO            TO  FLG-SYSKANRI
           INITIALIZE                  SYS-1040-REC
      *
           MOVE    "1040"          TO  SYS-1040-KANRICD
           MOVE    SPA-HOSPNUM     TO  SYS-1040-HOSPNUM
           MOVE    SPA-SYSYMD      TO  SYS-1040-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1040-EDYUKYMD
           MOVE    SYS-1040-REC    TO  MCPDATA-REC
      *
      ***  MOVE    "SYSKANRI-KEY5" TO  WRK-DBPATH-SYS
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
      *    システム管理検索処理
           PERFORM 3101-SYSKANRI-SELECT-SEC
      *
      *    チェックリストセット
           MOVE    ZERO            TO  IDX-LST
           PERFORM UNTIL ( FLG-SYSKANRI    =   1  )
                   OR    ( IDX-LST         >=  CONST-CLIST-MAX )
      *
               MOVE    MCPDATA-REC     TO  SYS-1040-REC
               ADD     1               TO  IDX-LST
      *
      *        番号
               MOVE    IDX-LST         TO  SPA-GMN-NUMCL(IDX-LST)
      *
      *        公費の種類
               PERFORM 3101-HKNNUM-CLIST-SET-SEC
      *
      *        負担者番号
               MOVE    SYS-1040-FTNJANUMCHK
                                   TO  SPA-GMN-FTNNUMCL(IDX-LST)
      *
      *        受給者番号
               MOVE    SYS-1040-JKYSNUMCHK
                                   TO  SPA-GMN-JKSNUMCL(IDX-LST)
      *
      *        負担者番号半角
               IF    ( SYS-1040-FTNJHANKAKUCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-FTSHANKAKUCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-FTNJHANKAKUCHK
                                   TO  SPA-GMN-FTSHANKAKUCL(IDX-LST)
               END-IF
      *
      *        負担者番号全角
               IF    ( SYS-1040-FTNJZENKAKUCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-FTSZENKAKUCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-FTNJZENKAKUCHK
                                   TO  SPA-GMN-FTSZENKAKUCL(IDX-LST)
               END-IF
      *
      *        負担者番号数字チェック
               IF    ( SYS-1040-FTNJNUMERICCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-FTSNUMERICCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-FTNJNUMERICCHK
                                   TO  SPA-GMN-FTSNUMERICCL(IDX-LST)
               END-IF
      *
      *        負担者番号桁数チェック
               IF    ( SYS-1040-FTNJKETACHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-FTSKETACL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-FTNJKETACHK
                                   TO  SPA-GMN-FTSKETACL(IDX-LST)
               END-IF
      *
               IF    ( SYS-1040-FTNJKETAMIN
                                   =   SPACE )
                   MOVE    "08"    TO  SPA-GMN-FTSKETAMINCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-FTNJKETAMIN
                                   TO  SPA-GMN-FTSKETAMINCL(IDX-LST)
               END-IF
      *
      *        受給者番号半角
               IF    ( SYS-1040-JKYSHANKAKUCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-JKSHANKAKUCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-JKYSHANKAKUCHK
                                   TO  SPA-GMN-JKSHANKAKUCL(IDX-LST)
               END-IF
      *
      *        受給者番号全角
               IF    ( SYS-1040-JKYSZENKAKUCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-JKSZENKAKUCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-JKYSZENKAKUCHK
                                   TO  SPA-GMN-JKSZENKAKUCL(IDX-LST)
               END-IF
      *
      *        受給者番号数字チェック
               IF    ( SYS-1040-JKYSNUMERICCHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-JKSNUMERICCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-JKYSNUMERICCHK
                                   TO  SPA-GMN-JKSNUMERICCL(IDX-LST)
               END-IF
      *
      *        受給者番号桁数チェック
               IF    ( SYS-1040-JKYSKETACHK
                                   =   SPACE )
                   MOVE    "1"     TO  SPA-GMN-JKSKETACL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-JKYSKETACHK
                                   TO  SPA-GMN-JKSKETACL(IDX-LST)
               END-IF
      *
               IF    ( SYS-1040-JKYSKETAMIN
                                   =   SPACE )
                   MOVE    "07"    TO  SPA-GMN-JKSKETAMINCL(IDX-LST)
               ELSE
                   MOVE    SYS-1040-JKYSKETAMIN
                                   TO  SPA-GMN-JKSKETAMINCL(IDX-LST)
               END-IF
      *
      *        有効開始日付
               MOVE    SYS-1040-STYUKYMD   TO  WRK-YMD
      *        日付チェック
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-STYMDCL(IDX-LST)
               MOVE    WRK-SYMD        TO  SPA-NAI-STYMD(IDX-LST)
      *
      *        有終了日付
               MOVE    SYS-1040-EDYUKYMD   TO  WRK-YMD
      *        日付チェック
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-EDYMDCL(IDX-LST)
               MOVE    WRK-SYMD        TO  SPA-NAI-EDYMD(IDX-LST)
      *
      *        チェック排除読み込み
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 3101-SYSKANRI-FETCH-SEC
           END-PERFORM
      *
           MOVE    IDX-LST         TO  SPA-GMN-CLIST-MAX
      *
      *    チェック排除CLOSE
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-CLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"   ALSO    "B01"
      *            戻る　処理
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
      *            条件クリア処理
                   PERFORM 420-CLEAR-SEC
      *        追加
               WHEN    "CLICKED"   ALSO    "B06"
      *            追加処理
                   PERFORM 480-TUIKA-SEC
      *        変更
               WHEN    "CLICKED"   ALSO    "B07"
      *            変更処理
                   PERFORM 480-HENKOU-SEC
      *        削除
               WHEN    "CLICKED"   ALSO    "B08"
      *            削除確認処理
                   PERFORM 480-SAKUJO-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
      *            選択番号編集処理
                   PERFORM 4101-SELNUM-SET-SEC
      *        行選択
               WHEN    ANY         ALSO    "CHECKLIST"
      *            チェックリスト編集処理
                   PERFORM 4101-CLIST-SET-SEC
      *        その他
               WHEN    OTHER
                   MOVE    ZERO        TO  SPA-GMN-CUR
      *            入力チェック処理
                   PERFORM 4101-INPUT-SET-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4101-SELNUM-SET-SEC         SECTION.
      *
           MOVE    W17-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
      *        入力項目を初期化する
               PERFORM 420-CLEAR-SEC
               MOVE    002             TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-SET-EXT
           END-IF
      *
           IF      SPA-GMN-SELNUM      >   SPA-GMN-CLIST-MAX
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    001             TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-SET-EXT
           END-IF
      *
           MOVE    002             TO  SPA-GMN-CUR
           MOVE    SPA-GMN-SELNUM  TO  IDX-LST
      *    リストの内容をセット
           PERFORM 420-INPUT-LIST-SET-SEC
      *
           .
      *
       4101-SELNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックリスト編集処理
      *****************************************************************
       4101-CLIST-SET-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO            TO  WRK-SELNUM
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL   IDX-LST     >       SPA-GMN-CLIST-MAX
               IF      W17-CLIST-SELECT(IDX-LST)   =   "T"
      *            リストの内容をセット
                   PERFORM 420-INPUT-LIST-SET-SEC
                   MOVE    IDX-LST         TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-CLIST-MAX   TO  IDX-LST
               END-IF
           END-PERFORM
      *
           .
      *
       4101-CLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リストの内容をセット処理
      *****************************************************************
       420-INPUT-LIST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-MOT-AREA
      *
      *    有効期間開始日
           MOVE    SPA-NAI-STYMD(IDX-LST)
                                   TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
                                       SPA-MOT-STYMD
      *    有効期間終了日
           MOVE    SPA-NAI-EDYMD(IDX-LST)
                                   TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
                                       SPA-MOT-EDYMD
      *
      *    公費コンボ
           MOVE    SPA-GMN-CDCL(IDX-LST)
                                   TO  SPA-GMN-KOUHI-G
                                       SPA-MOT-KOHCD
      *    負担者番号コンボ
           MOVE    SPA-GMN-FTNNUMCL(IDX-LST)
                                   TO  SPA-GMN-FTSNUM-G
      *    受給者番号コンボ
           MOVE    SPA-GMN-JKSNUMCL(IDX-LST)
                                   TO  SPA-GMN-JKSNUM-G
      *    負担者番号半角コンボ
           MOVE    SPA-GMN-FTSHANKAKUCL(IDX-LST)
                                   TO  SPA-GMN-FTSHANKAKU-G
      *    負担者番号全角コンボ
           MOVE    SPA-GMN-FTSZENKAKUCL(IDX-LST)
                                   TO  SPA-GMN-FTSZENKAKU-G
      *    負担者番号数字コンボ
           MOVE    SPA-GMN-FTSNUMERICCL(IDX-LST)
                                   TO  SPA-GMN-FTSNUMERIC-G
      *    負担者番号桁数コンボ
           MOVE    SPA-GMN-FTSKETACL(IDX-LST)
                                   TO  SPA-GMN-FTSKETA-G
      *    負担者番号桁数コンボ
           MOVE    SPA-GMN-FTSKETAMINCL(IDX-LST)
                                   TO  SPA-GMN-FTSKETAMIN-G
      *    受給者番号半角コンボ
           MOVE    SPA-GMN-JKSHANKAKUCL(IDX-LST)
                                   TO  SPA-GMN-JKSHANKAKU-G
      *    受給者番号全角コンボ
           MOVE    SPA-GMN-JKSZENKAKUCL(IDX-LST)
                                   TO  SPA-GMN-JKSZENKAKU-G
      *    受給者番号数字コンボ
           MOVE    SPA-GMN-JKSNUMERICCL(IDX-LST)
                                   TO  SPA-GMN-JKSNUMERIC-G
      *    受給者番号桁数コンボ
           MOVE    SPA-GMN-JKSKETACL(IDX-LST)
                                   TO  SPA-GMN-JKSKETA-G
      *    受給者番号桁数コンボ
           MOVE    SPA-GMN-JKSKETAMINCL(IDX-LST)
                                   TO  SPA-GMN-JKSKETAMIN-G
      *
      *    公費コンボの内容を再セット
           PERFORM 3101-HKNNUM-COMBO-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       420-INPUT-LIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4101-INPUT-SET-SEC          SECTION.
      *
      *    公費コンボ
           MOVE    W17-KOUHI       TO  SPA-GMN-KOUHI-G
      *    負担者番号コンボ
           MOVE    W17-FTSNUM      TO  SPA-GMN-FTSNUM-G
      *    受給者番号コンボ
           MOVE    W17-JKSNUM      TO  SPA-GMN-JKSNUM-G
      *    負担者番号半角コンボ
           MOVE    W17-FTSHANKAKU   TO  SPA-GMN-FTSHANKAKU-G
      *    負担者番号全角コンボ
           MOVE    W17-FTSZENKAKU   TO  SPA-GMN-FTSZENKAKU-G
      *    負担者番号数字コンボ
           MOVE    W17-FTSNUMERIC   TO  SPA-GMN-FTSNUMERIC-G
      *    負担者番号桁数コンボ
           MOVE    W17-FTSKETA      TO  SPA-GMN-FTSKETA-G
      *    負担者番号桁数コンボ
           MOVE    W17-FTSKETAMIN   TO  SPA-GMN-FTSKETAMIN-G
      *    受給者番号半角コンボ
           MOVE    W17-JKSHANKAKU   TO  SPA-GMN-JKSHANKAKU-G
      *    受給者番号全角コンボ
           MOVE    W17-JKSZENKAKU   TO  SPA-GMN-JKSZENKAKU-G
      *    受給者番号数字コンボ
           MOVE    W17-JKSNUMERIC   TO  SPA-GMN-JKSNUMERIC-G
      *    受給者番号桁数コンボ
           MOVE    W17-JKSKETA      TO  SPA-GMN-JKSKETA-G
      *    受給者番号桁数コンボ
           MOVE    W17-JKSKETAMIN   TO  SPA-GMN-JKSKETAMIN-G
      *    有効期間開始日
           MOVE    W17-YUKOSTYMD   TO  SPA-GMN-YUKOSTYMD
      *    有効期間終了日
           MOVE    W17-YUKOEDYMD   TO  SPA-GMN-YUKOEDYMD
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       4101-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    有効期間開始日チェック
           PERFORM 4110-YUKOSTYMD-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    有効期間終了日チェック
           PERFORM 4111-YUKOEDYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費コンボチェック
           PERFORM 4112-KOUHI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号コンボチェック
           PERFORM 4113-FTSNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号コンボチェック
           PERFORM 4114-JKSNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号半角コンボチェック
           PERFORM 4114-FTSHANKAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号全角コンボチェック
           PERFORM 4114-FTSZENKAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号数字コンボチェック
           PERFORM 4114-FTSNUMERIC-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号桁数コンボチェック
           PERFORM 4114-FTSKETA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    負担者番号桁数コンボチェック
           PERFORM 4114-FTSKETAMIN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号半角コンボチェック
           PERFORM 4114-JKSHANKAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号全角コンボチェック
           PERFORM 4114-JKSZENKAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号数字コンボチェック
           PERFORM 4114-JKSNUMERIC-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号桁数コンボチェック
           PERFORM 4114-JKSKETA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    受給者番号桁数コンボチェック
           PERFORM 4114-JKSKETAMIN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効開始日編集処理
      *****************************************************************
       4110-YUKOSTYMD-CHK-SEC      SECTION.
      *
      *    空白ならばALL"0"をセットする
           IF    ( SPA-GMN-YUKOSTYMD       =   SPACE )
            OR   ( SPA-GMN-YUKOSTYMD (1:3) =  "000" )
               MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                           SPA-NAI-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-YUKOSTYMD
      *
           MOVE    SPACE               TO  SPA-NAI-YUKOSTYMD
      *
      *    有効開始日付
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-YMD
      *    日付チェック
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    WRK-YUKOSTYMD   TO  WRK-YMD
      *        日付チェック
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-YUKOSTYMD
      *        日付編集エラー
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    005             TO  SPA-GMN-CUR
               GO  TO  4110-YUKOSTYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-YUKOSTYMD
      *
           IF    ( WRK-MCP-WIDGET  =   "YUKOSTYMD" )
      *        公費コンボの内容を再セット
               PERFORM 3101-HKNNUM-COMBO-SET-SEC
           END-IF
      *
           .
      *
       4110-YUKOSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了日編集処理
      *****************************************************************
       4111-YUKOEDYMD-CHK-SEC      SECTION.
      *
      *    空白ならばALL"9"をセットする
           IF    ( SPA-GMN-YUKOEDYMD       =   SPACE )
            OR   ( SPA-GMN-YUKOSTYMD (1:3) =  "999" )
               MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YUKOEDYMD
      *
      *    有効開始日付
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-YMD
      *    日付チェック
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    WRK-YUKOEDYMD   TO  WRK-YMD
      *        日付チェック
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-YUKOEDYMD
      *        日付編集エラー
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    006             TO  SPA-GMN-CUR
               GO  TO  4111-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-YUKOEDYMD
      *
           IF    ( WRK-MCP-WIDGET  =   "YUKOEDYMD" )
      *        公費コンボの内容を再セット
               PERFORM 3101-HKNNUM-COMBO-SET-SEC
           END-IF
      *
           .
      *
       4111-YUKOEDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       4112-KOUHI-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-KOUHI-G     =   SPACE )
               GO  TO  4112-KOUHI-CHK-EXT
           END-IF
      *
      *    公費をセット
           PERFORM VARYING IDX-KOH     FROM    1   BY   1
                   UNTIL ( IDX-KOH     >   SPA-GMN-KOUHI-MAX )
                   OR    ( SPA-GMN-KOHCD
                                   =   SPA-GMN-KOHCDL(IDX-KOH))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-KOH   <=  SPA-GMN-KOUHI-MAX
               MOVE    SPA-GMN-KOUHI-LIST(IDX-KOH)
                                       TO  SPA-GMN-KOUHI-G
           ELSE
      *        公費番号エラー
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4112-KOUHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号編集処理
      *****************************************************************
       4113-FTSNUM-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-FTSNUM-G    =   SPACE )
               MOVE    SPA-GMN-FTSNUM-LIST(1)
                                   TO  SPA-GMN-FTSNUM-G
               GO  TO  4113-FTSNUM-CHK-EXT
           END-IF
      *
      *    負担者番号をセット
           PERFORM VARYING IDX-FTS     FROM    1   BY   1
                   UNTIL ( IDX-FTS     >   SPA-GMN-FTSNUM-MAX )
                   OR    ( SPA-GMN-FTSCD
                                       =   SPA-GMN-FTSCDL(IDX-FTS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-FTS   <=  SPA-GMN-FTSNUM-MAX
               MOVE    SPA-GMN-FTSNUM-LIST(IDX-FTS)
                                       TO  SPA-GMN-FTSNUM-G
           ELSE
      *        負担者番号エラー
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    003             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4113-FTSNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号編集処理
      *****************************************************************
       4114-JKSNUM-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-JKSNUM-G    =   SPACE )
               MOVE    SPA-GMN-JKSNUM-LIST(1)
                                   TO  SPA-GMN-JKSNUM-G
               GO  TO  4114-JKSNUM-CHK-EXT
           END-IF
      *
      *    受給者番号をセット
           PERFORM VARYING IDX-JKS     FROM    1   BY   1
                   UNTIL ( IDX-JKS     >   SPA-GMN-JKSNUM-MAX )
                   OR    ( SPA-GMN-JKSCD
                                       =   SPA-GMN-JKSCDL(IDX-JKS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-JKS   <=  SPA-GMN-JKSNUM-MAX
               MOVE    SPA-GMN-JKSNUM-LIST(IDX-JKS)
                                       TO  SPA-GMN-JKSNUM-G
           ELSE
      *        受給者番号エラー
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    004             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-JKSNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号半角編集処理
      *****************************************************************
       4114-FTSHANKAKU-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-FTSHANKAKU-G     =   SPACE )
               MOVE    SPA-GMN-FTSHANKAKU-LIST(1)
                                   TO  SPA-GMN-FTSHANKAKU-G
           END-IF
      *
           PERFORM VARYING IDX-FTS FROM    1   BY   1
                   UNTIL ( IDX-FTS >   SPA-GMN-FTSHANKAKU-MAX )
                   OR    ( SPA-GMN-FTSHANKAKUCD
                                   =   SPA-GMN-FTSHANKAKUCDL(IDX-FTS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-FTS   <=  SPA-GMN-FTSHANKAKU-MAX
               MOVE    SPA-GMN-FTSHANKAKU-LIST(IDX-FTS)
                                       TO  SPA-GMN-FTSHANKAKU-G
           ELSE
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    007             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-FTSHANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号全角編集処理
      *****************************************************************
       4114-FTSZENKAKU-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-FTSZENKAKU-G     =   SPACE )
               MOVE    SPA-GMN-FTSZENKAKU-LIST(1)
                                   TO  SPA-GMN-FTSZENKAKU-G
           END-IF
      *
           PERFORM VARYING IDX-FTS FROM    1   BY   1
                   UNTIL ( IDX-FTS >   SPA-GMN-FTSZENKAKU-MAX )
                   OR    ( SPA-GMN-FTSZENKAKUCD
                                   =   SPA-GMN-FTSZENKAKUCDL(IDX-FTS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-FTS   <=  SPA-GMN-FTSZENKAKU-MAX
               MOVE    SPA-GMN-FTSZENKAKU-LIST(IDX-FTS)
                                       TO  SPA-GMN-FTSZENKAKU-G
           ELSE
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    008             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-FTSZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号数字編集処理
      *****************************************************************
       4114-FTSNUMERIC-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-FTSNUMERIC-G     =   SPACE )
               MOVE    SPA-GMN-FTSNUMERIC-LIST(1)
                                   TO  SPA-GMN-FTSNUMERIC-G
           END-IF
      *
           PERFORM VARYING IDX-FTS FROM    1   BY   1
                   UNTIL ( IDX-FTS >   SPA-GMN-FTSNUMERIC-MAX )
                   OR    ( SPA-GMN-FTSNUMERICCD
                                   =   SPA-GMN-FTSNUMERICCDL(IDX-FTS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-FTS   <=  SPA-GMN-FTSNUMERIC-MAX
               MOVE    SPA-GMN-FTSNUMERIC-LIST(IDX-FTS)
                                       TO  SPA-GMN-FTSNUMERIC-G
           ELSE
               MOVE    "0015"          TO  SPA-ERRCD
               MOVE    011             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-FTSNUMERIC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号桁数チェック処理
      *****************************************************************
       4114-FTSKETA-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX-FTS FROM    1   BY   1
                   UNTIL ( IDX-FTS >   SPA-GMN-FTSKETA-MAX )
                   OR    ( SPA-GMN-FTSKETACD
                                   =   SPA-GMN-FTSKETACDL(IDX-FTS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-FTS   <=  SPA-GMN-FTSKETA-MAX
               MOVE    SPA-GMN-FTSKETA-LIST(IDX-FTS)
                                   TO  SPA-GMN-FTSKETA-G
           ELSE
               MOVE    SPA-GMN-FTSKETA-LIST(2)
                                   TO  SPA-GMN-FTSKETA-G
           END-IF
      *
           IF    ( SPA-GMN-FTSKETACD =  "1" )
               MOVE    SPACE       TO  SPA-GMN-FTSKETAMIN-G
           END-IF
           .
      *
       4114-FTSKETA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号桁数チェック処理
      *****************************************************************
       4114-FTSKETAMIN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-FTSKETACD =  "1" )
               CONTINUE
           ELSE
               PERFORM VARYING IDX-FTS FROM    1   BY   1
                       UNTIL ( IDX-FTS >   SPA-GMN-FTSKETAMIN-MAX )
                       OR    ( SPA-GMN-FTSKETAMINCD
                                       =   SPA-GMN-FTSKETAMINCDL
                                                          (IDX-FTS))
                   CONTINUE
               END-PERFORM
      *
               IF      IDX-FTS   <=  SPA-GMN-FTSKETAMIN-MAX
                   MOVE    SPA-GMN-FTSKETAMIN-LIST(IDX-FTS)
                                   TO  SPA-GMN-FTSKETAMIN-G
               ELSE
                   MOVE    SPA-GMN-FTSKETAMIN-LIST(8)
                                   TO  SPA-GMN-FTSKETAMIN-G
               END-IF
           END-IF
      *
           .
      *
       4114-FTSKETAMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号半角編集処理
      *****************************************************************
       4114-JKSHANKAKU-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-JKSHANKAKU-G     =   SPACE )
               MOVE    SPA-GMN-JKSHANKAKU-LIST(1)
                                   TO  SPA-GMN-JKSHANKAKU-G
           END-IF
      *
           PERFORM VARYING IDX-JKS FROM    1   BY   1
                   UNTIL ( IDX-JKS >   SPA-GMN-JKSHANKAKU-MAX )
                   OR    ( SPA-GMN-JKSHANKAKUCD
                                   =   SPA-GMN-JKSHANKAKUCDL(IDX-JKS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-JKS   <=  SPA-GMN-JKSHANKAKU-MAX
               MOVE    SPA-GMN-JKSHANKAKU-LIST(IDX-JKS)
                                       TO  SPA-GMN-JKSHANKAKU-G
           ELSE
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    009             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-JKSHANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号全角編集処理
      *****************************************************************
       4114-JKSZENKAKU-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-JKSZENKAKU-G     =   SPACE )
               MOVE    SPA-GMN-JKSZENKAKU-LIST(1)
                                   TO  SPA-GMN-JKSZENKAKU-G
           END-IF
      *
           PERFORM VARYING IDX-JKS FROM    1   BY   1
                   UNTIL ( IDX-JKS >   SPA-GMN-JKSZENKAKU-MAX )
                   OR    ( SPA-GMN-JKSZENKAKUCD
                                   =   SPA-GMN-JKSZENKAKUCDL(IDX-JKS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-JKS   <=  SPA-GMN-JKSZENKAKU-MAX
               MOVE    SPA-GMN-JKSZENKAKU-LIST(IDX-JKS)
                                       TO  SPA-GMN-JKSZENKAKU-G
           ELSE
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    010             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-JKSZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号数字編集処理
      *****************************************************************
       4114-JKSNUMERIC-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-JKSNUMERIC-G     =   SPACE )
               MOVE    SPA-GMN-JKSNUMERIC-LIST(1)
                                   TO  SPA-GMN-JKSNUMERIC-G
           END-IF
      *
           PERFORM VARYING IDX-JKS FROM    1   BY   1
                   UNTIL ( IDX-JKS >   SPA-GMN-JKSNUMERIC-MAX )
                   OR    ( SPA-GMN-JKSNUMERICCD
                                   =   SPA-GMN-JKSNUMERICCDL(IDX-JKS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-JKS   <=  SPA-GMN-JKSNUMERIC-MAX
               MOVE    SPA-GMN-JKSNUMERIC-LIST(IDX-JKS)
                                       TO  SPA-GMN-JKSNUMERIC-G
           ELSE
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    012             TO  SPA-GMN-CUR
           END-IF
           .
      *
       4114-JKSNUMERIC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号桁数チェック処理
      *****************************************************************
       4114-JKSKETA-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX-JKS FROM    1   BY   1
                   UNTIL ( IDX-JKS >   SPA-GMN-JKSKETA-MAX )
                   OR    ( SPA-GMN-JKSKETACD
                                   =   SPA-GMN-JKSKETACDL(IDX-JKS))
               CONTINUE
           END-PERFORM
      *
           IF      IDX-JKS   <=  SPA-GMN-JKSKETA-MAX
               MOVE    SPA-GMN-JKSKETA-LIST(IDX-JKS)
                                       TO  SPA-GMN-JKSKETA-G
           ELSE
               MOVE    SPA-GMN-JKSKETA-LIST(2)
                                       TO  SPA-GMN-JKSKETA-G
           END-IF
      *
           IF    ( SPA-GMN-JKSKETACD   =   "1" )
               MOVE    SPACE           TO  SPA-GMN-JKSKETAMIN-G
           END-IF
      *
           .
      *
       4114-JKSKETA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号桁数チェック処理
      *****************************************************************
       4114-JKSKETAMIN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-JKSKETACD   =   "1" )
               CONTINUE
           ELSE
               PERFORM VARYING IDX-JKS FROM    1   BY   1
                       UNTIL ( IDX-JKS >   SPA-GMN-JKSKETAMIN-MAX )
                       OR    ( SPA-GMN-JKSKETAMINCD
                                       =   SPA-GMN-JKSKETAMINCDL
                                                          (IDX-JKS))
                   CONTINUE
               END-PERFORM
      *
               IF      IDX-JKS   <=  SPA-GMN-JKSKETAMIN-MAX
                   MOVE    SPA-GMN-JKSKETAMIN-LIST(IDX-JKS)
                                   TO  SPA-GMN-JKSKETAMIN-G
               ELSE
                   MOVE    SPA-GMN-JKSKETAMIN-LIST(7)
                                   TO  SPA-GMN-JKSKETAMIN-G
               END-IF
           END-IF
           .
      *
       4114-JKSKETAMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W17"           TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"        TO  MCP-PUTTYPE
           MOVE    "W01"           TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
      *    選択番号クリア
           MOVE    ZERO            TO  SPA-GMN-SELNUM
      *
      *    公費番号クリア
           INITIALIZE                  SPA-GMN-KOUHI-G
      *
      *    負担者番号抜け初期値セット
           MOVE    SPA-GMN-FTSNUM-LIST(1)
                                   TO  SPA-GMN-FTSNUM-G
      *    受給者番号抜け初期値セット
           MOVE    SPA-GMN-JKSNUM-LIST(1)
                                   TO  SPA-GMN-JKSNUM-G
      *    負担者番号半角初期値セット
           MOVE    SPA-GMN-FTSHANKAKU-LIST(1)
                                   TO  SPA-GMN-FTSHANKAKU-G
      *    負担者番号全角初期値セット
           MOVE    SPA-GMN-FTSZENKAKU-LIST(1)
                                   TO  SPA-GMN-FTSZENKAKU-G
      *    負担者番号数字初期値セット
           MOVE    SPA-GMN-FTSNUMERIC-LIST(1)
                                   TO  SPA-GMN-FTSNUMERIC-G
      *    負担者番号桁数チェック初期値セット
           MOVE    SPA-GMN-FTSKETA-LIST(2)
                                   TO  SPA-GMN-FTSKETA-G
           MOVE    SPACE           TO  SPA-GMN-FTSKETAMIN-G
      *    受給者番号半角初期値セット
           MOVE    SPA-GMN-JKSHANKAKU-LIST(1)
                                   TO  SPA-GMN-JKSHANKAKU-G
      *    受給者番号全角初期値セット
           MOVE    SPA-GMN-JKSZENKAKU-LIST(1)
                                   TO  SPA-GMN-JKSZENKAKU-G
      *    受給者番号数字初期値セット
           MOVE    SPA-GMN-JKSNUMERIC-LIST(1)
                                   TO  SPA-GMN-JKSNUMERIC-G
      *    受給者番号桁数チェック初期値セット
           MOVE    SPA-GMN-JKSKETA-LIST(2)
                                   TO  SPA-GMN-JKSKETA-G
           MOVE    SPACE           TO  SPA-GMN-JKSKETAMIN-G
      *
      *    有効開始日初期値セット
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
      *    有効終了日初期値セット
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
      *    コンボ項目設定処理
           PERFORM 3101-HKNNUM-COMBO-SET-SEC
      *
      *    カーソル位置セット
           IF      SPA-GMN-CLIST-MAX   >   ZERO
               MOVE    001             TO  SPA-GMN-CUR
           ELSE
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       480-TUIKA-SEC               SECTION.
      *
      *    入力時チェック
           PERFORM 4101-INPUT-SET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TUIKA-EXT
           END-IF
      *
           MOVE    "INS"           TO  WRK-UPDKBN
      *
      *    関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TUIKA-EXT
           END-IF
      *
      *    履歴チェック
           PERFORM 4901-RIREKI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TUIKA-EXT
           END-IF
      *
      *    チェック排除情報セット処理
           PERFORM 4701-KOUMOKU-SET-SEC
      *
      *    チェック排除情報マスタ追加
           MOVE    SPA-HOSPNUM         TO  SYS-1040-HOSPNUM
           MOVE    SYS-1040-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
      *        追加失敗エラー
               MOVE   "4002"           TO  SPA-ERRCD
               MOVE    002             TO  SPA-GMN-CUR
               GO  TO 480-TUIKA-EXT
           END-IF
      *
      *    チェック排除情報リスト編集処理
           PERFORM 3101-CLIST-SET-SEC
           MOVE    2               TO  FLG-CLIST-ROW
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
      *    最大件数チェック
           IF    ( SPA-GMN-CLIST-MAX   >=  CONST-CLIST-MAX )
               MOVE    "1104"      TO  SPA-ERRCD
           END-IF
      *
           .
       480-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    チェック排除情報セット処理
      *****************************************************************
       4701-KOUMOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1040-REC
           INITIALIZE                  SYS-1040-REC
           MOVE    "1040"          TO  SYS-1040-KANRICD
           MOVE    SPA-GMN-KOHCD   TO  SYS-1040-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD
                                   TO  SYS-1040-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD
                                   TO  SYS-1040-EDYUKYMD
           MOVE    SPA-GMN-FTSCD   TO  SYS-1040-FTNJANUMCHK
           MOVE    SPA-GMN-JKSCD   TO  SYS-1040-JKYSNUMCHK
           MOVE    SPA-GMN-FTSHANKAKUCD
                                   TO  SYS-1040-FTNJHANKAKUCHK
           MOVE    SPA-GMN-FTSZENKAKUCD
                                   TO  SYS-1040-FTNJZENKAKUCHK
           MOVE    SPA-GMN-FTSNUMERICCD
                                   TO  SYS-1040-FTNJNUMERICCHK
           MOVE    SPA-GMN-FTSKETACD
                                   TO  SYS-1040-FTNJKETACHK
           MOVE    SPA-GMN-FTSKETAMINCD
                                   TO  SYS-1040-FTNJKETAMIN
                                       SYS-1040-FTNJKETAMAX
           MOVE    SPA-GMN-JKSHANKAKUCD
                                   TO  SYS-1040-JKYSHANKAKUCHK
           MOVE    SPA-GMN-JKSZENKAKUCD
                                   TO  SYS-1040-JKYSZENKAKUCHK
           MOVE    SPA-GMN-JKSNUMERICCD
                                   TO  SYS-1040-JKYSNUMERICCHK
           MOVE    SPA-GMN-JKSKETACD
                                   TO  SYS-1040-JKYSKETACHK
           MOVE    SPA-GMN-JKSKETAMINCD
                                   TO  SYS-1040-JKYSKETAMIN
                                       SYS-1040-JKYSKETAMAX
      *
           IF    ( WRK-UPDKBN          =   "UPD" )
      *        更新時間
               ACCEPT  SYS-TIME        FROM    TIME
               MOVE    SYS-TIME        TO  SYS-1040-UPHMS
      *        更新年月日
               MOVE    SPA-SYSYMD      TO  SYS-1040-UPYMD
           ELSE
      *        作成年月日
               MOVE    SPA-SYSYMD      TO  SYS-1040-CREYMD
           END-IF
      *
           .    
       4701-KOUMOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       480-HENKOU-SEC              SECTION.
      *
           IF      SPA-GMN-SELNUM      >   SPA-GMN-CLIST-MAX
      *        選択番号が大きすぎるエラー
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    001             TO  SPA-GMN-CUR
               GO  TO  480-HENKOU-EXT
           END-IF
      *
      *    入力時チェック
           PERFORM 4101-INPUT-SET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
           MOVE    "UPD"           TO  WRK-UPDKBN
      *
      *    関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
      *    履歴チェック
           PERFORM 4901-RIREKI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-HENKOU-EXT
           END-IF
      *
      *    チェック排除情報セット処理
           PERFORM 4701-KOUMOKU-SET-SEC
      *
      *    更新用キーをセット
           MOVE    SYS-1040-REC    TO  SYSKANRI-REC
           MOVE    "1040"          TO  SYSUP-KANRICD
           MOVE    SPA-MOT-KOHCD   TO  SYSUP-KBNCD
           MOVE    SPA-MOT-STYMD   TO  SYSUP-STYUKYMD
           MOVE    SPA-MOT-EDYMD   TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
      *
      *    チェック排除情報マスタ更新
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
      *        更新失敗エラー
               MOVE   "4004"           TO  SPA-ERRCD
               MOVE    002             TO  SPA-GMN-CUR
               GO  TO 480-HENKOU-EXT
           END-IF
      *
      *    チェック排除情報リスト編集処理
           PERFORM 3101-CLIST-SET-SEC
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
           .
       480-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    削除確認処理
      *****************************************************************
       480-SAKUJO-SEC              SECTION.
      *
           MOVE    "DEL"           TO  WRK-UPDKBN
      *
      *    入力項目の関連チェック処理
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-SAKUJO-EXT
           END-IF
      *
      *    削除するか確認する
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       481-SAKUJO-SEC              SECTION.
      *
           MOVE    SPACE           TO  SYS-1040-REC
           INITIALIZE                  SYS-1040-REC
           MOVE    "1040"          TO  SYS-1040-KANRICD
           MOVE    SPA-MOT-KOHCD   TO  SYS-1040-KBNCD
           MOVE    SPA-MOT-STYMD   TO  SYS-1040-STYUKYMD
           MOVE    SPA-MOT-EDYMD   TO  SYS-1040-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1040-HOSPNUM
           MOVE    SYS-1040-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
      *        削除失敗エラー
               MOVE    "4005"          TO  SPA-ERRCD
               MOVE    002             TO  SPA-GMN-CUR
               GO  TO  481-SAKUJO-EXT
           END-IF
      *
      *    チェック排除情報リスト編集処理
           PERFORM 3101-CLIST-SET-SEC
      *
      *    入力項目を初期化する
           PERFORM 420-CLEAR-SEC
      *
           .    
       481-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC          SECTION.
      *
           IF  ( SPA-NAI-YUKOEDYMD     <   SPA-NAI-YUKOSTYMD )
      *        日付関係編集エラー
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    005             TO  SPA-GMN-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    選択番号番号
           IF    ( SPA-GMN-SELNUM  NOT =   ZERO )
               IF    ( WRK-UPDKBN          =   "UPD" )
                 OR  ( WRK-UPDKBN          =   "DEL" )
                   CONTINUE
               ELSE
      *            選択番号が入力されているエラー
                   MOVE    "1101"          TO  SPA-ERRCD
                   MOVE    001             TO  SPA-GMN-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           ELSE
               IF    ( WRK-UPDKBN          =   "INS" )
                   IF    ( SPA-GMN-KOHCD       =   SPACE )
      *                公費番号が選択されていないエラー
                       MOVE    "0002"          TO  SPA-ERRCD
                       MOVE    002             TO  SPA-GMN-CUR
                       GO  TO  4900-KANRENCHK-EXT
                   END-IF
               ELSE
      *            選択番号が入力されていないエラー
                   MOVE    "1102"          TO  SPA-ERRCD
                   MOVE    001             TO  SPA-GMN-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4901-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DABURI
      *
      *    履歴チェック
           MOVE    SPACE               TO  SYS-1040-REC
           INITIALIZE                      SYS-1040-REC
           MOVE    "1040"              TO  SYS-1040-KANRICD
           MOVE    SPA-GMN-KOHCD       TO  SYS-1040-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1040-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1040-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1040-EDYUKYMD
           MOVE    SYS-1040-REC        TO  MCPDATA-REC
      *
      ***  MOVE    "SYSKANRI-KEY8"     TO  WRK-DBPATH-SYS
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
      *    システム管理検索処理
           PERFORM 3101-SYSKANRI-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1040-REC
      *
               IF    ( WRK-UPDKBN         =   "UPD" )
                   AND   ( SPA-MOT-KOHCD  =   SYS-1040-KBNCD )
                   AND   ( SPA-MOT-STYMD  =   SYS-1040-STYUKYMD )
                   AND   ( SPA-MOT-EDYMD  =   SYS-1040-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-YUKOEDYMD   <   SYS-1040-STYUKYMD )
                    OR   ( SPA-NAI-YUKOSTYMD   >   SYS-1040-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE     1              TO  FLG-DABURI
                   END-IF
               END-IF
      *
      *        システム管理読み込み処理
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 3101-SYSKANRI-FETCH-SEC
           END-PERFORM
      *
      *    システム管理CLOSE処理
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-DABURI    =   1 )
      *        期間重複エラー
               MOVE    "1103"          TO  SPA-ERRCD
               MOVE    005             TO  SPA-GMN-CUR
           END-IF
      *
           .
       4901-RIREKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *    画面編集処理
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーメッセージ表示処理
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
      *        ガイドメッセージ表示処理
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W17    "           TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    W17-CLIST-ROW   TO  WRK-CLIST-ROW
      *
           MOVE    SPACE           TO  W17
           INITIALIZE                  W17
      *
           IF    ( SPA-GMN-CLIST-MAX   >=  CONST-CLIST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W17-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL
                                   TO  W17-B06-STATE
           END-IF
      *
      *    チェックリストをセット
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >       SPA-GMN-CLIST-MAX )
               MOVE    SPA-GMN-NUMCL(IDX-LST)
                                       TO  W17-CLIST-NUML(IDX-LST)
               MOVE    SPA-GMN-CDCL(IDX-LST)
                                       TO  W17-CLIST-CDL(IDX-LST)
               MOVE    SPA-GMN-KOUHICL(IDX-LST)
                                       TO  W17-CLIST-KOUHIL(IDX-LST)
               IF    ( SPA-GMN-FTNNUMCL(IDX-LST)   =   "1" )
                   MOVE    "−"        TO  W17-CLIST-FTNNUML(IDX-LST)
               ELSE
                   MOVE    "○"        TO  W17-CLIST-FTNNUML(IDX-LST)
               END-IF
      *
               IF    ( SPA-GMN-JKSNUMCL(IDX-LST)   =   "1" )
                   MOVE    "−"        TO  W17-CLIST-JKSNUML(IDX-LST)
               ELSE
                   MOVE    "○"        TO  W17-CLIST-JKSNUML(IDX-LST)
               END-IF
               MOVE    SPA-GMN-STYMDCL(IDX-LST)
                                       TO  W17-CLIST-STYMDL(IDX-LST)
               MOVE    SPA-GMN-EDYMDCL(IDX-LST)
                                       TO  W17-CLIST-EDYMDL(IDX-LST)
               IF      IDX-LST         =   SPA-GMN-SELNUM
                   MOVE    "T"         TO  W17-CLIST-SELECT(IDX-LST)
               ELSE
                   MOVE    "F"         TO  W17-CLIST-SELECT(IDX-LST)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-CLIST-MAX   TO  W17-CLIST-COUNT
      *
      *    選択番号
           MOVE    SPA-GMN-SELNUM      TO  W17-SELNUM
      *
      *    公費の種類
           MOVE    SPA-GMN-KOUHI-G     TO  W17-KOUHI
           PERFORM VARYING IDX-KOH     FROM    1   BY  1
                   UNTIL   IDX-KOH     >       SPA-GMN-KOUHI-MAX
               MOVE    SPA-GMN-KOUHI-LIST(IDX-KOH)
                                           TO  W17-KOUHI-ITEM(IDX-KOH)
           END-PERFORM
           MOVE    SPA-GMN-KOUHI-MAX   TO  W17-KOUHI-COUNT
      *
      *    負担者番号抜け
           MOVE    SPA-GMN-FTSNUM-G    TO  W17-FTSNUM
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSNUM-MAX
               MOVE    SPA-GMN-FTSNUM-LIST(IDX-FTS)
                                           TO  W17-FTSNUM-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSNUM-MAX  TO  W17-FTSNUM-COUNT
      *
      *    受給者番号抜け
           MOVE    SPA-GMN-JKSNUM-G    TO  W17-JKSNUM
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSNUM-MAX
               MOVE    SPA-GMN-JKSNUM-LIST(IDX-JKS)
                                           TO  W17-JKSNUM-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSNUM-MAX  TO  W17-JKSNUM-COUNT
      *
      *    負担者番号半角チェック
           MOVE    SPA-GMN-FTSHANKAKU-G TO  W17-FTSHANKAKU
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSHANKAKU-MAX
               MOVE    SPA-GMN-FTSHANKAKU-LIST(IDX-FTS)
                                       TO  W17-FTSHANKAKU-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSHANKAKU-MAX
                                       TO  W17-FTSHANKAKU-COUNT
      *
      *    負担者番号全角チェック
           MOVE    SPA-GMN-FTSZENKAKU-G TO  W17-FTSZENKAKU
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSZENKAKU-MAX
               MOVE    SPA-GMN-FTSZENKAKU-LIST(IDX-FTS)
                                       TO  W17-FTSZENKAKU-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSZENKAKU-MAX
                                       TO  W17-FTSZENKAKU-COUNT
      *
      *    負担者番号数字チェック
           MOVE    SPA-GMN-FTSNUMERIC-G TO  W17-FTSNUMERIC
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSNUMERIC-MAX
               MOVE    SPA-GMN-FTSNUMERIC-LIST(IDX-FTS)
                                       TO  W17-FTSNUMERIC-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSNUMERIC-MAX
                                       TO  W17-FTSNUMERIC-COUNT
      *
      *    負担者番号桁数チェック
           MOVE    SPA-GMN-FTSKETA-G TO  W17-FTSKETA
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSKETA-MAX
               MOVE    SPA-GMN-FTSKETA-LIST(IDX-FTS)
                                       TO  W17-FTSKETA-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSKETA-MAX
                                       TO  W17-FTSKETA-COUNT
      *
      *    負担者番号桁数チェック
           MOVE    SPA-GMN-FTSKETAMIN-G TO  W17-FTSKETAMIN
           PERFORM VARYING IDX-FTS     FROM    1   BY  1
                   UNTIL   IDX-FTS     >       SPA-GMN-FTSKETAMIN-MAX
               MOVE    SPA-GMN-FTSKETAMIN-LIST(IDX-FTS)
                                       TO  W17-FTSKETAMIN-ITEM(IDX-FTS)
           END-PERFORM
           MOVE    SPA-GMN-FTSKETAMIN-MAX
                                       TO  W17-FTSKETAMIN-COUNT
      *
      *    受給者番号半角チェック
           MOVE    SPA-GMN-JKSHANKAKU-G TO  W17-JKSHANKAKU
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSHANKAKU-MAX
               MOVE    SPA-GMN-JKSHANKAKU-LIST(IDX-JKS)
                                       TO  W17-JKSHANKAKU-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSHANKAKU-MAX
                                       TO  W17-JKSHANKAKU-COUNT
      *
      *    受給者番号全角チェック
           MOVE    SPA-GMN-JKSZENKAKU-G TO  W17-JKSZENKAKU
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSZENKAKU-MAX
               MOVE    SPA-GMN-JKSZENKAKU-LIST(IDX-JKS)
                                       TO  W17-JKSZENKAKU-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSZENKAKU-MAX
                                       TO  W17-JKSZENKAKU-COUNT
      *
      *    受給者番号数字チェック
           MOVE    SPA-GMN-JKSNUMERIC-G TO  W17-JKSNUMERIC
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSNUMERIC-MAX
               MOVE    SPA-GMN-JKSNUMERIC-LIST(IDX-JKS)
                                       TO  W17-JKSNUMERIC-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSNUMERIC-MAX
                                       TO  W17-JKSNUMERIC-COUNT
      *    受給者番号桁数チェック
           MOVE    SPA-GMN-JKSKETA-G TO  W17-JKSKETA
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSKETA-MAX
               MOVE    SPA-GMN-JKSKETA-LIST(IDX-JKS)
                                       TO  W17-JKSKETA-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSKETA-MAX
                                       TO  W17-JKSKETA-COUNT
      *
      *    受給者番号桁数チェック
           MOVE    SPA-GMN-JKSKETAMIN-G TO  W17-JKSKETAMIN
           PERFORM VARYING IDX-JKS     FROM    1   BY  1
                   UNTIL   IDX-JKS     >       SPA-GMN-JKSKETAMIN-MAX
               MOVE    SPA-GMN-JKSKETAMIN-LIST(IDX-JKS)
                                       TO  W17-JKSKETAMIN-ITEM(IDX-JKS)
           END-PERFORM
           MOVE    SPA-GMN-JKSKETAMIN-MAX
                                       TO  W17-JKSKETAMIN-COUNT
      *
      *    有効開始日
           MOVE    SPA-GMN-YUKOSTYMD   TO  W17-YUKOSTYMD
      *    有効終了日
           MOVE    SPA-GMN-YUKOEDYMD   TO  W17-YUKOEDYMD
      *
      *  
           EVALUATE    FLG-CLIST-ROW
           WHEN    1
               MOVE    ZERO               TO   W17-CLIST-ROW
               MOVE    ZERO               TO   W17-CLIST-ROWATTR
           WHEN    2
               MOVE    SPA-GMN-CLIST-MAX  TO   W17-CLIST-ROW
               MOVE    1                  TO   W17-CLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-CLIST-ROW      TO   W17-CLIST-ROW
               MOVE    ZERO               TO   W17-CLIST-ROWATTR
           END-EVALUATE
      *
           IF    ( SPA-GMN-FTSKETACD      =    "1" )
               MOVE    WIDGET-INSENSITIVE TO   W17-FTSKETAMIN-STATE
                                               W17-FTSKETAMIN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-JKSKETACD      =    "1" )
               MOVE    WIDGET-INSENSITIVE TO   W17-JKSKETAMIN-STATE
                                               W17-JKSKETAMIN-ITEM-STATE
           END-IF
      *
      *    カーソルセット
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)
              AND (SPA-GMN-CUR         =   ZERO )
      *        入力個所のセット処理
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
      *    カーソルセット
           EVALUATE    SPA-GMN-CUR
               WHEN    001
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    002
                   MOVE    "KOUHI"         TO  MCP-WIDGET
               WHEN    003
                   MOVE    "FTSNUM"        TO  MCP-WIDGET
               WHEN    004
                   MOVE    "JKSNUM"        TO  MCP-WIDGET
               WHEN    005
                   MOVE    "YUKOSTYMD"     TO  MCP-WIDGET
               WHEN    006
                   MOVE    "YUKOEDYMD"     TO  MCP-WIDGET
               WHEN    007
                   MOVE    "FTSHANKAKU"     TO  MCP-WIDGET
               WHEN    008
                   MOVE    "FTSZENKAKU"     TO  MCP-WIDGET
               WHEN    009
                   MOVE    "JKSHANKAKU"     TO  MCP-WIDGET
               WHEN    010
                   MOVE    "JKSZENKAKU"     TO  MCP-WIDGET
               WHEN    011
                   MOVE    "FTSNUMERIC"     TO  MCP-WIDGET
               WHEN    012
                   MOVE    "JKSNUMERIC"     TO  MCP-WIDGET
               WHEN    013
                   MOVE    "FTSKETA"       TO  MCP-WIDGET
               WHEN    014
                   MOVE    "FTSKETAMIN"    TO  MCP-WIDGET
               WHEN    015
                   MOVE    "JKSKETA"       TO  MCP-WIDGET
               WHEN    016
                   MOVE    "JKSKETAMIN"    TO  MCP-WIDGET
               WHEN    201
                   MOVE    "B01"           TO  MCP-WIDGET
               WHEN    202
                   MOVE    "B02"           TO  MCP-WIDGET
               WHEN    206
                   MOVE    "B06"           TO  MCP-WIDGET
               WHEN    207
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    208
                   MOVE    "B08"           TO  MCP-WIDGET
               WHEN    212
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
      *    入力箇所からカーソルをセット
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    002             TO  SPA-GMN-CUR
               WHEN    "KOUHI"
                   MOVE    003             TO  SPA-GMN-CUR
               WHEN    "FTSNUM"
                   MOVE    004             TO  SPA-GMN-CUR
               WHEN    "JKSNUM"
                   MOVE    007             TO  SPA-GMN-CUR
               WHEN    "FTSHANKAKU"
                   MOVE    008             TO  SPA-GMN-CUR
               WHEN    "FTSZENKAKU"
                   MOVE    011             TO  SPA-GMN-CUR
               WHEN    "FTSNUMERIC"
                   MOVE    013             TO  SPA-GMN-CUR
               WHEN    "FTSKETA"
                   MOVE    014             TO  SPA-GMN-CUR
               WHEN    "FTSKETAMIN"
                   MOVE    009             TO  SPA-GMN-CUR
               WHEN    "JKSHANKAKU"
                   MOVE    010             TO  SPA-GMN-CUR
               WHEN    "JKSZENKAKU"
                   MOVE    012             TO  SPA-GMN-CUR
               WHEN    "JKSNUMERIC"
                   MOVE    015             TO  SPA-GMN-CUR
               WHEN    "JKSKETA"
                   MOVE    016             TO  SPA-GMN-CUR
               WHEN    "JKSKETAMIN"
                   MOVE    005             TO  SPA-GMN-CUR
               WHEN    "YUKOSTYMD"
                   MOVE    006             TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    206             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-CUR     =   14 )
               IF    ( W17-FTSKETAMIN-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    009     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   16 )
               IF    ( W17-JKSKETAMIN-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    005     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
      *    エラーメッセージセット
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "公費番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "負担者番号抜けチェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "受給者番号抜けチェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効期間開始日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "有効期間終了日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "有効開始日付より前の日付になっています"
                                           TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "日付が正しくありません"
                                           TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "負担者番号半角文字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "負担者番号全角文字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "受給者番号半角文字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "受給者番号全角文字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "負担者番号数字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "受給者番号数字チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "負担者番号桁数チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "負担者番号桁数入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "受給者番号桁数チェック入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "受給者番号桁数入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "1101"
                   MOVE    "対象のデータが選択されています"
                                           TO  SPA-ERRMSG
               WHEN    "1102"
                   MOVE    "選択番号を入力してください"
                                           TO  SPA-ERRMSG
               WHEN    "1103"
                   MOVE    "有効期間の重複するデータが存在します"
                                           TO  SPA-ERRMSG
               WHEN    "1104"
                   MOVE    "追加可能最大件数に達しました"
                                           TO  SPA-ERRMSG
               WHEN    "4001"
                   MOVE    "該当するデータが既に存在します"
                                           TO  SPA-ERRMSG
               WHEN    "4002"
                   MOVE    "追加処理に失敗しました"
                                           TO  SPA-ERRMSG
               WHEN    "4003"
                   MOVE    "該当するデータが存在しません"
                                           TO  SPA-ERRMSG
               WHEN    "4004"
                   MOVE    "更新処理に失敗しました"
                                           TO  SPA-ERRMSG
               WHEN    "4005"
                   MOVE    "削除処理に失敗しました"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W17"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示処理
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
      *    確認メッセージセット
           EVALUATE    SPA-WIDCD
               WHEN    "3001"
                   MOVE    "削除します。よろしいですか"
                                           TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE           TO  WID1
           INITIALIZE                  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "W17"           TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付がＡＬＬ”０”もしくはＡＬＬ”９”のとき処理抜ける
           IF    ( WRK-YMD             =   "00000000" )
             OR  ( WRK-YMD             =   "99999999" )
               MOVE    WRK-YMD         TO  WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENYMDG
                                       WRK-SYMD
      *
      *    日付画面チェック
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-YMD         TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
      *        日付エラー
               MOVE    "0010"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック排除情報検索処理
      *****************************************************************
       3101-SYSKANRI-SELECT-SEC    SECTION.
      *
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
      *        チェック排除情報読込処理
               PERFORM 3101-SYSKANRI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       3101-SYSKANRI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    チェック排除情報読込処理
      *****************************************************************
        3101-SYSKANRI-FETCH-SEC    SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       3101-SYSKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター検索処理
      *****************************************************************
       31011-HKNNUM-SELECT-SEC     SECTION.
      *
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
      *        保険番号マスター読込処理
               PERFORM 31011-HKNNUM-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
      *
       31011-HKNNUM-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター読込処理
      *****************************************************************
       31011-HKNNUM-FETCH-SEC      SECTION.
      *
           MOVE    1               TO  FLG-SKIP
      *
           PERFORM UNTIL ( FLG-SKIP    =   ZERO )
                    OR   ( FLG-HKNNUM  =   1    )
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    ZERO        TO  FLG-HKNNUM
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
      *            １００〜８９９のデータだけ抽出
                   IF    ( HKN-HKNNUM         >=  "100")
                     AND ( HKN-HKNNUM         <=  "899" )
                       MOVE    ZERO    TO  FLG-SKIP
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
      *
           END-PERFORM
           .
      *
       31011-HKNNUM-FETCH-EXT.
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
      *    ＤＢクロース処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
