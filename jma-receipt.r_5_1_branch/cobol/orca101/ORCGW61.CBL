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
       PROGRAM-ID.                 ORCGW61.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : レセプト特記事項編集情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-藤原    05/05/17  レセプト特記事項編集情報に
      *                                     「2 老人医療受給がない場合」
      *                                      を追加
      *
      *  02.06.01    NACL-藤原    05/05/20  削除ボタンをＦ４に変更
      *  02.06.02    NACL-藤原    05/08/03  条件に負担者番号を追加
      *
      *  02.08.01    NACL-藤原    06/02/25  有効期間の未入力時の更新処理
      *
      *  03.01.01    NACL-藤原    06/08/23  DBCLOSECURSOR追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *
      *  04.01.01    NACL-藤原    08/01/10  適用欄コメント対応
      *  04.01.02    NACL-藤原    08/02/14  保険名称の取得方法の修正
      *  04.01.03    NACL-藤原    08/01/10  適用欄コメントのチェックの修正
      *
      *  04.02.01    NACL-藤原    08/04/12  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/04/14  警告時の登録処理を可能にする
      *
      *  04.04.01    NACL-藤原    09/04/14  保険番号９００番台のチェック
      *  04.04.02    NACL-藤原    09/08/12  コラムリストの上限を２００に
      *                                     変更
      *  04.04.03    NACL-藤原    09/08/28  期間重複チェックに負担者番号
      *                                     を追加
      *  04.07.01    NACL-藤原    14/02/10  条件に入外区分、所得区分の追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
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
      *    画面用ＳＰＡ
       01  SPA-W61.
           03  SPA-W61-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TEISYUTUSAKILST     OCCURS  5.
                       09  SPA-GMN-TEISYUTUSAKIL   PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-TEISYUTUSAKIMEIL
                                                   PIC X(10).
                   07  SPA-GMN-TEISYUTUSAKI-MAX    PIC 9(01).
      *
                   07  SPA-GMN-FTNJANUMKBN-JYOKENLST
                                                   OCCURS  5.
                       09  SPA-GMN-FTNJANUMKBN-JYOKENL
                                                   PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-FTNJANUMKBN-JYOKENMEIL
                                                   PIC X(30).
                   07  SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
                                                   PIC 9(01).
      *
                   07  SPA-GMN-NYUGAIKBNLST        OCCURS  5.
                       09  SPA-GMN-NYUGAIKBNL      PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-NYUGAIKBNMEIL   PIC X(10).
                   07  SPA-GMN-NYUGAIKBN-MAX       PIC 9(01).
      *
                   07  SPA-GMN-SYOTOKUKBNLST       OCCURS  5.
                       09  SPA-GMN-SYOTOKUKBNL     PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-SYOTOKUKBNMEIL  PIC X(20).
                   07  SPA-GMN-SYOTOKUKBN-MAX      PIC 9(01).
      *
                   07  SPA-GMN-ZAITAKULST          OCCURS  5.
                       09  SPA-GMN-ZAITAKUL        PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-ZAITAKUMEIL     PIC X(42).
                   07  SPA-GMN-ZAITAKU-MAX         PIC 9(01).
      *
                   07  SPA-GMN-RJNKBNLST           OCCURS  5.
                       09  SPA-GMN-RJNKBNL         PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-RJNKBNMEIL      PIC X(50).
                   07  SPA-GMN-RJNKBN-MAX          PIC 9(01).
      *
                   07  SPA-GMN-FTNJANUMKBNLST      OCCURS  5.
                       09  SPA-GMN-FTNJANUMKBNL    PIC X(02).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-FTNJANUMKBNMEIL PIC X(22).
                   07  SPA-GMN-FTNJANUMKBN-MAX     PIC 9(01).
      *
                   07  SPA-GMN-JKYSNUMKBNLST       OCCURS  5.
                       09  SPA-GMN-JKYSNUMKBNL     PIC X(02).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-JKYSNUMKBNMEIL  PIC X(22).
                   07  SPA-GMN-JKYSNUMKBN-MAX      PIC 9(01).
      *
                   07  SPA-GMN-RECEDEN-KORECKBNLST OCCURS  5.
                       09  SPA-GMN-RECEDEN-KORECKBNL
                                                   PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-RECEDEN-KORECKBNMEIL
                                                   PIC X(10).
                   07  SPA-GMN-RECEDEN-KORECKBN-MAX
                                                   PIC 9(01).
      *
                   07  SPA-GMN-TEKIYOU-COMMENT1KBNLST
                                                   OCCURS  5.
                       09  SPA-GMN-TEKIYOU-COMMENT1KBNL
                                                   PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-TEKIYOU-COMMENT1KBNMEIL
                                                   PIC X(18).
                   07  SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
                                                   PIC 9(01).
      *
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-OCC         OCCURS  200.
                           11  SPA-GMN-TRENNUM     PIC 9(03).
                           11  SPA-GMN-THKNNUM     PIC X(03).
                           11  SPA-GMN-TSEIDONAME  PIC X(50).
                           11  SPA-GMN-TTEISYUTUSAKI
                                                   PIC X(10).
                           11  SPA-GMN-THKNJANUM   PIC X(08).
                           11  SPA-GMN-TFTNJANUMKBN-JYOKEN
                                                   PIC X(10).
                           11  SPA-GMN-TNYUGAIKBN  PIC X(06).
                           11  SPA-GMN-TSYOTOKUKBN PIC X(10).
                           11  SPA-GMN-TRJNKBN     PIC X(10).
                           11  SPA-GMN-TZAITAKU    PIC X(10).
                           11  SPA-GMN-TSTYUKYMD   PIC X(09).
                           11  SPA-GMN-TEDYUKYMD   PIC X(09).  
      *
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SELNUM          PIC 9(03).
                       09  SPA-GMN-HKNNUM          PIC X(03).
                       09  SPA-GMN-SEIDONAME       PIC X(50).
                       09  SPA-GMN-STYUKYMD        PIC X(10).
                       09  SPA-GMN-EDYUKYMD        PIC X(10).
                       09  SPA-GMN-KOUMOKU1. 
                           11  SPA-GMN-TEISYUTUSAKI-G.
                               13  SPA-GMN-TEISYUTUSAKI
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-TEISYUTUSAKIMEI
                                                   PIC X(10).
                           11  SPA-GMN-FTNJANUMKBN-JYOKEN-G.
                               13  SPA-GMN-FTNJANUMKBN-JYOKEN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-FTNJANUMKBN-JYOKENMEI
                                                   PIC X(30).
                           11  SPA-GMN-NYUGAIKBN-G.
                               13  SPA-GMN-NYUGAIKBN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-NYUGAIKBNMEI
                                                   PIC X(10).
                           11  SPA-GMN-SYOTOKUKBN-G.
                               13  SPA-GMN-SYOTOKUKBN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-SYOTOKUKBNMEI
                                                   PIC X(20).
                           11  SPA-GMN-ZAITAKU-G.
                               13  SPA-GMN-ZAITAKU
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-ZAITAKUMEI
                                                   PIC X(42).
                           11  SPA-GMN-RJNKBN-G.
                               13  SPA-GMN-RJNKBN  PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-RJNKBNMEI
                                                   PIC X(50).
                           11  SPA-GMN-FTNJANUMKBN-G.
                               13  SPA-GMN-FTNJANUMKBN
                                                   PIC X(02).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-FTNJANUMKBNMEI
                                                   PIC X(22).
                           11  SPA-GMN-JKYSNUMKBN-G.
                               13  SPA-GMN-JKYSNUMKBN
                                                   PIC X(02).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-JKYSNUMKBNMEI
                                                   PIC X(22).
                           11  SPA-GMN-RECEDEN-KORECKBN-G.
                               13  SPA-GMN-RECEDEN-KORECKBN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-RECEDEN-KORECKBNMEI
                                                   PIC X(10).
                           11  SPA-GMN-RECEDEN-FTNJANUMKBN-G.
                               13  SPA-GMN-RECEDEN-FTNJANUMKBN
                                                   PIC X(02).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-RECEDEN-FTNJANUMKBNMEI
                                                   PIC X(22).
                           11  SPA-GMN-RECEDEN-JKYSNUMKBN-G.
                               13  SPA-GMN-RECEDEN-JKYSNUMKBN
                                                   PIC X(02).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-RECEDEN-JKYSNUMKBNMEI
                                                   PIC X(22).
                           11  SPA-GMN-TEKIYOU-COMMENT1KBN-G.
                               13  SPA-GMN-TEKIYOU-COMMENT1KBN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-TEKIYOU-COMMENT1KBNMEI
                                                   PIC X(18).
                           11  SPA-GMN-TEKIYOU-COMMENT2KBN-G.
                               13  SPA-GMN-TEKIYOU-COMMENT2KBN
                                                   PIC X(01).
                               13  SPA-GMN-F1      PIC X(01).
                               13  SPA-GMN-TEKIYOU-COMMENT2KBNMEI
                                                   PIC X(18).
                           11  SPA-GMN-FTNJANUM-JYOKEN
                                                   PIC X(08).
                           11  SPA-GMN-HKNJANUM    PIC X(08).
                           11  SPA-GMN-HKNJANAME   PIC X(100).
                           11  SPA-GMN-TOKKI       PIC X(12).
                           11  SPA-GMN-FTNJANUM    PIC X(08).
                           11  SPA-GMN-JKYSNUM     PIC X(07).
                           11  SPA-GMN-KYURATE     PIC 9(03).
                           11  SPA-GMN-RECEDEN-TOKKI
                                                   PIC X(12).
                           11  SPA-GMN-RECEDEN-KYURATE
                                                   PIC 9(03).
                           11  SPA-GMN-RECEDEN-FTNJANUM
                                                   PIC X(08).
                           11  SPA-GMN-RECEDEN-JKYSNUM
                                                   PIC X(07).
                           11  SPA-GMN-TEKIYOU-COMMENT1
                                                   PIC X(52).
                           11  SPA-GMN-TEKIYOU-COMMENT2
                                                   PIC X(52).
      *  
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-KBNCD           PIC X(03).
                       09  SPA-NAI-STYUKYMD        PIC 9(08).
                       09  SPA-NAI-EDYUKYMD        PIC 9(08).
                       09  SPA-NAI-RECESKYKBN      PIC X(01).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBL-OCC         OCCURS  200.
                           11  SPA-NAI-TSTYUKYMD   PIC 9(08).
                           11  SPA-NAI-TEDYUKYMD   PIC 9(08).  
                           11  SPA-NAI-TKBNCD      PIC X(03).
                           11  SPA-NAI-TRJNKBN     PIC X(01).
                           11  SPA-NAI-TZAITAKU    PIC X(01).
                           11  SPA-NAI-TFTNJANUMKBN-JYOKEN
                                                   PIC X(01).
                           11  SPA-NAI-TFTNJANUM-JYOKEN
                                                   PIC X(08).
                           11  SPA-NAI-TRJNKBN-FLG PIC X(01).
                           11  SPA-NAI-TTEISYUTUSAKI
                                                   PIC X(01).
                           11  SPA-NAI-TNYUGAIKBN  PIC X(01).
                           11  SPA-NAI-TSYOTOKUKBN PIC X(01).
      *
              05  SPA-NAI-SELNUM                   PIC 9(03).
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-HKNJAINF                        PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).  
      *
           03  FLG-ROW                             PIC 9(01).
           03  FLG-CHK                             PIC 9(01).
           03  FLG-TOUROKU                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                 PIC 9(05).
           03  IDY                                 PIC 9(05).
           03  IDZ                                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-SELNUM                          PIC 9(03). 
      *
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
      *
           03  WRK-W61-ROW                         PIC S9(09).
      *
           03  WRK-2006-KBNCD.
               05  WRK-2006-KBNCD-9                PIC 9(03).
      *
           03  WRK-SELNUM-X.
               05  WRK-SELNUM-Z                    PIC ZZ9.
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN.
                   07  WRK-KBN1                    PIC X(01).
                   07  WRK-KBN2                    PIC X(01).
               05  WRK-KBNMEI                      PIC X(50).
      *  
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-GMN-CUR                         PIC 9(03).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  CONST-AREA.
           03  WRK-CONS-SRYYM-200804   PIC 9(08)   VALUE   20080401.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    レセプト特記事項編集情報
           COPY  "CPSK2006.INC".
      *
      *    保険者番号
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
       PROCEDURE                   DIVISION    USING
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
           MOVE    SPA-FREE        TO  SPA-W61
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W61         TO  SPA-FREE
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
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W61    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 320-WID1-SET-SEC
                   GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W61
           INITIALIZE                  SPA-W61
      *
           MOVE    "1"             TO  SPA-GMN-TEISYUTUSAKIL     (1)
           MOVE    "支払基金"      TO  SPA-GMN-TEISYUTUSAKIMEIL  (1)
           MOVE    "2"             TO  SPA-GMN-TEISYUTUSAKIL     (2)
           MOVE    "国保連合会"    TO  SPA-GMN-TEISYUTUSAKIMEIL  (2)
           MOVE    "6"             TO  SPA-GMN-TEISYUTUSAKIL     (3)
           MOVE    "広域連合"      TO  SPA-GMN-TEISYUTUSAKIMEIL  (3)
           MOVE    3               TO  SPA-GMN-TEISYUTUSAKI-MAX
      *
           MOVE    "1"             TO  SPA-GMN-ZAITAKUL          (1)
           MOVE    "在宅総合診療料を算定している場合のみ対象"
                                   TO  SPA-GMN-ZAITAKUMEIL       (1)
           MOVE    1               TO  SPA-GMN-ZAITAKU-MAX
      *
           MOVE    "1"             TO  SPA-GMN-FTNJANUMKBN-JYOKENL   (1)
           MOVE    "記載がある場合のみ対象"
                                   TO  SPA-GMN-FTNJANUMKBN-JYOKENMEIL(1)
           MOVE    "2"             TO  SPA-GMN-FTNJANUMKBN-JYOKENL   (2)
           MOVE    "記載がない場合のみ対象"
                                   TO  SPA-GMN-FTNJANUMKBN-JYOKENMEIL(2)
           MOVE    "3"             TO  SPA-GMN-FTNJANUMKBN-JYOKENL   (3)
           MOVE    "固定の負担者番号の場合のみ対象"
                                   TO  SPA-GMN-FTNJANUMKBN-JYOKENMEIL(3)
           MOVE    3               TO  SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL      (1)
           MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL   (1)
           MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL      (2)
           MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL   (2)
           MOVE    2               TO  SPA-GMN-NYUGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SYOTOKUKBNL     (1)
           MOVE    "低所得１のみ対象"
                                   TO  SPA-GMN-SYOTOKUKBNMEIL  (1)
           MOVE    "2"             TO  SPA-GMN-SYOTOKUKBNL     (2)
           MOVE    "低所得２のみ対象"
                                   TO  SPA-GMN-SYOTOKUKBNMEIL  (2)
           MOVE    2               TO  SPA-GMN-SYOTOKUKBN-MAX
      *
           MOVE    "01"            TO  SPA-GMN-FTNJANUMKBNL      (1)
           MOVE    "記録しない"    TO  SPA-GMN-FTNJANUMKBNMEIL   (1)
           MOVE    "02"            TO  SPA-GMN-FTNJANUMKBNL      (2)
           MOVE    "固定の文字列を記載する"
                                   TO  SPA-GMN-FTNJANUMKBNMEIL   (2)
           MOVE    "03"            TO  SPA-GMN-FTNJANUMKBNL      (3)
           MOVE    "前にゼロづめにして記載"
                                   TO  SPA-GMN-FTNJANUMKBNMEIL   (3)
           MOVE    3               TO  SPA-GMN-FTNJANUMKBN-MAX
      *
           MOVE    "01"            TO  SPA-GMN-JKYSNUMKBNL       (1)
           MOVE    "記録しない"    TO  SPA-GMN-JKYSNUMKBNMEIL    (1)
           MOVE    "02"            TO  SPA-GMN-JKYSNUMKBNL       (2)
           MOVE    "固定の文字列を記載する"
                                   TO  SPA-GMN-JKYSNUMKBNMEIL    (2)
           MOVE    2               TO  SPA-GMN-JKYSNUMKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-RJNKBNL           (1)
           MOVE    "老人医療（法別番号２７）の受給がある場合のみ対象"
                                   TO  SPA-GMN-RJNKBNMEIL        (1)
           MOVE    "2"             TO  SPA-GMN-RJNKBNL           (2)
           MOVE
                   "老人医療（法別番号２７）の受給がない場合のみ対象"
                                   TO  SPA-GMN-RJNKBNMEIL        (2)
           MOVE    2               TO  SPA-GMN-RJNKBN-MAX 
      *
           MOVE    "1"             TO  SPA-GMN-RECEDEN-KORECKBNL (1)
           MOVE    "記録しない"    TO  SPA-GMN-RECEDEN-KORECKBNMEIL
                                                                 (1)
           MOVE    1               TO  SPA-GMN-RECEDEN-KORECKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-TEKIYOU-COMMENT1KBNL(1)
           MOVE    "レセ電に記録しない"
                                   TO  SPA-GMN-TEKIYOU-COMMENT1KBNMEIL
                                                                   (1)
           MOVE    "2"             TO  SPA-GMN-TEKIYOU-COMMENT1KBNL(2)
           MOVE    "レセ電に記録する"
                                   TO  SPA-GMN-TEKIYOU-COMMENT1KBNMEIL
                                                                   (2)
           MOVE    2               TO  SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
      *
      *    レセプト特記事項編集情報
           PERFORM 3101-SPA-HENSYU-SEC
      *
           MOVE    1               TO  FLG-ROW
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト特記事項編集情報処理
      *****************************************************************
       3101-SPA-HENSYU-SEC         SECTION.
      *
           INITIALIZE              SPA-GMN-TBL
                                   SPA-NAI-TBL
                                   SPA-GMN-MAX
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    "2006"          TO  SYS-KANRICD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key5"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX            >=  200
               MOVE    SYSKANRI-REC        TO  SYS-2006-REC
      *
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  WRK-SELNUM-Z
               MOVE    WRK-SELNUM-X        TO  SPA-GMN-TRENNUM   (IDX)
               MOVE    SYS-2006-KBNCD      TO  SPA-NAI-TKBNCD    (IDX)
               MOVE    SYS-2006-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-2006-STYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-2006-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-2006-EDYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    SYS-2006-HKNNUM     TO  SPA-GMN-THKNNUM   (IDX)
               MOVE    SPACE               TO  HKNNUM-REC
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    SYS-2006-HKNNUM     TO  HKN-HKNNUM
               MOVE    "00"                TO  HKN-PAYKBN
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    SYS-2006-STYUKYMD   TO  SPA-NAI-STYUKYMD
               MOVE    SYS-2006-EDYUKYMD   TO  SPA-NAI-EDYUKYMD
               PERFORM 900-HKNNUM-READ-SEC
               MOVE    HKN-SEIDONAME       TO  SPA-GMN-TSEIDONAME (IDX)
               INITIALIZE                      SPA-NAI-STYUKYMD
                                               SPA-NAI-EDYUKYMD
      *
               MOVE    SYS-2006-HKNJANUM   TO  SPA-GMN-THKNJANUM  (IDX)
      *
               MOVE    SYS-2006-TEISYUTUSAKI 
                                           TO  WRK-KBN
                                               SPA-NAI-TTEISYUTUSAKI
                                                                 (IDX)
               PERFORM 5001-TEISYUTUSAKI-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-TTEISYUTUSAKI
                                                                 (IDX)
      *
               MOVE    SYS-2006-FTNJANUMKBN-JYOKEN
                                           TO
                                       SPA-NAI-TFTNJANUMKBN-JYOKEN (IDX)
               EVALUATE    SYS-2006-FTNJANUMKBN-JYOKEN
                   WHEN    "1"
                       MOVE    "記載がある"    TO
                                       SPA-GMN-TFTNJANUMKBN-JYOKEN (IDX)
                   WHEN    "2"
                       MOVE    "記載がない"    TO
                                       SPA-GMN-TFTNJANUMKBN-JYOKEN (IDX)
                   WHEN    "3"
                       MOVE    "固定"          TO
                                       SPA-GMN-TFTNJANUMKBN-JYOKEN (IDX)
                   WHEN    OTHER
                       MOVE    SPACE           TO
                                       SPA-GMN-TFTNJANUMKBN-JYOKEN (IDX)
               END-EVALUATE
      *
               MOVE    SYS-2006-FTNJANUM-JYOKEN
                                           TO
                                       SPA-NAI-TFTNJANUM-JYOKEN (IDX)
      *
               MOVE    SYS-2006-NYUGAIKBN  TO  WRK-KBN
                                               SPA-NAI-TNYUGAIKBN  (IDX)
               PERFORM 5001-NYUGAIKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-TNYUGAIKBN  (IDX)
      *
               MOVE    SYS-2006-SYOTOKUKBN TO  SPA-NAI-TSYOTOKUKBN (IDX)
               EVALUATE    SYS-2006-SYOTOKUKBN
                   WHEN    "1"
                       MOVE    "低所得１"      TO
                                               SPA-GMN-TSYOTOKUKBN (IDX)
                   WHEN    "2"
                       MOVE    "低所得２"      TO
                                               SPA-GMN-TSYOTOKUKBN (IDX)
                   WHEN    OTHER
                       MOVE    SPACE           TO
                                               SPA-GMN-TSYOTOKUKBN (IDX)
               END-EVALUATE
      *
               MOVE    SYS-2006-RJNKBN     TO  SPA-NAI-TRJNKBN   (IDX)
               EVALUATE    SYS-2006-RJNKBN
                   WHEN    "1"
                       MOVE    "受給がある"
                                           TO  SPA-GMN-TRJNKBN   (IDX)
                   WHEN    "2"
                       MOVE    "受給がない"
                                           TO  SPA-GMN-TRJNKBN   (IDX)
                   WHEN    OTHER
                       MOVE    SPACE       TO  SPA-GMN-TRJNKBN   (IDX)
               END-EVALUATE
      *
               MOVE    SYS-2006-ZAITAKU    TO  SPA-NAI-TZAITAKU  (IDX)
                                               WRK-KBN
               PERFORM 5001-ZAITAKU-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-TZAITAKU  (IDX)
      *
               IF    ( SYS-2006-EDYUKYMD   >=  WRK-CONS-SRYYM-200804 )
               AND   ( SYS-2006-RJNKBN NOT =   SPACE                 )
                   MOVE    "1"             TO  SPA-NAI-TRJNKBN-FLG (IDX)
               END-IF
      * 
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key5"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-MAX
      *
          MOVE    1                TO  SPA-GMN-CUR
 
           .
       3101-SPA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       320-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "2002"
               WHEN "2004"
               WHEN "2005"
                       PERFORM 421-TOUROKU-SEC
               WHEN "2003"
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
      *
           .
       320-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-SAKUJO-GID-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 420-TOUROKU-GID-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-MAX
               IF      SPA-NAI-TRJNKBN-FLG (IDX)   =   "1"
                   MOVE    "2003"                  TO  SPA-WIDCD 
                   GO  TO  210-BACK-EXT
               END-IF
           END-PERFORM
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *
           MOVE    "W61"               TO  SPA-MOTOPG
           MOVE    "W01"               TO  SPA-SAKIPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 400-SELNUM-CHK-SEC
      *    一覧
           WHEN    ANY             ALSO    "TOKKILST"
                   PERFORM 400-TOKKILST-CHK-SEC
      *
           WHEN    OTHER
                   PERFORM 410-ENTRY-SYORI-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分　入力処理
      *****************************************************************
       410-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       410-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W61-HKNNUM          TO  SPA-GMN-HKNNUM
           MOVE    W61-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
           MOVE    W61-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W61-TEISYUTUSAKI    TO  SPA-GMN-TEISYUTUSAKI-G
           MOVE    W61-HKNJANUM        TO  SPA-GMN-HKNJANUM
           MOVE    W61-FTNJANUMKBN-JYOKEN
                                       TO  SPA-GMN-FTNJANUMKBN-JYOKEN-G
           MOVE    W61-FTNJANUM-JYOKEN TO  SPA-GMN-FTNJANUM-JYOKEN
           MOVE    W61-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    W61-SYOTOKUKBN      TO  SPA-GMN-SYOTOKUKBN-G
           MOVE    W61-ZAITAKU         TO  SPA-GMN-ZAITAKU-G
           MOVE    W61-RJNKBN          TO  SPA-GMN-RJNKBN-G
           MOVE    W61-KYURATE         TO  SPA-GMN-KYURATE 
           MOVE    W61-TOKKI           TO  SPA-GMN-TOKKI 
           MOVE    W61-FTNJANUMKBN     TO  SPA-GMN-FTNJANUMKBN-G
           MOVE    W61-JKYSNUMKBN      TO  SPA-GMN-JKYSNUMKBN-G
           MOVE    W61-FTNJANUM        TO  SPA-GMN-FTNJANUM
           MOVE    W61-JKYSNUM         TO  SPA-GMN-JKYSNUM
           MOVE    W61-TEKIYOU-COMMENT1
                                       TO  SPA-GMN-TEKIYOU-COMMENT1
           MOVE    W61-TEKIYOU-COMMENT1KBN
                                       TO  SPA-GMN-TEKIYOU-COMMENT1KBN-G
           MOVE    W61-TEKIYOU-COMMENT2
                                       TO  SPA-GMN-TEKIYOU-COMMENT2
           MOVE    W61-TEKIYOU-COMMENT2KBN
                                       TO  SPA-GMN-TEKIYOU-COMMENT2KBN-G
           MOVE    W61-RECEDEN-KORECKBN
                                       TO  SPA-GMN-RECEDEN-KORECKBN-G
           MOVE    W61-RECEDEN-KYURATE TO  SPA-GMN-RECEDEN-KYURATE
           MOVE    W61-RECEDEN-TOKKI   TO  SPA-GMN-RECEDEN-TOKKI
           MOVE    W61-RECEDEN-FTNJANUMKBN
                                       TO  SPA-GMN-RECEDEN-FTNJANUMKBN-G
           MOVE    W61-RECEDEN-JKYSNUMKBN
                                       TO  SPA-GMN-RECEDEN-JKYSNUMKBN-G
           MOVE    W61-RECEDEN-FTNJANUM
                                       TO  SPA-GMN-RECEDEN-FTNJANUM
           MOVE    W61-RECEDEN-JKYSNUM TO  SPA-GMN-RECEDEN-JKYSNUM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    有効開始年月日
           PERFORM 41021-STYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効終了年月日
           PERFORM 41021-EDYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    保険番号
           PERFORM 41021-HKNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    提出先
           PERFORM 41021-TEISYUTUSAKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    保険者番号
           PERFORM 41021-HKNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    負担者番号
           PERFORM 41021-FTNJANUMKB-JYOKEN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    負担者番号内容
           PERFORM 41021-FTNJANUM-JYOKEN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    入外区分
           PERFORM 41021-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    所得区分
           PERFORM 41021-SYOTOKUKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    老人区分
           PERFORM 41021-RJNKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    在総診区分
           PERFORM 41021-ZAITAKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    特記事項
           PERFORM 41021-TOKKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    給付割合
           PERFORM 41021-KYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    負担者番号記載区分
           PERFORM 41021-FTNJANUMKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    負担者番号記載内容
           PERFORM 41021-FTNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    受給者番号記載区分
           PERFORM 41021-JKYSNUMKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    受給者番号
           PERFORM 41021-JKYSNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    適用欄上段コメント
           PERFORM 41021-TEKIYOU-COM1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    適用欄上段コメントレセ電編集区分
           PERFORM 41021-TEKIYOU-COM1KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    適用欄下段コメント
           PERFORM 41021-TEKIYOU-COM2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    適用欄下段コメントレセ電編集区分
           PERFORM 41021-TEKIYOU-COM2KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・特記事項
           PERFORM 41021-RECEDEN-TOKKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    レセ電・給付割合
           PERFORM 41021-RECEDEN-KYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・公費レコード区分
           PERFORM 41021-RECEDEN-KORECKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・負担者番号記載区分
           PERFORM 41021-RECEDEN-FTNJAKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・負担者番号
           PERFORM 41021-RECEDEN-FTNJA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・受給者番号記載区分
           PERFORM 41021-RECEDEN-JKYSNUMKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    レセ電・受給者番号
           PERFORM 41021-RECEDEN-JKYSNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       400-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *     
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-SELNUM
      *
           MOVE    W61-SELNUM      TO  SPA-GMN-SELNUM
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR 
               GO  TO  400-SELNUM-CHK-EXT 
           ELSE
               IF    ( SPA-GMN-SELNUM      >=  1           )
               AND   ( SPA-GMN-SELNUM      <=  SPA-GMN-MAX )
                   CONTINUE 
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
                   GO  TO  400-SELNUM-CHK-EXT 
               END-IF
           END-IF   
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNUM
      *
           PERFORM 4001-SELNUM-HENSYU-SEC               
           .
       400-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4001-SELNUM-HENSYU-SEC          SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2006"              TO  SYS-KANRICD
           MOVE    SPA-NAI-TKBNCD    (SPA-NAI-SELNUM)
                                       TO  SYS-KBNCD
           MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                       TO  SYS-STYUKYMD
           MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                       TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF     FLG-SYSKANRI         =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2006-REC
               MOVE    SYS-2006-KBNCD      TO  SPA-NAI-KBNCD
      *
               MOVE    SYS-2006-STYUKYMD   TO  SPA-NAI-STYUKYMD
               MOVE    SYS-2006-STYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               MOVE    SYS-2006-EDYUKYMD   TO  SPA-NAI-EDYUKYMD
               MOVE    SYS-2006-EDYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SYS-2006-HKNNUM     TO  SPA-GMN-HKNNUM
               MOVE    SPACE               TO  HKNNUM-REC
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    SPA-GMN-HKNNUM      TO  HKN-HKNNUM
               MOVE    "00"                TO  HKN-PAYKBN
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               PERFORM 900-HKNNUM-READ-SEC
               MOVE    HKN-SEIDONAME       TO SPA-GMN-SEIDONAME
               MOVE    HKN-RECESKYKBN      TO  SPA-NAI-RECESKYKBN
      *
               MOVE    SYS-2006-HKNJANUM   TO SPA-GMN-HKNJANUM
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
               MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC        TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               MOVE    HKNJA-HKNJANAME     TO SPA-GMN-HKNJANAME
      *
               MOVE    SYS-2006-TEISYUTUSAKI 
                                           TO  SPA-GMN-TEISYUTUSAKI
                                               WRK-KBN
               PERFORM 5001-TEISYUTUSAKI-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-TEISYUTUSAKIMEI
      *
               MOVE    SYS-2006-FTNJANUMKBN-JYOKEN
                                           TO
                                           SPA-GMN-FTNJANUMKBN-JYOKEN
                                           WRK-KBN
               PERFORM 5001-FTNJANUMKB-JYOKEN-HEN-SEC
               MOVE    WRK-KBNMEI          TO
                                           SPA-GMN-FTNJANUMKBN-JYOKENMEI
      *
               MOVE    SYS-2006-FTNJANUM-JYOKEN
                                           TO  SPA-GMN-FTNJANUM-JYOKEN
      *
               MOVE    SYS-2006-NYUGAIKBN  TO  SPA-GMN-NYUGAIKBN
                                               WRK-KBN
               PERFORM 5001-NYUGAIKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-NYUGAIKBNMEI
      *
               MOVE    SYS-2006-SYOTOKUKBN TO  SPA-GMN-SYOTOKUKBN
                                               WRK-KBN
               PERFORM 5001-SYOTOKUKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-SYOTOKUKBNMEI
      *
               MOVE    SYS-2006-RJNKBN     TO  SPA-GMN-RJNKBN
                                               WRK-KBN
               PERFORM 5001-RJNKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-RJNKBNMEI
      *
               MOVE    SYS-2006-ZAITAKU    TO  SPA-GMN-ZAITAKU
                                               WRK-KBN
               PERFORM 5001-ZAITAKU-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-ZAITAKUMEI
      *
               MOVE    SYS-2006-TOKKI      TO  SPA-GMN-TOKKI
               MOVE    SYS-2006-KYURATE    TO  SPA-GMN-KYURATE
      *
               MOVE    SYS-2006-FTNJANUMKBN
                                           TO  SPA-GMN-FTNJANUMKBN
                                               WRK-KBN
               PERFORM 5001-FTNJANUMKB-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-FTNJANUMKBNMEI
      *
               MOVE    SYS-2006-FTNJANUM   TO  SPA-GMN-FTNJANUM
      *
               MOVE    SYS-2006-JKYSNUMKBN TO  SPA-GMN-JKYSNUMKBN
                                               WRK-KBN
               PERFORM 5001-JKYSNUMKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-JKYSNUMKBNMEI
      *
               MOVE    SYS-2006-JKYSNUM    TO  SPA-GMN-JKYSNUM
      *
               MOVE    SYS-2006-RECEDEN-TOKKI
                                           TO  SPA-GMN-RECEDEN-TOKKI
               MOVE    SYS-2006-RECEDEN-KYURATE
                                           TO  SPA-GMN-RECEDEN-KYURATE
      *
               MOVE    SYS-2006-RECEDEN-KORECKBN
                                           TO  SPA-GMN-RECEDEN-KORECKBN
                                               WRK-KBN
               PERFORM 5001-RECEDEN-KORECKB-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO
                                           SPA-GMN-RECEDEN-KORECKBNMEI
      *
               MOVE    SYS-2006-RECEDEN-FTNJANUMKBN
                                           TO
                                           SPA-GMN-RECEDEN-FTNJANUMKBN
                                           WRK-KBN
               PERFORM 5001-FTNJANUMKB-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-RECEDEN-FTNJANUMKBNMEI
      *
               MOVE    SYS-2006-RECEDEN-FTNJANUM
                                           TO  SPA-GMN-RECEDEN-FTNJANUM
      *
               MOVE    SYS-2006-RECEDEN-JKYSNUMKBN
                                           TO
                                           SPA-GMN-RECEDEN-JKYSNUMKBN
                                           WRK-KBN
               PERFORM 5001-JKYSNUMKBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO
                                           SPA-GMN-RECEDEN-JKYSNUMKBNMEI
      *
               MOVE    SYS-2006-RECEDEN-JKYSNUM
                                           TO  SPA-GMN-RECEDEN-JKYSNUM
      *
               MOVE    SYS-2006-TEKIYOU-COMMENT1KBN
                                           TO
                                           SPA-GMN-TEKIYOU-COMMENT1KBN
                                           WRK-KBN
               PERFORM 5001-TEKIYOU-COM1KBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-TEKIYOU-COMMENT1KBNMEI
      *
               MOVE    SYS-2006-TEKIYOU-COMMENT1
                                           TO  SPA-GMN-TEKIYOU-COMMENT1
      *
               MOVE    SYS-2006-TEKIYOU-COMMENT2KBN
                                           TO
                                           SPA-GMN-TEKIYOU-COMMENT2KBN
                                           WRK-KBN
               PERFORM 5001-TEKIYOU-COM1KBN-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-TEKIYOU-COMMENT2KBNMEI
      *
               MOVE    SYS-2006-TEKIYOU-COMMENT2
                                           TO  SPA-GMN-TEKIYOU-COMMENT2
      *
               MOVE    SPA-NAI-SELNUM      TO  W61-ROW
      *
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0101"              TO  SPA-ERRCD
           END-IF    
           .
       4001-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       400-TOKKILST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               IF      W61-SELECT (IDX)    =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX         TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                 SPA-GMN-KOUMOKU
                                      SPA-NAI-KOUMOKU
      *
           MOVE    SPA-GMN-TRENNUM (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SELNUM
      *                                 
           PERFORM 4001-SELNUM-HENSYU-SEC               
           .
       400-TOKKILST-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC          SECTION.
      *     
           MOVE    ZERO                TO  SPA-NAI-STYUKYMD 
      *
           IF      SPA-GMN-STYUKYMD    =   SPACE
               IF      WRK-MCP-WIDGET      =   "STYUKYMD"
                   MOVE    "00000000"          TO  SPA-GMN-STYUKYMD
               END-IF
               GO  TO  41021-STYUKYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               IF      SPA-NAI-STYUKYMD
                                       NOT =   ZERO
                   IF      SPA-NAI-STYUKYMD (7:2)
                                               =   "01"
                       CONTINUE
                   ELSE
                       MOVE    "0106"              TO  SPA-ERRCD
                   END-IF
               END-IF  
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-STYUKYMD    >=  WRK-CONS-SRYYM-200804
                   MOVE    SPACE               TO  SPA-GMN-RJNKBN-G
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               IF      WRK-MCP-WIDGET      =   "EDYUKYMD"
                   MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
                   MOVE    99999999            TO  SPA-NAI-EDYUKYMD
               ELSE
                   MOVE    ZERO                TO  SPA-NAI-EDYUKYMD
                   GO  TO  41021-EDYUKYMD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
      *        末日チェック
               IF      SPA-NAI-EDYUKYMD
                                       NOT =   99999999 
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-EDYUKYMD (1:6)
                                               TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                   IF      LNK-DAY7-KOYOMI NOT =   SPA-NAI-EDYUKYMD
                       MOVE    "0107"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    開始日との関連チェック   
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-STYUKYMD    NOT =   SPACE )  AND
                     ( SPA-GMN-EDYUKYMD    NOT =   SPACE )
                   IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                       MOVE    "0020"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号チェック処理
      *****************************************************************
       41021-HKNNUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-SEIDONAME
                                       SPA-NAI-RECESKYKBN
      *
           IF      SPA-GMN-HKNNUM  =   SPACE
               CONTINUE 
           ELSE 
               MOVE    SPACE           TO  HKNNUM-REC
               INITIALIZE                  HKNNUM-REC
               MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
               MOVE    SPA-GMN-HKNNUM  TO  HKN-HKNNUM
               MOVE    "00"            TO  HKN-PAYKBN
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-READ-SEC
      *
               IF      FLG-HKNNUM      =   ZERO
                   MOVE    HKN-SEIDONAME   TO  SPA-GMN-SEIDONAME
                   MOVE    HKN-RECESKYKBN  TO  SPA-NAI-RECESKYKBN
                   IF      HKN-HKNNUM(1:1) =   "9"
                       MOVE    "0015"                  TO  SPA-ERRCD
                   ELSE
                       IF      HKN-HKNKOHSBTKBN    
                                               =   "5" OR  "6" OR  "7"
                           CONTINUE
                       ELSE
                           MOVE    "0015"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "0004"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    提出先入力処理
      *****************************************************************
       41021-TEISYUTUSAKI-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TEISYUTUSAKI
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-TEISYUTUSAKI-G
           ELSE
               MOVE    SPA-GMN-TEISYUTUSAKI
                                           TO  WRK-KBN
               PERFORM 5001-TEISYUTUSAKI-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0005"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                               SPA-GMN-TEISYUTUSAKIMEI
      *
                   IF    ( SPA-NAI-EDYUKYMD
                                           <   WRK-CONS-SRYYM-200804 )
                   AND   ( SPA-GMN-TEISYUTUSAKI
                                           =   "6"                   ) 
                       MOVE    SPACE       TO  SPA-GMN-TEISYUTUSAKI-G
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       41021-HKNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE            TO  SPA-GMN-HKNJANAME
      *
           IF      SPA-GMN-HKNJANUM =   SPACE
               CONTINUE
           ELSE 
               MOVE    SPACE           TO  HKNJAINF-REC
               INITIALIZE                  HKNJAINF-REC
               MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM
                                       TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC    TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-READ-SEC
               IF      FLG-HKNJAINF    =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO SPA-GMN-HKNJANAME
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号区分入力処理
      *****************************************************************
       41021-FTNJANUMKB-JYOKEN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-FTNJANUMKBN-JYOKEN
                                       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-FTNJANUMKBN-JYOKEN-G
                                           SPA-GMN-FTNJANUM-JYOKEN
           ELSE
               MOVE    SPA-GMN-FTNJANUMKBN-JYOKEN
                                           TO  WRK-KBN
               PERFORM 5001-FTNJANUMKB-JYOKEN-HEN-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                           SPA-GMN-FTNJANUMKBN-JYOKENMEI
                   IF      SPA-GMN-FTNJANUMKBN-JYOKEN =   "1" OR  "2"
                       MOVE    SPACE           TO
                                               SPA-GMN-FTNJANUM-JYOKEN
                   END-IF
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "FTNJANUMKBN_JYOKEN"    )
               AND   ( SPA-GMN-FTNJANUMKBN-JYOKEN
                                           =   SPACE   OR  "1" OR  "2" )
                   MOVE    21                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-FTNJANUMKB-JYOKEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号記載入力処理
      *****************************************************************
       41021-FTNJANUM-JYOKEN-CHK-SEC      SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    16                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-FTNJANUM-JYOKEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分入力処理
      *****************************************************************
       41021-NYUGAIKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-NYUGAIKBN
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               MOVE    SPA-GMN-NYUGAIKBN
                                           TO  WRK-KBN
               PERFORM 5001-NYUGAIKBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0033"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO  SPA-GMN-NYUGAIKBNMEI
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    21                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    所得区分入力処理
      *****************************************************************
       41021-SYOTOKUKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SYOTOKUKBN
                                       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-SYOTOKUKBN-G
           ELSE
               MOVE    SPA-GMN-SYOTOKUKBN
                                           TO  WRK-KBN
               PERFORM 5001-SYOTOKUKBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0034"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO  SPA-GMN-SYOTOKUKBNMEI
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   ELSE
                       IF      SPA-GMN-HKNNUM(1:1) =   "0"
                           MOVE    "0035"              TO  SPA-ERRCD
                       ELSE
                           IF      SPA-NAI-RECESKYKBN  =   "3"
                               MOVE    "0036"              TO  SPA-ERRCD
                           END-IF
      *********************IF      SPA-GMN-TEISYUTUSAKI    =   SPACE
      *                        MOVE    "0037"              TO  SPA-ERRCD
      *                    ELSE
      *                        EVALUATE    SPA-NAI-RECESKYKBN
      *                          WHEN    "0"
      *                            CONTINUE
      *                          WHEN    "1"
      *                            IF      SPA-GMN-TEISYUTUSAKI =   "1"
      *                                CONTINUE
      *                            ELSE 
      *                                MOVE    "0036"      TO  SPA-ERRCD
      *                            END-IF
      *                          WHEN    "2"
      *                            IF      SPA-GMN-TEISYUTUSAKI =   "2"
      *                                                        OR  "6"
      *                                CONTINUE
      *                            ELSE 
      *                                MOVE    "0036"      TO  SPA-ERRCD
      *                            END-IF
      *                          WHEN    "3"
      *                            MOVE    "0036"      TO  SPA-ERRCD
      *                          WHEN    "4"
      *                            IF      SPA-GMN-TEISYUTUSAKI =   "2"
      *                                CONTINUE
      *                             ELSE 
      *                                MOVE    "0036"      TO  SPA-ERRCD
      *                            END-IF
      *                          WHEN    "5"
      *                            IF      SPA-GMN-TEISYUTUSAKI =   "6"
      *                                CONTINUE
      *                            ELSE 
      *                                MOVE    "0036"      TO  SPA-ERRCD
      *                            END-IF
      *                        END-EVALUATE
      *********************END-IF
                       END-IF 
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    22                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-SYOTOKUKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    老人保険入力処理
      *****************************************************************
       41021-RJNKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-RJNKBN      =   SPACE
               MOVE    SPACE               TO  SPA-GMN-RJNKBN-G
           ELSE
               MOVE    SPA-GMN-RJNKBN      TO  WRK-KBN
               PERFORM 5001-RJNKBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                               SPA-GMN-RJNKBNMEI
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-STYUKYMD    >=  WRK-CONS-SRYYM-200804
                   MOVE    SPACE               TO  SPA-GMN-RJNKBN-G
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RJNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診区分入力処理
      *****************************************************************
       41021-ZAITAKU-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-ZAITAKU     =   SPACE
               MOVE    SPACE               TO  SPA-GMN-ZAITAKU-G
           ELSE
               MOVE    SPA-GMN-ZAITAKU     TO  WRK-KBN
               PERFORM 5001-ZAITAKU-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0008"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                               SPA-GMN-ZAITAKUMEI
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-ZAITAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項入力処理
      *****************************************************************
       41021-TOKKI-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TOKKI       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-TOKKI       TO  KANACHK-MAE-INPUT
               MOVE    12                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-TOKKI
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOKKI
               END-IF
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   2    )
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOKKI
                   IF      KANACHK-RC3         =   2
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "TOKKI" )
               AND   ( SPA-GMN-HKNNUM      =   SPACE   )
                   MOVE    100                 TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TOKKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    給付割合入力処理
      *****************************************************************
       41021-KYURATE-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-KYURATE     =   ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-KYURATE     >   100
                   MOVE    "0010"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-HKNNUM          =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-KYURATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号記載区分入力処理
      *****************************************************************
       41021-FTNJANUMKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-FTNJANUMKBN =   SPACE
               MOVE    SPACE               TO  SPA-GMN-FTNJANUMKBN-G
                                               SPA-GMN-FTNJANUM
           ELSE
               MOVE    SPA-GMN-FTNJANUMKBN TO  WRK-KBN
               PERFORM 5001-FTNJANUMKB-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0016"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                               SPA-GMN-FTNJANUMKBNMEI
                   IF      SPA-GMN-FTNJANUMKBN =   "01"    OR  "03"
                       MOVE    SPACE               TO  SPA-GMN-FTNJANUM
                   END-IF
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "FTNJANUMKBN"          )
               AND   ( SPA-GMN-FTNJANUMKBN =   SPACE   OR  "01"    OR
                                               "03"                   )
                   MOVE    13                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-FTNJANUMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号入力処理
      *****************************************************************
       41021-FTNJANUM-CHK-SEC      SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    12                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号記載区分入力処理
      *****************************************************************
       41021-JKYSNUMKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-JKYSNUMKBN  =   SPACE
               MOVE    SPACE               TO  SPA-GMN-JKYSNUMKBN-G
                                               SPA-GMN-JKYSNUM
           ELSE
               MOVE    SPA-GMN-JKYSNUMKBN  TO  WRK-KBN
               PERFORM 5001-JKYSNUMKBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0018"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO  SPA-GMN-JKYSNUMKBNMEI
                   IF      SPA-GMN-JKYSNUMKBN  =   "01"
                       MOVE    SPACE           TO  SPA-GMN-JKYSNUM
                   END-IF
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "JKYSNUMKBN"     )
               AND   ( SPA-GMN-JKYSNUMKBN  =   SPACE   OR  "01" )
                   MOVE    17                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-JKYSNUMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号入力処理
      *****************************************************************
       41021-JKYSNUM-CHK-SEC      SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    14                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-JKYSNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄上段コメント入力処理
      *****************************************************************
       41021-TEKIYOU-COM1-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TEKIYOU-COMMENT1    =   SPACE
               MOVE    SPACE               TO
                                           SPA-GMN-TEKIYOU-COMMENT1KBN-G
           ELSE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-TEKIYOU-COMMENT1
                                           TO  KANACHK-MAE-INPUT
               MOVE    52                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-TEKIYOU-COMMENT1
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO
                                               SPA-GMN-TEKIYOU-COMMENT1
               END-IF
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   2    )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEKIYOU-COMMENT1
                   IF      KANACHK-RC3         =   2
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0026"              TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "TEKIYOU_COMMENT1" )
               AND   ( SPA-GMN-TEKIYOU-COMMENT1
                                           =   SPACE              )
                   MOVE    19                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TEKIYOU-COM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄上段コメントレセ電編集区分入力処理
      *****************************************************************
       41021-TEKIYOU-COM1KBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TEKIYOU-COMMENT1KBN =   SPACE
               MOVE    SPACE               TO
                                           SPA-GMN-TEKIYOU-COMMENT1KBN-G
           ELSE
               MOVE    SPA-GMN-TEKIYOU-COMMENT1KBN
                                           TO  WRK-KBN
               PERFORM 5001-TEKIYOU-COM1KBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0027"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-TEKIYOU-COMMENT1KBNMEI
                   IF      SPA-GMN-TEKIYOU-COMMENT1
                                               =   SPACE
                       MOVE    "0030"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TEKIYOU-COM1KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄下段コメント入力処理
      *****************************************************************
       41021-TEKIYOU-COM2-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TEKIYOU-COMMENT2    =   SPACE
               MOVE    SPACE               TO
                                           SPA-GMN-TEKIYOU-COMMENT2KBN-G
           ELSE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-TEKIYOU-COMMENT2
                                           TO  KANACHK-MAE-INPUT
               MOVE    52                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-TEKIYOU-COMMENT2
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO
                                               SPA-GMN-TEKIYOU-COMMENT2
               END-IF
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   2    )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEKIYOU-COMMENT2
                   IF      KANACHK-RC3         =   2
                       MOVE    "0032"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0028"              TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-GMN-HKNNUM      =   SPACE
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "TEKIYOU_COMMENT2" )
               AND   ( SPA-GMN-TEKIYOU-COMMENT2
                                           =   SPACE              )
                   MOVE    100                 TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    19                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TEKIYOU-COM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄下段コメントレセ電編集区分入力処理
      *****************************************************************
       41021-TEKIYOU-COM2KBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-TEKIYOU-COMMENT2KBN =   SPACE
               MOVE    SPACE               TO
                                           SPA-GMN-TEKIYOU-COMMENT2KBN-G
           ELSE
               MOVE    SPA-GMN-TEKIYOU-COMMENT2KBN
                                           TO  WRK-KBN
               PERFORM 5001-TEKIYOU-COM1KBN-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0029"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-TEKIYOU-COMMENT2KBNMEI
                   IF      SPA-GMN-TEKIYOU-COMMENT2
                                               =   SPACE
                       MOVE    "0031"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-TEKIYOU-COM2KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：特記事項入力処理
      *****************************************************************
       41021-RECEDEN-TOKKI-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-RECEDEN-TOKKI
                                       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    SPA-GMN-RECEDEN-TOKKI
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0011"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-RECEDEN-TOKKI
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "RECEDEN_TOKKI" )
               AND   ( SPA-GMN-HKNNUM      =   SPACE           )
                   MOVE    999                 TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    100                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-TOKKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：給付割合入力処理
      *****************************************************************
       41021-RECEDEN-KYURATE-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-RECEDEN-KYURATE     =   ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-RECEDEN-KYURATE     >   100
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-HKNNUM          =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    101                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-KYURATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：公費レコード区分入力処理
      *****************************************************************
       41021-RECEDEN-KORECKBN-CHK-SEC  SECTION.
      *
           IF      SPA-GMN-RECEDEN-KORECKBN     
                                       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-RECEDEN-KORECKBN-G
           ELSE
               MOVE    SPA-GMN-RECEDEN-KORECKBN
                                       TO  WRK-KBN
               PERFORM 5001-RECEDEN-KORECKB-HENSYU-SEC
               IF      WRK-KBNMEI          =   SPACE
                   MOVE    "0013"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI          TO
                                           SPA-GMN-RECEDEN-KORECKBNMEI
                   IF      SPA-GMN-RECEDEN-KORECKBN
                                               =   "1"
                       MOVE    SPACE           TO
                                           SPA-GMN-RECEDEN-FTNJANUMKBN-G
                                           SPA-GMN-RECEDEN-FTNJANUM
                                           SPA-GMN-RECEDEN-JKYSNUMKBN-G
                                           SPA-GMN-RECEDEN-JKYSNUM
                   END-IF
                   IF      SPA-GMN-HKNNUM          =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "RECEDEN_KORECKBN" )
               AND   ( SPA-GMN-RECEDEN-KORECKBN
                                           =   "1"                )
                   MOVE    999             TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    102                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-KORECKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：負担者番号記載区分入力処理
      *****************************************************************
       41021-RECEDEN-FTNJAKBN-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-RECEDEN-FTNJANUMKBN 
                                       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-RECEDEN-FTNJANUMKBN-G
                                           SPA-GMN-RECEDEN-FTNJANUM
           ELSE
               MOVE    SPA-GMN-RECEDEN-FTNJANUMKBN
                                       TO  WRK-KBN
               PERFORM 5001-FTNJANUMKB-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0020"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI      TO
                                       SPA-GMN-RECEDEN-FTNJANUMKBNMEI
                   IF      SPA-GMN-RECEDEN-FTNJANUMKBN =   "01"    OR
                                                           "03"
                       MOVE    SPACE       TO  SPA-GMN-RECEDEN-FTNJANUM
                   END-IF
                   IF      SPA-GMN-HKNNUM  =   SPACE
                       MOVE    "0014"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "RECEDEN_FTNJANUMKBN"  )
               AND   ( SPA-GMN-RECEDEN-FTNJANUMKBN
                                           =   SPACE   OR  "01"    OR
                                               "03"                   )
                   MOVE    105                 TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    103                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-FTNJAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：負担者番号記載入力処理
      *****************************************************************
       41021-RECEDEN-FTNJA-CHK-SEC      SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    104                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-FTNJA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：受給者番号記載区分入力処理
      *****************************************************************
       41021-RECEDEN-JKYSNUMKB-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-RECEDEN-JKYSNUMKBN  
                                       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-RECEDEN-JKYSNUMKBN-G
                                           SPA-GMN-RECEDEN-JKYSNUM
           ELSE
               MOVE    SPA-GMN-RECEDEN-JKYSNUMKBN
                                       TO  WRK-KBN
               PERFORM 5001-JKYSNUMKBN-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0022"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KBNMEI      TO
                                           SPA-GMN-RECEDEN-JKYSNUMKBNMEI
                   IF      SPA-GMN-RECEDEN-JKYSNUMKBN  =   "01"
                       MOVE    SPACE       TO  SPA-GMN-RECEDEN-JKYSNUM
                   END-IF
                   IF      SPA-GMN-HKNNUM  =   SPACE
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( WRK-MCP-WIDGET      =   "RECEDEN_JKYSNUMKBN" )
               AND   ( SPA-GMN-RECEDEN-JKYSNUMKBN
                                           =   SPACE   OR  "01"     )
                   MOVE    999                 TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    105                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-JKYSNUMKB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：受給者番号記載入力処理
      *****************************************************************
       41021-RECEDEN-JKYSNUM-CHK-SEC      SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    106                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-RECEDEN-JKYSNUM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *   
      *    編集項目が全て未入力
           IF    ( SPA-GMN-TOKKI       =   SPACE )
           AND   ( SPA-GMN-KYURATE     =   ZERO  )
           AND   ( SPA-GMN-FTNJANUMKBN =   SPACE )
           AND   ( SPA-GMN-JKYSNUMKBN  =   SPACE )
           AND   ( SPA-GMN-RECEDEN-TOKKI
                                       =   SPACE )
           AND   ( SPA-GMN-RECEDEN-KYURATE
                                       =   ZERO  )
           AND   ( SPA-GMN-RECEDEN-KORECKBN
                                       =   SPACE )
           AND   ( SPA-GMN-RECEDEN-FTNJANUMKBN
                                       =   SPACE )
           AND   ( SPA-GMN-RECEDEN-JKYSNUMKBN
                                       =   SPACE )
           AND   ( SPA-GMN-TEKIYOU-COMMENT1
                                       =   SPACE )
           AND   ( SPA-GMN-TEKIYOU-COMMENT2
                                       =   SPACE )
               MOVE    "0103"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
      *    負担者番号（条件）と負担者番号のチェック
           IF    ( SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-FTNJANUM-JYOKEN
                                           =   SPACE
                   IF      SPA-GMN-FTNJANUMKBN-JYOKEN  =   "3"
                       MOVE    "0025"          TO  SPA-ERRCD
                       MOVE    16              TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-FTNJANUMKBN-JYOKEN  =   "3"
                       CONTINUE
                   ELSE
                       MOVE    "0025"          TO  SPA-ERRCD
                       MOVE    16              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
      *    負担者番号記載区分と負担者番号のチェック
           IF    ( SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-FTNJANUM    =   SPACE
                   IF      SPA-GMN-FTNJANUMKBN  =   "02"
                       MOVE    "0017"              TO  SPA-ERRCD
                       MOVE    12                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-FTNJANUMKBN  =   "02"
                       CONTINUE
                   ELSE
                       MOVE    "0017"              TO  SPA-ERRCD
                       MOVE    12                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
      *    受給者番号記載区分と受給者番号のチェック
           IF    ( SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-JKYSNUM     =   SPACE
                   IF      SPA-GMN-JKYSNUMKBN  =   "02"
                       MOVE    "0019"              TO  SPA-ERRCD
                       MOVE    14                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-JKYSNUMKBN  =   "02"
                       CONTINUE
                   ELSE
                       MOVE    "0019"              TO  SPA-ERRCD
                       MOVE    14                  TO  SPA-GMN-CUR
                   END-IF
               END-IF 
           END-IF
      *
      *    レセ電：負担者番号記載区分とレセ電：負担者番号のチェック
           IF    ( SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-RECEDEN-FTNJANUM    
                                           =   SPACE
                   IF      SPA-GMN-RECEDEN-FTNJANUMKBN
                                               =   "02"
                       MOVE    "0021"              TO  SPA-ERRCD
                       MOVE    104                 TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-RECEDEN-FTNJANUMKBN  
                                           =   "02"
                       CONTINUE
                   ELSE
                       MOVE    "0021"          TO  SPA-ERRCD
                       MOVE    104             TO  SPA-GMN-CUR
                   END-IF
               END-IF 
           END-IF
      *
      *    レセ電：受給者番号記載区分とレセ電：受給者番号のチェック
           IF    ( SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-RECEDEN-JKYSNUM     
                                           =   SPACE
                   IF      SPA-GMN-RECEDEN-JKYSNUMKBN
                                               =   "02"
                       MOVE    "0023"              TO  SPA-ERRCD
                       MOVE    106                 TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-RECEDEN-JKYSNUMKBN
                                               =   "02"
                       CONTINUE
                   ELSE
                       MOVE    "0023"              TO  SPA-ERRCD
                       MOVE    106                 TO  SPA-GMN-CUR
                   END-IF
               END-IF 
           END-IF
      *
      *    有効期間の開始日と終了日が同時に変更されていないこと
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-NAI-SELNUM      >   ZERO  )
               IF    ( SPA-NAI-STYUKYMD
                                       NOT =   SPA-NAI-TSTYUKYMD
                                                      (SPA-NAI-SELNUM) )
               AND
                     ( SPA-NAI-EDYUKYMD
                                       NOT =   SPA-NAI-TEDYUKYMD
                                                      (SPA-NAI-SELNUM) )
                   MOVE    "0108"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF      
      *
      *    同一条件で期間重複があるか
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF    ( SPA-GMN-THKNNUM       (IDX)
                                           =   SPA-GMN-HKNNUM         )
                   AND   ( SPA-NAI-TTEISYUTUSAKI (IDX)
                                           =   SPA-GMN-TEISYUTUSAKI   )
                   AND   ( SPA-GMN-THKNJANUM     (IDX)
                                           =   SPA-GMN-HKNJANUM       )
                   AND   ( SPA-NAI-TRJNKBN       (IDX)
                                           =   SPA-GMN-RJNKBN         )
                   AND   ( SPA-NAI-TZAITAKU      (IDX)
                                           =   SPA-GMN-ZAITAKU        )
                   AND   ( SPA-NAI-TFTNJANUMKBN-JYOKEN (IDX)
                                           =
                                           SPA-GMN-FTNJANUMKBN-JYOKEN )
                   AND   ( SPA-NAI-TSYOTOKUKBN   (IDX)
                                           =   SPA-GMN-SYOTOKUKBN     )
      *                入外区分は空白とのチェックも行う
                       IF    ( SPA-NAI-TNYUGAIKBN    (IDX)
                                           NOT =   SPACE             )
                       AND   ( SPA-GMN-NYUGAIKBN
                                           NOT =   SPACE             )
                       AND   ( SPA-NAI-TNYUGAIKBN    (IDX)
                                           NOT =   SPA-GMN-NYUGAIKBN )
                           CONTINUE
                       ELSE
                           IF      SPA-NAI-SELNUM  NOT =   IDX 
                               IF    ( SPA-NAI-EDYUKYMD
                                           <   SPA-NAI-TSTYUKYMD (IDX) )
                               OR    ( SPA-NAI-STYUKYMD
                                           >   SPA-NAI-TEDYUKYMD (IDX) )
                                   CONTINUE
                               ELSE
                                   MOVE    "0105"          TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF 
               END-PERFORM 
               IF      SPA-ERRCD           NOT =   SPACE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF      
      *
      *    有効期間と老人区分のチェック
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-NAI-STYUKYMD    <   WRK-CONS-SRYYM-200804 )
               AND   ( SPA-NAI-EDYUKYMD    >=  WRK-CONS-SRYYM-200804 )
               AND   ( SPA-GMN-RJNKBN  NOT =   SPACE                 )
                   MOVE    "XXXX"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    有効期間と提出先のチェック
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-NAI-STYUKYMD    <   WRK-CONS-SRYYM-200804 )
               AND   ( SPA-NAI-EDYUKYMD    >=  WRK-CONS-SRYYM-200804 )
               AND   ( SPA-GMN-TEISYUTUSAKI    =   "6"               )
                   MOVE    "YYYY"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-SELNUM
      *
           MOVE    ZERO                TO  FLG-ROW
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       420-CLEAR-EXT.
           EXIT.
      *
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-GID-SEC              SECTION.
      *
      *    対象データ未選択時チェック
           IF      SPA-NAI-SELNUM      =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
      *    有効期間変時チェック
           IF      SPA-ERRCD           =   SPACE 
               IF    ( SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                       NOT =   SPA-NAI-STYUKYMD )
               OR    ( SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                       NOT =   SPA-NAI-EDYUKYMD )      
                   MOVE    "0104"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE 
               MOVE    "2001"              TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
      *    削除処理
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2006"              TO  SYS-KANRICD
           MOVE    SPA-NAI-KBNCD       TO  SYS-KBNCD
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 420-CLEAR-SEC
      *
      *        一覧再編集処理
               PERFORM 3101-SPA-HENSYU-SEC
               IF      W61-ROW             =   ZERO
                   MOVE    SPA-GMN-MAX         TO  W61-ROW
               END-IF
               MOVE    ZERO                TO  FLG-ROW
      *
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       421-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前処理
      *****************************************************************
       420-TOUROKU-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2002"          TO  SPA-WIDCD
           ELSE
               IF      SPA-ERRCD       =    "XXXX"
                   MOVE    "2004"           TO  SPA-WIDCD
                   MOVE    SPACE            TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD       =    "YYYY"
                   MOVE    "2005"           TO  SPA-WIDCD
                   MOVE    SPACE            TO  SPA-ERRCD
               END-IF
           END-IF
           
      *
           .
       420-TOUROKU-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       421-TOUROKU-SEC                 SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-NAI-SELNUM       >   ZERO
               MOVE    SPACE               TO  SYS-2006-REC
               INITIALIZE                      SYS-2006-REC
               MOVE    "2006"              TO  SYS-2006-KANRICD
               MOVE    SPA-NAI-KBNCD       TO  SYS-2006-KBNCD
               MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                           TO  SYS-2006-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                           TO  SYS-2006-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-2006-HOSPNUM
               MOVE    SYS-2006-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2006-REC
               ELSE
                   MOVE    "0101"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-2006-REC
               INITIALIZE                      SYS-2006-REC
               MOVE    "2006"              TO  SYS-2006-KANRICD
               MOVE    SPA-HOSPNUM         TO  SYS-2006-HOSPNUM
               MOVE    SYS-2006-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2006-REC
                   MOVE    SYS-2006-KBNCD(1:3) TO  WRK-2006-KBNCD
               ELSE
                   MOVE    ZERO                TO  WRK-2006-KBNCD
               END-IF
               ADD     1                   TO  WRK-2006-KBNCD-9
           END-IF
      *
      *   登録処理
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SELNUM       =   ZERO
                   MOVE    SPACE               TO  SYS-2006-REC
                   INITIALIZE                      SYS-2006-REC
                   MOVE    "2006"              TO  SYS-2006-KANRICD
                   MOVE    WRK-2006-KBNCD      TO  SYS-2006-KBNCD
                   MOVE    SPA-NAI-STYUKYMD    TO  SYS-2006-STYUKYMD
                   IF      SPA-NAI-EDYUKYMD    =   ZERO
                       MOVE    99999999            TO  SPA-NAI-EDYUKYMD
                   END-IF 
                   MOVE    SPA-NAI-EDYUKYMD    TO  SYS-2006-EDYUKYMD
               ELSE
                   MOVE    SPA-NAI-STYUKYMD    TO  SYS-2006-STYUKYMD
                   MOVE    SPA-NAI-EDYUKYMD    TO  SYS-2006-EDYUKYMD
               END-IF 
      *
               MOVE    SPA-GMN-HKNNUM      TO  SYS-2006-HKNNUM
               MOVE    SPA-GMN-TEISYUTUSAKI
                                           TO  SYS-2006-TEISYUTUSAKI
               MOVE    SPA-GMN-HKNJANUM    TO  SYS-2006-HKNJANUM
               MOVE    SPA-GMN-FTNJANUMKBN-JYOKEN
                                           TO
                                           SYS-2006-FTNJANUMKBN-JYOKEN
               MOVE    SPA-GMN-FTNJANUM-JYOKEN
                                           TO  SYS-2006-FTNJANUM-JYOKEN
               MOVE    SPA-GMN-NYUGAIKBN   TO  SYS-2006-NYUGAIKBN
               MOVE    SPA-GMN-SYOTOKUKBN  TO  SYS-2006-SYOTOKUKBN
               MOVE    SPA-GMN-RJNKBN      TO  SYS-2006-RJNKBN 
               MOVE    SPA-GMN-ZAITAKU     TO  SYS-2006-ZAITAKU
               MOVE    SPA-GMN-TOKKI       TO  SYS-2006-TOKKI
               MOVE    SPA-GMN-KYURATE     TO  SYS-2006-KYURATE 
               MOVE    SPA-GMN-FTNJANUMKBN TO  SYS-2006-FTNJANUMKBN
               MOVE    SPA-GMN-FTNJANUM    TO  SYS-2006-FTNJANUM
               MOVE    SPA-GMN-JKYSNUMKBN  TO  SYS-2006-JKYSNUMKBN
               MOVE    SPA-GMN-JKYSNUM     TO  SYS-2006-JKYSNUM
               MOVE    SPA-GMN-KYURATE     TO  SYS-2006-KYURATE 
               MOVE    SPA-GMN-RECEDEN-TOKKI
                                           TO  SYS-2006-RECEDEN-TOKKI
               MOVE    SPA-GMN-RECEDEN-KYURATE
                                           TO  SYS-2006-RECEDEN-KYURATE
               MOVE    SPA-GMN-RECEDEN-KORECKBN
                                           TO  SYS-2006-RECEDEN-KORECKBN
               MOVE    SPA-GMN-RECEDEN-FTNJANUMKBN
                                           TO
                                           SYS-2006-RECEDEN-FTNJANUMKBN
               MOVE    SPA-GMN-RECEDEN-FTNJANUM
                                           TO  SYS-2006-RECEDEN-FTNJANUM
               MOVE    SPA-GMN-RECEDEN-JKYSNUMKBN
                                           TO
                                           SYS-2006-RECEDEN-JKYSNUMKBN
               MOVE    SPA-GMN-RECEDEN-JKYSNUM
                                           TO  SYS-2006-RECEDEN-JKYSNUM
               MOVE    SPA-GMN-TEKIYOU-COMMENT1
                                           TO  SYS-2006-TEKIYOU-COMMENT1
               MOVE    SPA-GMN-TEKIYOU-COMMENT1KBN
                                           TO
                                           SYS-2006-TEKIYOU-COMMENT1KBN
               MOVE    SPA-GMN-TEKIYOU-COMMENT2
                                           TO  SYS-2006-TEKIYOU-COMMENT2
               MOVE    SPA-GMN-TEKIYOU-COMMENT2KBN
                                           TO
                                           SYS-2006-TEKIYOU-COMMENT2KBN
      *
               MOVE    SYS-2006-REC        TO  SYSKANRI-REC
               IF      SPA-NAI-SELNUM      =   ZERO
                   MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
               ELSE
                   MOVE    "2006"              TO  SYSUP-KANRICD
                   MOVE    SPA-NAI-KBNCD       TO  SYSUP-KBNCD
                   MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
               END-IF 
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   IF      SPA-NAI-SELNUM        =   ZERO
                       MOVE    "1001"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "1002"              TO  SPA-ERRCD
                   END-IF
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 420-CLEAR-SEC
      *
      *        一覧再編集処理
               PERFORM 3101-SPA-HENSYU-SEC
               IF      W61-ROW             =   ZERO
                   MOVE    SPA-GMN-MAX         TO  W61-ROW
               END-IF
               MOVE    ZERO                TO  FLG-ROW
      *
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       421-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W61    "           TO  MCP-WINDOW
      *
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
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-W61-ROW
           ELSE
               MOVE    W61-ROW             TO  WRK-W61-ROW
           END-IF
      *
           INITIALIZE                      W61
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  W61-TRENNUM    (IDX)
               MOVE    SPA-GMN-THKNNUM   (IDX) TO  W61-THKNNUM    (IDX)
               MOVE    SPA-GMN-TSEIDONAME(IDX) TO  W61-TSEIDONAME (IDX)
               MOVE    SPA-GMN-TTEISYUTUSAKI (IDX)
                                               TO
                                               W61-TTEISYUTUSAKI  (IDX)
               MOVE    SPA-GMN-THKNJANUM (IDX) TO  W61-THKNJANUM  (IDX)
               MOVE    SPA-GMN-TFTNJANUMKBN-JYOKEN (IDX)
                                               TO
                                           W61-TFTNJANUMKBN-JYOKEN(IDX)
               MOVE    SPA-GMN-TNYUGAIKBN (IDX)
                                               TO W61-TNYUGAIKBN  (IDX)
               MOVE    SPA-GMN-TSYOTOKUKBN (IDX)
                                               TO  W61-TSYOTOKUKBN(IDX)
               MOVE    SPA-GMN-TRJNKBN   (IDX) TO  W61-TRJNKBN    (IDX)
               MOVE    SPA-GMN-TZAITAKU  (IDX) TO  W61-TZAITAKU   (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX) TO  W61-TSTYUKYMD  (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX) TO  W61-TEDYUKYMD  (IDX)
      *         
               IF      IDX                 =   SPA-NAI-SELNUM
                   MOVE    "T"                 TO  W61-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  W61-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  W61-COUNT
           MOVE    ZERO                TO  W61-ROW
           IF      WRK-W61-ROW         >   ZERO
               MOVE    WRK-W61-ROW         TO  W61-ROW
           END-IF
           IF      SPA-NAI-SELNUM      >   ZERO
               MOVE    2                   TO  W61-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  W61-SELNUM
           MOVE    SPA-GMN-STYUKYMD    TO  W61-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W61-EDYUKYMD
           MOVE    SPA-GMN-HKNNUM      TO  W61-HKNNUM
           MOVE    SPA-GMN-SEIDONAME   TO  W61-SEIDONAME 
           MOVE    SPA-GMN-TEISYUTUSAKI-G
                                       TO  W61-TEISYUTUSAKI
           MOVE    SPA-GMN-HKNJANUM    TO  W61-HKNJANUM 
           MOVE    SPA-GMN-FTNJANUMKBN-JYOKEN-G
                                       TO  W61-FTNJANUMKBN-JYOKEN
           MOVE    SPA-GMN-FTNJANUM-JYOKEN
                                       TO  W61-FTNJANUM-JYOKEN
           MOVE    SPA-GMN-HKNJANAME   TO  W61-HKNJANAME
           MOVE    SPA-GMN-NYUGAIKBN-G TO  W61-NYUGAIKBN
           MOVE    SPA-GMN-SYOTOKUKBN-G
                                       TO  W61-SYOTOKUKBN
           MOVE    SPA-GMN-RJNKBN-G    TO  W61-RJNKBN 
           MOVE    SPA-GMN-ZAITAKU-G   TO  W61-ZAITAKU
           MOVE    SPA-GMN-TOKKI       TO  W61-TOKKI
           MOVE    SPA-GMN-KYURATE     TO  W61-KYURATE
           MOVE    SPA-GMN-FTNJANUMKBN-G
                                       TO  W61-FTNJANUMKBN
           MOVE    SPA-GMN-FTNJANUM    TO  W61-FTNJANUM
           MOVE    SPA-GMN-JKYSNUMKBN-G
                                       TO  W61-JKYSNUMKBN
           MOVE    SPA-GMN-JKYSNUM     TO  W61-JKYSNUM
           MOVE    SPA-GMN-RECEDEN-TOKKI
                                       TO  W61-RECEDEN-TOKKI
           MOVE    SPA-GMN-RECEDEN-KYURATE
                                       TO  W61-RECEDEN-KYURATE
           MOVE    SPA-GMN-RECEDEN-KORECKBN-G
                                       TO  W61-RECEDEN-KORECKBN
           MOVE    SPA-GMN-RECEDEN-FTNJANUMKBN-G
                                       TO  W61-RECEDEN-FTNJANUMKBN
           MOVE    SPA-GMN-RECEDEN-FTNJANUM
                                       TO  W61-RECEDEN-FTNJANUM
           MOVE    SPA-GMN-RECEDEN-JKYSNUMKBN-G
                                       TO  W61-RECEDEN-JKYSNUMKBN
           MOVE    SPA-GMN-RECEDEN-JKYSNUM
                                       TO  W61-RECEDEN-JKYSNUM
           MOVE    SPA-GMN-TEKIYOU-COMMENT1KBN-G
                                       TO  W61-TEKIYOU-COMMENT1KBN
           MOVE    SPA-GMN-TEKIYOU-COMMENT1
                                       TO  W61-TEKIYOU-COMMENT1
           MOVE    SPA-GMN-TEKIYOU-COMMENT2KBN-G
                                       TO  W61-TEKIYOU-COMMENT2KBN
           MOVE    SPA-GMN-TEKIYOU-COMMENT2
                                       TO  W61-TEKIYOU-COMMENT2
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-TEISYUTUSAKI-MAX
                MOVE   SPA-GMN-TEISYUTUSAKILST (IDX)
                                       TO  W61-TEISYUTUSAKI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                   TO  W61-TEISYUTUSAKI-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
                MOVE   SPA-GMN-FTNJANUMKBN-JYOKENLST (IDX)
                                       TO
                                       W61-FTNJANUMKBN-JYOKEN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
                                   TO  W61-FTNJANUMKBN-JYOKEN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-NYUGAIKBN-MAX
                MOVE   SPA-GMN-NYUGAIKBNLST (IDX)
                                       TO  W61-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                   TO  W61-NYUGAIKBN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-SYOTOKUKBN-MAX
                MOVE   SPA-GMN-SYOTOKUKBNLST (IDX)
                                       TO  W61-SYOTOKUKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYOTOKUKBN-MAX
                                   TO  W61-SYOTOKUKBN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-RJNKBN-MAX
                MOVE   SPA-GMN-RJNKBNLST (IDX)
                                       TO  W61-RJNKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RJNKBN-MAX
                                   TO  W61-RJNKBN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-ZAITAKU-MAX
                MOVE   SPA-GMN-ZAITAKULST (IDX)
                                       TO  W61-ZAITAKU-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ZAITAKU-MAX
                                   TO  W61-ZAITAKU-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-FTNJANUMKBN-MAX
                MOVE   SPA-GMN-FTNJANUMKBNLST (IDX)
                                       TO
                                       W61-FTNJANUMKBN-ITEM        (IDX)
                                       W61-RECEDEN-FTNJANUMKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-FTNJANUMKBN-MAX
                                   TO  W61-FTNJANUMKBN-COUNT
                                       W61-RECEDEN-FTNJANUMKBN-COUNT 
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-JKYSNUMKBN-MAX
                MOVE   SPA-GMN-JKYSNUMKBNLST (IDX)
                                       TO
                                       W61-JKYSNUMKBN-ITEM        (IDX)
                                       W61-RECEDEN-JKYSNUMKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-JKYSNUMKBN-MAX
                                   TO  W61-JKYSNUMKBN-COUNT
                                       W61-RECEDEN-JKYSNUMKBN-COUNT 
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-RECEDEN-KORECKBN-MAX
                MOVE   SPA-GMN-RECEDEN-KORECKBNLST (IDX)
                                   TO  W61-RECEDEN-KORECKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-KORECKBN-MAX
                                   TO  W61-RECEDEN-KORECKBN-COUNT
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
                MOVE   SPA-GMN-TEKIYOU-COMMENT1KBNLST (IDX)
                                       TO
                                       W61-TEKIYOU-COMMENT1KBN-ITEM(IDX)
                                       W61-TEKIYOU-COMMENT2KBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
                                   TO  W61-TEKIYOU-COMMENT1KBN-COUNT
                                       W61-TEKIYOU-COMMENT2KBN-COUNT 
      *
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                    MOVE  "SELNUM"         TO  MCP-WIDGET   
             WHEN    2
                    MOVE  "STYUKYMD"       TO  MCP-WIDGET   
             WHEN    3
                    MOVE  "EDYUKYMD"       TO  MCP-WIDGET   
             WHEN    4
                    MOVE  "HKNNUM"         TO  MCP-WIDGET   
             WHEN    5
                    MOVE  "TEISYUTUSAKI"   TO  MCP-WIDGET   
             WHEN    6
                    MOVE  "HKNJANUM"       TO  MCP-WIDGET   
             WHEN    15
                    MOVE  "FTNJANUMKBN_JYOKEN"
                                           TO  MCP-WIDGET   
             WHEN    16
                    MOVE  "FTNJANUM_JYOKEN"
                                           TO  MCP-WIDGET   
             WHEN    21
                    MOVE  "NYUGAIKBN"      TO  MCP-WIDGET   
             WHEN    22
                    MOVE  "SYOTOKUKBN"     TO  MCP-WIDGET   
             WHEN    7
                    MOVE  "RJNKBN"         TO  MCP-WIDGET   
             WHEN    8
                    MOVE  "ZAITAKU"        TO  MCP-WIDGET   
             WHEN    9
                    MOVE  "TOKKI"          TO  MCP-WIDGET   
             WHEN    10
                    MOVE  "KYURATE"        TO  MCP-WIDGET   
             WHEN    11
                    MOVE  "FTNJANUMKBN"    TO  MCP-WIDGET   
             WHEN    12
                    MOVE  "FTNJANUM"       TO  MCP-WIDGET   
             WHEN    13
                    MOVE  "JKYSNUMKBN"     TO  MCP-WIDGET   
             WHEN    14
                    MOVE  "JKYSNUM"        TO  MCP-WIDGET   
             WHEN    17
                    MOVE  "TEKIYOU_COMMENT1"
                                           TO  MCP-WIDGET   
             WHEN    18
                    MOVE  "TEKIYOU_COMMENT1KBN" 
                                           TO  MCP-WIDGET   
             WHEN    19
                    MOVE  "TEKIYOU_COMMENT2"
                                           TO  MCP-WIDGET   
             WHEN    20
                    MOVE  "TEKIYOU_COMMENT2KBN" 
                                           TO  MCP-WIDGET   
      *
             WHEN    100
                    MOVE  "RECEDEN_TOKKI"  TO  MCP-WIDGET   
             WHEN    101
                    MOVE  "RECEDEN_KYURATE"
                                           TO  MCP-WIDGET   
             WHEN    102
                    MOVE  "RECEDEN_KORECKBN"
                                           TO  MCP-WIDGET   
             WHEN    103
                    MOVE  "RECEDEN_FTNJANUMKBN"
                                           TO  MCP-WIDGET   
             WHEN    104
                    MOVE  "RECEDEN_FTNJANUM"
                                           TO  MCP-WIDGET   
             WHEN    105
                    MOVE  "RECEDEN_JKYSNUMKBN"
                                           TO  MCP-WIDGET   
             WHEN    106
                    MOVE  "RECEDEN_JKYSNUM"
                                           TO  MCP-WIDGET   
      *
             WHEN    999
                    MOVE  "B12"            TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "HKNNUM"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "TEISYUTUSAKI"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "HKNJANUM"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "FTNJANUMKBN_JYOKEN"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "FTNJANUM_JYOKEN"
                   MOVE    21           TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "SYOTOKUKBN"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "RJNKBN"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "ZAITAKU"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "TOKKI"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "KYURATE"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "FTNJANUMKBN"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "FTNJANUM"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "JKYSNUMKBN"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "JKYSNUM"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "TEKIYOU_COMMENT1"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "TEKIYOU_COMMENT1KBN"
                   MOVE    19          TO  SPA-GMN-CUR
               WHEN    "TEKIYOU_COMMENT2"
                   MOVE    20          TO  SPA-GMN-CUR
               WHEN    "TEKIYOU_COMMENT2KBN"
                   MOVE    100         TO  SPA-GMN-CUR
      *
               WHEN    "RECEDEN_TOKKI"
                   MOVE    101         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_KYURATE"
                   MOVE    102         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_KORECKBN"
                   MOVE    103         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_FTNJANUMKBN"
                   MOVE    104         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_FTNJANUM"
                   MOVE    105         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_JKYSNUMKBN"
                   MOVE    106         TO  SPA-GMN-CUR
               WHEN    "RECEDEN_JKYSNUM"
                   MOVE    999         TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効期間開始日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効期間終了日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "保険番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "提出先入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "保険者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "老人区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "在総診入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "特記事項は全角で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "給付割合入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "レセプト電算・特記事項入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "レセプト電算・給付割合入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "レセプト電算・公費レコード区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE
                       "保険番号が未入力のときは入力できない項目です"
                                           TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "入力できない種類の保険番号です"
                                           TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "負担者番号記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "負担者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "受給者番号記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "受給者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "レセプト電算・負担者番号記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "レセプト電算・負担者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "レセプト電算・受給者番号記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "レセプト電算・受給者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "負担者番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "負担者番号固定値入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "適用欄上段コメントは全角で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "適用欄上段コメント記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "適用欄下段コメントは全角で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "適用欄下段コメント記載区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "適用欄上段コメント入力時のみ選択できます"
                                           TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "適用欄上段コメント入力時のみ選択できます"
                                           TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "全角空白のみ入力されています"
                                           TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "入外区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "所得区分入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "入力した保険番号には設定できません"
                                           TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE   "レセプト請求しない保険番号には設定できません"
                                           TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE   "所得区分の設定には提出先の設定が必要です"
                                           TO  SPA-ERRMSG
      *
               WHEN    "0101"
                   MOVE    "選択されたデータの読み込みができません"
                                           TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE
                       "有効期間開始日＞有効期間終了日となっています"
                                           TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "編集項目が未入力です"
                                           TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "有効期間の変更が行われています"
                                           TO  SPA-ERRMSG
               WHEN    "0105"
                   MOVE
                      "条件が同じで有効期間の重複するデータが存在します"
                                           TO  SPA-ERRMSG
               WHEN    "0106"
                   MOVE    "有効期間開始日は１日で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0107"
                   MOVE    "有効期間終了日は月末日で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0108"
                   MOVE
                       "有効期間の開始日と終了日は同時に変更できません"
                                           TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "追加できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "更新できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W61"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
      *
           EVALUATE    SPA-WIDCD
               WHEN    "2001"
                   MOVE
                       "選択されたデータを削除します。よろしいですか"
                                                TO  WID1-ID1MSG
               WHEN    "2002"
                   MOVE    "データを登録します。よろしいですか"
                                                TO  WID1-ID1MSG
               WHEN    "2003"
                   STRING  "平成２０年４月以降に老人区分の設"
                                                DELIMITED  BY  SIZE
                           "定があります。処理を終了してよろしいですか"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
               WHEN    "2004"
                   STRING  "平成２０年４月以降に老人区分の設定があ"
                                                DELIMITED  BY  SIZE
                          "ります。データを登録してよろしいですか"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
               WHEN    "2005"
                   STRING  "平成２０年４月以前に広域連合の設定"
                                                DELIMITED  BY  SIZE
                          "があります。データを登録してよろしいですか"
                                                DELIMITED  BY  SIZE
                                                INTO  WID1-ID1MSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W61"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    提出先編集処理
      *****************************************************************
       5001-TEISYUTUSAKI-HENSYU-SEC   SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-TEISYUTUSAKI-MAX
                   IF      WRK-KBN =   SPA-GMN-TEISYUTUSAKIL (IDY)
                       MOVE    SPA-GMN-TEISYUTUSAKIMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-TEISYUTUSAKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号編集処理
      *****************************************************************
       5001-FTNJANUMKB-JYOKEN-HEN-SEC  SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
                   IF      WRK-KBN =   SPA-GMN-FTNJANUMKBN-JYOKENL (IDY)
                       MOVE    SPA-GMN-FTNJANUMKBN-JYOKENMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-FTNJANUMKBN-JYOKEN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-FTNJANUMKB-JYOKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC   SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-NYUGAIKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-NYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-NYUGAIKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-NYUGAIKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-NYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    所得区分編集処理
      *****************************************************************
       5001-SYOTOKUKBN-HENSYU-SEC  SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-SYOTOKUKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-SYOTOKUKBNL (IDY)
                       MOVE    SPA-GMN-SYOTOKUKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-SYOTOKUKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SYOTOKUKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人区分編集処理
      *****************************************************************
       5001-RJNKBN-HENSYU-SEC    SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-RJNKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-RJNKBNL (IDY)
                       MOVE    SPA-GMN-RJNKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-RJNKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-RJNKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診区分編集処理
      *****************************************************************
       5001-ZAITAKU-HENSYU-SEC    SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-ZAITAKU-MAX
                   IF      WRK-KBN =   SPA-GMN-ZAITAKUL (IDY)
                       MOVE    SPA-GMN-ZAITAKUMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-ZAITAKU-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-ZAITAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電：公費レコード区分編集処理
      *****************************************************************
       5001-RECEDEN-KORECKB-HENSYU-SEC    SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-RECEDEN-KORECKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-RECEDEN-KORECKBNL (IDY)
                       MOVE    SPA-GMN-RECEDEN-KORECKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-RECEDEN-KORECKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-RECEDEN-KORECKB-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号記載区分編集処理
      *****************************************************************
       5001-FTNJANUMKB-HENSYU-SEC   SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-FTNJANUMKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-FTNJANUMKBNL (IDY)
                       MOVE    SPA-GMN-FTNJANUMKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-FTNJANUMKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-FTNJANUMKB-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号記載区分編集処理
      *****************************************************************
       5001-JKYSNUMKBN-HENSYU-SEC    SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-JKYSNUMKBN-MAX
                   IF      WRK-KBN =   SPA-GMN-JKYSNUMKBNL (IDY)
                       MOVE    SPA-GMN-JKYSNUMKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-JKYSNUMKBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-JKYSNUMKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄コメントレセ電編集区分編集処理
      *****************************************************************
       5001-TEKIYOU-COM1KBN-HENSYU-SEC    SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >   SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
                   IF      WRK-KBN =   SPA-GMN-TEKIYOU-COMMENT1KBNL(IDY)
                       MOVE    SPA-GMN-TEKIYOU-COMMENT1KBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                       MOVE    SPA-GMN-TEKIYOU-COMMENT1KBN-MAX
                                              TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-TEKIYOU-COM1KBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
           MOVE    ZERO            TO  WRK-SYMD
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD             TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
                   MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               END-IF
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC 
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           PERFORM     UNTIL (     FLG-HKNNUM    =  1                  )
                        OR   (    (HKN-TEKSTYMD  <= SPA-NAI-EDYUKYMD )
                              AND (HKN-TEKEDYMD  >= SPA-NAI-STYUKYMD ) )
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-N-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNNUM
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-DB-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DB-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

