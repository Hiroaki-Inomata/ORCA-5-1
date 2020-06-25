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
       PROGRAM-ID.             ORCGW09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : レセプト作成指示（印刷順）情報（Ｗ０９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-藤原     01/09/11  削除処理の修正 
      * 01.00.02     MCC-藤原     01/09/21  JOINの修正     
      * 01.00.03     MCC-藤原     02/07/03  提出先の追加     
      * 01.00.04     NACL-藤原    02/09/20  自賠責の提出先の追加     
      *
      * 01.01.01     NACL-藤原    02/09/25  入外区分・表示順の追加     
      * 01.01.06     NACL-多々納  02/11/13  一括入力 
      * 03.05.00     NACL-多々納  07/05/02  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/26  平成２０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
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
       01  SPA-W09.
           03  SPA-W09-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-PRTJYUNI       PIC X(03).
                   07  SPA-GMN-TEISYUTUSAKILST    OCCURS  10.
                       09  SPA-GMN-TEISYUTUSAKIL  PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-TEISYUTUSAKIMEIL
                                                  PIC X(24).
                   07  SPA-GMN-TEISYUTUSAKI-MAX   PIC 9(02).    
                   07  SPA-GMN-NYUGAIKBNLST       OCCURS  2.
                       09  SPA-GMN-NYUGAIKBNL     PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-NYUGAIKBNMEIL  PIC X(06).
                   07  SPA-GMN-NYUGAIKBN-MAX      PIC 9(02).    
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SYURUINAME
                                              PIC X(40).
                       09  SPA-GMN-SYUBETU-TBL    OCCURS  10.
                           11  SPA-GMN-SYUBETU    PIC X(04).
                           11  SPA-GMN-SYUBETUNAME
                                                  PIC X(40).
                       09  SPA-GMN-TEISYUTUSAKI-G.
                           11  SPA-GMN-TEISYUTUSAKI   PIC X(01).
                           11  SPA-GMN-F1             PIC X(01).
                           11  SPA-GMN-TEISYUTUSAKIMEI
                                                      PIC X(24).
                       09  SPA-GMN-NYUGAIKBN-G.
                           11  SPA-GMN-NYUGAIKBN      PIC X(01).
                           11  SPA-GMN-F1             PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEI   PIC X(06).
                       09  SPA-GMN-DSPSEQ         PIC 9(03).        
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *   
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(07).
               05  WRK-WIDGET2     PIC X(02). 
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    レセプト作成指示（印刷順）情報
           COPY  "CPSK2003.INC".
      *    レセプト作成指示（レセプト種別）情報
           COPY  "CPSK2004.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W09
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W09         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W09"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               IF      SPA-WIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W09-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-W01-KBNCD       TO  SPA-GMN-PRTJYUNI
      *
           MOVE    "1"                 TO  SPA-GMN-TEISYUTUSAKIL   (1) 
           MOVE    "社会保険診療報酬支払基金"
                                       TO  SPA-GMN-TEISYUTUSAKIMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-TEISYUTUSAKIL   (2)
           MOVE    "国民健康保険団体連合会"
                                       TO  SPA-GMN-TEISYUTUSAKIMEIL(2)
           MOVE    "3"                 TO  SPA-GMN-TEISYUTUSAKIL   (3)
           MOVE    "労働基準監督署"    TO  SPA-GMN-TEISYUTUSAKIMEIL(3)
           MOVE    "4"                 TO  SPA-GMN-TEISYUTUSAKIL   (4)
           MOVE    "保険会社"          TO  SPA-GMN-TEISYUTUSAKIMEIL(4)
           MOVE    "5"                 TO  SPA-GMN-TEISYUTUSAKIL   (5)
           MOVE    "公害"              TO  SPA-GMN-TEISYUTUSAKIMEIL(5)
           MOVE    "6"                 TO  SPA-GMN-TEISYUTUSAKIL   (6)
           MOVE    "広域連合"          TO  SPA-GMN-TEISYUTUSAKIMEIL(6)
           MOVE    6                   TO  SPA-GMN-TEISYUTUSAKI-MAX 
      *
           MOVE    "1"                 TO  SPA-GMN-NYUGAIKBNL   (1) 
           MOVE    "入院"              TO  SPA-GMN-NYUGAIKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-NYUGAIKBNL   (2)
           MOVE    "入院外"            TO  SPA-GMN-NYUGAIKBNMEIL(2)
           MOVE    2                   TO  SPA-GMN-NYUGAIKBN-MAX
      *
           INITIALIZE                          SPA-GMN-KOUMOKU
      * 
           MOVE    SPACE                   TO  SYS-2003-REC
           INITIALIZE                          SYS-2003-REC
           MOVE    "2003"                  TO  SYS-2003-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-2003-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2003-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2003-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC            TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2003-REC
               MOVE    SYS-2003-SYURUINAME     TO  SPA-GMN-SYURUINAME
               MOVE    SYS-2003-TEISYUTUSAKI   TO  SPA-GMN-TEISYUTUSAKI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-TEISYUTUSAKI-MAX
                   IF      SPA-GMN-TEISYUTUSAKI    =
                                           SPA-GMN-TEISYUTUSAKIL (IDX)
                       MOVE    SPA-GMN-TEISYUTUSAKILST (IDX)
                                           TO  SPA-GMN-TEISYUTUSAKI-G
                       MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                   MOVE    SYS-2003-SYUBETU (IDX)
                                           TO  SPA-GMN-SYUBETU (IDX)
               END-PERFORM
               MOVE    SYS-2003-NYUGAIKBN  TO  SPA-GMN-NYUGAIKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
                   IF      SPA-GMN-NYUGAIKBN    =
                                           SPA-GMN-NYUGAIKBNL (IDX)
                       MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                       MOVE    SPA-GMN-NYUGAIKBN-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
               IF      SYS-2003-DSPSEQ (1:3)   NOT NUMERIC
                   MOVE    ZERO                TO  SYS-2003-DSPSEQ
               END-IF
               MOVE    SYS-2003-DSPSEQ     TO  SPA-GMN-DSPSEQ     
           END-IF     
      *         
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      SPA-GMN-SYUBETU (IDX)   =   SPACE   
               PERFORM 3101-SYS-2004-READ-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SYS-2004-SYUBETUNAME
                                       TO  SPA-GMN-SYUBETUNAME(IDX)
               END-IF
           END-PERFORM    
      *
           MOVE    2                   TO  SPA-GMN-CUR
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト名称読み込み処理
      *****************************************************************
       3101-SYS-2004-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-2004-REC
           INITIALIZE                      SYS-2004-REC
           MOVE    "2004"                  TO  SYS-2004-KANRICD
           MOVE    SPA-GMN-SYUBETU (IDX)   TO  SYS-2004-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2004-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2004-HOSPNUM
           MOVE    SYS-2004-REC            TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2004-REC
           END-IF 
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
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
           MOVE    W09-SYURUINAME      TO  SPA-GMN-SYURUINAME
           MOVE    W09-TEISYUTUSAKI    TO  SPA-GMN-TEISYUTUSAKI-G
      *    レセプト種別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    W09-SYUBETU (IDX)   TO  SPA-GMN-SYUBETU (IDX)
           END-PERFORM
      *
           MOVE    W09-NYUGAIKBN        TO  SPA-GMN-NYUGAIKBN-G
           MOVE    W09-DSPSEQ           TO  SPA-GMN-DSPSEQ
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
           PERFORM 4100-SYURUINAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4100-TEISYUTUSAKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SYUBETU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4102-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4103-DSPSEQ-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    種類名称チェック
      *****************************************************************
       4100-SYURUINAME-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-SYURUINAME  NOT =   SPACE
      *        全角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-SYURUINAME  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-SYURUINAME
               END-IF
           END-IF
      *****MOVE    13                   TO  SPA-GMN-CUR
           .
       4100-SYURUINAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    提出先チェック
      *****************************************************************
       4100-TEISYUTUSAKI-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-TEISYUTUSAKI-MAX
               IF      SPA-GMN-TEISYUTUSAKI    =
                                       SPA-GMN-TEISYUTUSAKIL (IDX)
                   MOVE    SPA-GMN-TEISYUTUSAKILST (IDX)
                                       TO  SPA-GMN-TEISYUTUSAKI-G
                   MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                       TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-TEISYUTUSAKIMEI =   SPACE
               MOVE    13                  TO  SPA-GMN-CUR
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF
           .
       4100-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別チェック
      *****************************************************************
       4101-SYUBETU-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >       10  )  OR
                          (SPA-ERRCD   NOT =   SPACE)
      *
           MOVE    SPACE               TO  SPA-GMN-SYUBETUNAME (IDX)
           IF      SPA-GMN-SYUBETU (IDX)   =   SPACE
               CONTINUE
           ELSE 
               PERFORM 3101-SYS-2004-READ-SEC
               IF      FLG-SYSKANRI            =   ZERO
                   MOVE    SYS-2004-SYUBETUNAME
                                       TO  SPA-GMN-SYUBETUNAME(IDX)
                   IF      SYS-2004-PRTJYUNI
                                       NOT =   SPACE
                   AND     SYS-2004-PRTJYUNI
                                       NOT =   SPA-W01-KBNCD (1:2)
                       MOVE    "0002"                  TO  SPA-ERRCD
                   END-IF
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       10
                               OR (SPA-ERRCD   NOT =   SPACE)
                       IF      SPA-GMN-SYUBETU (IDY)   =   SPACE
                       OR      IDX                     =   IDY
                           CONTINUE
                       ELSE
                           IF      SPA-GMN-SYUBETU (IDX)
                                            =   SPA-GMN-SYUBETU (IDY)
                               MOVE    "0003"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
               END-IF
      *
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (IDX                 =   1     )  AND
                      (SPA-GMN-SYURUINAME  =   SPACE )
                   MOVE    SPA-GMN-SYUBETUNAME (IDX)
                                           TO  SPA-GMN-SYURUINAME
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-CUR =   IDX     +   2
               END-IF
           END-IF
      *
           END-PERFORM
      *
           .
       4101-SYUBETU-CHK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4102-NYUGAIKBN-CHK-SEC       SECTION.
      *
      *
           IF      SPA-GMN-NYUGAIKBN   NOT =   SPACE 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
                   IF      SPA-GMN-NYUGAIKBN    =
                                       SPA-GMN-NYUGAIKBNL (IDX)
                       MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                       TO  SPA-GMN-NYUGAIKBN-G
                       MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  IDX
                   END-IF
               END-PERFORM     
      *
               IF      SPA-GMN-NYUGAIKBNMEI    =   SPACE
                   MOVE    "0005"              TO  SPA-ERRCD
                   MOVE    14                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4102-NYUGAIKBN-CHK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    表示順チェック
      *****************************************************************
       4103-DSPSEQ-CHK-SEC       SECTION.
      *
           .
       4103-DSPSEQ-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W09"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-W01KYOUTU
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-2003-REC
           INITIALIZE                      SYS-2003-REC
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    SPA-W01-KBNCD       TO  SYS-2003-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-2003-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-2003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-2003-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
              MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SYS-2003-REC
           INITIALIZE                      SYS-2003-REC
           MOVE    "2003"                  TO  SYS-2003-KANRICD
           MOVE    SPA-W01-KBNCD           TO  SYS-2003-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2003-STYUKYMD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-2003-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC            TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-2003-REC
           ELSE
               MOVE    SPACE                   TO  SYS-2003-REC
               INITIALIZE                      SYS-2003-REC
               MOVE   "2003"                   TO  SYS-2003-KANRICD
               MOVE    SPA-W01-KBNCD           TO  SYS-2003-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD      TO  SYS-2003-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD      TO  SYS-2003-EDYUKYMD
           MOVE    SPA-GMN-SYURUINAME    TO  SYS-2003-SYURUINAME
           MOVE    SPA-GMN-TEISYUTUSAKI  TO  SYS-2003-TEISYUTUSAKI
           MOVE    SPA-GMN-NYUGAIKBN     TO  SYS-2003-NYUGAIKBN
           MOVE    SPA-GMN-DSPSEQ        TO  SYS-2003-DSPSEQ
           INITIALIZE                        SYS-2003-SYUBETU-G
           MOVE    ZERO                  TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      SPA-GMN-SYUBETU(IDX)    NOT =   SPACE
                   ADD     1                       TO  IDY    
                   MOVE    SPA-GMN-SYUBETU(IDX)    TO
                                           SYS-2003-SYUBETU (IDY)
               END-IF
           END-PERFORM
           MOVE    SYS-2003-REC            TO  SYSKANRI-REC
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1001"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    MOVE    SPACE                TO  SPA-ERRCD
      *    MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR 
      *
      *    PERFORM 4100-TEISYUTUSAKI-CHK-SEC
      *    PERFORM 4102-NYUGAIKBN-CHK-SEC
      *    PERFORM 4100-SYURUINAME-CHK-SEC
      *    PERFORM VARYING IDX FROM    1   BY  1
      *            UNTIL   IDX >       10
      *        PERFORM 4101-SYUBETU-CHK-SEC
      *    END-PERFORM
      *
      *    IF      SPA-ERRCD            =   SPACE
      *        MOVE    WRK-GMN-CUR          TO  SPA-GMN-CUR
      **   END-IF             
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W09    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *
           MOVE    SPA-GMN-STYUKYMD        TO  W09-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD        TO  W09-YUKOEDYMD
           MOVE    SPA-GMN-TEISYUTUSAKI-G  TO  W09-TEISYUTUSAKI
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX >       SPA-GMN-TEISYUTUSAKI-MAX
               MOVE    SPA-GMN-TEISYUTUSAKILST (IDX)
                                       TO  W09-TEISYUTUSAKILST (IDX)
           END-PERFORM    
           MOVE    SPA-GMN-TEISYUTUSAKI-MAX TO  W09-TEISYUTUSAKI-COUNT
           MOVE    SPA-GMN-PRTJYUNI         TO  W09-PRTJYUNI
           MOVE    SPA-GMN-NYUGAIKBN-G      TO  W09-NYUGAIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                       TO  W09-NYUGAIKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  W09-NYUGAIKBN-COUNT
           MOVE    SPA-GMN-DSPSEQ      TO  W09-DSPSEQ     
      * 
           MOVE    SPA-GMN-SYURUINAME      TO  W09-SYURUINAME
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10 
               MOVE    SPA-GMN-SYUBETU     (IDX)
                                       TO  W09-SYUBETU     (IDX)
               MOVE    SPA-GMN-SYUBETUNAME (IDX)
                                       TO  W09-SYUBETUNAME (IDX)
           END-PERFORM     
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE      TO  W09-B03-STATE
      *
      **   IF      SPA-W01-SELNUM          >   ZERO    
      **       MOVE    WIDGET-NORMAL      TO  W09-B03-STATE
      **   END-IF 
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
           DISPLAY "SPA-ERRCD = " SPA-ERRCD
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "レセプト種別が登録してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "すでにこのレセプト種別は入力してあります"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "提出先区分がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "入外区分がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W09"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "レセプト作成指示（印刷順）情報を登録します"
                                        TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "レセプト作成指示（印刷順）情報を削除します"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W09"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    2
                    MOVE  "SYURUINAME"     TO   MCP-WIDGET
                WHEN    13
                    MOVE  "TEISYUTUSAKI"   TO   MCP-WIDGET
                WHEN    3
                    MOVE  "SYUBETU01"      TO   MCP-WIDGET
                WHEN    4
                    MOVE  "SYUBETU02"      TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SYUBETU03"      TO   MCP-WIDGET
                WHEN    6
                    MOVE  "SYUBETU04"      TO   MCP-WIDGET
                WHEN    7
                    MOVE  "SYUBETU05"      TO   MCP-WIDGET
                WHEN    8
                    MOVE  "SYUBETU06"      TO   MCP-WIDGET
                WHEN    9
                    MOVE  "SYUBETU07"      TO   MCP-WIDGET
                WHEN    10
                    MOVE  "SYUBETU08"      TO   MCP-WIDGET
                WHEN    11
                    MOVE  "SYUBETU09"      TO   MCP-WIDGET
                WHEN    12
                    MOVE  "SYUBETU10"      TO   MCP-WIDGET
                WHEN    14
                    MOVE  "NYUGAIKBN"      TO   MCP-WIDGET
                WHEN    15
                    MOVE  "DSPSEQ"         TO   MCP-WIDGET
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SYURUINAME"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "TEISYUTUSAKI"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "DSPSEQ"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    "SYUBETU01"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "SYUBETU02"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "SYUBETU03"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "SYUBETU04"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "SYUBETU05"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "SYUBETU06"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "SYUBETU07"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "SYUBETU08"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "SYUBETU09"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "SYUBETU10"
                   MOVE    14          TO  SPA-GMN-CUR
                   MOVE    99          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
