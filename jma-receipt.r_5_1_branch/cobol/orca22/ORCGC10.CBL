      ******************************************************************
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
      ****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 簡単修正・病名複写（Ｃ１０）
      *  管理者            : 
      *  04/02/12    NACL−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/05/14  簡単修正時の開始日と
      *                                     保険組合せのチェックの修正
      *  01.00.02    NACL-藤原    05/05/14  入外区分のよる同一病名の
      *                                     チェックを追加
      *
      *  02.06.01    NACL-藤原    05/05/14  病名複写機能対応
      *  02.06.02    NACL-藤原    05/08/25  労災・自賠責の保険限定の場合の
      *                                     同一診療科の同一病名を登録可能
      *                                     とする
      *  02.06.03    NACL-藤原    05/10/20  警告表示後の登録処理ができない
      *                                     ので修正
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *
      *  02.08.01    NACL-藤原    06/03/22  改正対応
      *                                     疾患区分の表示を変更
      *
      *  03.00.01    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.02.01    NACL-藤原    06/07/25  自費による保険限定の病名の重複
      *                                     登録を可能とする
      *
      *  03.04.01    NACL-藤原    06/11/01  病名一括削除機能対応
      *  03.04.02    NACL-藤原    06/11/27   ＳＰＡデータの削除
      *  03.04.03    NACL-藤原    07/03/02  月途中保険変更時の病名対応中止
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/10/11  診療科コピー（移動）機能対応
      *
      *  04.02.01    NACL-藤原    08/03/03  特定疾患区分07,08の追加
      *
      *  04.03.01    NACL-藤原    08/06/11  慢性と急性のチェック
      *
      *  04.04.01    NACL-藤原    09/05/08  病名複写時のチェックの修正
      *
      *  04.05.01    NACL-藤原    11/02/15  誕生日と診療開始日のチェック
      *  04.05.02    NACL-藤原    11/11/14  公害による保険限定の病名の重
      *                                     複登録を可能とする
      *
      *  04.06.01    NACL-藤原    10/08/31  年月省略入力対応
      *  04.06.02    NACL-藤原    10/11/05  補足コメント対応
      *
      *  04.07.01    NACL-藤原    13/07/12  第三者行為対応
      *  04.07.02    NACL-藤原    14/07/14  第三者行為の重複チェック修正
      *
      *  05.00.01    NACL-藤原    18/11/22  Warning 対応
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
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
           02  SPA-C02-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-C02-PTBYO-//.
           02  WRK-C02-HAISIYMD-OCC.
               03  WRK-C02-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-C02-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-C02-PTBYO-//.
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-MANSEI          PIC 9(01).
           03  FLG-KYUSEI          PIC 9(01).
           03  FLG-TENKIKBN        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXZ                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ERR-HENSYU-AREA.
               05  WRK-ERR-BYOMEI  PIC X(130).
               05  WRK-ERR-SRYYMD  PIC X(22).
               05  WRK-ERR-SRYKA   PIC X(20).
      *
           03  WRK-CHK-HENSYU-AREA.
               05  WRK-CHK-BYOMEI  PIC X(130).
               05  WRK-CHK-SRYKA   PIC X(02).
           03  WRK-CHK-SRYYMD      PIC X(08).
           03  WRK-CHK-TENKIYMD    PIC X(08).            
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(04).
               05  WRK-KBNMEI      PIC X(42).
               05  WRK-KBNMEITAN   PIC X(10).
               05  WRK-KBNID       PIC 9(10).
      *
           03  WRK-TENKIYMD        PIC X(08).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-KOHID           PIC 9(10).
      *
           03  WRK-TEKSTYMD        PIC 9(08).
           03  WRK-TEKEDYMD        PIC 9(08).
           03  WRK-RYOSTYMD        PIC 9(08).
           03  WRK-RYOEDYMD        PIC 9(08).
      *
           03  WRK-YMD-9.
               05  WRK-YY          PIC 9(04).
               05  WRK-MD          PIC 9(04).
               05  WRK-MD-R        REDEFINES   WRK-MD. 
                   07  WRK-MM      PIC 9(02).
                   07  WRK-DD      PIC 9(02). 
      *
           03  WRK-KANTAN-HENSYU-AREA.
               05  WRK-HENSYU-NUM  PIC X(40).
               05  WRK-HENSYU-NUM-G    OCCURS  5.
                   07  WRK-HENSYU-NUM-O
                                   PIC X(10).
                   07  WRK-STNUM-O PIC 9(05).
                   07  WRK-EDNUM-O PIC 9(05).
               05  WRK-STNUM       PIC X(05).
               05  WRK-EDNUM       PIC X(05).
               05  WRK-HENSYU-DATA-G.
                   07  WRK-HENSYU-DATA-O
                                   PIC X(10)   OCCURS  9.
                   07  WRK-HENSYU-DATA-9
                                   PIC 9(10).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-CIDMSG          PIC X(80).
      *
      *    保険組合せ、第三者行為重複チェック用エリア
           01  WRK-CHK-HKNCOMBI-AREA.
               02  WRK-CHK-HKNCOMBI1
                                   PIC 9(04).
               02  WRK-CHK-HKNCOMBI2
                                   PIC 9(04).
      *
           02  WRK-CHK-KOHID1      PIC 9(10).
           02  WRK-CHK-KOHID2      PIC 9(10).
      *
               02  FLG-HKNCOMBI-DABURI
                                   PIC 9(01).
               02  WRK-CHK-HKNCOMBI-REC    OCCURS  2.
           COPY    "CPHKNCOMBI.INC"    REPLACING   //COMB-//
                                       BY          //WRK-CHK-COMB-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-CIDCD       NOT =   SPACE
                   EVALUATE    SPA-CIDCD
                       WHEN    "0103"
                           PERFORM 520-CIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C10    "           TO  MCP-WINDOW
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
      *    確認画面より
           IF      SPA-MOTOPG      =   "CID1"
               IF      SPA-CIDCD       =   "0103"  OR  "0101"  OR
                                           "0105"  OR  "0108"
                   IF      SPA-CID1-FLG    =   "OK"
                       IF      SPA-CIDCD       =   "0105"  OR  "0108"
                           IF      SPA-C02-KEIKOKU-FLG1
                                                   =   1
                               MOVE    SPACE           TO  SPA-CID1-FLG
                               MOVE    "0103"          TO  SPA-CIDCD
                               GO  TO  300-SCREEN-EXT
                           END-IF
                       END-IF
                       PERFORM 4501-TOUROKU-SYORI-SEC
                       GO  TO  300-SCREEN-EXT
                   ELSE
                       MOVE    1               TO  SPA-GMN-C10-CUR
                   END-IF
                   INITIALIZE              SPA-CIDCD-AREA
                                           SPA-CID1-AREA  
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
                                           SPA-C01-HENSYU-DATA-G
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF              
      *
      *    確認画面より
           IF      SPA-MOTOPG      =   "CID3"
               IF      SPA-CIDCD3      =   "0102"
                   IF      SPA-CID3-FLG    =   "MV"    OR  "CP"
                       PERFORM 4501-TOUROKU-SYORI-SEC
                       GO  TO  300-SCREEN-EXT
                   ELSE
                       MOVE    1               TO  SPA-GMN-C10-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD3
                                               SPA-CID3-FLG
                   INITIALIZE              SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C01-HENSYU-DATA-G
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF              
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-CIDCD-AREA
                                   SPA-CID1-AREA
      *
      *    検索画面スパ編集処理     
           INITIALIZE              SPA-C10
      *
           MOVE    "-"             TO  SPA-GMN-C10-TENKIKBNL        (1)
           MOVE    "取消"          TO  SPA-GMN-C10-TENKIKBNMEIL     (1)
           MOVE    "1"             TO  SPA-GMN-C10-TENKIKBNL        (2)
           MOVE    "治ゆ"          TO  SPA-GMN-C10-TENKIKBNMEIL     (2)
           MOVE    "2"             TO  SPA-GMN-C10-TENKIKBNL        (3)
           MOVE    "死亡"          TO  SPA-GMN-C10-TENKIKBNMEIL     (3)
           MOVE    "3"             TO  SPA-GMN-C10-TENKIKBNL        (4)
           MOVE    "中止"          TO  SPA-GMN-C10-TENKIKBNMEIL     (4)
           MOVE    "8"             TO  SPA-GMN-C10-TENKIKBNL        (5)
           MOVE    "移行"          TO  SPA-GMN-C10-TENKIKBNMEIL     (5)
           MOVE    5               TO  SPA-GMN-C10-TENKIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-UTAGAIFLGL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-UTAGAIFLGL       (2)
           MOVE    "疑い"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (2)
           MOVE    "2"             TO  SPA-GMN-C10-UTAGAIFLGL       (3)
           MOVE    "急性"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (3)
           MOVE    "3"             TO  SPA-GMN-C10-UTAGAIFLGL       (4)
           MOVE    "急性かつ疑い"  TO  SPA-GMN-C10-UTAGAIFLGMEIL    (4)
           MOVE    4               TO  SPA-GMN-C10-UTAGAIFLG-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-SYUBYOFLGL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-SYUBYOFLGMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-SYUBYOFLGL       (2)
           MOVE    "主病名である"  TO  SPA-GMN-C10-SYUBYOFLGMEIL    (2)
           MOVE    2               TO  SPA-GMN-C10-SYUBYOFLG-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-MANSEIKBNL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-MANSEIKBNMEIL    (1)
           MOVE    "03"            TO  SPA-GMN-C10-MANSEIKBNL       (2)
           MOVE    "皮膚科特定疾患指導管理料(１)"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (2)
           MOVE    "04"            TO  SPA-GMN-C10-MANSEIKBNL       (3)
           MOVE    "皮膚科特定疾患指導管理料(２)"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (3)
           MOVE    "05"            TO  SPA-GMN-C10-MANSEIKBNL       (4)
           MOVE    "特定疾患療養管理料"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (4)
           MOVE    "07"            TO  SPA-GMN-C10-MANSEIKBNL       (5)
           MOVE    "てんかん指導料"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (5)
           MOVE    "08"            TO  SPA-GMN-C10-MANSEIKBNL       (6)
           MOVE    "特定疾患療養管理料又はてんかん指導料"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (6)
           MOVE    6               TO  SPA-GMN-C10-MANSEIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-NYUGAIKBNL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-NYUGAIKBNMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-NYUGAIKBNL       (2)
           MOVE    "入院"          TO  SPA-GMN-C10-NYUGAIKBNMEIL    (2)
           MOVE    "2"             TO  SPA-GMN-C10-NYUGAIKBNL       (3)
           MOVE    "入院外"        TO  SPA-GMN-C10-NYUGAIKBNMEIL    (3)
           MOVE    3               TO  SPA-GMN-C10-NYUGAIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-HKNCOMBIL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-HKNCOMBIMEIL    (1)
           MOVE    1               TO  SPA-GMN-C10-HKNCOMBI-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C02-HKNCOMBI-MAX
               ADD     1               TO  SPA-GMN-C10-HKNCOMBI-MAX  
               MOVE   SPA-GMN-C02-HKNCOMBILST (IDX)
                                       TO
                                       SPA-GMN-C10-HKNCOMBILST
                                           (SPA-GMN-C10-HKNCOMBI-MAX)
           END-PERFORM
      *
           MOVE    "取消"          TO  SPA-GMN-C10-KOHIDL    (1)
           MOVE    1               TO  SPA-GMN-C10-KOHID-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C02-KOHID-MAX
               ADD     1               TO  SPA-GMN-C10-KOHID-MAX  
               MOVE   SPA-GMN-C02-KOHIDLST (IDX)
                                       TO  SPA-GMN-C10-KOHIDLST
                                               (SPA-GMN-C10-KOHID-MAX)
           END-PERFORM
      *
      *    全科または単科のときは科設定なし
           IF    ( SPA-GMN-C02-SRYKA
                                   =   "00" )
           OR    ( SPA-GMN-C02-SRYKA-MAX
                                   =   1    ) 
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-C02-SRYKA-MAX
                   IF      SPA-GMN-C02-SRYKAL (IDX) 
                                           =   SPA-GMN-C02-SRYKA   OR  
                                               "00"
                       CONTINUE
                   ELSE
                       ADD     1               TO  SPA-GMN-C10-SRYKA-MAX  
                       MOVE   SPA-GMN-C02-SRYKALST (IDX)
                                               TO
                                               SPA-GMN-C10-SRYKALST
                                                (SPA-GMN-C10-SRYKA-MAX)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    1               TO  SPA-GMN-C10-CUR
           .
       310-SPASET-EXT.
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
                   MOVE    SPACE               TO  SPA-CID1-FLG
                   PERFORM 210-BACK
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 450-DELETE-SEC
      *    複写
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 450-COPY-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C10-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-SYORI-EXT.
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
      *    関連チェック処理
           PERFORM 4103-KANREN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
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
           MOVE    C10-TENKIKBN         TO  SPA-GMN-C10-TENKIKBN-G
           MOVE    C10-TENKIYMD         TO  SPA-GMN-C10-TENKIYMD
           MOVE    C10-UTAGAIFLG        TO  SPA-GMN-C10-UTAGAIFLG-G
           MOVE    C10-SYUBYOFLG        TO  SPA-GMN-C10-SYUBYOFLG-G
           MOVE    C10-MANSEIFLG        TO  SPA-GMN-C10-MANSEIKBN-G
           MOVE    C10-NYUGAIKBN        TO  SPA-GMN-C10-NYUGAIKBN-G
           MOVE    C10-HKNCOMBI         TO  SPA-GMN-C10-HKNCOMBI-G
           MOVE    C10-SRYYMD           TO  SPA-GMN-C10-SRYYMD
           MOVE    C10-SRYKA            TO  SPA-GMN-C10-SRYKA-G
           MOVE    C10-KOHID            TO  SPA-GMN-C10-KOHID
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
           PERFORM 4106-TENKIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4107-TENKIYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4101-UTAGAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4102-SYUBYOFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4103-MANSEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4104-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4105-HKNCOMBI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4110-KOHID-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4108-SRYYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4109-SRYKA-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い　入力処理
      *****************************************************************
       4101-UTAGAI-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-UTAGAIFLG   TO  WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C10-UTAGAIFLGMEI
           IF      SPA-GMN-C10-UTAGAIFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-UTAGAIFLGMEI   =   SPACE    
                  MOVE    "0018"               TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4101-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病入力処理
      *****************************************************************
       4102-SYUBYOFLG-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-SYUBYOFLG
                                        TO  WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C10-SYUBYOFLGMEI
           IF      SPA-GMN-C10-SYUBYOFLG  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-SYUBYOFLGMEI     =   SPACE
                   MOVE    "0019"               TO  SPA-ERRCD
               END-IF
           END-IF        
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4102-SYUBYOFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性　入力処理
      *****************************************************************
       4103-MANSEI-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-MANSEIKBN
                                       TO  WRK-KBN
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-MANSEIKBNMEI
      *                                 
           IF      SPA-GMN-C10-MANSEIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-MANSEIKBNMEI   =   SPACE    
                  MOVE    "0021"               TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4103-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4104-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-NYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-NYUGAIKBNMEI
      *                                 
           IF      SPA-GMN-C10-NYUGAIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-NYUGAIKBNMEI   =   SPACE    
                   MOVE    "0022"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4104-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       4105-HKNCOMBI-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-C10-HKNCOMBI
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-C10-HKNCOMBIMEI
           ELSE
               MOVE    SPA-GMN-C10-HKNCOMBI
                                           TO  WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0009"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7                   TO  SPA-GMN-C10-CUR
           END-IF
      *
           .
       4105-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分  入力処理
      *****************************************************************
       4106-TENKIKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-C10-TENKIKBN
                                       TO  WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-TENKIKBNMEI
      *                                 
           IF      SPA-GMN-C10-TENKIKBN
                                       =   SPACE   OR  "-"
               MOVE    SPACE               TO  SPA-NAI-C10-TENKIYMD
               MOVE    SPACE               TO  SPA-GMN-C10-TENKIYMD
           ELSE
               IF      SPA-GMN-C10-TENKIKBNMEI
                                           =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
               END-IF
           END-IF
      *     
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-C10-TENKIKBN NOT =   SPACE   AND "-" )
               AND   ( SPA-GMN-C10-TENKIYMD     =   SPACE           )
                   MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C10-TENKIYMD
                   MOVE    SPA-GMN-C02-SYSYMD   TO  SPA-GMN-C10-TENKIYMD
               END-IF
           ELSE    
               MOVE    1                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4106-TENKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰年月日  入力処理
      *****************************************************************
       4107-TENKIYMD-CHK-SEC            SECTION.
      *     
           MOVE    SPACE              TO  SPA-NAI-C10-TENKIYMD
      *
           IF      SPA-GMN-C10-TENKIYMD
                                      =   SPACE
               IF      SPA-GMN-C10-TENKIKBN   NOT =   SPACE   AND "-"
                   MOVE    "0036"          TO  SPA-ERRCD
               END-IF    
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-C10-TENKIYMD
                                           TO  SGDAY-INDAY
               MOVE    SPA-NAI-C02-SYSYMD  TO  SGDAY-SYSYMD
               MOVE    "1"                 TO  SGDAY-SYSKBN
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C10-TENKIYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C10-TENKIYMD
                   IF      SPA-GMN-C10-TENKIKBN    =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4107-TENKIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日（複写）入力処理
      *****************************************************************
       4108-SRYYMD-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-C10-SRYYMD
      *     
           IF      SPA-GMN-C10-SRYYMD  =   SPACE
               CONTINUE
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-C10-SRYYMD  TO  SGDAY-INDAY
               MOVE    SPA-NAI-C02-SYSYMD  TO  SGDAY-SYSYMD
               MOVE    "1"                 TO  SGDAY-SYSKBN
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C10-SRYYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C10-SRYYMD
               ELSE
                   MOVE    "0017"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-C10-SRYYMD  =   SPACE
                   CONTINUE
               ELSE
                   IF      SPA-NAI-C10-SRYYMD  <   SPA-NAI-C02-BIRTHDAY
                       MOVE    "0096"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    8                   TO  SPA-GMN-C10-CUR
           END-IF
           .
       4108-SRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       4109-SRYKA-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-C10-SRYKA   =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-SRYKA-MAX
                   IF      SPA-GMN-C10-SRYKA    
                                       =   SPA-GMN-C10-SRYKAL (IDY)
                       MOVE    SPA-GMN-C10-SRYKALST (IDY)
                                          TO  SPA-GMN-C10-SRYKA-G
                       MOVE    SPA-GMN-C10-SRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
               IF      SPA-GMN-C10-SRYKAMEI
                                           =   SPACE  
                   MOVE    "0012"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    9                   TO  SPA-GMN-C10-CUR
           END-IF    
           .
       4109-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為入力処理
      *****************************************************************
       4110-KOHID-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-C10-KOHID
      *
           IF      SPA-GMN-C10-KOHID   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C10-KOHID   TO  WRK-KBNMEI
               MOVE    ZERO                TO  WRK-KBNID
               PERFORM 5001-KOHID-HENSYU-SEC
               IF      WRK-KBNID           =   ZERO
                   MOVE    "0097"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-NAI-C10-SRYYMD
                                           <=  WRK-RYOEDYMD
                       MOVE    WRK-KBNID       TO  SPA-NAI-C10-KOHID
                   ELSE    
                       MOVE    "0098"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    10                  TO  SPA-GMN-C10-CUR
           END-IF    
      *
           .
       4110-KOHID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC        SECTION.
      *
      *    開始日と転帰日のチェック
           IF    ( SPA-NAI-C10-TENKIYMD
                                       NOT =   SPACE )
           AND   ( SPA-NAI-C10-SRYYMD  NOT =   SPACE )
               MOVE    "0006"                  TO  SPA-ERRCD
               MOVE    8                       TO  SPA-GMN-C10-CUR
               GO  TO  4103-KANREN-CHK-EXT
           END-IF       
      *
      *    開始日と診療科のチェック
           IF    ( SPA-ERRCD           =   SPACE )    
               IF    ( SPA-GMN-C10-SRYKA   NOT =   SPACE )
                   IF    ( SPA-GMN-C10-TENKIKBN    =   SPACE )
                   AND   ( SPA-NAI-C10-TENKIYMD    =   SPACE )
                   AND   ( SPA-GMN-C10-UTAGAIFLG   =   SPACE )
                   AND   ( SPA-GMN-C10-SYUBYOFLG   =   SPACE )
                   AND   ( SPA-GMN-C10-MANSEIKBN   =   SPACE )
                   AND   ( SPA-GMN-C10-NYUGAIKBN   =   SPACE )
                   AND   ( SPA-GMN-C10-HKNCOMBI    =   SPACE )
                   AND   ( SPA-NAI-C10-SRYYMD      =   SPACE )
                   AND   ( SPA-NAI-C10-KOHID       =   ZERO  )
                       CONTINUE
                   ELSE
                       MOVE    "0028"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-C10-CUR
                       GO  TO  4103-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保険組合せと第三者行為のチェック
           IF    ( SPA-NAI-C10-KOHID
                                   NOT =   ZERO    AND 9999999999 )
           AND   ( SPA-GMN-C10-HKNCOMBI
                                   NOT =   SPACE   AND "-"        )
               MOVE    "0099"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-C10-CUR
               GO  TO  4103-KANREN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-C01-STNUM-O (IDX)
                                       TO  WRK-STNUM-O (IDX)
               MOVE    SPA-C01-EDNUM-O (IDX)
                                       TO  WRK-EDNUM-O (IDX)
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )    
           AND   ( SPA-NAI-C10-SRYYMD  NOT
                                       =   SPACE )
      *        対象病名の転帰済のチェック（複写機能時）
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                       OR      SPA-ERRCD
                                   NOT =   SPACE
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                               OR      SPA-ERRCD
                                           NOT =   SPACE
                           IF      SPA-NAI-C02-TTENKIYMD (IDX)
                                                   =   SPACE
                               MOVE    "0003"      TO  SPA-ERRCD
                               MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                                   TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                               MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                           END-IF
                       END-PERFORM 
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )    
           AND   ( SPA-NAI-C10-SRYYMD  NOT
                                       =   SPACE )
      *        対象病名の重複チェック（開始日での複写機能時）
               INITIALIZE                  WRK-CHK-HENSYU-AREA
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                       OR      SPA-ERRCD
                                   NOT =   SPACE
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                               OR      SPA-ERRCD
                                           NOT =   SPACE
                           MOVE    SPA-NAI-C02-TSRYKA     (IDX)
                                               TO  WRK-CHK-SRYKA
                           MOVE    SPA-GMN-C02-TBYOMEI    (IDX)
                                               TO  WRK-CHK-BYOMEI
                           PERFORM VARYING IDXY    FROM    IDXX    BY  1
                                   UNTIL   IDXY    >       5
                                   OR      SPA-ERRCD
                                               NOT =   SPACE
                               IF      WRK-STNUM-O (IDXY)    >   ZERO
                                   PERFORM VARYING IDXZ    FROM
                                               WRK-STNUM-O (IDXY)  BY  1
                                           UNTIL   IDXZ    >
                                                       WRK-EDNUM-O(IDXY)
                                           OR      SPA-ERRCD
                                                       NOT =   SPACE
                                       PERFORM
                                         41031-COPY-SYORI-DABURI-CHK-SEC
                                   END-PERFORM
                               END-IF
                           END-PERFORM
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    8               TO  SPA-GMN-C10-CUR
           END-IF       
           .
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *        対象病名の重複チェック（複写機能時）
      *****************************************************************
       41031-COPY-SYORI-DABURI-CHK-SEC        SECTION.
      *           
           IF      IDXZ        NOT =   IDX
               IF    ( SPA-NAI-C02-TSRYKA  (IDXZ)
                                       =   WRK-CHK-SRYKA  )
               AND   ( SPA-GMN-C02-TBYOMEI (IDXZ) 
                                       =   WRK-CHK-BYOMEI )
      *
      *            入外区分が違う場合はチェックしない
                   IF      SPA-GMN-C10-NYUGAIKBN   =   SPACE
                       IF (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "1" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDXZ)
                                                       =   "2" ) )
                       OR (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "2" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDXZ)
                                                       =   "1" ) )
                           GO  TO  41031-COPY-SYORI-DABURI-CHK-EXT
                       END-IF
                   END-IF
      *
                   MOVE    "0007"    TO  SPA-ERRCD
      *
                   INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                   IF      SPA-GMN-C10-HKNCOMBI    =   SPACE
                       MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                               TO  WRK-CHK-HKNCOMBI1
                       MOVE    SPA-NAI-C02-THKNCOMBI  (IDXZ)
                                               TO  WRK-CHK-HKNCOMBI2
                   ELSE 
                       IF      SPA-GMN-C10-HKNCOMBI    =   "-"
                           MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                                                   WRK-CHK-HKNCOMBI2
                       ELSE
                           MOVE    SPA-GMN-C10-HKNCOMBI
                                               TO  WRK-CHK-HKNCOMBI1
                                                   WRK-CHK-HKNCOMBI2
                       END-IF
                   END-IF
                   IF      SPA-NAI-C10-KOHID   =   ZERO
                       MOVE    SPA-NAI-C02-TKOHID (IDX)
                                               TO  WRK-CHK-KOHID1
                       MOVE    SPA-NAI-C02-TKOHID  (IDXZ)
                                               TO  WRK-CHK-KOHID2
                   ELSE 
                       IF      WRK-HENSYU-DATA-9   =   9999999999
                           MOVE    ZERO            TO  WRK-CHK-KOHID1
                                                       WRK-CHK-KOHID2
                       ELSE
                           MOVE    SPA-NAI-C10-KOHID
                                                   TO  WRK-CHK-KOHID1
                                                       WRK-CHK-KOHID2
                       END-IF
                   END-IF
                   PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                   IF      FLG-HKNCOMBI-DABURI =   1
                       MOVE    SPACE               TO  SPA-ERRCD
                   ELSE 
                       MOVE    WRK-CHK-BYOMEI      TO  WRK-ERR-BYOMEI
                   END-IF
               END-IF
           END-IF
           .
       41031-COPY-SYORI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
      *    未入力チェック
           IF    ( SPA-GMN-C10-TENKIKBN    =   SPACE )
           AND   ( SPA-NAI-C10-TENKIYMD    =   SPACE )
           AND   ( SPA-GMN-C10-UTAGAIFLG   =   SPACE )
           AND   ( SPA-GMN-C10-SYUBYOFLG   =   SPACE )
           AND   ( SPA-GMN-C10-MANSEIKBN   =   SPACE )
           AND   ( SPA-GMN-C10-NYUGAIKBN   =   SPACE )
           AND   ( SPA-GMN-C10-HKNCOMBI    =   SPACE )
           AND   ( SPA-NAI-C10-SRYYMD      =   SPACE )
           AND   ( SPA-GMN-C10-SRYKA       =   SPACE )
           AND   ( SPA-NAI-C10-KOHID       =   ZERO  )
               MOVE    "0001"                  TO  SPA-ERRCD
               MOVE    1                       TO  SPA-GMN-C10-CUR
               GO  TO  4101-KAKUTEI-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-C01-STNUM-O (IDX)
                                       TO  WRK-STNUM-O (IDX)
               MOVE    SPA-C01-EDNUM-O (IDX)
                                       TO  WRK-EDNUM-O (IDX)
           END-PERFORM
      *
           INITIALIZE                          WRK-HENSYU-DATA-G
           MOVE    SPA-GMN-C10-TENKIKBN    TO  WRK-HENSYU-DATA-O (1)
           MOVE    SPA-NAI-C10-TENKIYMD    TO  WRK-HENSYU-DATA-O (2)
           MOVE    SPA-GMN-C10-UTAGAIFLG   TO  WRK-HENSYU-DATA-O (3)
           MOVE    SPA-GMN-C10-SYUBYOFLG   TO  WRK-HENSYU-DATA-O (4)
           MOVE    SPA-GMN-C10-MANSEIKBN   TO  WRK-HENSYU-DATA-O (5)
           MOVE    SPA-GMN-C10-NYUGAIKBN   TO  WRK-HENSYU-DATA-O (6)
           MOVE    SPA-GMN-C10-HKNCOMBI    TO  WRK-HENSYU-DATA-O (7)
           MOVE    SPA-NAI-C10-SRYYMD      TO  WRK-HENSYU-DATA-O (8)
           MOVE    SPA-GMN-C10-SRYKA       TO  WRK-HENSYU-DATA-O (9)
           MOVE    SPA-NAI-C10-KOHID       TO  WRK-HENSYU-DATA-9
      *
      *    簡単修正機能時のチェック
           IF    ( WRK-HENSYU-DATA-O (1)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (6)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-O (7)   NOT =   SPACE )
           OR    ( WRK-HENSYU-DATA-9       NOT =   ZERO  )
           OR    (   ( WRK-HENSYU-DATA-O (8)       =   SPACE )
                  AND( WRK-HENSYU-DATA-O (9)       =   SPACE ) )
      *        転帰・保険適用と開始日のチェック
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 41011-KANTAN-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               MOVE    1           TO  SPA-GMN-C10-CUR
                               GO  TO  4101-KAKUTEI-CHK-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    開始日複写機能時のチェック
           IF      WRK-HENSYU-DATA-O (8)   NOT =   SPACE
      *        開始日と保険適用の関連チェック
               IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   "-" AND SPACE  
                   IF      WRK-HENSYU-DATA-O (8) 
                                       <=   WRK-TEKEDYMD
                       CONTINUE
                   ELSE    
                       MOVE    "0005"      TO  SPA-ERRCD
                       MOVE    8           TO  SPA-GMN-C10-CUR
                       GO  TO  41012-COPY-DABURI-CHK-EXT
                   END-IF
               END-IF
      *        対象病名の転帰済のチェック
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 41012-COPY-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               MOVE    8           TO  SPA-GMN-C10-CUR
                               GO  TO  4101-KAKUTEI-CHK-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    診療科複写機能時のチェック
           IF      WRK-HENSYU-DATA-O (9)   NOT =   SPACE
      *        対象病名の重複チェック（診療科での複写・移動機能時）
               INITIALIZE                  WRK-CHK-HENSYU-AREA
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                       OR      SPA-ERRCD
                                   NOT =   SPACE
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 41032-SRYKA-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               MOVE    8           TO  SPA-GMN-C10-CUR
                               GO  TO  4101-KAKUTEI-CHK-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    慢性と急性の警告チェック
           IF    ( SPA-ERRCD            =   SPACE       )
           AND   ( WRK-HENSYU-DATA-O(3) =   "2" OR  "3" )
               MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                           WRK-ERR-SRYYMD
                                           WRK-ERR-SRYKA
               INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
               PERFORM 41014-KEIKOKU-CHK-SEC
           END-IF
      *
      *    警告チェック
           IF      SPA-ERRCD            =   SPACE
               MOVE    SPACE           TO  WRK-ERR-BYOMEI
                                           WRK-ERR-SRYYMD
                                           WRK-ERR-SRYKA
               INITIALIZE                  SPA-C02-KEIKOKU-FLG-AREA
                                           SPA-C02-KEIKOKU-MSG-AREA
      *
      *        他診療科の同一病名の警告チェック（診療科複写以外）
               PERFORM 41013-KEIKOKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-C10-SRYKA
                                   NOT =   SPACE )
               MOVE    "0102"              TO  SPA-CIDCD3
           END-IF
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正時の転帰・保険適用チェック処理
      *****************************************************************
       41011-KANTAN-DABURI-CHK-SEC         SECTION.
      *
      *    転記日のセット
           MOVE    SPA-NAI-C02-TTENKIYMD (IDX)
                                   TO  WRK-TENKIYMD
           IF      WRK-HENSYU-DATA-O (1)
                                   =   SPACE
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (1)
                                       =   "-"
                   MOVE    SPACE                   TO  WRK-TENKIYMD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (2)   TO  WRK-TENKIYMD
               END-IF
           END-IF
      * 
      *    転帰と開始日のチェック
           IF      WRK-HENSYU-DATA-O (1)
                               NOT =   "-" AND SPACE  
               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                       >    WRK-HENSYU-DATA-O (2)
                   MOVE    "0084"      TO  SPA-ERRCD
                   GO  TO  41011-KANTAN-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    保険適用と第三者行為の重複チェック
           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                   TO  WRK-HKNCOMBI
           MOVE    SPA-NAI-C02-TKOHID    (IDX)
                                   TO  WRK-KOHID
           IF      WRK-HENSYU-DATA-O (7)
                                   =   SPACE
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (7)
                                       =   "-"
                   MOVE    ZERO                    TO  WRK-HKNCOMBI
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (7)   TO  WRK-HKNCOMBI
               END-IF
           END-IF
           IF     WRK-HENSYU-DATA-9
                                   =   ZERO
               CONTINUE
           ELSE
               IF     WRK-HENSYU-DATA-9
                                       =   9999999999
                   MOVE    ZERO                TO  WRK-KOHID
               ELSE
                   MOVE    WRK-HENSYU-DATA-9   TO  WRK-KOHID
               END-IF
           END-IF
           IF    ( WRK-HKNCOMBI    >   ZERO )
           AND   ( WRK-KOHID       >   ZERO )
               MOVE    "0083"          TO  SPA-ERRCD
               MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                       TO  WRK-ERR-BYOMEI
               GO  TO  41011-KANTAN-DABURI-CHK-EXT
           END-IF 
      *
      *    保険適用と開始日のチェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   "-" AND SPACE  
               IF    ( SPA-NAI-C02-TSRYYMD (IDX)
                                   <=   WRK-TEKEDYMD )
                   CONTINUE
               ELSE    
                   MOVE    "0085"     TO  SPA-ERRCD
                   MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   GO  TO  41011-KANTAN-DABURI-CHK-EXT
               END-IF
           END-IF
      *    第三者行為と開始日のチェック
           IF     WRK-HENSYU-DATA-9
                                   =   ZERO    OR  9999999999
                   CONTINUE
           ELSE
               IF    ( SPA-NAI-C02-TSRYYMD (IDX)
                                   <=   WRK-RYOEDYMD )
                   CONTINUE
               ELSE    
                   MOVE    "0086"     TO  SPA-ERRCD
                   MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   GO  TO  41011-KANTAN-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    保険適用、第三者行為指定時の指定病名の重複チェック
           IF   ( WRK-HENSYU-DATA-O (7)  
                               NOT =   SPACE )
           OR   ( WRK-HENSYU-DATA-9
                                   >   ZERO  ) 
               PERFORM VARYING IDXY    FROM    IDXX    BY  1
                       UNTIL   IDXY    >       5
                       OR      SPA-ERRCD
                                   NOT =       SPACE
                   IF      WRK-STNUM-O (IDXY)  >   ZERO
                       PERFORM VARYING IDY FROM    WRK-STNUM-O(IDXY)
                                                   BY 1
                               UNTIL   IDY >       WRK-EDNUM-O(IDXY)
                               OR      SPA-ERRCD
                                           NOT =       SPACE
                           PERFORM 410111-KANTAN-HKNCOMB-CHK-SEC
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  41011-KANTAN-DABURI-CHK-EXT
           END-IF
      *
      *    マスタとのチェック
           INITIALIZE                      PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                       TO  PTBYO-HOSOKU-COMMENT
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE
               CONTINUE
           ELSE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE               TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)
                                               TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key40"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key41"             TO  WRK-MCP-PATHNAME
           END-IF    
           PERFORM 900-PTBYO-INV-SEC
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-TSRYYMD (IDX)   =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-TSRYKA  (IDX)   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-TRENNUM (IDX)   =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   PTBYO-SRYYMD
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >   PTBYO-SRYYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                           END-IF
                       END-IF
                       MOVE    PTBYO-HKNCOMBI      TO  WRK-CHK-HKNCOMBI2
                       IF      WRK-HENSYU-DATA-9       =   ZERO
                           MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                       ELSE 
                           IF      WRK-HENSYU-DATA-9   =   9999999999
                               MOVE    ZERO        TO  WRK-CHK-KOHID1
                           ELSE
                               MOVE    WRK-HENSYU-DATA-9
                                                   TO  WRK-CHK-KOHID1
                           END-IF
                       END-IF
                       MOVE    PTBYO-KOHID         TO  WRK-CHK-KOHID2
                       PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       41011-KANTAN-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更時同一病名重複チェック処理
      *****************************************************************
       410111-KANTAN-HKNCOMB-CHK-SEC       SECTION.
      *
      *    同一診療科・同一病名の場合だけチェック
           IF       IDX               =    IDY
                CONTINUE
           ELSE
               IF    ( SPA-GMN-C02-TBYOMEI (IDX)   =
                                   SPA-GMN-C02-TBYOMEI (IDY) )
               AND   ( SPA-NAI-C02-TSRYKA  (IDX)   =
                                   SPA-NAI-C02-TSRYKA  (IDY) )
      *
      *            入外区分が違う場合はチェックしない
                   IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                       IF (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "1" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "2" ) )
                       OR (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "2" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "1" ) )
                           GO  TO  410111-KANTAN-HKNCOMB-CHK-EXT
                       END-IF
                   END-IF     
      *
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           =   SPA-NAI-C02-TSRYYMD (IDY)
                           MOVE    "0002"              TO  SPA-ERRCD
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   SPA-NAI-C02-TSRYYMD (IDY)
                           IF      SPA-NAI-C02-TTENKIYMD (IDY)
                                               =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)   <
                                               SPA-NAI-C02-TSRYYMD (IDY)
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   IF      SPA-ERRCD           =   "0038"  OR
                                                   "0002"
                       INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                       IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                           MOVE    SPA-NAI-C02-THKNCOMBI (IDY)
                                                   TO  WRK-CHK-HKNCOMBI2
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (7)   =   "-"
                               MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                                                       WRK-CHK-HKNCOMBI2
                           END-IF
                       END-IF
                       IF      WRK-HENSYU-DATA-9       =   ZERO
                           MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                           MOVE    SPA-NAI-C02-TKOHID (IDY)
                                                   TO  WRK-CHK-KOHID2
                       ELSE 
                           IF      WRK-HENSYU-DATA-9   =   9999999999
                               MOVE    ZERO        TO  WRK-CHK-KOHID1
                                                       WRK-CHK-KOHID2
                           ELSE
                               MOVE    WRK-HENSYU-DATA-9
                                                   TO  WRK-CHK-KOHID1
                                                       WRK-CHK-KOHID2
                           END-IF
                       END-IF
                       PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                       IF      FLG-HKNCOMBI-DABURI =   1
                           MOVE   SPACE            TO  SPA-ERRCD
                       ELSE 
                           MOVE    SPA-NAI-C02-TSRYYMD (IDY)
                                                   TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3
                                                   TO  WRK-ERR-SRYYMD
                           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       410111-KANTAN-HKNCOMB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       410112-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
      *    医保以外の保険がない場合はチェックしない
           IF    ( SPA-NAI-C02-HKNCOMBI-ROUSAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-JIHI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-KOUGAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-KOHID-DAISANSYA
                                       =   ZERO )
               GO  TO   410112-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険限定の設定がない場合はチェックしない
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
           AND   ( WRK-CHK-KOHID1      =   ZERO )
           AND   ( WRK-CHK-KOHID2      =   ZERO )
               GO  TO   410112-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの重複チェック
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
             CONTINUE
           ELSE
             IF      WRK-CHK-HKNCOMBI1
                                       NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI1   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (1)
             END-IF    
             IF      WRK-CHK-HKNCOMBI2
                                   NOT =   ZERO
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI2   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (2)
             END-IF
      *
      *      片方のみ労災または自賠責または公害の場合は登録可能とする
             EVALUATE    WRK-CHK-COMB-HKNNUM (1)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2)
               WHEN    "971"       ALSO    "971"
               WHEN    "973"       ALSO    "973"
               WHEN    "975"       ALSO    "975"
                   IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                       MOVE    1                TO  FLG-HKNCOMBI-DABURI
                   END-IF
               WHEN    "971"       ALSO    ANY
               WHEN    "973"       ALSO    ANY
               WHEN    "975"       ALSO    ANY
               WHEN    ANY         ALSO    "971"
               WHEN    ANY         ALSO    "973"
               WHEN    ANY         ALSO    "975"
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
             END-EVALUATE
      *
      *      自費と保険限定なしまたは同じ自費の場合は登録不可とする
             IF    ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   =   "98" ) 
             OR    ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                   =   "98" ) 
               EVALUATE    WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2) (1:2)
                   WHEN    "98"       ALSO    "98"
                       IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                           MOVE    1            TO  FLG-HKNCOMBI-DABURI
                       END-IF
                   WHEN    "98"       ALSO    SPACE
                   WHEN    SPACE      ALSO    "98"
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
             END-IF
      *
      *      保険組合せで登録不可のとき
             IF      FLG-HKNCOMBI-DABURI =   ZERO
                 GO  TO   410112-HKNCOMBI-DABURI-CHK-EXT
             END-IF
           END-IF
      *
      *    第三者行為の設定がない場合はチェックしない
           IF    ( WRK-CHK-KOHID1   =   ZERO )
           AND   ( WRK-CHK-KOHID2   =   ZERO )
               GO  TO   410112-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    第三者行為の設定が両方の場合
           IF    ( WRK-CHK-KOHID1
                               NOT =   ZERO )
           AND   ( WRK-CHK-KOHID2
                               NOT =   ZERO )
      *        同じ第三者行為の場合は登録不可とする
               IF      WRK-CHK-KOHID1  =   WRK-CHK-KOHID2
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-IF
               GO  TO  410112-HKNCOMBI-DABURI-CHK-EXT
           ELSE
      *        片方が医保以外のときチェックしない
               IF    ( WRK-CHK-KOHID1
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  410112-HKNCOMBI-DABURI-CHK-EXT
               END-IF
               IF    ( WRK-CHK-KOHID2
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  410112-HKNCOMBI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    医保と第三者行為のため登録不可とする
           MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
      *
           .                   
       410112-HKNCOMBI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名複写機能時の転帰・保険適用チェック処理
      *****************************************************************
       41012-COPY-DABURI-CHK-SEC         SECTION.
      *
      *    保険適用、第三者行為指定時の指定病名の重複チェック
           IF   ( WRK-HENSYU-DATA-O (7)  
                               NOT =   SPACE )
           OR   ( WRK-HENSYU-DATA-9
                                   >   ZERO  ) 
               PERFORM VARYING IDXY    FROM    IDXX    BY  1
                       UNTIL   IDXY    >       5
                       OR      SPA-ERRCD
                                   NOT =       SPACE
                   IF      WRK-STNUM-O (IDXY)  >   ZERO
                       PERFORM VARYING IDY FROM    WRK-STNUM-O (IDXY)
                                           BY 1
                               UNTIL   IDY >       WRK-EDNUM-O(IDXY)
                               OR      SPA-ERRCD
                                       NOT =       SPACE
                           PERFORM 410121-COPY-HKNCOMBI-CHK-SEC
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  41012-COPY-DABURI-CHK-EXT
           END-IF
      *
      *
      *    マスタとのチェック
           INITIALIZE                  PTBYOMEI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                       TO  PTBYO-HOSOKU-COMMENT
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE
               CONTINUE
           ELSE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE               TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)
                                               TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key40"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"      TO  WRK-MCP-TABLE
               MOVE    "key41"             TO  WRK-MCP-PATHNAME
           END-IF    
           PERFORM 900-PTBYO-INV-SEC
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               EVALUATE    TRUE
                   WHEN    WRK-HENSYU-DATA-O (8)
                                       =   PTBYO-SRYYMD
                       MOVE    "0002"              TO  SPA-ERRCD
                   WHEN    WRK-HENSYU-DATA-O (8)
                                       <   PTBYO-SRYYMD
                       MOVE    "0038"      TO  SPA-ERRCD
                   WHEN    WRK-HENSYU-DATA-O (8)
                                       >   PTBYO-SRYYMD
                       IF      PTBYO-TENKIYMD  =   SPACE
                           MOVE    "0038"      TO  SPA-ERRCD
                       ELSE
                           IF      WRK-HENSYU-DATA-O (8)
                                           <   PTBYO-TENKIYMD
                               MOVE    "0038"      TO  SPA-ERRCD
                           END-IF
                       END-IF
               END-EVALUATE
      *
               IF      SPA-ERRCD           =   "0038"  OR
                                               "0002"
                   INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                   IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                       MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                               TO  WRK-CHK-HKNCOMBI1
                   ELSE 
                       IF      WRK-HENSYU-DATA-O (7)   =   "-"
                           MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                       ELSE
                           MOVE    WRK-HENSYU-DATA-O (7)
                                               TO  WRK-CHK-HKNCOMBI1
                       END-IF
                   END-IF
                   MOVE    PTBYO-HKNCOMBI      TO  WRK-CHK-HKNCOMBI2
                   IF      WRK-HENSYU-DATA-9       =   ZERO
                       MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                   ELSE 
                       IF      WRK-HENSYU-DATA-9   =   9999999999
                           MOVE    ZERO            TO  WRK-CHK-KOHID1
                        ELSE
                           MOVE    WRK-HENSYU-DATA-9
                                                   TO  WRK-CHK-KOHID1
                       END-IF
                   END-IF
                   MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                   PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                   IF      FLG-HKNCOMBI-DABURI =   1
                       MOVE   SPACE            TO  SPA-ERRCD
                   ELSE 
                       MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3
                                               TO  WRK-ERR-SRYYMD
                       MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       41012-COPY-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名複写機能時の保険組合せ変更時同一病名重複チェック処理
      *****************************************************************
       410121-COPY-HKNCOMBI-CHK-SEC SECTION.
      *
      *    同一診療科・同一病名の場合だけチェック
           IF      IDX             =    IDY
                CONTINUE
           ELSE
               IF    ( SPA-GMN-C02-TBYOMEI (IDX)   =
                                   SPA-GMN-C02-TBYOMEI (IDY) )
               AND   ( SPA-NAI-C02-TSRYKA  (IDX)   =
                                   SPA-NAI-C02-TSRYKA  (IDY) )
      *
      *            入外区分が違う場合はチェックしない
                   IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                       IF (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "1" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "2" ) )
                       OR (      ( SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                       =   "2" )
                           AND   ( SPA-NAI-C02-TNYUGAIKBN (IDY)
                                                       =   "1" ) )
                           GO  TO  410121-COPY-HKNCOMBI-CHK-EXT
                       END-IF
                   END-IF
      *     
                   INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                   IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                       MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                               TO  WRK-CHK-HKNCOMBI1
                       MOVE    SPA-NAI-C02-THKNCOMBI (IDY)
                                               TO  WRK-CHK-HKNCOMBI2
                   ELSE 
                       IF      WRK-HENSYU-DATA-O (7)   =   "-"
                           MOVE    ZERO        TO  WRK-CHK-HKNCOMBI1
                                                   WRK-CHK-HKNCOMBI2
                       ELSE
                           MOVE    WRK-HENSYU-DATA-O (7)
                                               TO  WRK-CHK-HKNCOMBI1
                                                   WRK-CHK-HKNCOMBI2
                       END-IF
                   END-IF
                   IF      WRK-HENSYU-DATA-9       =   ZERO
                       MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                       MOVE    SPA-NAI-C02-TKOHID (IDY)
                                                   TO  WRK-CHK-KOHID2
                   ELSE 
                       IF      WRK-HENSYU-DATA-9   =   9999999999
                           MOVE    ZERO        TO  WRK-CHK-KOHID1
                                                   WRK-CHK-KOHID2
                       ELSE
                           MOVE    WRK-HENSYU-DATA-9
                                               TO  WRK-CHK-KOHID1
                                                   WRK-CHK-KOHID2
                       END-IF
                   END-IF
                   PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                   IF      FLG-HKNCOMBI-DABURI =   1
                       MOVE   SPACE            TO  SPA-ERRCD
                   ELSE 
                       MOVE    SPA-NAI-C02-TSRYYMD (IDY)
                                               TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3
                                               TO  WRK-ERR-SRYYMD
                       MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   END-IF
               END-IF
           END-IF
           .
       410121-COPY-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       41013-KEIKOKU-CHK-SEC                SECTION.
      *
      *    死亡区分更新のチェック
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
      *        転帰区分が２で死亡区分が生存のとき
               IF      PTINF-DEATHKBN  NOT =   "1"
                   IF      WRK-HENSYU-DATA-O (1)
                                               =   "2"
                       IF      SPA-C02-1042-MSG5-DSPKBN
                                                   =   0
                           MOVE    "0105"              TO  SPA-CIDCD
                       END-IF
                       MOVE    1           TO  SPA-C02-KEIKOKU-FLG4
                   END-IF
               ELSE
      *            転帰区分を２以外に変更したときに死亡区分が死亡のとき
                   IF      WRK-HENSYU-DATA-O (1)
                                           NOT =   "2"
                       MOVE    ZERO        TO  FLG-TENKIKBN
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       5
                               OR      FLG-TENKIKBN    =   1
                           IF      WRK-STNUM-O (IDXX)  >   ZERO
                               PERFORM VARYING IDX     FROM
                                               WRK-STNUM-O (IDXX)  BY  1
                                       UNTIL   IDX     >
                                                   WRK-EDNUM-O (IDXX)
                                       OR      FLG-TENKIKBN
                                                       =   1
                                   IF      SPA-NAI-C02-TTENKIKBN  (IDX)
                                                       =   "2"
                                       MOVE    1   TO  FLG-TENKIKBN
                                   END-IF
                               END-PERFORM
                          END-IF
                       END-PERFORM
                       IF      FLG-TENKIKBN     =   1
                           PERFORM 410131-KEIKOKU-TENKIKBN-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    他診療科の同一病名の警告チェック
           IF    ( SPA-C02-1042-MSG4-DSPKBN
                                       =   1     )
           AND   ( SPA-GMN-C10-SRYKA   =   SPACE )
               CONTINUE
           ELSE
               GO  TO  41013-KEIKOKU-CHK-EXT
           END-IF
      * 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
      ************ OR      SPA-CIDCD           NOT =   SPACE
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   PERFORM VARYING IDX     FROM
                                       WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
      ************         OR      SPA-CIDCD       NOT =   SPACE
      *
                       IF      WRK-HENSYU-DATA-O (2)   =   SPACE
                           IF      WRK-HENSYU-DATA-O (8)   =   SPACE
                               MOVE    SPA-NAI-C02-TTENKIYMD  (IDX)
                                                   TO  WRK-CHK-TENKIYMD
                           ELSE
                               MOVE    SPACE       TO  WRK-CHK-TENKIYMD
                           END-IF
                       ELSE 
                           MOVE    WRK-HENSYU-DATA-O (2)
                                                   TO  WRK-CHK-TENKIYMD
                       END-IF     
      *
                       IF      WRK-HENSYU-DATA-O (8)   =   SPACE
                           MOVE    SPA-NAI-C02-TSRYYMD  (IDX)
                                                   TO  WRK-CHK-SRYYMD
                       ELSE 
                           MOVE    WRK-HENSYU-DATA-O (8)
                                                   TO  WRK-CHK-SRYYMD
                       END-IF     
      *
                       INITIALIZE                      PTBYOMEI-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
                       MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
                       MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                                   TO  PTBYO-SRYKA
                       MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                                   TO  PTBYO-BYOMEI
                       MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                                   TO
                                                   PTBYO-HOSOKU-COMMENT
                       MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                                   TO  PTBYO-NYUGAIKBN
                       IF      WRK-HENSYU-DATA-O (6)   =   SPACE
                           CONTINUE
                       ELSE 
                           IF      WRK-HENSYU-DATA-O (6)   =   "-"
                               MOVE    SPACE       TO  PTBYO-NYUGAIKBN
                           ELSE
                               MOVE    WRK-HENSYU-DATA-O (6)
                                                   TO  PTBYO-NYUGAIKBN
                           END-IF
                       END-IF     
                       IF      PTBYO-NYUGAIKBN     =   SPACE
                           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                           MOVE    "key42"         TO  WRK-MCP-PATHNAME
                       ELSE
                           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
                           MOVE    "key43"         TO  WRK-MCP-PATHNAME
                       END-IF
      *
                       PERFORM 900-PTBYO-INV-SEC
      *
                       PERFORM         UNTIL   FLG-PTBYOMEI    =   1
      *********                        OR      SPA-CIDCD   NOT =   SPACE
                           EVALUATE    TRUE
                               WHEN    WRK-CHK-SRYYMD
                                                   =   PTBYO-SRYYMD
                                   MOVE    1              TO
                                                   SPA-C02-KEIKOKU-FLG1
                               WHEN    WRK-CHK-SRYYMD
                                                   <   PTBYO-SRYYMD
                                   IF      WRK-CHK-TENKIYMD
                                                       =   SPACE
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   ELSE
                                       IF     WRK-CHK-TENKIYMD
                                                   >   PTBYO-SRYYMD
                                           MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                       END-IF
                                   END-IF
                               WHEN    WRK-CHK-SRYYMD
                                                   >   PTBYO-SRYYMD
                                   IF      PTBYO-TENKIYMD =   SPACE
                                       MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                   ELSE
                                       IF      WRK-CHK-SRYYMD
                                                   <   PTBYO-TENKIYMD
                                           MOVE    1          TO
                                                   SPA-C02-KEIKOKU-FLG1
                                       END-IF
                                   END-IF
                           END-EVALUATE
                           IF      SPA-C02-KEIKOKU-FLG1    =   1
                               INITIALIZE          WRK-CHK-HKNCOMBI-AREA
      *
                               IF      WRK-HENSYU-DATA-O (7)   =   SPACE
                                   MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                                   TO  WRK-CHK-HKNCOMBI1
                               ELSE 
                                   IF      WRK-HENSYU-DATA-O (7)
                                                       =   "-"
                                       MOVE    ZERO        TO
                                                       WRK-CHK-HKNCOMBI1
                                   ELSE
                                       MOVE    WRK-HENSYU-DATA-O (7)
                                                   TO  WRK-CHK-HKNCOMBI1
                                   END-IF
                               END-IF
                               MOVE    PTBYO-HKNCOMBI
                                                   TO  WRK-CHK-HKNCOMBI2
                               IF      WRK-HENSYU-DATA-9       =   ZERO
                                   MOVE    SPA-NAI-C02-TKOHID (IDX)
                                                   TO  WRK-CHK-KOHID1
                               ELSE 
                                   IF      WRK-HENSYU-DATA-9   =
                                                           9999999999
                                       MOVE    ZERO
                                                   TO  WRK-CHK-KOHID1
                                   ELSE
                                       MOVE    WRK-HENSYU-DATA-9
                                                   TO  WRK-CHK-KOHID1
                                   END-IF
                               END-IF
                               MOVE    PTBYO-KOHID TO  WRK-CHK-KOHID2
                               PERFORM 410112-HKNCOMBI-DABURI-CHK-SEC
                               IF      FLG-HKNCOMBI-DABURI =   1
                                   MOVE    ZERO       TO
                                                   SPA-C02-KEIKOKU-FLG1
                               ELSE 
                                   MOVE    1           TO  FLG-PTBYOMEI
                                   IF      SPA-CIDCD   =   SPACE
                                       MOVE    "0103"      TO  SPA-CIDCD
                                   END-IF
                                   MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                                   PERFORM 5002-HIZUKE-HEN-SEC
                                   MOVE    LNK-DAY2-EDTYMD3
                                                           TO
                                                       WRK-ERR-SRYYMD
                                   MOVE    PTBYO-SRYKA     TO  WRK-KBN
                                   PERFORM 5001-SRYKA-HENSYU-SEC
                                   MOVE    WRK-KBNMEI          TO
                                                           WRK-ERR-SRYKA
                                   MOVE    PTBYO-BYOMEI
                                                  TO  WRK-ERR-BYOMEI
                               END-IF
                           END-IF
      *
                           IF      FLG-PTBYOMEI    =   ZERO
                               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
                               MOVE    WRK-MCP-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-PTBYO-READ-SEC
                           END-IF
                       END-PERFORM
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF      WRK-ERR-BYOMEI      NOT =   SPACE
               MOVE    WRK-ERR-BYOMEI      TO  SPA-C02-KEIKOKU-BYOMEI
               MOVE    WRK-ERR-SRYKA       TO  SPA-C02-KEIKOKU-SRYKA
               MOVE    WRK-ERR-SRYYMD      TO  SPA-C02-KEIKOKU-SRYYMD
               MOVE    SPACE               TO  WRK-ERR-BYOMEI
                                               WRK-ERR-SRYKA
                                               WRK-ERR-SRYYMD
           END-IF
           .
       41013-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    死亡区分更新確認メッセージ表示のチェック処理
      *****************************************************************
       410131-KEIKOKU-TENKIKBN-CHK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-TENKIKBN
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID
                                   TO  PTBYO-PTID
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key48"         TO  MCP-PATHNAME
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptbyomei"
                                       TO  MCP-TABLE
               MOVE    "key48"     TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
           PERFORM     UNTIL   FLG-PTBYOMEI    =   1
                       OR      FLG-TENKIKBN    =   9
               MOVE    ZERO            TO  FLG-CHK
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                       OR      FLG-CHK =       9
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX >   WRK-EDNUM-O (IDXX)
                               OR      FLG-CHK     =   9
                           IF    ( PTBYO-SRYYMD    =
                                           SPA-NAI-C02-TSRYYMD (IDX) )
                           AND   ( PTBYO-RENNUM    =
                                           SPA-NAI-C02-TRENNUM (IDX) )
                           AND   ( PTBYO-SRYKA     =
                                           SPA-NAI-C02-TSRYKA  (IDX) )
                               MOVE    9       TO  FLG-CHK
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
      *        対象病名以外に転帰区分２が存在するとき
               IF      FLG-CHK         =   0
                   MOVE    9               TO  FLG-TENKIKBN
               END-IF
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key48"         TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key48"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    転帰区分２の病名が存在しないとき
           IF      FLG-TENKIKBN    =   0
               IF      SPA-C02-1042-MSG5-DSPKBN
                                       =   0
                   MOVE    "0108"          TO  SPA-CIDCD
               END-IF
               MOVE    9           TO  SPA-C02-KEIKOKU-FLG4
           END-IF
           .
       410131-KEIKOKU-TENKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       41014-KEIKOKU-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
                   OR      SPA-ERRCD           NOT =   SPACE
               IF      WRK-STNUM-O (IDXX)  >   ZERO
                   PERFORM VARYING IDX     FROM
                                       WRK-STNUM-O (IDXX)  BY  1
                           UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           OR      SPA-ERRCD       NOT =   SPACE
                       MOVE    ZERO                TO  FLG-MANSEI
                                                       FLG-KYUSEI
                       PERFORM VARYING IDZZ    FROM    1   BY  2
                               UNTIL   IDZZ    >       79
                           IF      SPA-GMN-C02-TBYOMEI (IDX)(IDZZ:4)
                                                           =   "慢性"
                               MOVE    1               TO  FLG-MANSEI
                           END-IF
                           IF      SPA-GMN-C02-TBYOMEI (IDX)(IDZZ:4)
                                                       =   "急性"
                               MOVE    1               TO  FLG-KYUSEI
                           END-IF
                           IF    ( FLG-MANSEI              =   1 )
                           AND   ( FLG-KYUSEI              =   1 )
                               MOVE    80                      TO  IDZZ
                           END-IF
                       END-PERFORM    
      *
                       IF      FLG-MANSEI              =   1
                           IF      FLG-KYUSEI              =   1
                               CONTINUE
                           ELSE
                               MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                                   TO  WRK-ERR-BYOMEI
                               MOVE    "0104"      TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       41014-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力病名診療科の重複チェック処理
      *****************************************************************
       41032-SRYKA-DABURI-CHK-SEC        SECTION.
      *
      *    マスタとの重複チェック
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-GMN-C10-SRYKA   TO  PTBYO-SRYKA
           MOVE    SPA-NAI-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-THOSOKU-COMMENT (IDX)
                                       TO  PTBYO-HOSOKU-COMMENT
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      PTBYO-NYUGAIKBN     =   SPACE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key40"                 TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_ptbyomei"          TO  WRK-MCP-TABLE
               MOVE    "key41"                 TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               EVALUATE    TRUE
                   WHEN    SPA-NAI-C02-TSRYYMD (IDX)  =   PTBYO-SRYYMD
                       MOVE    "0002"              TO  SPA-ERRCD
                   WHEN    SPA-NAI-C02-TSRYYMD (IDX)  <   PTBYO-SRYYMD
                       IF      SPA-NAI-C02-TTENKIYMD (IDX) =   SPACE
                           MOVE    "0038"          TO  SPA-ERRCD
                       ELSE
                           IF     SPA-NAI-C02-TTENKIYMD (IDX)
                                               >   PTBYO-SRYYMD
                               MOVE    "0038"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   WHEN    SPA-NAI-C02-TSRYYMD (IDX)  >   PTBYO-SRYYMD
                       IF      PTBYO-TENKIYMD  =   SPACE
                           MOVE    "0038"      TO  SPA-ERRCD
                       ELSE
                           IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-TENKIYMD
                               MOVE    "0038"      TO  SPA-ERRCD
                           END-IF
                       END-IF
               END-EVALUATE
      *
               IF      SPA-ERRCD           =   "0038"  OR
                                               "0002"
                   INITIALIZE                  WRK-CHK-HKNCOMBI-AREA
      *
                   MOVE    SPA-NAI-C02-THKNCOMBI (IDX)
                                           TO  WRK-CHK-HKNCOMBI1
                   MOVE    PTBYO-HKNCOMBI  TO  WRK-CHK-HKNCOMBI2
                   MOVE    SPA-NAI-C02-TKOHID    (IDX)
                                           TO  WRK-CHK-KOHID1
                   MOVE    PTBYO-KOHID     TO  WRK-CHK-KOHID2
                   PERFORM 410321-HKNCOMBI-DABURI-CHK-SEC
                   IF      FLG-HKNCOMBI-DABURI =   1
                       MOVE    SPACE           TO  SPA-ERRCD
                   ELSE 
                       MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3
                                               TO  WRK-ERR-SRYYMD
                       MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .                   
       41032-SRYKA-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一病名における保険限定の重複チェック処理
      *****************************************************************
       410321-HKNCOMBI-DABURI-CHK-SEC   SECTION.
      *
      *    医保以外の保険がない場合はチェックしない
           IF    ( SPA-NAI-C02-HKNCOMBI-ROUSAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-JIHI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-HKNCOMBI-KOUGAI
                                       =   ZERO )
           AND   ( SPA-NAI-C02-KOHID-DAISANSYA
                                       =   ZERO )
               GO  TO   410321-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険限定の設定がない場合はチェックしない
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
           AND   ( WRK-CHK-KOHID1      =   ZERO )
           AND   ( WRK-CHK-KOHID2      =   ZERO )
               GO  TO   410321-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    保険組合せの重複チェック
           IF    ( WRK-CHK-HKNCOMBI1   =   ZERO )
           AND   ( WRK-CHK-HKNCOMBI2   =   ZERO )
             CONTINUE
           ELSE
             IF      WRK-CHK-HKNCOMBI1
                                   NOT =   ZERO
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI1   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (1)
             END-IF    
             IF      WRK-CHK-HKNCOMBI2
                                   NOT =   ZERO
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-NAI-C02-PTID    TO  COMB-PTID
               MOVE    WRK-CHK-HKNCOMBI2   TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 900-COMB-INV-SEC
               MOVE    HKNCOMBI-REC        TO  WRK-CHK-HKNCOMBI-REC (2)
             END-IF
      *
      *      片方のみ労災または自賠責または公害の場合は登録可能とする
             EVALUATE    WRK-CHK-COMB-HKNNUM (1)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2)
               WHEN    "971"       ALSO    "971"
               WHEN    "973"       ALSO    "973"
               WHEN    "975"       ALSO    "975"
                   IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                       MOVE    1                TO  FLG-HKNCOMBI-DABURI
                   END-IF
               WHEN    "971"       ALSO    ANY
               WHEN    "973"       ALSO    ANY
               WHEN    "975"       ALSO    ANY
               WHEN    ANY         ALSO    "971"
               WHEN    ANY         ALSO    "973"
               WHEN    ANY         ALSO    "975"
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
             END-EVALUATE
      *
      *      自費と保険限定なしまたは同じ自費の場合は登録不可とする
             IF    ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   =   "98" ) 
             OR    ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                   =   "98" ) 
               EVALUATE    WRK-CHK-COMB-HKNNUM (1) (1:2)
                                   ALSO    WRK-CHK-COMB-HKNNUM (2) (1:2)
                   WHEN    "98"       ALSO    "98"
                       IF      WRK-CHK-COMB-HKNID (1)
                                       NOT =    WRK-CHK-COMB-HKNID (2)
                           MOVE    1            TO  FLG-HKNCOMBI-DABURI
                       END-IF
                   WHEN    "98"       ALSO    SPACE
                   WHEN    SPACE      ALSO    "98"
                       MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
                   WHEN    OTHER
                       MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-EVALUATE
             END-IF
      *
      *      保険組合せで登録不可のとき
             IF      FLG-HKNCOMBI-DABURI =   ZERO
                 GO  TO   410321-HKNCOMBI-DABURI-CHK-EXT
             END-IF
           END-IF
      *
      *    第三者行為の設定がない場合はチェックしない
           IF    ( WRK-CHK-KOHID1   =   ZERO )
           AND   ( WRK-CHK-KOHID2   =   ZERO )
               GO  TO   410321-HKNCOMBI-DABURI-CHK-EXT
           END-IF
      *
      *    第三者行為の設定が両方の場合
           IF    ( WRK-CHK-KOHID1
                               NOT =   ZERO )
           AND   ( WRK-CHK-KOHID2
                               NOT =   ZERO )
      *        同じ第三者行為の場合は登録不可とする
               IF      WRK-CHK-KOHID1  =   WRK-CHK-KOHID2
                   MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI-DABURI
               END-IF
               GO  TO  410321-HKNCOMBI-DABURI-CHK-EXT
           ELSE
      *        片方が医保以外のときチェックしない
               IF    ( WRK-CHK-KOHID1
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (2) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  410321-HKNCOMBI-DABURI-CHK-EXT
               END-IF
               IF    ( WRK-CHK-KOHID2
                                   NOT =   ZERO             )
               AND   ( WRK-CHK-COMB-HKNNUM (1) (1:2)
                                       =   "98"    OR  "97" )
                   GO  TO  410321-HKNCOMBI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    医保と第三者行為のため登録不可とする
           MOVE    ZERO            TO  FLG-HKNCOMBI-DABURI
      *
           .                   
       410321-HKNCOMBI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C10"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    病名削除処理
      *****************************************************************
       450-DELETE-SEC                    SECTION.
      *
           MOVE    "0101"                TO SPA-CIDCD
      *
           .
       450-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名複写処理
      *****************************************************************
       450-COPY-SEC                    SECTION.
      *
           IF     SPA-GMN-C10-SRYYMD   =   SPACE
               MOVE    SPA-GMN-C02-SYSYMD  TO  C10-SRYYMD
               MOVE    "B10"               TO  WRK-MCP-WIDGET
               MOVE    ZERO                TO  SPA-GMN-C10-CUR
      *
               MOVE    ZERO                TO  FLG-TOUROKU
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           ELSE
               MOVE    99                  TO  SPA-GMN-C10-CUR
           END-IF
      *
           .
       450-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD            =   SPACE )
      *        修正項目をパラメタに設定     
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       9
                   MOVE    WRK-HENSYU-DATA-O (IDX)
                                       TO  SPA-C01-HENSYU-DATA-O (IDX)
               END-PERFORM
               MOVE    WRK-HENSYU-DATA-9
                                       TO  SPA-C01-HENSYU-DATA-9
           END-IF
      *
           IF    ( SPA-ERRCD            =   SPACE )
           AND   ( SPA-CIDCD            =   SPACE )
           AND   ( SPA-CIDCD3           =   SPACE )
               PERFORM 4501-TOUROKU-SYORI-SEC
           END-IF
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4501-TOUROKU-SYORI-SEC             SECTION.
      *
           IF      SPA-CIDCD3      =   "0102"
               MOVE    SPA-CID3-FLG    TO  SPA-CID1-FLG
               MOVE    SPACE           TO  SPA-CID3-FLG
           ELSE
               IF      SPA-CIDCD       =   "0103"    OR  "0105"    OR
                                           SPACE     OR  "0108"
                   MOVE    "OK"            TO  SPA-CID1-FLG
               ELSE
                   MOVE    "DL"            TO  SPA-CID1-FLG
               END-IF
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
      *
           PERFORM 210-BACK
           .
       4501-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑いフラグ編集処理
      *****************************************************************
       5001-UTAGAIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-UTAGAIFLG-MAX
                   IF      WRK-KBN (1:1) 
                                  =   SPA-GMN-C10-UTAGAIFLGL (IDY)
                       MOVE    SPA-GMN-C10-UTAGAIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-UTAGAIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-UTAGAIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性フラグ編集処理
      *****************************************************************
       5001-MANSEIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE 
               IF      WRK-KBN  =   "-"
                   MOVE    SPA-GMN-C10-MANSEIKBNMEIL (1)
                                          TO  WRK-KBNMEI
               ELSE
                   PERFORM VARYING IDY FROM   2   BY  1
                           UNTIL   IDY >      SPA-GMN-C10-MANSEIKBN-MAX
                       IF      WRK-KBN (1:2)
                                      =   SPA-GMN-C10-MANSEIKBNL (IDY)
                           MOVE    SPA-GMN-C10-MANSEIKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                           MOVE    SPA-GMN-C10-MANSEIKBN-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       5001-MANSEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分編集処理
      *****************************************************************
       5001-TENKIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-TENKIKBN-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C10-TENKIKBNL (IDY)
                       MOVE    SPA-GMN-C10-TENKIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-TENKIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病フラグ編集処理
      *****************************************************************
       5001-SYUBYOFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-SYUBYOFLG-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C10-SYUBYOFLGL (IDY)
                       MOVE    SPA-GMN-C10-SYUBYOFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-SYUBYOFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-NYUGAIKBN-MAX
                       OR      SPA-GMN-C10-NYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C10-NYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C10-NYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-NYUGAIKBN-MAX
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
      *    保険組合せ編集処理
      *****************************************************************
       5001-HKNCOMBI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
           MOVE    ZERO               TO  WRK-TEKEDYMD
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE 
               IF      WRK-KBN  =   "-"
                   MOVE    SPA-GMN-C10-HKNCOMBIMEIL (1)
                                          TO  WRK-KBNMEI
               ELSE
                   PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
                       IF      WRK-KBN        =
                                          SPA-GMN-C02-HKNCOMBIL (IDY)
                           MOVE    SPA-GMN-C02-HKNCOMBIMEIL (IDY)
                                          TO  WRK-KBNMEI
                           MOVE    SPA-NAI-C02-TEKSTYMD     (IDY)
                                          TO  WRK-TEKSTYMD
                           MOVE    SPA-NAI-C02-TEKEDYMD     (IDY)
                                          TO  WRK-TEKEDYMD
                           MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       5001-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為編集処理
      *****************************************************************
       5001-KOHID-HENSYU-SEC       SECTION.
      * 
           MOVE    ZERO                TO  WRK-RYOEDYMD
      *     
           IF      WRK-KBNMEI      =   SPACE
               CONTINUE
           ELSE
               IF      WRK-KBNMEI      =   "取消"
                   MOVE    SPA-GMN-C02-KOHIDL (1)
                                           TO  WRK-KBNMEI
                   MOVE    9999999999      TO  WRK-KBNID
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-C02-KOHID-MAX
                       IF      WRK-KBNMEI      =   SPA-GMN-C02-KOHIDL
                                                                  (IDY)
                           MOVE    SPA-NAI-C02-KOHIDL (IDY)
                                               TO  WRK-KBNID
                           MOVE    SPA-NAI-C02-RYOSTYMDL    (IDY)
                                               TO  WRK-RYOSTYMD
                           MOVE    SPA-NAI-C02-RYOEDYMDL    (IDY)
                                               TO  WRK-RYOEDYMD
                           MOVE    SPA-GMN-C02-KOHID-MAX
                                               TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       5001-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-SRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-SRYKAL (IDY)
                       MOVE    SPA-GMN-C02-SRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-SRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SRYKA-HENSYU-EXT.
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
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD3      NOT =   SPACE
               PERFORM 540-CIDSET3-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C10    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
           MOVE    SPACE               TO  C10
           INITIALIZE                      C10
           MOVE    SPA-GMN-C10-TENKIYMD       TO  C10-TENKIYMD
      *
           MOVE    SPA-GMN-C10-TENKIKBN-G TO  C10-TENKIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-TENKIKBN-MAX
               MOVE    SPA-GMN-C10-TENKIKBNLST (IDX)
                                           TO  C10-TENKIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-TENKIKBN-MAX
                                       TO  C10-TENKIKBN-COUNT
      *
           MOVE    SPA-GMN-C10-UTAGAIFLG-G TO  C10-UTAGAIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-UTAGAIFLG-MAX
               MOVE    SPA-GMN-C10-UTAGAIFLGLST (IDX)
                                           TO  C10-UTAGAIFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-UTAGAIFLG-MAX
                                       TO  C10-UTAGAIFLG-COUNT    
      *
           MOVE    SPA-GMN-C10-SYUBYOFLG-G TO  C10-SYUBYOFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-SYUBYOFLG-MAX
               MOVE    SPA-GMN-C10-SYUBYOFLGLST (IDX)
                                           TO  C10-SYUBYOFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-SYUBYOFLG-MAX
                                       TO  C10-SYUBYOFLG-COUNT  
      *
           MOVE    SPA-GMN-C10-MANSEIKBN-G TO  C10-MANSEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-MANSEIKBN-MAX
               MOVE    SPA-GMN-C10-MANSEIKBNLST (IDX)
                                           TO  C10-MANSEIFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-MANSEIKBN-MAX
                                       TO  C10-MANSEIFLG-COUNT
      *
           MOVE    SPA-GMN-C10-NYUGAIKBN-G TO  C10-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-NYUGAIKBN-MAX
               MOVE    SPA-GMN-C10-NYUGAIKBNLST (IDX)
                                           TO  C10-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-NYUGAIKBN-MAX 
                                       TO  C10-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-C10-HKNCOMBI-G TO  C10-HKNCOMBI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-HKNCOMBI-MAX
               MOVE    SPA-GMN-C10-HKNCOMBILST (IDX)
                                           TO  C10-HKNCOMBI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-HKNCOMBI-MAX
                                       TO  C10-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-C10-KOHID   TO  C10-KOHID
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C10-KOHID-MAX
               MOVE    SPA-GMN-C10-KOHIDLST (IDX)
                                           TO  C10-KOHID-ITEM (IDX)
               MOVE    IDX                 TO  C10-KOHID-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C10-SRYYMD  TO  C10-SRYYMD
      *
           MOVE    SPA-GMN-C10-SRYKA-G TO  C10-SRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-SRYKA-MAX
               MOVE    SPA-GMN-C10-SRYKALST (IDX)
                                           TO  C10-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-SRYKA-MAX
                                       TO  C10-SRYKA-COUNT
      *
           IF      SPA-GMN-C10-SRYKA-MAX
                                       =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  C10-SRYKA-STATE
                                               C10-B08-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  C10-SRYKA-STATE
                                               C10-B08-STATE
           END-IF
      *
           IF      SPA-NAI-C02-KOHID-DAISANSYA
                                       =   1
               MOVE    WIDGET-NORMAL       TO  C10-KOHIDCOMBO-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  C10-KOHIDCOMBO-STATE
           END-IF
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
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-C10-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-C10-CUR
             WHEN    1
                     MOVE  "TENKIKBN"   TO  MCP-WIDGET
             WHEN    2
                     MOVE  "TENKIYMD"   TO  MCP-WIDGET
             WHEN    3
                     MOVE  "UTAGAIFLG"  TO  MCP-WIDGET
             WHEN    4
                     MOVE  "SYUBYOFLG"  TO  MCP-WIDGET
             WHEN    5
                     MOVE  "MANSEIFLG"  TO  MCP-WIDGET
             WHEN    6
                     MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    7
                     MOVE  "HKNCOMBI"   TO  MCP-WIDGET
             WHEN    8
                     MOVE  "SRYYMD"     TO  MCP-WIDGET
             WHEN    9
                     MOVE  "SRYKA"      TO  MCP-WIDGET
             WHEN    10
                     MOVE  "KOHID"      TO  MCP-WIDGET
             WHEN    99
                     MOVE  "B12"        TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "TENKIKBN"
                   IF      SPA-GMN-C10-TENKIKBN    =   SPACE   OR  "-"
                       MOVE    3           TO  SPA-GMN-C10-CUR
                   ELSE
                       MOVE    2           TO  SPA-GMN-C10-CUR
                   END-IF 
               WHEN    "TENKIYMD"
                   MOVE    3                   TO  SPA-GMN-C10-CUR
               WHEN    "UTAGAIFLG"
                   MOVE    4                   TO  SPA-GMN-C10-CUR
               WHEN    "SYUBYOFLG"
                   MOVE    5                   TO  SPA-GMN-C10-CUR
               WHEN    "MANSEIFLG"
                   MOVE    6                   TO  SPA-GMN-C10-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    7                   TO  SPA-GMN-C10-CUR
               WHEN    "HKNCOMBI"
                   IF      SPA-NAI-C02-KOHID-DAISANSYA
                                               =   ZERO
                       MOVE    8                   TO  SPA-GMN-C10-CUR
                   ELSE
                       MOVE    10                  TO  SPA-GMN-C10-CUR
                   END-IF
               WHEN    "KOHID"
                   MOVE    8                   TO  SPA-GMN-C10-CUR
               WHEN    "SRYYMD"
               WHEN    "B10"
                   IF      SPA-GMN-C10-SRYYMD  =   SPACE
                       IF      SPA-GMN-C10-SRYKA-MAX   =   ZERO 
                           MOVE    99          TO  SPA-GMN-C10-CUR
                       ELSE
                           MOVE    9           TO  SPA-GMN-C10-CUR
                       END-IF 
                   ELSE
                       MOVE    3           TO  SPA-GMN-C10-CUR
                   END-IF 
               WHEN    "SRYKA"
                   IF      SPA-GMN-C10-SRYKA   =   SPACE
                       MOVE    99          TO  SPA-GMN-C10-CUR
                   ELSE
                       MOVE    3           TO  SPA-GMN-C10-CUR
                   END-IF 
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C10-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。"  DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING
                       "開始日が"              DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "の"                    DELIMITED   BY  SIZE 
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "が転帰済でありません"
                                               DELIMITED   BY  SIZE  
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   MOVE
                       "入力した開始日では該当の保険適用は範囲外です"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
                       "開始日と転帰日・転帰区分は同時に入力できません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は同一診療科で複数選択してあるので"
                                               DELIMITED   BY  SIZE  
                       "複写処理はできません"
                                               DELIMITED   BY  SIZE  
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0009"
                   MOVE    "該当の保険組合せがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
               MOVE "開始日が該当の保険組合せの適用日の範囲外です"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "診療科目入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "年号入力エラー"            TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "数字で入力してください"    TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "転帰日入力エラー"          TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "開始日入力エラー"          TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "疑い入力エラー"            TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "主病名入力エラー"          TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "慢性区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "転帰区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "転帰年月日が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "転帰年月日が入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "開始日以前の転帰日付が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "診療科と他の項目は同時に入力できません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "転帰区分が入力してあります"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "転帰区分が入力してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0038"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。転帰日等を確認して下さい"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0083"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は既に保険適用または第三者行為の指定が" 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "行われています"        DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0084"
                 MOVE
                     "入力した転帰日以降の診療開始日の病名が指定"
                                                   TO  SPA-ERRMSG
                 MOVE
                     "してあります"                TO  SPA-ERRMSG (43:)
               WHEN    "0085"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は開始日が"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "ですので該当の保険適用は範囲外です"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0086"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は開始日が"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "ですので該当の第三者行為は範囲外です"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0096"
                   MOVE    "誕生日前の開始日は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0097"
                   MOVE    "該当の第三者行為がありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0098"
                   MOVE
                       "開始日が該当の第三者行為の適用日の範囲外です"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE
                       "保険組合せと第三者行為の両方の入力はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0104"
                   STRING
                       "この病名は「急性」と「慢性」が混在して"
                                                DELIMITED   BY  SIZE 
                       "レセプトに記載されます。確認して下さい。"
                                                DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  CERR
           EVALUATE    SPA-ERRCD
               WHEN    "0104"
                   MOVE    WRK-ERR-BYOMEI  TO  CERR-BYOMEI
           END-EVALUATE 
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C10"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           INITIALIZE                      CID1
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0101"
                   MOVE    "選択された病名を削除しますか？"
                                        TO  CID1-ID1MSG
               WHEN    "0103"
                   STRING
                       "この病名は"            DELIMITED   BY  SIZE 
                       SPA-C02-KEIKOKU-SRYKA   DELIMITED   BY  SPACE
                       "に"                    DELIMITED   BY  SIZE 
                       SPA-C02-KEIKOKU-SRYYMD  DELIMITED   BY  SPACE
                       "に登録してあります。更新しますか？"
                                               DELIMITED   BY  SIZE 
                                               INTO    CID1-ID1MSG
                   END-STRING
               WHEN    "0105"
                   STRING  "転帰区分の設定により患者情報の死亡区分を"
                                               DELIMITED   BY  SIZE 
                           "「死亡」として更新します。よろしいですか"
                                               DELIMITED   BY  SIZE 
                                               INTO    CID1-ID1MSG
                   END-STRING
               WHEN    "0108"
                   STRING  "転帰区分の変更により患者情報の死亡区分を"
                                                DELIMITED   BY  SIZE 
                           "「生存」として更新します。よろしいですか"
                                                DELIMITED   BY  SIZE 
                                               INTO    CID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  CID1-ID1MSG
           END-EVALUATE
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0103"
                   MOVE    SPA-C02-KEIKOKU-BYOMEI
                                       TO  CID1-BYOMEI
           END-EVALUATE 
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C10"               TO  SPA-MOTOPG
           MOVE    "CID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面３表示処理
      *****************************************************************
       540-CIDSET3-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-CIDMSG
      *
           EVALUATE    SPA-CIDCD3
               WHEN    "0102"
                   STRING  "診療科による処理を次のいずれかの方法により"
                                       DELIMITED  BY  SIZE
                           "選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-CIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID3-FLG
      *
           MOVE    SPACE               TO  CID3
           INITIALIZE                      CID3
           MOVE    SPA-CIDCD3          TO  CID3-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID3-MSG
           MOVE    "F11:選択した診療科に移動"  
                                       TO  CID3-B11-VALUE  
           MOVE    "F12:選択した診療科に複写" 
                                       TO  CID3-B12-VALUE  
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C10"               TO  SPA-MOTOPG
           MOVE    "CID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-CIDSET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                      PTBYOMEI-REC
           END-IF
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-INV-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-COMB-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
