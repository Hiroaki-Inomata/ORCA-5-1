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
       PROGRAM-ID.             ORCGC04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 自院病名検索（Ｃ０４）
      *  管理者            : 
      *  01/12/11    MCC−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/05/08  半角チェック追加
      *  01.00.02    MCC-多々納   02/05/08  半角チェック時、半角空白を
      *                                     全角空白へ置換えの追加
      *  01.00.03    MCC-藤原     02/05/17  コラムリスト700に変更
      *
      *  01.01.01    NACL-藤原    02/08/23  新傷病名マスタ対応
      *  01.01.02    NACL-藤原    02/11/18  分類名の選択番号のチェック修正
      *
      *  01.02.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *                                     病名種別の追加
      *  01.02.02    NACL-藤原    04/02/25  分類毎の表示の前次頁の処理
      *  ver.2.6.0
      *  01.02.03    NACL-藤原    05/04/20  移行病名警告メッセージの表示、
      *                                     廃止年月日警告メッセージ表示
      *                                     をシス管の設定より行う
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    06/03/13  病名検索時の修正
      *
      *  02.08.01    NACL-藤原    06/02/01  自院病名コード検索追加
      *  02.08.02    NACL-藤原    06/03/30  複数病名検索時のまとめかたの修正
      *
      *  03.00.01    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.01.01    NACL-藤原    06/07/03   同一病名のときのエラーメッセ
      *                                      ージ(0002)を変更
      *  03.01.02    NACL-藤原    06/07/05   入力したコードにエラーがある
      *                                      とき確定で戻らないようにする
      *  03.01.03    NACL-藤原    06/07/07   移行先病名で遷移したとき移行
      *                                      前の病名に戻らないようにする
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.06.01    NACL-藤原    10/04/28  自院病名に不明な病名コードが
      *                                     登録された場合のチェックを追加
      *  04.06.02    NACL-藤原    10/10/26  同義語変換対応
      *  04.06.03    NACL-藤原    10/11/04  補足コメント対応
      *  04.06.04    NACL-藤原    11/10/11  同義語変換時の編集病名フラグ
      *                                     の設定
      *  04.06.05    NACL-藤原    11/12/19  自院病名検索時のチェックと
      *                                     自院病名検索から戻り時の表示の修正
      *  04.06.06    NACL-藤原    12/07/02  検索対象が１件のときＥＮＴＥ
      *                                     Ｒのみで確定可能にする
      *
      *  04.07.01    NACL-藤原    11/06/10  未コード化病名となる病名の別
      *                                     組み立てへの移行を可能にする
      *  04.07.02    NACL-藤原    13/03/26  システム管理がチェックしない
      *                                     設定時の推奨病名チェックの修正
      *  04.07.03    NACL-藤原    13/07/30  自院病名のセット登録対応
      *  04.07.04    NACL-藤原    14/05/20  セット登録選択時のコード置換え
      *                                     画面表示後の初期化
      *
      *  04.08.01    NACL-藤原    16/02/10  自院病名への補足コメント追加
      *                                     対応
      *  04.08.02    NACL-藤原    16/03/17  セット入力時のエラー表示修正
      *
      *  05.00.01    NACL-藤原    18/11/22  Warning 対応
      ****************************************************************
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
           03  FLG-BYOMEIINPUTSET  PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).   
           03  FLG-ROW             PIC 9(01).  
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDWW                PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM             PIC ZZ9.
           03  WRK-SELNO           PIC 9(03). 
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(07).
               05  WRK-WIDGET2     PIC 9(01).  
               05  WRK-WIDGET3     PIC 9(01).  
               05  WRK-WIDGET4     PIC 9(01).  
           03  WRK-BYOMEI          PIC X(400).
           03  WRK-BYOMEI1         PIC X(800).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-STR             PIC 9(05).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-BYOSET-MCP-PATHNAME
                                   PIC X(64).
      *     
           03  WRK-HAISIYMDH       PIC X(22).
      * 
           03  WRK-SYOKBN          PIC 9(01).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-SEIGYO          PIC X(01). 
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
      * 
           03  WRK-C04-BYO-ROW     PIC S9(09). 
      *
           03  WRK-CIDMSG          PIC X(80).
      *
           03  WRK-BYOMEI-MAX1     PIC 9(02).
           03  WRK-BYOMEI-MAX2     PIC 9(02).
           03  WRK-BYOMEI-INDEX    PIC 9(02).
      *
      *    退避エリア
           03  WRK-NAI-C04-BYOMEI-AREA.
               05  WRK-NAI-C04-BYOMEI-OCC  OCCURS  6.
                   07  WRK-NAI-C04-BYOMEICD    PIC X(10).
                   07  WRK-NAI-C04-BYOMEITOP   PIC X(01).
                   07  WRK-NAI-C04-BYOMEISBT   PIC X(01).
                   07  WRK-NAI-C04-BYOMEI-G.
                       09  WRK-NAI-C04-BYOMEI  PIC X(80).
                       09  WRK-NAI-C04-BYOMEICD-O
                                               PIC X(07)
                                                   OCCURS  21.
                       09  WRK-NAI-C04-BYOMEI-O
                                               PIC X(80)
                                                   OCCURS  21.
                   07  WRK-NAI-C04-BYOMEI-CHG  PIC X(03).
      *
           03  WRK-GMN-C04-INPUTCD-G.
               05  WRK-GMN-C04-INPUTCD         PIC X(10)
                                                   OCCURS  6.
      *
       01  WRK-TBL-MAX             PIC 9(03)   VALUE   100.
      *
      *    未コード化傷病名コード
       01  WRK-CONS-BYOMEICD       PIC X(07)   VALUE   "0000999". 
      *
      *    患者病名マスタ−（ワーク）
       01  WRK-BYOMEI-AREA.
           03  WRK-BYOMEI-O        PIC X(100)  OCCURS  6.
      *
      *    ワーク病名マスタ
       01  WRK-BYOMEI-REC.
           COPY    "CPBYOMEI.INC"  REPLACING   //BYO-//
                                   BY          //WRK-BYO-//.
      *
       01  SPAN-COLOR-AREA.
           03  SPAN-COLOR-RED      PIC X(18)    VALUE
                                                "<span color=""red"">".  
           03  SPAN-COLOR-BLUE     PIC X(19)    VALUE
                                                "<span color=""blue"">".  
           03  SPAN-COLOR-GREEN    PIC X(20)    VALUE
                                               "<span color=""green"">".  
           03  SPAN-COLOR-END      PIC X(07)    VALUE
                                               "</span>".  
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
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    自院病名マスタ
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    病名マスタ
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    同義語マスタ
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    病名入力セット
       01  BYOMEIINPUTSET-REC.
           COPY    "CPBYOMEIINPUTSET.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *****************************************************************
      *    連絡領域
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
      *    主処理
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
      *
               INITIALIZE                  SPA-CIDCD-AREA
                                           SPA-CID1-AREA
               INITIALIZE                  SPA-C04-IKOSAKI-AREA
                                           SPA-C04-CHANGE-AREA
                                           SPA-ERRCD
                                           WRK-HAISIYMDH                                           
      *
               IF      SPA-C04-BYOSET-INDEX-MAX
                                       >   ZERO
      *            病名入力セットから自院病名編集処理
                   PERFORM 41002111-BYOMEIINPUTSET-HEN-SEC
                   IF    ( SPA-CIDCD2      NOT =   SPACE )
                   OR    ( SPA-CIDCD6      NOT =   SPACE )
                   OR    ( SPA-ERRCD       NOT =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SPA-C01-INDEX   =   ZERO
                           ADD     1               TO  SPA-GMN-C04-CUR
                           IF      SPA-GMN-C04-CUR >   8
                               MOVE    2           TO  SPA-GMN-C04-CUR
                           END-IF
                       END-IF
                   END-IF
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
                   IF      SPA-CIDCD2      NOT =   SPACE
                       PERFORM 530-CIDSET2-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
                   IF      SPA-CIDCD6      NOT =   SPACE
                       PERFORM 560-CIDSET6-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
               END-IF
      *
               IF    ( SPA-C01-ERRCD       =   "0075"  OR  "0094" )
               AND   ( SPA-C01-INDEX       >   ZERO               )
                   MOVE    SPACE           TO  SPA-C01-ERRCD
                   PERFORM 450-KAKUTEI-SEC
                   IF      SPA-ERRCD           =   SPACE
                       GO  TO  100-INIT-EXT
                   END-IF
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
      *
               END-IF
               PERFORM 5001-MAPCUR-SEC
               MOVE    SPACE           TO  SPA-C01-ERRCD
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-CIDCD2      NOT =   SPACE
                   PERFORM 530-CIDSET2-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-CIDCD6      NOT =   SPACE
                   PERFORM 560-CIDSET6-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C04    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID2"
               MOVE    SPACE               TO  SPA-NAI-C04-BYOMEI-CHG
                                                 (SPA-C04-IKOSAKI-INDEX)
      *
               IF      SPA-CID2-FLG    =   "OK"
                   MOVE    SPA-C04-IKOSAKI-INDEX
                                               TO  IDZ
                   MOVE    SPA-C04-IKOSAKI-INDEX1
                                               TO  IDX2
                   MOVE    SPA-CID2-FLG        TO  
                                           SPA-NAI-C04-BYOMEI-CHG (IDZ)
                   INITIALIZE              SPA-NAI-C04-BYOMEI (IDZ)
                                           SPA-NAI-C04-BYOMEICD-O
                                                              (IDZ IDX2)
                                           SPA-NAI-C04-BYOMEI-O
                                                              (IDZ IDX2)
                                            
      *
                   MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                   MOVE     SPA-C04-IKOSAKI-BYOMEI TO
                                       SPA-NAI-C04-BYOMEI-O   (IDZ IDX2)
                   MOVE    SPA-NAI-C04-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C04-BYOMEI (IDZ)
                   PERFORM VARYING IDX2    FROM    2   BY  1
                           UNTIL   IDX2    >       21
                           OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                                               =   SPACE
                       STRING   SPA-NAI-C04-BYOMEI (IDZ)
                                           DELIMITED  BY  SPACE
                                SPA-NAI-C04-BYOMEI-O (IDZ IDX2)
                                           DELIMITED  BY  SPACE
                                    INTO    SPA-NAI-C04-BYOMEI (IDZ)
                       END-STRING
                   END-PERFORM        
               END-IF
      *
               INITIALIZE                  SPA-CIDCD-AREA
                                           SPA-CID1-AREA
               INITIALIZE                  SPA-C04-IKOSAKI-AREA
                                           SPA-C04-CHANGE-AREA
                                           SPA-ERRCD
                                           WRK-HAISIYMDH
      *
               IF      SPA-C04-BYOSET-INDEX-MAX
                                       >   ZERO
      *            病名入力セットから自院病名編集処理
                   PERFORM 41002111-BYOMEIINPUTSET-HEN-SEC
                   IF    ( SPA-CIDCD2      NOT =   SPACE )
                   OR    ( SPA-CIDCD6      NOT =   SPACE )
                   OR    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  300-SCREEN-EXT
                   END-IF
               END-IF
               IF      SPA-C01-INDEX   =   ZERO
                   PERFORM 410022-BYOMEI-HENSYU-SEC
               ELSE
                   PERFORM 410023-BYOMEI-HENSYU-SEC
               END-IF
      *
               IF      SPA-C01-INDEX   =   ZERO
                   ADD     1                   TO  SPA-GMN-C04-CUR
                   IF      SPA-GMN-C04-CUR     >   8
                       MOVE    2                   TO  SPA-GMN-C04-CUR
                   END-IF
               ELSE
                   PERFORM 450-KAKUTEI-SEC
               END-IF
               INITIALIZE                  SPA-C04-CHANGE-AREA
               GO      TO      300-SCREEN-EXT
           END-IF
      *
      *    病名移行確認画面より
           IF      SPA-MOTOPG(1:4)     =   "CID6"
               PERFORM 370-CID6-SET-SEC
               GO  TO  300-SCREEN-EXT
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
           INITIALIZE              SPA-C04
      *
      *    検索対象設定
           IF      SPA-C01-WIDGET      =   SPACE
               MOVE    "BYOMEICD"          TO  SPA-NAI-C04-B10-WIDGET
           ELSE
               MOVE    SPA-C01-WIDGET      TO  SPA-NAI-C04-B10-WIDGET
           END-IF
      *
      *    病名編集
           IF      SPA-C01-MOTOPG    =   "C02"
             PERFORM VARYING IDX     FROM    1   BY  1
                     UNTIL   IDX     >       6
               IF      SPA-C01-TBYOMEICD(IDX)  NOT =   SPACE
                 MOVE    SPA-C01-TBYOMEICD(IDX)
                                       TO  SPA-GMN-C04-INPUTCD(IDX)
                                           SPA-NAI-C04-BYOMEICD(IDX)
      *
                 IF      SPA-C01-TUSERBYOMEIFLG (IDX)    =   "1"
                     PERFORM VARYING IDZ FROM    1   BY  1
                             UNTIL   IDZ >       6
                         IF      SPA-C01-TBYOMEICD-INDEX (IDX)
                                                       =   IDZ
                               PERFORM VARYING IDX3    FROM    1   BY  1
                                       UNTIL   IDX3    >       21
                                       OR      SPA-NAI-C02-BYOMEICD-O
                                                              (IDZ IDX3)
                                                       =   SPACE
                                       OR      SPA-NAI-C02-BYOMEICD-O
                                                              (IDZ IDX3)
                                                       =   ALL "9"
                                   MOVE
                                       SPA-NAI-C02-BYOMEICD-O (IDZ IDX3)
                                           TO  SPA-NAI-C04-BYOMEICD-O
                                                              (IDX IDX3)
                                   MOVE
                                       SPA-NAI-C02-BYOMEI-O   (IDZ IDX3)
                                           TO  SPA-NAI-C04-BYOMEI-O
                                                              (IDX IDX3)
                               END-PERFORM    
                               MOVE    SPA-NAI-C02-BYOMEI  (IDZ)
                                           TO  SPA-NAI-C04-BYOMEI (IDX)
      *******                  MOVE    SPA-NAI-C02-BYOMEITOP  (IDZ)
      *******                              TO SPA-NAI-C04-BYOMEITOP(IDX)
                               MOVE    SPA-NAI-C02-BYOMEISBT  (IDZ)
                                           TO SPA-NAI-C04-BYOMEISBT(IDX)
                               MOVE    SPA-NAI-C02-BYOMEI-CHG (IDZ)
                                           TO SPA-NAI-C04-BYOMEI-CHG
                                                                   (IDX)
                               MOVE    6   TO  IDZ
                         END-IF
                     END-PERFORM 
                 ELSE
                     MOVE    SPA-C01-TBYOMEI  (IDX)
                                       TO  SPA-NAI-C04-BYOMEI    (IDX)
                 END-IF
               END-IF            
             END-PERFORM
           ELSE
      *      セット登録画面より遷移
             PERFORM VARYING IDX     FROM    1   BY  1
                     UNTIL   IDX     >       6
               IF      SPA-C01-TBYOMEICD(IDX)  NOT =   SPACE
                   MOVE    SPA-C01-TBYOMEICD(IDX)
                                           TO  SPA-GMN-C04-INPUTCD (IDX)
                                           SPA-NAI-C04-BYOMEICD(IDX)
                   MOVE    SPA-C01-TBYOMEISBT  (IDX)
                                           TO SPA-NAI-C04-BYOMEISBT(IDX)
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       6
                       IF      SPA-C01-TBYOMEICD-INDEX (IDX)
                                                   =   IDZ
                           MOVE    SPA-GMN-C06-BYOMEI  (IDZ)
                                           TO  SPA-NAI-C04-BYOMEI (IDX)
                           PERFORM VARYING IDX3    FROM    1   BY  1
                                   UNTIL   IDX3    >       21
                                   OR      SPA-NAI-C06-BYOMEICD-O
                                                          (IDZ IDX3)
                                                   =   SPACE
                                   OR      SPA-NAI-C06-BYOMEICD-O
                                                          (IDZ IDX3)
                                                   =   ALL "9"
                               MOVE    SPA-NAI-C06-BYOMEICD-O (IDZ IDX3)
                                           TO  SPA-NAI-C04-BYOMEICD-O
                                                              (IDX IDX3)
                               MOVE    SPA-NAI-C06-BYOMEI-O   (IDZ IDX3)
                                           TO  SPA-NAI-C04-BYOMEI-O
                                                              (IDX IDX3)
                           END-PERFORM    
                           MOVE    6   TO  IDZ
                       END-IF
                   END-PERFORM 
               END-IF
             END-PERFORM
           END-IF
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 3100-BYOMEI-HENSYU-SEC
      *   
      *    補足コメント編集
           IF      SPA-C01-HOSOKU-COMMENT  =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       3
                   IF      SPA-C01-THOSOKUCD(IDX)  NOT =   SPACE
                       MOVE    SPA-C01-THOSOKUCD(IDX)
                                       TO  SPA-GMN-C04-HOSOKUCD (IDX)
                       MOVE    SPA-C01-THOSOKU-COMMENT(IDX)
                                       TO  SPA-NAI-C04-HOSOKU-COMMENT
                                                                (IDX)
                   END-IF
               END-PERFORM
               PERFORM 3103-HOSOKU-COMMENT-HENSYU-SEC
           ELSE
               MOVE    SPA-C01-HOSOKU-COMMENT
                                       TO  SPA-GMN-C04-HOSOKU-COMMENT
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
      *        分類一覧
               PERFORM 3102-BUNRUI-LST-SEC
      *
      *        自院病名一覧
               MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
      *        分類が選択されている場合
               IF      SPA-C01-BUNRUICD    >   ZERO
                   MOVE    SPA-C01-BUNRUICD    TO  C04-BUNRUICD
                   PERFORM 41001-BUNRUICD-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    ZERO            TO  SPA-GMN-C04-BUNRUICD
                   END-IF
               END-IF
               IF      SPA-GMN-C04-BUNRUICD    =   ZERO
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key3"              TO  WRK-MCP-PATHNAME
                   PERFORM 3101-USERBYOMEI-LST-SEC
                   MOVE    9                   TO  SPA-GMN-C04-CUR
               END-IF
           ELSE
               MOVE    SPA-C01-BYOMEIINPUTCD
                                       TO  SPA-GMN-C04-KENBYOMEI
      *
      *        自院病名一覧
               MOVE    1                   TO  SPA-GMN-C04-PAGE
               INITIALIZE                      USERBYOMEI-REC
               STRING  SPA-C01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                       "%"                     DELIMITED   BY  SIZE
                                           INTO    USBYO-BYOMEIINPUTCD
               END-STRING
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key11"             TO  WRK-MCP-PATHNAME
               PERFORM 3101-USERBYOMEI-LST-SEC
               MOVE    2                   TO  SPA-GMN-C04-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3100-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名コード数
           MOVE    ZERO            TO  SPA-NAI-C04-BYOMEICD-MAX
                                       SPA-NAI-C04-BYOMEISU-MAX
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)   NOT =   SPACE
                   ADD     1               TO  SPA-NAI-C04-BYOMEICD-MAX
                   IF      SPA-NAI-C04-BYOMEITOP (IDX) =   "1"
                       ADD     1           TO  SPA-NAI-C04-BYOMEISU-MAX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    病名編集
           MOVE    SPACE                   TO  WRK-BYOMEI-AREA
           MOVE    ZERO                    TO  IDY
      *
           MOVE    SPA-NAI-C04-BYOMEI (1)  TO  WRK-BYOMEI
      *     
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)
                                   NOT =   SPACE
                   IF      SPA-NAI-C04-BYOMEITOP (IDX) =   "1"
                       IF      WRK-BYOMEI  NOT =   SPACE
                           ADD     1           TO  IDY
                           MOVE    WRK-BYOMEI  TO  WRK-BYOMEI-O (IDY)
                       END-IF
                       MOVE    SPACE           TO  WRK-BYOMEI
                   END-IF
                   STRING  WRK-BYOMEI              DELIMITED   BY  SPACE
                           SPA-NAI-C04-BYOMEI(IDX) DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           IF      WRK-BYOMEI      NOT =   SPACE
               ADD     1               TO  IDY
               MOVE    WRK-BYOMEI      TO  WRK-BYOMEI-O (IDY)
           END-IF
      *
           MOVE   WRK-BYOMEI-O (1)     TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
                   OR      WRK-BYOMEI-O (IDX)  =   SPACE     
               STRING  WRK-BYOMEI      DELIMITED  BY  SPACE
                   "／"                DELIMITED  BY  SIZE
                   WRK-BYOMEI-O (IDX)  DELIMITED  BY  SPACE
                                 INTO             WRK-BYOMEI
               END-STRING
           END-PERFORM
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C04-BYOMEI
           .
       3100-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名の先頭フラグ編集処理
      *****************************************************************
       3101-BYOMEITOP-HENSYU-SEC      SECTION.
      *
           IF      SPA-GMN-C04-INPUTCD (IDX) 
                                     =   SPACE
               MOVE    SPACE       TO  SPA-NAI-C04-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
           IF      IDX               =   1  
               MOVE    "1"               TO  SPA-NAI-C04-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
      *    自院病名の部位が入力されている場合は複数病名にはしない
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       6
               IF      SPA-NAI-C04-BYOMEISBT(IDW)
                                           =   "1"
                   MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                   GO  TO  3101-BYOMEITOP-HENSYU-EXT
               END-IF
           END-PERFORM
      *
      *    存在しない自院病名のとき
           IF      SPA-NAI-C04-BYOMEI (IDX)
                                   =   "■■■"
               MOVE    "1"             TO  SPA-NAI-C04-BYOMEITOP(IDX) 
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
           COMPUTE IDW     =   IDX     -   1
           IF    ( SPA-GMN-C04-INPUTCD (IDW)
                                       =   SPACE )
           AND   ( IDW                 >   1     )
               PERFORM VARYING IDW1    FROM    IDX     BY  -1
                       UNTIL   IDW1    <       1
                   IF      IDW1            =   IDX
                       CONTINUE
                   ELSE 
                       IF      SPA-GMN-C04-INPUTCD (IDW1)
                                               NOT =   SPACE
                           MOVE    IDW1            TO  IDW
                           MOVE    1               TO  IDW1
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    前が存在しない自院病名のとき
           IF      SPA-NAI-C04-BYOMEI (IDW)
                                   =   "■■■"
               MOVE    "1"             TO  SPA-NAI-C04-BYOMEITOP(IDX) 
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-C04-BYOMEISBT(IDX)
               WHEN    "8"
                   MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
               WHEN    "7"
               WHEN    "0"
                   EVALUATE    SPA-NAI-C04-BYOMEISBT (IDW)
                       WHEN    "7"
                       WHEN    "1"
                           MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                       WHEN    "8"
                       WHEN    "0"
                           MOVE    "1"         TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                   END-EVALUATE
               WHEN    "1"
                   MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
           END-EVALUATE
           GO TO  3101-BYOMEITOP-HENSYU-EXT
      *
      *   自院病名の部位が入力されている場合は複数病名にはしない
           IF      SPA-C01-INDEX   =   ZERO
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW >       6
                   IF      SPA-NAI-C04-BYOMEISBT(IDW)
                                               =   "1"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                       GO  TO  3101-BYOMEITOP-HENSYU-EXT
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW >       6
                   IF    ( SPA-C01-TUSERBYOMEIFLG (IDW)
                                               =   "1" )
                   AND   ( SPA-NAI-C04-BYOMEISBT(IDW)
                                               =   "1" )
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                       GO  TO  3101-BYOMEITOP-HENSYU-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
           COMPUTE IDW     =   IDX     -   1
           IF      SPA-C01-INDEX   =   ZERO
               EVALUATE    SPA-NAI-C04-BYOMEISBT(IDX)
                   WHEN    "8"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                   WHEN    "7"
                   WHEN    "0"
                       EVALUATE    SPA-NAI-C04-BYOMEISBT (IDW)
                           WHEN    "7"
                           WHEN    "1"
                               MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                           WHEN    "8"
                           WHEN    "0"
                               MOVE    "1"         TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                       END-EVALUATE
                   WHEN    "1"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
               END-EVALUATE
           ELSE
             IF      SPA-C01-TUSERBYOMEIFLG (IDX)
                                       =   "1"
               EVALUATE    SPA-NAI-C04-BYOMEISBT(IDX)
                   WHEN    "8"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                   WHEN    "7"
                   WHEN    "0"
                       IF      SPA-C01-TUSERBYOMEIFLG (IDW)
                                                       =   "1"
                           EVALUATE    SPA-NAI-C04-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"         TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                           END-EVALUATE
                       ELSE
                           EVALUATE    SPA-GMN-C04-INPUTCD (IDW)(1:1)
                                ALSO   SPA-GMN-C04-INPUTCD (IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                             END-EVALUATE
                           END-IF
                   WHEN    "1"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
               END-EVALUATE
             ELSE
               EVALUATE    SPA-GMN-C04-INPUTCD (IDX)(1:1)
                           ALSO   SPA-GMN-C04-INPUTCD (IDX)(4:1) 
                   WHEN    "Z"             ALSO    "8"
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
                   WHEN    "Z"             ALSO    "1"    THRU    "7"
                   WHEN    "0" THRU    "9" ALSO    ANY
                       IF      SPA-C01-TUSERBYOMEIFLG (IDW)
                                                       =   "1"
                           EVALUATE    SPA-NAI-C04-BYOMEISBT (IDW)
                               WHEN    "7"
                               WHEN    "1"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "8"
                               WHEN    "0"
                                   MOVE    "1"     TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                           END-EVALUATE
                       ELSE
                           EVALUATE    SPA-GMN-C04-INPUTCD (IDW)(1:1)
                                   ALSO    SPA-GMN-C04-INPUTCD(IDW)(4:1)
                               WHEN    "Z" ALSO    "1"    THRU    "7"
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "Z" ALSO    "8"
                                   MOVE    "1"         TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    "Z" ALSO    ANY
                                   MOVE    SPACE       TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                               WHEN    OTHER
                                   MOVE    "1"     TO
                                           SPA-NAI-C04-BYOMEITOP(IDX)
                             END-EVALUATE
                         END-IF
                   WHEN    "Z"                 ALSO    ANY
                       MOVE    SPACE   TO  SPA-NAI-C04-BYOMEITOP(IDX)
               END-EVALUATE    
             END-IF
           END-IF    
           .
       3101-BYOMEITOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメント編集処理
      *****************************************************************
       3103-HOSOKU-COMMENT-HENSYU-SEC  SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-C04-HOSOKU-COMMENT
                                       WRK-BYOMEI
      *     
           STRING  SPA-NAI-C04-HOSOKU-COMMENT (1)
                                    DELIMITED   BY  SPACE
                   SPA-NAI-C04-HOSOKU-COMMENT (2)
                                    DELIMITED   BY  SPACE
                   SPA-NAI-C04-HOSOKU-COMMENT (3)
                                    DELIMITED   BY  SPACE    
                                       INTO    WRK-BYOMEI
                                           
           END-STRING
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C04-HOSOKU-COMMENT
           .
       3103-HOSOKU-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類一覧 編集処理
      *****************************************************************
       3102-BUNRUI-LST-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
      *
      *    分類コード ’Ｃ’のもの
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           PERFORM UNTIL       (FLG-USERBYOMEI   =   1    )  OR
                               (IDX              >=  100  )
      *
               ADD     1                   TO  IDX
               IF      USBYO-BYOMEIINPUTCD NOT =   SPACE
                   MOVE    USBYO-BYOMEIINPUTCD(1:3)
                                       TO  SPA-GMN-C04-BBUNRUICD(IDX)
               END-IF                            
               MOVE    USBYO-BYOMEI    TO  SPA-GMN-C04-BBUNRUIMEI(IDX)
      *
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-GMN-C04-BUNRUI-MAX
           .
       3102-BUNRUI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧 編集処理
      *****************************************************************
       3101-USERBYOMEI-LST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-C04-BYOMEI-TBL
                                       SPA-NAI-C04-BYOMEI-TBL
                                       SPA-GMN-C04-SELNUM 
           MOVE    ZERO                TO  SPA-GMN-C04-MAX
      *
           COMPUTE WRK-STR         = ( SPA-GMN-C04-PAGE  -  1 )
                                   *   WRK-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
      *
           IF    ( SPA-C01-MOTOPG      =   "C02"                 )
           AND   ( SPA-GMN-C04-BUNRUICD
                                       =   ZERO                  )
           AND   ( SPA-GMN-C04-KENBYOMEI
                                       =   SPA-C01-BYOMEIINPUTCD )
           AND   ( SPA-NAI-C04-B10-WIDGET
                                   NOT =   "HOSOKUCD"            )
      *        病名入力セット編集
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               IF      SPA-C01-BYOMEIINPUTCD
                                       NOT =   SPACE
                   STRING  SPA-C01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                           "%"                     DELIMITED   BY  SIZE
                                           INTO    BYOSET-BYOMEIINPUTSET
                   END-STRING
                   MOVE    "key3"          TO  WRK-BYOSET-MCP-PATHNAME
               ELSE
                   MOVE    "key2"          TO  WRK-BYOSET-MCP-PATHNAME
               END-IF
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    WRK-BYOSET-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_byomeiinputset"
                                               TO  MCP-TABLE
                   MOVE    WRK-BYOSET-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
               ELSE
                   INITIALIZE                  BYOMEIINPUTSET-REC
                   MOVE    1               TO  FLG-BYOMEIINPUTSET
               END-IF
               PERFORM UNTIL ( IDX             >=  WRK-TBL-MAX )
                       OR    ( FLG-BYOMEIINPUTSET
                                               =   1           )
                   ADD     1                   TO  IDX2
                   IF      IDX2                    >=  WRK-STR
                       ADD     1               TO  IDX
                       IF      IDX             <=  WRK-TBL-MAX
                           MOVE    IDX2        TO
                                           SPA-GMN-C04-TBANGO  (IDX)
                           MOVE    BYOSET-BYOMEIINPUTSET
                                               TO
                                           SPA-GMN-C04-TINPUTCD(IDX)
                           MOVE    BYOSET-BYOMEIINPUTSETNAME
                                               TO
                                           SPA-GMN-C04-TBYOMEI (IDX)
                           MOVE    "1"         TO
                                       SPA-NAI-C04-TBYOMEIINPUTSET (IDX)
      ******************** MOVE    USBYO-BYOMEISBT
      ********************                     TO
      ********************                SPA-NAI-C04-TBYOMEISBT (IDX)
                           MOVE    IDX         TO  SPA-GMN-C04-MAX
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_byomeiinputset"
                                               TO  MCP-TABLE
                   MOVE    WRK-BYOSET-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    WRK-BYOSET-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    自院病名一覧（自院病名）編集処理
           PERFORM 31011-USERBYOMEI-LST-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-ROW 
           .
       3101-USERBYOMEI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧（自院病名）編集処理
      *****************************************************************
       31011-USERBYOMEI-LST-HENSYU-SEC SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           PERFORM UNTIL ( IDX             >=  WRK-TBL-MAX ) OR
                         ( FLG-USERBYOMEI  =   1                )
               ADD     1                   TO  IDX2
               IF      IDX2                    >=  WRK-STR
                   ADD     1               TO  IDX
                   IF      IDX             <=  WRK-TBL-MAX
                       MOVE    IDX2        TO  SPA-GMN-C04-TBANGO  (IDX)
                       MOVE    USBYO-BYOMEIINPUTCD
                                           TO  SPA-GMN-C04-TINPUTCD(IDX)
                       MOVE    USBYO-BYOMEI
                                           TO  SPA-GMN-C04-TBYOMEI (IDX)
                       MOVE    USBYO-BYOMEISBT
                                           TO
                                           SPA-NAI-C04-TBYOMEISBT (IDX)
                       MOVE    SPACE       TO
                                       SPA-NAI-C04-TBYOMEIINPUTSET (IDX)
                       MOVE    IDX         TO  SPA-GMN-C04-MAX
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-TBL-MAX )  AND
                 ( FLG-USERBYOMEI      =   ZERO             )
               MOVE    1                   TO  SPA-GMN-C04-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C04-NEXT
           END-IF
           .
       31011-USERBYOMEI-LST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面６からの戻り処理
      *****************************************************************
       370-CID6-SET-SEC            SECTION.
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0104"
               WHEN    "0106"
      *            自院病名コードの移行
               WHEN    "0109"
      *            自院病名コード廃止処理
               WHEN    "0108"
      *            自院病名コード未コード化病名処理
                   MOVE    SPACE           TO
                                           SPA-NAI-C04-BYOMEI-CHG
                                               (SPA-C04-IKOSAKI-INDEX)
      *
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C04-IKOSAKI-INDEX
                                               TO  IDZ
                       MOVE    SPA-C04-IKOSAKI-INDEX1
                                               TO  IDX2
                       MOVE    SPA-CID6-FLG    TO
                                       SPA-NAI-C04-BYOMEI-CHG (IDZ)
                       INITIALIZE          SPA-NAI-C04-BYOMEI (IDZ)
                                           SPA-NAI-C04-BYOMEICD-O
                                                              (IDZ IDX2)
                                           SPA-NAI-C04-BYOMEI-O
                                                              (IDZ IDX2)
      *
                       MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                               TO
                                       SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                       MOVE     SPA-C04-IKOSAKI-BYOMEI TO
                                       SPA-NAI-C04-BYOMEI-O   (IDZ IDX2)
                       MOVE    SPA-NAI-C04-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C04-BYOMEI (IDZ)
                       PERFORM VARYING IDX2    FROM    2   BY  1
                               UNTIL   IDX2    >       21
                               OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX2)
                                               =   SPACE
                           STRING   SPA-NAI-C04-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                    SPA-NAI-C04-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                        INTO    SPA-NAI-C04-BYOMEI (IDZ)
                           END-STRING
                       END-PERFORM
      *
                       INITIALIZE              SPA-C04-CHANGE-INDEX-ST
                                               SPA-C04-CHANGE-INDEX-ED
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       PERFORM VARYING IDXX    FROM
                                       SPA-C04-CHANGE-INDEX-ST   BY  1
                               UNTIL   IDXX    >
                                       SPA-C04-CHANGE-INDEX-ED
                           INITIALIZE      SPA-NAI-C04-BYOMEI-G (IDXX)
                           MOVE    SPA-CID6-FLG    TO
                                           SPA-NAI-C04-BYOMEI-CHG (IDXX)
                       END-PERFORM
                       MOVE    SPA-C04-CHANGE-INDEX-ST
                                               TO  IDZ
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C04-CHANGE-MAX
                           MOVE    SPA-C04-CHANGE-BYOMEICD (IDXX)
                                           TO 
                                       SPA-NAI-C04-BYOMEICD-O(IDZ IDXX)
                           MOVE    SPA-C04-CHANGE-BYOMEI   (IDXX)
                                           TO 
                                       SPA-NAI-C04-BYOMEI-O  (IDZ IDXX)
      *
                           MOVE    SPA-NAI-C04-BYOMEI-O (IDZ 1)
                                               TO
                                           SPA-NAI-C04-BYOMEI (IDZ)
                           PERFORM VARYING IDX2    FROM    2   BY  1
                                   UNTIL   IDX2    >       21
                                   OR      SPA-NAI-C04-BYOMEICD-O
                                                          (IDZ IDX2)
                                               =   SPACE
                               STRING   SPA-NAI-C04-BYOMEI (IDZ)
                                               DELIMITED  BY  SPACE
                                        SPA-NAI-C04-BYOMEI-O (IDZ IDX2)
                                               DELIMITED  BY  SPACE
                                        INTO    SPA-NAI-C04-BYOMEI (IDZ)
                               END-STRING
                           END-PERFORM        
                      END-PERFORM
                   END-IF
                   IF      SPA-CID6-FLG    =   SPACE
                       INITIALIZE              SPA-C04-CHANGE-INDEX-ST
                                               SPA-C04-CHANGE-INDEX-ED
                   END-IF         
           END-EVALUATE
      *
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C04-IKOSAKI-AREA
                                       SPA-C04-CHANGE-AREA
                                       SPA-ERRCD
                                       WRK-HAISIYMDH
      *
           IF      SPA-C04-BYOSET-INDEX-MAX
                                       >   ZERO
      *        病名入力セットから自院病名編集処理
               PERFORM 41002111-BYOMEIINPUTSET-HEN-SEC
               IF    ( SPA-CIDCD2      NOT =   SPACE )
               OR    ( SPA-CIDCD6      NOT =   SPACE )
               OR    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  370-CID6-SET-EXT
               END-IF
           END-IF
           IF      SPA-C01-INDEX   =   ZERO
               PERFORM 410022-BYOMEI-HENSYU-SEC
           ELSE
               PERFORM 410023-BYOMEI-HENSYU-SEC
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
               ADD     1                   TO  SPA-GMN-C04-CUR
               IF      SPA-GMN-C04-CUR     >   8
                   MOVE    2                   TO  SPA-GMN-C04-CUR
               END-IF
           ELSE
               PERFORM 450-KAKUTEI-SEC
           END-IF
      *
           INITIALIZE                  SPA-C04-CHANGE-AREA
           .
       370-CID6-SET-EXT.
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
                   PERFORM 210-BACK
      *    全分類検索
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 450-BUNRUI-HENSYU-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    検索切替
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 490-KENSAKU-CHANGE-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "BUNRUICD"
                   PERFORM 41001-BUNRUICD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   PERFORM 41002-SELNUM2-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "INPUTCD1"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD2"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD3"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD4"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD5"
               WHEN    "ACTIVATE"      ALSO    "INPUTCD6"
                   PERFORM 41003-INPUTCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_1"
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_2"
               WHEN    "ACTIVATE"      ALSO    "HOSOKUCD_3"
                   PERFORM 41005-HOSOKUCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "KENBYOMEI"
                   PERFORM 41004-KENBYOMEI-CHK-SEC
      *        病名選択
               WHEN    ANY             ALSO    "BYOMEILST"
                   PERFORM 41020-BYOMEISEL-SEC
      *        分類選択
               WHEN    ANY             ALSO    "BUNRUILST"
                   PERFORM 41021-BUNRUILST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名選択処理
      *****************************************************************
       41001-BUNRUICD-CHK-SEC            SECTION.
      *
           MOVE    9                   TO  SPA-GMN-C04-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-C04-BUNRUICD-IDX 
      *
           MOVE    C04-BUNRUICD        TO  SPA-GMN-C04-BUNRUICD
      *
           IF      SPA-GMN-C04-BUNRUICD =   ZERO
               MOVE    2                   TO  SPA-GMN-C04-CUR
           ELSE
               MOVE    ZERO                TO  FLG-CHK  
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-C04-BUNRUI-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-C04-BUNRUICD =
                                           SPA-GMN-C04-BBUNRUICD (IDX)
                       MOVE    IDX             TO
                                               SPA-NAI-C04-BUNRUICD-IDX
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO                           
                   MOVE    "0012"          TO  SPA-ERRCD
               ELSE
                   PERFORM 410011-BUNRUICD-SELECT-SEC
               END-IF
           END-IF    
           .
       41001-BUNRUICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択分類コード編集処理
      *****************************************************************
       410011-BUNRUICD-SELECT-SEC      SECTION.
      *                                     
           INITIALIZE                      SPA-GMN-C04-KENBYOMEI
           MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-GMN-C04-BBUNRUICD 
                           (SPA-NAI-C04-BUNRUICD-IDX)
                                       TO  USBYO-BUNRUICD
           PERFORM 3101-USERBYOMEI-LST-SEC
           MOVE    2                   TO  SPA-GMN-C04-CUR
           .
       410011-BUNRUICD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名番号選択処理
      *****************************************************************
       41002-SELNUM2-CHK-SEC            SECTION.
      *                                     
           MOVE    2                   TO  SPA-GMN-C04-CUR
      *
           MOVE    C04-SELNUM2         TO  SPA-GMN-C04-SELNUM
      *
      *    検索時対象が１件のときは選択番号なしで確定
           IF      SPA-GMN-C04-SELNUM  =   ZERO
               IF    ( SPA-C01-INDEX       >   ZERO )
               AND   ( SPA-GMN-C04-MAX     =   1    )
               AND   ( SPA-GMN-C04-PAGE    =   1    )
                   MOVE    1           TO  SPA-GMN-C04-SELNUM
               END-IF
           END-IF
      *
           IF      SPA-GMN-C04-SELNUM  >   ZERO
               COMPUTE WRK-BANGO       =   SPA-GMN-C04-SELNUM
                                       - ( SPA-GMN-C04-PAGE    -   1 )
                                           *   WRK-TBL-MAX
               IF    ( WRK-BANGO           >   SPA-GMN-C04-MAX )
               OR    ( WRK-BANGO           <   1               )
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
      *
                   EVALUATE    SPA-NAI-C04-B10-WIDGET
                       WHEN    "BYOMEICD"
                           PERFORM 410201-BYOMEI-SELECT-SEC
                       WHEN    "HOSOKUCD"
                           PERFORM 410202-HOSOKU-SELECT-SEC
                   END-EVALUATE
               END-IF
           ELSE 
               IF      SPA-C01-INDEX   >   ZERO
                   MOVE    "0008"              TO  SPA-ERRCD
                   GO  TO  41002-SELNUM2-CHK-EXT
               END-IF
           END-IF
           .
       41002-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       410021-INPUTCD-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           INITIALIZE                      SPA-NAI-C04-BYOMEI-OCC (IDZ)
           MOVE    SPA-GMN-C04-INPUTCD (IDZ)
                                       TO  SPA-NAI-C04-BYOMEICD   (IDZ)
      *
           IF      SPA-GMN-C04-INPUTCD (IDZ)
                                   NOT =   SPACE
      *        病名入力セット検索
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE    SPA-GMN-C04-INPUTCD (IDZ)
                                           TO  BYOSET-BYOMEIINPUTSET
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               PERFORM 900-BYOMEIINPUTSET-READ-SEC
               IF      FLG-BYOMEIINPUTSET  =   ZERO
                   IF      SPA-GMN-C02-BANGO   =   ZERO
      *                病名入力セット編集処理
                       PERFORM 4100211-BYOMEIINPUTSET-CHK-SEC
                       IF      SPA-ERRCD       =   "0097"
                           MOVE    "■■■"        TO
                                               SPA-NAI-C04-BYOMEI (IDZ)
                       END-IF
                   ELSE
                       MOVE    "0098"          TO  SPA-ERRCD
                       MOVE    "■■■"        TO
                                               SPA-NAI-C04-BYOMEI (IDZ)
                   END-IF
                   GO  TO  410021-INPUTCD-HENSYU-EXT
               END-IF
      *
      *        自院病名検索
               PERFORM  4100212-USERBYOMEI-HENSYU-SEC
           END-IF
      *
           .
       410021-INPUTCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       4100212-USERBYOMEI-HENSYU-SEC   SECTION.
      *
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-GMN-C04-INPUTCD (IDZ)
                                       TO  USBYO-BYOMEIINPUTCD
           PERFORM 900-USBYO-INV-SEC                            
           IF      FLG-USERBYOMEI      =   1
               MOVE    "0004"          TO  SPA-ERRCD    
               MOVE    "■■■"        TO  SPA-NAI-C04-BYOMEI (IDZ)
           ELSE
               MOVE    USBYO-BYOMEI    TO  SPA-NAI-C04-BYOMEI (IDZ)
               IF      USBYO-HOSOKU-COMMENT
                                       =   SPACE
                   CONTINUE
               ELSE
                   MOVE    USBYO-HOSOKU-COMMENT
                                         TO  SPA-GMN-C04-HOSOKU-COMMENT
               END-IF
               MOVE    USBYO-BYOMEISBT TO  SPA-NAI-C04-BYOMEISBT (IDZ)
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       21
                       OR      USBYO-BYOMEICD (IDY)    =   SPACE
                   IF      USBYO-BYOMEICD (IDY)  =   "9999999"
                       MOVE    21                      TO  IDY
                   ELSE
                       MOVE    USBYO-BYOMEICD (IDY)    TO
                                       SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                       IF      SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                                       =   WRK-CONS-BYOMEICD
                           MOVE    SPA-NAI-C04-BYOMEI   (IDZ)
                                           TO
                                       SPA-NAI-C04-BYOMEI-O (IDZ IDY)
                       ELSE                           
                           IF  (   SPA-NAI-C04-BYOMEICD-O (IDZ IDY)(1:3)
                                               =   "ZZZ"    )
                           AND ( ( SPA-NAI-C04-BYOMEICD-O (IDZ IDY)(4:1)
                                               <   "1" ) OR 
                                 ( SPA-NAI-C04-BYOMEICD-O (IDZ IDY)(4:1)
                                               =   "C" )    )
      *                        予約病名コード検索
                               SET     TBL-BYOMEICD-IDX  TO  1
                               SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                                   AT  END
                                       MOVE    "■■■"    TO
                                           SPA-NAI-C04-BYOMEI-O(IDZ IDY)
                                   WHEN    SPA-NAI-C04-BYOMEICD-O
                                                           (IDZ IDY)
                                               =   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                       MOVE    TBL-YYK-BYOMEI 
                                                      (TBL-BYOMEICD-IDX)
                                                   TO
                                           SPA-NAI-C04-BYOMEI-O(IDZ IDY)
                               END-SEARCH
                           ELSE
                               MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDY)
                                               TO   BYO-BYOMEICD
                               PERFORM 900-BYOMEI-READ-SEC
                               IF      FLG-BYOMEI        =   ZERO
                                   MOVE    BYO-BYOMEI  TO
                                           SPA-NAI-C04-BYOMEI-O(IDZ IDY)
                               ELSE
                                   MOVE    "■■■"    TO
                                           SPA-NAI-C04-BYOMEI-O(IDZ IDY)
                               END-IF
                           END-IF
                       END-IF    
                   END-IF
               END-PERFORM
      *        廃止年月日・移行先、推奨がある病名のチェック
               PERFORM 4100211-HAISIYMD-CHECK-SEC
           END-IF
      *
           .
       4100212-USERBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット編集処理
      *****************************************************************
       4100211-BYOMEIINPUTSET-CHK-SEC  SECTION.
      *
      *    コード検索時
           IF      SPA-C01-INDEX       >   ZERO
               IF      SPA-C04-BYOSET-INDEX-ST >   ZERO
                   PERFORM VARYING IDWW    FROM
                                           SPA-C04-BYOSET-INDEX-ST
                                                               BY  1
                           UNTIL   IDWW    >   SPA-C04-BYOSET-INDEX-ED
                       INITIALIZE          SPA-GMN-C04-INPUTCD    (IDWW)
                                           SPA-NAI-C04-BYOMEI-OCC (IDWW)
                   END-PERFORM
               END-IF
           ELSE
               INITIALIZE                  SPA-C04-BYOSET-INDEX-AREA
           END-IF
      *
           MOVE    ZERO                TO  WRK-BYOMEI-MAX1
                                           WRK-BYOMEI-MAX2
                                           WRK-BYOMEI-INDEX
      *
      *    セットのコード数とコード欄のチェック
           PERFORM VARYING IDWW    FROM    6   BY  -1
                   UNTIL   IDWW    <       1
               IF      BYOSET-BYOMEIINPUTCD (IDWW)
                                   NOT =   SPACE
                   MOVE    IDWW                TO  WRK-BYOMEI-MAX1
                   MOVE    1                   TO  IDWW
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDWW    FROM    IDZ  BY  1
                   UNTIL   IDWW    >       6
               IF      IDWW                =   IDZ
                   ADD     1                   TO  WRK-BYOMEI-MAX2
               ELSE
                   IF      SPA-GMN-C04-INPUTCD (IDWW)
                                          NOT =   SPACE
                       IF      WRK-BYOMEI-INDEX    =   ZERO
                           MOVE    IDWW            TO  WRK-BYOMEI-INDEX
                       END-IF
                   ELSE
                       ADD     1                   TO  WRK-BYOMEI-MAX2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-BYOMEI-MAX1 >   WRK-BYOMEI-MAX2
               MOVE    "0097"          TO  SPA-ERRCD
               GO  TO  4100211-BYOMEIINPUTSET-CHK-EXT
           END-IF
      *
      *    セットコード退避
           INITIALIZE                      SPA-C04-BYOMEIINPUTSET-AREA
                                           SPA-C04-BYOSET-INDEX-AREA
      *
           MOVE    IDZ                 TO  SPA-C04-BYOSET-INDEX-ST
           MOVE    BYOSET-BYOMEIINPUTCD-G
                                       TO
                                       SPA-C04-BYOSET-BYOMEIINPUTCD-G
           MOVE    WRK-BYOMEI-MAX1     TO  SPA-C04-BYOSET-INDEX-MAX
      *
      *    セットのコードを全て表示できるように入力済コードを移動
           IF      WRK-BYOMEI-INDEX    =   ZERO
               CONTINUE
           ELSE
               COMPUTE IDWW    =    WRK-BYOMEI-MAX1    +
                                  ( 6   -   WRK-BYOMEI-MAX2 )
      *
               MOVE    SPA-GMN-C04-INPUTCD-G   TO  WRK-GMN-C04-INPUTCD-G
               MOVE    SPA-NAI-C04-BYOMEI-AREA TO
                                               WRK-NAI-C04-BYOMEI-AREA
               INITIALIZE                      SPA-GMN-C04-INPUTCD-G
                                               SPA-NAI-C04-BYOMEI-AREA
               PERFORM VARYING IDW1    FROM    IDWW    BY  -1
                       UNTIL   IDW1    <       1
                   IF      IDW1                =   IDZ
                       CONTINUE
                   ELSE
                     IF      IDW1                <   IDZ
                           MOVE    WRK-GMN-C04-INPUTCD (IDW1)
                                       TO  SPA-GMN-C04-INPUTCD    (IDW1)
                           MOVE    WRK-NAI-C04-BYOMEI-OCC (IDW1)
                                       TO  SPA-NAI-C04-BYOMEI-OCC (IDW1)
                     ELSE
                       PERFORM VARYING IDW2    FROM    6   BY  -1
                               UNTIL   IDW2    <       IDZ
                           IF      IDW2                =   IDZ
                               CONTINUE
                           ELSE
                               IF      WRK-GMN-C04-INPUTCD (IDW2)
                                                   NOT =   SPACE
                                   MOVE    WRK-GMN-C04-INPUTCD (IDW2)
                                       TO  SPA-GMN-C04-INPUTCD    (IDW1)
                                   MOVE    WRK-NAI-C04-BYOMEI-OCC (IDW2)
                                       TO  SPA-NAI-C04-BYOMEI-OCC (IDW1)
      *
                                   INITIALIZE  WRK-GMN-C04-INPUTCD(IDW2)
                                               WRK-NAI-C04-BYOMEI-OCC
                                                                  (IDW2)
                                   MOVE    1               TO  IDW2
                               END-IF
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    病名入力セットから自院病名編集処理
           PERFORM 41002111-BYOMEIINPUTSET-HEN-SEC
      *
           .
       4100211-BYOMEIINPUTSET-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セットから自院病名編集処理
      *****************************************************************
       41002111-BYOMEIINPUTSET-HEN-SEC SECTION.
      *
           PERFORM VARYING IDWW    FROM    1   BY  1
                   UNTIL   IDWW    >       6
               IF      SPA-C04-BYOSET-BYOMEIINPUTCD (IDWW)
                                   NOT =   SPACE
                   COMPUTE IDZ =   SPA-C04-BYOSET-INDEX-ST
                               +   IDWW
                               -    1
      *
                   MOVE    SPACE           TO  WRK-HAISIYMDH
                                               SPA-ERRCD
      *
                   INITIALIZE              SPA-NAI-C04-BYOMEI-OCC (IDZ)
                   MOVE    SPA-C04-BYOSET-BYOMEIINPUTCD (IDWW)
                                       TO  SPA-GMN-C04-INPUTCD    (IDZ)
                                           SPA-NAI-C04-BYOMEICD   (IDZ)
      *
                   MOVE    IDZ         TO  SPA-C04-BYOSET-INDEX-ED
      *
      *            自院病名検索
                   PERFORM  4100212-USERBYOMEI-HENSYU-SEC
      *
                   MOVE    SPACE       TO  SPA-C04-BYOSET-BYOMEIINPUTCD
                                                             (IDWW)
                   IF    ( SPA-CIDCD2      NOT =   SPACE )
                   OR    ( SPA-CIDCD6      NOT =   SPACE )
                       GO  TO  41002111-BYOMEIINPUTSET-HEN-EXT
                   END-IF
      *
                   IF      SPA-C01-INDEX   =   ZERO
                       PERFORM 410022-BYOMEI-HENSYU-SEC
                   ELSE
                       PERFORM 410023-BYOMEI-HENSYU-SEC
                   END-IF
      *
                   IF      SPA-ERRCD           =   "0075"  OR  "0094"
                       GO  TO  41002111-BYOMEIINPUTSET-HEN-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    SPA-GMN-C04-CUR
               WHEN    3   THRU    8
                   COMPUTE SPA-GMN-C04-CUR =   SPA-GMN-C04-CUR
                                           +   SPA-C04-BYOSET-INDEX-MAX
                                           -   1
           END-EVALUATE
      *
           INITIALIZE                  SPA-C04-BYOMEIINPUTSET-AREA
      *
           .
       41002111-BYOMEIINPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理（自院病名検索時）
      *****************************************************************
       410022-BYOMEI-HENSYU-SEC      SECTION.
      *
           EVALUATE    SPA-NAI-C04-B10-WIDGET
               WHEN    "BYOMEICD"
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       6
                       PERFORM 3101-BYOMEITOP-HENSYU-SEC
                   END-PERFORM
      *                                     
                   PERFORM 3100-BYOMEI-HENSYU-SEC
      *
      *            変更処理のときの複数病名入力
                   IF    ( SPA-NAI-C04-BYOMEISU-MAX
                                               >   1     )
                   AND   ( SPA-GMN-C02-BANGO   >   ZERO  )
                   AND   ( SPA-C01-MOTOPG      =   "C02" )
                       MOVE    99                  TO  SPA-GMN-C04-CUR
                       MOVE    "0003"              TO  SPA-ERRCD
                   END-IF
      *
      *            複数病名のとき補足コメントの入力不可
                   IF      SPA-ERRCD               =   SPACE  
                       IF    ( SPA-NAI-C04-BYOMEISU-MAX    >   1     )
                       AND   ( SPA-GMN-C04-HOSOKU-COMMENT
                                                       NOT =   SPACE )
                           MOVE    "0094"              TO  SPA-ERRCD
                           INITIALIZE          SPA-GMN-C04-HOSOKUCD-G
                                               SPA-NAI-C04-HOSOKU-TBL
                           MOVE    SPACE               TO
                                           SPA-GMN-C04-HOSOKU-COMMENT
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE   OR  "0094"
                       IF    ( WRK-HAISIYMDH       =   SPACE )
                       OR    ( SPA-C02-1042-MSG3-DSPKBN
                                                   =   "1"   )
                           CONTINUE
                       ELSE
                           MOVE    "0075"              TO  SPA-ERRCD
                       END-IF                     
                   END-IF
               WHEN    "HOSOKUCD"
                   PERFORM 3103-HOSOKU-COMMENT-HENSYU-SEC 
      *
      *            文字数チェック
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI 
                   MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   0 )
                   AND   ( KANACHK-SYUBETU     =   2 )
      *                病名文字数           
                       IF      KANACHK-MAX         >   40
                           MOVE    "0095"              TO  SPA-ERRCD
                       END-IF
      *************ELSE
      *************    MOVE    "0071"              TO  SPA-ERRCD
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO  TO  410022-BYOMEI-HENSYU-EXT
                   END-IF
      *
      *            前後の（）チェック 
                   IF      SPA-GMN-C04-HOSOKU-COMMENT
                                           NOT =   SPACE
                       COMPUTE IDX3    =   KANACHK-MAX     -   1
                       IF    ( SPA-GMN-C04-HOSOKU-COMMENT (1:2)
                                                   =   "（" )
                       OR    ( SPA-GMN-C04-HOSOKU-COMMENT (IDX3:2)
                                                   =   "）" )
                           MOVE    "0092"              TO  SPA-ERRCD
                       END-IF
                   END-IF
          END-EVALUATE
      *
           .
       410022-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理（自院コード検索時）
      *****************************************************************
       410023-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名編集処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
      *
      *   入力病名コード数カウント
           PERFORM 3100-BYOMEI-HENSYU-SEC
      *                                       
      *    新規以外のときは複数病名の入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF    ( SPA-NAI-C04-BYOMEISU-MAX
                                           >   1     )
               AND   ( SPA-GMN-C02-BANGO   >   ZERO  )               
               AND   ( SPA-C01-MOTOPG      =   "C02" )
                   MOVE    "0003"              TO  SPA-ERRCD
               END-IF
           END-IF        
      *
      *    複数病名のとき補足コメントの入力不可
           IF      SPA-ERRCD               =   SPACE  
               IF    ( SPA-NAI-C04-BYOMEISU-MAX    >   1     )
               AND   ( SPA-GMN-C04-HOSOKU-COMMENT
                                               NOT =   SPACE )
                   MOVE    "0094"              TO  SPA-ERRCD
                   INITIALIZE          SPA-GMN-C04-HOSOKUCD-G
                                       SPA-NAI-C04-HOSOKU-TBL
                   MOVE    SPACE               TO
                                       SPA-GMN-C04-HOSOKU-COMMENT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE   OR  "0094"
               IF    ( WRK-HAISIYMDH       =   SPACE )
               OR    ( SPA-C02-1042-MSG3-DSPKBN
                                           =   "1"   )
                   CONTINUE
               ELSE
                   MOVE    "0075"              TO  SPA-ERRCD
               END-IF                     
           END-IF
           .
       410023-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理（補足コメント）
      *****************************************************************
       410024-HOSOKUCD-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      SPA-NAI-C04-HOSOKU-G (IDZ)
      *
           IF      SPA-GMN-C04-HOSOKUCD (IDZ)
                                   NOT =   SPACE
      *        病名入力セットは入力不可
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE    SPA-GMN-C04-HOSOKUCD (IDZ)
                                           TO  BYOSET-BYOMEIINPUTSET
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               PERFORM 900-BYOMEIINPUTSET-READ-SEC
               IF      FLG-BYOMEIINPUTSET  =   ZERO
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    "■■■"            TO
                                       SPA-NAI-C04-HOSOKU-COMMENT (IDZ)
               ELSE
      *            自院病名検索
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-GMN-C04-HOSOKUCD (IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-INV-SEC                            
                   IF      FLG-USERBYOMEI      =   1
                       MOVE    "0004"          TO  SPA-ERRCD    
                       MOVE    "■■■"        TO
                                       SPA-NAI-C04-HOSOKU-COMMENT (IDZ)
                   ELSE
                       MOVE    USBYO-BYOMEI    TO
                                       SPA-NAI-C04-HOSOKU-COMMENT (IDZ)
                   END-IF
               END-IF
           END-IF
      *
           .
       410024-HOSOKUCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    廃止年月日・移行先、推奨チェック処理
      *****************************************************************
       4100211-HAISIYMD-CHECK-SEC      SECTION.
      * 
           IF      USBYO-BYOMEIHENFLG      =   SPACE        
             PERFORM VARYING IDX1    FROM    1   BY  1
                     UNTIL   IDX1    >       21
                     OR      SPA-NAI-C04-BYOMEICD-O (IDZ IDX1)
                                     =   SPACE
               IF      SPA-NAI-C04-BYOMEICD-O (IDZ IDX1) (1:1)
                                         =   "Z"
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDX1)
                                           TO  BYO-BYOMEICD
                   MOVE    ZERO            TO  FLG-BYOMEI
                   PERFORM 900-BYOMEI-READ-SEC
                   IF      FLG-BYOMEI      =   ZERO
                       MOVE    BYOMEI-REC      TO  WRK-BYOMEI-REC
      *
                       IF    ( BYO-IKOSAKICD   NOT =   SPACE )
                       AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                           MOVE    BYO-IKOSAKICD
                                           TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI          =   ZERO
                               MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C04-BYOMEI-O   (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEI
                               MOVE    SPA-NAI-C04-BYOMEI-O   (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEI
                               MOVE    WRK-BYO-HAISIYMD
                                                       TO
                                               SPA-C04-IKOMOTO-HAISIYMD
                               MOVE    BYO-BYOMEICD    TO
                                               SPA-C04-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI      TO
                                               SPA-C04-IKOSAKI-BYOMEI
                               MOVE    IDZ             TO
                                               SPA-C04-IKOSAKI-INDEX                    
                               MOVE    IDX1            TO
                                               SPA-C04-IKOSAKI-INDEX1                    
                               MOVE    "0104"          TO
                                               SPA-CIDCD2
      *                        別のコードでの組み立てを検索
                               PERFORM 500-BYOMEI-KENSAKU-SEC
                               GO  TO  4100211-HAISIYMD-CHECK-EXT
                           END-IF
                       ELSE        
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                               CONTINUE
                           ELSE
                               INITIALIZE              SYNONYM-REC
                                MOVE    SPA-NAI-C04-BYOMEI-O (IDZ IDX1)
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    SPA-NAI-C04-BYOMEICD-O
                                                           (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEICD
                                   MOVE    SPA-NAI-C04-BYOMEI-O
                                                           (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEI
                                   MOVE    IDZ         TO
                                               SPA-C04-IKOSAKI-INDEX
                                   MOVE    IDX1        TO
                                               SPA-C04-IKOSAKI-INDEX1
                                   MOVE    BYO-BYOMEICD
                                                       TO
                                               SPA-C04-IKOSAKI-BYOMEICD
                                   MOVE    BYO-BYOMEI  TO
                                               SPA-C04-IKOSAKI-BYOMEI
                                   MOVE    "0106"      TO  SPA-CIDCD2
      *                            別のコードでの組み立てを検索
                                   PERFORM 500-BYOMEI-KENSAKU-SEC
                                   GO  TO  4100211-HAISIYMD-CHECK-EXT
                               END-IF 
                               MOVE    WRK-BYO-HAISIYMD
                                                       TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                       TO  WRK-HAISIYMDH
                               MOVE    WRK-BYO-BYOMEI  TO
                                                       WRK-ERR-BYOMEI
                               IF      SPA-C02-1042-MSG3-DSPKBN
                                                            =   "0"
                                   MOVE    SPA-NAI-C04-BYOMEICD-O
                                                           (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEICD
                                   MOVE    SPA-NAI-C04-BYOMEI-O
                                                           (IDZ IDX1)
                                                       TO
                                               SPA-C04-IKOMOTO-BYOMEI
                                   MOVE    IDZ         TO
                                               SPA-C04-IKOSAKI-INDEX
                                   MOVE    IDX1        TO
                                               SPA-C04-IKOSAKI-INDEX1
      *                            別のコードでの組み立てを検索
                                   PERFORM 500-BYOMEI-KENSAKU-SEC
                               END-IF
                               GO  TO  4100211-HAISIYMD-CHECK-EXT
                           END-IF
                       END-IF
                   END-IF
                   MOVE    21                  TO  IDX1
               END-IF    
             END-PERFORM
           ELSE
             MOVE    "0108"              TO  SPA-CIDCD2
             IF      SPA-NAI-C04-BYOMEICD-O (IDZ 1)
                                         =   WRK-CONS-BYOMEICD
                 MOVE    SPA-NAI-C04-BYOMEICD-O (IDZ 1)
                                         TO  SPA-C04-IKOMOTO-BYOMEICD
                 MOVE    SPA-NAI-C04-BYOMEI-O   (IDZ 1)
                                         TO  SPA-C04-IKOMOTO-BYOMEI
                 MOVE    IDZ             TO  SPA-C04-IKOSAKI-INDEX                    
                 INITIALIZE                  SYNONYM-REC
                 MOVE    SPA-NAI-C04-BYOMEI-O   (IDZ 1)
                                         TO  SYNONYM-BYOMEI
                 PERFORM 5001-SYNONYM-CHK-SEC
                 IF      FLG-SYNONYM     =   ZERO
                     MOVE    1           TO  SPA-C04-IKOSAKI-INDEX1
                     MOVE    BYO-BYOMEICD
                                         TO  SPA-C04-IKOSAKI-BYOMEICD
                     MOVE    BYO-BYOMEI  TO  SPA-C04-IKOSAKI-BYOMEI
                     MOVE    "0106"      TO  SPA-CIDCD2
                 END-IF
             END-IF
      *      別のコードでの組み立てを検索
             PERFORM 500-BYOMEI-KENSAKU-SEC
           END-IF
           .
        4100211-HAISIYMD-CHECK-EXT.
           EXIT.         
      *
      *****************************************************************
      *    別のコードでの組み立てを検索処理
      *****************************************************************
       500-BYOMEI-KENSAKU-SEC      SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
      *
           MOVE    SPACE               TO  ORCSBYOMEIAREA
           INITIALIZE                      ORCSBYOMEIAREA
           MOVE    "1"                 TO  LNK-BYO-SYORI
      *****IF      SPA-C01-INDEX       =   ZERO
               MOVE    SPACE               TO  WRK-BYOMEI
               MOVE    ZERO                TO  IDYY
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                   IF      SPA-NAI-C04-BYOMEITOP (IDY)
                                               =   "1"
                       IF      IDY                 >   IDZ
                           COMPUTE SPA-C04-CHANGE-INDEX-ED
                                              =    IDY     -   1
                           MOVE    6                   TO  IDY
                       ELSE
                           MOVE    SPA-NAI-C04-BYOMEI (IDY)
                                                       TO  WRK-BYOMEI
                           MOVE    IDY                 TO
                                               SPA-C04-CHANGE-INDEX-ST
                           IF      IDY                 =   6
                               MOVE    IDY                 TO
                                               SPA-C04-CHANGE-INDEX-ED
                           END-IF
                       END-IF
                   ELSE
                       MOVE    IDY                 TO
                                               SPA-C04-CHANGE-INDEX-ED
                       STRING  WRK-BYOMEI          DELIMITED  BY  SPACE
                               SPA-NAI-C04-BYOMEI(IDY)
                                                   DELIMITED  BY  SPACE
                                                   INTO       WRK-BYOMEI
                       END-STRING
                   END-IF    
               END-PERFORM
               MOVE    WRK-BYOMEI          TO  LNK-BYO-BYOMEI
      *****ELSE
      ******   MOVE    SPA-C01-INDEX       TO  SPA-C04-CHANGE-INDEX-ST
      ******                                   SPA-C04-CHANGE-INDEX-ED
      ******   MOVE    SPA-NAI-C04-BYOMEI (SPA-C01-INDEX)
      ******                              TO  LNK-BYO-BYOMEI
      *****END-IF
           COMPUTE SPA-C04-CHANGE-INDEX-MAX
                   =   SPA-C04-CHANGE-INDEX-ED -
                       SPA-C04-CHANGE-INDEX-ST +   1
      *
           CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
                                               SPA-AREA
           IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
           AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
           AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
               MOVE    LNK-BYO-BYOMEI-MAX  TO SPA-C04-CHANGE-MAX
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                   MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C04-CHANGE-BYOMEICD(IDY) 
                   MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C04-CHANGE-BYOMEI  (IDY) 
               END-PERFORM
      *
               IF      WRK-HAISIYMDH   NOT =   SPACE
                   MOVE    "0109"          TO  SPA-CIDCD6
               ELSE
                   MOVE    SPA-CIDCD2      TO  SPA-CIDCD6
                   MOVE    SPACE           TO  SPA-CIDCD2
               END-IF 
           ELSE
               IF      SPA-CIDCD2          =   "0108"
                   MOVE    SPACE           TO  SPA-CIDCD2
               END-IF
           END-IF
           .
       500-BYOMEI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名コードチェック処理
      *****************************************************************
       41003-INPUTCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-HAISIYMDH
                                       WRK-SEIGYO
      *
           MOVE    MCP-WIDGET      TO  WRK-WIDGET
           MOVE    WRK-WIDGET2     TO  IDZ
      *
           COMPUTE SPA-GMN-C04-CUR =   2   +   IDZ
      *
      *    病名コード置換え後にコード入力した時の処理
           IF    ( SPA-NAI-C04-BYOMEICD (1)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (1 1)
                                   =   SPACE                   )
           OR    ( SPA-NAI-C04-BYOMEICD (2)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (2 1)
                                   =   SPACE                   )
           OR    ( SPA-NAI-C04-BYOMEICD (3)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (3 1)
                                   =   SPACE                   )
           OR    ( SPA-NAI-C04-BYOMEICD (4)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (4 1)
                                   =   SPACE                   )
           OR    ( SPA-NAI-C04-BYOMEICD (5)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (5 1)
                                   =   SPACE                   )
           OR    ( SPA-NAI-C04-BYOMEICD (6)
                               NOT =   SPACE               AND
                   SPA-NAI-C04-BYOMEICD-O (6 1)
                                   =   SPACE                   )
               MOVE    ZERO                TO  IDZ1
                                               IDZ2 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       6
                   IF      SPA-NAI-C04-BYOMEITOP (IDY)
                                               =   "1"
                       IF      IDY                 >   IDZ
                           COMPUTE IDZ2    =    IDY     -   1
                           MOVE    6                   TO  IDY
                       ELSE
                           MOVE    IDY                 TO  IDZ1
                           IF      IDY                 =   6
                               MOVE    IDY                 TO  IDZ2
                           END-IF
                       END-IF
                   ELSE
                       MOVE    IDY                 TO  IDZ2
                   END-IF    
               END-PERFORM
               PERFORM VARYING IDZ FROM    IDZ1    BY  1
                       UNTIL   IDZ >       IDZ2
                   PERFORM 410021-INPUTCD-HENSYU-SEC
               END-PERFORM
      *
               INITIALIZE                  SPA-CIDCD-AREA
                                           SPA-CID1-AREA
               INITIALIZE                  SPA-C04-IKOSAKI-AREA
                                           SPA-C04-CHANGE-AREA
                                           WRK-HAISIYMDH
                                           SPA-ERRCD
      *
               MOVE    WRK-WIDGET2     TO  IDZ
           END-IF 
      *
      *    コードの挿入・削除の制御文字取得
           IF      C04-INPUTCD (IDZ)  NOT =   SPACE
               PERFORM VARYING IDX FROM    10  BY  -1
                       UNTIL   IDX <       2
                   IF      C04-INPUTCD (IDZ)(IDX:1)
                                       NOT =   SPACE
                       EVALUATE    C04-INPUTCD (IDZ)(IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      C04-INPUTCD (IDZ)(IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      C04-INPUTCD (IDZ)(IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
      *****                IF      SPA-GMN-C04-INPUTCD (6)
                           IF      SPA-NAI-C04-BYOMEICD (6)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
      *****                    IF      SPA-GMN-C04-INPUTCD (IDW)
                               IF      SPA-NAI-C04-BYOMEICD(IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO      =   SPACE
                               COMPUTE SPA-GMN-C04-CUR =   2   +   IDZ
                               GO  TO  41003-INPUTCD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    2               TO  IDX
                   END-IF 
               END-PERFORM
           ELSE
               MOVE    "-"                     TO  WRK-SEIGYO
           END-IF
      *       
           EVALUATE    WRK-SEIGYO
               WHEN    "-"
                   PERFORM VARYING IDX FROM    IDZ BY  1
                           UNTIL   IDX >       6
                       IF      IDX             =   6
                           INITIALIZE  SPA-NAI-C04-BYOMEI-OCC (IDX)
                                       SPA-GMN-C04-INPUTCD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C04-BYOMEI-OCC (IDW)
                                   TO  SPA-NAI-C04-BYOMEI-OCC (IDX)
                           MOVE    SPA-GMN-C04-INPUTCD   (IDW)
                                   TO  SPA-GMN-C04-INPUTCD  (IDX)
                       END-IF                     
                   END-PERFORM                           
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41003-INPUTCD-CHK-EXT
               WHEN    "+"
                   PERFORM VARYING IDX FROM    6   BY  -1
                           UNTIL   IDX <       IDZ
                       IF      IDX             =   IDZ
                           INITIALIZE  SPA-NAI-C04-BYOMEI-OCC(IDX)
                                       SPA-GMN-C04-INPUTCD (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     -   1
                           MOVE    SPA-NAI-C04-BYOMEI-OCC (IDW)
                                   TO  SPA-NAI-C04-BYOMEI-OCC(IDX)
                           MOVE    SPA-GMN-C04-INPUTCD   (IDW)
                                   TO  SPA-GMN-C04-INPUTCD  (IDX)
                       END-IF                     
                   END-PERFORM
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41003-INPUTCD-CHK-EXT
           END-EVALUATE                               
      *
           INITIALIZE              SPA-NAI-C04-BYOMEI-OCC (IDZ)
      *
      *    半角チェック
           IF      C04-INPUTCD (IDZ)   =   SPACE
               CONTINUE
           ELSE    
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    C04-INPUTCD (IDZ)   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-RC2         =   ZERO
                   MOVE    KANACHK-OUT-INPUT   TO  C04-INPUTCD (IDZ)
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
                   GO  TO  41003-INPUTCD-CHK-EXT
               END-IF
           END-IF    
      *
           MOVE    C04-INPUTCD (IDZ)   TO  SPA-GMN-C04-INPUTCD (IDZ)
                                           SPA-NAI-C04-BYOMEICD(IDZ)
      *
           PERFORM 410021-INPUTCD-HENSYU-SEC
           IF    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO  TO  41003-INPUTCD-CHK-EXT
           END-IF     
      *
           PERFORM 410022-BYOMEI-HENSYU-SEC
      *
           IF      SPA-C04-BYOSET-INDEX-MAX
                                       >   ZERO
               GO  TO  41003-INPUTCD-CHK-EXT
           END-IF     
      *
           IF      SPA-ERRCD           =   SPACE   OR  "0075"  OR
                                           "0094"
               ADD     1                   TO  SPA-GMN-C04-CUR
               IF      SPA-GMN-C04-CUR     >   8
                   MOVE    2                   TO  SPA-GMN-C04-CUR
               END-IF
           END-IF
           .
       41003-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       41004-KENBYOMEI-CHK-SEC          SECTION.
      *
           MOVE    1                  TO  SPA-GMN-C04-CUR
      *         
           MOVE    C04-KENBYOMEI      TO  SPA-GMN-C04-KENBYOMEI
      *
           IF      SPA-GMN-C04-KENBYOMEI  =   SPACE
               CONTINUE
           ELSE
      *        半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C04-KENBYOMEI
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C04-KENBYOMEI
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
                   GO  TO  41004-KENBYOMEI-CHK-EXT
               END-IF
      *
               MOVE    ZERO                TO  SPA-GMN-C04-BUNRUICD
                                               SPA-NAI-C04-BUNRUICD-IDX
               MOVE    1                   TO  SPA-GMN-C04-PAGE
      *
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key8"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               STRING  "%"                 DELIMITED  BY  SIZE
                       SPA-GMN-C04-KENBYOMEI
                                           DELIMITED  BY  SPACE
                       "%"                 DELIMITED  BY  SIZE
                                           INTO    USBYO-BYOMEI
               END-STRING
               PERFORM 3101-USERBYOMEI-LST-SEC
           END-IF    
      *
           MOVE    2                  TO  SPA-GMN-C04-CUR
           .
       41004-KENBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメントコードチェック処理
      *****************************************************************
       41005-HOSOKUCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SEIGYO
      *
           MOVE    MCP-WIDGET      TO  WRK-WIDGET
           MOVE    WRK-WIDGET4     TO  IDZ
      *
           COMPUTE SPA-GMN-C04-CUR =   9   +   IDZ
      *
      *    コードの挿入・削除の制御文字取得
           IF      C04-HOSOKUCD (IDZ)
                               NOT =   SPACE
               PERFORM VARYING IDX FROM    10  BY  -1
                       UNTIL   IDX <       2
                   IF      C04-HOSOKUCD (IDZ)(IDX:1)
                                       NOT =   SPACE
                       EVALUATE    C04-HOSOKUCD (IDZ)(IDX:1)
                           WHEN    "-"
                               MOVE    "-"         TO  WRK-SEIGYO
                           WHEN    "+"
                               MOVE    "+"         TO  WRK-SEIGYO
                           WHEN    OTHER
                               IF      IDX             >   2
                                   COMPUTE IDW     =   IDX     -   1
                                   IF      C04-HOSOKUCD (IDZ)(IDW:2)
                                                   =   "−"
                                       MOVE    "-"     TO  WRK-SEIGYO
                                   END-IF
                                   IF      C04-HOSOKUCD (IDZ)(IDW:2)
                                                   =   "＋"
                                       MOVE    "+"     TO  WRK-SEIGYO
                                   END-IF
                               END-IF
                       END-EVALUATE
      *                追加のとき
                       IF      WRK-SEIGYO      =   "+"
                           IF      SPA-GMN-C04-HOSOKUCD (3)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF 
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-GMN-C04-HOSOKUCD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO      =   SPACE
                               COMPUTE SPA-GMN-C04-CUR =   9   +   IDZ
                               GO  TO  41005-HOSOKUCD-CHK-EXT
                           END-IF
                       END-IF
                       MOVE    2               TO  IDX
                   END-IF 
               END-PERFORM
           ELSE
               MOVE    "-"                     TO  WRK-SEIGYO
           END-IF
      *       
           EVALUATE    WRK-SEIGYO
               WHEN    "-"
                   PERFORM VARYING IDX FROM    IDZ BY  1
                           UNTIL   IDX >       3
                       IF      IDX             =   3
                           INITIALIZE  SPA-NAI-C04-HOSOKU-G (IDX)
                                       SPA-GMN-C04-HOSOKUCD  (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     +   1
                           MOVE    SPA-NAI-C04-HOSOKU-G (IDW)
                                   TO  SPA-NAI-C04-HOSOKU-G (IDX)
                           MOVE    SPA-GMN-C04-HOSOKUCD   (IDW)
                                   TO  SPA-GMN-C04-HOSOKUCD  (IDX)
                       END-IF                     
                   END-PERFORM                           
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41005-HOSOKUCD-CHK-EXT
               WHEN    "+"
                   PERFORM VARYING IDX FROM    3   BY  -1
                           UNTIL   IDX <       IDZ
                       IF      IDX             =   IDZ
                           INITIALIZE  SPA-NAI-C04-HOSOKU-G (IDX)
                                       SPA-GMN-C04-HOSOKUCD (IDX)
                       ELSE
                           COMPUTE IDW     =   IDX     -   1
                           MOVE    SPA-NAI-C04-HOSOKU-G (IDW)
                                   TO  SPA-NAI-C04-HOSOKU-G(IDX)
                           MOVE    SPA-GMN-C04-HOSOKUCD (IDW)
                                   TO  SPA-GMN-C04-HOSOKUCD  (IDX)
                       END-IF                     
                   END-PERFORM
                   PERFORM 410022-BYOMEI-HENSYU-SEC 
                   MOVE    SPACE           TO  WRK-SEIGYO     
                   GO  TO  41005-HOSOKUCD-CHK-EXT
           END-EVALUATE                               
      *
           INITIALIZE              SPA-NAI-C04-HOSOKU-G (IDZ)
      *
      *    半角チェック
           IF      C04-HOSOKUCD (IDZ)   =   SPACE
               CONTINUE
           ELSE    
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    C04-HOSOKUCD (IDZ)  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-RC2         =   ZERO )
                   MOVE    KANACHK-OUT-INPUT   TO  C04-HOSOKUCD (IDZ)
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
                   GO  TO  41005-HOSOKUCD-CHK-EXT
               END-IF
           END-IF    
      *
           MOVE    C04-HOSOKUCD (IDZ)   TO  SPA-GMN-C04-HOSOKUCD(IDZ)
      *
           PERFORM 410024-HOSOKUCD-HENSYU-SEC
      *
           PERFORM 410022-BYOMEI-HENSYU-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               ADD     1                   TO  SPA-GMN-C04-CUR
               IF      SPA-GMN-C04-CUR     >   12
                   MOVE    2                   TO  SPA-GMN-C04-CUR
               END-IF
           END-IF
           .
       41005-HOSOKUCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名行選択処理
      *****************************************************************
       41020-BYOMEISEL-SEC       SECTION.
      * 
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-MAX
               IF      C04-BYOMEILST-SELECT (IDX)    =   "T"
                   MOVE    IDX                  TO  WRK-SELNO
                   MOVE    SPA-GMN-C04-MAX      TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    WRK-SELNO           TO  WRK-BANGO
      *
           EVALUATE    SPA-NAI-C04-B10-WIDGET
               WHEN    "BYOMEICD"
                   PERFORM 410201-BYOMEI-SELECT-SEC
               WHEN    "HOSOKUCD"
                   PERFORM 410202-HOSOKU-SELECT-SEC
           END-EVALUATE
           .
       41020-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名行選択処理（病名検索時）
      *****************************************************************
       410201-BYOMEI-SELECT-SEC       SECTION.
      *
      * 
           IF      SPA-C01-INDEX       =   ZERO
      *        病名コードが６つのとき
               IF      SPA-NAI-C04-BYOMEICD-MAX
                                           =   6
                   MOVE    99                  TO  SPA-GMN-C04-CUR
                   MOVE    "0013"              TO  SPA-ERRCD
                   GO  TO  410201-BYOMEI-SELECT-EXT
               END-IF
      *
               MOVE    ZERO            TO  IDZ
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       6
                   IF      SPA-GMN-C04-INPUTCD (IDXX)  =   SPACE
                       MOVE    SPA-GMN-C04-TINPUTCD   (WRK-BANGO)
                                           TO  SPA-GMN-C04-INPUTCD(IDXX)
                                               SPA-NAI-C04-BYOMEICD
                                                                  (IDXX)
                       MOVE    IDXX        TO  IDZ
                       PERFORM 410021-INPUTCD-HENSYU-SEC
      *
                       MOVE    6           TO  IDXX
                   END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-C01-INDEX   TO  IDZ
               MOVE    SPA-GMN-C04-TINPUTCD (WRK-BANGO)
                                       TO  SPA-GMN-C04-INPUTCD(IDZ)
                                           SPA-NAI-C04-BYOMEICD(IDZ)
               PERFORM 410021-INPUTCD-HENSYU-SEC
           END-IF
      *
           IF    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO  TO  410201-BYOMEI-SELECT-EXT
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
               PERFORM 410022-BYOMEI-HENSYU-SEC
           ELSE
               PERFORM 410023-BYOMEI-HENSYU-SEC
           END-IF
      *
           IF      SPA-C04-BYOSET-INDEX-MAX
                                       >   ZERO
               GO  TO  410201-BYOMEI-SELECT-EXT
           END-IF     
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-C01-INDEX   =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-C04-SELNUM
                   MOVE    9                   TO  SPA-GMN-C04-CUR
               ELSE
                   PERFORM 450-KAKUTEI-SEC
               END-IF 
           END-IF
           .
       410201-BYOMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名行選択処理（補足コメント検索時）
      *****************************************************************
       410202-HOSOKU-SELECT-SEC       SECTION.
      *
      *    補足コメントコードが３つのとき
               IF    ( SPA-GMN-C04-HOSOKUCD (1)
                                   NOT =   SPACE )
               AND   ( SPA-GMN-C04-HOSOKUCD (2)
                                   NOT =   SPACE )
               AND   ( SPA-GMN-C04-HOSOKUCD (3)
                                   NOT =   SPACE )
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO  TO  410202-HOSOKU-SELECT-EXT
           END-IF
      * 
           IF      SPA-C01-INDEX       =   ZERO
               MOVE    ZERO            TO  IDZ
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       3
                   IF      SPA-GMN-C04-HOSOKUCD (IDXX)  =   SPACE
                       MOVE    SPA-GMN-C04-TINPUTCD   (WRK-BANGO)
                                           TO  SPA-GMN-C04-HOSOKUCD
                                                                 (IDXX)
                       MOVE    IDXX        TO  IDZ
                       PERFORM 410024-HOSOKUCD-HENSYU-SEC
      *
                       MOVE    3           TO  IDXX
                   END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-C01-INDEX   TO  IDZ
               MOVE    SPA-GMN-C04-TINPUTCD (WRK-BANGO)
                                       TO  SPA-GMN-C04-HOSOKUCD(IDZ)
               PERFORM 410024-HOSOKUCD-HENSYU-SEC
           END-IF
      *
           IF      SPA-CIDCD2      NOT =   SPACE
               GO  TO  410202-HOSOKU-SELECT-EXT
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
               PERFORM 410022-BYOMEI-HENSYU-SEC
           ELSE
               PERFORM 410023-BYOMEI-HENSYU-SEC
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-C01-INDEX   =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-C04-SELNUM
                   MOVE    9                   TO  SPA-GMN-C04-CUR
               ELSE
                   PERFORM 450-KAKUTEI-SEC
               END-IF 
           END-IF
           .
       410202-HOSOKU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名行選択処理
      *****************************************************************
       41021-BUNRUILST-SEC       SECTION.
      *          
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-BUNRUI-MAX
               IF      C04-BUNRUI-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C04-BUNRUICD-IDX
                       CONTINUE
                   ELSE        
                       MOVE    IDX                     TO  WRK-SELNO
                       MOVE    SPA-GMN-C04-BUNRUI-MAX  TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-C04-BUNRUICD-IDX
               MOVE    SPA-GMN-C04-BBUNRUICD (SPA-NAI-C04-BUNRUICD-IDX) 
                                           TO SPA-GMN-C04-BUNRUICD
           END-IF
      *
           PERFORM 410011-BUNRUICD-SELECT-SEC
           .
       41021-BUNRUISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-C01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "C04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-C01KYOUTU 
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
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
      *    全分類編集処理
      *****************************************************************
       450-BUNRUI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN-C04-KENBYOMEI
           MOVE    ZERO                TO  SPA-GMN-C04-BUNRUICD
                                           SPA-NAI-C04-BUNRUICD-IDX
           MOVE    1                   TO  SPA-GMN-C04-PAGE
      *    検索処理
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key3"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    9                   TO  SPA-GMN-C04-CUR     
           .
       450-BUNRUI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C04-PAGE        =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C04-PAGE    =   SPA-GMN-C04-PAGE    -   1
      *
      *    検索処理
           IF      SPA-GMN-C04-BUNRUICD    =   ZERO
               IF      SPA-C01-INDEX   =   ZERO
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key3"              TO  WRK-MCP-PATHNAME
                   INITIALIZE                  USERBYOMEI-REC
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   STRING  SPA-C01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                           "%"                     DELIMITED   BY  SIZE
                                           INTO    USBYO-BYOMEIINPUTCD
                   END-STRING
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
               END-IF
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-GMN-C04-BUNRUICD 
                                       TO  USBYO-BUNRUICD
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-C04-CUR     
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C04-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C04-PAGE
      *
      *    検索処理
           IF      SPA-GMN-C04-BUNRUICD    =   ZERO
               IF      SPA-C01-INDEX   =   ZERO
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key3"              TO  WRK-MCP-PATHNAME
                   INITIALIZE                  USERBYOMEI-REC
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   STRING  SPA-C01-BYOMEIINPUTCD   DELIMITED   BY  SPACE
                           "%"                     DELIMITED   BY  SIZE
                                           INTO    USBYO-BYOMEIINPUTCD
                   END-STRING
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
               END-IF
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-GMN-C04-BUNRUICD 
                                       TO  USBYO-BUNRUICD
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-C04-CUR     
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索切替処理
      *****************************************************************
       490-KENSAKU-CHANGE-SEC      SECTION.
      *
           EVALUATE    SPA-NAI-C04-B10-WIDGET
               WHEN    "BYOMEICD"
      *            複数病名入力時の補足コメント入力はエラー
                   IF      SPA-NAI-C04-BYOMEISU-MAX
                                           >   1
                       MOVE    "0099"          TO  SPA-ERRCD
                       GO  TO  490-KENSAKU-CHANGE-EXT
                   END-IF
      *
                   MOVE    "HOSOKUCD"      TO  SPA-NAI-C04-B10-WIDGET
                   EVALUATE    SPA-GMN-C04-CUR
                       WHEN    3   THRU    8
                           MOVE    10          TO  SPA-GMN-C04-CUR
                   END-EVALUATE 
               WHEN    "HOSOKUCD"
                   MOVE    "BYOMEICD"      TO  SPA-NAI-C04-B10-WIDGET
                   EVALUATE    SPA-GMN-C04-CUR
                       WHEN    10  THRU    12
                           MOVE    3           TO  SPA-GMN-C04-CUR
                   END-EVALUATE 
           END-EVALUATE
           .
       490-KENSAKU-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC             SECTION.
      *
      *    変更処理のときの複数病名入力
           IF    ( SPA-NAI-C04-BYOMEISU-MAX
                                       >   1     )
           AND   ( SPA-GMN-C02-BANGO   >   ZERO  )               
           AND   ( SPA-C01-MOTOPG      =   "C02" )
               MOVE    99                  TO  SPA-GMN-C04-CUR
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
      *    病名入力可のときのチェック
           IF      SPA-C01-B10-FLG =   ZERO    OR  1
      *        コードチェック
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF    ( SPA-GMN-C04-INPUTCD (IDX)
                                               NOT =   SPACE )
                   AND   ( SPA-NAI-C04-BYOMEI(IDX) (1:2)
                                                   =   "■"  )
                       COMPUTE SPA-GMN-C04-CUR     =   IDX     +   2
                       MOVE    "0004"              TO  SPA-ERRCD
                       GO  TO  450-KAKUTEI-EXT
                   END-IF
               END-PERFORM    
      *
      *        病名重複チェック
               MOVE    SPACE                   TO  WRK-BYOMEI-AREA
               MOVE    ZERO                    TO  IDY
      *
               MOVE    SPA-NAI-C04-BYOMEI (1)  TO  WRK-BYOMEI
      *     
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       6
                   IF      SPA-GMN-C04-INPUTCD (IDX)
                                       NOT =   SPACE
                       IF      SPA-NAI-C04-BYOMEITOP (IDX) =   "1"
                           IF      WRK-BYOMEI  NOT =   SPACE
                               ADD     1           TO  IDY
                               MOVE    WRK-BYOMEI  TO  WRK-BYOMEI-O(IDY)
                           END-IF
                           MOVE    SPACE           TO  WRK-BYOMEI
                       END-IF
                       STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                               SPA-NAI-C04-BYOMEI(IDX)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                       END-STRING
                   END-IF    
               END-PERFORM    
               IF      WRK-BYOMEI      NOT =   SPACE
                   ADD     1               TO  IDY
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI-O (IDY)
               END-IF
      *
               MOVE    SPACE               TO  WRK-ERR-BYOMEI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                       OR      WRK-BYOMEI-O (IDX)  =   SPACE
                   PERFORM VARYING IDY FROM    IDX  BY  1
                           UNTIL   IDY >       6
                           OR      WRK-BYOMEI-O (IDY)  =   SPACE
                       IF      IDY             >   IDX
                           IF      WRK-BYOMEI-O (IDX)  =
                                                   WRK-BYOMEI-O (IDY)
                               MOVE    "0002"      TO  SPA-ERRCD
                               MOVE    WRK-BYOMEI-O (IDX)
                                                   TO  WRK-ERR-BYOMEI
                               GO  TO  450-KAKUTEI-EXT
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    補足コメント入力可のときのチェック
           IF      SPA-C01-B10-FLG =   ZERO    OR  2
      *        コードチェック
               IF      SPA-GMN-C04-HOSOKUCD-G  =   SPACE
                   MOVE    SPA-GMN-C04-HOSOKU-COMMENT
                                       TO  WRK-BYOMEI 
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       3
                       IF    ( SPA-GMN-C04-HOSOKUCD (IDX)
                                                   NOT =   SPACE )
                       AND   ( SPA-NAI-C04-HOSOKU-COMMENT(IDX) (1:2)
                                                       =   "■"  )
                           COMPUTE SPA-GMN-C04-CUR     =   IDX     +   9
                           MOVE    "0093"              TO  SPA-ERRCD
                           GO  TO  450-KAKUTEI-EXT
                       END-IF
                   END-PERFORM 
      *            文字数チェック
                   PERFORM 3103-HOSOKU-COMMENT-HENSYU-SEC
               END-IF 
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    WRK-BYOMEI          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   0 )
               AND   ( KANACHK-SYUBETU     =   2 )
      *            病名文字数
                   IF      KANACHK-MAX         >   40
                       MOVE    "0095"              TO  SPA-ERRCD
                       GO  TO  450-KAKUTEI-EXT
                   END-IF
      *********ELSE
      *********    MOVE    "0071"              TO  SPA-ERRCD
               END-IF
      *        前後の（）チェック 
      *********IF      SPA-GMN-C04-HOSOKU-COMMENT
      *********                        NOT =   SPACE
      *********    COMPUTE IDX3    =   KANACHK-MAX     -   1
      *********    IF    ( SPA-GMN-C04-HOSOKU-COMMENT (1:2)
      *********                                =   "（" )
      *********    OR    ( SPA-GMN-C04-HOSOKU-COMMENT (IDX3:2)
      *********                                =   "）" )
      *********        MOVE    "0092"              TO  SPA-ERRCD
      *********        GO  TO  450-KAKUTEI-EXT
      *********    END-IF
      *********END-IF
           END-IF            
      *     
           INITIALIZE                  SPA-C01-BYOMEI-TBL
      *
           MOVE    ZERO            TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               IF      SPA-GMN-C04-INPUTCD (IDX)   NOT =   SPACE
                   ADD     1           TO  IDY    
                   MOVE    SPA-GMN-C04-INPUTCD  (IDX)
                                       TO  SPA-C01-TBYOMEICD (IDY)
                   MOVE    IDX         TO  SPA-C01-TBYOMEICD-INDEX
                                                             (IDY)
               END-IF 
           END-PERFORM
      *
          IF      SPA-C01-INDEX   =   ZERO
               MOVE    ZERO            TO  IDY
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       3
                   IF      SPA-GMN-C04-HOSOKUCD (IDX)   NOT =   SPACE
                       ADD     1           TO  IDY    
                       MOVE    SPA-GMN-C04-HOSOKUCD  (IDX)
                                           TO  SPA-C01-THOSOKUCD (IDY)
                       MOVE    IDX         TO  SPA-C01-THOSOKUCD-INDEX
                                                                 (IDY)
                   END-IF 
               END-PERFORM
               IF      SPA-C01-THOSOKUCD (1)
                                           =   SPACE
                   MOVE    SPA-GMN-C04-HOSOKU-COMMENT
                                           TO  SPA-C01-HOSOKU-COMMENT
               END-IF
           ELSE    
               EVALUATE    SPA-C01-WIDGET (1:8)
                   WHEN    "HOSOKUCD"
                       MOVE    SPA-C01-INDEX   TO  IDX
                       MOVE    SPA-GMN-C04-HOSOKUCD (IDX)
                                               TO  SPA-C01-BYOMEIINPUTCD
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
      *
           MOVE    SPA-C01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "C04"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      *
           IF      SPA-C02-1042-MSG2-DSPKBN
                                       =   "1"
               MOVE    1                   TO  FLG-SYNONYM
               GO  TO  5001-SYNONYM-CHK-EXT
           END-IF
      * 
           PERFORM 900-SYNONYM-READ-SEC
           IF      FLG-SYNONYM     =   ZERO
      *        同義語マスタから病名検索
               MOVE    SYNONYM-BYOMEICD
                                       TO  BYO-BYOMEICD
               MOVE    ZERO            TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI      =   1
                   MOVE    1               TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-SYNONYM     =   ZERO
               MOVE    BYO-BYOMEICD    TO  SPA-C04-IKOSAKI-BYOMEICD
               MOVE    BYO-BYOMEI      TO  SPA-C04-IKOSAKI-BYOMEI
               MOVE    "0106"          TO  SPA-CIDCD2
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
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
           IF      SPA-CIDCD2      NOT =   SPACE
               PERFORM 530-CIDSET2-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD6      NOT =   SPACE
               PERFORM 560-CIDSET6-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C04    "           TO  MCP-WINDOW
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
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C04-BYO-ROW
           ELSE
               MOVE    C04-BYOMEILST-ROW   TO  WRK-C04-BYO-ROW
           END-IF
      * 
           MOVE    SPACE               TO  C04
           INITIALIZE                      C04
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C04-MAX
               MOVE    SPA-GMN-C04-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C04-TBANGO    (IDX)
               MOVE    SPA-GMN-C04-TINPUTCD(IDX)
                                           TO  C04-TBYOMEIINPUTCD(IDX)
               MOVE    SPA-GMN-C04-TBYOMEI    (IDX)
                                           TO  C04-TBYOMEI   (IDX)
               IF      SPA-NAI-C04-TBYOMEIINPUTSET (IDX)
                                           =   "1"
                   MOVE    "◎"                TO  C04-TMARK (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C04-MAX     TO  C04-BYOMEILST-COUNT
           MOVE    ZERO                TO  C04-BYOMEILST-ROW
           IF      WRK-C04-BYO-ROW     >   ZERO
               MOVE    WRK-C04-BYO-ROW     TO  C04-BYOMEILST-ROW
           END-IF
      *
           MOVE    SPA-GMN-C04-KENBYOMEI   TO  C04-KENBYOMEI
           MOVE    SPA-GMN-C04-BUNRUICD    TO  C04-BUNRUICD
           MOVE    SPA-GMN-C04-SELNUM      TO  C04-SELNUM2
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C04-INPUTCD(IDX)   TO  C04-INPUTCD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C04-BYOMEI      TO  C04-BYOMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      3
               MOVE    SPA-GMN-C04-HOSOKUCD(IDX)  TO  C04-HOSOKUCD(IDX)
           END-PERFORM
           MOVE    SPA-GMN-C04-HOSOKU-COMMENT
                                           TO  C04-HOSOKU-COMMENT
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C04-BUNRUI-MAX
               MOVE    SPA-GMN-C04-BBUNRUICD (IDX)
                                           TO  C04-BBUNRUICD (IDX)
               MOVE    SPA-GMN-C04-BBUNRUIMEI(IDX)
                                           TO  C04-BBUNRUIMEI(IDX)
      *        
               IF      IDX             =   SPA-NAI-C04-BUNRUICD-IDX
                   MOVE    "T"         TO  C04-BUNRUI-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  C04-BUNRUI-SELECT (IDX)
               END-IF
           END-PERFORM    
           MOVE    SPA-GMN-C04-BUNRUI-MAX  TO  C04-BUNRUI-COUNT
           MOVE    ZERO                    TO  C04-BUNRUI-ROW
      *
           IF      SPA-C01-INDEX   =   ZERO
               MOVE    WIDGET-NORMAL       TO  C04-B04-STATE
                                               C04-B10-STATE
                                               C04-B12-STATE
                                               C04-BUNRUICD-STATE
                                               C04-KENBYOMEI-STATE
               IF      SPA-NAI-C04-B10-WIDGET
                                       =   "BYOMEICD"
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >       6
                       MOVE    WIDGET-NORMAL   TO  C04-INPUTCD-STATE
                                                               (IDX)
                   END-PERFORM
                   PERFORM VARYING     IDX     FROM   1   BY  1
                           UNTIL       IDX     >      3
                       MOVE    WIDGET-INSENSITIVE
                                           TO  C04-HOSOKUCD-STATE (IDX)
                   END-PERFORM
                   MOVE    "F10:補足検索"      TO  C04-B10-LABEL
                   IF      SPA-C01-B10-FLG     =   ZERO
                       MOVE    WIDGET-NORMAL       TO  C04-B10-STATE
                   ELSE
                       MOVE    WIDGET-INSENSITIVE  TO  C04-B10-STATE
                   END-IF
               ELSE
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >       6
                       MOVE    WIDGET-INSENSITIVE
                                           TO  C04-INPUTCD-STATE
                                                               (IDX)
                   END-PERFORM
                   PERFORM VARYING     IDX     FROM   1   BY  1
                           UNTIL       IDX     >      3
                       MOVE    WIDGET-NORMAL  
                                           TO  C04-HOSOKUCD-STATE (IDX)
                   END-PERFORM
                   MOVE    "F10:病名検索"  TO  C04-B10-LABEL
                   IF      SPA-C01-B10-FLG     =   ZERO
                       MOVE    WIDGET-NORMAL       TO  C04-B10-STATE
                   ELSE
                       MOVE    WIDGET-INSENSITIVE  TO  C04-B10-STATE
                   END-IF
               END-IF    
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  C04-B04-STATE
                                               C04-B10-STATE
                                               C04-B12-STATE
                                               C04-BUNRUICD-STATE
                                               C04-KENBYOMEI-STATE
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >      6
                   MOVE    WIDGET-INSENSITIVE
                                           TO  C04-INPUTCD-STATE (IDX)
               END-PERFORM
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >      3
                   MOVE    WIDGET-INSENSITIVE
                                           TO  C04-HOSOKUCD-STATE (IDX)
               END-PERFORM
               IF      SPA-NAI-C04-B10-WIDGET
                                       =   "BYOMEICD"
                   MOVE    "F10:補足検索"  TO  C04-B10-LABEL
               ELSE
                   MOVE    "F10:病名検索"  TO  C04-B10-LABEL
               END-IF
           END-IF
      *
           IF      SPA-C01-INDEX   =   ZERO
               MOVE    "検索病名"      TO  C04-TITLE
           ELSE
               MOVE    "検索コード"    TO  C04-TITLE
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
           EVALUATE    SPA-GMN-C04-CUR
             WHEN    1
                     MOVE  "KENBYOMEI"  TO  MCP-WIDGET
             WHEN    2
                     MOVE  "SELNUM2"    TO  MCP-WIDGET
             WHEN    3
                     MOVE  "INPUTCD1"   TO  MCP-WIDGET
             WHEN    4
                     MOVE  "INPUTCD2"   TO  MCP-WIDGET
             WHEN    5
                     MOVE  "INPUTCD3"   TO  MCP-WIDGET
             WHEN    6
                     MOVE  "INPUTCD4"   TO  MCP-WIDGET
             WHEN    7
                     MOVE  "INPUTCD5"   TO  MCP-WIDGET
             WHEN    8
                     MOVE  "INPUTCD6"   TO  MCP-WIDGET
             WHEN    9
                     MOVE  "BUNRUICD"   TO  MCP-WIDGET
             WHEN    10
                     MOVE  "HOSOKUCD_1" TO  MCP-WIDGET
             WHEN    11
                     MOVE  "HOSOKUCD_2" TO  MCP-WIDGET
             WHEN    12
                     MOVE  "HOSOKUCD_3" TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPA-ERRCD           TO  SPA-C01-ERRCD
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "が複数入力されています"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   MOVE
                       "変更処理中なので病名はひとつしか入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "補足コメントにセットコードは入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "病名入力コード入力エラー" 
                                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "病名が入力されていません"  TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "病名コードの入力でエラーがあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "これ以上選択できません"    TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"       TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "入力された病名はマスター上で"
                                       TO  SPA-ERRMSG
                   MOVE    WRK-HAISIYMDH
                                       TO  SPA-ERRMSG (29:22)
                   MOVE    "で廃止の扱いとなっています。"
                                       TO  SPA-ERRMSG (51:)
               WHEN    "0092"
                   MOVE    "補足コメントの前後に（）があります"
                                               TO  SPA-ERRMSG   
               WHEN    "0093"
                   MOVE    "補足コメントが入力の途中です"
                                               TO  SPA-ERRMSG   
               WHEN    "0094"
                   STRING
                     "複数病名の入力時は補足コメントの入力はできません"
                                                DELIMITED   BY  SIZE 
                       "ので、クリアします"     DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0099"
                   MOVE
                     "複数病名の入力時は補足コメントの入力はできません"
                                               TO  SPA-ERRMSG   
               WHEN    "0095"
                   MOVE    "補足コメントが長すぎます"
                                               TO  SPA-ERRMSG
               WHEN    "0097"
                   MOVE    "セットコードの病名を全て表示できません"
                                               TO  SPA-ERRMSG
               WHEN    "0098"
                   MOVE
                       "変更処理のときはセットコードの入力はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  CERR
           EVALUATE    SPA-ERRCD
               WHEN    "0075"
                   MOVE    WRK-ERR-BYOMEI   TO  CERR-BYOMEI
           END-EVALUATE
      *
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C04"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           MOVE    SPACE                TO  WRK-HAISIYMDH
                                            WRK-ERR-BYOMEI
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       530-CIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID2-FLG
                                           WRK-CIDMSG
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0104"
                   STRING  "入力された自院病名の病名はマスタ"
                                       DELIMITED  BY  SIZE
                           "ー上で移行病名の扱いとなっています。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "入力された自院病名の病名は「傷病名コードの"
                                       DELIMITED  BY  SIZE
                           "統一の推進」による推奨病名があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID2
           INITIALIZE                      CID2
           MOVE    WRK-CIDMSG          TO  CID2-MSG
           MOVE    SPA-C04-IKOMOTO-BYOMEICD
                                       TO  CID2-BYOMEICD1
           MOVE    SPA-C04-IKOMOTO-BYOMEI  
                                       TO  CID2-BYOMEI1
           MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                       TO  CID2-BYOMEICD2
           MOVE    SPA-C04-IKOSAKI-BYOMEI
                                       TO  CID2-BYOMEI2
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C04"               TO  SPA-MOTOPG
           MOVE    "CID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-CIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       560-CIDSET6-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID6-FLG
                                           WRK-CIDMSG
      *
           INITIALIZE                  CID6
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0104"
                   STRING
                       "入力された自院病名の病名は" 
                                               DELIMITED   BY  SIZE 
                       "移行病名の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "入力された自院病名の病名は「傷病名コードの"
                                               DELIMITED   BY  SIZE
                   "統一の推進」による推奨病名がありますが、別のコード"
                                               DELIMITED   BY  SIZE
                   "で登録が可能です"          DELIMITED   BY  SIZE
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0109"
                   STRING
                       "入力された自院病名の病名は"
                                               DELIMITED   BY  SIZE 
                       "廃止の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0108"
                   STRING
                       "入力された自院病名の病名は"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
           MOVE    WRK-CIDMSG          TO  CID6-MSG
      *
           MOVE    SPA-C04-IKOMOTO-BYOMEICD
                                       TO  CID6-BYOMEICD1
           MOVE    SPA-C04-IKOMOTO-BYOMEI  
                                       TO  CID6-BYOMEI1
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0108"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
               WHEN    "0109"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
                   MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                   MOVE    WRK-HAISIYMDH       TO  CID6-HAISIYMD
               WHEN    OTHER
                   MOVE    SPA-C04-IKOSAKI-BYOMEICD
                                               TO  CID6-BYOMEICD2
                   MOVE    SPA-C04-IKOSAKI-BYOMEI
                                               TO  CID6-BYOMEI2
                   MOVE    WIDGET-NORMAL       TO  CID6-B10-STATE
                   IF      SPA-C04-IKOMOTO-HAISIYMD
                                               >   ZERO
                       MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                       MOVE    SPA-C04-IKOMOTO-HAISIYMD
                                                   TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3    TO  CID6-HAISIYMD
                   END-IF
                   EVALUATE    SPA-CIDCD6
                       WHEN    "0106"
                           STRING
                               "推奨する病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "推奨する病名"  TO  CID6-LABEL1
                           MOVE    "F10:推奨する病名へ変更"
                                                   TO  CID6-B10-LABEL
                       WHEN    OTHER
                           STRING
                               "移行先病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "移行先病名"    TO  CID6-LABEL1
                           MOVE    "F10:移行先病名へ変更"
                                                   TO  CID6-B10-LABEL
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    WIDGET-NORMAL       TO  CID6-B12-STATE
           MOVE    "red"               TO  CID6-MSG2-STYLE
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   6
               MOVE    SPA-C04-CHANGE-BYOMEICD (IDX)
                                       TO  CID6-BYOMEICD (IDX)
               MOVE    "black"         TO  CID6-BYOMEICD-STYLE (IDX)
               IF      SPA-C04-CHANGE-MAX  <=  6
                   EVALUATE    IDX
                       WHEN    1
                       WHEN    3
                       WHEN    5
                           MOVE    "black" TO  CID6-BYOMEICD-STYLE (IDX)
                       WHEN    OTHER
                           MOVE    "blue"  TO  CID6-BYOMEICD-STYLE (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-BYOMEI1
           MOVE    "black"             TO  CID6-BYOMEI10-STYLE
           IF      SPA-C04-CHANGE-MAX  <=  6
               STRING  SPA-C04-CHANGE-BYOMEI (1)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C04-CHANGE-BYOMEI (2)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C04-CHANGE-BYOMEI (3)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C04-CHANGE-BYOMEI (4)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C04-CHANGE-BYOMEI (5)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C04-CHANGE-BYOMEI (6)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                                           INTO    WRK-BYOMEI1
               END-STRING
           ELSE    
               MOVE    SPA-C04-CHANGE-BYOMEI (1)
                                           TO  WRK-BYOMEI1
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       SPA-C04-CHANGE-MAX
                   STRING  WRK-BYOMEI1         DELIMITED   BY  SPACE
                           SPA-C04-CHANGE-BYOMEI (IDX)
                                               DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI1
                   END-STRING
               END-PERFORM
           END-IF
           MOVE    WRK-BYOMEI1          TO  CID6-BYOMEI10
      *
           MOVE    SPACE               TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    SPA-C04-CHANGE-INDEX-ST
                                       BY  1
                   UNTIL   IDX >       SPA-C04-CHANGE-INDEX-ED
               STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                       SPA-NAI-C04-BYOMEI(IDX)
                                           DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
                END-STRING
           END-PERFORM
           MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI1
           MOVE    "blue"              TO  CID6-H-BYOMEI1-STYLE
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0104"
               WHEN    "0106"
                   MOVE    SPACE               TO  WRK-BYOMEI
                   PERFORM VARYING IDX FROM    SPA-C04-CHANGE-INDEX-ST
                                               BY  1
                           UNTIL   IDX >       SPA-C04-CHANGE-INDEX-ED
                       IF      IDX         =   SPA-C04-IKOSAKI-INDEX
                           PERFORM VARYING IDX2    FROM    1   BY  1
                                   UNTIL   IDX2    >       21
                                   OR      SPA-NAI-C04-BYOMEICD-O
                                                           (IDX IDX2)
                                               =   SPACE
                               IF      IDX2        =
                                                SPA-C04-IKOSAKI-INDEX1
                                   STRING  WRK-BYOMEI
                                                   DELIMITED   BY  SPACE
                                           SPA-C04-IKOSAKI-BYOMEI
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                                   END-STRING
                               ELSE
                                   STRING  WRK-BYOMEI
                                                   DELIMITED   BY  SPACE
                                           SPA-NAI-C04-BYOMEI-O
                                                           (IDX IDX2)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                                   END-STRING
                               END-IF 
                           END-PERFORM
                       ELSE
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                   SPA-NAI-C04-BYOMEI (IDX)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       END-IF  
                   END-PERFORM
                   MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI2
           END-EVALUATE
           MOVE    "blue"              TO  CID6-H-BYOMEI2-STYLE
      *
           MOVE    "C04"               TO  SPA-MOTOPG
           MOVE    "CID6"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID6"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       560-CIDSET6-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自院病名マスター読込（主キー）
      *****************************************************************
       900-USBYO-INV-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込
      *****************************************************************
       900-USBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-SEC     SECTION.
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEIINPUTSET-REC
                   MOVE    ZERO                TO  FLG-BYOMEIINPUTSET
               ELSE
                   INITIALIZE                      BYOMEIINPUTSET-REC
                   MOVE    1                   TO  FLG-BYOMEIINPUTSET
               END-IF
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEIINPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-N-SEC   SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BYOMEIINPUTSET-REC
               MOVE    ZERO                TO  FLG-BYOMEIINPUTSET
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           .
       900-BYOMEIINPUTSET-READ-N-EXT.
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
