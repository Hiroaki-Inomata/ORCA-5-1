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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（担当医・変更）
      *                      担当医：歴の枝番を採番して新規に歴作成
      *                        変更：最新の歴を更新
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-BUNBENCHK           PIC 9(01).
           03  FLG-END                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-NYUINKA             PIC 9(05).
           03  CNT-HKNCOMBI            PIC 9(05).
           03  CNT-DR                  PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDX-ODR                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-COMBI-HKNCOMBI      PIC 9(04).
           03  WRK-SYS-KANRICD         PIC X(04).
           03  WRK-SYS-KBNCD           PIC X(08).
           03  WRK-DIALOG              PIC 9(02).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
       01  ITEM-AREA.
           03  ITEM-NYUINKA            PIC X(02).
           03  ITEM-HKNCOMBI           PIC 9(04).
           03  ITEM-TEIKIKBN           PIC X(01).
           03  ITEM-DISPKBN            PIC X(01).
           03  ITEM-DRCD-G.
               05  ITEM-DRCD-MAX       PIC 9(02).
               05  ITEM-DRCD           PIC X(05)   OCCURS  3.
           03  ITEM-BUNBENKBN          PIC X(01).
           03  ITEM-RIYOUKBN           PIC X(01).
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-ITEMNUM-MAX       PIC 9(05)   VALUE 30.
           03  CONST-API               PIC 9(01)   VALUE 2.
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目テーブル
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    入院診療行為照会テーブル作成サブ
           COPY  "CPORCSS010.INC".
      *
      *    分娩情報編集サブ
           COPY    "CPORCSI02.INC".
       01  SNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //SNRRK-//.
      *
      *    入退院登録PUSH通知
           COPY    "CPORCSI03.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  ITEM-AREA
           INITIALIZE                  SODR-OT
           INITIALIZE                  TRRK-REC
      *
           IF    ( SODR-SRCKBN     =    "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
      *
           END-IF
      *
           IF    ( ERRCD-ITEM-G  =   SPACE )
      *
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SODR-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
               PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
               ELSE
                   MOVE    "0051"          TO  ERRCD-ORDER
               END-IF
      *
           END-IF
      *
           IF    ( SODR-ORDERID        =   NORDERID-HENKOID )
               MOVE    PTNYUINRRK-NYUINKA     TO  SODR-SRYKA
                                                  ITEM-NYUINKA
               MOVE    PTNYUINRRK-HKNCOMBINUM TO  SODR-HKNCOMBI
                                                  ITEM-HKNCOMBI
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
      *
               IF    ( FLG-END =   ZERO )
                   IF    ( SODR-ORDERID        =   NORDERID-HENKOID )
                       PERFORM 2101-PTNYUINRRK-UPDATE-SEC
                   ELSE
                       PERFORM 2101-PTNYUINRRK-INSERT-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-SRCKBN =   "1" OR "2" )
                       PERFORM 2101-ORCSI02-CALL-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END =   ZERO )
                   PERFORM 2101-PTINF-UPDATE-SEC
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   PERFORM 2101-ORCSS010-CALL-SEC
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "1" )
                       PERFORM 2101-ORDER-RECONF-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "2" )
                       CONTINUE
                   ELSE
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
       *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-KEYGENERATE
                                           =   CONST-API )
                       PERFORM 2101-USERPG-CALL-SEC
      *
                       PERFORM 2101-PUSHEVENT-SEC
                   END-IF
               END-IF
      *
           ELSE
               IF    ( SODR-PROCKBN    =   "1" OR "2" )
                       CONTINUE
               ELSE
                   IF    ( FLG-END  =   ZERO )
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           MOVE    TRRK-TEIKI-SEIKYUKBN
                                   TO  ITEM-TEIKIKBN
           MOVE    TRRK-KENSAKU-DISPKBN
                                   TO  ITEM-DISPKBN
      *
           PERFORM VARYING IDX-ODR    FROM    1   BY  1
                   UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
               EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
               WHEN    NCLAIMID-NYUINKAJMA
                   PERFORM 20011-NYUINKA-CHK-SEC
               WHEN    NCLAIMID-HKNCOMBIJMA
                   PERFORM 20011-HKNCOMBI-CHK-SEC
               WHEN    NCLAIMID-DRJMA
                   PERFORM 20011-DR-CHK-SEC
               WHEN    NCLAIMID-TEIKIKBNJMA
                   PERFORM 20011-TEIKIKBN-CHK-SEC
               WHEN    NCLAIMID-DISPKBNJMA
                   PERFORM 20011-DISPKBN-CHK-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           IF    ( SODR-SRCKBN     =   "1" OR "2" )
      *
               INITIALIZE              SI02-AREA
               MOVE    "1"         TO  SI02-REQID
               MOVE    TRRK-REC    TO  SNRRK-AREA
               CALL    "ORCSI02"       USING
                                       SI02-AREA
                                       SNRRK-AREA
                                       MCPAREA
                                       SPA-AREA
      *
               MOVE    SI02-BUNBENCHK  TO  FLG-BUNBENCHK
           END-IF
      *
           IF    ( FLG-BUNBENCHK   NOT =   ZERO )
      *
               MOVE    "1"         TO  ITEM-BUNBENKBN
               MOVE    "1"         TO  ITEM-RIYOUKBN
               IF    ( SI02-BUNBENKBN  NOT =   SPACE )
                   MOVE    SI02-BUNBENKBN
                                   TO  ITEM-BUNBENKBN
                   MOVE    SI02-RIYOUKBN
                                   TO  ITEM-RIYOUKBN
               END-IF
      *
               PERFORM VARYING IDX-ODR    FROM    1   BY  1
                       UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-BUNBENKBNJMA
                       PERFORM 20011-BUNBENKBN-CHK-SEC
                   WHEN    NCLAIMID-RIYOUKBNJMA
                       PERFORM 20011-RIYOUKBN-CHK-SEC
                   END-EVALUATE
               END-PERFORM
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20011-NYUINKA-CHK-SEC           SECTION.
      *
           COMPUTE CNT-NYUINKA  =   CNT-NYUINKA  +   1
      *
           IF    ( CNT-NYUINKA  =   1 )
            AND  ( ITEM-NYUINKA =   SPACE )
      *
               MOVE    "1005"          TO  WRK-SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-SYS-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
      *
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-NYUINKA
      *
               ELSE
                   MOVE    "0002"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC           SECTION.
      *
           COMPUTE CNT-HKNCOMBI  =   CNT-HKNCOMBI  +   1
      *
           IF    ( CNT-HKNCOMBI  =   1 )
            AND  ( ITEM-HKNCOMBI =   ZERO )
      *
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-COMBI-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI        =   ZERO )
                   IF    ( COMB-DELKBN NOT =   "1" )
                     AND ( SODR-SRYYMD    >=  COMB-TEKSTYMD )
                     AND ( SODR-SRYYMD    <=  COMB-TEKEDYMD )
      *
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-HKNCOMBI
                   ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                    END-IF
               ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       20011-DR-CHK-SEC                SECTION.
      *
           COMPUTE CNT-DR  =   CNT-DR  +   1
           COMPUTE ITEM-DRCD-MAX   =   ITEM-DRCD-MAX   +   1
      *
           IF    ( CNT-DR  <=  3 )
      *
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "1010"      TO  WRK-SYS-KANRICD
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-SYS-KBNCD
      *
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-DRCD (ITEM-DRCD-MAX)
                   ELSE
                       MOVE    "0025"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               END-IF
      *
           ELSE
               PERFORM 2100-ITEM-UNPROCSTS-SET-SEC
               MOVE    "0026"  TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分チェック処理
      *****************************************************************
       20011-TEIKIKBN-CHK-SEC          SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "1" OR "2" OR "3" )
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-TEIKIKBN
               ELSE
                   MOVE    "0141"          TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-TEIKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院患者照会表示チェック処理
      *****************************************************************
       20011-DISPKBN-CHK-SEC           SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "1" OR "2" )
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-DISPKBN
               ELSE
                   MOVE    "0158"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       20011-BUNBENKBN-CHK-SEC         SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( FLG-BUNBENCHK   NOT =   ZERO )
                   IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "0" OR "1" OR "2" )
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-BUNBENKBN
                   ELSE
                       MOVE    "0162"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       20011-RIYOUKBN-CHK-SEC         SECTION.
      *
           IF    ( ITEM-BUNBENKBN         =    "1" OR "2" )
            AND  ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "0" OR "1" )
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-RIYOUKBN
               ELSE
                   MOVE    "0163"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目未処理状態設定処理
      *****************************************************************
       2100-ITEM-UNPROCSTS-SET-SEC        SECTION.
      *
           MOVE    3               TO  LNK-ODRITEM-STATUS (IDX-ODR)
      *
           .
       2100-ITEM-UNPROCSTS-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目チェックエラー設定処理
      *****************************************************************
       2100-ITEM-ERR-SET-SEC           SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
               MOVE    ERRCD-ERRCD TO  ERRCD-ITEM
           END-IF
      *
           IF  ( LNK-ODRITEM-STATUS (IDX-ODR)  =   SPACE )
               MOVE    2       TO  LNK-ODRITEM-STATUS (IDX-ODR)
           END-IF
      *
           MOVE    ERRCD-ERRCD TO  LNK-ODRITEM-ERRCD (IDX-ODR)
      *
           .
       2100-ITEM-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院科チェック処理
               PERFORM 20021-NYUINKA-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        保険組合せチェック処理
               PERFORM 20021-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        担当医チェック処理
               PERFORM 20021-DR-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院履歴チェック処理
               PERFORM 20021-NYUINRRK-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20021-NYUINKA-CHK-SEC           SECTION.
      *
           IF    ( SODR-ORDERID        =   NORDERID-IDODRID )
               IF    ( ITEM-NYUINKA    =   SPACE )
                   MOVE    "0002"          TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       20021-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20021-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF    ( SODR-ORDERID        =   NORDERID-IDODRID )
               IF    ( ITEM-HKNCOMBI   =   ZERO )
                   MOVE    "0005"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       20021-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       20021-DR-CHK-SEC               SECTION.
      *
           IF    ( SODR-ORDERID        =   NORDERID-IDODRID )
               IF    ( ITEM-DRCD (1) =    SPACE )
                AND  ( ITEM-DRCD (2) =    SPACE )
                AND  ( ITEM-DRCD (3) =    SPACE )
                   MOVE    "0099"         TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       20021-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴チェック処理
      *****************************************************************
       20021-NYUINRRK-CHK-SEC          SECTION.
      *
           MOVE    TRRK-REC            TO  PTNYUINRRK-REC
      *
           IF    ( SODR-ORDERID        =   NORDERID-IDODRID )
      *        診療科と保険組合せの変更は不可
               IF    ( PTNYUINRRK-NYUINKA  NOT =   ITEM-NYUINKA )
                   MOVE    "0100"      TO  ERRCD-ORDER
                   GO  TO  20021-NYUINRRK-CHK-EXT
               END-IF
      *
               IF    ( PTNYUINRRK-HKNCOMBINUM
                                           NOT =   ITEM-HKNCOMBI )
                   MOVE    "0070"      TO  ERRCD-ORDER
                   GO  TO  20021-NYUINRRK-CHK-EXT
               END-IF
           END-IF
      *
      *
      *    入院中であること
           IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
               MOVE    "0052"      TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-NYUINYMD     >  SODR-SRYYMD )
               MOVE    "0097"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-TENNYUYMD    >  SODR-SRYYMD )
               MOVE    "0098"  TO  ERRCD-ORDER
               GO  TO  20021-NYUINRRK-CHK-EXT
           END-IF
      *
           .
       20021-NYUINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー情報再編成処理
      *****************************************************************
       2101-ORDER-RECONF-SEC           SECTION.
      *
           CALL    "ORCSODRS01"        USING   SODR-AREA
                                               SODRS01-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           MOVE    SODRS01-RC      TO  FLG-END
      *
           .
       2101-ORDER-RECONF-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴追加処理
      *****************************************************************
       2101-PTNYUINRRK-INSERT-SEC      SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
           MOVE    SODR-SRYYMD     TO  PTNYUINRRK-TENSTUYMD
           MOVE    SODR-OPID       TO  PTNYUINRRK-OPID
           MOVE    SODR-UPYMD      TO  PTNYUINRRK-UPYMD
           MOVE    SODR-UPHMS      TO  PTNYUINRRK-UPHMS
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 001 :PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-INSERT-EXT
           END-IF
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
           COMPUTE PTNYUINRRK-RRKEDANUM
               =   PTNYUINRRK-RRKEDANUM  +   1
      *
           MOVE    SODR-SRYYMD     TO  PTNYUINRRK-TENNYUYMD
           MOVE    "99999999"      TO  PTNYUINRRK-TENSTUYMD
      *
           MOVE    ITEM-DRCD (1)   TO  PTNYUINRRK-DRCD1
           MOVE    ITEM-DRCD (2)   TO  PTNYUINRRK-DRCD2
           MOVE    ITEM-DRCD (3)   TO  PTNYUINRRK-DRCD3
      *
           MOVE    "7"             TO  PTNYUINRRK-JTIKBN
      *
           MOVE    ITEM-TEIKIKBN   TO  PTNYUINRRK-TEIKI-SEIKYUKBN
           MOVE    ITEM-DISPKBN    TO  PTNYUINRRK-KENSAKU-DISPKBN
      *
           MOVE    SODR-OPID       TO  PTNYUINRRK-OPID
           MOVE    SODR-UPYMD      TO  PTNYUINRRK-CREYMD
                                       PTNYUINRRK-UPYMD
           MOVE    SODR-UPHMS      TO  PTNYUINRRK-UPHMS
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 002 :PTNYUINRRK INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-INSERT-EXT
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 009 :PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-INSERT-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  TRRK-REC
      *
           .
       2101-PTNYUINRRK-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴追加処理
      *****************************************************************
       2101-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
           MOVE    ITEM-DRCD (1)   TO  PTNYUINRRK-DRCD1
           MOVE    ITEM-DRCD (2)   TO  PTNYUINRRK-DRCD2
           MOVE    ITEM-DRCD (3)   TO  PTNYUINRRK-DRCD3
      *
           MOVE    ITEM-TEIKIKBN   TO  PTNYUINRRK-TEIKI-SEIKYUKBN
           MOVE    ITEM-DISPKBN    TO  PTNYUINRRK-KENSAKU-DISPKBN
      *
           MOVE    SODR-OPID       TO  PTNYUINRRK-OPID
           MOVE    SODR-UPYMD      TO  PTNYUINRRK-UPYMD
           MOVE    SODR-UPHMS      TO  PTNYUINRRK-UPHMS
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 002 :PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-UPDATE-EXT
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 009 :PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTNYUINRRK-UPDATE-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  TRRK-REC
      *
           .
       2101-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       2101-ORDER-INSERT-SEC           SECTION.
      *
           PERFORM 21011-ORDER-HEN-SEC
      *
           PERFORM VARYING IDX-ODR FROM    1   BY  1
                   UNTIL ( IDX-ODR >   LNK-ODRITEM-MAX )
                    OR   ( FLG-END =   1 )
      *
                PERFORM 21011-ODRITEM-HEN-SEC
      *
           END-PERFORM
      *
           .
       2101-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム実行処理
      *****************************************************************
       2101-USERPG-CALL-SEC            SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "I01"               TO  ORCSUSERCHK-GMNPG
           MOVE    "2"                 TO  ORCSUSERCHK-SYORI
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
           MOVE    TRRK-TENNYUYMD      TO  ORCSUSERCHK-SRYYMD
           MOVE    SODR-PTID           TO  ORCSUSERCHK-PTID
           MOVE    SODR-PTNUM          TO  ORCSUSERCHK-PTNUM
           MOVE    TRRK-NYUINKA        TO  ORCSUSERCHK-SRYKA
           MOVE    TRRK-HKNCOMBINUM    TO  ORCSUSERCHK-HKNCOMBI
           MOVE    TRRK-DRCD1          TO  ORCSUSERCHK-DRCD
           IF    ( SODR-ORDERID        =   NORDERID-HENKOID )
                MOVE    03             TO  ORCSUSERCHK-SYORI-FLG
           ELSE
                MOVE    08             TO  ORCSUSERCHK-SYORI-FLG
           END-IF
           CALL    "ORCSUSERCHK"           USING
                                           ORCSUSERCHKAREA
                                           SPA-AREA
      *
           .
       2101-USERPG-CALL-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       2101-PUSHEVENT-SEC            SECTION.
      *
           INITIALIZE                      SI03-AREA
      *
           IF    ( SODR-ORDERID        =   NORDERID-HENKOID )
               MOVE    "03"            TO  SI03-REQNUM
           ELSE
               MOVE    "08"            TO  SI03-REQNUM
           END-IF
           MOVE    SODR-PTNUM          TO  SI03-PTNUM
           MOVE    TRRK-NYUINYMD       TO  SI03-NYUINYMD
           CALL    "ORCSI03"           USING
                                       SI03-AREA
                                       SPA-AREA
      *
           .
       2101-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    オーダ編集処理
      *****************************************************************
       21011-ORDER-HEN-SEC             SECTION.
      *
           INITIALIZE                      ORDER-REC
      *
           MOVE   SODR-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE   SODR-KARTE-KEY       TO  ORDER-KARTE-KEY
           MOVE   SODR-ORDERNUM        TO  ORDER-ORDERNUM
           MOVE   SODR-NYUGAIKBN       TO  ORDER-NYUGAIKBN
           MOVE   SODR-PTNUM           TO  ORDER-PTNUM
           MOVE   SODR-PTID            TO  ORDER-PTID
           MOVE   SODR-SRYKA           TO  ORDER-SRYKA
           MOVE   SODR-SRYYMD          TO  ORDER-SRYYMD
           MOVE   SODR-HKNCOMBI        TO  ORDER-HKNCOMBI
           MOVE   SODR-ORDERID         TO  ORDER-ORDERID
           MOVE   SODR-UKEYMD          TO  ORDER-UKEYMD
           MOVE   SODR-UKEHMS          TO  ORDER-UKEHMS
           MOVE   SODR-KEYGENERATE     TO  ORDER-KEYGENERATE
      *
           MOVE   ERRCD-ORDER          TO  ORDER-ERRCD1
           MOVE   ERRCD-ITEM           TO  ORDER-ERRCD2
      *
           IF    ( ERRCD-ITEM          =   SPACE )
            AND  ( ERRCD-ORDER         =   SPACE )
               MOVE    TRRK-RRKNUM     TO  ORDER-RRKNUM
               MOVE    TRRK-RRKEDANUM  TO  ORDER-RRKEDANUM
           ELSE
               IF    ( WRK-DIALOG      =   ZERO )
                   MOVE    2           TO  ORDER-STATUS
               ELSE
                   MOVE    5           TO  ORDER-STATUS
               END-IF
           END-IF
      *
           IF    ( SODR-PROCKBN    =   "1" )
               MOVE     1              TO  ORDER-MODIFY
           END-IF
      *
           MOVE    SODR-OPID           TO  ORDER-OPID
           MOVE    SODR-UPYMD          TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SODR-UPHMS          TO  ORDER-UPHMS
      *
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           IF    ( FLG-ORDER           =   ZERO )
               DISPLAY "ORCSDR05 ERROR 003:ORDER INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ORDER-HEN-EXT
           END-IF
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 004:ORDER INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ORDER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目編集処理
      *****************************************************************
       21011-ODRITEM-HEN-SEC           SECTION.
      *
           MOVE    LNK-ODRITEM-OCC (IDX-ODR)
                                       TO  ODRITEM-REC
      *
           IF    ( LNK-ODRITEM-ITEMID (IDX-ODR)
                                       =   NCLAIMID-NYUINKAJMA
                                       OR  NCLAIMID-HKNCOMBIJMA
                                       OR  NCLAIMID-DRJMA
                                       OR  NCLAIMID-TEIKIKBNJMA
                                       OR  NCLAIMID-DISPKBNJMA
                                       OR  NCLAIMID-BUNBENKBNJMA
                                       OR  NCLAIMID-RIYOUKBNJMA
                                        )
               CONTINUE
           ELSE
               MOVE    3               TO  ODRITEM-STATUS
               MOVE    "9001"          TO  ODRITEM-ERRCD
           END-IF
      *
           MOVE    SODR-OPID           TO  ODRITEM-OPID
           MOVE    SODR-UPYMD          TO  ODRITEM-CREYMD
                                           ODRITEM-UPYMD
           MOVE    SODR-UPHMS          TO  ODRITEM-UPHMS
      *
      *
           PERFORM 900-ODRITEM-KEY-SEL-SEC
           IF    ( FLG-ODRITEM           =   ZERO )
               DISPLAY "ORCSDR05 ERROR 005:ODRITEM INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ODRITEM-HEN-EXT
           END-IF
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 006:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理情報更新処理
      *****************************************************************
       2101-ORCSI02-CALL-SEC           SECTION.
      *
           INITIALIZE                  SI02-AREA
           MOVE    "2"             TO  SI02-REQID
           MOVE    ITEM-BUNBENKBN  TO  SI02-BUNBENKBN
           MOVE    ITEM-RIYOUKBN   TO  SI02-RIYOUKBN
           MOVE    TRRK-REC        TO  SNRRK-AREA
           CALL    "ORCSI02"       USING
                                   SI02-AREA
                                   SNRRK-AREA
                                   MCPAREA
                                   SPA-AREA
           IF     ( MCP-RC         NOT =  ZERO )
               DISPLAY "ORCSDR05 ERROR 010:BUNBEN UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-ORCSI02-CALL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       2101-PTINF-UPDATE-SEC           SECTION.
      *
      *    入院歴より最新の歴情報を取得
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SODR-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
      *
      *    患者マスタ検索
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SODR-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SODR-PTID           TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTINF           NOT =   ZERO )
               DISPLAY "ORCSDR05 ERROR 007:PTINF UPDATE CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTINF-UPDATE-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           MOVE   PTNYUINRRK-RRKNUM
                                       TO  PTINF-RRKNUM
           MOVE   PTNYUINRRK-RRKEDANUM
                                       TO  PTINF-RRKEDANUM
      *
           MOVE    SODR-OPID           TO  PTINF-OPID
           MOVE    SODR-UPYMD          TO  PTINF-UPYMD
           MOVE    SODR-UPHMS          TO  PTINF-UPHMS
      *
      *    患者情報更新
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF     ( MCP-RC         NOT =  ZERO )
               DISPLAY "ORCSDR05 ERROR 008:PTINF UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-PTINF-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会テーブル作成処理
      *****************************************************************
       2101-ORCSS010-CALL-SEC          SECTION.
      *
           INITIALIZE                  SS010-AREA
           MOVE    SODR-PTID           TO  SS010-PTID
           MOVE    SODR-SRYYMD (1:6)   TO  SS010-SRYYM
           CALL    "ORCSS010"      USING   SS010-AREA
                                           SPA-AREA
           IF    ( SS010-RC        NOT =   ZERO )
               DISPLAY "ORCSS010 ERROR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-ORCSS010-CALL-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
            AND  ( ERRCD-ORDER     =   SPACE )
               MOVE    ZERO        TO  SODR-ERRFLG
           ELSE
               MOVE    1           TO  SODR-ERRFLG
           END-IF
      *
           MOVE    ERRCD-ORDER     TO  SODR-ERRCD
      *
           MOVE    FLG-END         TO  SODR-RC
      *
           IF    ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               MOVE    TRRK-RRKNUM TO  SODR-RRKNUM
               MOVE    TRRK-RRKEDANUM
                                   TO  SODR-RRKEDANUM
               MOVE    TRRK-SHONUM TO  SODR-SHONUM
           END-IF
      *
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY3)
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目検索処理(KEY)
      *****************************************************************
       900-ODRITEM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ODRITEM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-SYS-KBNCD       TO  SYS-KBNCD
           MOVE    SODR-SRYYMD         TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SODR-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SODR-PTID           TO  COMB-PTID
           MOVE    WRK-COMBI-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＳＨＥＶＥＮＴ処理
      *****************************************************************
       910-PUSHEVENT-SEC               SECTION.
      *
           MOVE    "PUSHEVENT"         TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理（チェックのみ）
      *****************************************************************
       990-LOCK-CHK-SEC                SECTION.
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    排他制御処理
           MOVE    "I01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SODR-PTID           TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  ERRCD-ORDER
           END-IF
      *
      *    排他制御解除処理
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
      *
           .
       990-LOCK-CHK-EXT.
           EXIT.
      *
