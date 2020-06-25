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
       PROGRAM-ID.                     ORCBD009S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 入院オーダーエラーリスト編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
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
           03  FLG-KHNERR              PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
           03  IDX-MAX                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX         PIC 9(05).
               05  WRK-CMB-ITM         PIC X(200).
               05  WRK-CMB-ITM2        PIC X(200).
           03  WRK-STR                 PIC X(500).
      *
       01  ERR-ORDER-ERRCD             PIC X(04).
       01  ERR-ODRITEM-REC.
           02  ERR-ODRITEM-MAX         PIC 9(03).
           02  ERR-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //ERR-ODRITEM-//.
       01  CONST-AREA.
           03  CONST-ERRCD-MAX             PIC 9(05)   VALUE 100.
           03  CONST-ERRCD9001             PIC X(04)   VALUE "9001".
           03  CONST-ODRITEM-MAX           PIC 9(05)   VALUE 30.
           03  CONST-ITEM-MAX              PIC 9(05)   VALUE 80.
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    エラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
      *    入院オーダーエラーメッセージ取得
           COPY    "CPORCSODRS03.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  SODRITEM-REC.
           02  SODRITEM-MAX            PIC 9(03).
           02  SODRITEM-OCC            OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //SODRITEM-//.
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院オーダーエラーリスト編集サブ
           COPY    "CPORCBD009S.INC".
       01  LNK-ORDER-REC.
           COPY    "CPORDER.INC"       REPLACING  //ORDER-//
                                       BY         //LNK-ORDER-//.
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               ORCSBD009SAREA
                                               LNK-ORDER-REC
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
           PERFORM 200-MAIN-SEC
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
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSBD009SAREA
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
           PERFORM 4101-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       4101-EDIT-SEC                   SECTION.
      *
           IF    ( LNK-ORDER-STATUS    =   4 )
               PERFORM 41011-DIRECT-UPD-KHNCHK-SEC
               IF    ( FLG-KHNERR  NOT =   ZERO )
                   GO  TO  4101-EDIT-EXT
               END-IF
           ELSE
               PERFORM 41011-KHNERR-HEN-SEC
           END-IF
      *
           .
       4101-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    基本項目エラー時編集処理
      *****************************************************************
       41011-KHNERR-HEN-SEC            SECTION.
      *
           INITIALIZE                  ERR-ODRITEM-REC
      *
           PERFORM 900-ODRITEM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ODRITEM   NOT =   ZERO )
      *
               IF    ( ERR-ODRITEM-MAX   >=  CONST-ODRITEM-MAX )
                   CONTINUE
               ELSE
                   COMPUTE ERR-ODRITEM-MAX   =   ERR-ODRITEM-MAX   +   1
                   MOVE    ODRITEM-REC   TO  ERR-ODRITEM-OCC
                                                (ERR-ODRITEM-MAX)
               END-IF
      *
               PERFORM 900-ODRITEM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    LNK-ORDER-ERRCD1    TO  ERR-ORDER-ERRCD
           PERFORM 410111-EDIT-ERRMES-SEC
      *
           .
       41011-KHNERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    直接更新時基本項目チェック処理
      *****************************************************************
       41011-DIRECT-UPD-KHNCHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-KHNERR
      *
           INITIALIZE                  SODR-AREA
           INITIALIZE                  SODRITEM-REC
      *
           MOVE    LNK-ORDER-HOSPNUM   TO  SODR-HOSPNUM
           MOVE    LNK-ORDER-KARTE-KEY TO  SODR-KARTE-KEY
           MOVE    LNK-ORDER-ORDERNUM  TO  SODR-ORDERNUM
           CALL    "ORCSODRS02"        USING   SODR-AREA
                                               SODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           IF    ( SODR-ERRFLG NOT =   ZERO )
      *
               MOVE    1               TO  FLG-KHNERR
               MOVE    SODR-ERRCD      TO  ERR-ORDER-ERRCD
               MOVE    SODRITEM-REC    TO  ERR-ODRITEM-REC
               PERFORM 410111-EDIT-ERRMES-SEC
      *
           END-IF
      *
           .
       41011-DIRECT-UPD-KHNCHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       410111-EDIT-ERRMES-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           DISPLAY "ERR-ORDER-ERRCD =" ERR-ORDER-ERRCD
      *
           IF    ( ERR-ORDER-ERRCD NOT =   SPACE )
               INITIALIZE              SODRS03-AREA
               MOVE    ERR-ORDER-ERRCD TO  SODRS03-ERRCD
               CALL    "ORCSODRS03"    USING
                                       SODRS03-AREA
               MOVE    SODRS03-ERRCD   TO  BD009S-ERRCD (1)
               MOVE    SODRS03-ERRMSG  TO  BD009S-ERRMES(1)
               MOVE    1               TO  IDX-MAX
               DISPLAY IDX-MAX ODRITEM-ERRCD "-" BD009S-ERRMES(IDX-MAX)
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ERR-ODRITEM-MAX )
      *
               MOVE    ERR-ODRITEM-OCC (IDX0)  TO  ODRITEM-REC
      *
               IF    ( ODRITEM-ERRCD    NOT =   SPACE )
                AND  ( ODRITEM-ERRCD    NOT =   CONST-ERRCD9001 )
                   IF    ( IDX-MAX    >=   CONST-ERRCD-MAX )
                       CONTINUE
                   ELSE
                       COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                       INITIALIZE              SODRS03-AREA
                       MOVE    ODRITEM-ERRCD
                                           TO  SODRS03-ERRCD
                       CALL    "ORCSODRS03"    USING
                                               SODRS03-AREA
                       MOVE    SODRS03-ERRCD
                                           TO  BD009S-ERRCD (IDX-MAX)
                       MOVE    SODRS03-ERRMSG
                                           TO  BD009S-ERRMES(IDX-MAX)
               DISPLAY IDX-MAX ODRITEM-ERRCD "-" BD009S-ERRMES(IDX-MAX)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  BD009S-ERR-CNT
      *
           .
       410111-EDIT-ERRMES-EXT.
           EXIT.
      *****************************************************************
      *    オーダーエラーコードチェック処理
      *****************************************************************
       4200-ERRCD-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMERRCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMERRCDL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    NCLAIMERRCDLMEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー項目名称チェック処理
      *****************************************************************
       4200-NCLAIMID-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMID-ITEM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMID-ITEMJMA (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    NCLAIMID-ITEMMES  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NCLAIMID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目検索(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    LNK-ORDER-HOSPNUM   TO  ODRITEM-HOSPNUM
           MOVE    LNK-ORDER-KARTE-KEY TO  ODRITEM-KARTE-KEY
           MOVE    LNK-ORDER-ORDERNUM  TO  ODRITEM-ORDERNUM
           MOVE    ODRITEM-REC           TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  ODRITEM-REC
           ELSE
               INITIALIZE                  ODRITEM-REC
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダ項目FETCH(KEY4)
      *****************************************************************
       900-ODRITEM-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_odritem"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  ODRITEM-REC
           ELSE
               INITIALIZE              ODRITEM-REC
               MOVE    1           TO  FLG-ODRITEM
           END-IF
      *
           .
       900-ODRITEM-KEY4-FET-EXT.
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
       910-DBSELECT-EXT.
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
       910-DBSELECT-EXT.
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
