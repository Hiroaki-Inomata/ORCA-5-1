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
       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      ORCGP02W3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 保険組合せＩＤ付番
      *  コンポーネント名  : 保険組合せＩＤ付番
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  05/11/14  MONFUNC 対応他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       INPUT-OUTPUT                     SECTION.
       FILE-CONTROL.
      *
      *
       DATA                             DIVISION.
      *FILE                             SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-PARA                          PIC 9(01).
           03  FLG-NC01                          PIC 9(01).
           03  FLG-HKNCOMBI                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARAEDANUM                    PIC 9(04).
           03  CNT-AUTOCOMBINUM                  PIC 9(04).
      *
      *    インデックス領域
       01  INDEX-AREA.
           03  IDX1                              PIC 9(04).
           03  IDX2                              PIC 9(04).
      *
      *    該当レコード番号収納
       01  WRK-RECEDATBL.
           03  WRK-RECEDA                    PIC 9(04) OCCURS 4 TIMES.
      *    新組合せパラメタデータ用レイアウト
       01  WRK-NC01PAREA-AREA.
           03  WRK-NC01PARA-OCC                OCCURS   999.
               05  NC01PARA-REC.
                   07  PARA-NC01-REC           PIC X(500).
                   07  PARA-NC01-FLG           PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組み合わせ（ＤＢ用）
       01  DBHKNCOMBI-REC.
           COPY  "CPHKNCOMBI.INC" REPLACING  //COMB-// BY //DBCOMB-//.
      *
      *    保険組み合わせ（ファイル用）
       01  HKNCOMBI-REC.
           COPY  "CPHKNCOMBI.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                                  SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *
       PROCEDURE                                 DIVISION    USING
           SPA-AREA
           SPA-P02KYOUTU
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
      *    組み合わせパラメタを削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  FLG-AREA
      *
      *    パラメタファイルに作成した新しい組合せをテーブルへ
           PERFORM 100-NC01AREA-TBL-SEC
      *
      *  　既存の組合せ番号をあたらしい組合せに継続する
           PERFORM 100-NUMBCONT-SEC
      *　  新規の組み合わせに番号を採番する
           IF  SPA-ERRCD             =  SPACE
               PERFORM  200-NUMBNEW-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT  PROGRAM.
      *
      *****************************************************************
      *    パラメタファイルに作成した新しい組合せをテーブル保存
      *****************************************************************
       100-NC01AREA-TBL-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-NC01PAREA-AREA
           INITIALIZE                  WRK-NC01PAREA-AREA
           MOVE    ZERO                TO  IDX2
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "NC01PARA"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1                   TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =  1  )  OR
                      (IDX2                >=  999)
               ADD     1                   TO  IDX2
               MOVE    PARA-DATA-REC       TO  NC01PARA-REC(IDX2)
               MOVE    ZERO                TO  PARA-NC01-FLG (IDX2)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-NC01AREA-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号継続検索ルーチン
      *****************************************************************
       100-NUMBCONT-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-PARAEDANUM
      *    組合せ削除データ数（パラメータファイル内）
           MOVE    ZERO                TO  SPA-P02-COMBDCNT
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                      DBHKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  DBCOMB-HOSPNUM
           MOVE    SPA-PTID            TO  DBCOMB-PTID
           MOVE    SPACE               TO  DBCOMB-DELKBN
           MOVE    DBHKNCOMBI-REC      TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 812-COMBIFET-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
           PERFORM
               UNTIL  (FLG-HKNCOMBI        =   1 )  OR
                      (SPA-ERRCD       NOT =   SPACE)
               IF      DBCOMB-HKNCOMBINUM    <  9000
                   PERFORM 1001-NUMBCONT-SEC
                   IF     (WRK-RECEDA(1)   NOT =  ZERO)  OR
                          (WRK-RECEDA(2)   NOT =  ZERO)  OR
                          (WRK-RECEDA(3)   NOT =  ZERO)  OR
                          (WRK-RECEDA(4)   NOT =  ZERO)  
                       IF      CNT-PARAEDANUM      =   999
                           MOVE    "0010"              TO  SPA-ERRCD
                       ELSE
                           PERFORM  1002-COMBIWRT-SEC
                       END-IF
                   ELSE
                       IF      CNT-PARAEDANUM      =   999
                           MOVE    "0010"              TO  SPA-ERRCD
                       ELSE
                           PERFORM  1003-COMBIDEL-SEC
                       END-IF
                   END-IF
               END-IF
               IF      SPA-ERRCD           =  SPACE
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 812-COMBIFET-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
           .
       100-NUMBCONT-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号継続検索ルーチン
      *****************************************************************
       1001-NUMBCONT-SEC                     SECTION.
      *
      *H26.4
      *    労災・自賠責の本人家族区分は空白とする
           IF    (DBCOMB-HKNNUM            =   "971"
                                           OR  "973")
             AND (DBCOMB-HONKZKKBN     NOT =   SPACE)
                 MOVE    SPACE             TO  DBCOMB-HONKZKKBN
           END-IF
      *
           INITIALIZE                  WRK-RECEDATBL
      *
           PERFORM     VARYING     IDX2    FROM    1   BY  1
               UNTIL  (IDX2                >   999 )  OR
                      (PARA-NC01-REC(IDX2) =   SPACE )
             MOVE  PARA-NC01-REC (IDX2)      TO HKNCOMBI-REC
      *!!
      *      労災・自賠責の本人家族区分は空白とする
             IF    (COMB-HKNNUM            =   "971"
                                           OR  "973")
               AND (COMB-HONKZKKBN     NOT =   SPACE)
                 MOVE    SPACE             TO  COMB-HONKZKKBN
             END-IF
      *
             IF  COMB-HOSPNUM                =  DBCOMB-HOSPNUM     AND
                 COMB-PTID                   =  DBCOMB-PTID        AND
                 COMB-HKNID                  =  DBCOMB-HKNID       AND
                 COMB-HKNNUM                 =  DBCOMB-HKNNUM      AND
                 COMB-HONKZKKBN              =  DBCOMB-HONKZKKBN   AND
                 COMB-HOJOKBN                =  DBCOMB-HOJOKBN     AND
                 COMB-CONTKBN                =  DBCOMB-CONTKBN     AND
                 COMB-KOH1ID                 =  DBCOMB-KOH1ID      AND
                 COMB-KOH1HKNNUM             =  DBCOMB-KOH1HKNNUM  AND
                 COMB-KOH2ID                 =  DBCOMB-KOH2ID      AND
                 COMB-KOH2HKNNUM             =  DBCOMB-KOH2HKNNUM  AND
                 COMB-KOH3ID                 =  DBCOMB-KOH3ID      AND
                 COMB-KOH3HKNNUM             =  DBCOMB-KOH3HKNNUM  AND
                 COMB-KOH4ID                 =  DBCOMB-KOH4ID      AND
                 COMB-KOH4HKNNUM             =  DBCOMB-KOH4HKNNUM  AND
                 PARA-NC01-FLG (IDX2)        =  ZERO
      *　　　　　適用開始年月日、適用終了年月日が共に同じデータ
                 IF  (COMB-TEKSTYMD          =  DBCOMB-TEKSTYMD )  AND
                     (COMB-TEKEDYMD          =  DBCOMB-TEKEDYMD )  AND
                     (WRK-RECEDA(1)          =  ZERO            )
                     MOVE    IDX2            TO  WRK-RECEDA(1)
                 END-IF
      *　　　　　適用開始年月日のみ同じデータ
                 IF (COMB-TEKSTYMD           =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD       NOT =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(2)           =  ZERO            )
                     MOVE    IDX2            TO WRK-RECEDA(2)
                 END-IF
      *　　　　　適用終了年月日のみ同じデータ
                 IF (COMB-TEKSTYMD       NOT =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD           =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(3)           =  ZERO            )
                     MOVE    IDX2            TO WRK-RECEDA(3)
                 END-IF
      *　　　　　適用年月日が一部重複しているデータ
                 IF (COMB-TEKSTYMD       NOT =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD       NOT =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(4)           =  ZERO            )  AND
                   ((COMB-TEKSTYMD           >  DBCOMB-TEKSTYMD    AND
                     COMB-TEKSTYMD           <  DBCOMB-TEKEDYMD)   OR
                    (COMB-TEKEDYMD           >  DBCOMB-TEKSTYMD    AND
                     COMB-TEKEDYMD           <  DBCOMB-TEKEDYMD)   OR
                    (COMB-TEKSTYMD           <  DBCOMB-TEKSTYMD    AND
                     COMB-TEKEDYMD           >  DBCOMB-TEKEDYMD))
                     MOVE    IDX2            TO WRK-RECEDA(4)
                 END-IF
             END-IF
      *
             IF      WRK-RECEDA(1)       NOT =  ZERO
                 MOVE   999              TO  IDX2
             END-IF
           END-PERFORM
      *
           .
       1001-NUMBCONT-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号継続時出力ルーチン
      *****************************************************************
       1002-COMBIWRT-SEC                         SECTION.
      *
      *  入力組み合わせデータに処理済フラグを立てる
           EVALUATE    TRUE
               WHEN    WRK-RECEDA(1)   NOT =   ZERO
                   MOVE    1               TO  IDX1
               WHEN    WRK-RECEDA(2)   NOT =   ZERO
                   MOVE    2               TO  IDX1
               WHEN    WRK-RECEDA(3)   NOT =   ZERO
                   MOVE    3               TO  IDX1
               WHEN    WRK-RECEDA(4)   NOT =   ZERO
                   MOVE    4               TO  IDX1
           END-EVALUATE
           MOVE    WRK-RECEDA(IDX1)    TO  IDX2
           IF      IDX2                =   ZERO
               CONTINUE
           ELSE
               PERFORM 10021-COMBIWRT2-SEC
           END-IF
           .
       1002-COMBIWRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号継続時出力ルーチン
      *****************************************************************
       10021-COMBIWRT2-SEC                         SECTION.
      *
           MOVE    PARA-NC01-REC (IDX2)
                                       TO  HKNCOMBI-REC
           MOVE    1                   TO  PARA-NC01-FLG (IDX2)
      *
      *    パラメータデータに内容を出力する
      *    保険組合せ
           COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM  +  1
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    DBCOMB-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
               MOVE    DBCOMB-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           .
       10021-COMBIWRT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号削除時出力ルーチン
      *****************************************************************
       1003-COMBIDEL-SEC                         SECTION.
      *
      *  パラメータデータに内容を出力する
           COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM  +  1
      *    保険組合せ
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    "1"                 TO  DBCOMB-DELKBN
               MOVE    DBHKNCOMBI-REC      TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        組合せ削除データ数（パラメータファイル内）
               ADD    1                    TO  SPA-P02-COMBDCNT
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
               MOVE    "1"                 TO  DBCOMB-DELKBN
               MOVE    DBHKNCOMBI-REC      TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        組合せ削除データ数（パラメータファイル内）
               ADD    1                    TO  SPA-P02-COMBDCNT
           END-IF
           .
       1003-COMBIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規組合せ出力ルーチン
      *****************************************************************
       200-NUMBNEW-SEC                           SECTION.
      *
      *    ＳＰＡに保存してある
           MOVE    SPA-MAE-AUTOCOMBINUM    TO  CNT-AUTOCOMBINUM
           IF      SPA-MAE-AUTOCOMBINUM    =   8999
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
      ***  INITIALIZE                      PARA-REC
      *    MOVE    "P02"               TO  PARA-GYOUMUID
      *    MOVE    SPA-TERMID          TO  PARA-TERMID
      *    MOVE    "NC01PARA"          TO  PARA-FILEMEI
      *
      *    MOVE    PARA-REC            TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "PARA-KEY2"         TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "PARA-KEY2"         TO  ORC-DBPATH
      *        PERFORM 990-PARA-READ-SEC
      *    ELSE
      *        INITIALIZE                  PARA-REC
      *        MOVE    1                   TO  FLG-PARA
      *****END-IF
      *****MOVE    SPACE               TO  SPA-ERRCD
      *
           PERFORM     VARYING     IDX2    FROM    1   BY  1
               UNTIL  (IDX2                >   999 )  OR
                      (PARA-NC01-REC(IDX2) =   SPACE )  OR
                      (SPA-ERRCD       NOT =   SPACE )
               IF      PARA-NC01-FLG (IDX2)  =   ZERO
                   MOVE    PARA-NC01-REC (IDX2)  TO  HKNCOMBI-REC
                   IF      CNT-AUTOCOMBINUM    =   8999
                       MOVE    "0011"              TO  SPA-ERRCD
                   ELSE
                       COMPUTE CNT-AUTOCOMBINUM    =   CNT-AUTOCOMBINUM
                                                   +   1
                   END-IF
                   IF      CNT-PARAEDANUM      =   9999
                       MOVE  "0010"                TO SPA-ERRCD
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
      *  パラメータデータに内容を出力する
                       COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM
                                                   +   1
                       MOVE    CNT-AUTOCOMBINUM    TO  COMB-HKNCOMBINUM
      *  新規設定を変更区分に設定（他で使用しないこと）
                       MOVE    "A"                 TO  COMB-CHGKBN
      *
                       PERFORM 2001-PARAHKNCOMBI-INS-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        ＳＰＡの保険組合せ番号変更
               MOVE    CNT-AUTOCOMBINUM    TO  SPA-PTNUM-AUTOCOMBINUM
           END-IF
      *
           .
       200-NUMBNEW-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ追加ルーチン
      *****************************************************************
       2001-PARAHKNCOMBI-INS-SEC       SECTION.
      *    保険組合せ
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       2001-PARAHKNCOMBI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *
      *****************************************************************
      *    保険組合せＦＥＴＣＨ処理
      *****************************************************************
       812-COMBIFET-SEC                          SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBHKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  DBHKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
      *
           .
       812-COMBIFET-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
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
      *
