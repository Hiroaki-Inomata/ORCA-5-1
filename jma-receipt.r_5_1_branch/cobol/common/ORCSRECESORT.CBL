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
       PROGRAM-ID.             ORCSRECESORT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : レセ用器材再編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/03/13    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/10  グループ診療対応
      *  03.05.01    NACL-多々納  07/07/10  フィルム無しの対応
      *  04.06.01    NACL-多々納  11/01/25  同時併施対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-PLUS-ARI        PIC 9(01).
           03  FLG-TENSU          PIC 9(01).
      *
           03  FLG-DOUJI           PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-END3            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
           03  IDXZ                PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
           03  IDX-HOSI            PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KIZIGOK         PIC 9(07).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-72-SRYCD        PIC X(09).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  TBL-IN-CHK-G.
               05  TBL-IN-CHK          PIC X(01)   OCCURS   50.
      *
      *    診療行為テーブル
       01  TBL-SRYACT-AREA.
            03  TBL-SRYACT-OCC     OCCURS   50.
                05  TBL-SRY-SRYCD      PIC X(09).
                05  TBL-SRY-SRYSYUKBN  PIC X(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *    レセ器材再編集パラメタ
           COPY    "CPORCSRECESORT.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSRECESORTAREA
           SPA-AREA
           .
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
           INITIALIZE                  ORCSRECESORT-OUT-AREA
      *    診療会計附加情報検索
           PERFORM 100-INIT-SYORI-SEC
           IF      ORCSRECESORT-ERRCD  =   ZERO
      *        器材再編集
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       100-INIT-SYORI-SEC              SECTION.
      *
      *    診療会計附加情報検索
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ORCSRECESORT-HOSPNUM    TO  ACCTP-HOSPNUM
           MOVE    ORCSRECESORT-NYUGAIKBN
                                       TO  ACCTP-NYUGAIKBN
           MOVE    ORCSRECESORT-PTID   TO  ACCTP-PTID
           MOVE    ORCSRECESORT-SRYYM  TO  ACCTP-SRYYM
           MOVE    ORCSRECESORT-ZAINUM TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCTPLUS     =   1
               MOVE    1                   TO  ORCSRECESORT-ERRCD
           END-IF
      *    画像診断で同時併施がある場合、診療行為テーブル検索
           IF     (FLG-SRYACCTPLUS     =   ZERO )  AND
                  (ORCSRECESORT-SRYKBN(1:1)    =   "7" )
               IF      (ACCTP-FIRTEN (2)   >   ZERO )   OR
                       (ACCTP-FIRTEN (3)   >   ZERO )   OR
                       (ACCTP-FIRTEN (4)   >   ZERO )   OR
                       (ACCTP-FIRTEN (5)   >   ZERO )
                   MOVE    1               TO  FLG-DOUJI
                   PERFORM 1001-SRYACT-HEN-SEC
               END-IF
           END-IF
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       1001-SRYACT-HEN-SEC             SECTION.
      *
           INITIALIZE                      TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SRY
      *
           INITIALIZE                      SRYACT-REC
           MOVE    ORCSRECESORT-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    ORCSRECESORT-NYUGAIKBN
                                       TO  SRY-NYUGAIKBN
           MOVE    ORCSRECESORT-PTID   TO  SRY-PTID
           MOVE    ORCSRECESORT-SRYYM  TO  SRY-SRYYM
           MOVE    ORCSRECESORT-ZAINUM TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM
               UNTIL  (FLG-SRYACT          =   1     )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (SRY-SRYCD (IDY) =   SPACE)
                   ADD     1               TO  IDX-SRY
                   MOVE    SRY-SRYCD (IDY) TO  TBL-SRY-SRYCD (IDX-SRY)
                   IF      SRY-SRYCD (IDY)(1:1)    =   "1"
                       MOVE    SRY-SRYCD (IDY)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-DATAKBN     =   "1"
                           MOVE    TNS-SRYSYUKBN   TO  TBL-SRY-SRYSYUKBN
                                                            (IDX-SRY)
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材再編集処理
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
      *    酸素補正率決定
           MOVE    ZERO                TO  IDX-HOSI
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )
                         OR   (ORCSRECESORT-IN-SRYCD (IDX2)  =   SPACE)
      *        酸素補正率
               IF      ORCSRECESORT-IN-SRYCD (IDX2)
                                               =   "770020070"
                   MOVE    IDX2                TO  IDX-HOSI
                   MOVE    50                  TO  IDX2
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDY1
           MOVE    ZERO                TO  IDY2
           MOVE    ZERO                TO  IDY3
           MOVE    ZERO                TO  IDY4
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )
                         OR   (ORCSRECESORT-IN-SRYCD (IDX2)  =   SPACE)
      *        器材
               IF      ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "7"
      *            フィルム判定
                   IF     (ORCSRECESORT-IN-DATAKBN(IDX2)   =   "3" ) OR
                          (ORCSRECESORT-IN-SRYCD (IDX2)
                                                       =   "799990070")
                       IF      FLG-DOUJI           =   ZERO
      *                    同時併施なし
                           MOVE    1               TO  IDX
                           ADD     1               TO  IDY1
                           MOVE    IDY1            TO  IDXZ
                           PERFORM 2009-TBL-HEN-SEC
                       END-IF
                   ELSE
      *            フィルム以外
      *                酸素補正率は対象外とする
                       IF      ORCSRECESORT-IN-SRYCD (IDX2)
                                               =   "770020070"
                           CONTINUE
                       ELSE
                           PERFORM 2001-KIZAI-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *        コメント
               IF     (ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "8")
                   IF    IDX                   >   ZERO
                       PERFORM 2002-COMMENT-HEN-SEC
                   END-IF
               END-IF
      *        器材・コメント以外
               IF     (ORCSRECESORT-IN-SRYCD (IDX2)(1:1)   =   "8"
                                                           OR  "7")
                   MOVE    ZERO                TO  IDX
               END-IF
           END-PERFORM
      *
      *    テーブル数
           MOVE    IDY1                TO  ORCSRECESORT-TBLMAX (1)
           MOVE    IDY2                TO  ORCSRECESORT-TBLMAX (2)
           MOVE    IDY3                TO  ORCSRECESORT-TBLMAX (3)
           MOVE    IDY4                TO  ORCSRECESORT-TBLMAX (4)
      *    点数編集
      *    酸素
           MOVE    ACCTP-SANSOTEN      TO  ORCSRECESORT-ZAITEN (2)
      *    窒素
           MOVE    ACCTP-CHISOTEN      TO  ORCSRECESORT-ZAITEN (3)
      *
      *!!!!!!!
      *H25.2
      *    画像診断のフィルム料同時併施有
           IF      FLG-DOUJI           =   1
               PERFORM 20071-GAZOU-SRYCD-HEN-SEC
           END-IF
      *!!!!!!!
      *    手技毎の点数を集計（本来は別々にレセ電記載する）
           MOVE    ZERO                TO  FLG-PLUS-ARI
           MOVE    ZERO                TO  IDX-SRY
           MOVE    1                   TO  IDX4
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   5
      *        画像診断
               IF      FLG-DOUJI           =   1
                   PERFORM 2007-GAZOU-HEN-SEC
               ELSE
      *        画像診断以外は集計
      *        フィルム
                   ADD     ACCTP-FIRTEN (IDX3)
                                           TO  ORCSRECESORT-ZAITEN (1)
               END-IF
      *
      *        その他器材点数（フィルム・酸素・窒素以外）
               ADD     ACCTP-KIZAITEN(IDX3) TO  ORCSRECESORT-ZAITEN (4)
      *
               IF     (ACCTP-FIRTEN (IDX3)  NOT =   ZERO )  OR
                      (ACCTP-KIZAITEN(IDX3) NOT =   ZERO )
                   MOVE    1                   TO  FLG-PLUS-ARI
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(1)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(2)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(3)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(4)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(6)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(7)  TO  WRK-KIZIGOK
           ADD     ORCSRECESORT-ZAITEN(8)  TO  WRK-KIZIGOK
      *    器材合計が違う時、付加情報に器材点記載なしとする
           IF     (WRK-KIZIGOK         <   ORCSRECESORT-KIZAITEN ) AND
                  (FLG-PLUS-ARI        =   ZERO     )
               COMPUTE ORCSRECESORT-ZAITEN (4) =   ORCSRECESORT-KIZAITEN
                                               -  (ACCTP-SANSOTEN
                                               +   ACCTP-CHISOTEN )
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *!!!!!!!
      *H25.2
      *****************************************************************
      *    画像診断フィルム編集処理
      *****************************************************************
       20071-GAZOU-SRYCD-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX5
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL      (IDX3    >   5   )
                          OR  (ACCTP-SRYCD(IDX3)   =   SPACE )
               MOVE    SPACE           TO  WRK-72-SRYCD
               MOVE    SPACE           TO  WRK-SRYSYUKBN
               COMPUTE IDX4            =   IDX5    +   1
               PERFORM VARYING     IDX-SRY    FROM    IDX4  BY  1
                       UNTIL      (IDX-SRY    >   50  )
                             OR   (TBL-SRY-SRYCD(IDX-SRY)  =   SPACE)
                   MOVE    IDX-SRY            TO  IDX5
                   IF      ACCTP-SRYCD(IDX3)  =   TBL-SRY-SRYCD(IDX-SRY)
                       MOVE    TBL-SRY-SRYSYUKBN(IDX-SRY)
                                                  TO  WRK-SRYSYUKBN
                   END-IF
                   IF      TBL-SRY-SRYCD (IDX-SRY)(1:1)    =   "7"
                       IF      WRK-72-SRYCD    NOT =   SPACE
                           MOVE    50              TO  IDX-SRY
                       END-IF
                   ELSE
                       IF      TBL-SRY-SRYSYUKBN (IDX-SRY)
                                                    =   "721"
                                                    OR  "720"
      *                    Ｘ線撮影料
                           MOVE    TBL-SRY-SRYCD(IDX-SRY)
                                                    TO  WRK-72-SRYCD
                       END-IF
                   END-IF
               END-PERFORM
      *        付加情報のコードが撮影料でない場合は撮影料に置換える
               IF      WRK-72-SRYCD    NOT =   SPACE
                   IF      WRK-SRYSYUKBN(1:2)  =   "72"
                                               OR  "71"
                       CONTINUE
                   ELSE
                       MOVE    WRK-72-SRYCD    TO  ACCTP-SRYCD(IDX3)
                   END-IF
               END-IF
           END-PERFORM
           .
       20071-GAZOU-SRYCD-HEN-EX.
           EXIT.
      *!!!!!!!
      *!!!!!!!
      *
      *****************************************************************
      *    画像診断フィルム編集処理
      *****************************************************************
       2007-GAZOU-HEN-SEC                SECTION.
      *
           IF      IDX3                <   5
               MOVE    ACCTP-SRYCD (IDX3 + 1)  TO  WRK-SRYCD
           ELSE
               MOVE    SPACE                   TO  WRK-SRYCD
           END-IF
           IF      IDX3                =   1
               MOVE    1               TO  IDX
           ELSE
               COMPUTE IDX             =   IDX3    +   3
           END-IF
           MOVE    ZERO                TO  IDXZ
           MOVE    ZERO                TO  FLG-END2
           PERFORM VARYING     IDX2    FROM    1     BY  1
                   UNTIL      (IDX2    >   50  )
                         OR   (ORCSRECESORT-IN-SRYCD (IDX2)  =   SPACE)
                         OR   (FLG-END2    =   1   )
      *        器材
               MOVE    ZERO                TO  FLG-END3
               IF     (TBL-IN-CHK  (IDX2)  =   SPACE )
                   PERFORM VARYING     IDX-SRY    FROM    IDX4  BY  1
                           UNTIL      (IDX-SRY    >   50  )
                                  OR  (FLG-END2  =   1   )
                                  OR  (FLG-END3  =   1   )
                      IF     (TBL-SRY-SRYCD(IDX-SRY)   =   WRK-SRYCD )
                           MOVE    1               TO  FLG-END2
                           COMPUTE IDX4        =   IDX-SRY   +   1
                      ELSE
                          IF     (ORCSRECESORT-IN-SRYCD (IDX2)
                                                   =   TBL-SRY-SRYCD
                                                          (IDX-SRY))
                               IF     (ORCSRECESORT-IN-SRYCD(IDX2)(1:1)
                                                       =   "7"   )
                                 AND  (ORCSRECESORT-IN-DATAKBN(IDX2)
                                                       =   "3"   )
                                   ADD     1               TO  IDXZ
                                   PERFORM 2009-TBL-HEN-SEC
                                   MOVE    1               TO  FLG-END3
                               END-IF
                               MOVE    "1"             TO  TBL-IN-CHK
                                                               (IDX2)
                               COMPUTE IDX4        =   IDX-SRY   +   1
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    ACCTP-FIRTEN (IDX3) TO  ORCSRECESORT-ZAITEN (IDX)
           MOVE    IDXZ                TO  ORCSRECESORT-TBLMAX (IDX)
           .
       2007-GAZOU-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    フィルム以外編集処理
      *****************************************************************
       2009-TBL-HEN-SEC            SECTION.
      *
           MOVE    ORCSRECESORT-IN-SRYCD (IDX2)
                                       TO  ORCSRECESORT-SRYCD
                                                        (IDX IDXZ)
      *    データ区分
           MOVE    ORCSRECESORT-IN-DATAKBN (IDX2)
                                       TO  ORCSRECESORT-DATAKBN
                                                        (IDX IDXZ)
      *    数量
           MOVE    ORCSRECESORT-IN-SRYSURYO (IDX2)
                                       TO  ORCSRECESORT-SRYSURYO
                                                        (IDX IDXZ)
      *    回数
           MOVE    ORCSRECESORT-IN-SRYKAISU (IDX2)
                                       TO  ORCSRECESORT-SRYKAISU
                                                        (IDX IDXZ)
      *    単位コード
           MOVE    ORCSRECESORT-IN-TANICD (IDX2)
                                       TO  ORCSRECESORT-TANICD
                                                        (IDX IDXZ)
      *    点数識別区分
           MOVE    ORCSRECESORT-IN-TENSIKIBETU (IDX2)
                                       TO  ORCSRECESORT-TENSIKIBETU
                                                        (IDX IDXZ)
      *    点数
           MOVE    ORCSRECESORT-IN-TEN (IDX2)
                                       TO  ORCSRECESORT-TEN
                                                        (IDX IDXZ)
      *    漢字名称
           MOVE    ORCSRECESORT-IN-NAME (IDX2)
                                       TO  ORCSRECESORT-NAME
                                                        (IDX IDXZ)
      *    レセ電用特定器材商品名
           MOVE    ORCSRECESORT-IN-RECEDENNAME (IDX2)
                                       TO  ORCSRECESORT-RECEDENNAME
                                                        (IDX IDXZ)
      *    関連コメント検索用ＫＥＹ
           MOVE    ORCSRECESORT-IN-KEY (IDX2)
                                       TO  ORCSRECESORT-KEY
                                                        (IDX IDXZ)
           MOVE    ORCSRECESORT-IN-SHORI-RENNUM (IDX2)
                                       TO  ORCSRECESORT-SHORI-RENNUM
                                                        (IDX IDXZ)
           .
       2009-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    フィルム以外編集処理
      *****************************************************************
       2001-KIZAI-HEN-SEC            SECTION.
      *
           EVALUATE    ORCSRECESORT-IN-TOKUKIZAISBT1 (IDX2)
               WHEN    ZERO
      *            器材
                   MOVE    4                   TO  IDX
                   ADD     1                   TO  IDY4
                   MOVE    IDY4                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    2
                   IF      ORCSRECESORT-IN-SANSOKBN (IDX2) =   "9"
      *            窒素
                       MOVE    3               TO  IDX
                       ADD     1               TO  IDY3
                       MOVE    IDY3            TO  IDXZ
                       PERFORM 2009-TBL-HEN-SEC
                   ELSE
      *            酸素
                       MOVE    2               TO  IDX
                       ADD     1               TO  IDY2
                       MOVE    IDY2            TO  IDXZ
                       PERFORM 2009-TBL-HEN-SEC
      *                高気圧加算以外
                       IF     (ORCSRECESORT-IN-SANSOKBN (IDX2) =   "1")
                         AND  (ORCSRECESORT-IN-SRYCD (IDX2)
                                               NOT =   "770000000"  )
                           CONTINUE
                       ELSE
      *                酸素補正率を酸素の直下に再編集
                           IF      IDX-HOSI    NOT =   ZERO
                               ADD     1               TO  IDY2
                               MOVE    IDX2            TO  IDX-HZN
                               MOVE    IDX-HOSI        TO  IDX2
                               MOVE    IDY2            TO  IDXZ
                               PERFORM 2009-TBL-HEN-SEC
                               MOVE    IDX-HZN         TO  IDX2
                           END-IF
                       END-IF
                   END-IF
               WHEN    3
      *            高線量率イリジウム（その他器材）
                   MOVE    4                   TO  IDX
                   ADD     1                   TO  IDY4
                   MOVE    IDY4            TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
           END-EVALUATE
      *
           .
       2001-KIZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       2002-COMMENT-HEN-SEC             SECTION.
      *
           EVALUATE    IDX
               WHEN    1
      *            フィルム
                   ADD     1                   TO  IDY1
                   MOVE    IDY1                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    2
      *            酸素
                   ADD     1                   TO  IDY2
                   MOVE    IDY2                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    3
      *            窒素
                   ADD     1                   TO  IDY3
                   MOVE    IDY3                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
               WHEN    4
      *            その他器材
                   ADD     1                   TO  IDY4
                   MOVE    IDY4                TO  IDXZ
                   PERFORM 2009-TBL-HEN-SEC
           END-EVALUATE
      *
           .
       2002-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ付加テーブル検索処理
      *****************************************************************
       910-SRYACCTPLUS-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       910-SRYACCTPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ORCSRECESORT-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    ORCSRECESORT-SRYYM      TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD           TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
           .
       910-TENSU-READ-EXT.
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
       990-DBCLOSE-SEC           SECTION.
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
