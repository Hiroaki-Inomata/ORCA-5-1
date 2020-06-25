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
       PROGRAM-ID.             ORCSSKDEF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタメンテナンス
      *  コンポーネント名  : システム管理レコード定義取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/11/29    NACL−伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  IN01-FILE       ASSIGN   IN01PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  IN01-FILE.
       01  IN01-REC                PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
       01  IN01PARA                PIC X(1000).
      *
       01  STS-AREA.
           03  STS-IN01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-REC             PIC X(1000).
           03  WRK-KEY             PIC X(15).
           03  WRK-BODY            PIC X(1000).
           03  WRK-DATA            PIC X(1000).
           03  WRK-STR             PIC X(26).
      *
           03  WRK-FILENAME        PIC X(50).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPMKPASS.INC".
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKDEF.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSSKDEFAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC    UNTIL FLG-END   =   1
           CLOSE   IN01-FILE
      *debug start
      *     DISPLAY "ITEM-MAX [ " ORCSSKDEF-ITEM-MAX " ]"
      *     PERFORM VARYING IDX
      *             FROM 1 BY 1
      *             UNTIL ( IDX > ORCSSKDEF-ITEM-MAX )
      *               OR  ( IDX > 10 )
      *         DISPLAY "NO : " ORCSSKDEF-ITEM-NO (IDX)
      *         DISPLAY "     ITEM     : " ORCSSKDEF-ITEM (IDX)
      *         DISPLAY "     ITEM_JP  : " ORCSSKDEF-ITEM-JP (IDX)
      *         DISPLAY "     TYPE     : " ORCSSKDEF-TYPE (IDX)
      *         DISPLAY "     START    : " ORCSSKDEF-START (IDX)
      *         DISPLAY "     LENGTH   : " ORCSSKDEF-LENGTH (IDX)
      *         DISPLAY "     PRECISION: " ORCSSKDEF-PRECISION (IDX)
      *     END-PERFORM
      *debug end
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  STS-AREA
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           MOVE    SPACE           TO  WRK-FILENAME
           STRING  "data/SKDEF"        DELIMITED BY SIZE
                   ORCSSKDEF-KANRICD   DELIMITED BY SPACE
                   ORCSSKDEF-KBNCD     DELIMITED BY SPACE
                   ".yml"              DELIMITED BY SIZE
                   INTO                WRK-FILENAME
           END-STRING
      *
      *    patch-libに存在？
           INITIALIZE                  ORCSMKPASSAREA
           STRING  "patch-lib/"        DELIMITED BY SIZE
                   WRK-FILENAME        DELIMITED BY SPACE
                   INTO                MKPASS-IN-01
           END-STRING
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  IN01PARA
      *
           OPEN    INPUT   IN01-FILE
      *
      *    ファイルがない場合
           IF      STS-IN01        NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    WRK-FILENAME    TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING
                                           ORCSMKPASSAREA
               MOVE    MKPASS-OT-01    TO  IN01PARA
      *
               OPEN    INPUT   IN01-FILE
               IF      STS-IN01        NOT =   ZERO
                   DISPLAY "SKDEFnnnn.yml not found."
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-IN01-READ-SEC
           PERFORM UNTIL   FLG-END =   1
               MOVE    SPACE               TO  WRK-REC
               MOVE    FUNCTION TRIM ( IN01-REC )
                                           TO  WRK-REC
               PERFORM 2001-SKDEF-EDIT-SEC
               PERFORM 900-IN01-READ-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    定義情報編集処理
      *****************************************************************
       2001-SKDEF-EDIT-SEC         SECTION.
      *
      *    項目キーと設定値に分解
           MOVE    SPACE               TO  WRK-KEY
                                           WRK-BODY
           UNSTRING    WRK-REC         DELIMITED BY ":"
               INTO    WRK-KEY
                       WRK-BODY
           END-UNSTRING
      *
           MOVE    SPACE               TO  WRK-DATA
           MOVE    FUNCTION TRIM ( WRK-BODY )
                                       TO  WRK-DATA
           MOVE    WRK-DATA            TO  WRK-BODY
           IF      WRK-DATA (1:1)      =   '"'
               MOVE    WRK-DATA (2:)       TO  WRK-BODY
           END-IF
           MOVE    SPACE               TO  WRK-DATA
           STRING  WRK-BODY            DELIMITED BY '"'
               INTO    WRK-DATA
           END-STRING
      *
           EVALUATE    WRK-KEY
               WHEN    "name"
                   MOVE    WRK-DATA            TO  ORCSSKDEF-NAME
               WHEN    "item_max"
                   MOVE    WRK-DATA            TO  WRK-STR
                   PERFORM 800-STR2NUM-SEC
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO
                                                   ORCSSKDEF-ITEM-MAX
                   ELSE
                       DISPLAY "item_max error!"
                   END-IF
               WHEN    "kanri_cd"
               WHEN    "class_cd"
               WHEN    "item_list"
                   CONTINUE
               WHEN    OTHER
                   PERFORM 20011-SKDEF-LIST-SEC
           END-EVALUATE
           .
       2001-SKDEF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    定義情報編集処理
      *****************************************************************
       20011-SKDEF-LIST-SEC        SECTION.
      *
           EVALUATE    WRK-KEY
               WHEN    "- item_no"
                   MOVE    WRK-DATA            TO  WRK-STR
                   PERFORM 800-STR2NUM-SEC
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO  IDX
                   ELSE
                       DISPLAY "item_no error!"
                   END-IF
                   MOVE    IDX                 TO  ORCSSKDEF-ITEM-NO
                                                                  (IDX)
               WHEN    "item"
                   MOVE    WRK-DATA            TO  ORCSSKDEF-ITEM (IDX)
               WHEN    "item_jp"
                   MOVE    WRK-DATA            TO  ORCSSKDEF-ITEM-JP
                                                                  (IDX)
               WHEN    "type"
                   MOVE    WRK-DATA            TO  ORCSSKDEF-TYPE (IDX)
               WHEN    "start"
                   MOVE    WRK-DATA            TO  WRK-STR
                   PERFORM 800-STR2NUM-SEC
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO
                                                   ORCSSKDEF-START (IDX)
                   ELSE
                       DISPLAY "start error!"
                   END-IF
               WHEN    "length"
                   MOVE    WRK-DATA            TO  WRK-STR
                   PERFORM 800-STR2NUM-SEC
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO
                                               ORCSSKDEF-LENGTH  (IDX)
                   ELSE
                       DISPLAY "length error!"
                   END-IF
               WHEN    "precision"
                   MOVE    WRK-DATA            TO  WRK-STR
                   PERFORM 800-STR2NUM-SEC
                   IF      SNUM-RC             =   ZERO
                       MOVE    SNUM-OUTNUM         TO
                                               ORCSSKDEF-PRECISION
                                                                 (IDX)
                   ELSE
                       DISPLAY "precision error!"
                   END-IF
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
           .
       20011-SKDEF-LIST-EXT.
           EXIT.
      *****************************************************************
      *    数字変換処理
      *****************************************************************
       800-STR2NUM-SEC             SECTION.
      *
           INITIALIZE  ORCSNUMAREA
           MOVE    WRK-STR             TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           .
       800-STR2NUM-EXT.
           EXIT.
      *****************************************************************
      *    ファイル読込処理
      *****************************************************************
       900-IN01-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-END
           READ    IN01-FILE
           AT  END
               MOVE    1               TO  FLG-END
           END-READ
           .
       900-IN01-READ-EXT.
           EXIT.
