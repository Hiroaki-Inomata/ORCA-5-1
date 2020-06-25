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
       PROGRAM-ID.             ORCSSYUKAMAIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 主科情報取得メインサブ
      *  管理者            : 
      *  06/02/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-藤原    07/04/23  入院基本料カレンダー未設定時の
      *                                     主科設定対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.05.01    NACL-藤原    11/09/16  公費単独対応
      *                                     公費を最大５０に変更に併せて
      *                                     保険組合せも変更
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YY          PIC 9(04).
               05  WRK-MM          PIC 9(02).
               05  WRK-DD          PIC 9(02).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804
                                   PIC 9(06)   VALUE   200804.      
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計作成用パラメタ
           COPY    "CPORCSTOTALACCT.INC". 
      *
      *    入院会計作成用パラメタ
           COPY    "CPORCHCN33SUB.INC".
      *
      *    レセプト種別取得パラメタ
           COPY    "CPORCSSYUBETU.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    SPA-AREA
                                    ORCSSYUACCAREA.
     *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO            TO  LNK-SYUACC-RC
      *****INITIALIZE                  LNK-SYUACC-IO
      *
      *
      *    入力項目チェック
           IF      SPA-SYUKA-KBN   =   ZERO
               MOVE    1               TO  FLG-END 
           ELSE
               EVALUATE    SPA-SYUKA-MODE
                   WHEN    "1"
                       IF      LNK-SYUACC-I-KBN
                                           =   "01"    OR  "02"    OR
                                               "03"    OR  "04"    OR
                                               "05"    OR  "06"    OR
                                               "07"    OR  "08"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-END
                       END-IF
                   WHEN    "2"
                       IF      LNK-SYUACC-I-KBN
                                           =   "01"    OR  "02"    OR
                                               "05"    OR  "06"    OR
                                               "07"    OR  "08"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-END
                       END-IF
               END-EVALUATE
           END-IF
      *???
      *     DISPLAY "FLG-END=" FLG-END "#" SPA-SYUKA-KBN "#" 
      *                      SPA-SYUKA-MODE "#" LNK-SYUACC-I-KBN "#"
      *??
           .
       100-INIT-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           IF      LNK-SYUACC-I-KBN    =   "05"
               EVALUATE    SPA-NYUGAIKBN
                   WHEN    "1"    
                       EVALUATE    SPA-SYUKA-NYUIN
                           WHEN    "2"    
                               PERFORM 2001-ORCSSYUBETU-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               PERFORM 2002-ORCSTOTALACCT-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               MOVE    "03"        TO  LNK-SYUACC-I-KBN
                               PERFORM 2004-ORCSSYUACC-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               PERFORM VARYING IDY FROM    1   BY  1
                                       UNTIL   IDY >       100
                                       OR      LNK-SYUBETU-TEISYUTUSAKI
                                                              (IDY)
                                                   =   ZERO
                                   INITIALIZE  LNK-SYUACC-IO-SYUKA-AREA
                                                                  (IDY)
                               END-PERFORM
                               PERFORM 2003-ORCHCN33SUB-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               MOVE    "04"        TO  LNK-SYUACC-I-KBN
                               PERFORM 2004-ORCSSYUACC-SEC
                           WHEN    OTHER
                               PERFORM 2001-ORCSSYUBETU-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               PERFORM 2003-ORCHCN33SUB-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               MOVE    "04"        TO  LNK-SYUACC-I-KBN
                               PERFORM 2004-ORCSSYUACC-SEC
                       END-EVALUATE
                   WHEN    "2"
                       PERFORM 2001-ORCSSYUBETU-SEC
                       IF      FLG-END         =   1
                           GO  TO  200-MAIN-EXT
                       END-IF
                       PERFORM 2002-ORCSTOTALACCT-SEC
                       IF      FLG-END         =   1
                           GO  TO  200-MAIN-EXT
                       END-IF
                       MOVE    "03"        TO  LNK-SYUACC-I-KBN
                       PERFORM 2004-ORCSSYUACC-SEC
               END-EVALUATE
           ELSE
               IF      LNK-SYUACC-I-KBN    =   "01"    OR  "02"    OR  
                                               "06"    OR  "07"    OR
                                               "08"
                   PERFORM 2004-ORCSSYUACC-SEC
               ELSE
                   IF      SPA-SYUKA-MODE  =   "2"
                       PERFORM 2004-ORCSSYUACC-SEC
                   ELSE
                       EVALUATE    SPA-NYUGAIKBN
                           WHEN    "1"
                               EVALUATE    LNK-SYUACC-I-KBN
                                   WHEN    "04"
                                       PERFORM 2001-ORCSSYUBETU-SEC
                                       IF      FLG-END         =   1
                                           GO  TO  200-MAIN-EXT
                                       END-IF
                                       PERFORM 2003-ORCHCN33SUB-SEC
                                       IF      FLG-END         =   1
                                           GO  TO  200-MAIN-EXT
                                       END-IF
                                       PERFORM 2004-ORCSSYUACC-SEC
                                   WHEN    "03"
                                       PERFORM 2001-ORCSSYUBETU-SEC
                                       IF      FLG-END         =   1
                                           GO  TO  200-MAIN-EXT
                                       END-IF
                                       PERFORM 2002-ORCSTOTALACCT-SEC
                                       IF      FLG-END         =   1
                                           GO  TO  200-MAIN-EXT
                                       END-IF
                                       PERFORM 2004-ORCSSYUACC-SEC
                               END-EVALUATE    
                           WHEN    "2"
                               PERFORM 2001-ORCSSYUBETU-SEC
                               IF      FLG-END         =   1
                                   GO  TO  200-MAIN-EXT
                               END-IF
                               PERFORM 2002-ORCSTOTALACCT-SEC
                               IF      FLG-END         =   1
                                    GO  TO  200-MAIN-EXT
                               END-IF
                               PERFORM 2004-ORCSSYUACC-SEC
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF    
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別取得サブ
      *****************************************************************
       2001-ORCSSYUBETU-SEC        SECTION.
      *
           INITIALIZE              ORCSSYUBETUAREA
grpsys     MOVE    SPA-HOSPNUM     TO  LNK-SYUBETU-HOSPNUM
           MOVE    SPA-PREFNUM     TO  LNK-SYUBETU-PREFNUM
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  LNK-SYUBETU-SRYYM
           MOVE    SPA-NYUGAIKBN   TO  LNK-SYUBETU-NYUGAIKBN
           MOVE    SPA-PTID        TO  LNK-SYUBETU-PTID
           MOVE    SPA-BIRTHDAY    TO  LNK-SYUBETU-BIRTHDAY
           CALL    "ORCSSYUBETU"   USING   ORCSSYUBETUAREA
                                           SPA-AREA
           IF      LNK-SYUBETU-RC  =   ZERO
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       100
                       OR      LNK-SYUBETU-TEISYUTUSAKI (IDY)
                                                   =   ZERO
                   MOVE    SPA-NYUGAIKBN
                                   TO  LNK-SYUACC-IO-NYUGAIKBN    (IDY)
                   MOVE    LNK-SYUBETU-TEISYUTUSAKI (IDY)
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI (IDY)
                   MOVE    LNK-SYUBETU-RECESYUBETU  (IDY)
                                   TO  LNK-SYUACC-IO-RECESYUBETU  (IDY)
                   MOVE    LNK-SYUBETU-HKNJANUM     (IDY)
                                   TO  LNK-SYUACC-IO-HKNJANUM     (IDY)
                   MOVE    LNK-SYUBETU-HOJOKBN      (IDY)
                                   TO  LNK-SYUACC-IO-HOJOKBN      (IDY)
                   MOVE    LNK-SYUBETU-KOHID        (IDY)
                                   TO  LNK-SYUACC-IO-KOHID        (IDY)
                   MOVE    LNK-SYUBETU-RECEIPT-KBN  (IDY)
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN  (IDY)
                   MOVE    LNK-SYUBETU-HKNNUM       (IDY)
                                   TO  LNK-SYUACC-IO-HKNNUM       (IDY)
      *            精神入院か生保単独複数のとき
                   IF    ( LNK-SYUBETU-HKNNUM   (IDY)
                                               =   SPACE      )
                   AND   ( LNK-SYUBETU-TEKSTYMD (IDY)
                                           NOT =   "00000000" )
                       MOVE    LNK-SYUBETU-TEKSTYMD     (IDY)
                                   TO  LNK-SYUACC-IO-HKNJANUM     (IDY)
                   END-IF 
               END-PERFORM
           ELSE
               MOVE    LNK-SYUBETU-RC  TO  LNK-SYUACC-RC
               MOVE    1               TO  FLG-END
      *
               DISPLAY "ORCSSYUBETU=" LNK-SYUACC-RC
           END-IF
           .
       2001-ORCSSYUBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計請求点取得サブ
      *****************************************************************
       2002-ORCSTOTALACCT-SEC        SECTION.
      *
      *    対象レセプトが存在しないとき処理はしない
           IF      LNK-SYUACC-IO-TEISYUTUSAKI (1)
                                       =   ZERO
               GO  TO  2002-ORCSTOTALACCT-EXT
           END-IF
      *
           INITIALIZE                  ORCSTOTALACCT-AREA
grpsys     MOVE    SPA-HOSPNUM     TO  LNK-ORCSTOTALACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN   TO  LNK-ORCSTOTALACCT-NYUGAIKBN
           MOVE    SPA-PTID        TO  LNK-ORCSTOTALACCT-PTID
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  LNK-ORCSTOTALACCT-SANTEIYM
           IF      SPA-NYUGAIKBN   =   "2"
               EVALUATE    SPA-SYUKA-GAIRAI
                   WHEN    "1"
                       MOVE    "2"      TO  LNK-ORCSTOTALACCT-SHORIKBN
                   WHEN    "2"
                       MOVE    "1"      TO  LNK-ORCSTOTALACCT-SHORIKBN
               END-EVALUATE
           END-IF
           CALL    "ORCSTOTALACCT" USING   ORCSTOTALACCT-AREA
                                           SPA-AREA
           IF      LNK-ORCSTOTALACCT-RC    =   ZERO
               CONTINUE
           ELSE
               MOVE    LNK-ORCSTOTALACCT-RC    TO  LNK-SYUACC-RC
               MOVE    1                       TO  FLG-END
      *
              DISPLAY "ORCSTOTALACCT=" LNK-SYUACC-RC
      *
               GO  TO  2002-ORCSTOTALACCT-EXT
           END-IF
      *
      *    レセプト別集計
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       50
                   OR      LNK-ORCSTOTALACCT-HKNCOMBINUM (IDX1)
                                               =   ZERO
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       100
                       OR      LNK-SYUBETU-TEISYUTUSAKI (IDY1)
                                                   =   ZERO
                   PERFORM VARYING IDY2    FROM    1   BY  1
                           UNTIL   IDY2    >       10
                           OR      LNK-SYUBETU-HKNCOMBI (IDY1 IDY2)
                                                   =   ZERO
                       IF      LNK-ORCSTOTALACCT-HKNCOMBINUM (IDX1)
                                   =   LNK-SYUBETU-HKNCOMBI (IDY1 IDY2)
                           PERFORM 20021-ORCSTOTALACCT-SYUKEI-SEC
                           MOVE    100         TO  IDY1  
                           MOVE    10          TO  IDY2  
                       END-IF
                   END-PERFORM
               END-PERFORM        
           END-PERFORM
           .
       2002-ORCSTOTALACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得サブ（点数集計）
      *****************************************************************
       20021-ORCSTOTALACCT-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       50
                   OR      LNK-ORCSTOTALACCT-SRYKA  (IDX1 IDX2)
                                   =   SPACE
               PERFORM VARYING IDY3    FROM    1   BY  1
                       UNTIL   IDY3    >       50
                   IF      LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                                   =   SPACE
                       MOVE    LNK-ORCSTOTALACCT-SRYKA (IDX1 IDX2)
                                  TO  LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                   END-IF
                   IF      LNK-ORCSTOTALACCT-SRYKA (IDX1 IDX2)
                                   =   LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                       PERFORM VARYING IDY4    FROM    1   BY  1
                               UNTIL   IDY4    >       10
                           IF      LNK-SYUACC-IO-HKNCOMBI
                                                       (IDY1 IDY3 IDY4)
                                                   =   ZERO
                               MOVE    LNK-ORCSTOTALACCT-HKNCOMBINUM
                                                       (IDX1)
                                           TO  LNK-SYUACC-IO-HKNCOMBI
                                                        (IDY1 IDY3 IDY4)
                           END-IF
                           IF      LNK-ORCSTOTALACCT-HKNCOMBINUM
                                                       (IDX1)
                                           =   LNK-SYUACC-IO-HKNCOMBI
                                                        (IDY1 IDY3 IDY4)
                               ADD     LNK-ORCSTOTALACCT-TENSU
                                                        (IDX1 IDX2)
                                           TO
                                       LNK-SYUACC-IO-TENSU  (IDY1 IDY3)
                               ADD     LNK-ORCSTOTALACCT-KAISU
                                                        (IDX1 IDX2)
                                           TO
                                      LNK-SYUACC-IO-KAISU   (IDY1 IDY3)
                               MOVE    50          TO  IDY3
                               MOVE    10          TO  IDY4
                           END-IF
                       END-PERFORM
                   END-IF        
               END-PERFORM                       
           END-PERFORM
           .
       20021-ORCSTOTALACCT-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得サブ
      *****************************************************************
       2003-ORCHCN33SUB-SEC        SECTION.
      *
      *    対象レセプトが存在しないとき処理はしない
           IF      LNK-SYUACC-IO-TEISYUTUSAKI (1)
                                       =   ZERO
               GO  TO  2003-ORCHCN33SUB-EXT
           END-IF
      *
           INITIALIZE                  ORCHCN33SUB-AREA
grpsys     MOVE    SPA-HOSPNUM     TO  LNK-ORCHCN33SUB-HOSPNUM
           MOVE    SPA-PTID        TO  LNK-ORCHCN33SUB-PTID
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  LNK-ORCHCN33SUB-SANTEIYM
           MOVE    "00"            TO  LNK-ORCHCN33SUB-SRYKA
           EVALUATE    SPA-SYUKA-NYUIN
               WHEN    "2"
                   MOVE    "1"         TO  LNK-ORCHCN33SUB-SHORIKBN
               WHEN    "1"
               WHEN    "3"
                   MOVE    "2"         TO  LNK-ORCHCN33SUB-SHORIKBN
           END-EVALUATE
           CALL    "ORCHCN33SUB"   USING   ORCHCN33SUB-AREA
                                           SPA-AREA
           IF      LNK-ORCHCN33SUB-RC  =   ZERO
               CONTINUE
           ELSE
               IF      LNK-ORCHCN33SUB-RC  =   4   OR  7
                   DISPLAY "ORCHCN33SUB=" LNK-ORCHCN33SUB-RC
                   GO  TO  2003-ORCHCN33SUB-EXT
               ELSE
                   MOVE    LNK-ORCHCN33SUB-RC      TO  LNK-SYUACC-RC
                   MOVE    1                       TO  FLG-END
      *
                   DISPLAY "ORCHCN33SUB=" LNK-SYUACC-RC
      *
                   GO  TO  2003-ORCHCN33SUB-EXT
               END-IF
           END-IF
      *
      *    レセプト別集計
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      LNK-ORCHCN33SUB-HENKYAKU-HKNCOM (IDX1)
                                           =   ZERO
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       100
                       OR      LNK-SYUBETU-TEISYUTUSAKI (IDY1)
                                                   =   ZERO
                   PERFORM VARYING IDY2    FROM    1   BY  1
                           UNTIL   IDY2    >       10
                           OR      LNK-SYUBETU-HKNCOMBI (IDY1 IDY2)
                                                   =   ZERO
                       IF      LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                        (IDX1)
                                   =   LNK-SYUBETU-HKNCOMBI (IDY1 IDY2)
                           PERFORM 20031-ORCHCN33SUB-SYUKEI-SEC
                           MOVE    100         TO  IDY1  
                           MOVE    10          TO  IDY2  
                       END-IF
                   END-PERFORM
               END-PERFORM        
           END-PERFORM
           .
       2003-ORCHCN33SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得サブ（点数集計）
      *****************************************************************
       20031-ORCHCN33SUB-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       50
                   OR      LNK-ORCHCN33SUB-NYUINKA (IDX1 IDX2)
                                   =   SPACE
               PERFORM VARYING IDY3    FROM    1   BY  1
                       UNTIL   IDY3    >       50
                   IF      LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                                   =   SPACE
                       MOVE    LNK-ORCHCN33SUB-NYUINKA (IDX1 IDX2)
                                  TO  LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                   END-IF
                   IF      LNK-ORCHCN33SUB-NYUINKA (IDX1 IDX2)
                                   =   LNK-SYUACC-IO-SRYKA (IDY1 IDY3)
                       PERFORM VARYING IDY4    FROM    1   BY  1
                               UNTIL   IDY4    >       10
                           IF      LNK-SYUACC-IO-HKNCOMBI
                                                       (IDY1 IDY3 IDY4)
                                                   =   ZERO
                               MOVE
                                   LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                       (IDX1)
                                           TO  LNK-SYUACC-IO-HKNCOMBI
                                                        (IDY1 IDY3 IDY4)
                           END-IF
                           IF      LNK-ORCHCN33SUB-HENKYAKU-HKNCOM
                                                       (IDX1)
                                           =   LNK-SYUACC-IO-HKNCOMBI
                                                        (IDY1 IDY3 IDY4)
                               IF      SPA-SYUKA-NYUIN     =   "3"
                                   IF       LNK-SYUACC-IO-EDDAY
                                                        (IDY1 IDY3)
                                                       <
                                                 LNK-ORCHCN33SUB-EDDAY
                                                            (IDX1 IDX2)
                                       MOVE    LNK-ORCHCN33SUB-EDDAY
                                                        (IDX1 IDX2)
                                           TO    LNK-SYUACC-IO-EDDAY
                                                            (IDY1 IDY3)
                                   END-IF
                               ELSE
                                   ADD     LNK-ORCHCN33SUB-TENSU
                                                        (IDX1 IDX2)
                                           TO  LNK-SYUACC-IO-TENSU
                                                            (IDY1 IDY3)
                                   ADD     LNK-ORCHCN33SUB-DAY 
                                                        (IDX1 IDX2)
                                           TO  LNK-SYUACC-IO-NISSU
                                                            (IDY1 IDY3)
                               END-IF    
                               MOVE    50          TO  IDY3
                               MOVE    10          TO  IDY4
                           END-IF
                       END-PERFORM
                   END-IF        
               END-PERFORM                       
           END-PERFORM
           .
       20031-ORCHCN33SUB-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計請求点取得サブ
      *****************************************************************
       2004-ORCSSYUACC-SEC         SECTION.
      *
           CALL    "ORCSSYUACC"    USING   SPA-AREA
                                           ORCSSYUACCAREA
           IF      LNK-SYUACC-RC   =   ZERO
               CONTINUE
           ELSE
      *
               DISPLAY "ORCSSYUACC=" LNK-SYUACC-RC
      *
               MOVE    1                       TO  FLG-END
           END-IF
           .
       2004-ORCSSYUACC-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *??
      *     DISPLAY "SYUKAMAIN OT=" LNK-SYUACC-RC
      *     DISPLAY "    PTID=" SPA-PTID
      *     DISPLAY " H-SRYKA=" LNK-SYUACC-IO-H-SRYKA
      *     DISPLAY "--------------------------------------------------"
      *     DISPLAY
      *          "|種別|保険者NO|HKN|提|au|科|保1 |保2 |保3 |   点数   |"
      *             "日数|回数|最終日|"
      *     PERFORM VARYING     IDX       FROM    1   BY  1
      *             UNTIL      (IDX       >   100   )
      *         IF    LNK-SYUACC-IO-RECESYUBETU (IDX) = SPACE
      *               MOVE  100   TO  IDX
      *         ELSE
      *               PERFORM VARYING     IDY       FROM    1   BY  1
      *                       UNTIL      (IDY       >   50   )
      *                 IF   LNK-SYUACC-IO-SRYKA (IDX IDY)
      *                                   NOT  =  SPACE
      *                   DISPLAY LNK-SYUACC-IO-RECESYUBETU(IDX) "   "
      *                   LNK-SYUACC-IO-HKNJANUM(IDX) " "
      *                   LNK-SYUACC-IO-HKNNUM  (IDX) " "
      *                   LNK-SYUACC-IO-TEISYUTUSAKI(IDX) " "
      *                   LNK-SYUACC-IO-SYUKA-AUTO(IDX) " "
      *                   LNK-SYUACC-IO-SRYKA (IDX IDY) " "
      *                   LNK-SYUACC-IO-HKNCOMBI (IDX IDY 1) " "
      *                   LNK-SYUACC-IO-HKNCOMBI (IDX IDY 2) " "
      *                   LNK-SYUACC-IO-HKNCOMBI (IDX IDY 3) " "
      *                   LNK-SYUACC-IO-TENSU(IDX IDY) "  "
      *                   LNK-SYUACC-IO-NISSU(IDX IDY)       "   "
      *                   LNK-SYUACC-IO-KAISU(IDX IDY)       "   "
      *                   LNK-SYUACC-IO-EDDAY(IDX IDY)
      *                 END-IF
      *             END-PERFORM
      *         END-IF
      *     END-PERFORM
      *??
      *
           .
       300-END-EXT.
           EXIT. 
