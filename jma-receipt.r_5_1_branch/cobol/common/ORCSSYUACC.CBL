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
       PROGRAM-ID.             ORCSSYUACC.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 主科テーブルアクセスサブ
      *  管理者            : 
      *  06/02/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-藤原    06/12/27  主科設定情報（入院）が２以外
      *                                     で科が設定できない場合の対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
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
           03  FLG-SYUKA           PIC 9(01).
      *
           03  FLG-DELETE          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYACC-AREA.
               05  WRK-SRYACC-OCC          OCCURS  100.
                   07  WRK-SYUACC-TENSU
                                   PIC S9(09). 
                   07  WRK-SYUACC-KAISU
                                   PIC 9(03). 
                   07  WRK-SYUACC-NISSU
                                   PIC 9(03).
                   07  WRK-SYUACC-EDDAY
                                   PIC 9(02).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    主科テーブル
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
           EVALUATE    LNK-SYUACC-I-KBN
               WHEN    "01"
                   PERFORM 2001-KBN01-HENSYU-SEC
               WHEN    "02"
                   PERFORM 2002-KBN02-HENSYU-SEC
               WHEN    "03"
               WHEN    "04"
                   PERFORM 2003-KBN03-HENSYU-SEC
               WHEN    "06"
               WHEN    "08"
                   PERFORM 2006-KBN06-HENSYU-SEC
               WHEN    "07"
                   PERFORM 2007-KBN07-HENSYU-SEC
           END-EVALUATE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科テーブル情報取得処理
      *****************************************************************
       2001-KBN01-HENSYU-SEC       SECTION.
      *
           INITIALIZE              WRK-SRYACC-AREA
      * 
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID        TO  SYUKA-PTID
           MOVE    SPA-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-SYUKA-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
      *
           IF      FLG-SYUKA       =   1
               MOVE    1               TO  LNK-SYUACC-RC
           END-IF
      *
           PERFORM                 UNTIL   FLG-SYUKA   =   1
               IF      SYUKA-RECESYUBETU   =   "XXXX" 
                   MOVE    SYUKA-SRYKA-INPUT   TO  LNK-SYUACC-IO-H-SRYKA
               ELSE
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX >   100
                       IF      LNK-SYUACC-IO-TEISYUTUSAKI  (IDX)
                                               =   ZERO
                           MOVE    SYUKA-NYUGAIKBN
                                   TO  LNK-SYUACC-IO-NYUGAIKBN   (IDX)
                           MOVE    SYUKA-TEISYUTUSAKI
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI(IDX)
                           MOVE    SYUKA-RECESYUBETU
                                   TO  LNK-SYUACC-IO-RECESYUBETU (IDX)
                           MOVE    SYUKA-HKNJANUM
                                   TO  LNK-SYUACC-IO-HKNJANUM    (IDX)
                           MOVE    SYUKA-HOJOKBN
                                   TO  LNK-SYUACC-IO-HOJOKBN     (IDX)
                           MOVE    SYUKA-KOHID
                                   TO  LNK-SYUACC-IO-KOHID       (IDX)
                           MOVE    SYUKA-RECEIPT-KBN
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN (IDX)
                           MOVE    SYUKA-HKNNUM
                                   TO  LNK-SYUACC-IO-HKNNUM      (IDX)
                       END-IF
                       IF    ( SYUKA-NYUGAIKBN
                                   =   LNK-SYUACC-IO-NYUGAIKBN   (IDX) )
                       AND   ( SYUKA-TEISYUTUSAKI
                                   =   LNK-SYUACC-IO-TEISYUTUSAKI(IDX) )
                       AND   ( SYUKA-RECESYUBETU
                                   =   LNK-SYUACC-IO-RECESYUBETU (IDX) )
                       AND   ( SYUKA-HKNJANUM
                                   =   LNK-SYUACC-IO-HKNJANUM    (IDX) )
                       AND   ( SYUKA-HOJOKBN
                                   =   LNK-SYUACC-IO-HOJOKBN     (IDX) )
                       AND   ( SYUKA-KOHID
                                   =   LNK-SYUACC-IO-KOHID       (IDX) )
                       AND   ( SYUKA-RECEIPT-KBN
                                   =   LNK-SYUACC-IO-RECEIPT-KBN (IDX) )
      *
                           PERFORM 20011-KBN01-SYUACC-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-IF          
      *        
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYUKA-READ-N-SEC
           END-PERFORM   
      *     
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    
      *    請求点数によるシステム主科の設定（入院）
           INITIALIZE              WRK-SRYACC-AREA
      *
           IF    ( SPA-NYUGAIKBN       =   "1" )
           AND   ( SPA-SYUKA-NYUIN     =   "2" )
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   100
                       OR      LNK-SYUACC-IO-TEISYUTUSAKI  (IDX)
                                       =   ZERO
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       50
                           OR      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                           =   SPACE
                       IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                           =   WRK-SYUACC-TENSU (IDX)
                           EVALUATE    SPA-SYUKA-SELECT    
                               WHEN    "0"
                                   MOVE    SPACE       TO
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               WHEN    "1"
                                   IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                       <
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    LNK-SYUACC-IO-SRYKA
                                                               (IDX IDY)
                                                       TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                               WHEN    "2"
                                   IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                       >
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    LNK-SYUACC-IO-SRYKA
                                                               (IDX IDY)
                                                       TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                               >   WRK-SYUACC-TENSU(IDX)
                               MOVE    LNK-SYUACC-IO-TENSU (IDX IDY)
                                           TO  WRK-SYUACC-TENSU (IDX) 
                               MOVE    LNK-SYUACC-IO-SRYKA (IDX IDY)  
                                           TO  LNK-SYUACC-IO-SYUKA-AUTO
                                                                (IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM         
           END-IF
           .
       2001-KBN01-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返却値の設定（手入力による主科設定時）処理
      *****************************************************************
       20011-KBN01-SYUACC-HENSYU-SEC       SECTION.
      *
      *    手入力主科
           IF      SYUKA-SRYKA-INPUT   NOT =   SPACE 
               MOVE    SYUKA-SRYKA-INPUT
                                   TO  LNK-SYUACC-IO-SYUKA-INPUT (IDX)
           END-IF
      *    システム主科
           IF    ( SPA-NYUGAIKBN       =   "1" )
           AND   ( SYUKA-TEN-KBN       =   "2" )
               EVALUATE    SPA-SYUKA-NYUIN
                   WHEN    "1"
                       IF      SYUKA-NISSU =   WRK-SYUACC-NISSU(IDX)
                           EVALUATE    SPA-SYUKA-SELECT    
                               WHEN    "0"
                                   MOVE    SPACE       TO
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               WHEN    "1"
                                   IF      SYUKA-SRYKA   <
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                               WHEN    "2"
                                   IF      SYUKA-SRYKA   >
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      SYUKA-NISSU >   WRK-SYUACC-NISSU(IDX)
                               MOVE    SYUKA-NISSU     TO  
                                               WRK-SYUACC-NISSU  (IDX) 
                               MOVE    SYUKA-SRYKA     TO  
                                               LNK-SYUACC-IO-SYUKA-AUTO
                                                                 (IDX)
                           END-IF
                       END-IF    
                   WHEN    "3"
                       IF      SYUKA-EDDAY     >
                                               WRK-SYUACC-EDDAY (IDX)
                           MOVE    SYUKA-EDDAY     TO  
                                               WRK-SYUACC-EDDAY  (IDX) 
                           MOVE    SYUKA-SRYKA       TO  
                                               LNK-SYUACC-IO-SYUKA-AUTO
                                                                 (IDX)
                       END-IF
               END-EVALUATE 
           END-IF
           IF      SPA-NYUGAIKBN       =   "2"
               EVALUATE    SPA-SYUKA-GAIRAI
                   WHEN    "1"
                       IF      SYUKA-KAISU =   WRK-SYUACC-KAISU (IDX)
                           EVALUATE    SPA-SYUKA-SELECT    
                               WHEN    "0"
                                   MOVE    SPACE       TO
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               WHEN    "1"
                                   IF      SYUKA-SRYKA   <
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                               WHEN    "2"
                                   IF      SYUKA-SRYKA   >
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      SYUKA-KAISU >  WRK-SYUACC-KAISU (IDX)
                               MOVE    SYUKA-KAISU   TO  
                                           WRK-SYUACC-KAISU        (IDX)
                               MOVE    SYUKA-SRYKA   TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                           END-IF
                       END-IF
                   WHEN    "2"
                       IF      SYUKA-TENSU =   WRK-SYUACC-TENSU (IDX)
                           EVALUATE    SPA-SYUKA-SELECT    
                               WHEN    "0"
                                   MOVE    SPACE       TO
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               WHEN    "1"
                                   IF      SYUKA-SRYKA   <
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                               WHEN    "2"
                                   IF      SYUKA-SRYKA   >
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    SYUKA-SRYKA     TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      SYUKA-TENSU >   WRK-SYUACC-TENSU(IDX)
                               MOVE    SYUKA-TENSU TO  
                                           WRK-SYUACC-TENSU        (IDX) 
                               MOVE    SYUKA-SRYKA TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                           END-IF
                       END-IF 
               END-EVALUATE 
           END-IF
      *
      *    科別情報  
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       50
               IF      LNK-SYUACC-IO-SRYKA (IDX IDY)   =   SPACE
                   MOVE    SYUKA-SRYKA          TO
                                           LNK-SYUACC-IO-SRYKA (IDX IDY)
                   MOVE    SYUKA-HKNCOMBI-AREA  TO
                                           LNK-SYUACC-IO-HKNCOMBI-AREA
                                                               (IDX IDY)
               END-IF
               IF      SYUKA-SRYKA     =   LNK-SYUACC-IO-SRYKA (IDX IDY)
                   ADD     SYUKA-TENSU     TO
                                           LNK-SYUACC-IO-TENSU (IDX IDY)
                   ADD     SYUKA-KAISU     TO
                                           LNK-SYUACC-IO-KAISU (IDX IDY)
                   ADD     SYUKA-NISSU     TO
                                           LNK-SYUACC-IO-NISSU (IDX IDY)
                   IF      SYUKA-EDDAY     >
                                           LNK-SYUACC-IO-EDDAY (IDX IDY)                                   
                       MOVE    SYUKA-EDDAY     TO
                                           LNK-SYUACC-IO-EDDAY (IDX IDY)
                   END-IF                                   
                   MOVE    50              TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    100             TO  IDX
           .
       20011-KBN01-SYUACC-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手入力による主科設定処理
      *****************************************************************
       2002-KBN02-HENSYU-SEC       SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    手入力による主科情報の更新
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID        TO  SYUKA-PTID
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key4"          TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   1
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    ZERO            TO  SYUKA-NYUGAIKBN
               MOVE    SPA-PTID        TO  SYUKA-PTID
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               MOVE    "XXXX"          TO  SYUKA-RECESYUBETU
           END-IF
           MOVE    LNK-SYUACC-IO-H-SRYKA
                                   TO  SYUKA-SRYKA-INPUT 
      *
           IF      FLG-SYUKA       =   1
               MOVE    SMCNDATE-YMD        TO  SYUKA-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
           ELSE
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
           END-IF
           MOVE    SYUKA-REC       TO  MCPDATA-REC
      *  
           IF      FLG-SYUKA       =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    1               TO  FLG-END
               MOVE    9               TO  LNK-SYUACC-RC
               DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                       MCP-FUNC " " SYUKA-KEY
           END-IF
      *
           GO  TO  2002-KBN02-HENSYU-EXT
      * 
      *    手入力による主科情報以外の更新
           IF      FLG-END         =   ZERO
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    SPA-PTID        TO  SYUKA-PTID
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               MOVE    LNK-SYUACC-IO-H-SRYKA
                                       TO  SYUKA-SRYKA-INPUT 
      *
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
      *
               MOVE    SYUKA-REC       TO  MCPDATA-REC
      *  
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    1               TO  FLG-END
                   MOVE    9               TO  LNK-SYUACC-RC
                   DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                       MCP-FUNC " " SYUKA-KEY
               END-IF
           END-IF
           .
       2002-KBN02-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本料・診療点数更新処理
      *****************************************************************
       2003-KBN03-HENSYU-SEC       SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    対象レセプトが存在しないとき削除フラグをセット
           IF      LNK-SYUACC-IO-TEISYUTUSAKI (1)
                                       =   ZERO
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    SPA-PTID        TO  SYUKA-PTID
               MOVE    SPA-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               MOVE    "1"                 TO  SYUKA-DLT-FLG
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
      *
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    "upd3"              TO  MCP-PATHNAME
               MOVE    SYUKA-REC           TO  MCPDATA-REC  
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    1                   TO  FLG-END
                   MOVE    9                   TO  LNK-SYUACC-RC
                   DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                       MCP-FUNC " " SYUKA-KEY
               END-IF
               GO  TO  2003-KBN03-HENSYU-EXT
           END-IF        
      *
      *    取得情報から更新
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       100 
                   OR      LNK-SYUACC-IO-TEISYUTUSAKI (IDX)
                                   =   ZERO
                   OR      FLG-END =   1
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    LNK-SYUACC-IO-NYUGAIKBN   (IDX)
                                       TO  SYUKA-NYUGAIKBN
               MOVE    SPA-PTID        TO  SYUKA-PTID
               EVALUATE    LNK-SYUACC-I-KBN
                   WHEN    "03"
                       MOVE    "1"             TO  SYUKA-TEN-KBN
                   WHEN    "04"
                       MOVE    "2"             TO  SYUKA-TEN-KBN
               END-EVALUATE
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               MOVE    LNK-SYUACC-IO-TEISYUTUSAKI(IDX)
                                       TO  SYUKA-TEISYUTUSAKI
               MOVE    LNK-SYUACC-IO-HKNJANUM    (IDX)
                                       TO  SYUKA-HKNJANUM
               MOVE    LNK-SYUACC-IO-HOJOKBN     (IDX)
                                       TO  SYUKA-HOJOKBN
               MOVE    LNK-SYUACC-IO-KOHID       (IDX)
                                       TO  SYUKA-KOHID
               MOVE    LNK-SYUACC-IO-RECEIPT-KBN (IDX)
                                       TO  SYUKA-RECEIPT-KBN
               MOVE    LNK-SYUACC-IO-RECESYUBETU (IDX)
                                       TO  SYUKA-RECESYUBETU
      *
               IF    ( LNK-SYUACC-IO-SRYKA (IDX 1)
                                               =   SPACE )
               AND   ( SPA-SYUKA-NYUIN     NOT =   "2"   )
               AND   ( LNK-SYUACC-I-KBN    NOT =   "03"  )
                   MOVE    "1"             TO  SYUKA-TEN-KBN
                   MOVE    SPACE           TO  SYUKA-SRYKA
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
                   MOVE    "key"           TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYUKA-READ-SEC
                   IF      FLG-SYUKA       =   1
                       INITIALIZE                  SYUKA-REC
grpsys                 MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                       MOVE    LNK-SYUACC-IO-NYUGAIKBN   (IDX)
                                               TO  SYUKA-NYUGAIKBN
                       MOVE    SPA-PTID        TO  SYUKA-PTID
      *****************EVALUATE    LNK-SYUACC-I-KBN
      *****************    WHEN    "03"
      *****************        MOVE    "1"         TO  SYUKA-TEN-KBN
      *****************    WHEN    "04"
      *****************        MOVE    "2"         TO  SYUKA-TEN-KBN
      *****************END-EVALUATE
                       MOVE    "1"         TO  SYUKA-TEN-KBN
                       MOVE    LNK-SYUACC-I-SRYYM
                                               TO  SYUKA-SRYYM
                       MOVE    LNK-SYUACC-IO-TEISYUTUSAKI(IDX)
                                               TO  SYUKA-TEISYUTUSAKI
                       MOVE    LNK-SYUACC-IO-RECESYUBETU (IDX)
                                               TO  SYUKA-RECESYUBETU
                       MOVE    LNK-SYUACC-IO-HKNJANUM    (IDX)
                                               TO  SYUKA-HKNJANUM
                       MOVE    LNK-SYUACC-IO-HOJOKBN     (IDX)
                                               TO  SYUKA-HOJOKBN
                       MOVE    LNK-SYUACC-IO-KOHID       (IDX)
                                               TO  SYUKA-KOHID
                       MOVE    LNK-SYUACC-IO-RECEIPT-KBN (IDX)
                                               TO  SYUKA-RECEIPT-KBN
                       MOVE    SPACE           TO  SYUKA-SRYKA
                       MOVE    99999999        TO  SYUKA-TENSU
                       MOVE    999             TO  SYUKA-KAISU
                       MOVE    999             TO  SYUKA-NISSU
                       MOVE    99              TO  SYUKA-EDDAY
                   END-IF    
                   MOVE    LNK-SYUACC-IO-HKNNUM   (IDX)
                                           TO  SYUKA-HKNNUM
                   MOVE    SPACE           TO  SYUKA-DLT-FLG
      *
                   IF      FLG-SYUKA       =   1
                       MOVE    SMCNDATE-YMD    TO  SYUKA-CREYMD
                       MOVE    SMCNDATE-YMD    TO  SYUKA-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYUKA-UPHMS
                       MOVE    "DBINSERT"      TO  MCP-FUNC
                   ELSE
                       MOVE    SMCNDATE-YMD    TO  SYUKA-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYUKA-UPHMS
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                   END-IF 
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   MOVE    SYUKA-REC       TO  MCPDATA-REC  
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    1               TO  FLG-END
                       MOVE    9               TO  LNK-SYUACC-RC
                       DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                           MCP-FUNC " " SYUKA-KEY
                   END-IF
               ELSE
                 PERFORM VARYING IDY     FROM    1   BY  1
                         UNTIL   IDY     >       50
                         OR      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                         =   SPACE
                         OR      FLG-END =   1
                   MOVE    LNK-SYUACC-IO-SRYKA (IDX IDY)
                                           TO  SYUKA-SRYKA
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
                   MOVE    "key"           TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYUKA-READ-SEC
      *
                   IF      FLG-SYUKA       =   1
                       INITIALIZE                  SYUKA-REC
grpsys                 MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                       MOVE    LNK-SYUACC-IO-NYUGAIKBN   (IDX)
                                               TO  SYUKA-NYUGAIKBN
                       MOVE    SPA-PTID        TO  SYUKA-PTID
                       EVALUATE    LNK-SYUACC-I-KBN
                           WHEN    "03"
                               MOVE    "1"         TO  SYUKA-TEN-KBN
                           WHEN    "04"
                               MOVE    "2"         TO  SYUKA-TEN-KBN
                       END-EVALUATE
                       MOVE    LNK-SYUACC-I-SRYYM
                                               TO  SYUKA-SRYYM
                       MOVE    LNK-SYUACC-IO-TEISYUTUSAKI(IDX)
                                               TO  SYUKA-TEISYUTUSAKI
                       MOVE    LNK-SYUACC-IO-RECESYUBETU (IDX)
                                               TO  SYUKA-RECESYUBETU
                       MOVE    LNK-SYUACC-IO-HKNJANUM    (IDX)
                                               TO  SYUKA-HKNJANUM
                       MOVE    LNK-SYUACC-IO-HOJOKBN     (IDX)
                                               TO  SYUKA-HOJOKBN
                       MOVE    LNK-SYUACC-IO-KOHID       (IDX)
                                               TO  SYUKA-KOHID
                       MOVE    LNK-SYUACC-IO-RECEIPT-KBN (IDX)
                                               TO  SYUKA-RECEIPT-KBN
                       MOVE    LNK-SYUACC-IO-SRYKA (IDX IDY)
                                               TO  SYUKA-SRYKA
                   END-IF
                   MOVE    LNK-SYUACC-IO-HKNNUM   (IDX)
                                           TO  SYUKA-HKNNUM
                   MOVE    LNK-SYUACC-IO-HKNCOMBI-AREA (IDX IDY)
                                           TO  SYUKA-HKNCOMBI-AREA
                   MOVE    LNK-SYUACC-IO-TENSU    (IDX IDY)
                                           TO  SYUKA-TENSU
                   MOVE    LNK-SYUACC-IO-KAISU    (IDX IDY)
                                           TO  SYUKA-KAISU
                   MOVE    LNK-SYUACC-IO-NISSU    (IDX IDY)
                                           TO  SYUKA-NISSU
                   MOVE    LNK-SYUACC-IO-EDDAY    (IDX IDY)
                                           TO  SYUKA-EDDAY
                   MOVE    SPACE           TO  SYUKA-DLT-FLG
      *
                   IF      FLG-SYUKA       =   1
                       MOVE    SMCNDATE-YMD        TO  SYUKA-CREYMD
                       MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
                       MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
                   ELSE
                       MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
                       MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
                   END-IF
      *
                   IF      FLG-SYUKA       =   ZERO
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                   ELSE
                       MOVE    "DBINSERT"      TO  MCP-FUNC
                   END-IF
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   MOVE    SYUKA-REC       TO  MCPDATA-REC  
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    1               TO  FLG-END
                       MOVE    9               TO  LNK-SYUACC-RC
                       DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                               MCP-FUNC " " SYUKA-KEY
                   END-IF        
                 END-PERFORM
               END-IF
           END-PERFORM        
      *
      *    取得情報にないテーブルの削除フラグの設定
           IF      FLG-END             =   ZERO
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    SPA-PTID        TO  SYUKA-PTID
               MOVE    SPA-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               EVALUATE    LNK-SYUACC-I-KBN
                   WHEN    "03"
                       MOVE    "1"             TO  SYUKA-TEN-KBN
                   WHEN    "04"
                       MOVE    "2"             TO  SYUKA-TEN-KBN
               END-EVALUATE
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_syuka"         TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-READ-N-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUKA
               END-IF
      *
               PERFORM                 UNTIL   FLG-SYUKA   =   1
                                       OR      FLG-END     =   1
                   MOVE    ZERO            TO  FLG-DELETE
      * 
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   100
                           OR      LNK-SYUACC-IO-NYUGAIKBN    (IDX)
                                           =   SPACE
                       IF    ( SYUKA-NYUGAIKBN
                                   =   LNK-SYUACC-IO-NYUGAIKBN   (IDX) )
                       AND   ( SYUKA-TEISYUTUSAKI
                                   =   LNK-SYUACC-IO-TEISYUTUSAKI(IDX) )
                       AND   ( SYUKA-RECESYUBETU
                                   =   LNK-SYUACC-IO-RECESYUBETU (IDX) )
                       AND   ( SYUKA-HKNJANUM
                                   =   LNK-SYUACC-IO-HKNJANUM    (IDX) )
                       AND   ( SYUKA-HOJOKBN
                                   =   LNK-SYUACC-IO-HOJOKBN     (IDX) )
                       AND   ( SYUKA-KOHID
                                   =   LNK-SYUACC-IO-KOHID       (IDX) )
                       AND   ( SYUKA-RECEIPT-KBN
                                   =   LNK-SYUACC-IO-RECEIPT-KBN (IDX) )
                         IF      SYUKA-SRYKA       =   SPACE  
                           MOVE    1           TO  FLG-DELETE
                           MOVE    100         TO  IDX
                         ELSE  
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       50
                                   OR      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                   =   SPACE
                               IF      SYUKA-SRYKA     =
                                           LNK-SYUACC-IO-SRYKA (IDX IDY)
                                   MOVE    1           TO  FLG-DELETE
                                   MOVE    50          TO  IDY
                                   MOVE    100         TO  IDX
                               END-IF
                           END-PERFORM
                         END-IF
                       END-IF
                   END-PERFORM
      *
      *            対象なしのとき削除フラグセット
                   IF      FLG-DELETE          =   ZERO
                       MOVE    "1"                 TO  SYUKA-DLT-FLG
                       MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
                       MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
      *
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_syuka"         TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       MOVE    SYUKA-REC           TO  MCPDATA-REC  
grpsys                 PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    1                   TO  FLG-END
                           MOVE    9                   TO  LNK-SYUACC-RC
                           DISPLAY "ORCSSYUACC SYUKA KEY=" 
                                               MCP-FUNC " " SYUKA-KEY
                       END-IF        
                   END-IF
      *        
                   MOVE    "tbl_syuka"         TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-READ-N-SEC
               END-PERFORM   
      *     
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2003-KBN03-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科テーブル情報取得処理
      *****************************************************************
       2006-KBN06-HENSYU-SEC       SECTION.
      *
           INITIALIZE              WRK-SRYACC-AREA
      *
      *    手入力による主科情報の取得
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID        TO  SYUKA-PTID
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key4"          TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   ZERO
               MOVE    SYUKA-SRYKA-INPUT   TO  LNK-SYUACC-IO-H-SRYKA
           END-IF
      * 
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID        TO  SYUKA-PTID
           MOVE    SPA-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    LNK-SYUACC-IO-RECESYUBETU (1)
                                   TO  SYUKA-RECESYUBETU
           MOVE    LNK-SYUACC-IO-HKNJANUM    (1)
                                   TO  SYUKA-HKNJANUM
           MOVE    LNK-SYUACC-IO-HOJOKBN     (1)
                                   TO  SYUKA-HOJOKBN
           MOVE    LNK-SYUACC-IO-KOHID       (1)
                                   TO  SYUKA-KOHID
           MOVE    LNK-SYUACC-IO-RECEIPT-KBN (1)
                                   TO  SYUKA-RECEIPT-KBN
           MOVE    LNK-SYUACC-IO-TEISYUTUSAKI (1)
                                   TO  SYUKA-TEISYUTUSAKI
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           IF      LNK-SYUACC-I-KBN    =   "06"               
               MOVE    "key7"          TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key17"         TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
      *
           IF      FLG-SYUKA       =   1
               MOVE    1               TO  LNK-SYUACC-RC
           END-IF
      *
           PERFORM                 UNTIL   FLG-SYUKA   =   1
               MOVE    1               TO  IDX
               PERFORM 20011-KBN01-SYUACC-HENSYU-SEC
      *        
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-READ-N-SEC
           END-PERFORM   
      *     
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    
      *    請求点数によるシステム主科の設定（入院）
           INITIALIZE              WRK-SRYACC-AREA
      *
           IF    ( SPA-NYUGAIKBN       =   "1" )
           AND   ( SPA-SYUKA-NYUIN     =   "2" )
               MOVE   1                TO  IDX
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       50
                       OR      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                       =   SPACE
                   IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                       =   WRK-SYUACC-TENSU (IDX)
                       EVALUATE    SPA-SYUKA-SELECT    
                           WHEN    "0"
                               MOVE    SPACE       TO
                                       LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                           WHEN    "1"
                               IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                   <
                                       LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   MOVE    LNK-SYUACC-IO-SRYKA
                                                           (IDX IDY)
                                                   TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               END-IF
                           WHEN    "2"
                               IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                   >
                                       LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   MOVE    LNK-SYUACC-IO-SRYKA
                                                           (IDX IDY)
                                                   TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               END-IF
                       END-EVALUATE
                   ELSE
                       IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                               >   WRK-SYUACC-TENSU(IDX)
                           MOVE    LNK-SYUACC-IO-TENSU (IDX IDY)
                                           TO  WRK-SYUACC-TENSU (IDX) 
                           MOVE    LNK-SYUACC-IO-SRYKA (IDX IDY)  
                                           TO  LNK-SYUACC-IO-SYUKA-AUTO
                                                                (IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       2006-KBN06-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科テーブル情報取得処理
      *****************************************************************
       2007-KBN07-HENSYU-SEC       SECTION.
      *
           INITIALIZE              WRK-SRYACC-AREA
      * 
grpsys     IF  LNK-IO2-SYU-HOSPNUM(1)  =   ZERO
               MOVE    1               TO  LNK-SYUACC-RC
           END-IF
      *
           PERFORM VARYING IDW     FROM    1   BY  1
                   UNTIL   IDW >   100
grpsys             OR      LNK-IO2-SYU-HOSPNUM (IDW)   =   ZERO
               MOVE    LNK-SYUACC-IO2-OCC (IDW)
                                           TO  SYUKA-REC
      *
               IF      SYUKA-RECESYUBETU   =   "XXXX" 
                   MOVE    SYUKA-SRYKA-INPUT   TO  LNK-SYUACC-IO-H-SRYKA
               ELSE
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX >   100
                       IF      LNK-SYUACC-IO-TEISYUTUSAKI  (IDX)
                                               =   ZERO
                           MOVE    SYUKA-NYUGAIKBN
                                   TO  LNK-SYUACC-IO-NYUGAIKBN   (IDX)
                           MOVE    SYUKA-TEISYUTUSAKI
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI(IDX)
                           MOVE    SYUKA-RECESYUBETU
                                   TO  LNK-SYUACC-IO-RECESYUBETU (IDX)
                           MOVE    SYUKA-HKNJANUM
                                   TO  LNK-SYUACC-IO-HKNJANUM    (IDX)
                           MOVE    SYUKA-HOJOKBN
                                   TO  LNK-SYUACC-IO-HOJOKBN     (IDX)
                           MOVE    SYUKA-KOHID
                                   TO  LNK-SYUACC-IO-KOHID       (IDX)
                           MOVE    SYUKA-RECEIPT-KBN
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN (IDX)
                           MOVE    SYUKA-HKNNUM
                                   TO  LNK-SYUACC-IO-HKNNUM      (IDX)
                       END-IF
                       IF    ( SYUKA-NYUGAIKBN
                                   =   LNK-SYUACC-IO-NYUGAIKBN   (IDX) )
                       AND   ( SYUKA-TEISYUTUSAKI
                                   =   LNK-SYUACC-IO-TEISYUTUSAKI(IDX) )
                       AND   ( SYUKA-RECESYUBETU
                                   =   LNK-SYUACC-IO-RECESYUBETU (IDX) )
                       AND   ( SYUKA-HKNJANUM
                                   =   LNK-SYUACC-IO-HKNJANUM    (IDX) )
                       AND   ( SYUKA-HOJOKBN
                                   =   LNK-SYUACC-IO-HOJOKBN     (IDX) )
                       AND   ( SYUKA-KOHID
                                   =   LNK-SYUACC-IO-KOHID       (IDX) )
                       AND   ( SYUKA-RECEIPT-KBN
                                   =   LNK-SYUACC-IO-RECEIPT-KBN (IDX) )
      *
                           PERFORM 20011-KBN01-SYUACC-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-IF          
           END-PERFORM   
      *    
      *    請求点数によるシステム主科の設定（入院）
           INITIALIZE              WRK-SRYACC-AREA
      *
           IF    ( SPA-NYUGAIKBN       =   "1" )
           AND   ( SPA-SYUKA-NYUIN     =   "2" )
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   100
                       OR      LNK-SYUACC-IO-TEISYUTUSAKI  (IDX)
                                       =   ZERO
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       50
                           OR      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                           =   SPACE
                       IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                           =   WRK-SYUACC-TENSU (IDX)
                           EVALUATE    SPA-SYUKA-SELECT    
                               WHEN    "0"
                                   MOVE    SPACE       TO
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                               WHEN    "1"
                                   IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                       <
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    LNK-SYUACC-IO-SRYKA
                                                               (IDX IDY)
                                                       TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                               WHEN    "2"
                                   IF      LNK-SYUACC-IO-SRYKA (IDX IDY)
                                                       >
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                       MOVE    LNK-SYUACC-IO-SRYKA
                                                               (IDX IDY)
                                                       TO  
                                           LNK-SYUACC-IO-SYUKA-AUTO(IDX)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      LNK-SYUACC-IO-TENSU (IDX IDY)
                                               >   WRK-SYUACC-TENSU(IDX)
                               MOVE    LNK-SYUACC-IO-TENSU (IDX IDY)
                                           TO  WRK-SYUACC-TENSU (IDX) 
                               MOVE    LNK-SYUACC-IO-SRYKA (IDX IDY)  
                                           TO  LNK-SYUACC-IO-SYUKA-AUTO
                                                                (IDX)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM         
           END-IF
           .
       2007-KBN07-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT. 
      *
      *****************************************************************
      *    主科テーブル読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 910-SYUKA-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    主科テーブル読込
      *****************************************************************
       910-SYUKA-READ-N-SEC         SECTION.
      *
grpsys         PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUKA
               MOVE    MCPDATA-REC         TO  SYUKA-REC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
           .
       910-SYUKA-READ-N-EXT.
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
