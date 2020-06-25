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
       PROGRAM-ID.             ORCDBSEIKYU.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB 請求管理アクセスモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/24    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  04.05.01    NACL-藤原    09/11/13  特記事項照会対応
      *                                     特記事項略称追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.02    NACL-藤原    10/11/30  統計帳票用の項目追加
      *                                     請求額情報（医療費、食事）
      *                                     保険組合せ情報
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *                                     直接請求フラグの追加
      *  04.07.03    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分（自賠責用）の追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SKYETC          PIC 9(01).
           03  FLG-SKYKOH          PIC 9(01).
           03  FLG-SKYRSI          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-KOH             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
      *
       01  CONST-AREA.
           03  CONST-SRYKBNINF-MAX     PIC 9(03)   VALUE   45.
           03  CONST-KOH-MAX           PIC 9(03)   VALUE   4.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    請求管理（新）
           COPY    "CPSKYMAIN.INC".
      *
      *    請求管理（公費情報）
           COPY    "CPSKYKOH.INC".
      *
      *    請求管理（レセプト記載外公費情報）
           COPY    "CPSKYETC.INC".
      *
      *    請求管理（労災情報）
           COPY    "CPSKYRSI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-EDIT-SEIKYU-SEC
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理編集処理
      *****************************************************************
       200-EDIT-SEIKYU-SEC             SECTION.
      *
           MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
           MOVE    MCPDATA-REC         TO  RECE10-REC
      *
           EVALUATE    MCP-FUNC
           WHEN    "DBSELECT"
               PERFORM 2001-EDIT-SKYMAIN-SEC
               MOVE    SKYMAIN-REC     TO  MCPDATA-REC
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
           WHEN    "DBCLOSECURSOR"
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    "DBFETCH"
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
               IF    ( MCP-RC  =   ZERO )
                   PERFORM 2001-SKYMAIN-FET-SEC
               END-IF
           WHEN    "DBUPDATE"
               PERFORM 2001-SKYMAIN-UPD-SEC
           WHEN    "DBDELETE"
               PERFORM 2001-SKYMAIN-DEL-SEC
           WHEN    "DBINSERT"
               PERFORM 2001-SKYMAIN-INS-SEC
           END-EVALUATE
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
      *
           .
       200-EDIT-SEIKYU-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）編集処理
      *****************************************************************
       2001-EDIT-SKYMAIN-SEC          SECTION.
      *
           INITIALIZE                          SKYMAIN-REC
           MOVE    RECE10-KEY              TO  SKYMAIN-KEY
           MOVE    RECE10-PTNUM            TO  SKYMAIN-PTNUM
           MOVE    RECE10-PREFKBN          TO  SKYMAIN-PREFKBN
           MOVE    RECE10-PREFNUM          TO  SKYMAIN-PREFNUM
           MOVE    RECE10-AGE              TO  SKYMAIN-AGE
           MOVE    RECE10-KYURATE          TO  SKYMAIN-KYURATE
           MOVE    RECE10-ZAITAKU          TO  SKYMAIN-ZAITAKU
           MOVE    RECE10-SKYYM            TO  SKYMAIN-SKYYM
           MOVE    RECE10-HENREIYM         TO  SKYMAIN-HENREIYM
           MOVE    RECE10-SKYKBN           TO  SKYMAIN-SKYKBN
           MOVE    RECE10-SKYKBN-RECEDEN   TO  SKYMAIN-SKYKBN-RECEDEN
           MOVE    RECE10-TEISEIKBN        TO  SKYMAIN-TEISEIKBN
           MOVE    RECE10-HKNNUM           TO  SKYMAIN-HKNNUM
           MOVE    RECE10-HKNID            TO  SKYMAIN-HKNID
           MOVE    RECE10-HONKZKKBN        TO  SKYMAIN-HONKZKKBN
           MOVE    RECE10-HOJOKBN          TO  SKYMAIN-HOJOKBN
           MOVE    RECE10-CONTKBN          TO  SKYMAIN-CONTKBN
           MOVE    RECE10-KIGO             TO  SKYMAIN-KIGO
           MOVE    RECE10-NUM              TO  SKYMAIN-NUM
           MOVE    RECE10-RJNHKNNUM        TO  SKYMAIN-RJNHKNNUM
           MOVE    RECE10-RJNHKNID         TO  SKYMAIN-RJNHKNID
           MOVE    RECE10-KOHNUM (1)       TO  SKYMAIN-KOHNUM (1)
           MOVE    RECE10-KOHNUM (2)       TO  SKYMAIN-KOHNUM (2)
           MOVE    RECE10-KOHNUM (3)       TO  SKYMAIN-KOHNUM (3)
           MOVE    RECE10-KOHNUM (4)       TO  SKYMAIN-KOHNUM (4)
           MOVE    RECE10-JNISSU (1)       TO  SKYMAIN-JNISSU
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    RECE10-KHNTEN (IDX1)    TO  SKYMAIN-KHNTEN (IDX1)
               MOVE    RECE10-KAISU  (IDX1)    TO  SKYMAIN-KAISU  (IDX1)
               MOVE    RECE10-TENSU  (IDX1 1)  TO  SKYMAIN-TENSU  (IDX1)
           END-PERFORM
           MOVE    RECE10-TOTALTEN(1)      TO  SKYMAIN-TOTALTEN
           MOVE    RECE10-FTNMONEY(1)      TO  SKYMAIN-FTNMONEY
           MOVE    RECE10-FTNMONEY-FTN(1)  TO  SKYMAIN-FTNMONEY-FTN
           MOVE    RECE10-FTNMONEY-KOJ(1)  TO  SKYMAIN-FTNMONEY-KOJ
           MOVE    RECE10-SHOKUJINISSU(1)  TO  SKYMAIN-SHOKUJINISSU
           MOVE    RECE10-SHOKUJIRYOYOHI(1)
                                           TO SKYMAIN-SHOKUJIRYOYOHI
           MOVE    RECE10-SHOKUJIFTN(1)    TO  SKYMAIN-SHOKUJIFTN
           MOVE    RECE10-SHOKUJIFTNKBN    TO  SKYMAIN-SHOKUJIFTNKBN
           MOVE    RECE10-YKZFTN(1)        TO  SKYMAIN-YKZFTN
           MOVE    RECE10-SKYIRYO-NISSU    TO  SKYMAIN-SKYIRYO-NISSU
           MOVE    RECE10-SKYIRYO-TOTALTEN TO  SKYMAIN-SKYIRYO-TOTALTEN
           MOVE    RECE10-SKYIRYO-ICHIBUFTN
                                           TO  SKYMAIN-SKYIRYO-ICHIBUFTN
           MOVE    RECE10-SKYIRYO-HOKENJA  TO  SKYMAIN-SKYIRYO-HOKENJA
           MOVE    RECE10-SKYIRYO-KOHFTN   TO  SKYMAIN-SKYIRYO-KOHFTN
           MOVE    RECE10-SKYSKJ-KAISU     TO  SKYMAIN-SKYSKJ-KAISU
           MOVE    RECE10-SKYSKJ-RYOYOHI   TO  SKYMAIN-SKYSKJ-RYOYOHI
           MOVE    RECE10-SKYSKJ-ICHIBUFTN TO  SKYMAIN-SKYSKJ-ICHIBUFTN
           MOVE    RECE10-SKYSKJ-HOKENJA   TO  SKYMAIN-SKYSKJ-HOKENJA
           MOVE    RECE10-SKYSKJ-KOHFTN    TO  SKYMAIN-SKYSKJ-KOHFTN
           MOVE    RECE10-HKNCOMBI (1)     TO  SKYMAIN-HKNCOMBI (1)
           MOVE    RECE10-HKNCOMBI (2)     TO  SKYMAIN-HKNCOMBI (2)
           MOVE    RECE10-HKNCOMBI (3)     TO  SKYMAIN-HKNCOMBI (3)
           MOVE    RECE10-HKNCOMBI (4)     TO  SKYMAIN-HKNCOMBI (4)
           MOVE    RECE10-HKNCOMBI (5)     TO  SKYMAIN-HKNCOMBI (5)
           MOVE    RECE10-HKNCOMBI (6)     TO  SKYMAIN-HKNCOMBI (6)
           MOVE    RECE10-HKNCOMBI (7)     TO  SKYMAIN-HKNCOMBI (7)
           MOVE    RECE10-HKNCOMBI (8)     TO  SKYMAIN-HKNCOMBI (8)
           MOVE    RECE10-HKNCOMBI (9)     TO  SKYMAIN-HKNCOMBI (9)
           MOVE    RECE10-HKNCOMBI (10)    TO  SKYMAIN-HKNCOMBI (10)
           MOVE    RECE10-TOKKI (1)        TO  SKYMAIN-TOKKI (1)
           MOVE    RECE10-TOKKI (2)        TO  SKYMAIN-TOKKI (2)
           MOVE    RECE10-TOKKI (3)        TO  SKYMAIN-TOKKI (3)
           MOVE    RECE10-TOKKI (4)        TO  SKYMAIN-TOKKI (4)
           MOVE    RECE10-TOKKI (5)        TO  SKYMAIN-TOKKI (5)
           MOVE    RECE10-TOKKINM (1)      TO  SKYMAIN-TOKKINM (1)
           MOVE    RECE10-TOKKINM (2)      TO  SKYMAIN-TOKKINM (2)
           MOVE    RECE10-TOKKINM (3)      TO  SKYMAIN-TOKKINM (3)
           MOVE    RECE10-TOKKINM (4)      TO  SKYMAIN-TOKKINM (4)
           MOVE    RECE10-TOKKINM (5)      TO  SKYMAIN-TOKKINM (5)
           MOVE    RECE10-FLG   (1)        TO  SKYMAIN-FLG   (1)
           MOVE    RECE10-FLG   (2)        TO  SKYMAIN-FLG   (2)
           MOVE    RECE10-FLG   (3)        TO  SKYMAIN-FLG   (3)
           MOVE    RECE10-FLG   (4)        TO  SKYMAIN-FLG   (4)
           MOVE    RECE10-FLG   (5)        TO  SKYMAIN-FLG   (5)
           MOVE    RECE10-RESERVE-AREA     TO  SKYMAIN-RESERVE-AREA
           MOVE    RECE10-HENREI-SIJI-FLG  TO  SKYMAIN-HENREI-SIJI-FLG
           MOVE    RECE10-DIRECT-FLG       TO  SKYMAIN-DIRECT-FLG
           MOVE    RECE10-HKNKBN-RSI       TO  SKYMAIN-HKNKBN-RSI
           MOVE    RECE10-HKNNUM-S         TO  SKYMAIN-HKNNUM-S
           MOVE    RECE10-HKNJANUM-S       TO  SKYMAIN-HKNJANUM-S
           MOVE    RECE10-CREYMD           TO  SKYMAIN-CREYMD
           MOVE    RECE10-CREHMS           TO  SKYMAIN-CREHMS
           MOVE    RECE10-UPDYMD           TO  SKYMAIN-UPDYMD
           MOVE    RECE10-UPDHMS           TO  SKYMAIN-UPDHMS
           MOVE    RECE10-UPHKNJANUM       TO  SKYMAIN-UPHKNJANUM
           MOVE    RECE10-XXPREFNUM        TO  SKYMAIN-XXPREFNUM
           MOVE    RECE10-XXSRYYM          TO  SKYMAIN-XXSRYYM
           MOVE    RECE10-XXSTHKNJANUM     TO  SKYMAIN-XXSTHKNJANUM
           MOVE    RECE10-XXEDHKNJANUM     TO  SKYMAIN-XXEDHKNJANUM
           MOVE    RECE10-XXTEISYUTUSAKI   TO  SKYMAIN-XXTEISYUTUSAKI
           IF    ( RECE10-ETC-KOHNUM (1)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (2)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (3)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (4)   NOT =   SPACE )
             OR  ( RECE10-ETC-YKZFTN (1)       >   ZERO  )
             OR  ( RECE10-ETC-YKZFTN (2)       >   ZERO  )
               MOVE    1                   TO  SKYMAIN-ETCCREATEFLG
           END-IF
      *
           .
       2001-EDIT-SKYMAIN-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）FETCH処理
      *****************************************************************
       2001-SKYMAIN-FET-SEC            SECTION.
      *
           MOVE    MCPDATA-REC             TO  SKYMAIN-REC
      *
           INITIALIZE                          RECE10-REC
           MOVE    SKYMAIN-KEY             TO  RECE10-KEY
           MOVE    SKYMAIN-PTNUM           TO  RECE10-PTNUM
           MOVE    SKYMAIN-PREFKBN         TO  RECE10-PREFKBN
           MOVE    SKYMAIN-PREFNUM         TO  RECE10-PREFNUM
           MOVE    SKYMAIN-AGE             TO  RECE10-AGE
           MOVE    SKYMAIN-KYURATE         TO  RECE10-KYURATE
           MOVE    SKYMAIN-ZAITAKU         TO  RECE10-ZAITAKU
           MOVE    SKYMAIN-SKYYM           TO  RECE10-SKYYM
           MOVE    SKYMAIN-HENREIYM        TO  RECE10-HENREIYM
           MOVE    SKYMAIN-SKYKBN          TO  RECE10-SKYKBN
           MOVE    SKYMAIN-SKYKBN-RECEDEN  TO  RECE10-SKYKBN-RECEDEN
           MOVE    SKYMAIN-TEISEIKBN       TO  RECE10-TEISEIKBN
           MOVE    SKYMAIN-HKNNUM          TO  RECE10-HKNNUM
           MOVE    SKYMAIN-HKNID           TO  RECE10-HKNID
           MOVE    SKYMAIN-HONKZKKBN       TO  RECE10-HONKZKKBN
           MOVE    SKYMAIN-HOJOKBN         TO  RECE10-HOJOKBN
           MOVE    SKYMAIN-CONTKBN         TO  RECE10-CONTKBN
           MOVE    SKYMAIN-KIGO            TO  RECE10-KIGO
           MOVE    SKYMAIN-NUM             TO  RECE10-NUM
           MOVE    SKYMAIN-RJNHKNNUM       TO  RECE10-RJNHKNNUM
           MOVE    SKYMAIN-RJNHKNID        TO  RECE10-RJNHKNID
           MOVE    SKYMAIN-KOHNUM (1)      TO  RECE10-KOHNUM (1)
           MOVE    SKYMAIN-KOHNUM (2)      TO  RECE10-KOHNUM (2)
           MOVE    SKYMAIN-KOHNUM (3)      TO  RECE10-KOHNUM (3)
           MOVE    SKYMAIN-KOHNUM (4)      TO  RECE10-KOHNUM (4)
           MOVE    SKYMAIN-JNISSU          TO  RECE10-JNISSU (1)
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    SKYMAIN-KHNTEN (IDX1)    TO  RECE10-KHNTEN (IDX1)
               MOVE    SKYMAIN-KAISU  (IDX1)    TO  RECE10-KAISU  (IDX1)
               MOVE    SKYMAIN-TENSU  (IDX1)    TO  RECE10-TENSU(IDX1 1)
           END-PERFORM
           MOVE    SKYMAIN-TOTALTEN        TO  RECE10-TOTALTEN(1)
           MOVE    SKYMAIN-FTNMONEY        TO  RECE10-FTNMONEY(1)
           MOVE    SKYMAIN-FTNMONEY-FTN    TO  RECE10-FTNMONEY-FTN(1)
           MOVE    SKYMAIN-FTNMONEY-KOJ    TO  RECE10-FTNMONEY-KOJ(1)
           MOVE    SKYMAIN-SHOKUJINISSU    TO  RECE10-SHOKUJINISSU(1)
           MOVE    SKYMAIN-SHOKUJIRYOYOHI  TO  RECE10-SHOKUJIRYOYOHI(1)
           MOVE    SKYMAIN-SHOKUJIFTN      TO  RECE10-SHOKUJIFTN(1)
           MOVE    SKYMAIN-SHOKUJIFTNKBN   TO  RECE10-SHOKUJIFTNKBN
           MOVE    SKYMAIN-YKZFTN          TO  RECE10-YKZFTN(1)
           MOVE    SKYMAIN-SKYIRYO-NISSU   TO  RECE10-SKYIRYO-NISSU
           MOVE    SKYMAIN-SKYIRYO-TOTALTEN
                                           TO  RECE10-SKYIRYO-TOTALTEN
           MOVE    SKYMAIN-SKYIRYO-ICHIBUFTN
                                           TO  RECE10-SKYIRYO-ICHIBUFTN
           MOVE    SKYMAIN-SKYIRYO-HOKENJA TO  RECE10-SKYIRYO-HOKENJA
           MOVE    SKYMAIN-SKYIRYO-KOHFTN  TO  RECE10-SKYIRYO-KOHFTN
           MOVE    SKYMAIN-SKYSKJ-KAISU    TO  RECE10-SKYSKJ-KAISU
           MOVE    SKYMAIN-SKYSKJ-RYOYOHI  TO  RECE10-SKYSKJ-RYOYOHI
           MOVE    SKYMAIN-SKYSKJ-ICHIBUFTN
                                           TO  RECE10-SKYSKJ-ICHIBUFTN
           MOVE    SKYMAIN-SKYSKJ-HOKENJA  TO  RECE10-SKYSKJ-HOKENJA
           MOVE    SKYMAIN-SKYSKJ-KOHFTN   TO  RECE10-SKYSKJ-KOHFTN
           MOVE    SKYMAIN-HKNCOMBI (1)    TO  RECE10-HKNCOMBI (1)
           MOVE    SKYMAIN-HKNCOMBI (2)    TO  RECE10-HKNCOMBI (2)
           MOVE    SKYMAIN-HKNCOMBI (3)    TO  RECE10-HKNCOMBI (3)
           MOVE    SKYMAIN-HKNCOMBI (4)    TO  RECE10-HKNCOMBI (4)
           MOVE    SKYMAIN-HKNCOMBI (5)    TO  RECE10-HKNCOMBI (5)
           MOVE    SKYMAIN-HKNCOMBI (6)    TO  RECE10-HKNCOMBI (6)
           MOVE    SKYMAIN-HKNCOMBI (7)    TO  RECE10-HKNCOMBI (7)
           MOVE    SKYMAIN-HKNCOMBI (8)    TO  RECE10-HKNCOMBI (8)
           MOVE    SKYMAIN-HKNCOMBI (9)    TO  RECE10-HKNCOMBI (9)
           MOVE    SKYMAIN-HKNCOMBI (10)   TO  RECE10-HKNCOMBI (10)
           MOVE    SKYMAIN-TOKKI (1)       TO  RECE10-TOKKI (1)
           MOVE    SKYMAIN-TOKKI (2)       TO  RECE10-TOKKI (2)
           MOVE    SKYMAIN-TOKKI (3)       TO  RECE10-TOKKI (3)
           MOVE    SKYMAIN-TOKKI (4)       TO  RECE10-TOKKI (4)
           MOVE    SKYMAIN-TOKKI (5)       TO  RECE10-TOKKI (5)
           MOVE    SKYMAIN-TOKKINM (1)     TO  RECE10-TOKKINM (1)
           MOVE    SKYMAIN-TOKKINM (2)     TO  RECE10-TOKKINM (2)
           MOVE    SKYMAIN-TOKKINM (3)     TO  RECE10-TOKKINM (3)
           MOVE    SKYMAIN-TOKKINM (4)     TO  RECE10-TOKKINM (4)
           MOVE    SKYMAIN-TOKKINM (5)     TO  RECE10-TOKKINM (5)
           MOVE    SKYMAIN-FLG   (1)       TO  RECE10-FLG   (1)
           MOVE    SKYMAIN-FLG   (2)       TO  RECE10-FLG   (2)
           MOVE    SKYMAIN-FLG   (3)       TO  RECE10-FLG   (3)
           MOVE    SKYMAIN-FLG   (4)       TO  RECE10-FLG   (4)
           MOVE    SKYMAIN-FLG   (5)       TO  RECE10-FLG   (5)
           MOVE    SKYMAIN-RESERVE-AREA    TO  RECE10-RESERVE-AREA
           MOVE    SKYMAIN-HENREI-SIJI-FLG TO  RECE10-HENREI-SIJI-FLG
           MOVE    SKYMAIN-DIRECT-FLG      TO  RECE10-DIRECT-FLG
           MOVE    SKYMAIN-HKNKBN-RSI      TO  RECE10-HKNKBN-RSI
           MOVE    SKYMAIN-HKNNUM-S        TO  RECE10-HKNNUM-S
           MOVE    SKYMAIN-HKNJANUM-S      TO  RECE10-HKNJANUM-S
           MOVE    SKYMAIN-CREYMD          TO  RECE10-CREYMD
           MOVE    SKYMAIN-CREHMS          TO  RECE10-CREHMS
           MOVE    SKYMAIN-UPDYMD          TO  RECE10-UPDYMD
           MOVE    SKYMAIN-UPDHMS          TO  RECE10-UPDHMS
           MOVE    SKYMAIN-UPHKNJANUM      TO  RECE10-UPHKNJANUM
           MOVE    SKYMAIN-XXPREFNUM       TO  RECE10-XXPREFNUM
           MOVE    SKYMAIN-XXSRYYM         TO  RECE10-XXSRYYM
           MOVE    SKYMAIN-XXSTHKNJANUM    TO  RECE10-XXSTHKNJANUM
           MOVE    SKYMAIN-XXEDHKNJANUM    TO  RECE10-XXEDHKNJANUM
           MOVE    SKYMAIN-XXTEISYUTUSAKI  TO  RECE10-XXTEISYUTUSAKI
      *
           IF    ( SKYMAIN-KOHNUM (1)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (2)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (3)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (4)      NOT =   SPACE )
                   PERFORM 20011-SKYKOH-GET-SEC
           END-IF
      *
           IF      SKYMAIN-HKNNUM              =   "971"   OR  "973"
                                                           OR  "970"
               PERFORM 900-SKYRSI-KEY-SEL-SEC
               MOVE    SKYRSI-SYOCD            TO  RECE10-SYOCD
               MOVE    SKYRSI-SYOKAIFLG        TO  RECE10-SYOKAIFLG
               MOVE    SKYRSI-KOFUNUM          TO  RECE10-KOFUNUM
               MOVE    SKYRSI-SAIGAIKBN        TO  RECE10-SAIGAIKBN
               MOVE    SKYRSI-COMMENT          TO  RECE10-COMMENT
               MOVE    SKYRSI-RECEKBN          TO  RECE10-RECEKBN
               MOVE    SKYRSI-HKNKBN           TO  RECE10-HKNKBN
           END-IF
      *
           IF    ( SKYMAIN-ETCCREATEFLG    =   1 )
               PERFORM 900-SKYETC-KEY-SEL-SEC
               PERFORM VARYING IDX-KOH FROM    1   BY  1
                       UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                   MOVE    SKYETC-ETC-KOHNUM (IDX-KOH)
                                   TO  RECE10-ETC-KOHNUM      (IDX-KOH)
                   MOVE    SKYETC-ETC-JNISSU (IDX-KOH)
                                   TO  RECE10-ETC-JNISSU      (IDX-KOH)
                   MOVE    SKYETC-ETC-TOTALTEN (IDX-KOH)
                                   TO  RECE10-ETC-TOTALTEN    (IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY    (IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY-FTN (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY-FTN(IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY-KOJ (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY-KOJ(IDX-KOH)
                   MOVE    SKYETC-ETC-YKZFTN (IDX-KOH)
                                   TO  RECE10-ETC-YKZFTN      (IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJINISSU (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJINISSU(IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJIRYOYOHI (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJIRYOYOHI
                                                              (IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJIFTN (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJIFTN  (IDX-KOH)
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                    TO  MCP-RC
      *
           .
       2001-SKYMAIN-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）取得処理
      *****************************************************************
       20011-SKYKOH-GET-SEC            SECTION.
      *
           PERFORM 900-SKYKOH-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SKYKOH  NOT =   ZERO )
      *
               COMPUTE IDX2    =   SKYKOH-RENNUM   +   1
      *
               MOVE    SKYKOH-KOHID    TO  RECE10-KOHID (SKYKOH-RENNUM)
               MOVE    SKYKOH-JNISSU   TO  RECE10-JNISSU(IDX2)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
                   MOVE    SKYKOH-TENSU (IDX1)
                                           TO  RECE10-TENSU (IDX1 IDX2)
               END-PERFORM
               MOVE    SKYKOH-TOTALTEN     TO  RECE10-TOTALTEN (IDX2)
               MOVE    SKYKOH-FTNMONEY     TO  RECE10-FTNMONEY (IDX2)
               MOVE    SKYKOH-FTNMONEY-FTN TO  RECE10-FTNMONEY-FTN(IDX2)
               MOVE    SKYKOH-FTNMONEY-KOJ TO  RECE10-FTNMONEY-KOJ(IDX2)
               MOVE    SKYKOH-SHOKUJINISSU TO  RECE10-SHOKUJINISSU(IDX2)
               MOVE    SKYKOH-SHOKUJIRYOYOHI
                                           TO  RECE10-SHOKUJIRYOYOHI
                                                                  (IDX2)
               MOVE    SKYKOH-SHOKUJIFTN   TO  RECE10-SHOKUJIFTN  (IDX2)
               MOVE    SKYKOH-YKZFTN       TO  RECE10-YKZFTN      (IDX2)
      *
               PERFORM 900-SKYKOH-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SKYKOH-GET-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）更新処理
      *****************************************************************
       2001-SKYMAIN-UPD-SEC            SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYRSI-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
           END-IF
      *
           .
       2001-SKYMAIN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）追加処理
      *****************************************************************
       2001-SKYMAIN-INS-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYRSI-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           .
       2001-SKYMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）削除処理
      *****************************************************************
       2001-SKYMAIN-DEL-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-DEL-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-DEL-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYRSI-DEL-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC        TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           .
       2001-SKYMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）削除処理
      *****************************************************************
       2001-SKYKOH-DEL-SEC              SECTION.
      *
           INITIALIZE                      SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
           IF    ( MCP-PATHNAME    =   "del1" OR "del3" OR "del4" 
                                    OR "del6" OR "del7" OR "del8"
                                    OR "del9" OR "del10" OR "del11"
                                    OR "del12" OR "del13" OR "del14"
                                    OR "del15" OR "del16" OR "del17" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SKYKOH-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SKYKOH-DEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）追加処理
      *****************************************************************
       2001-SKYKOH-INS-SEC              SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SKYKOH-DEL-SEC
      *
           PERFORM VARYING IDX-KOH      FROM 1 BY  1
                   UNTIL ( IDX-KOH      >    CONST-KOH-MAX )
                    OR   ( MCP-RC  NOT  =   ZERO )
      *
               IF    ( SKYMAIN-KOHNUM (IDX-KOH)    NOT =   SPACE )
                   PERFORM 20011-SKYKOH-EDIT-SEC
                   MOVE    SKYKOH-REC          TO  MCPDATA-REC
                   MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SKYKOH-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）編集処理
      *****************************************************************
       20011-SKYKOH-EDIT-SEC           SECTION.
      *
           COMPUTE IDX2    =   IDX-KOH +   1
      *
           INITIALIZE                          SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
      *
           MOVE    IDX-KOH                 TO  SKYKOH-RENNUM
           MOVE    RECE10-KOHNUM(IDX-KOH)  TO  SKYKOH-KOHNUM
           MOVE    RECE10-KOHID (IDX-KOH)  TO  SKYKOH-KOHID
           MOVE    RECE10-JNISSU(IDX2)     TO  SKYKOH-JNISSU
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    RECE10-TENSU (IDX1 IDX2)
                                           TO  SKYKOH-TENSU (IDX1)
           END-PERFORM
           MOVE    RECE10-TOTALTEN(IDX2)   TO  SKYKOH-TOTALTEN
           MOVE    RECE10-FTNMONEY (IDX2)  TO  SKYKOH-FTNMONEY
           MOVE    RECE10-FTNMONEY-FTN (IDX2)
                                           TO  SKYKOH-FTNMONEY-FTN
           MOVE    RECE10-FTNMONEY-KOJ (IDX2)
                                           TO  SKYKOH-FTNMONEY-KOJ 
           MOVE    RECE10-SHOKUJINISSU (IDX2)
                                           TO  SKYKOH-SHOKUJINISSU 
           MOVE    RECE10-SHOKUJIRYOYOHI(IDX2)
                                           TO  SKYKOH-SHOKUJIRYOYOHI
           MOVE    RECE10-SHOKUJIFTN   (IDX2)
                                           TO  SKYKOH-SHOKUJIFTN
           MOVE    RECE10-YKZFTN       (IDX2)
                                           TO  SKYKOH-YKZFTN
      *
           .
       20011-SKYKOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）削除処理
      *****************************************************************
       2001-SKYETC-DEL-SEC             SECTION.
      *
           INITIALIZE                  SKYETC-REC
           PERFORM 9001-SKYETC-KEY-EDIT-SEC
           IF    ( MCP-PATHNAME    =   "del1" OR "del3" OR "del4" 
                                    OR "del6" OR "del7" OR "del8"
                                    OR "del9" OR "del10" OR "del11"
                                    OR "del12" OR "del13" OR "del14"
                                    OR "del15" OR "del16" OR "del17" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SKYETC-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SKYETC-DEL-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（レセプト記載外公費情報）追加処理
      *****************************************************************
       2001-SKYETC-INS-SEC             SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SKYETC-DEL-SEC
      *
           IF    ( SKYMAIN-ETCCREATEFLG    =   1 )
      *
               INITIALIZE                  SKYETC-REC
               PERFORM 9001-SKYETC-KEY-EDIT-SEC
               PERFORM VARYING IDX-KOH FROM    1   BY  1
                       UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                   MOVE    RECE10-ETC-KOHNUM      (IDX-KOH)
                                   TO  SKYETC-ETC-KOHNUM       (IDX-KOH)
                   MOVE    RECE10-ETC-JNISSU      (IDX-KOH)
                                   TO  SKYETC-ETC-JNISSU       (IDX-KOH)
                   MOVE    RECE10-ETC-TOTALTEN    (IDX-KOH)
                                   TO  SKYETC-ETC-TOTALTEN     (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY    (IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY     (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY-FTN(IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY-FTN (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY-KOJ(IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY-KOJ (IDX-KOH)
                   MOVE    RECE10-ETC-YKZFTN      (IDX-KOH)
                                   TO  SKYETC-ETC-YKZFTN       (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJINISSU(IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJINISSU (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJIRYOYOHI (IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJIRYOYOHI
                                                               (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJIFTN (IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJIFTN   (IDX-KOH)
               END-PERFORM
               MOVE    SKYETC-REC          TO  MCPDATA-REC
               MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
           END-IF
      *
           .
       2001-SKYETC-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（労災情報）削除処理
      *****************************************************************
       2001-SKYRSI-DEL-SEC             SECTION.
      *
           INITIALIZE                  SKYRSI-REC
           PERFORM 9001-SKYRSI-KEY-EDIT-SEC
           IF    ( MCP-PATHNAME    =   "del1" OR "del3" OR "del4" 
                                    OR "del6" OR "del7" OR "del8"
                                    OR "del9" OR "del10" OR "del11"
                                    OR "del12" OR "del13" OR "del14"
                                    OR "del15" OR "del16" OR "del17" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SKYRSI-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_rsi"    TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SKYRSI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（労災情報）追加処理
      *****************************************************************
       2001-SKYRSI-INS-SEC             SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SKYRSI-DEL-SEC
      *
           IF    ( SKYMAIN-HKNNUM    =   "971"   OR  "973"   OR  "970" )
      *
               INITIALIZE                  SKYRSI-REC
               PERFORM 9001-SKYRSI-KEY-EDIT-SEC
               MOVE    RECE10-SYOCD        TO  SKYRSI-SYOCD
               MOVE    RECE10-SYOKAIFLG    TO  SKYRSI-SYOKAIFLG
               MOVE    RECE10-KOFUNUM      TO  SKYRSI-KOFUNUM
               MOVE    RECE10-SAIGAIKBN    TO  SKYRSI-SAIGAIKBN
               MOVE    RECE10-COMMENT      TO  SKYRSI-COMMENT
               MOVE    RECE10-RECEKBN      TO  SKYRSI-RECEKBN
               MOVE    RECE10-HKNKBN       TO  SKYRSI-HKNKBN
               MOVE    SKYRSI-REC          TO  MCPDATA-REC
               MOVE    "tbl_seikyu_rsi"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
           END-IF
      *
           .
       2001-SKYRSI-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）検索処理
      *****************************************************************
       900-SKYKOH-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYKOH
      *
           INITIALIZE                      SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
           MOVE    SKYKOH-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYKOH-REC
           ELSE
               MOVE    1               TO  FLG-SKYKOH
               INITIALIZE                  SKYKOH-REC
           END-IF
      *
           .
       900-SKYKOH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）キー情報編集処理
      *****************************************************************
       9001-SKYKOH-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SKYMAIN-KEY         TO  SKYKOH-KEY1
           MOVE    SKYMAIN-XXTEISYUTUSAKI
                                       TO  SKYKOH-XXTEISYUTUSAKI
           MOVE    SKYMAIN-HKNNUM      TO  SKYKOH-XXHKNNUM
           MOVE    SKYMAIN-HKNKBN-RSI  TO  SKYKOH-XXHKNKBN-RSI
      *
           .
       9001-SKYKOH-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）FETCH処理
      *****************************************************************
       900-SKYKOH-KEY2-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYKOH
      *
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYKOH-REC
           ELSE
               MOVE    1               TO  FLG-SKYKOH
               INITIALIZE                  SKYKOH-REC
           END-IF
      *
           .
       900-SKYKOH-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）検索処理
      *****************************************************************
       900-SKYETC-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYETC
      *
           INITIALIZE                      SKYETC-REC
           PERFORM 9001-SKYETC-KEY-EDIT-SEC
           MOVE    SKYETC-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYETC-REC
           ELSE
               MOVE    1               TO  FLG-SKYETC
               INITIALIZE                  SKYETC-REC
           END-IF
      *
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SKYETC-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）キー情報編集処理
      *****************************************************************
       9001-SKYETC-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SKYMAIN-KEY         TO  SKYETC-KEY
           MOVE    SKYMAIN-XXTEISYUTUSAKI
                                       TO  SKYETC-XXTEISYUTUSAKI
           MOVE    SKYMAIN-HKNNUM      TO  SKYETC-XXHKNNUM
           MOVE    SKYMAIN-HKNKBN-RSI  TO  SKYETC-XXHKNKBN-RSI
           .
       9001-SKYETC-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（労災情報）検索処理
      *****************************************************************
       900-SKYRSI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYRSI
      *
           INITIALIZE                      SKYRSI-REC
           PERFORM 9001-SKYRSI-KEY-EDIT-SEC
           MOVE    SKYRSI-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_rsi"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYRSI-REC
           ELSE
               MOVE    1               TO  FLG-SKYRSI
               INITIALIZE                  SKYRSI-REC
           END-IF
      *
           MOVE    "tbl_seikyu_rsi"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SKYRSI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（労災情報）キー情報編集処理
      *****************************************************************
       9001-SKYRSI-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SKYMAIN-KEY         TO  SKYRSI-KEY
           MOVE    SKYMAIN-XXTEISYUTUSAKI
                                       TO  SKYRSI-XXTEISYUTUSAKI
           MOVE    SKYMAIN-HKNNUM      TO  SKYRSI-XXHKNNUM
           MOVE    SKYMAIN-HKNKBN-RSI  TO  SKYRSI-XXHKNKBN-RSI
           .
       9001-SKYRSI-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
       990-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
