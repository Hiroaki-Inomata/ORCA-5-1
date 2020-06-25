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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI034R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 退院仮計算
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/11/14    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXJ                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXS                    PIC 9(05).
           03  IDXT                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-S02MSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
      *
       01  KEY-AREA.
           03  KEY-TAIINYMD            PIC X(08).
           03  KEY-SKYSTYMD            PIC X(08).
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    分娩入院チェックサブ
           COPY  "CPORCSBUNBENCHK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    請求情報編集サブ
           COPY    "CPORCSNINF02.INC".
      *
      *    負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    負担金計算サブエラーメッセージ編集
           COPY    "CPORCS02MSG.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPHSACSIMULATEV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSACSIMULATEV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
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
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-HSACSIMULATERES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "99"         TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE   "89"         TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           PERFORM 10011-TAIINYMD-CHECK-SEC
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-PTNYUINRRK-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    退院日チェック処理
      *****************************************************************
       10011-TAIINYMD-CHECK-SEC        SECTION.
      *
           IF    ( HSACSREQ-TAIINYMD   =   SPACE )
               MOVE    SPA-SYSYMD  TO  KEY-TAIINYMD
           ELSE
               MOVE    HSACSREQ-TAIINYMD
                                   TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    WRK-SYMD    TO  KEY-TAIINYMD
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "01"    TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           MOVE    KEY-TAIINYMD    TO  SPA-SRYYMD
      *
           .
       10011-TAIINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSACSREQ-PTNUM   =   SPACE )
               MOVE    "02"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSACSREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "02"        TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "02"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴チェック処理
      *****************************************************************
       10011-PTNYUINRRK-CHECK-SEC      SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY29-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                   MOVE    "04"    TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           ELSE
               MOVE    "03"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    請求開始日取得処理
           PERFORM 100111-SKYSTYMD-GET-SEC
      *
           .
       10011-PTNYUINRRK-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理
      *****************************************************************
       100111-SKYSTYMD-GET-SEC         SECTION.
      *
           MOVE    SPACE           TO  KEY-SKYSTYMD
      *
           PERFORM 900-PTTEIKIRRK-KEY7-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  =   ZERO )
               MOVE    PTTEIKIRRK-SKYEDYMD
                                   TO  WRK-SYMD
               MOVE    "1"         TO  WRK-ZOGENPTN
               MOVE    1           TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD    TO  KEY-SKYSTYMD
           ELSE
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  KEY-SKYSTYMD
           END-IF
      *
           IF    ( KEY-SKYSTYMD    >   KEY-TAIINYMD )
               MOVE    "05"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    KEY-SKYSTYMD    TO  WRK-SYMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           IF    ( KEY-TAIINYMD (1 : 6)
                                   <=  WRK-SYMD (1 : 6) )
               CONTINUE
           ELSE
               MOVE    "06"        TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100111-SKYSTYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    KEY-SKYSTYMD    TO  HSACSRES-SKYSTYMD
           MOVE    KEY-TAIINYMD    TO  HSACSRES-SKYEDYMD
      *
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSACSRES-NYUINYMD
      *
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-ORCSNYUFTN-SEC
      *
           PERFORM 2001-SYUNOU-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-PTNUM           TO  HSACSRES-PTNUM
           MOVE    PTINF-NAME          TO  HSACSRES-NAME
           MOVE    PTINF-KANANAME      TO  HSACSRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACSRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  HSACSRES-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    負担金計算処理
      *****************************************************************
       2001-ORCSNYUFTN-SEC                 SECTION.
      *
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           INITIALIZE                  SBUNBENCHK-AREA
           MOVE    SPA-PTID
                                   TO  SBUNBENCHK-PTID
           MOVE    KEY-SKYSTYMD    TO  SBUNBENCHK-SKYSTYMD
           MOVE    KEY-TAIINYMD    TO  SBUNBENCHK-SKYEDYMD
           CALL    "ORCSBUNBENCHK" USING
                                   SBUNBENCHK-AREA
                                   SPA-AREA
      *
           MOVE    SPA-HOSPNUM     TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    SPA-PTID        TO  LNK-S02-PTID
           MOVE    KEY-SKYSTYMD    TO  LNK-S02-SKYSTYMD
           MOVE    KEY-TAIINYMD    TO  LNK-S02-SKYEDYMD
           MOVE    "Y"             TO  LNK-S02-TEIKI-KBN
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           IF    ( SBUNBENCHK-BUNBEN   =   ZERO )
               MOVE    "1"         TO  LNK-S02-SYORI-KBN
           ELSE
               MOVE    "8"         TO  LNK-S02-SYORI-KBN
           END-IF
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      NOT =   ZERO )
               INITIALIZE              S02MSG-AREA
               MOVE    LNK-S02-RC  TO  S02MSG-S02-RC
               CALL    "ORCS02MSG" USING
                                   S02MSG-AREA
                                   SPA-AREA
               MOVE    "07"        TO  WRK-ERRCD
               MOVE    SPACE       TO  WRK-S02MSG
               IF    ( S02MSG-MSG (1)  =   SPACE )
                   MOVE    "負担金計算に失敗しました"
                                   TO  WRK-S02MSG
               ELSE
                   STRING  "負担金計算に失敗しました。"
                                           DELIMITED BY SIZE
                           S02MSG-MSG (1)  DELIMITED BY SPACE
                   INTO    WRK-S02MSG
                   END-STRING
               END-IF
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2001-ORCSNYUFTN-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       2001-SYUNOU-EDIT-SEC            SECTION.
      *
           INITIALIZE                  SNINF02-AREA
      *
           MOVE    KEY-SKYSTYMD   TO  SNINF02-IN-SKYSTYMD
           MOVE    KEY-TAIINYMD   TO  SNINF02-IN-SKYEDYMD
      *
           CALL    "ORCSNINF02"    USING
                                   SNINF02-AREA
                                   LNK-SYUNOU-REC-T
                                   SPA-AREA
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SNINF02-SKYINF-MAX )
      *
               MOVE    SNINF02-SKYINF-KBN-LBL (IDXA)
                               TO  HSACSRES-SKYINF-KBN-LBL (IDXA)
               MOVE    SNINF02-SKYINF-KBN-DATA (IDXA)
                               TO  HSACSRES-SKYINF-KBN-DATA (IDXA)
               MOVE    SNINF02-SKYINF-KBN-NAME (IDXA)
                               TO  HSACSRES-SKYINF-KBN-NAME (IDXA)
               MOVE    SNINF02-SRYYM (IDXA)
                               TO  HSACSRES-SRYYM (IDXA)
               MOVE    SNINF02-SRYKA (IDXA)
                               TO  HSACSRES-SRYKA (IDXA)
               MOVE    SNINF02-SRYKANAME (IDXA)
                               TO  HSACSRES-SRYKANAME (IDXA)
      *
               MOVE    SNINF02-HKNCOMBI (IDXA)
                               TO  HSACSRES-HKNCOMBI (IDXA)
               MOVE    SNINF02-HKNNUM (IDXA)
                               TO  HSACSRES-HKNNUM (IDXA)
               MOVE    SNINF02-HKNJANUM (IDXA)
                               TO  HSACSRES-HKNJANUM (IDXA)
               MOVE    SNINF02-HKNJANAME (IDXA)
                               TO  HSACSRES-HKNJANAME (IDXA)
               MOVE    SNINF02-HKIGO (IDXA)
                               TO  HSACSRES-HKIGO (IDXA)
               MOVE    SNINF02-HNUM (IDXA)
                               TO  HSACSRES-HNUM (IDXA)
      *
               PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
                   MOVE    SNINF02-KOHNUM (IDXA IDXK)
                               TO  HSACSRES-KOHNUM (IDXA IDXK)
                   MOVE    SNINF02-KOHNAME (IDXA IDXK)
                               TO  HSACSRES-KOHNAME (IDXA IDXK)
                   MOVE    SNINF02-FTNJANUM (IDXA IDXK)
                               TO  HSACSRES-FTNJANUM (IDXA IDXK)
                   MOVE    SNINF02-JKYSNUM (IDXA IDXK)
                               TO  HSACSRES-JKYSNUM (IDXA IDXK)
               END-PERFORM
      *
               MOVE    SNINF02-SKYMONEY (IDXA)
                               TO  HSACSRES-SKYMONEY (IDXA)
               MOVE    SNINF02-HKNMONEY (IDXA)
                               TO  HSACSRES-HKNMONEY (IDXA)
               MOVE    SNINF02-JIHIMONEY (IDXA)
                               TO  HSACSRES-JIHIMONEY (IDXA)
               MOVE    SNINF02-RJNMONEY (IDXA)
                               TO  HSACSRES-RJNMONEY (IDXA)
               MOVE    SNINF02-KOHMONEY (IDXA)
                               TO  HSACSRES-KOHMONEY (IDXA)
               MOVE    SNINF02-SKJMONEY (IDXA)
                               TO  HSACSRES-SKJMONEY (IDXA)
               MOVE    SNINF02-RSIMONEY (IDXA)
                               TO  HSACSRES-RSIMONEY (IDXA)
               MOVE    SNINF02-GENMEN (IDXA)
                               TO  HSACSRES-GENMEN (IDXA)
               MOVE    SNINF02-TOTAL-TEN (IDXA)
                               TO  HSACSRES-TOTAL-TEN (IDXA)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   16 )
                   MOVE    SNINF02-SRYKBN-NAME (IDXA IDX0)
                               TO  HSACSRES-SRYKBN-NAME  (IDXA IDX0)
                   MOVE    SNINF02-SRYKBN-TENSU (IDXA IDX0)
                               TO  HSACSRES-SRYKBN-TENSU (IDXA IDX0)
               END-PERFORM
      *
               MOVE    SNINF02-SKJRYOYO (IDXA)
                                   TO  HSACSRES-SKJRYOYO (IDXA)
      *
           END-PERFORM
      *
           .
       2001-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACSRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSACSRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSACSRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSACSRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "00"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "01"
               MOVE    "退院日の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "入院情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "退院登録済みの患者です"
                                       TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "請求開始日＞退院日です"
                                       TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "請求期間の月数が２か月を超えています"
                                       TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    WRK-S02MSG      TO  WRK-ERRMSG
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                                               DELIMITED  BY  SIZE
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "92"
               STRING  "診療年月は平成２０年（２００８年）"
                                       DELIMITED BY SIZE
                       "４月以降"
                                       DELIMITED BY SIZE
                       "を指定してください"
                                       DELIMITED BY SIZE
               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "97"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST18-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsfoodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsacsimulatev2req" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST18-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "private_objects"   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"                 TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  APILST18-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  HSACSREQ-PRIVATE-OBJECTS
      *
           IF    ( HSACSREQ-PTNUM      =   LOW-VALUE )
               MOVE    SPACE           TO  HSACSREQ-PTNUM
           END-IF
      *
           IF    ( HSACSREQ-TAIINYMD   =   LOW-VALUE )
               MOVE    SPACE           TO  HSACSREQ-TAIINYMD
           END-IF
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST18-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsfoodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    HSACSRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsacsimulatev2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST18-BODY
               MOVE    "application/xml"   TO  APILST18-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
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
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY29-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY29-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           IF    ( KEY-SKYSTYMD (1:6)  NOT =   KEY-TAIINYMD (1:6))
            AND  ( KEY-SKYSTYMD (1:6)      =   SYU-SKYSTYMD (1:6))
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-GETUMATU-SEC
               MOVE    WRK-SYMD        TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
           ELSE
               MOVE    KEY-TAIINYMD    TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
           END-IF
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY7-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTTEIKIRRK-PTID
           MOVE    PTNYUINRRK-RRKNUM   TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "99" )
               MOVE    404             TO  APILST18-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACSRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSACSRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSACSRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSACSRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
