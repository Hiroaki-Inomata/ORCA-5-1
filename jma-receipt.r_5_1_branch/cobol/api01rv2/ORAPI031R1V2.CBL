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
       PROGRAM-ID.         ORAPI031R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院患者基本取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/10/15    NACL−太田　新規作成
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
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SENTEICDCHG         PIC 9(01).
           03  FLG-SENTEI              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXK                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
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
           03  WRK-DRCD                PIC X(05)
                                       OCCURS 3.
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-KJNYMD              PIC X(08).
      *
           03  WRK-NENREI              PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NYUINRYO-G.
               05  WRK-NYUINRYO-CD     PIC X(09).
               05  WRK-NYUINRYO-NAME   PIC X(100).
      *
       01  KEY-AREA.
           03  KEY-NYUINYMD            PIC X(08).
           03  KEY-SHONUM              PIC 9(03).
           03  KEY-SRYCD               PIC X(09).
      *
      *
       01  CONST-AREA.
           03  CONST-H211001           PIC X(08)   VALUE "20091001".
           03  CONST-RRK-MAX           PIC 9(03)   VALUE 100.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    入院基本情報取得
           COPY    "CPORCSNINF01.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPHSPTINFV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSPTINFV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
           INITIALIZE                  XML-HSPTINFRES
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
           INITIALIZE                      ORCSNTYMDAREA
           MOVE    SPA-HOSPNUM         TO  SNTYMD-HOSPNUM
           MOVE    SPA-PTID            TO  SNTYMD-PTID
           MOVE    "1"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                           SPA-AREA
           IF  ( SNTYMD-RC         NOT =   ZERO )
               MOVE    "03"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( KEY-NYUINYMD    NOT =   SPACE )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0        >   SNTYMD-MAX )
                        OR   ( KEY-SHONUM  NOT =   ZERO )
      *
                   IF    ( SNTYMD-NYUINYMD (IDX0)  =   KEY-NYUINYMD )
                       IF    ( KEY-SHONUM  =   ZERO )
                               MOVE    SNTYMD-SHONUM (IDX0)
                                           TO  KEY-SHONUM
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( KEY-SHONUM      =   ZERO )
                   MOVE    "04"            TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
           END-IF
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFREQ-NYUINYMD =   SPACE )
               CONTINUE
           ELSE
               MOVE    HSPTINFREQ-NYUINYMD
                                           TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    WRK-SYMD            TO  KEY-NYUINYMD
      *
           END-IF
      *
           .
       10011-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSPTINFREQ-PTNUM   =   SPACE )
               MOVE    "02"            TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSPTINFREQ-PTNUM   TO  SPTID-PTNUM
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
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  SNINF01-AREA
           MOVE    KEY-SHONUM      TO  SNINF01-IN-SHONUM
           CALL    "ORCSNINF01"      USING
                                   SNINF01-AREA
                                   SPA-AREA
      *
           MOVE    SNINF01-PTNUM          TO  HSPTINFRES-PTNUM
           MOVE    SNINF01-NAME           TO  HSPTINFRES-NAME
           MOVE    SNINF01-KANANAME       TO  HSPTINFRES-KANANAME
           MOVE    SNINF01-BIRTHDAY       TO  HSPTINFRES-BIRTHDAY
           MOVE    SNINF01-SEX            TO  HSPTINFRES-SEX
      *
           PERFORM VARYING IDXA     FROM    1   BY  1
                  UNTIL ( IDXA     >   SNINF01-NYUINRRK-INF-MAX )
      *
               MOVE    SNINF01-RRKNUM (IDXA)
                                   TO  HSPTINFRES-RRKNUM (IDXA)
      *
               MOVE    SNINF01-CREATEKBN-LBL (IDXA)
                                   TO  HSPTINFRES-CREATEKBN-LBL (IDXA)
               MOVE    SNINF01-CREATEKBN-DATA (IDXA)
                                   TO  HSPTINFRES-CREATEKBN-DATA (IDXA)
               MOVE    SNINF01-CREATEKBN-NAME (IDXA)
                                   TO  HSPTINFRES-CREATEKBN-NAME (IDXA)
      *
               MOVE    SNINF01-NYUINYMD (IDXA)
                                   TO  HSPTINFRES-NYUINYMD (IDXA)
      *
               MOVE    SNINF01-TIMEKBN-IN-LBL (IDXA)
                                   TO  HSPTINFRES-TIMEKBN-IN-LBL  (IDXA)
               MOVE    SNINF01-TIMEKBN-IN-DATA (IDXA)
                                   TO  HSPTINFRES-TIMEKBN-IN-DATA (IDXA)
               MOVE    SNINF01-TIMEKBN-IN-NAME (IDXA)
                                   TO  HSPTINFRES-TIMEKBN-IN-NAME (IDXA)
      *
               MOVE    SNINF01-TAIINYMD (IDXA)
                                   TO  HSPTINFRES-TAIINYMD (IDXA)
      *
               MOVE    SNINF01-TAIINCD-LBL (IDXA)
                                   TO  HSPTINFRES-TAIINCD-LBL (IDXA)
               MOVE    SNINF01-TAIINCD-DATA (IDXA)
                                   TO  HSPTINFRES-TAIINCD-DATA (IDXA)
               MOVE    SNINF01-TAIINCD-NAME (IDXA)
                                   TO  HSPTINFRES-TAIINCD-NAME (IDXA)
      *
               MOVE    SNINF01-BTUNUM-LBL (IDXA)
                                   TO  HSPTINFRES-BTUNUM-LBL (IDXA)
               MOVE    SNINF01-BTUNUM-DATA (IDXA)
                                   TO  HSPTINFRES-BTUNUM-DATA (IDXA)
      *
               MOVE    SNINF01-BTUNAME-LBL (IDXA)
                                   TO  HSPTINFRES-BTUNAME-LBL (IDXA)
               MOVE    SNINF01-BTUNAME-DATA (IDXA)
                                   TO  HSPTINFRES-BTUNAME-DATA (IDXA)
      *
               MOVE    SNINF01-BRMNUM-LBL (IDXA)
                                   TO  HSPTINFRES-BRMNUM-LBL (IDXA)
               MOVE    SNINF01-BRMNUM-DATA (IDXA)
                                   TO  HSPTINFRES-BRMNUM-DATA (IDXA)
      *
               MOVE    SNINF01-SRYKA-LBL (IDXA)
                                   TO  HSPTINFRES-SRYKA-LBL (IDXA)
               MOVE    SNINF01-SRYKA-DATA (IDXA)
                                   TO  HSPTINFRES-SRYKA-DATA (IDXA)
               MOVE    SNINF01-SRYKA-NAME (IDXA)
                                   TO  HSPTINFRES-SRYKA-NAME (IDXA)
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   3 )
      *
                   MOVE    SNINF01-DR-LBL  (IDXA IDXD)
                                   TO  HSPTINFRES-DR-LBL  (IDXA IDXD)
                   MOVE    SNINF01-DR-DATA (IDXA IDXD)
                                   TO  HSPTINFRES-DR-DATA (IDXA IDXD)
                   MOVE    SNINF01-DR-NAME (IDXA IDXD)
                                   TO  HSPTINFRES-DR-NAME (IDXA IDXD)
      *
               END-PERFORM
      *
               MOVE    SNINF01-HKNCOMBI (IDXA)
                                   TO  HSPTINFRES-HKNCOMBI (IDXA)
               MOVE    SNINF01-HKNNUM (IDXA)
                                   TO  HSPTINFRES-HKNNUM (IDXA)
               MOVE    SNINF01-HKNJANUM (IDXA)
                                   TO  HSPTINFRES-HKNJANUM (IDXA)
               MOVE    SNINF01-HKNJANAME (IDXA)
                                   TO  HSPTINFRES-HKNJANAME (IDXA)
               MOVE    SNINF01-HKIGO (IDXA)
                                   TO  HSPTINFRES-HKIGO (IDXA)
               MOVE    SNINF01-HNUM (IDXA)
                                   TO  HSPTINFRES-HNUM (IDXA)
      *
               PERFORM VARYING IDXK    FROM    1   BY  1
                       UNTIL ( IDXK    >   4 )
                   MOVE    SNINF01-KOHNUM (IDXA IDXK)
                                   TO  HSPTINFRES-KOHNUM (IDXA IDXK)
                   MOVE    SNINF01-KOHNAME (IDXA IDXK)
                                   TO  HSPTINFRES-KOHNAME (IDXA IDXK)
                   MOVE    SNINF01-FTNJANUM (IDXA IDXK)
                                   TO  HSPTINFRES-FTNJANUM (IDXA IDXK)
                   MOVE    SNINF01-JKYSNUM (IDXA IDXK)
                                   TO  HSPTINFRES-JKYSNUM (IDXA IDXK)
               END-PERFORM
      *
               MOVE    SNINF01-SHOKAIYMD (IDXA)
                                   TO HSPTINFRES-SHOKAIYMD (IDXA)
               MOVE    SNINF01-KAIGOKBN-LBL (IDXA)
                                   TO HSPTINFRES-KAIGOKBN-LBL (IDXA)
               MOVE    SNINF01-KAIGOKBN-DATA (IDXA)
                                   TO HSPTINFRES-KAIGOKBN-DATA (IDXA)
               MOVE    SNINF01-KAIGOKBN-NAME (IDXA)
                                   TO HSPTINFRES-KAIGOKBN-NAME (IDXA)
               MOVE    SNINF01-SAGAKU-LBL (IDXA)
                                   TO HSPTINFRES-SAGAKU-LBL (IDXA)
               MOVE    SNINF01-SAGAKU-DATA (IDXA)
                                   TO HSPTINFRES-SAGAKU-DATA (IDXA)
               MOVE    SNINF01-SAGAKU-NAME (IDXA)
                                   TO HSPTINFRES-SAGAKU-NAME (IDXA)
               MOVE    SNINF01-SENTEIKBN-LBL (IDXA)
                                   TO HSPTINFRES-SENTEIKBN-LBL (IDXA)
               MOVE    SNINF01-SENTEIKBN-DATA (IDXA)
                                   TO HSPTINFRES-SENTEIKBN-DATA (IDXA)
               MOVE    SNINF01-SENTEIKBN-NAME (IDXA)
                                   TO HSPTINFRES-SENTEIKBN-NAME (IDXA)
               MOVE    SNINF01-NYUINRYO-LBL (IDXA)
                                   TO HSPTINFRES-NYUINRYO-LBL (IDXA)
               MOVE    SNINF01-NYUINRYO-DATA (IDXA)
                                   TO HSPTINFRES-NYUINRYO-DATA (IDXA)
               MOVE    SNINF01-NYUINRYO-NAME (IDXA)
                                   TO HSPTINFRES-NYUINRYO-NAME (IDXA)
               IF    ( SNINF01-TOKSNTCD-DATA (IDXA)    NOT =   SPACE )
                   MOVE    SNINF01-TOKSNTCD-DATA (IDXA)
                                   TO HSPTINFRES-NYUINRYO-DATA (IDXA)
                   MOVE    SNINF01-TOKSNTCD-NAME (IDXA)
                                   TO HSPTINFRES-NYUINRYO-NAME (IDXA)
               END-IF
               MOVE    SNINF01-TOKSNTKBN-LBL (IDXA)
                                   TO HSPTINFRES-TOKSNTKBN-LBL (IDXA)
               MOVE    SNINF01-TOKSNTKBN-DATA (IDXA)
                                   TO HSPTINFRES-TOKSNTKBN-DATA (IDXA)
               MOVE    SNINF01-TOKSNTKBN-NAME (IDXA)
                                   TO  HSPTINFRES-TOKSNTKBN-NAME (IDXA)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   3 )
                   MOVE    SNINF01-KSNSRYCD-LBL (IDXA IDX1)
                               TO HSPTINFRES-KSNSRYCD-LBL (IDXA IDX1)
                   MOVE    SNINF01-KSNSRYCD-DATA (IDXA IDX1)
                               TO HSPTINFRES-KSNSRYCD-DATA (IDXA IDX1)
                   MOVE    SNINF01-KSNSRYCD-NAME (IDXA IDX1)
                               TO HSPTINFRES-KSNSRYCD-NAME (IDXA IDX1)
               END-PERFORM
               MOVE    SNINF01-NACCTKBN-LBL (IDXA)
                                   TO HSPTINFRES-NACCTKBN-LBL (IDXA)
               MOVE    SNINF01-NACCTKBN-DATA (IDXA)
                                   TO HSPTINFRES-NACCTKBN-DATA (IDXA)
               MOVE    SNINF01-NACCTKBN-NAME (IDXA)
                                   TO HSPTINFRES-NACCTKBN-NAME (IDXA)
               MOVE    SNINF01-BUNBENKBN-LBL (IDXA)
                                   TO HSPTINFRES-BUNBENKBN-LBL (IDXA)
               MOVE    SNINF01-BUNBENKBN-DATA (IDXA)
                                   TO HSPTINFRES-BUNBENKBN-DATA (IDXA)
               MOVE    SNINF01-BUNBENKBN-NAME (IDXA)
                                   TO HSPTINFRES-BUNBENKBN-NAME (IDXA)
               MOVE    SNINF01-RIYOUKBN-LBL (IDXA)
                                   TO HSPTINFRES-RIYOUKBN-LBL (IDXA)
               MOVE    SNINF01-RIYOUKBN-DATA (IDXA)
                                   TO HSPTINFRES-RIYOUKBN-DATA (IDXA)
               MOVE    SNINF01-RIYOUKBN-NAME (IDXA)
                                   TO HSPTINFRES-RIYOUKBN-NAME (IDXA)
               MOVE    SNINF01-TEIKIKBN-LBL (IDXA)
                                   TO HSPTINFRES-TEIKIKBN-LBL (IDXA)
               MOVE    SNINF01-TEIKIKBN-DATA (IDXA)
                                   TO HSPTINFRES-TEIKIKBN-DATA (IDXA)
               MOVE    SNINF01-TEIKIKBN-NAME (IDXA)
                                   TO HSPTINFRES-TEIKIKBN-NAME (IDXA)
               MOVE   SNINF01-DISPKBN-LBL (IDXA)
                                   TO HSPTINFRES-DISPKBN-LBL (IDXA)
               MOVE   SNINF01-DISPKBN-DATA (IDXA)
                                   TO HSPTINFRES-DISPKBN-DATA (IDXA)
               MOVE   SNINF01-DISPKBN-NAME (IDXA)
                                   TO HSPTINFRES-DISPKBN-NAME (IDXA)
               MOVE   SNINF01-SKJKBN-LBL (IDXA)
                                   TO HSPTINFRES-SKJKBN-LBL (IDXA)
               MOVE   SNINF01-SKJKBN-DATA (IDXA)
                                   TO HSPTINFRES-SKJKBN-DATA (IDXA)
               MOVE   SNINF01-SKJKBN-NAME (IDXA)
                                   TO HSPTINFRES-SKJKBN-NAME (IDXA)
               IF    ( SNINF01-SKJKBN-DATA (IDXA)  =   "B" OR "C" )
                   PERFORM VARYING IDXB   FROM 1   BY  1
                           UNTIL ( IDXB   >   3 )
                       MOVE    SNINF01-TSKJKBN-LBL (IDXA IDXB)
                               TO  HSPTINFRES-TSKJKBN-LBL (IDXA IDXB)
                       MOVE    SNINF01-TSKJKBN-DATA (IDXA IDXB)
                               TO  HSPTINFRES-TSKJKBN-DATA (IDXA IDXB)
                       MOVE    SNINF01-TSKJKBN-NAME (IDXA IDXB)
                               TO  HSPTINFRES-TSKJKBN-NAME (IDXA IDXB)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSPTINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTINFRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTINFRES-API-RESULT-MSG
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
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "00"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "01"
               MOVE    "診療年月の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "入退院情報の取得に失敗しました"
                                       TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "指定された入院日の入院情報が存在しません"
                                       TO  WRK-ERRMSG
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
           IF    ( APILST17-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    APILST17-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "private_objects"   TO  APIREQ-RECORDNAME
           MOVE    "xml_hsptinfv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST17-BODY
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
                                       TO  HSPTINFREQ-PRIVATE-OBJECTS
      *
           IF    ( HSPTINFREQ-PTNUM    =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTINFREQ-PTNUM
           END-IF
      *
           IF    ( HSPTINFREQ-NYUINYMD =   LOW-VALUE )
               MOVE    SPACE           TO  HSPTINFREQ-NYUINYMD
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
           IF    ( APILST17-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsptinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    HSPTINFRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsptinfv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST17-BODY
               MOVE    "application/xml"   TO  APILST17-CONTENT-TYPE
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
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
               MOVE    404             TO  APILST17-HTTPSTATUS
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
           MOVE    WRK-DATE            TO  HSPTINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSPTINFRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSPTINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSPTINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
