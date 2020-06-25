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
       PROGRAM-ID.         ORAPI023R2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納情報取得
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
      *    収納ファイル
           SELECT  IN01-FILE       ASSIGN   ASS-IN01
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    収納ファイル
       FD  IN01-FILE.
       01  IN01-REC.
           COPY    "CPSYUNOU.INC"  REPLACING       //SYU-//
                                   BY              //IN01-//.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT3.INC".
      *
           COPY    "CPCOMMONDAT3.INC"      REPLACING    //TMPFLPARA//
                                           BY           //ASS-IN01//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  STS-AREA.
           03  STS-IN01                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-IN01                PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-SYUNOU              PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXJ                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXN                    PIC 9(05).
           03  IDXS                    PIC 9(05).
           03  IDXT                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
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
           03  WRK-SKYINF-KBN          PIC X(01).
           03  WRK-KINGAKU             PIC S9(10).
           03  WRK-KINGAKUM9           PIC ---------9.
           03  WRK-KINGAKUMM           PIC ----------.
           03  WRK-RATEZ               PIC ZZ9.
           03  WRK-HKNCOMBI.
               05  WRK-HKNCOMBINUM     PIC 9(04).
           03  WRK-MISYU-TOTAL         PIC S9(10).
           03  WRK-KBNCDJ.
               05  WRK-KBNCDJ9         PIC 9(02).
           03  WRK-NOZ                 PIC Z9.
      *
       01  KEY-AREA.
           03  KEY-RANGE               PIC X(01).
           03  KEY-SRYYMD              PIC X(08).
      *
       01  TCOMB-AREA.
           03  TCOMB-MAX               PIC 9(05).
           03  TCOMB-OCC               OCCURS  30.
               05  TCOMB-HKNCOMBI      PIC X(04).
      *
      *    請求点数名称
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(03)   VALUE   "A00".
           03  FILLER              PIC X(30)   VALUE   "初・再診料".
           03  FILLER              PIC X(03)   VALUE   "B00".
           03  FILLER              PIC X(30)   VALUE   "医学管理等".
           03  FILLER              PIC X(03)   VALUE   "C00".
           03  FILLER              PIC X(30)   VALUE   "在宅療養".
           03  FILLER              PIC X(03)   VALUE   "F00".
           03  FILLER              PIC X(30)   VALUE   "投薬".
           03  FILLER              PIC X(03)   VALUE   "G00".
           03  FILLER              PIC X(30)   VALUE   "注射".
           03  FILLER              PIC X(03)   VALUE   "J00".
           03  FILLER              PIC X(30)   VALUE   "処置".
           03  FILLER              PIC X(03)   VALUE   "K00".
           03  FILLER              PIC X(30)   VALUE   "手術".
           03  FILLER              PIC X(03)   VALUE   "L00".
           03  FILLER              PIC X(30)   VALUE   "麻酔".
           03  FILLER              PIC X(03)   VALUE   "D00".
           03  FILLER              PIC X(30)   VALUE   "検査".
           03  FILLER              PIC X(03)   VALUE   "E00".
           03  FILLER              PIC X(30)   VALUE   "画像診断".
           03  FILLER              PIC X(03)   VALUE   "H00".
           03  FILLER              PIC X(30)   VALUE   "リハビリ".
           03  FILLER              PIC X(03)   VALUE   "I00".
           03  FILLER              PIC X(30)   VALUE   "精神科専門".
           03  FILLER              PIC X(03)   VALUE   "M00".
           03  FILLER              PIC X(30)   VALUE   "放射線治療".
           03  FILLER              PIC X(03)   VALUE   "N00".
           03  FILLER              PIC X(30)   VALUE   "病理診断".
           03  FILLER              PIC X(03)   VALUE   "A10".
           03  FILLER              PIC X(30)   VALUE   "入院料".
           03  FILLER              PIC X(03)   VALUE   "001".
           03  FILLER              PIC X(30)   VALUE   "療養担当手当".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI-OCC  OCCURS   16.
               05  TBL-HKNTEN-KBN  PIC X(03).
               05  TBL-HKNTEN-MEI  PIC X(30).
      *
      *
       01  CONST-AREA.
           03  CONST-TCOMB-MAX         PIC 9(05)   VALUE   30.
           03  CONST-TJIHINAME-MAX     PIC 9(05)   VALUE   40.
           03  CONST-SYUNOU-MAX        PIC 9(05)   VALUE   200.
           03  CONST-MISYU-MAX         PIC 9(05)   VALUE   50.
           03  CONST-H190401           PIC X(08)   VALUE   "20070401".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPINCOMEINFV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPINCOMEINFV2RES.INC".
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
      *    自費名称
           COPY    "CPSK1013.INC".
      *    収納機能情報
           COPY    "CPSK1039.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TCOMB-AREA
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
           INITIALIZE                  XML-INCOMEINFRES
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
               MOVE   "0099"       TO  WRK-ERRCD
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
               MOVE   "0089"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1039"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1039-REC
      *
           MOVE    SPA-HOSPNUM     TO  TMPFLPARA-HOSPNUM
           MOVE    "API023R2"      TO  TMPFLPARA-FILE-ID
           MOVE    SPA-TERMID      TO  TMPFLPARA-TERMID
      *
           MOVE    TMPFLPARA       TO  ASS-IN01
      *
           MOVE    1               TO  ASS-IN01-SYOKBN1
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
           PERFORM 10011-SRYYMD-CHECK-SEC
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    診療日チェック処理
      *****************************************************************
       10011-SRYYMD-CHECK-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN    ( ICINFREQ-SRYYMD   NOT =   SPACE )
               MOVE    "D"             TO  KEY-RANGE
               MOVE    ICINFREQ-SRYYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0001"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               MOVE    WRK-SYMD        TO  KEY-SRYYMD
           WHEN    ( ICINFREQ-SRYYM    NOT =   SPACE )
               MOVE    "M"             TO  KEY-RANGE
               MOVE    ICINFREQ-SRYYM  TO  WRK-DATE
               MOVE    "-01"           TO  WRK-DATE(8:)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0002"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               MOVE    WRK-SYMD    TO  KEY-SRYYMD
           WHEN    ( ICINFREQ-SRYYEAR  NOT =   SPACE )
               MOVE    "Y"             TO  KEY-RANGE
               MOVE    ICINFREQ-SRYYEAR TO WRK-DATE
               MOVE    "-01-01"         TO WRK-DATE(5:)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0003"  TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
               MOVE    WRK-SYMD    TO  KEY-SRYYMD
           WHEN    OTHER
               MOVE    SPA-SYSYMD  TO  KEY-SRYYMD
           END-EVALUATE
      *
           .
       10011-SRYYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( ICINFREQ-PTNUM   =   SPACE )
               MOVE    "0004"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    ICINFREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "0004"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "0004"          TO  WRK-ERRCD
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
           PERFORM 2001-SYUNOU-GET-SEC
      *
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-SYUNOU-EDIT-SEC
      *
           PERFORM 2001-HOKEN-EDIT-SEC
      *
           PERFORM 2001-MISYU-GET-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報取得処理
      *****************************************************************
       2001-SYUNOU-GET-SEC         SECTION.
      *
           OPEN    OUTPUT          IN01-FILE
      *
           MOVE    ZERO        TO  CNT-SYUNOU
      *
           PERFORM 900-SYUNOU-KEY85-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU NOT =   ZERO )
      *
               COMPUTE CNT-SYUNOU  =   CNT-SYUNOU  +   1
      *
               MOVE    SYUNOU-REC  TO  IN01-REC
      *
               WRITE   IN01-REC
      *
               PERFORM 900-SYUNOU-KEY85-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key85"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   IN01-FILE
      *
           .
       2001-SYUNOU-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-PTNUM           TO  ICINFRES-PTNUM
           MOVE    PTINF-NAME          TO  ICINFRES-NAME
           MOVE    PTINF-KANANAME      TO  ICINFRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  ICINFRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  ICINFRES-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       2001-SYUNOU-EDIT-SEC            SECTION.
      *
           OPEN    INPUT               IN01-FILE
      *
           IF    ( CNT-SYUNOU          >   ZERO )
               IF    ( CNT-SYUNOU      >   CONST-SYUNOU-MAX )
                   MOVE    "true"      TO  ICINFRES-INCONEINF-OVER
               ELSE
                   MOVE    "false"     TO  ICINFRES-INCONEINF-OVER
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                    OR   ( IDXA    >=  CONST-SYUNOU-MAX )
      *
               COMPUTE IDXA    =   IDXA    +   1
      *
               PERFORM 20011-SYUNOU-EDIT-SEC
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   IN01-FILE
      *
           .
       2001-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       20011-SYUNOU-EDIT-SEC           SECTION.
      *
           MOVE    IN01-REC            TO  SYUNOU-REC
      *
           PERFORM 200111-TCOMB-EDIT-SEC
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  ICINFRES-SRYYMD (IDXA)
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  ICINFRES-SKYEDYMD (IDXA)
           ELSE
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  ICINFRES-SRYYMD (IDXA)
           END-IF
      *
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  ICINFRES-DENPPRTYMD (IDXA)
      *
           MOVE    SYU-NYUGAIKBN       TO  ICINFRES-NYUGAIKBN (IDXA)
           MOVE    SYU-DENPNUM         TO  ICINFRES-DENPNUM   (IDXA)
           MOVE    SYU-HKNCOMBINUM     TO  ICINFRES-HKNCOMBI  (IDXA)
           MOVE    SYU-PTFTNRATE       TO  WRK-RATEZ
           MOVE    WRK-RATEZ           TO  ICINFRES-FTNRATE   (IDXA)
      *
           PERFORM 200111-SRYKA-EDIT-SEC
      *
           MOVE    SYU-SKYMONEY        TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  ICINFRES-SKYMONEY (IDXA)
           MOVE    SYU-SKYMONEY-TAX-SAI
                                   TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-SKYMONEY-TAX-SAI (IDXA)
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  ICINFRES-NYUKIN   (IDXA)
           MOVE    SYU-HKNTEKMONEY     TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  ICINFRES-HKNMONEY (IDXA)
      *
           COMPUTE WRK-KINGAKU         =   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
           IF    ( SYU-SRYYMD  <   CONST-H190401 )
               COMPUTE WRK-KINGAKU
                   =   WRK-KINGAKU +   SYU-JIHI-TAX
           END-IF
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  ICINFRES-JIHIMONEY (IDXA)
      *
           MOVE    SYU-YKZ-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-YKZMONEY  (IDXA)
      *
           MOVE    SYU-RJN-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-RJNMONEY  (IDXA)
      *
           MOVE    SYU-KOH-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-KOHMONEY  (IDXA)
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               COMPUTE WRK-KINGAKU         =   SYU-SKYMONEY-SKJ-KEI
                                           +   SYU-SKYMONEY-LIFE-KEI
               MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9       TO  ICINFRES-SLMONEY (IDXA)
      *
               MOVE    SYU-SKYMONEY-SKJ-KEI TO WRK-KINGAKUMM
               MOVE    WRK-KINGAKUMM       TO  ICINFRES-SKJMONEY (IDXA)
      *
               MOVE    SYU-SKYMONEY-LIFE-KEI TO WRK-KINGAKUMM
               MOVE    WRK-KINGAKUMM       TO  ICINFRES-LIFEMONEY (IDXA)
      *
           END-IF
      *
           MOVE    SYU-RSI-TOTAL       TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-RSI-TOTAL (IDXA)
      *
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-RSIMONEY (IDXA)
      *
           MOVE    SYU-DISCOUNT-MONEY  TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-GENMEN (IDXA)
      *
           MOVE    SYU-CHOSEI1         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-CHOSEI1 (IDXA)
      *
           MOVE    SYU-CHOSEI2         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  ICINFRES-CHOSEI2 (IDXA)
      *
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  ICINFRES-TOTAL-TEN (IDXA)
      *
           COMPUTE WRK-KINGAKU
               =   SYU-TOTAL-TGMONEY
               +   SYU-TOTAL-TGMONEY-TAX
           MOVE    WRK-KINGAKU     TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-TOTAL-TGMONEY (IDXA)
      *
           MOVE    SYU-TGMONEY-TAX-SAI
                                   TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-TGMONEY-TAX-SAI (IDXA)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   16 )
      *
               IF    ( IDX0                    NOT =   16 )
                 OR  ( SYU-HKNTEN (IDX0)       NOT =   ZERO )
                 OR  ( SYU-TGMONEY(IDX0)       NOT =   ZERO )
                 OR  ( SYU-TGMONEY-TAX (IDX0)  NOT =   ZERO )
                   MOVE    TBL-HKNTEN-KBN (IDX0)
                               TO  ICINFRES-SRYITEM-KBN  (IDXA IDX0)
                   MOVE    TBL-HKNTEN-MEI (IDX0)
                               TO  ICINFRES-SRYITEM-NAME  (IDXA IDX0)
                   MOVE    SYU-HKNTEN (IDX0)
                               TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                               TO  ICINFRES-SRYITEM-TENSU (IDXA IDX0)
                   COMPUTE WRK-KINGAKU
                       =   SYU-TGMONEY (IDX0)
                       +   SYU-TGMONEY-TAX (IDX0)
                   MOVE    WRK-KINGAKU
                               TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                               TO  ICINFRES-SRYITEM-TGMONEY (IDXA IDX0)
               END-IF
           END-PERFORM
           MOVE    SYU-JIHI-TOTAL  TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-JIHI-TOTAL (IDXA)
      *
           MOVE    SYU-JIHI-TOTAL-TAX
                                   TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-JIHI-TOTAL-TAX (IDXA)
      *
           MOVE    SYU-JIHI-TAX    TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  ICINFRES-JIHI-TOTAL-TAX-SAI(IDXA)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
      *
               MOVE    2               TO  IDXN
               IF    ( SYU-NYUGAIKBN   =   "1" )
                   MOVE    1           TO  IDXN
               END-IF
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "1013"              TO  SYS-KANRICD
               MOVE    SYU-NYUGAIKBN       TO  SYS-KBNCD(1:1)
               MOVE    IDX0                TO  WRK-KBNCDJ9
               MOVE    WRK-KBNCDJ (2:1)    TO  SYS-KBNCD(2:1)
               MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1013-REC
      *
               IF    ( SYS-1013-JIHINAME           NOT =   SPACE )
                OR   ( SYU-JIHI-TAXNASI (IDX0)     NOT =   ZERO )
                OR   ( SYU-JIHI-TAXARI (IDX0)      NOT =   ZERO )
      *
                   MOVE    IDX0    TO  WRK-NOZ
                   MOVE    WRK-NOZ TO  ICINFRES-JIHI-NO (IDXA IDX0)
      *
                   MOVE    SYS-1013-JIHINAME
                                   TO  ICINFRES-JIHI-NAME (IDXA IDX0)
                   MOVE    SYU-JIHI-TAXNASI (IDX0)
                                   TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                                   TO  ICINFRES-JIHI-TAXNASI (IDXA IDX0)
                   MOVE    SYU-JIHI-TAXARI (IDX0)
                                   TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                                   TO  ICINFRES-JIHI-TAXARI (IDXA IDX0)
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    SYU-RSISHOSHIN-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RSISHOSHIN-MONEY (IDXA)
      *
           MOVE    SYU-RSISAISHIN-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RSISAISHIN-MONEY (IDXA)
      *
           MOVE    SYU-RSISDO-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RSISDO-MONEY (IDXA)
      *
           MOVE    SYU-RSIETC-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RSIETC-MONEY (IDXA)
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               COMPUTE WRK-KINGAKU     =   SYU-RYOYOHI-SKJ
                                       +   SYU-RYOYOHI-LIFE
                                       +   SYU-RYOYOHI-SKJ-JIHI
                                       +   SYU-RYOYOHI-LIFE-JIHI
               MOVE    WRK-KINGAKU     TO  WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9   TO  ICINFRES-SLRYOYO (IDXA)
           END-IF
      *
           MOVE    SYU-RYOYOHI-SKJ
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-SKJRYOYO (IDXA)
      *
           MOVE    SYU-RYOYOHI-LIFE
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-LIFERYOYO (IDXA)
      *
           MOVE    SYU-RYOYOHI-SKJ-JIHI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-SKJRYOYO-JIHI (IDXA)
      *
           MOVE    SYU-SKYMONEY-SKJ-JIHI-KEI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-SKJFTN-JIHI (IDXA)
      *
           MOVE    SYU-RYOYOHI-LIFE-JIHI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-LIFERYOYO-JIHI (IDXA)
      *
           MOVE    SYU-SKYMONEY-LIFE-JIHI-KEI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-LIFEFTN-JIHI (IDXA)
      *
           MOVE    SYU-RMSAGAKU
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RMSAGAKU (IDXA)
      *
           MOVE    SYU-RMSAGAKU-TAX-SAI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  ICINFRES-RMSAGAKU-TAX-SAI (IDXA)
      *
           .
       20011-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       200111-SRYKA-EDIT-SEC          SECTION.
      *
           MOVE    SYU-SRYKA           TO  ICINFRES-SRYKA (IDXA)
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    SYU-SRYKA           TO  SYS-KBNCD
           MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME  TO  ICINFRES-SRYKANAME (IDXA)
      *
           .
       200111-SRYKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合退避領域編集処理
      *****************************************************************
       200111-TCOMB-EDIT-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXC
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   CONST-TCOMB-MAX )
      *
               EVALUATE    TRUE
               WHEN  ( TCOMB-HKNCOMBI (IDXC)   =   SPACE )
                   MOVE    SYU-HKNCOMBINUM
                                   TO  TCOMB-HKNCOMBI (IDXC)
                   MOVE    IDXC    TO  TCOMB-MAX
                   MOVE    CONST-TCOMB-MAX TO  IDXC
               WHEN  ( SYU-HKNCOMBINUM
                                   =   TCOMB-HKNCOMBI (IDXC))
                   MOVE    CONST-TCOMB-MAX TO  IDXC
               END-EVALUATE
      *
           END-PERFORM
      *
          .
       200111-TCOMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       2001-HOKEN-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   TCOMB-MAX )
      *
               MOVE    TCOMB-HKNCOMBI (IDXA)
                                   TO  ICINFRES-HKNCOMBI-KEY (IDXA)
      *
               MOVE    TCOMB-HKNCOMBI (IDXA)
                                   TO  WRK-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               IF    ( COMB-HKNID          NOT =   ZERO )
                   PERFORM 20011-HOKEN-EDIT-SEC
               END-IF
      *
               PERFORM VARYING IDXK    FROM    1   BY  1
                       UNTIL ( IDXK    >   4 )
                        OR   ( COMB-KOHID (IDXK)   =   ZERO )
                   PERFORM 20011-KOHI-EDIT-SEC
               END-PERFORM
           END-PERFORM
      *
          .
       2001-HOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       20011-HOKEN-EDIT-SEC           SECTION.
      *
           PERFORM 900-PTHKNINF-KEY-SEL-SEC
      *
      *    保険の種類
           MOVE    COMB-HKNNUM     TO  ICINFRES-HKNNUM   (IDXA)
      *    保険者番号
           MOVE    PTHKN-HKNJANUM  TO  ICINFRES-HKNJANUM (IDXA)
      *    保険の名称
           MOVE    COMB-SYU-TANSEIDONAME
                                   TO  ICINFRES-HKNJANAME(IDXA)
      *    記号
           MOVE    PTHKN-KIGO      TO  ICINFRES-KIGO (IDXA)
      *    番号
           MOVE    PTHKN-NUM       TO  ICINFRES-NUM  (IDXA)
      *
          .
       20011-HOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20011-KOHI-EDIT-SEC             SECTION.
      *
           PERFORM 900-PTKOHINF-KEY-SEL-SEC
      *
      *    公費の種類
           MOVE    COMB-KOHHKNNUM (IDXK)
                                   TO  ICINFRES-KOHNUM  (IDXA IDXK)
      *    公費の種類名称
           MOVE    COMB-KOH-TANSEIDONAME (IDXK)
                                   TO  ICINFRES-KOHNAME (IDXA IDXK)
      *    負担者番号
           MOVE    PTKOH-FTNJANUM  TO  ICINFRES-FTNJANUM(IDXA IDXK)
      *    受給者番号
           MOVE    PTKOH-JKYSNUM   TO  ICINFRES-JKYSNUM (IDXA IDXK)
      *
          .
       20011-KOHI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    未収収情報取得処理
      *****************************************************************
       2001-MISYU-GET-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDXA
      *
           PERFORM 900-SYUNOU-KEY87-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU NOT =   ZERO )
      *
               COMPUTE IDXA    =   IDXA   +   1
      *
               COMPUTE WRK-KINGAKU
                   =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
               COMPUTE WRK-MISYU-TOTAL
                   =   WRK-MISYU-TOTAL +   WRK-KINGAKU
      *
               IF   (  CONST-MISYU-MAX >=  IDXA )
                   MOVE    SYU-SRYYMD
                                   TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE
                                   TO  ICINFRES-MISYU-SRYYMD (IDXA)
                   MOVE    SYU-NYUGAIKBN
                                   TO  ICINFRES-MISYU-NYUGAIKBN (IDXA)
                   MOVE    SYU-DENPNUM
                                   TO  ICINFRES-MISYU-DENPNUM (IDXA)
                   MOVE    WRK-KINGAKU
                                   TO  WRK-KINGAKUM9
                   MOVE    WRK-KINGAKUM9
                                   TO  ICINFRES-MISYU (IDXA)
               END-IF
      *
               PERFORM 900-SYUNOU-KEY87-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key87"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXA            >   ZERO )
      *
               MOVE    WRK-MISYU-TOTAL
                                   TO  WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9
                                   TO  ICINFRES-MISYU-TOTAL
      *
               IF    ( CONST-MISYU-MAX >=  IDXA )
                   MOVE    "false" TO  ICINFRES-MISYU-OVER
               ELSE
                   MOVE    "true"  TO  ICINFRES-MISYU-OVER
               END-IF
           END-IF
      *
           .
       2001-MISYU-GET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  ICINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  ICINFRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "0000"          TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  ICINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  ICINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           PERFORM 980-FILE-DELETE-SEC
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
           WHEN    "0000"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "0001"
               MOVE    "診療日の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0002"
               MOVE    "診療月の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0003"
               MOVE    "診療年の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0004"
               MOVE    "患者番号の設定に誤りがあります"
                                       TO  WRK-ERRMSG
      *共通エラー
           WHEN    "0089"
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
           WHEN    "0097"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0098"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "0099"
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
           IF    ( APILST19-BODY    NOT =   LOW-VALUE )
               DISPLAY "incomeinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_incomeinfv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST19-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"               TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  APILST19-BODY
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
                                       TO  ICINFREQ-PRIVATE-OBJECTS
      *
           IF    ( ICINFREQ-PTNUM      =   LOW-VALUE )
               MOVE    SPACE           TO  ICINFREQ-PTNUM
           END-IF
      *
           IF    ( ICINFREQ-SRYYMD     =   LOW-VALUE )
               MOVE    SPACE           TO  ICINFREQ-SRYYMD
           END-IF
      *
           IF    ( ICINFREQ-SRYYM      =   LOW-VALUE )
               MOVE    SPACE           TO  ICINFREQ-SRYYM
           END-IF
      *
           IF    ( ICINFREQ-SRYYEAR    =   LOW-VALUE )
               MOVE    SPACE           TO  ICINFREQ-SRYYEAR
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
           IF    ( APILST19-BODY    NOT =   LOW-VALUE )
               DISPLAY "incomeinfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    ICINFRES-PRIVATE-OBJECTS
                                           TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_incomeinfv2res"    TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST19-BODY
               MOVE    "application/xml"   TO  APILST19-CONTENT-TYPE
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
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY85-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
      *
           EVALUATE    KEY-RANGE
           WHEN    "Y"
               MOVE    KEY-SRYYMD(1:4) TO  SYU-SRYYMD
               MOVE    "%"             TO  SYU-SRYYMD(5:1)
           WHEN    "M"
               MOVE    KEY-SRYYMD(1:6) TO  SYU-SRYYMD
               MOVE    "%"             TO  SYU-SRYYMD(7:1)
           WHEN    OTHER
               MOVE    KEY-SRYYMD      TO  SYU-SRYYMD
           END-EVALUATE
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key85"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY85-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納FETCH処理
      *****************************************************************
       900-SYUNOU-KEY85-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key85"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY85-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY87-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key87"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY87-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納FETCH処理
      *****************************************************************
       900-SYUNOU-KEY87-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key87"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY87-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
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
      *
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTHKNINF
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID        TO  PTHKN-PTID
           MOVE    COMB-HKNID      TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
                MOVE    MCPDATA-REC TO PTHKNINF-REC
           ELSE
                INITIALIZE             PTHKNINF-REC
                MOVE    1          TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTKOHINF
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID        TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDXK)
                                   TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
                MOVE    MCPDATA-REC TO PTKOHINF-REC
           ELSE
                INITIALIZE             PTKOHINF-REC
                MOVE    1          TO  FLG-PTKOHINF
           END-IF
     *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納ファイル読込処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-IN01
      *
           READ    IN01-FILE
           AT  END
               MOVE    1               TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-EXT.
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
      *    ファイル削除処理
      *****************************************************************
       980-FILE-DELETE-SEC             SECTION.
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ASS-IN01        TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       980-FILE-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "0099" )
               MOVE    404             TO  APILST19-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 980-FILE-DELETE-SEC
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
           MOVE    WRK-DATE            TO  ICINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  ICINFRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  ICINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  ICINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
