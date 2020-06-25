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
       PROGRAM-ID.             ORAPI001R6V2S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : カルテ３号紙（外来）印刷
      *  管理者            :
      *  作成日付    作業者      記述
      *  18/07/30    NACL−太田　新規作成
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXH                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
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
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
      *
       01  API-AREA.
           03  API-TABLE               PIC X(32).
           03  API-NYUGAIKBN           PIC X(01).
           03  API-SRYYMD              PIC X(08).
           03  API-SRYDD               PIC 9(02).
           03  API-ENDPAGE             PIC 9(03).
           03  API-ENDLINE             PIC 9(02).
           03  API-HKNCOMBINUM-G.
               05  API-HKNCOMBINUM     PIC 9(04).
      *
       01  TPTINF-AREA.
           COPY    "CPPTINF.INC"       REPLACING  //PTINF-//
                                       BY         //TPTINF-//.
      *
       01  CONST-AREA.
           03  CONST-TRUE              PIC X(05)   VALUE "True".
           03  CONST-FALSE             PIC X(05)   VALUE "False".
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得
           COPY    "CPORCSPRTNM.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    カルテ３号紙（外来）
           COPY    "CPORCHC33.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
           COPY    "CPORCSHKNSET.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPKARTE3V2REQ.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    API用帳票データレスポンス作成
           COPY    "CPORCSAPIFM.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK1030.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPDATA.INC".
      *
       01  APILST00-AREA.
           COPY    "CPAPILSTV2.INC"    REPLACING
                                     //APILST-// BY //APILST00-//.
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY     "API01RV2SCRAREA.INC".
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
           DISPLAY   "*******************"
           DISPLAY   "* karte_no3 start *"
           DISPLAY   "*******************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  API-AREA
           INITIALIZE                  TPTINF-AREA
           INITIALIZE                  KARTE3REQ-AREA
           INITIALIZE                  APILST00-AREA
      *
           MOVE    SPA-COMMON          TO  API-NYUGAIKBN
           MOVE    SPA-FREE            TO  APILST00-REC
      *
           IF    ( API-NYUGAIKBN       =   "1" )
               MOVE    "1"             TO  API-NYUGAIKBN
               MOVE    "xml_karte_no3_hv2req"
                                       TO  API-TABLE
           ELSE
               MOVE    "2"             TO  API-NYUGAIKBN
               MOVE    "xml_karte_no3v2req"
                                       TO  API-TABLE
           END-IF
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "*****************"
           DISPLAY   "* karte_no3 end *"
           DISPLAY   "*****************"
      *
           MOVE    APILST00-REC          TO  SPA-FREE
      *
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
               MOVE   "0099"       TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        ＳＰＡ共通設定
               INITIALIZE              SYS-1010-REC
               INITIALIZE              ORCSCOMMONAREA
               MOVE    "M00"       TO  ORCSCOMMON-PG
               CALL    "ORCSCOMMON"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
      *        IF    ( SPA-ERRCD   NOT =   SPACE )
      *            MOVE   "0089"   TO  SPA-ERRCD
      *        END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        XML情報取り出し
               PERFORM 900-XML-READ-SEC
               MOVE    LOW-VALUE   TO  APILST00-BODY
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-REQUEST-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト項目処理
      *****************************************************************
       2001-REQUEST-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-REQNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-PRINT-MODE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-HAKKOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-PTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-SRYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-HKNCOMBINUM-CHK-SEC
           END-IF
      *
           IF    ( KARTE3REQ-HAKKOKBN  =   "1" )
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 2001-SRYDD-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 2001-LAST-PAGE-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 2001-LAST-ROW-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001-REQUEST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト番号チェック処理
      *****************************************************************
       2001-REQNUM-CHK-SEC             SECTION.
      *
           IF    ( KARTE3REQ-REQNUM    =   "01" OR "02" )
               CONTINUE
           ELSE
               MOVE    "0101"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-REQNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷モードチェック処理
      *****************************************************************
       2001-PRINT-MODE-CHK-SEC         SECTION.
      *
           EVALUATE    KARTE3REQ-REQNUM
           WHEN    "01"
               MOVE    FUNCTION UPPER-CASE(KARTE3REQ-PRINT-MODE)
                                   TO  KARTE3REQ-PRINT-MODE
               EVALUATE    KARTE3REQ-PRINT-MODE
               WHEN    SPACE
                   CONTINUE
               WHEN    "PDF"
                   INITIALIZE                  SRPLNK-AREA
                   MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
                   CALL    "ORCSRPLNK"         USING
                                               SRPLNK-AREA
                                               SPA-AREA
                   IF    ( SRPLNK-RC       NOT =   ZERO )
                       MOVE    "0128"      TO  SPA-ERRCD
                   END-IF
               WHEN    OTHER
                   MOVE    "0127"      TO  SPA-ERRCD
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2001-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    発行区分チェック処理
      *****************************************************************
       2001-HAKKOKBN-CHK-SEC           SECTION.
      *
           IF    ( KARTE3REQ-HAKKOKBN  =   SPACE )
               MOVE    "1"             TO  KARTE3REQ-HAKKOKBN
           END-IF
      *
           IF    ( KARTE3REQ-HAKKOKBN  =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    "0107"          TO  SPA-ERRCD
           END-IF
      *
           .
       2001-HAKKOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       2001-PTNUM-CHK-SEC               SECTION.
      *
           IF    ( KARTE3REQ-PTNUM     =   SPACE )
               MOVE    "0102"      TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    KARTE3REQ-PTNUM     TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
      *
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       =   ZERO )
                   MOVE    PTINF-REC   TO  TPTINF-AREA
               ELSE
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       2001-SRYYM-CHK-SEC              SECTION.
      *
           IF    ( KARTE3REQ-SRYYM    =   SPACE )
               MOVE    "0114"          TO  SPA-ERRCD
           ELSE
               MOVE    KARTE3REQ-SRYYM
                                       TO  WRK-DATE
               MOVE    "-01"           TO  WRK-DATE(8:)
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0115"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-SRYYMD
               END-IF
           END-IF
      *
           .
       2001-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷開始日チェック処理
      *****************************************************************
       2001-SRYDD-CHK-SEC              SECTION.
      *
           IF    ( KARTE3REQ-SRYDD     =   SPACE )
               MOVE    1               TO  API-SRYDD
           ELSE
               MOVE    API-SRYYMD      TO  WRK-SYMD
               MOVE    FUNCTION NUMVAL (KARTE3REQ-SRYDD)
                                       TO  WRK-SDD
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0116"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SDD     TO  API-SRYDD
               END-IF
           END-IF
      *
           .
       2001-SRYDD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       2001-SRYKA-CHK-SEC              SECTION.
      *
           IF    ( KARTE3REQ-SRYKA     =   SPACE OR ZERO )
               MOVE    ZERO            TO  KARTE3REQ-SRYKA
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1005"          TO  SYS-KANRICD
               MOVE    KARTE3REQ-SRYKA TO  SYS-KBNCD
               MOVE    API-SRYYMD       TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC TO SYS-1005-REC
                   MOVE    KARTE3REQ-SRYKA
                                       TO  SPA-SRYKA
                   MOVE    SYS-1005-SRYKANAME1
                                       TO  SPA-SRYKAMEI
               ELSE
                   MOVE    "0108"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       2001-HKNCOMBINUM-CHK-SEC        SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( FUNCTION TRIM(KARTE3REQ-HKNCOMBINUM) IS NUMERIC  )
            AND  ( FUNCTION NUMVAL(KARTE3REQ-HKNCOMBINUM)
                                           =  ZERO )
           WHEN  ( KARTE3REQ-HKNCOMBINUM   =   SPACE )
               MOVE    ZERO            TO  API-HKNCOMBINUM
           WHEN    OTHER
      *
               IF    ( FUNCTION TRIM(KARTE3REQ-HKNCOMBINUM)
                                           IS NOT NUMERIC  )
                   MOVE    "0109"      TO  SPA-ERRCD
               ELSE
                   MOVE    FUNCTION NUMVAL(KARTE3REQ-HKNCOMBINUM)
                                       TO  API-HKNCOMBINUM
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   MOVE    TPTINF-BIRTHDAY     TO  SPA-BIRTHDAY
                   MOVE    API-SRYYMD          TO  SPA-SRYYMD(1:6)
                   MOVE    "00"                TO  SPA-SRYYMD(7:2)
                   MOVE    API-NYUGAIKBN       TO  SPA-NYUGAIKBN
      *
                   INITIALIZE                  ORCSHKNSETAREA
                   MOVE    "1"                 TO  ORCSHKN-KBN
                   MOVE    "1"                 TO  ORCSHKN-KBN2
                   CALL    "ORCSHKNSET"        USING
                                               SPA-AREA
                                               ORCSHKNSETAREA
      *
                   MOVE    ZERO                TO  IDXH
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   ORCSHKN-HKN-MAX )
                            OR   ( IDXH    >   ZERO )
      *
                       IF    ( API-HKNCOMBINUM-G
                                   =   ORCSHKN-GMN-HKNCOMBINUM (IDX0))
                           MOVE    IDX0    TO  IDXH
                       END-IF
                   END-PERFORM
      *
                   IF    ( IDXH            =   ZERO )
                       MOVE    "0109"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           END-EVALUATE
      *
           .
       2001-HKNCOMBINUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    最終印刷ページチェック処理
      *****************************************************************
       2001-LAST-PAGE-CHK-SEC          SECTION.
      *
           IF    ( KARTE3REQ-LAST-PAGE =   SPACE )
               MOVE    ZERO            TO  API-ENDPAGE
           ELSE
               IF    ( FUNCTION TRIM (KARTE3REQ-LAST-PAGE)
                                       IS  NOT NUMERIC )
                   MOVE    "0117"      TO  SPA-ERRCD
               ELSE
                   MOVE FUNCTION NUMVAL (KARTE3REQ-LAST-PAGE)
                                       TO  API-ENDPAGE
      *
               END-IF
           END-IF
      *
           .
       2001-LAST-PAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    最終印刷行チェック処理
      *****************************************************************
       2001-LAST-ROW-CHK-SEC           SECTION.
      *
           IF    ( KARTE3REQ-LAST-ROW =   SPACE )
               MOVE    ZERO            TO  API-ENDLINE
           ELSE
               IF    ( FUNCTION TRIM (KARTE3REQ-LAST-ROW)
                                       IS  NOT NUMERIC )
                   MOVE    "0118"      TO  SPA-ERRCD
               ELSE
                   MOVE FUNCTION NUMVAL (KARTE3REQ-LAST-ROW)
                                       TO  API-ENDLINE
      *
                   IF    ( API-ENDLINE >=  40 )
                       MOVE    "0119"  TO  SPA-ERRCD
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       2001-LAST-ROW-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷・データ取得処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           PERFORM 20011-PRE-PRINT-SEC
      *
      *    カルテ印刷
           INITIALIZE                      ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-PTNUM           TO  ORCHC33-PTNUM
           MOVE    SPA-PTID            TO  ORCHC33-PTID
           MOVE    TPTINF-NAME         TO  ORCHC33-NAME
           MOVE    TPTINF-BIRTHDAY     TO  ORCHC33-BIRTHDAY
           MOVE    TPTINF-SEX          TO  ORCHC33-SEX
           MOVE    API-SRYYMD(1:6)     TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    API-NYUGAIKBN       TO  ORCHC33-NYUGAIKBN
           MOVE    KARTE3REQ-SRYKA     TO  ORCHC33-SRYKA
           MOVE    API-HKNCOMBINUM     TO  ORCHC33-HKNCOMBI
      *
           IF    ( KARTE3REQ-HAKKOKBN  =   "1" )
               MOVE    ZERO            TO  ORCHC33-PRINT-FLG
               MOVE    API-SRYDD       TO  ORCHC33-STRDD
               MOVE    API-ENDPAGE     TO  ORCHC33-ENDPAGE
               MOVE    API-ENDLINE     TO  ORCHC33-ENDLINE
           ELSE
               MOVE    1               TO  ORCHC33-PRINT-FLG
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1030"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    API-SRYYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO SYS-1030-REC
           IF    ( SYS-1030-KARUTE3KBN =   SPACE )
               MOVE    ZERO        TO  ORCHC33-KARUTE3KBN
           ELSE
               MOVE    SYS-1030-KARUTE3KBN
                                   TO  ORCHC33-KARUTE3KBN
           END-IF
      *
      *    クライアント印刷
           INITIALIZE              SRPLNK-AREA
           INITIALIZE              ORCSPRTNMAREA
           MOVE    "01"            TO  SPA-PRT-FLG
           MOVE    "INS"           TO  SRPLNK-MODE
           IF    ( API-NYUGAIKBN   =   "1" )
               MOVE    RPKEY-KARTE-NO3-N
                                   TO  SRPLNK-VAL
           ELSE
               MOVE    RPKEY-KARTE-NO3
                                   TO  SRPLNK-VAL
           END-IF
           CALL    "ORCSRPLNK"         USING
                                       SRPLNK-AREA
                                       SPA-AREA
      *
           IF    ( API-NYUGAIKBN   =   "1" )
               MOVE    "00000004"  TO  ORCSPRTNM-KBNCD
           ELSE
               MOVE    "00000005"  TO  ORCSPRTNM-KBNCD
           END-IF
           MOVE    API-NYUGAIKBN   TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"             TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD      TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
      *
           CALL    ORCSPRTNM-PRTPG     USING
                                       SPA-AREA
                                       ORCHC33AREA
           MOVE    SPACE           TO  SPA-PRT-FLG
      *
           EVALUATE    ORCHC33-RC
           WHEN    ZERO
               CONTINUE
           WHEN    8
               MOVE    "0120"      TO  SPA-ERRCD
           WHEN    OTHER
               MOVE    "0121"      TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷・データ取得処理
      *****************************************************************
       20011-PRE-PRINT-SEC             SECTION.
      *
           IF    ( KARTE3REQ-REQNUM    =   "01" )
               MOVE    2           TO  SPA-API-FLG
               IF    ( KARTE3REQ-PRINT-MODE   =   "PDF" )
                   MOVE    "3"     TO  SPA-PRTCONF
               END-IF
           ELSE
               MOVE    3           TO  SPA-API-FLG
           END-IF
      *
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
           .
       20011-PRE-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD    =  "0099" )
               MOVE    404             TO  APILST00-HTTPSTATUS
           ELSE
               INITIALIZE                  SAPIFM-AREA
               MOVE    SPA-ERRCD       TO  SAPIFM-ERRCD
               CALL    "ORCSAPIFM"         USING
                                           SAPIFM-AREA
                                           SPA-AREA
               IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
                   MOVE    SAPIFM-OBJECT
                                       TO  APILST00-BODY
                   MOVE    "application/json"
                                       TO  APILST00-CONTENT-TYPE
               END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
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
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
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
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
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
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST00-BODY    NOT =   LOW-VALUE )
               DISPLAY "karte_no3 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    API-TABLE               TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST00-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           IF    ( API-NYUGAIKBN           =   "1" )
               MOVE    "karte_no3_hv2req"  TO  APIREQ-RECORDNAME
           ELSE
               MOVE    "karte_no3v2req"    TO  APIREQ-RECORDNAME
           END-IF
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST00-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ   TO  KARTE3REQ-DATA-AREA
      *
           .
       9001-XML-FILTER-EXT.
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
