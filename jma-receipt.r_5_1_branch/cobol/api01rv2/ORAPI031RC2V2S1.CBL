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
       PROGRAM-ID.         ORAPI031RC2V2S1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院病棟情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/09/03    NACL−太田　新規作成
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TAISHO              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-BYOTO               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXW                    PIC 9(05).
           03  IDXR                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-TNS-SRYCD           PIC X(09).
           03  WRK-TNS-SRYYMD          PIC X(08).
           03  WRK-KJNYMD              PIC X(08).
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
           03  WRK-NUMZ                PIC ZZZ9.
           03  WRK-SRYCD               PIC X(09).
      *
       01  KEY-AREA.
           03  KEY-BTUNUM              PIC X(02).
           03  KEY-BRMNUM              PIC X(06).
      *
       01  CONST-AREA.
           03  CONST-H260401           PIC X(08)   VALUE "20140401".
           03  CONST-H240401           PIC X(08)   VALUE "20120401".
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
           03  CONST-BYOTO-MAX         PIC 9(03)   VALUE 50.
           03  CONST-BYOSHITSU-MAX     PIC 9(03)   VALUE 200.
           03  CONST-NYUINKSN-MAX      PIC 9(03)   VALUE 20.
           03  CONST-USERKSN-MAX       PIC 9(03)   VALUE 10.
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
      *    病室番号編集サブ
           COPY    "CPORCSBRMNUM.INC".
      *
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *
      *    API XML書き込み用定義体
           COPY    "CPHSCONFWARDV2RES.INC".
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
      *    入院基本情報
           COPY    "CPSK5000.INC".
      *    病棟情報
           COPY    "CPSK5001.INC".
      *    病室情報
           COPY    "CPSK5002.INC".
      *    特定入院料
           COPY    "CPSK5110.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
      *    API XML読み込み用定義体
           COPY    "CPHSCONFWARDV2REQ.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA
           XML-HSCONFWARDV2REQ.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
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
           IF    ( WRK-ERRCD          =   "99" )
      *        ユーザＩＤエラー
               MOVE   404             TO  APILST12-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-END-SEC
           END-IF
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
           INITIALIZE                  XML-HSCONFWARDRES
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
           END-IF
      *
           IF    ( WRK-ERRCD          =   SPACE )
      *        ＳＰＡ共通設定
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
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 1001-INIT-CHECK-SEC
           END-IF
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
           MOVE    SPACE           TO  WRK-KJNYMD
      *
           MOVE    HSCWREQ-BASE-DATE
                                   TO  WRK-DATE
           PERFORM 800-SYMD-SEC
           IF    ( WRK-SYMD        =   SPACE )
               MOVE    SPA-SYSYMD  TO  WRK-SYMD
           END-IF
           PERFORM 800-HIZUKE-SEC
           IF    ( WRK-EDTYMD1     =   SPACE )
               MOVE    "01"        TO  WRK-ERRCD
           ELSE
               MOVE    WRK-SYMD    TO  WRK-KJNYMD
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
               IF    ( WRK-KJNYMD  <   CONST-H200401 )
                   MOVE    "92"    TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
      *
               MOVE    HSCWREQ-BTUNUM      TO  KEY-BTUNUM
      *
               IF    ( HSCWREQ-BRMNUM  NOT =   SPACE )
                   INITIALIZE                  ORCSBRMNUMAREA
                   MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
                   MOVE    HSCWREQ-BRMNUM
                                           TO  SBRMNUM-IN-BRMNUM
                   CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
                   MOVE    SBRMNUM-OT-BRMNUM
                                           TO  KEY-BRMNUM
               END-IF
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
           IF    ( FLG-SYSKANRI    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "02"        TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           MOVE    ZERO            TO  IDXW
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXW        >=   CONST-BYOTO-MAX )
                    OR   ( WRK-ERRCD       NOT =   SPACE )
      *
               PERFORM 2001-TAISHO-BYOTO-CHK-SEC
      *
               IF    ( FLG-TAISHO  =   1 )
                   PERFORM 2001-BYOTO-EDIT-SEC
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDXW                TO  CNT-BYOTO
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    対象病棟チェック処理
      *****************************************************************
       2001-TAISHO-BYOTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TAISHO
      *
           IF    ( KEY-BTUNUM          =   SPACE )
            AND  ( KEY-BRMNUM          =   SPACE )
               MOVE    1               TO  FLG-TAISHO
           ELSE
               IF    ( KEY-BTUNUM  NOT =   SPACE )
                   IF    ( KEY-BTUNUM  =   SYS-5001-BTU-NUM )
                       MOVE    1       TO  FLG-TAISHO
                   END-IF
               ELSE
                   IF    ( KEY-BRMNUM  NOT =   SPACE )
                       INITIALIZE                  SYSKANRI-REC
                       MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                       MOVE    "5002"          TO  SYS-KANRICD
                       MOVE    SYS-5001-BTU-NUM
                                               TO  SYS-KBNCD
                       MOVE    KEY-BRMNUM      TO  SYS-KBNCD(3:)
                       MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                                   SYS-EDYUKYMD
                       PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                       IF    ( FLG-SYSKANRI    =   ZERO )
                           MOVE    1           TO  FLG-TAISHO
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-TAISHO-BYOTO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理
      *****************************************************************
       2001-BYOTO-EDIT-SEC             SECTION.
      *
           COMPUTE IDXW    =   IDXW    +   1
      *
           IF    ( SYS-5000-SENTEIKBN      =   SPACE )
               MOVE    "1"             TO  SYS-5000-SENTEIKBN
               MOVE    "1"             TO  SYS-5000-SENTEI-FTNKBN
               MOVE   10               TO  SYS-5000-SENTEI-TANKA
               MOVE   "0"              TO  SYS-5000-SENTEI-TAXKBN
           END-IF
      *
           IF    ( SYS-5001-SENTEI-FTNKBN  =   SPACE )
               MOVE    "1"             TO  SYS-5001-SENTEI-FTNKBN
               MOVE   10               TO  SYS-5001-SENTEI-TANKA
               MOVE   "0"              TO  SYS-5001-SENTEI-TAXKBN
           END-IF
      *
           IF    ( SYS-5001-BTU-KHNSRYCD   NOT =   SPACE )
               EVALUATE    SYS-5001-BTU-KHNSRYCD
               WHEN    "099999950"
                   MOVE    "190097010" TO  SYS-5001-BTU-KHNSRYCD
               WHEN    "099999951"
                   MOVE    "190097210" TO  SYS-5001-BTU-KHNSRYCD
               WHEN    "099999952"
                   MOVE    "190135110" TO  SYS-5001-BTU-KHNSRYCD
               WHEN    "099999955"
                   MOVE    "190169610" TO  SYS-5001-BTU-KHNSRYCD
               WHEN    "099999956"
                   MOVE    "190169910" TO  SYS-5001-BTU-KHNSRYCD
               WHEN    "099999957"
                   MOVE    "190170210" TO  SYS-5001-BTU-KHNSRYCD
               END-EVALUATE
           END-IF
      *
           IF    ( SYS-5001-YKNCHOKAKBN    =   SPACE )
               MOVE    "0"             TO  SYS-5001-YKNCHOKAKBN
           END-IF
      *
           IF    ( SYS-5000-SENTEI-FTNKBN  =   "1" OR "2" )
               MOVE    SYS-5000-SENTEI-FTNKBN
                                       TO  SYS-5001-SENTEI-FTNKBN
               MOVE    SYS-5000-SENTEI-TAXKBN
                                       TO  SYS-5001-SENTEI-TAXKBN
               MOVE    SYS-5000-SENTEI-TANKA-X
                                       TO  SYS-5001-SENTEI-TANKA-X
               MOVE    SYS-5000-SENTEI-KINGAKU-X
                                       TO  SYS-5001-SENTEI-KINGAKU-X
               MOVE    SYS-5000-SENTEI-IPN-KINGAKU-X
                                       TO  SYS-5001-SENTEI-IPN-KINGAKU-X
               MOVE    SYS-5000-SENTEI-RJN-KINGAKU-X
                                       TO  SYS-5001-SENTEI-RJN-KINGAKU-X
           END-IF
      *
           MOVE    SYS-5001-STYUKYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSCWRES-ST-DATE (IDXW)
      *
           MOVE    SYS-5001-EDYUKYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSCWRES-ED-DATE (IDXW)
      *
           MOVE    "病棟番号"          TO  HSCWRES-BTUNUM-LBL (IDXW)
           MOVE    SYS-5001-BTU-NUM    TO  HSCWRES-BTUNUM-DATA (IDXW)
      *
           MOVE    "病棟名称"          TO  HSCWRES-BTUNAME-LBL (IDXW)
           MOVE    SYS-5001-BTU-NAME   TO  HSCWRES-BTUNAME-DATA (IDXW)
      *
           MOVE    "短縮病棟名"        TO  HSCWRES-BTUNAME2-LBL (IDXW)
           MOVE    SYS-5001-BTU-TANNAME
                                       TO  HSCWRES-BTUNAME2-DATA (IDXW)
      *
           MOVE    "病棟種別"          TO  HSCWRES-BTUSBT-LBL (IDXW)
           MOVE    SYS-5001-BTU-SBT    TO  HSCWRES-BTUSBT-DATA (IDXW)
           EVALUATE    SYS-5001-BTU-SBT
           WHEN    "01"
               MOVE    "一般病棟"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "02"
               MOVE    "精神病棟"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "03"
               MOVE    "結核病棟"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "04"
               MOVE    "療養病棟"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "05"
               MOVE    "障害者施設等"  TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "06"
               MOVE    "専門病院"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "07"
               MOVE    "老人病棟"      TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "08"
               MOVE    "有床診療所"    TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "09"
               MOVE    "有床診療所療養病床"
                                       TO  HSCWRES-BTUSBT-NAME (IDXW)
           WHEN    "10"
               MOVE    "医療観察病棟"
                                       TO  HSCWRES-BTUSBT-NAME (IDXW)
           END-EVALUATE
      *
           IF    ( SYS-5001-BTU-KA     NOT =   SPACE )
               MOVE    "診療科"        TO  HSCWRES-SRYKA-LBL (IDXW)
               MOVE    SYS-5001-BTU-KA TO  HSCWRES-SRYKA-DATA (IDXW)
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1005"          TO  SYS-KANRICD
               MOVE    SYS-5001-BTU-KA TO  SYS-KBNCD
               MOVE    SYS-5001-STYUKYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                       TO  HSCWRES-SRYKA-NAME (IDXW)
           END-IF
      *
           IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                       NOT =   SPACE )
               MOVE    "特定入院料"    TO  HSCWRES-TOKNYUIN-LBL (IDXW)
      *
               INITIALIZE                  ORCSTOKNYUIN-AREA
               MOVE    "2"             TO  LNK-TOKNYUIN-KBN
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  LNK-TOKNYUIN-NYUIN
               MOVE    SYS-5001-STYUKYMD (1:6)
                                       TO  LNK-TOKNYUIN-SANTEIYM
               MOVE    SYS-5001-BTU-NUM
                                       TO  LNK-TOKNYUIN-BTUNUM
               CALL    "ORCSTOKNYUIN"  USING
                                       ORCSTOKNYUIN-AREA
                                       SPA-AREA
               MOVE    LNK-TOKNYUIN-SRYCD
                                       TO  HSCWRES-TOKNYUIN-DATA (IDXW)
               MOVE    LNK-TOKNYUIN-SRYCD
                                       TO  WRK-TNS-SRYCD
               MOVE    SYS-5001-STYUKYMD
                                       TO  WRK-TNS-SRYYMD
               PERFORM 901-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME        TO  HSCWRES-TOKNYUIN-NAME (IDXW)
           END-IF
      *
           MOVE    SPACE               TO  WRK-SRYCD
           EVALUATE    TRUE
           WHEN    ( SYS-5001-BTU-KHNSRYCD
                                       NOT =   SPACE )
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  WRK-SRYCD
           WHEN    ( SYS-5001-KHNSRYCD NOT =   SPACE )
               MOVE    SYS-5001-KHNSRYCD
                                       TO  WRK-SRYCD
           END-EVALUATE
      *
           IF    ( WRK-SRYCD       NOT =   SPACE )
               MOVE    "入院基本料"    TO  HSCWRES-NYUINKHN-LBL (IDXW)
               MOVE    WRK-SRYCD       TO  HSCWRES-NYUINKHN-DATA(IDXW)
      *
               MOVE    WRK-SRYCD       TO  WRK-TNS-SRYCD
               MOVE    SYS-5001-STYUKYMD
                                       TO  WRK-TNS-SRYYMD
               PERFORM 901-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME        TO  HSCWRES-NYUINKHN-NAME (IDXW)
      *
               IF    ( WRK-KJNYMD      >=  CONST-H260401 )
                   IF    ( TNS-CDKBN-KBNNUM   =   100 )
                    OR   ( TNS-CDKBN-KBNNUM   =   101
                       AND TNS-CDKBN-KOUBAN   =   2 )
                    OR   ( TNS-CDKBN-KBNNUM   =   102 )
                    OR   ( TNS-CDKBN-KBNNUM   =   103 )
                    OR   ( TNS-CDKBN-KBNNUM   =   106 )
                    OR   ( TNS-SRYCD          =   "099999954" )
                       MOVE    "月平均夜勤時間超過"
                                       TO  HSCWRES-YKNCHOKA-LBL (IDXW)
                       MOVE    SYS-5001-YKNCHOKAKBN
                                       TO  HSCWRES-YKNCHOKA-DATA(IDXW)
                       EVALUATE    SYS-5001-YKNCHOKAKBN
                       WHEN    "0"
                           MOVE    "超過なし"
                                       TO  HSCWRES-YKNCHOKA-NAME(IDXW)
                       WHEN    "1"
                           MOVE    "超過あり"
                                       TO  HSCWRES-YKNCHOKA-NAME(IDXW)
                       END-EVALUATE
                   END-IF
               END-IF
      *
           END-IF
      *
           PERFORM 20011-NYUIN-KSN-EDIT-SEC
      *
           PERFORM 20012-USER-KSN-EDIT-SEC
      *
           IF    ( SYS-5000-SENTEIKBN  =   "1" )
               PERFORM 20013-SENTEI-EDIT-SEC
           END-IF
      *
           PERFORM 20014-BRMNUM-EDIT-SEC
      *
          .
       2001-BYOTO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算編集処理
      *****************************************************************
       20011-NYUIN-KSN-EDIT-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDXK
      *
           MOVE    SYS-5001-STYUKYMD   TO  WRK-TNS-SRYYMD
      *
           MOVE    SPACE               TO  WRK-SRYCD
           EVALUATE     SYS-5001-BTU-KNGHJOKSN
           WHEN    "06"
      *        看護補助加算１
               MOVE  "190103770"       TO  WRK-SRYCD
           WHEN     "07"
      *        看護補助加算１（１３対１入院基本料）
               MOVE  "190103870"       TO  WRK-SRYCD
           WHEN     "08"
      *        看護補助加算２
               MOVE  "190103970"       TO  WRK-SRYCD
           WHEN     "09"
      *        看護補助加算３
               MOVE  "190104070"       TO  WRK-SRYCD
           END-EVALUATE
      *
           IF    ( WRK-SRYCD       NOT =   SPACE )
               MOVE    WRK-SRYCD   TO  WRK-TNS-SRYCD
               PERFORM 200111-NYUIN-KSN-EDIT-SEC
           END-IF
      *
           IF    ( SYS-5001-BTU-SYOKUDOKBN =   "2" )
               MOVE    "197000570"     TO  WRK-TNS-SRYCD
               PERFORM 200111-NYUIN-KSN-EDIT-SEC
           END-IF
      *
           IF    ( SYS-5001-BTU-HIKSISUKBN =   "2" )
               MOVE    "190111370"     TO  WRK-TNS-SRYCD
               PERFORM 200111-NYUIN-KSN-EDIT-SEC
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   14 )
               IF    ( SYS-5001-KSNKBN (IDX0)  =   "2" )
                   MOVE    SYS-5001-KSNCD (IDX0)
                                       TO  WRK-TNS-SRYCD
                   PERFORM 200111-NYUIN-KSN-EDIT-SEC
               END-IF
           END-PERFORM
      *
          .
       20011-NYUIN-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算編集処理
      *****************************************************************
       200111-NYUIN-KSN-EDIT-SEC        SECTION.
      *
           IF    ( WRK-TNS-SRYCD   NOT =   SPACE )
             AND ( IDXK                <   CONST-NYUINKSN-MAX )
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   IF    ( TNS-HOSPSRYKBN  =   0 )
                    OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
      *
                       COMPUTE IDXK  = IDXK  + 1
      *
                       MOVE    "入院加算"
                                   TO  HSCWRES-NYUINKSN-LBL  (IDXW IDXK)
                       MOVE    TNS-SRYCD
                                   TO  HSCWRES-NYUINKSN-DATA (IDXW IDXK)
                       MOVE    TNS-NAME
                                   TO  HSCWRES-NYUINKSN-NAME (IDXW IDXK)
                   END-IF
               END-IF
           END-IF
      *
          .
       200111-NYUIN-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算（ユーザ設定）編集処理
      *****************************************************************
       20012-USER-KSN-EDIT-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDXK
      *
           MOVE    SYS-5001-STYUKYMD   TO  WRK-TNS-SRYYMD
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
               IF    ( SYS-5001-KSNCD2 (IDX0)  NOT =   SPACE )
                   MOVE    SYS-5001-KSNCD2 (IDX0)
                                       TO  WRK-TNS-SRYCD
                   PERFORM 200121-USER-KSN-EDIT-SEC
               END-IF
           END-PERFORM
      *
          .
       20012-USER-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算（ユーザ設定）編集処理
      *****************************************************************
       200121-USER-KSN-EDIT-SEC        SECTION.
      *
           IF    ( WRK-TNS-SRYCD   NOT =   SPACE )
             AND ( IDXK                <   CONST-USERKSN-MAX )
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   IF    ( TNS-HOSPSRYKBN  =   0 )
                    OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
      *
                       COMPUTE IDXK  = IDXK  + 1
      *
                       MOVE    "入院加算（ユーザ設定）"
                                   TO  HSCWRES-USERKSN-LBL  (IDXW IDXK)
                       MOVE    TNS-SRYCD
                                   TO  HSCWRES-USERKSN-DATA (IDXW IDXK)
                       MOVE    TNS-NAME
                                   TO  HSCWRES-USERKSN-NAME (IDXW IDXK)
                   END-IF
               END-IF
           END-IF
      *
          .
       200121-USER-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    選定入院料編集処理
      *****************************************************************
       20013-SENTEI-EDIT-SEC           SECTION.
      *
           MOVE    "選定入院料"        TO  HSCWRES-SENTEI-LBL (IDXW)
      *
           MOVE    "消費税"        TO  HSCWRES-SENTEI-TAX-LBL (IDXW)
           MOVE    SYS-5001-SENTEI-TAXKBN
                                   TO  HSCWRES-SENTEI-TAX-DATA (IDXW)
           EVALUATE    SYS-5001-SENTEI-TAXKBN
           WHEN    "0"
               MOVE    "なし"      TO  HSCWRES-SENTEI-TAX-NAME (IDXW)
           WHEN    "1"
               MOVE    "あり"      TO  HSCWRES-SENTEI-TAX-NAME (IDXW)
           END-EVALUATE
      *
           MOVE    "負担計算"      TO  HSCWRES-SENTEI-FTN-LBL (IDXW)
           MOVE    SYS-5001-SENTEI-FTNKBN
                                   TO  HSCWRES-SENTEI-FTN-DATA (IDXW)
           EVALUATE    SYS-5001-SENTEI-FTNKBN
           WHEN    "1"
               MOVE    "定率設定"  TO  HSCWRES-SENTEI-FTN-NAME (IDXW)
      *
               MOVE    "一般点数単価"
                                   TO  HSCWRES-SENTEI-IPPAN-LBL (IDXW)
               MOVE    "後期高齢者点数単価"
                                   TO  HSCWRES-SENTEI-KOUKI-LBL (IDXW)
               MOVE    SYS-5001-SENTEI-TANKA
                                   TO  WRK-NUMZ
               MOVE    WRK-NUMZ    TO  HSCWRES-SENTEI-IPPAN-DATA (IDXW)
                                       HSCWRES-SENTEI-KOUKI-DATA (IDXW)
               MOVE    "点"        TO  HSCWRES-SENTEI-IPPAN-NAME (IDXW)
                                       HSCWRES-SENTEI-KOUKI-NAME (IDXW)
           WHEN    "2"
               MOVE    "定額設定"  TO  HSCWRES-SENTEI-FTN-NAME (IDXW)
      *
               MOVE    "一般１日金額"
                                   TO  HSCWRES-SENTEI-IPPAN-LBL (IDXW)
               MOVE    "後期高齢者１日金額"
                                   TO  HSCWRES-SENTEI-KOUKI-LBL (IDXW)
               IF    ( SYS-5001-SENTEI-KINGAKU-X NOT = SPACE)
                AND  ( SYS-5001-SENTEI-KINGAKU-X NOT = "0000")
                   MOVE    SYS-5001-SENTEI-KINGAKU
                                   TO  WRK-NUMZ
                   MOVE    WRK-NUMZ
                                   TO  HSCWRES-SENTEI-IPPAN-DATA (IDXW)
                                       HSCWRES-SENTEI-KOUKI-DATA (IDXW)
               ELSE
                   MOVE   ZERO     TO  WRK-NUMZ
                   IF    ( SYS-5001-SENTEI-IPN-KINGAKU-X NOT =  SPACE )
                       MOVE   SYS-5001-SENTEI-IPN-KINGAKU
                                   TO  WRK-NUMZ
                   END-IF
                   MOVE   WRK-NUMZ TO  HSCWRES-SENTEI-IPPAN-DATA (IDXW)
      *
                   MOVE   ZERO     TO  WRK-NUMZ
                   IF    ( SYS-5001-SENTEI-RJN-KINGAKU-X NOT =  SPACE )
                       MOVE   SYS-5001-SENTEI-RJN-KINGAKU
                                   TO  WRK-NUMZ
                   END-IF
                   MOVE   WRK-NUMZ TO  HSCWRES-SENTEI-KOUKI-DATA (IDXW)
               END-IF
      *
               MOVE    "円"        TO  HSCWRES-SENTEI-IPPAN-NAME (IDXW)
                                       HSCWRES-SENTEI-KOUKI-NAME (IDXW)
      *
           END-EVALUATE
      *
          .
       20013-SENTEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       20014-BRMNUM-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SYS-5001-BTU-NUM
                                   TO  SYS-KBNCD
           MOVE    "%"             TO  SYS-KBNCD (3:1)
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY9-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           MOVE    ZERO            TO  IDXR
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXR        >=  CONST-BYOSHITSU-MAX )
                    OR   ( WRK-ERRCD       NOT =   SPACE )
      *
               COMPUTE IDXR    =   IDXR    +   1
      *
               MOVE    "病室番号"  TO  HSCWRES-BRMNUM-LBL  (IDXW IDXR)
               MOVE    SYS-5002-BRM-NUM
                                   TO  HSCWRES-BRMNUM-DATA (IDXW IDXR)
      *
               PERFORM 900-SYSKANRI-KEY9-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       20014-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSCWRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSCWRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               IF    ( CNT-BYOTO       =   ZERO )
                   MOVE    "03"        TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSCWRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSCWRES-API-RESULT-MSG
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
               IF    ( WRK-SYMD            =   "00000000" )
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "00"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "01"
               MOVE    "基準日の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "入院基本情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "病棟情報が取得できません"
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
               STRING  "基準日は平成２０年（２００８年）"
                                       DELIMITED BY SIZE
                       "４月１日以降"
                                       DELIMITED BY SIZE
                       "を指定してください"
                                       DELIMITED BY SIZE
               INTO  WRK-ERRMSG
               END-STRING
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
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST12-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsconfwardv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    HSCWRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_hsconfwardv2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST12-BODY
               MOVE    "application/xml"   TO  APILST12-CONTENT-TYPE
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY9-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY9-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-KJNYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       901-TENSU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-TNS-SRYYMD      TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-TENSU-KEY-SEL-EXT.
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
