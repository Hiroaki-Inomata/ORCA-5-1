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
       PROGRAM-ID.         ORAPI031RC1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院基本情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/08/20    NACL−太田　新規作成
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXK                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-TNS-SRYCD           PIC X(09).
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
      *
       01  CONST-AREA.
           03  CONST-NYUINKSN-MAX      PIC 9(03)   VALUE 21.
           03  CONST-H260401           PIC X(08)   VALUE "20140401".
           03  CONST-H240401           PIC X(08)   VALUE "20120401".
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
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
      *    API XML読み込み用定義体
           COPY    "CPHSCONFBASEV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSCONFBASEV2RES.INC".
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
      *    入院基本
           COPY    "CPSK5000.INC".
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
               MOVE   404             TO  APILST11-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-END-SEC
           END-IF
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
           INITIALIZE                  XML-HSCONFBASERES
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
      *        XML情報取り出し
               PERFORM 900-XML-READ-SEC
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
           MOVE    HSCBREQ-BASE-DATE
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
               PERFORM 2001-NYUIN-KIHON-EDIT-SEC
           ELSE
               MOVE    "02"        TO  WRK-ERRCD
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院基本情報編集処理
      *****************************************************************
       2001-NYUIN-KIHON-EDIT-SEC       SECTION.
      *
           IF    ( SYS-5000-EIYOUKSNKBN    =   SPACE )
               MOVE    "0"             TO  SYS-5000-EIYOUKSNKBN
           END-IF
      *
           IF    ( SYS-5000-BRMSGK-TAXKBN  =   SPACE )
               MOVE    "0"             TO  SYS-5000-BRMSGK-TAXKBN
           END-IF
      *
           IF    ( SYS-5000-HYOUKETUKBN    =   SPACE )
               MOVE    "0"             TO  SYS-5000-HYOUKETUKBN
           END-IF
      *
           IF    ( SYS-5000-TEISUKBN       =   SPACE )
               MOVE    "0"             TO  SYS-5000-TEISUKBN
           END-IF
      *
           IF    ( SYS-5000-SENTEIKBN      =   SPACE )
               MOVE    "1"             TO  SYS-5000-SENTEIKBN
               MOVE    "1"             TO  SYS-5000-SENTEI-FTNKBN
               MOVE   10               TO  SYS-5000-SENTEI-TANKA
               MOVE   "0"              TO  SYS-5000-SENTEI-TAXKBN
           END-IF
      *
           MOVE    "食事療養費"        TO  HSCBRES-SKJRYOYO-LBL
           MOVE    SYS-5000-SHOKUJIRYOYOCD
                                       TO  HSCBRES-SKJRYOYO-DATA
           MOVE    SYS-5000-SHOKUJIRYOYOCD
                                       TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-NAME            TO  HSCBRES-SKJRYOYO-NAME
      *
           IF    ( SYS-5000-STYUKYMD   >=  CONST-H240401 )
               IF    ( SYS-5000-STYUKYMD   >=  CONST-H260401 )
                AND  ( SPA-HOSPSBT     =   2 )
                   CONTINUE
               ELSE
                   MOVE    "栄養管理経過措置"
                                       TO  HSCBRES-EIYOKANRI-LBL
                   MOVE    SYS-5000-EIYOUKSNKBN
                                       TO  HSCBRES-EIYOKANRI-DATA
                   EVALUATE    SYS-5000-EIYOUKSNKBN
                   WHEN    "0"
                       MOVE    "栄養管理体制基準適合"
                                       TO  HSCBRES-EIYOKANRI-NAME
                   WHEN    "1"
                       MOVE    "栄養管理体制基準未適合"
                                       TO  HSCBRES-EIYOKANRI-NAME
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    "室料差額消費税"    TO  HSCBRES-SAGAKUTAX-LBL
           MOVE    SYS-5000-BRMSGK-TAXKBN
                                       TO  HSCBRES-SAGAKUTAX-DATA
           EVALUATE    SYS-5000-BRMSGK-TAXKBN
           WHEN    "0"
               MOVE    "なし"          TO  HSCBRES-SAGAKUTAX-NAME
           WHEN    "1"
               MOVE    "あり"          TO  HSCBRES-SAGAKUTAX-NAME
           END-EVALUATE
      *
           MOVE    "標欠による減額"    TO  HSCBRES-HYOKETSU-LBL
           MOVE    SYS-5000-HYOUKETUKBN
                                       TO  HSCBRES-HYOKETSU-DATA
           EVALUATE    SYS-5000-HYOUKETUKBN
           WHEN    "0"
               MOVE    "減額なし"
                                       TO  HSCBRES-HYOKETSU-NAME
           WHEN    "1"
               MOVE    "１００分の１０減額"
                                       TO  HSCBRES-HYOKETSU-NAME
           WHEN    "2"
               MOVE    "１００分の１５減額"
                                       TO  HSCBRES-HYOKETSU-NAME
           WHEN    "3"
               MOVE    "１００分の２減額"
                                       TO  HSCBRES-HYOKETSU-NAME
           WHEN    "4"
               MOVE    "１００分の３減額"
                                       TO  HSCBRES-HYOKETSU-NAME
           END-EVALUATE
      *
           MOVE    "定数超過"          TO  HSCBRES-TEISUCHOKA-LBL
           MOVE    SYS-5000-TEISUKBN   TO  HSCBRES-TEISUCHOKA-DATA
           EVALUATE    SYS-5000-TEISUKBN
           WHEN    "0"
               MOVE    "定数超過なし"  TO  HSCBRES-TEISUCHOKA-NAME
           WHEN    "1"
               MOVE    "定数超過あり"  TO  HSCBRES-TEISUCHOKA-NAME
           END-EVALUATE
      *
           MOVE    ZERO                TO  IDXK
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   20 )
               IF    ( SYS-5000-KSNKBN (IDX0)  =   "2" )
                   MOVE    SYS-5000-KSNCD (IDX0)
                                       TO  WRK-TNS-SRYCD
                   PERFORM 20011-NYUIN-KSN-EDIT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    SYS-5000-CHIIKIKSN  TO  WRK-TNS-SRYCD
           PERFORM 20011-NYUIN-KSN-EDIT-SEC
      *
           MOVE    "入院時の選定入院"  TO  HSCBRES-SENTEI-DEF-LBL
           MOVE    SYS-5000-SENTEIKBN  TO  HSCBRES-SENTEI-DEF-DATA
           EVALUATE    SYS-5000-SENTEIKBN
           WHEN    "1"
               MOVE    "選定対象"      TO  HSCBRES-SENTEI-DEF-NAME
           WHEN    "2"
               MOVE    "選定対象外"    TO  HSCBRES-SENTEI-DEF-NAME
           END-EVALUATE
      *
          .
       2001-NYUIN-KIHON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算編集処理
      *****************************************************************
       20011-NYUIN-KSN-EDIT-SEC        SECTION.
      *
           IF    ( WRK-TNS-SRYCD   NOT =   SPACE )
            AND  ( IDXK                <   CONST-NYUINKSN-MAX )
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   IF    ( TNS-HOSPSRYKBN  =   0 )
                    OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
      *
                       COMPUTE IDXK  = IDXK  + 1
      *
                       MOVE    "入院加算"
                                   TO  HSCBRES-NYUINKSN-LBL  (IDXK)
                       MOVE    TNS-SRYCD
                                   TO  HSCBRES-NYUINKSN-DATA (IDXK)
                       MOVE    TNS-NAME
                                   TO  HSCBRES-NYUINKSN-NAME (IDXK)
                   END-IF
               END-IF
           END-IF
      *
          .
       20011-NYUIN-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSCBRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSCBRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "00"            TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  HSCBRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  HSCBRES-API-RESULT-MSG
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
           IF    ( APILST11-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsconfbasev2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsconfbasev2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST11-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "private_objects"   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST11-BODY
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
                                       TO  HSCBREQ-PRIVATE-OBJECTS
      *
           IF    ( HSCBREQ-BASE-DATE   =   LOW-VALUE )
               MOVE    SPACE           TO  HSCBREQ-BASE-DATE
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
           IF    ( APILST11-BODY    NOT =   LOW-VALUE )
               DISPLAY "hsconfbasev2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsconfbasev2res"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    HSCBRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST11-BODY
               MOVE    "application/xml"   TO  APILST11-CONTENT-TYPE
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
