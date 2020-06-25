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
       PROGRAM-ID.         ORAPI021R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為返却１  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      * 04.08.00     NACL-多々納  13/01/11  診療回数テーブル１０対応
      * 04.08.00     NACL-多々納  13/12/XX  診療行為剤点数情報対応
      * 04.07.00     NACL-多々納  14/12/XX  診療月数対応
      * 04.07.00     NACL-多々納  15/08/XX  入院対応、単位追加対応
      *                                     包括対象項目追加
      * 04.08.00     NACL-多々納  15/12/14  伝票番号追加対応
      * 05.00.00     NACL-多々納  17/08/XX  CLASS=02伝票番号追加対応
      * 05.00.00     NACL-多々納  17/11/XX  CLASS=02点数など追加対応
      * 05.00.00     NACL-多々納  18/01/XX  レセ電データ移行分対応
      * 05.01.00     NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACCTPLUS         PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK02               PIC 9(01).
           03  FLG-TENCHK              PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
           03  FLG-HKNKBN              PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-HKNCHK              PIC 9(01).
      *
           03  FLG-RECEDEN             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDXR                PIC 9(02).
           03  IDXM                PIC 9(02).
      *    診療行為用
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-TBL             PIC 9(04).
      *
           03  IDX-D               PIC 9(02).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-BYOCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-KEY-HKNCOMBI        PIC X(04).
           03  WRK-KEY-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(30).
      *    月集計用
           03  WRK-H-SRYKBN            PIC X(02).
           03  WRK-CNT                 PIC 9(02).
           03  WRK-GOK-SURYO           PIC S9(08)V9(05).
           03  WRK-TBL-DAY-G.
               05  WRK-TBL-DAY         PIC 9(01)
                                       OCCURS   31.
      *
      *    名称
           03  WRK-TNS-NAME            PIC X(200).
           03  WRK-HZN-NAME            PIC X(200).
      *    剤内容
           03  WRK-KAISU               PIC 9(05).
           03  WRK-ZAITEN              PIC S9(07).
           03  WRK-SUM-TENSU           PIC 9(07).
           03  WRK-SUM-KINGAKU         PIC 9(07).
      *H26.12
      *    月数
           03  WRK-TUKICNT             PIC 9(02).
           03  WRK-END-SRYYM.
               05  WRK-END-SRYYY           PIC 9(04).
               05  WRK-END-SRYMM           PIC 9(02).
           03  WRK-SRYYMD              PIC X(08).
      *    包括検査数
           03  WRK-HOUKNS-CNT          PIC 9(02).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
      * CLASS=4　用
           03  WRK-RSI-MONEY           PIC S9(09).
           03  WRK-MONEY               PIC S9(09).
           03  WRK-SUM-TEN             PIC S9(09).
           03  WRK-SRY-SRYSYUKBN       PIC X(03).
      *    単位
           03  WRK-TANICD              PIC X(03).
           03  WRK-TANINAME            PIC X(24).
      *
           03  WRK-DENPNUM             PIC 9(07).
      *
      *
       01  TBL-JYURRK-AREA.
           03  TBL-JYURRK-OCC          OCCURS   150.
               05  TBL-RRK-SRYYMD              PIC X(08).
               05  TBL-RRK-SRYKA               PIC X(02).
               05  TBL-RRK-HKNCOMBI            PIC X(04).
               05  TBL-RRK-RENNUM              PIC 9(01).
               05  TBL-RRK-DOUJI-RENNUM        PIC 9(01).
      *        伝票番号
               05  TBL-RRK-DENPNUM             PIC 9(07).
      *    月集計
       01  TBL-SYORI03-AREA.
           03  TBL-SYORI03-REC              OCCURS   40.
               05  TBL-SRYKBN               PIC X(02).
               05  TBL-SRYKBNMEI            PIC X(50).
               05  TBL-SYORI03-OCC2         OCCURS   400.
                   07  TBL-SRYCD            PIC X(09).
                   07  TBL-NAME             PIC X(80).
      *H27.8       単位
                   07  TBL-TANICD           PIC X(03).
                   07  TBL-TANINAME         PIC X(24).
      *
                   07  TBL-HKTKBN           PIC X(01).
                   07  TBL-DAY-G            OCCURS   31.
                       09  TBL-D-DAY        PIC 9(03).
                       09  TBL-D-SURYO      PIC S9(8)V9(05).
      *    月剤毎
       01  TBL-SYORI04-AREA.
           03  TBL-SYORI04-REC              OCCURS   31.
               05  TBL-SYORI04-REC2             OCCURS   40.
                   07  TBL-S4-SRYKBN            PIC X(02).
                   07  TBL-S4-SRYSYUKBN         PIC X(03).
                   07  TBL-S4-SRYKBNMEI         PIC X(50).
                   07  TBL-S4-IDX               PIC 9(04).
                   07  TBL-SYORI04-OCC2         OCCURS   100.
                       09  TBL-S4-JIHOKBN          PIC  X(01).
                       09  TBL-S4-ZAITEN           PIC S9(09).
                       09  TBL-S4-MOENY            PIC S9(09).
                       09  TBL-S4-KAISU            PIC  9(03).
      *
           03  TBL-S4-HKNCOMBI-G.
               05  TBL-S4-HKNCOMBI-OCC     OCCURS   20.
                   07  TBL-S4-HKNCOMBI         PIC X(04).
                   07  TBL-S4-HKNKBN           PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE.
               05  WRK-PERFORM-DATE1       PIC X(07).
               05  WRK-PERFORM-DATE2       PIC X(03).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-RENNUM              PIC 9(01).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCGETV2REQ.INC".
      *    API XML書き込み用定義体（受診履歴一覧）
           COPY    "CPMDCGET01V2RES.INC".
      *    API XML書き込み用定義体（診療行為明細）
           COPY    "CPMDCGET02V2RES.INC".
      *    API XML書き込み用定義体（月診療行為）
           COPY    "CPMDCGET03V2RES.INC".
      *    API XML書き込み用定義体（月剤毎診療行為）
           COPY    "CPMDCGET04V2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
           DISPLAY   "*******************"
           DISPLAY   "* medicalget start*"
           DISPLAY   "*******************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST6-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* medicalget end  *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-MEDICALGET01RES
           INITIALIZE                      XML-MEDICALGET02RES
           INITIALIZE                      XML-MEDICALGET03RES
           INITIALIZE                      XML-MEDICALGET04RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDC01RES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDC01RES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MDC01RES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST6-CLASS
           WHEN    "01"
      *        受診履歴情報
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        診療行為内容（受診）
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        診療行為（月）
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
      *        診療行為剤点数情報
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APILST6-CLASS
           WHEN    "01"
      *        受診履歴情報
               PERFORM 2002-SYORI-01-SEC
           WHEN    "02"
      *        診療行為内容情報
               PERFORM 2003-SYORI-02-SEC
           WHEN    "03"
      *        月診療行為内容情報
               PERFORM 2004-SYORI-03-SEC
           WHEN    "04"
      *        診療行為剤点数情報
               PERFORM 2005-SYORI-04-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST6-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalgetv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST6-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalgetreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalgetv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST6-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalgetreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-MEDICALGETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  MDC01RES-API-RESULT
               MOVE    "処理終了"          TO  MDC01RES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  MDC01RES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDC01RES-API-RESULT-MSG
           END-IF
      *
           EVALUATE    FLG-SYORI
               WHEN    0
               WHEN    1
                   PERFORM 3001-MDC01XMLMAKE-SEC
               WHEN    2
                   PERFORM 3002-MDC02XMLMAKE-SEC
               WHEN    3
                   PERFORM 3003-MDC03XMLMAKE-SEC
               WHEN    4
                   PERFORM 3004-MDC04XMLMAKE-SEC
           END-EVALUATE
      *
           .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（受診履歴内容）
      *****************************************************************
       3001-MDC01XMLMAKE-SEC        SECTION.
      *
           IF      APILST6-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalget01v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC01RES-MODE
           MOVE    "medicalget01res"   TO  MDC01RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET01RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalget01v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC01RES-MODE
           MOVE    "medicalget01res"   TO  MDC01RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET01RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MDC01RES-OBJECT     TO  APILST6-BODY 
               MOVE    "application/xml"   TO  APILST6-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-MDC01XMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（診療行為内容）
      *****************************************************************
       3002-MDC02XMLMAKE-SEC        SECTION.
      *
      *    共通編集
           MOVE   "Medical Info"           TO  MDC02RES-RESKEY
      *
           MOVE    MDC01RES-INFORMATION-DATE
                                           TO  MDC02RES-INFORMATION-DATE
           MOVE    MDC01RES-INFORMATION-TIME
                                           TO  MDC02RES-INFORMATION-TIME
           MOVE    MDC01RES-API-RESULT     TO  MDC02RES-API-RESULT
           MOVE    MDC01RES-API-RESULT-MSG TO  MDC02RES-API-RESULT-MSG
      *
           IF      APILST6-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalget02v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC02RES-MODE
           MOVE    "medicalget02res"   TO  MDC02RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET02RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalget02v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC02RES-MODE
           MOVE    "medicalget02res"   TO  MDC02RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET02RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MDC02RES-OBJECT     TO  APILST6-BODY 
               MOVE    "application/xml"   TO  APILST6-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3002-MDC02XMLMAKE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報書き込み処理（月診療行為）
      *****************************************************************
       3003-MDC03XMLMAKE-SEC        SECTION.
      *
      *    共通編集
           MOVE   "Medical Info"           TO  MDC03RES-RESKEY
      *
           MOVE    MDC01RES-INFORMATION-DATE
                                           TO  MDC03RES-INFORMATION-DATE
           MOVE    MDC01RES-INFORMATION-TIME
                                           TO  MDC03RES-INFORMATION-TIME
           MOVE    MDC01RES-API-RESULT     TO  MDC03RES-API-RESULT
           MOVE    MDC01RES-API-RESULT-MSG TO  MDC03RES-API-RESULT-MSG
      *
           IF      APILST6-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalget03v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC03RES-MODE
           MOVE    "medicalget03res"   TO  MDC03RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET03RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalget03v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC03RES-MODE
           MOVE    "medicalget03res"   TO  MDC03RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET03RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MDC03RES-OBJECT     TO  APILST6-BODY 
               MOVE    "application/xml"   TO  APILST6-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3003-MDC03XMLMAKE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報書き込み処理（月診療行為）
      *****************************************************************
       3004-MDC04XMLMAKE-SEC        SECTION.
      *
      *    共通編集
           MOVE   "Medical Info"           TO  MDC04RES-RESKEY
      *
           MOVE    MDC01RES-INFORMATION-DATE
                                           TO  MDC04RES-INFORMATION-DATE
           MOVE    MDC01RES-INFORMATION-TIME
                                           TO  MDC04RES-INFORMATION-TIME
           MOVE    MDC01RES-API-RESULT     TO  MDC04RES-API-RESULT
           MOVE    MDC01RES-API-RESULT-MSG TO  MDC04RES-API-RESULT-MSG
      *
           IF      APILST6-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalget04v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC04RES-MODE
           MOVE    "medicalget04res"   TO  MDC04RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET04RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalget04v2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDC04RES-MODE
           MOVE    "medicalget04res"   TO  MDC04RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALGET04RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MDC04RES-OBJECT     TO  APILST6-BODY 
               MOVE    "application/xml"   TO  APILST6-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3004-MDC04XMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF      MDCREQ-DEP-CODE     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-DEP-CODE
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               MOVE    SPA-SYSYMD          TO  WRK-SRYYMD
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    レセ電移行分
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               IF      (WRK-SRYKA(1:1)     =   "A"  )
                  AND  (FLG-RECEDEN        =   1    )
      *            対象外
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    MDCREQ-PATIENTID        TO  WRK-PTNUM
      *    診療科
           MOVE    MDCREQ-DEP-CODE         TO  WRK-SRYKA
      *    診療日付
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDC01RES-INFORMATION-DATE
                                           TO  MDCREQ-PERFORM-DATE
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
           IF      FLG-SYORI       NOT =   2
      *        診療行為詳細以外は、日を０１とする
               MOVE    "01"            TO  WRK-PERFORM-DD
           END-IF
      *    システム日付とする
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *H27.8
      *    入外区分（Ｉ：入院）
           IF      MDCREQ-INOUT-CLASS  =   "I"
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (FLG-SYORI           =   2      )
               AND    (WRK-ERRCD           =   SPACE  )
      *!!
      *            伝票番号なし、入院は必須
                   IF     (MDCREQ-INVOICD-NUMBER   =   SPACE)
                      OR  (SPA-NYUGAIKBN           =   "1"  )
                       MOVE    "02"                TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *H30.1
      *    レセ電データ移行分
           EVALUATE    MDCREQ-CONTAIN-MIGRATION
           WHEN    "False"
      *        含めない
               MOVE    1               TO  FLG-RECEDEN
           WHEN    "True"
           WHEN    SPACE
      *        含める
               MOVE    ZERO            TO  FLG-RECEDEN
           WHEN    OTHER
               MOVE    ZERO            TO  FLG-RECEDEN
           END-EVALUATE
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PERFORM-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-PERFORM-DATE
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  MDC01RES-PATIENTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *H30.1
      *    レセ電データ移行分
           IF     (WRK-SRYKA (1:1)     =   "A" )
              AND (WRK-SRYKA (2:1)     NUMERIC )
               MOVE    SPACE           TO  WRK-SRYKA-NAME
               GO      TO      20011-SRYKA-CHK-EXT
           END-IF
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
      **** MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
      **** MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDC01RES-HEN-SEC
      *
      *H26.12
      *    診療月数チェック
           PERFORM 20023-TUKICNT-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *!
      *    外来とする
      **** MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *H26.12
      *    検索終了診療年月
           MOVE    ZERO                TO  WRK-END-SRYYM
           IF      WRK-TUKICNT         >   ZERO
               MOVE    WRK-PERFORM-YM      TO  WRK-END-SRYYM
               IF      WRK-END-SRYMM       <=  WRK-TUKICNT
                   COMPUTE WRK-END-SRYYY       =   WRK-END-SRYYY
                                               -   1
                   COMPUTE WRK-END-SRYMM       =  (WRK-END-SRYMM
                                               +   12 )
                                               -   WRK-TUKICNT
               ELSE
                   COMPUTE WRK-END-SRYMM       =   WRK-END-SRYMM
                                               -   WRK-TUKICNT
               END-IF
               IF      WRK-TUKICNT         =   99
                   MOVE    ZERO            TO  WRK-END-SRYYM
               END-IF
           END-IF
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
      *H26.12
           IF      WRK-TUKICNT         =   ZERO
      *        月数指定なし（昇順）
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-PTID            TO  JYURRK-PTID
               MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
               MOVE    WRK-PERFORM-YM      TO  JYURRK-SRYYMD(1:6)
               MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
               MOVE    WRK-PERFORM-YM      TO  JYURRK-UPSRYYMD(1:6)
               MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
               MOVE    "key14"             TO  WRK-MCP-PATHNAME
      *
           ELSE
      *        月数指定あり（降順）
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-PTID            TO  JYURRK-PTID
               MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
               MOVE    WRK-PERFORM-YM      TO  JYURRK-SRYYMD(1:6)
               MOVE    "31"                TO  JYURRK-SRYYMD(7:2)
      *
               MOVE    "key60"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  TBL-MAX
           INITIALIZE                      TBL-JYURRK-AREA
           PERFORM
               UNTIL   (FLG-JYURRK         =   1    )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *H26.12
               IF      WRK-TUKICNT         >   ZERO
                   IF      JYURRK-SRYYMD(1:6)  <=  WRK-END-SRYYM
                       MOVE    1               TO  FLG-JYURRK
                   END-IF
               END-IF
      *
               IF      FLG-JYURRK          =   ZERO
      *
               MOVE    ZERO            TO  FLG-ARI
      *H30.1
      *        レセ電移行分対象外
               IF     (FLG-RECEDEN         =   1   )
                 AND  (JYURRK-SRYKA(1:1)   =   "A" )
                   MOVE    1               TO  FLG-ARI
               END-IF
      *
      *        診療日、連番で１行とする
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   TBL-MAX  )
                               OR (FLG-ARI =   1        )
                   IF     (JYURRK-SRYYMD       =   TBL-RRK-SRYYMD(IDX2))
                     AND  (JYURRK-SRYKA        =   TBL-RRK-SRYKA(IDX2))
                     AND  (JYURRK-RENNUM       =   TBL-RRK-RENNUM(IDX2))
                     AND  (JYURRK-HKNCOMBINUM  =   TBL-RRK-HKNCOMBI
                                                                 (IDX2))
                     AND  (JYURRK-DOUJI-RENNUM =   TBL-RRK-DOUJI-RENNUM
                                                                 (IDX2))
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-PERFORM
      *
               IF      FLG-ARI             =   ZERO
                   ADD     1               TO  IDX-MAX
                   IF      IDX-MAX         >   150
                       MOVE    "14"            TO  WRK-ERRCD
                   ELSE
      *                明細項目退避
                       ADD     1               TO  TBL-MAX
                       MOVE    JYURRK-SRYYMD   TO  TBL-RRK-SRYYMD
                                                             (TBL-MAX)
                       MOVE    JYURRK-SRYKA    TO  TBL-RRK-SRYKA
                                                             (TBL-MAX)
                       MOVE    JYURRK-HKNCOMBINUM
                                               TO  TBL-RRK-HKNCOMBI
                                                             (TBL-MAX)
                       MOVE    JYURRK-RENNUM   TO  TBL-RRK-RENNUM
                                                             (TBL-MAX)
                       MOVE    JYURRK-DOUJI-RENNUM
                                               TO  TBL-RRK-DOUJI-RENNUM
                                                             (TBL-MAX)
                       MOVE    JYURRK-DENPNUM
                                               TO  TBL-RRK-DENPNUM
                                                             (TBL-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
      *
               END-IF
      *
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-MAX             >   ZERO
               PERFORM 20021-XMLHENSYU-SEC
           ELSE
      *        対象なし
               MOVE    "15"                TO  WRK-ERRCD
               MOVE    WRK-PERFORM-DATE1   TO  MDC01RES-PERFORM-DATE (1)
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-MDC01RES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PTNUM           TO  MDC01RES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  MDC01RES-NAME
           MOVE    PTINF-KANANAME      TO  MDC01RES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDC01RES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDC01RES-SEX
      *
           .
       20021-MDC01RES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療月数チェック処理
      *****************************************************************
       20023-TUKICNT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-TUKICNT
           IF      MDCREQ-FOR-MONTHS   NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-FOR-MONTHS   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "16"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TUKICNT
               END-IF
           END-IF
      *
           IF     (WRK-TUKICNT         >=  0  AND  <=  12 )
              OR  (WRK-TUKICNT         =   99      )
               CONTINUE
           ELSE
               MOVE    "16"                TO  WRK-ERRCD
           END-IF
           .
       20023-TUKICNT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴内容編集処理
      *****************************************************************
       20021-XMLHENSYU-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-MAX
      *        診療日
               MOVE    TBL-RRK-SRYYMD (IDX)    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDC01RES-PERFORM-DATE
                                                                 (IDX)
      *
               MOVE    TBL-RRK-SRYYMD (IDX)    TO  WRK-SRYYMD
      *        診療科
               MOVE    TBL-RRK-SRYKA (IDX) TO  MDC01RES-DEP-CODE
                                                                (IDX)
               PERFORM 20011-SRYKAMEI-HEN01-SEC
               MOVE    WRK-SRYKA-NAME      TO  MDC01RES-DEP-NAME
                                                                (IDX)
      *        連番
               MOVE    TBL-RRK-RENNUM(IDX) TO  MDC01RES-NUMBER (IDX)
      *        保険組合せ
               MOVE    TBL-RRK-HKNCOMBI(IDX)
                                       TO  MDC01RES-COMBINATION-NUMBER
                                                               (IDX)
      *        伝票番号
               IF      SPA-NYUGAIKBN   =   "2"
      *H30.1     レセ電データ移行分
                  AND (WRK-SRYKA(1:1)  NOT =   "A" )
                   MOVE    TBL-RRK-DENPNUM (IDX)
                                       TO  MDC01RES-INVOICD-NUMBER
                                                               (IDX)
               END-IF
               PERFORM 20011-HKNCOMBI-HEN01-SEC
           END-PERFORM
           .
       20021-XMLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       20011-SRYKAMEI-HEN01-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (TBL-RRK-SRYKA(IDX)
                                       =  MDC01RES-DEP-CODE (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        診療科検索
               MOVE    TBL-RRK-SRYKA(IDX)  TO   WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
           ELSE
               MOVE    MDC01RES-DEP-NAME (IDX-H)
                                           TO  WRK-SRYKA-NAME
           END-IF
           .
       20011-SRYKAMEI-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20011-HKNCOMBI-HEN01-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (TBL-RRK-HKNCOMBI(IDX)
                                       =  MDC01RES-COMBINATION-NUMBER
                                                                 (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        保険組合せテーブル検索
               MOVE    TBL-RRK-HKNCOMBI(IDX)  TO   WRK-HKNCOMBI
               PERFORM 20012-HKNCOMBI-SYORI-SEC
           ELSE
      *        保険情報複写
               MOVE    MDC01RES-INSURANCE-INFO (IDX-H)
                                       TO  MDC01RES-INSURANCE-INFO (IDX)
           END-IF
           .
       20011-HKNCOMBI-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       20012-HKNCOMBI-SYORI-SEC     SECTION.
      *
           IF      WRK-HKNCOMBI           =    "9999"
               INITIALIZE                  HKNCOMBI-REC
               MOVE    "包括分入力"        TO  COMB-SYU-TANSEIDONAME
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-PTID            TO  COMB-PTID
               MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
               PERFORM 920-HKNCOMBI-READ-SEC
           END-IF
           MOVE    WRK-HKNCOMBI        TO  MDC01RES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *    主保険−保険番号
           MOVE    COMB-HKNNUM         TO  MDC01RES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  MDC01RES-MAIN-INSURANCE-NAME
                                                            (IDX)
           IF      COMB-HKNID      NOT =   ZERO
      *        保険情報編集
               PERFORM 930-PTHKNINF-READ-SEC
      *        保険者番号
               MOVE    PTHKN-HKNJANUM  TO
                                       MDC01RES-MAIN-INSURANCE-NUMBER
                                                            (IDX)
      *        記号・番号
               MOVE    PTHKN-KIGO      TO  MDC01RES-MAIN-MARK  (IDX)
               MOVE    PTHKN-NUM       TO  MDC01RES-MAIN-NUMBER(IDX)
           END-IF
      *    公費情報
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   4
               IF      COMB-KOHID (IDK)    NOT =   ZERO
      *            公費の種類
                   MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  MDC01RES-INSURANCE-CLASS
                                                            (IDX IDK)
                   MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  MDC01RES-INSURANCE-NAME
                                                            (IDX IDK)
      *            公費情報編集
                   PERFORM 940-PTKOHINF-READ-SEC
      *            負担者番号
                   MOVE    PTKOH-FTNJANUM  TO  MDC01RES-PROVIDER-NUMBER
                                                            (IDX IDK)
                   MOVE    PTKOH-JKYSNUM   TO  MDC01RES-RECIPIENT-NUMBER
                                                            (IDX IDK)
               END-IF
           END-PERFORM
           .
       20012-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴診療行為内容編集処理
      *****************************************************************
       2003-SYORI-02-SEC          SECTION.
      *
      *H29.8
      *    伝票番号なし
           IF     (MDCREQ-INVOICD-NUMBER   =   SPACE )
              OR  (SPA-NYUGAIKBN           =   "1"   )
      *        保険組合せチェック
               PERFORM 20031-HKNCOMBI-CHK-SEC
           ELSE
      *        伝票番号チェック
               PERFORM 20033-DENPNUM-CHK-SEC
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20032-MDC02RES-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *!
      *    外来とする
      **** MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYSYMD
      *
      *    受診歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PERFORM-YMD     TO  JYURRK-SRYYMD
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
           IF      WRK-HKNCOMBI        =   SPACE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    WRK-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WRK-KEY-HKNCOMBI
           PERFORM
               UNTIL   (FLG-JYURRK         =   1    )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *        保険組合せ毎
               IF     (WRK-KEY-HKNCOMBI    =   SPACE )
                  OR  (WRK-KEY-HKNCOMBI    NOT =   JYURRK-HKNCOMBINUM)
                   ADD     1                   TO  IDX
                   MOVE    ZERO                TO  IDY
                   IF      IDX                 >   5
                       MOVE    "21"            TO  WRK-ERRCD
                   ELSE
                       MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBI
                       PERFORM 20033-HKNCOMBI-HEN-SEC
      *H29.8
                       IF     (SPA-NYUGAIKBN       =   "2"  )
      *H30.1
                          AND (JYURRK-SRYKA(1:1)   NOT =   "A" )
      *                    伝票番号
                           MOVE    JYURRK-DENPNUM  TO
                                               MDC02RES-INVOICD-NUMBER
                                                               (IDX)
                       END-IF
                   END-IF
               END-IF
      *        剤毎編集
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
      *v4.8
      *        IF      IDX-REN         >   4
      *            MOVE    4               TO  IDX-REN
      *******  END-IF
               PERFORM VARYING     IDX-J   FROM    1   BY  1
                       UNTIL      (IDX-J       >   15      )
                               OR (WRK-ERRCD   NOT =   SPACE)
                   IF      JYURRK-ZAINUM (IDX-J)   NOT =   ZERO
                       PERFORM 20034-ZAISET-HEN-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
      *        対象なし
               MOVE    "15"                TO  WRK-ERRCD
               IF      WRK-HKNCOMBI    NOT =   SPACE
                   MOVE    1                   TO  IDX
                   MOVE    WRK-HKNCOMBI        TO  WRK-KEY-HKNCOMBI
                   PERFORM 20033-HKNCOMBI-HEN-SEC
               END-IF
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20031-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           IF     (MDCREQ-COMBINATION-NUMBER   =   SPACE )
             AND  (MDCREQ-INSURANCE-INFO       =   SPACE )
      *        保険組合せ設定なし
               GO      TO      20031-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
      *        対象なし有
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *        当月有効分から
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                           TO  ORCSAPIHKN-IN-HKNNUM
      *        保険者番号
               MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                           TO  ORCSAPIHKN-IN-HKNJANUM
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                           TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *        記号
               MOVE    MDCREQ-MAIN-MARK    TO  ORCSAPIHKN-IN-MARK
      *        番号
               MOVE    MDCREQ-MAIN-NUMBER  TO  ORCSAPIHKN-IN-NUMBER
      *        公費情報
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
      *            公費種類
                   MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *            公費名称
                   MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *            負担者番号
                   MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *            受給者番号
                   MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
               END-PERFORM
               CALL    "ORCSAPIHKN"        USING
                                           SPA-AREA
                                           ORCSAPIHKNAREA
               IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
                   MOVE    "20"                TO  WRK-ERRCD
               ELSE
                   MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
               END-IF
           ELSE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
      *        存在チェック
               IF     (WRK-ERRCD           =   SPACE   )
                  AND (WRK-HKNCOMBI    NOT =    "9999" )
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    SPA-PTID            TO  COMB-PTID
                   MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
                   PERFORM 920-HKNCOMBI-READ-SEC
                   IF      FLG-HKNCOMBI        =   1
                       MOVE    "20"                TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       20031-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "12"                TO  WRK-ERRCD
               MOVE    MDCREQ-COMBINATION-NUMBER   TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
           .
       200311-HKNCOMBI-NUM-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号チェック処理
      *****************************************************************
       20033-DENPNUM-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-DENPNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-INVOICD-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "22"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-DENPNUM
           END-IF
      *
           IF      WRK-DENPNUM         =   ZERO
               MOVE    "22"                TO  WRK-ERRCD
               GO      TO      20033-DENPNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      JYURRK-SRYYMD   NOT =   WRK-PERFORM-YMD
                   MOVE    "23"                 TO  WRK-ERRCD
               END-IF
               IF      WRK-ERRCD           =   SPACE
                   MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
                   MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
      *            診療科名称
                   MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
                   PERFORM 20011-SRYKA-CHK-SEC
               END-IF
           ELSE
               MOVE    "15"                 TO  WRK-ERRCD
           END-IF
      *
           .
       20033-DENPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20032-MDC02RES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PERFORM-DATE    TO  MDC02RES-PERFORM-DATE
      *
           MOVE    WRK-PTNUM           TO  MDC02RES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  MDC02RES-NAME
           MOVE    PTINF-KANANAME      TO  MDC02RES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDC02RES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDC02RES-SEX
      *
      *    診療科
           MOVE    WRK-SRYKA           TO  MDC02RES-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  MDC02RES-DEP-NAME
      *
      *H29.8
           IF      MDCREQ-INVOICD-NUMBER   =   SPACE
      *    連番
               IF      MDCREQ-NUMBER   NOT NUMERIC
                   MOVE    1                   TO  WRK-RENNUM
               ELSE
                   MOVE    MDCREQ-NUMBER       TO  WRK-RENNUM
               END-IF
               MOVE    WRK-RENNUM          TO  MDC02RES-NUMBER
           ELSE
      *        伝票番号
               MOVE    MDCREQ-INVOICD-NUMBER
                                           TO  MDC02RES-INVOICD-NUMBER
                                                               (1)
      *        連番
               MOVE    WRK-RENNUM          TO  MDC02RES-NUMBER
           END-IF
      *
      *    保険組合せ情報
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-HKNCOMBI    TO  MDC02RES-COMBINATION-NUMBER
                                                                 (1)
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                       TO  MDC02RES-MAIN-INSURANCE-CLASS
                                                                 (1)
      *        保険者番号
               MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                   TO  MDC02RES-MAIN-INSURANCE-NUMBER(1)
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                       TO  MDC02RES-MAIN-INSURANCE-NAME
                                                                 (1)
      *        記号
               MOVE    MDCREQ-MAIN-MARK    TO  MDC02RES-MAIN-MARK (1)
      *        番号
               MOVE    MDCREQ-MAIN-NUMBER  TO  MDC02RES-MAIN-NUMBER (1)
      *        公費情報
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL       IDK     >   4
      *            公費種類
                   MOVE    MDCREQ-INSURANCE-CLASS (IDK)
                                       TO  MDC02RES-INSURANCE-CLASS
                                                              (1 IDK)
      *            公費名称
                   MOVE    MDCREQ-INSURANCE-NAME  (IDK)
                                       TO  MDC02RES-INSURANCE-NAME
                                                              (1 IDK)
      *            負担者番号
                   MOVE    MDCREQ-PROVIDER-NUMBER(IDK)
                                       TO  MDC02RES-PROVIDER-NUMBER
                                                              (1 IDK)
      *            受給者番号
                   MOVE    MDCREQ-RECIPIENT-NUMBER (IDK)
                                       TO  MDC02RES-RECIPIENT-NUMBER
                                                              (1 IDK)
               END-PERFORM
           END-IF
           .
       20032-MDC02RES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ内容編集処理
      *****************************************************************
       20033-HKNCOMBI-HEN-SEC          SECTION.
      *
           IF      WRK-KEY-HKNCOMBI        =    "9999"
               INITIALIZE                  HKNCOMBI-REC
               MOVE    "包括分入力"        TO  COMB-SYU-TANSEIDONAME
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-PTID            TO  COMB-PTID
               MOVE    WRK-KEY-HKNCOMBI    TO  COMB-HKNCOMBINUM
               PERFORM 920-HKNCOMBI-READ-SEC
           END-IF
           MOVE    WRK-KEY-HKNCOMBI    TO  MDC02RES-COMBINATION-NUMBER
                                                            (IDX)
      *    主保険−保険番号
           MOVE    COMB-HKNNUM         TO  MDC02RES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  MDC02RES-MAIN-INSURANCE-NAME
                                                            (IDX)
           IF      COMB-HKNID      NOT =   ZERO
      *        保険情報編集
               PERFORM 930-PTHKNINF-READ-SEC
      *        保険者番号
               MOVE    PTHKN-HKNJANUM  TO
                                       MDC02RES-MAIN-INSURANCE-NUMBER
                                                            (IDX)
      *        記号・番号
               MOVE    PTHKN-KIGO      TO  MDC02RES-MAIN-MARK  (IDX)
               MOVE    PTHKN-NUM       TO  MDC02RES-MAIN-NUMBER(IDX)
           END-IF
      *    公費情報
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   4
               IF      COMB-KOHID (IDK)    NOT =   ZERO
      *            公費の種類
                   MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  MDC02RES-INSURANCE-CLASS
                                                            (IDX IDK)
                   MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  MDC02RES-INSURANCE-NAME
                                                            (IDX IDK)
      *            公費情報編集
                   PERFORM 940-PTKOHINF-READ-SEC
      *            負担者番号
                   MOVE    PTKOH-FTNJANUM  TO  MDC02RES-PROVIDER-NUMBER
                                                            (IDX IDK)
                   MOVE    PTKOH-JKYSNUM   TO  MDC02RES-RECIPIENT-NUMBER
                                                            (IDX IDK)
               END-IF
           END-PERFORM
           .
       20033-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       20034-ZAISET-HEN-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-J)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *
           MOVE    ZERO                TO  FLG-CHK
      *    回数ゼロは対象外
           IF     (WRK-KAISU           =   ZERO )
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SUM-TENSU
           MOVE    ZERO                TO  WRK-SUM-KINGAKU
           MOVE    ZERO                TO  FLG-TENCHK
      *H29.11
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
      *
           IF     (FLG-CHK             =   ZERO  )
               INITIALIZE                  SRYACCTPLUS-REC
               IF     (ACCT-ZAIREQFLG      =   "1"   )
      *            附加情報あり
                   PERFORM 32012-SRYACCTPLUS-READ-SEC
               END-IF
               IF      ACCTP-PLUSKBN        =   1
                    COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                                *   -1
               ELSE
                   MOVE    ACCT-ZAITEN           TO  WRK-ZAITEN
               END-IF
           END-IF
           IF     (FLG-CHK             =   ZERO  )
      *        診療行為テーブル検索
               INITIALIZE                  SRYACT-REC
               MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
               MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
               MOVE    ACCT-PTID           TO  SRY-PTID
               MOVE    ACCT-SRYKA          TO  SRY-SRYKA
               MOVE    ACCT-SRYYM          TO  SRY-SRYYM
               MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
      *        自費金額・点数＝ゼロは対象外
      *        IF     (ACCT-ZAITEN         =   ZERO )  AND
      *               (SRY-JIHIMONEYTOTAL  =   ZERO )
      *            MOVE    1                   TO  FLG-SRYACT
      *        END-IF
      *
               MOVE    ZERO                TO  IDX-S
               PERFORM
                   UNTIL   FLG-SRYACT      =   1
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (SRY-SRYCD (IDXA)    =   SPACE)
      *                対象外
                       IF      (SRY-SRYCD (IDXA)(1:4)      =   "0086" )
                           CONTINUE
                       ELSE
                           PERFORM 200341-MEISE-HEN-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        金額編集
               IF      IDX-S               >   ZERO
                   PERFORM 200342-KINGAKU-HEN-SEC
               END-IF
           END-IF
           .
       20034-ZAISET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200341-MEISE-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDX-S
           IF      IDX-S               =   1
               ADD     1                   TO  IDY
      *        診療種別編集
               PERFORM 200342-ZAINUM-HEN-SEC
           END-IF
      *    明細
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           MOVE    TNS-NAME            TO  WRK-TNS-NAME
      *    器材商品名コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "058"
               MOVE   TNS-FORMALNAME       TO  WRK-TNS-NAME
           END-IF
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" ) AND
                  (SRY-MEISKYFLG  (IDXA)   =   "1" )
               MOVE    SPACE               TO  WRK-TNS-NAME
               STRING     "〔"             DELIMITED   BY  SIZE
                          TNS-NAME         DELIMITED   BY  "    "
                          "〕"             DELIMITED   BY  SIZE
                          INTO             WRK-TNS-NAME
               END-STRING
           END-IF
      *    患者コメント
           IF      SRY-INPUTNUM (IDXA) NOT =   ZERO
               PERFORM 200342-PTCOM-HEN-SEC
           END-IF
      *    用法コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "001"
               MOVE    SPACE               TO  WRK-HZN-NAME
               STRING     "【"             DELIMITED   BY  SIZE
                          WRK-TNS-NAME     DELIMITED   BY  "    "
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-HZN-NAME
               END-STRING
               MOVE    WRK-HZN-NAME        TO  WRK-TNS-NAME
           END-IF
      *
      *    数量
           MOVE    SRY-SRYSURYO(IDXA)  TO  WRK-SURYO
      *    減点点数編集
           IF      TNS-TENSIKIBETU     =   7
                                       OR  8
               MOVE    ZERO                TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    1                   TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    SPACE               TO  WRK-SURYO-HX
           ELSE
               PERFORM 810-NUMHENKAN-SEC
           END-IF
      *
           MOVE    SRY-SRYCD (IDXA)    TO  MDC02RES-PRSCRPT-CODE
                                                       (IDX IDY IDX-S)
           MOVE    WRK-TNS-NAME        TO  MDC02RES-PRSCRPT-NAME
                                                       (IDX IDY IDX-S)
           MOVE    WRK-SURYO-HX        TO  MDC02RES-PRSCRPT-NUMBER
                                                       (IDX IDY IDX-S)
      *H27.8
      *    単位
           PERFORM 200341-TANINAME-HEN-SEC
           MOVE    WRK-TANICD          TO  MDC02RES-UNIT-CODE
                                                     (IDX IDY IDX-S)
           MOVE    WRK-TANINAME        TO  MDC02RES-UNIT-NAME
                                                     (IDX IDY IDX-S)
      *H29.11
      *    点数識別、点数
           IF     (SRY-SRYCD (IDXA) (1:1)  =   "1"
                                           OR  "6"
                                           OR  "7" )
              OR  (SRY-SRYCD (IDXA) (1:3)  =   "059"
                                           OR  "058")
               MOVE    TNS-TENSIKIBETU     TO  MDC02RES-TENSIKIBETU
                                                     (IDX IDY IDX-S)
               MOVE    TNS-TEN             TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  MDC02RES-TEN
                                                     (IDX IDY IDX-S)
           END-IF
      *
      *    診療コードの金額集計
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
               MOVE    SRY-JIHIMONEYTOTAL      TO  WRK-SUM-KINGAKU
           ELSE
               IF     (SRY-SRYCD (IDXA)(1:1)   =   "1"  )
                   EVALUATE    TNS-TENSIKIBETU
                       WHEN    3
                           ADD     TNS-TEN         TO  WRK-SUM-TENSU
                       WHEN    1
                           ADD     TNS-TEN         TO  WRK-SUM-KINGAKU
                   END-EVALUATE
               ELSE
                   IF      TNS-TEN             >   ZERO
                       MOVE    1               TO  FLG-TENCHK
                   END-IF
               END-IF
      *        自賠責のその他
               IF     (SRY-SRYCD (IDXA)(1:4)   =   "0959" )
      *            自賠責金額入力を点数とする
                   IF     (SRY-SRYSYUKBN       =   "809")
                       MOVE    SRY-JIHIMONEY(IDXA)     TO  TNS-TEN
                   END-IF
                   ADD     TNS-TEN             TO  WRK-SUM-KINGAKU
               END-IF
           END-IF
      *H29.11
      *    包括検査の判定
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "1" )  AND
                  (SRY-SRYKBN              =   "60" ) AND
                  (TNS-DATAKBN             =   1    ) AND
                  (TNS-HOUKNSKBN       NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
           END-IF
           .
       200341-MEISE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する処理
      *****************************************************************
       200342-PTCOM-HEN-SEC          SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-TNS-NAME
           END-IF
           .
       200342-PTCOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       200342-ZAINUM-HEN-SEC          SECTION.
      *
      *    診療種別編集
           IF      SRY-SRYSYUKBN       =   SPACE
               MOVE    SRY-SRYKBN          TO  WRK-SRYSYUKBN(1:2)
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           END-IF
           MOVE    WRK-SRYSYUKBN       TO  MDC02RES-MDC-CLASS
                                                             (IDX IDY)
      *    診療種別区分名称
           PERFORM 200341-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI   TO  MDC02RES-MDC-CLASS-NAME
                                                             (IDX IDY)
      *    回数
           MOVE    WRK-KAISU           TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC02RES-MDC-CLASS-NUMBER
                                                             (IDX IDY)
      *
           MOVE    "0"                 TO  MDC02RES-MDC-CODE  (IDX IDY)
      *    薬評分
           IF      ACCT-ZAIKBN         =   1
               MOVE    "2"             TO  MDC02RES-MDC-CODE  (IDX IDY)
           END-IF
      *    包括区分
           IF      ACCT-JIHOKBN        =   "1"
               MOVE    "1"             TO  MDC02RES-MDC-CODE  (IDX IDY)
               MOVE    "True"          TO  MDC02RES-INCLUSION-CLASS
                                                              (IDX IDY)
           END-IF
           .
       200342-ZAINUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤点数編集処理
      *****************************************************************
       200342-KINGAKU-HEN-SEC          SECTION.
      *
      *    診療コードの金額集計
           IF      ACCT-SRYKBN         =   "95"
                                       OR  "96"
               MOVE    ZERO                TO  WRK-ZAITEN
           ELSE
               IF     (WRK-SUM-KINGAKU     =   ZERO )
                OR    (FLG-TENCHK          =   1    )
                   MOVE    ZERO                TO  WRK-SUM-KINGAKU
               ELSE
      *            金額有
                   MOVE    WRK-SUM-TENSU       TO  WRK-ZAITEN
               END-IF
           END-IF
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC02RES-MDC-POINT
                                                             (IDX IDY)
      *    金額（自費・労災円）
           MOVE    WRK-SUM-KINGAKU     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC02RES-MDC-MOENY
                                                             (IDX IDY)
      *H29.11
      *    包括検査の検査数
           IF      WRK-HOUKNS-CNT      >   ZERO
               MOVE    WRK-HOUKNS-CNT  TO  MDC02RES-HKTKNSA-CNT
                                                             (IDX IDY)
           END-IF
           .
       200342-KINGAKU-HEN-EXT.
           EXIT.
      *H27.8
      *****************************************************************
      *     単位名称編集処理
      *****************************************************************
       200341-TANINAME-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-TANICD
           MOVE    SPACE               TO  WRK-TANINAME
      *    薬剤・器材
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"
                                           OR  "7" )
               MOVE    TNS-TANICD          TO  WRK-TANICD
               MOVE    TNS-TANINAME        TO  WRK-TANINAME
           END-IF
      *    診療コード・商品コードは単位ありの時
           IF     (TNS-TANICD          =   ZERO
                                       OR  SPACE )
               CONTINUE
           ELSE
               IF     (SRY-SRYCD (IDXA)(1:1)   =   "1"  ) 
                  OR  (SRY-SRYCD (IDXA)(1:3)   =   "059"
                                               OR  "058")
                   MOVE    TNS-TANICD          TO  WRK-TANICD
                   MOVE    TNS-TANINAME        TO  WRK-TANINAME
               END-IF
           END-IF
           .
       200341-TANINAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       200341-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       200341-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    月内診療行為内容編集処理
      *****************************************************************
       2004-SYORI-03-SEC          SECTION.
      *
      *    保険組合せチェック
           PERFORM 20031-HKNCOMBI-CHK-SEC
      *
      *    患者情報内容編集
           PERFORM 20041-MDC03RES-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2004-SYORI-03-EXT
           END-IF
      *!
      *    外来とする
      *****MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           INITIALIZE                      TBL-SYORI03-AREA
      *    診療会計マスターから編集
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SRYKA           TO  ACCT-SRYKA
           MOVE    WRK-PERFORM-YM      TO  ACCT-SRYYM
      *
           IF      WRK-SRYKA           =   SPACE
               MOVE    "key7"          TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-SRYACCT     =   1   )
               MOVE    ZERO                TO  FLG-CHK
      *        薬評剤対象外
               IF      ACCT-ZAIKBN         =   1
                   MOVE    1                   TO  FLG-CHK
               END-IF
      *        保険組合せ判定
               IF      WRK-HKNCOMBI        =   SPACE
      *            包括保険以外対象
                   IF     (ACCT-HKNCOMBI       =   "9999")
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               ELSE
                   IF     (WRK-HKNCOMBI        =   "9999" )
                       IF     (ACCT-HKNCOMBI       =   "9999")
                          OR  (ACCT-JIHOKBN        =   "1"    )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   ELSE
                       IF      ACCT-HKNCOMBI   NOT =   WRK-HKNCOMBI
                           MOVE    1                   TO  FLG-CHK
                       END-IF
                   END-IF
               END-IF
      *H30.1
      *        レセ電移行分対象外
               IF      WRK-SRYKA           =   SPACE
                   IF     (FLG-RECEDEN         =   1   )
                     AND  (ACCT-SRYKA(1:1)     =   "A" )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   PERFORM 20043-SRYACCT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    返却内容編集
           PERFORM 20044-MDC03RES-MEIHEN-SEC
           .
       2004-SYORI-03-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20041-MDC03RES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PERFORM-DATE1   TO  MDC03RES-PERFORM-DATE
      *
           MOVE    WRK-PTNUM           TO  MDC03RES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  MDC03RES-NAME
           MOVE    PTINF-KANANAME      TO  MDC03RES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDC03RES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDC03RES-SEX
      *
           .
       20041-MDC03RES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容処理
      *****************************************************************
       20043-SRYACCT-HEN-SEC                SECTION.
      *
      *    診療種別名称編集処理
      **** PERFORM 200431-SRYKBN-CHK-SEC
           MOVE    ACCT-SRYKBN         TO  WRK-H-SRYKBN
           MOVE    WRK-H-SRYKBN        TO  WRK-SRYSYUKBN(1:2)
           MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           PERFORM 200341-SRYKBN-MEI-SEC
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SYSYM
           MOVE    "01"                TO  WRK-SYSDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SYSDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1       
      *        剤明細編集
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA             >   5   )  OR
                                  (SRY-SRYCD (IDXA) =   SPACE )
                   MOVE    ZERO                TO  FLG-CHK02
      *            コメント、用法、部位、商品名、自費は対象外
                   IF     (SRY-SRYCD (IDXA)(1:1)   =   "8"  )
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "001")
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "002")
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "008")
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "058")
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "095")
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "096")
      *              OR   (SRY-AUTOKBN(IDXA)       =   "2"
      *                                            OR  "3" )
                       MOVE    1                   TO  FLG-CHK02
                   END-IF
      *            システム予約で診察料以外
                   IF     (SRY-SRYCD (IDXA)(1:3)   =   "099"  )
                       IF     (SRY-SRYCD (IDXA)(1:5)   =   "09911" 
                                                       OR  "09912" )
                           CONTINUE
                       ELSE
                           MOVE    1                   TO  FLG-CHK02
                       END-IF
                   END-IF
      *
                   IF      FLG-CHK02               =   ZERO
      *                点数マスタを取得する
                       MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                金額・点数のみ対象
                       IF     (FLG-TENSU           =   ZERO  )
                       AND    (TNS-TENSIKIBETU     =   1
                                                   OR  2
                                                   OR  3     )
                           CONTINUE
                       ELSE
                           MOVE    1                   TO  FLG-CHK02
                       END-IF
                       IF      FLG-CHK02               =   ZERO
                           IF      (SRY-SRYCD (IDXA)(1:1)  =   "1" )
                            AND    (TNS-KOKUJISKBKBN1  NOT =   1
                                                           AND 3
                                                           AND 5   )
                               MOVE    1                   TO  FLG-CHK02
                           END-IF
                       END-IF
                       IF      FLG-CHK02               =   ZERO
                           PERFORM 200431-SRYACT-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20043-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別名称編集処理
      *****************************************************************
       200431-SRYKBN-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-H-SRYKBN
           IF      SRY-SRYSYUKBN (1:2) =   ACCT-SRYKBN
               MOVE    ACCT-SRYKBN         TO  WRK-H-SRYKBN
           ELSE
               EVALUATE    SRY-SRYSYUKBN(1:2)
                   WHEN    "82"
                       MOVE    "21"            TO  WRK-H-SRYKBN
                   WHEN    "83"
                       MOVE    "83"            TO  WRK-H-SRYKBN
                   WHEN    "84"
                       MOVE    "84"            TO  WRK-H-SRYKBN
                   WHEN    "85"
                       MOVE    "85"            TO  WRK-H-SRYKBN
                   WHEN    "89"
                       MOVE    "89"            TO  WRK-H-SRYKBN
                   WHEN    OTHER
                       MOVE    ACCT-SRYKBN     TO  WRK-H-SRYKBN
               END-EVALUATE
           END-IF
           MOVE    WRK-H-SRYKBN        TO  WRK-SRYSYUKBN(1:2)
           MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           PERFORM 200341-SRYKBN-MEI-SEC
      *
           .
       200431-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為内容編集処理
      *****************************************************************
       200431-SRYACT-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   40
               IF      TBL-SRYKBN (IDX)    =   SPACE
                   MOVE    WRK-SRYKBN          TO  TBL-SRYKBN (IDX)
                   MOVE    WRK-SRYSYUKBNMEI    TO  TBL-SRYKBNMEI (IDX)
               END-IF
               IF      WRK-SRYKBN          =   TBL-SRYKBN (IDX)
                   PERFORM VARYING    IDY      FROM    1   BY  1
                           UNTIL      IDY      >   400
                       IF      TBL-SRYCD (IDX IDY) =   SPACE
                           MOVE    SRY-SRYCD (IDXA)    TO  TBL-SRYCD
                                                               (IDX IDY)
                           MOVE    TNS-NAME            TO  TBL-NAME
                                                               (IDX IDY)
      *H27.8               単位
                           PERFORM 200341-TANINAME-HEN-SEC
                           MOVE    WRK-TANICD          TO  TBL-TANICD
                                                               (IDX IDY)
                           MOVE    WRK-TANINAME        TO  TBL-TANINAME
                                                               (IDX IDY)
      *                    包括区分
                           IF      ACCT-JIHOKBN        =   "1"
                               MOVE    "1"             TO  TBL-HKTKBN
                                                               (IDX IDY)
                           ELSE
                               MOVE    "0"             TO  TBL-HKTKBN
                                                               (IDX IDY)
                           END-IF
                       END-IF
                       IF      TBL-SRYCD (IDX IDY) =   SRY-SRYCD (IDXA)
                           PERFORM 200432-SRYACCT-KAISU-SEC
                           MOVE    400             TO  IDY
                       END-IF
                   END-PERFORM
                   MOVE    40                  TO  IDX
               END-IF
           END-PERFORM
           .
       200431-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量・算定日編集処理
      *****************************************************************
       200432-SRYACCT-KAISU-SEC                SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   ADD     1               TO  TBL-D-DAY (IDX IDY IDX-D)
      *            薬剤・器材は総量集計
                   IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"
                                                   OR  "7" )
                     OR   (SRY-SRYCD (IDXA)(1:3)   =   "059")
      *                数量×回数
                       COMPUTE WRK-GOK-SURYO   =   SRY-SRYSURYO (IDXA)
                                               *   ACCT-DAY (1 IDX-D)
                       COMPUTE TBL-D-SURYO (IDX IDY IDX-D)
                                               =   TBL-D-SURYO
                                                        (IDX IDY IDX-D)
                                               +   WRK-GOK-SURYO
                   END-IF
               END-IF
           END-PERFORM
           .
       200432-SRYACCT-KAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返却内容編集処理
      *****************************************************************
       20044-MDC03RES-MEIHEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  FLG-TEND
           PERFORM 
                   UNTIL    (FLG-TEND      =   1     )
               MOVE    ZERO                TO  IDX-TBL
               MOVE    "99"                TO  WRK-KEY-SRYKBN
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL     (IDX      >   40  )
                   IF     (TBL-SRYKBN (IDX)    NOT =   SPACE   )  AND
                          (TBL-SRYKBN (IDX)        <   WRK-KEY-SRYKBN)
                       MOVE    IDX                 TO  IDX-TBL
                       MOVE    TBL-SRYKBN (IDX)    TO  WRK-KEY-SRYKBN
                   END-IF
               END-PERFORM
               IF      IDX-TBL             =   ZERO
                   MOVE    1               TO  FLG-TEND
               ELSE
                   MOVE    IDX-TBL         TO  IDX
               END-IF
               PERFORM VARYING    IDY      FROM    1   BY  1
                       UNTIL     (IDY      >   400 )
                           OR    (FLG-TEND             =   1     )
                           OR    (TBL-SRYCD (IDX IDY)  =   SPACE )
                   ADD     1                   TO  IDX-T
                   IF      IDX-T               >   400
                       MOVE    1               TO  FLG-TEND
                   ELSE
                       PERFORM 200441-MEISAI-HEN-SEC
                   END-IF
               END-PERFORM
               IF      FLG-TEND        =   ZERO
                   MOVE    SPACE           TO  TBL-SRYKBN (IDX-TBL)
               END-IF
           END-PERFORM
      *
           IF      IDX-T               =   ZERO
               MOVE    "15"            TO  WRK-ERRCD
           END-IF
           .
       20044-MDC03RES-MEIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200441-MEISAI-HEN-SEC                SECTION.
      *
           MOVE    TBL-SRYKBN (IDX)    TO  MDC03RES-MDC-CLASS  (IDX-T)
           MOVE    TBL-SRYKBNMEI(IDX)  TO  MDC03RES-MDC-CLASS-NAME
                                                                (IDX-T)
           MOVE    TBL-SRYCD (IDX IDY) TO  MDC03RES-PRSCRPT-CODE(IDX-T)
           MOVE    TBL-NAME  (IDX IDY) TO  MDC03RES-PRSCRPT-NAME(IDX-T)
      *    単位
           MOVE    TBL-TANICD (IDX IDY)
                                       TO  MDC03RES-UNIT-CODE(IDX-T)
           MOVE    TBL-TANINAME (IDX IDY)
                                       TO  MDC03RES-UNIT-NAME(IDX-T)
      *    包括区分
           MOVE    TBL-HKTKBN(IDX IDY) TO  MDC03RES-MDC-CODE (IDX-T)
      *H27.8
           IF      TBL-HKTKBN(IDX IDY) =   "1"
               MOVE    "True"          TO  MDC03RES-INCLUSION-CLASS
                                                             (IDX-T)
           END-IF
      *
      *    算定回数
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-TBL-DAY-G
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      TBL-D-DAY (IDX IDY IDX-D)  NOT =   ZERO
                   ADD     1               TO  WRK-CNT
                   MOVE    1               TO  WRK-TBL-DAY (IDX-D)
      *            薬剤・器材のみ数量編集
                   IF      TBL-SRYCD (IDX IDY)(1:1)
                                               =   "6"
                                               OR  "7" 
                       ADD     1               TO  IDZ
      *                算定日
                       MOVE    IDX-D           TO  MDC03RES-DAY
                                                         (IDX-T IDZ)
      *                総数量
                       MOVE    TBL-D-SURYO (IDX IDY IDX-D)
                                               TO  WRK-SURYO
                       PERFORM 810-NUMHENKAN-SEC
                       MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC03RES-DAY-PRSCRPT-NUMBER
                                                           (IDX-T IDZ)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-CNT             TO  MDC03RES-MDC-CLASS-NUMBER
                                                                (IDX-T)
           MOVE    WRK-TBL-DAY-G       TO  MDC03RES-MDC-DAY-INF (IDX-T)
      *
           .
       200441-MEISAI-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為剤点数情報処理
      *****************************************************************
       2005-SYORI-04-SEC          SECTION.
      *
      *    保険組合せチェック
           PERFORM 20031-HKNCOMBI-CHK-SEC
           IF     (WRK-HKNCOMBI        =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        保険指定なしの時
               INITIALIZE                      ORCSAPIHKNAREA
      *        当月有効をすべて
               MOVE    "2"                 TO  ORCSAPIHKN-KBN2
               CALL    "ORCSAPIHKN"        USING
                                           SPA-AREA
                                           ORCSAPIHKNAREA
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20051-MDC04RES-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2005-SYORI-04-EXT
           END-IF
      *!
      *    外来とする
      **** MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           INITIALIZE                      TBL-SYORI04-AREA
           MOVE    SPACE               TO  WRK-KEY-HKNCOMBI
      *
      *    診療会計マスターから編集
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SRYKA           TO  ACCT-SRYKA
           MOVE    WRK-PERFORM-YM      TO  ACCT-SRYYM
      *
           IF      WRK-SRYKA           =   SPACE
               MOVE    "key7"          TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-SRYACCT     =   1   )
               MOVE    ZERO                TO  FLG-CHK
      *        薬評剤対象外
               IF      ACCT-ZAIKBN         =   1
                   MOVE    1                   TO  FLG-CHK
               END-IF
      *        保険組合せ判定
               IF      WRK-HKNCOMBI        =   SPACE
      *            包括保険以外対象
                   IF     (ACCT-HKNCOMBI       =   "9999")
                       MOVE    1                   TO  FLG-CHK
                   ELSE
      *                対象の保険判定
                       PERFORM 20052-HKNCOMBI-CHK-SEC
                   END-IF
               ELSE
                   IF     (WRK-HKNCOMBI        =   "9999" )
                       IF     (ACCT-HKNCOMBI       =   "9999")
                          OR  (ACCT-JIHOKBN        =   "1"    )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   ELSE
                       IF      ACCT-HKNCOMBI   NOT =   WRK-HKNCOMBI
                           MOVE    1                   TO  FLG-CHK
                       END-IF
                   END-IF
               END-IF
      *H30.1
      *        レセ電移行分対象外
               IF      WRK-SRYKA           =   SPACE
                   IF     (FLG-RECEDEN         =   1   )
                     AND  (ACCT-SRYKA(1:1)     =   "A" )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
      *
               IF     (FLG-CHK             =   ZERO )
                   PERFORM 20053-SRYACCT-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-KEY-HKNCOMBI    =   SPACE
      *        対象なし
               MOVE    "15"            TO  WRK-ERRCD
               PERFORM 20051-MDC04RES-HEN-SEC
           ELSE
      *        返却内容編集
               PERFORM 20054-MDC04RES-MEIHEN-SEC
           END-IF
           .
       2005-SYORI-04-EXT.
           EXIT.
      *****************************************************************
      *    対象保険組合せ判定処理
      *****************************************************************
       20052-HKNCOMBI-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCHK
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX             >   20  )  OR
                              (FLG-HKNCHK      =   1   )  OR
                              (ORCSAPIHKN-THKNCOMBI(IDX) =   SPACE)
               IF      ORCSAPIHKN-THKNCOMBI(IDX)   =   ACCT-HKNCOMBI
                   IF     (ORCSAPIHKN-HKNNUM (IDX) =   "971"
                                                   OR  "973")
                      OR  (ORCSAPIHKN-HKNNUM (IDX)(1:2) 
                                                   =   "98"  )
      *            労災・自賠責・自費保険は対象外
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   MOVE    1                   TO  FLG-HKNCHK
               END-IF
           END-PERFORM
      *    該当なし
           IF      FLG-HKNCHK          =   ZERO
               INITIALIZE                  HKNCOMBI-REC
               MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
               PERFORM 920-HKNCOMBI-READ-SEC
               IF     (COMB-HKNNUM         =   "971"
                                           OR  "973" )
                  OR  (COMB-HKNNUM (1:2)   =   "98"  )
      *            労災・自賠責・自費保険は対象外
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-IF
           .
       20052-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20051-MDC04RES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PERFORM-DATE1   TO  MDC04RES-PERFORM-DATE
      *
           MOVE    WRK-PTNUM           TO  MDC04RES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  MDC04RES-NAME
           MOVE    PTINF-KANANAME      TO  MDC04RES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDC04RES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDC04RES-SEX
      *    診療科
           MOVE    WRK-SRYKA           TO  MDC04RES-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  MDC04RES-DEP-NAME
      *
      *    保険組合せ情報
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-HKNCOMBI    TO  MDC04RES-COMBINATION-NUMBER
                                                                 (1)
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                       TO  MDC04RES-MAIN-INSURANCE-CLASS
                                                                 (1)
      *        保険者番号
               MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                   TO  MDC04RES-MAIN-INSURANCE-NUMBER(1)
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                       TO  MDC04RES-MAIN-INSURANCE-NAME
                                                                 (1)
      *        記号
               MOVE    MDCREQ-MAIN-MARK    TO  MDC04RES-MAIN-MARK (1)
      *        番号
               MOVE    MDCREQ-MAIN-NUMBER  TO  MDC04RES-MAIN-NUMBER (1)
      *        公費情報
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL       IDK     >   4
      *            公費種類
                   MOVE    MDCREQ-INSURANCE-CLASS (IDK)
                                       TO  MDC04RES-INSURANCE-CLASS
                                                              (1 IDK)
      *            公費名称
                   MOVE    MDCREQ-INSURANCE-NAME  (IDK)
                                       TO  MDC04RES-INSURANCE-NAME
                                                              (1 IDK)
      *            負担者番号
                   MOVE    MDCREQ-PROVIDER-NUMBER(IDK)
                                       TO  MDC04RES-PROVIDER-NUMBER
                                                              (1 IDK)
      *            受給者番号
                   MOVE    MDCREQ-RECIPIENT-NUMBER (IDK)
                                       TO  MDC04RES-RECIPIENT-NUMBER
                                                              (1 IDK)
               END-PERFORM
          END-IF
      *
           .
       20051-MDC04RES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容処理
      *****************************************************************
       20053-SRYACCT-HENSYU-SEC         SECTION.
      *
      *    保険判定
           IF      WRK-KEY-HKNCOMBI    NOT =   ACCT-HKNCOMBI
               PERFORM 200531-HKNCOMBI-HEN-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-RSI-MONEY
           MOVE    ZERO                TO  WRK-MONEY
           MOVE    SPACE               TO  WRK-SRY-SRYSYUKBN
      *    労災・自賠責の時、円分を減算、自費の金額編集
           IF    ((FLG-HKNKBN          =   1    )  AND
                  (ACCT-SRYKBN         =   "11"
                                       OR  "12"
                                       OR  "13"
                                       OR  "80"  )  AND
                  (ACCT-ZAIREQFLG      =   "1"   ))
            OR   ((FLG-HKNKBN          =   2    )  AND
                  (ACCT-SRYKBN         =   "80" )  AND
                  (ACCT-ZAIREQFLG      =   "1"   ))
            OR   ( ACCT-SRYKBN         =   "96"
                                       OR  "95"  )
               PERFORM 20053-RSI-ZAITEN-SEC
           ELSE
      *        診療種別取得
               IF      ACCT-SRYKBN     =   "80"
                   PERFORM 20054-SRYACT-GET-SEC
                   MOVE    SRY-SRYSYUKBN       TO  WRK-SRY-SRYSYUKBN
               END-IF
           END-IF
      *
           IF      FLG-CHK2             =  ZERO
      *        逓減点数を判定する
               INITIALIZE                  SRYACCTPLUS-REC
               IF      ACCT-ZAIREQFLG      =   "1"
      *            附加情報あり
                   PERFORM 32012-SRYACCTPLUS-READ-SEC
               END-IF
               IF      ACCTP-PLUSKBN        =   1
                    COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                                *   -1
               ELSE
                   MOVE    ACCT-ZAITEN           TO  WRK-ZAITEN
               END-IF
           END-IF
           IF     (WRK-ZAITEN          =   ZERO )
             AND  (WRK-RSI-MONEY       =   ZERO )
             AND  (WRK-MONEY           =   ZERO )
               GO      TO      20053-SRYACCT-HENSYU-EXT
           END-IF
      *    診療種別名称編集処理
           MOVE    SPACE               TO  WRK-H-SRYKBN
           MOVE    ACCT-SRYKBN         TO  WRK-SRYKBN
           IF      ACCT-SRYKBN         =   "80"
               EVALUATE    WRK-SRY-SRYSYUKBN(1:2)
                   WHEN    "82"
                       MOVE    "82"            TO  WRK-H-SRYKBN
                   WHEN    "83"
                       MOVE    "83"            TO  WRK-H-SRYKBN
                   WHEN    "84"
                       MOVE    "84"            TO  WRK-H-SRYKBN
                   WHEN    "85"
                       MOVE    "85"            TO  WRK-H-SRYKBN
                   WHEN    "89"
                       MOVE    "89"            TO  WRK-H-SRYKBN
                   WHEN    OTHER
                       MOVE    ACCT-SRYKBN     TO  WRK-H-SRYKBN
               END-EVALUATE
           ELSE
               MOVE    ACCT-SRYKBN     TO  WRK-H-SRYKBN
           END-IF
           MOVE    WRK-H-SRYKBN        TO  WRK-SRYSYUKBN(1:2)
           MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           PERFORM 200341-SRYKBN-MEI-SEC
      *
      *    集計先テーブル
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   31 )
               IF      ACCT-DAY (1 IDX1)   >   ZERO
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   40 )
                       IF      TBL-S4-SRYKBN (IDX1 IDX2)   =   SPACE
                           MOVE    WRK-SRYKBN      TO  TBL-S4-SRYKBN
                                                            (IDX1 IDX2)
                           MOVE    WRK-SRYSYUKBN   TO  TBL-S4-SRYSYUKBN
                                                            (IDX1 IDX2)
                           MOVE    WRK-SRYSYUKBNMEI
                                                   TO  TBL-S4-SRYKBNMEI
                                                            (IDX1 IDX2)
                       END-IF
                       IF    ( WRK-SRYKBN      =   TBL-S4-SRYKBN
                                                            (IDX1 IDX2))
                         AND ( WRK-SRYSYUKBN   =   TBL-S4-SRYSYUKBN
                                                            (IDX1 IDX2))
                           ADD     1           TO  TBL-S4-IDX(IDX1 IDX2)
                           MOVE    TBL-S4-IDX(IDX1 IDX2)    TO  IDX3
                           IF      IDX3                >   100
                                MOVE    100            TO  IDX3
                                MOVE    100            TO  TBL-S4-IDX
                                                            (IDX1 IDX2)
                           END-IF
                           MOVE    WRK-ZAITEN      TO  TBL-S4-ZAITEN
                                                        (IDX1 IDX2 IDX3)
                           IF      WRK-RSI-MONEY       >   ZERO
                               MOVE    WRK-RSI-MONEY   TO  TBL-S4-MOENY
                                                        (IDX1 IDX2 IDX3)
                           END-IF
                           IF      WRK-MONEY           >   ZERO
                               MOVE    WRK-MONEY       TO  TBL-S4-MOENY
                                                        (IDX1 IDX2 IDX3)
                           END-IF
                           MOVE    ACCT-DAY (1 IDX1)   TO  TBL-S4-KAISU
                                                        (IDX1 IDX2 IDX3)
                           MOVE    ACCT-JIHOKBN    TO  TBL-S4-JIHOKBN
                                                        (IDX1 IDX2 IDX3)
                           MOVE    40              TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20053-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ決定処理
      *****************************************************************
       200531-HKNCOMBI-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNKBN
           MOVE    ACCT-HKNCOMBI       TO  WRK-KEY-HKNCOMBI
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   20  )
               IF      TBL-S4-HKNCOMBI(IDX)    =   SPACE
                   MOVE    ACCT-HKNCOMBI       TO  TBL-S4-HKNCOMBI(IDX)
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
                   MOVE    ACCT-PTID           TO  COMB-PTID
                   MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
                   PERFORM 920-HKNCOMBI-READ-SEC
                   IF     (COMB-HKNNUM         =   "971"
                                               OR  "973" )
                       MOVE    "1"             TO  TBL-S4-HKNKBN (IDX)
                   END-IF
                   IF     (COMB-KOH1HKNNUM     =   "970" )
                       MOVE    "2"             TO  TBL-S4-HKNKBN (IDX)
                   END-IF
               END-IF
               IF      TBL-S4-HKNCOMBI(IDX)    =   ACCT-HKNCOMBI
                   MOVE    TBL-S4-HKNKBN (IDX) TO  FLG-HKNKBN
                   MOVE    20                  TO  IDX
               END-IF
           END-PERFORM
           .
       200531-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読み込み処理
      *****************************************************************
       20054-SRYACT-GET-SEC                SECTION.
      *
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
               INITIALIZE                      SRYACT-REC
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20054-SRYACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    労災・自費金額編集処理
      *****************************************************************
       20053-RSI-ZAITEN-SEC                SECTION.
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SYSYM
           MOVE    "01"                TO  WRK-SYSDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SYSDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  WRK-SUM-TEN
           MOVE    ZERO                TO  WRK-MONEY
           MOVE    ZERO                TO  WRK-RSI-MONEY
      *    自費コードの自費金額
           IF     (SRY-SRYKBN          =   "95"
                                       OR  "96" )
               MOVE    SRY-JIHIMONEYTOTAL  TO  WRK-MONEY
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRY-SRYSYUKBN
      *
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA             >   5   )  OR
                                  (SRY-SRYCD (IDXA) =   SPACE )
      *            点数マスタ検索
                   IF      (SRY-SRYCD (IDXA)(1:3)  =   "101"  )  OR
                           (SRY-SRYCD (IDXA)(1:5)  =   "09591" OR
                                                       "09592" OR
                                                       "09593" )
                       MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                労災の金額算定分の時、点数をゼロとする
                       IF     (TNS-TENSIKIBETU     =   1  )  OR
                              (TNS-SRYCD (1:5)     =   "09591" OR
                                                       "09592" OR
                                                       "09593" )
      *                自賠責金額入力を点数とする
                           IF     (SRY-SRYSYUKBN       =   "809")
                              MOVE    SRY-JIHIMONEY(IDZ)   TO  TNS-TEN
                           END-IF
                           COMPUTE WRK-SUM-TEN     =   WRK-SUM-TEN
                                                   +  (TNS-TEN
                                                   /   10  )
                           COMPUTE WRK-RSI-MONEY   =   WRK-RSI-MONEY
                                                   +   TNS-TEN
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      ACCT-ZAITEN         >=  WRK-SUM-TEN
               COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                           -   WRK-SUM-TEN
           ELSE
               MOVE    ZERO                TO  WRK-ZAITEN
           END-IF
      *    労災円分
           IF      WRK-RSI-MONEY       >   ZERO
               MOVE      1                 TO  FLG-CHK2
           END-IF
      *    自費分
           IF      WRK-MONEY           >   ZERO
               MOVE      1                 TO  FLG-CHK2
               MOVE    ZERO                TO  WRK-ZAITEN
           END-IF
           .
       20053-RSI-ZAITEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    返却内容編集処理
      *****************************************************************
       20054-MDC04RES-MEIHEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      TBL-S4-SRYKBN (IDX-D 1) NOT =   SPACE
                   ADD     1               TO  IDX1
      *            診療区分順に編集
                   PERFORM 200541-SRYKBN-HEN-SEC
               END-IF
           END-PERFORM
      *    対象なし
           IF      IDX1                =   ZERO
               MOVE    "15"            TO  WRK-ERRCD
           END-IF
      *    保険組合せ編集
           PERFORM 200543-HKNCOMBI-HEN-SEC
           .
       20054-MDC04RES-MEIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200541-SRYKBN-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  FLG-TEND
           MOVE    ZERO                TO  IDX2
           PERFORM 
                   UNTIL    (FLG-TEND      =   1     )
               MOVE    ZERO                TO  IDX-TBL
               MOVE    "99"                TO  WRK-KEY-SRYKBN
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL     (IDX      >   40  )
                   IF     (TBL-S4-SRYKBN (IDX-D IDX)   NOT =   SPACE )
                   AND    (TBL-S4-SRYKBN (IDX-D IDX)
                                                   <   WRK-KEY-SRYKBN)
                       MOVE    IDX                 TO  IDX-TBL
                       MOVE    TBL-S4-SRYKBN (IDX-D IDX)
                                                   TO  WRK-KEY-SRYKBN
                   END-IF
               END-PERFORM
               IF      IDX-TBL             =   ZERO
                   MOVE    1               TO  FLG-TEND
               ELSE
                   MOVE    IDX-TBL         TO  IDX
                   ADD     1               TO  IDX2
                   PERFORM 2005411-MEISAI-HEN01-SEC
               END-IF
               PERFORM VARYING    IDY      FROM    1   BY  1
                       UNTIL     (IDY      >   100 )
                           OR    (FLG-TEND =   1    )
                           OR    (IDY      >   TBL-S4-IDX (IDX-D IDX))
                   ADD     1                   TO  IDX3
                   IF      IDX3                >   400
                       MOVE    1               TO  FLG-TEND
                   ELSE
                       PERFORM 2005412-MEISAI-HEN02-SEC
                   END-IF
               END-PERFORM
               IF      FLG-TEND        =   ZERO
                   MOVE    SPACE           TO  TBL-S4-SRYKBN (IDX-D IDX)
               END-IF
           END-PERFORM
      *
           .
       200541-SRYKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2005411-MEISAI-HEN01-SEC        SECTION.
      *
           MOVE    IDX-D               TO  MDC04RES-MDC-DAY (IDX1)
      *
           MOVE    TBL-S4-SRYKBN (IDX-D IDX)
                                       TO  MDC04RES-MDC-CLASS
                                                       (IDX1 IDX2)
           MOVE    TBL-S4-SRYSYUKBN (IDX-D IDX)
                                       TO  MDC04RES-MDC-CLASS2
                                                       (IDX1 IDX2)
           MOVE    TBL-S4-SRYKBNMEI (IDX-D IDX)
                                       TO  MDC04RES-MDC-CLASS-NAME
                                                       (IDX1 IDX2)
           .
       2005411-MEISAI-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2005412-MEISAI-HEN02-SEC                SECTION.
      *
           IF      TBL-S4-ZAITEN (IDX-D IDX IDY)   NOT =   ZERO
               MOVE    TBL-S4-ZAITEN (IDX-D IDX IDY)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC04RES-MDC-POINT
                                                     (IDX1 IDX2 IDX3)
           END-IF
      *
           IF      TBL-S4-MOENY (IDX-D IDX IDY)    NOT =   ZERO
               MOVE    TBL-S4-MOENY (IDX-D IDX IDY)    TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDC04RES-MDC-MOENY
                                                     (IDX1 IDX2 IDX3)
           END-IF
      *
           MOVE    TBL-S4-KAISU (IDX-D IDX IDY)    TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                      TO MDC04RES-MDC-CLASS-NUMBER
                                                     (IDX1 IDX2 IDX3)
      *    包括区分
           IF      TBL-S4-JIHOKBN (IDX-D IDX IDY)  =   "1"
               MOVE    TBL-S4-JIHOKBN (IDX-D IDX IDY)
                                       TO  MDC04RES-MDC-CODE
                                                     (IDX1 IDX2 IDX3)
               MOVE    "True"          TO  MDC04RES-INCLUSION-CLASS
                                                     (IDX1 IDX2 IDX3)
           END-IF
      *
           .
       2005412-MEISAI-HEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ内容編集処理
      *****************************************************************
       200543-HKNCOMBI-HEN-SEC               SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )
                         OR   (TBL-S4-HKNCOMBI (IDX)   =   SPACE)
                IF     TBL-S4-HKNCOMBI (IDX)   =   "9999"
      *
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    "包括分入力"        TO  COMB-SYU-TANSEIDONAME
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    SPA-PTID            TO  COMB-PTID
                   MOVE    TBL-S4-HKNCOMBI (IDX)
                                               TO  COMB-HKNCOMBINUM
                   PERFORM 920-HKNCOMBI-READ-SEC
               END-IF
               MOVE    TBL-S4-HKNCOMBI (IDX)
                                       TO  MDC04RES-COMBINATION-NUMBER
                                                            (IDX)
      *        主保険−保険番号
               MOVE    COMB-HKNNUM     TO  MDC04RES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *        主保険−短縮制度名
               MOVE    COMB-SYU-TANSEIDONAME
                                       TO  MDC04RES-MAIN-INSURANCE-NAME
                                                            (IDX)
               IF      COMB-HKNID      NOT =   ZERO
      *            保険情報編集
                   PERFORM 930-PTHKNINF-READ-SEC
      *            保険者番号
                   MOVE    PTHKN-HKNJANUM  TO
                                       MDC04RES-MAIN-INSURANCE-NUMBER
                                                            (IDX)
      *            記号・番号
                   MOVE    PTHKN-KIGO      TO  MDC04RES-MAIN-MARK  (IDX)
                   MOVE    PTHKN-NUM       TO  MDC04RES-MAIN-NUMBER(IDX)
               END-IF
      *        公費情報
               PERFORM VARYING    IDK      FROM    1   BY  1
                       UNTIL      IDK      >   4
                   IF      COMB-KOHID (IDK)    NOT =   ZERO
      *                公費の種類
                       MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  MDC04RES-INSURANCE-CLASS
                                                            (IDX IDK)
                       MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  MDC04RES-INSURANCE-NAME
                                                            (IDX IDK)
      *                公費情報編集
                       PERFORM 940-PTKOHINF-READ-SEC
      *                負担者番号
                       MOVE    PTKOH-FTNJANUM
                                           TO  MDC04RES-PROVIDER-NUMBER
                                                            (IDX IDK)
                       MOVE    PTKOH-JKYSNUM
                                           TO  MDC04RES-RECIPIENT-NUMBER
                                                            (IDX IDK)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       200543-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
      ***      MOVE    "患者番号未設定"
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "02"
      ***      MOVE    "診療科未設定"
               MOVE    "診療科の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
      ***      MOVE    "該当患者番号なし"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
      ***      MOVE    "診療日設定誤り"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
      *****    MOVE    "保険組合せ番号誤り"
               MOVE    "保険組合せ番号の設定が違います"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "対象が１５０件以上存在します。"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "対象がありません"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "月数は０〜１２、９９を設定して下さい。"
                                               TO  WRK-ERRMSG
           WHEN    "20"
     ******    MOVE    "保険組合せがありません"
               MOVE    "該当する保険組合せがありません"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "対象の保険組合せが５件以上あります。"
                                               TO  WRK-ERRMSG
      *H29.8
           WHEN    "22"
               MOVE    "伝票番号エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "伝票番号の受診履歴が送信内容と一致しません"
                                               TO  WRK-ERRMSG
      *H30.1
           WHEN    "24"
               STRING  "レセ電データ移行の診療科です。"
                       "レセ電データは対象外となっています。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
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
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC              SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       930-PTHKNINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID 
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       940-PTKOHINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDK)    TO  PTKOH-KOHID 
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       32012-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       32012-SRYACCTPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF     (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
      *R02.2
      *        ユーザー単位反映
               IF     (FLG-TENSUPLUS       =   ZERO)
                  AND (TNS-SRYCD(1:1)      =   "6"
                                           OR  "7" )
                  AND (TNSP-USERTANICD NOT =   SPACE)
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SYSYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SYSYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
