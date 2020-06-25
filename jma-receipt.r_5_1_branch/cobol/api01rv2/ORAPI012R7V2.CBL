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
       PROGRAM-ID.         ORAPI012R7V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者　メモ内容取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/11/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-PTMEMOINF           PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-NEXT                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDX-U                   PIC 9(04).
           03  IDX-U2                  PIC 9(04).
           03  IDH                     PIC 9(04).
           03  IDH2                    PIC 9(04).
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
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-HEN-SRYKA           PIC X(02).
           03  WRK-HEN-SRYKA-NAME      PIC X(80).
      *
           03  WRK-CNT                 PIC 9(03).
      *
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-BASE-DATE           PIC X(10).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-MEMOKBN             PIC X(01).
      *
      *    受付
       01  TBL-UKETUKE-AREA.
           02  TBL-UKETUKE-REC         OCCURS   10.
           COPY    "CPUKETUKE.INC"   REPLACING
                                     //UKE-// BY //TBL-UKE-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張）
      **   COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
      **   COPY    "CPORCSSTRING.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPPATIENTLST7V2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPPATIENTLST7V2RES.INC".
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療科
           COPY  "CPSK1005.INC".
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    患者メモタ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
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
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "********************"
           DISPLAY   "* patient res start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
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
               MOVE   404                  TO  APILST43-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* patient res end *"
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
           INITIALIZE                      XML-PATIENTLST7RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  PTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PTRES-INFORMATION-TIME
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
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  PTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    PTREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    PTREQ-REQUEST-NUMBER
           WHEN    "01"
      *        メモ取得処理
               PERFORM 2001-PTMEMOINF-MEMO-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST43-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
                ",APILST43-BODY:"  APILST43-BODY
           END-IF
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-PATIENTLST7REQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst7v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST43-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst7req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO      TO      1000-APTLSTREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_patientlst7v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST43-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst7req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-PATIENTLST7REQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *****************************************************************
      *    設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    患者番号チェック
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-PTNUM       NOT =   SPACE )
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    基準日付チェック
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-BASE-YMD    NOT =   SPACE )
               PERFORM 20011-BASEYMD-CHK-SEC
           END-IF
      *    診療科チェック
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-SRYKA       NOT =   SPACE )
               PERFORM 20011-SRYKA-CHK-SEC
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
           MOVE    PTREQ-PATIENTID     TO  WRK-PTNUM
           MOVE    PTREQ-PATIENTID     TO  PTRES-PATIENTID
      *    基準日付
           MOVE    PTREQ-BASE-DATE     TO  PTRES-BASE-DATE
           IF      PTREQ-BASE-DATE     =   SPACE
      *        未設定はシステム日付
               MOVE    SPA-SYSYMD          TO  WRK-BASE-YMD
           ELSE
               MOVE    PTREQ-BASE-DATE     TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-BASE-YMD
           END-IF
      *    診療科
           MOVE    PTREQ-SRYKA        TO  PTRES-SRYKA (1)
           MOVE    PTREQ-SRYKA        TO  WRK-SRYKA
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
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
           MOVE    WRK-PTNUM           TO  PTRES-PATIENTID
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
      *    基準日付チェック処理
      *****************************************************************
       20011-BASEYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-BASE-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        基準日を編集
               MOVE    WRK-BASE-YMD        TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-BASE-DATE
      *
               MOVE    WRK-BASE-YMD        TO  SPA-SYSYMD
           END-IF
      *
           .
       20011-BASEYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
           IF      WRK-SRYKA           =   "00"
               MOVE    "全科"          TO  WRK-SRYKA-NAME
               GO      TO      20011-SRYKA-CHK-EXT
           END-IF
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "12"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険組合せ一覧処理
      *****************************************************************
       2001-PTMEMOINF-MEMO-SEC          SECTION.
      *
      *    患者情報編集
           PERFORM 20021-PTINF-HEN-SEC
      *
      *    受付編集
           PERFORM 20011-UKETUKE-HEN-SEC
      *
           IF      PTREQ-MEMO-KBN      =   "1"
               MOVE    "1"                 TO  WRK-MEMOKBN
           ELSE
               MOVE    "2"                 TO  WRK-MEMOKBN
           END-IF
      *    患者メモ検索
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-BASE-YMD        TO  PTMEMOINF-SYSYMD
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    WRK-SRYKA           TO  PTMEMOINF-SRYKA
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-NEXT
           PERFORM UNTIL       (FLG-PTMEMOINF  =   1 )
                          OR   (FLG-NEXT       =   1 )
               ADD     1               TO  IDX
               IF      IDX             >   10
                   MOVE    1               TO  FLG-NEXT
               ELSE
                   PERFORM 20021-PTMEMOINF-SPAHEN-SEC
               END-IF
      *
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "13"            TO  WRK-ERRCD
               IF      PTREQ-MEMO-KBN      =   "1"
                   MOVE    "14"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      FLG-NEXT            =   1
               MOVE    "15"            TO  WRK-ERRCD
               IF      PTREQ-MEMO-KBN      =   "1"
                   MOVE    "16"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2001-PTMEMOINF-MEMO-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-PTINF-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  PTRES-NAME
           MOVE    PTINF-KANANAME      TO  PTRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  PTRES-SEX
      *
           .
       20021-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付編集処理
      *****************************************************************
       20011-UKETUKE-HEN-SEC      SECTION.
      *
           INITIALIZE                  TBL-UKETUKE-AREA
           MOVE    ZERO                TO  IDX-U
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    WRK-BASE-YMD        TO  UKE-UKEYMD
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           PERFORM UNTIL       (FLG-UKETUKE    =   1  )
                          OR   (IDX-U          >=  10 )
               IF     (UKE-UKEID         >   ZERO    )   AND
                      (UKE-KAIKEITIME    =   ZERO    )
                   ADD     1             TO  IDX-U
                   MOVE    UKETUKE-REC   TO  TBL-UKETUKE-REC
                                                    (IDX-U)
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20011-UKETUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ内容編集処理
      *****************************************************************
       20021-PTMEMOINF-SPAHEN-SEC      SECTION.
      *
           MOVE    PTMEMOINF-SRYKA     TO  PTRES-SRYKA (IDX)
           MOVE    PTMEMOINF-SRYKA     TO  WRK-HEN-SRYKA
           PERFORM 200211-SRYKA-HEN-SEC
           MOVE    WRK-HEN-SRYKA-NAME  TO  PTRES-SRYKA-MEI
                                                       (IDX)
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    PTMEMOINF-MEMO      TO  PTRES-MEMO  (IDX)
      *
      *    受付存在チェック
           PERFORM VARYING     IDX-U2  FROM    1   BY  1
                   UNTIL      (IDX-U2      >   IDX-U )
               IF      (PTMEMOINF-SRYKA        =   "00" )
                   OR ((PTMEMOINF-SRYKA    NOT =   "00" )  AND
                       (PTMEMOINF-SRYKA        =   TBL-UKE-SRYKA
                                                      (IDX-U2)))
                   MOVE    TBL-UKE-HKNCOMBI (IDX-U2)
                                           TO  WRK-HKNCOMBI
                   PERFORM 200211-HKNCOMBI-HEN-SEC
      *            受付診療科
                   IF      PTMEMOINF-SRYKA     =   "00"
                       MOVE    TBL-UKE-SRYKA (IDX-U2)
                                           TO  PTRES-UKE-SRYKA
                                                          (IDX)
                       MOVE    TBL-UKE-SRYKA (IDX-U2)
                                               TO  WRK-HEN-SRYKA
                       PERFORM 200211-SRYKA-HEN-SEC
                       MOVE    WRK-HEN-SRYKA-NAME
                                               TO  PTRES-UKE-SRYKA-MEI
                                                       (IDX)
                   END-IF
      *
                   MOVE    IDX-U           TO  IDX-U2
               END-IF
           END-PERFORM
           .
       20021-PTMEMOINF-SPAHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コード編集処理
      *****************************************************************
       200211-SRYKA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-HEN-SRYKA-NAME
           IF      WRK-HEN-SRYKA       =   "00"
               MOVE    "全科"          TO  WRK-HEN-SRYKA-NAME
           ELSE
      *        診療科の存在チェック
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    WRK-HEN-SRYKA       TO  SYS-1005-KBNCD
               MOVE    WRK-BASE-YMD        TO  SYS-1005-STYUKYMD
               MOVE    WRK-BASE-YMD        TO  SYS-1005-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *        システム管理検索
               MOVE    SYS-1005-REC        TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-SEL-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1 TO  WRK-HEN-SRYKA-NAME
               END-IF
           END-IF
           .
       200211-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200211-HKNCOMBI-HEN-SEC              SECTION.
      *
      *    保険組合せ番号
           MOVE    WRK-HKNCOMBI        TO  PTRES-COMBINATION-NUMBER
                                                              (IDX)
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    指定保険組合せのみ編集
           MOVE    "3"                 TO  ORCSAPIHKN-KBN
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
           IF      ORCSAPIHKN-ERRCD    =   ZERO
               MOVE    1               TO  IDH
               IF      ORCSAPIHKN-THKNCOMBI (IDH)
                                       =   WRK-HKNCOMBI
      *            保険組合せ内容編集
                   PERFORM 2002111-HKNCOMBI-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200211-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002111-HKNCOMBI-HENSYU-SEC     SECTION.
      *
           MOVE    ORCSAPIHKN-THKNCOMBI (IDH)
                                   TO  PTRES-COMBINATION-NUMBER (IDX)
      *    非表示区分
           MOVE    ORCSAPIHKN-HYOJIKBN (IDH)
                                   TO  PTRES-COMBINATION-HYOJI (IDX)
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDH)
                                   TO  PTRES-MAIN-INSURANCE-CLASS
                                                       (IDX)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDH)
                                   TO  PTRES-MAIN-INSURANCE-NUMBER
                                                       (IDX)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDH)
                                   TO  PTRES-MAIN-INSURANCE-NAME
                                                       (IDX)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDH)
                                   TO  PTRES-MAIN-MARK
                                                       (IDX)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDH)
                                   TO  PTRES-MAIN-NUMBER
                                                       (IDX)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDH)
                                   TO  PTRES-MAIN-CONTINUATION
                                                       (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDH)
                                   TO  PTRES-MAIN-ASSISTANCE
                                                       (IDX)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDH)
                                   TO  PTRES-MAIN-FAMILY
                                                       (IDX)
      *    被保険名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDH)
                                   TO  PTRES-MAIN-POLICYHOLDER
                                                       (IDX)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDH)
                                   TO  PTRES-MAIN-START-DATE
                                                       (IDX)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDH)
                                   TO  PTRES-MAIN-END-DATE
                                                       (IDX)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDH IDY2)
                                       TO  PTRES-INSURANCE-CLASS
                                                       (IDX IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDH IDY2)
                                       TO  PTRES-INSURANCE-NAME
                                                       (IDX IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDH IDY2)
                                       TO  PTRES-PROVIDER-NUMBER
                                                       (IDX IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDH IDY2)
                                       TO  PTRES-RECIPIENT-NUMBER
                                                       (IDX IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDH IDY2)
                                       TO  PTRES-INSURANCE-START-DATE
                                                       (IDX IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDH IDY2)
                                       TO  PTRES-INSURANCE-END-DATE
                                                       (IDX IDY2)
           END-PERFORM
           .
       2002111-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
               IF      WRK-KEICD       NOT =   SPACE
                   MOVE    "K"             TO  WRK-ERRKBN
               END-IF
           ELSE
               MOVE    "E"                 TO  WRK-ERRKBN
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  PTRES-API-RESULT
               MOVE    "処理終了"          TO  PTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD-G         TO  PTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST43-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst7v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTRES-MODE
           MOVE    "patientlst7res"    TO  PTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST7RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_patientlst7v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTRES-MODE
           MOVE    "patientlst7res"    TO  PTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST7RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
      *        CONTINUE
               MOVE    PTRES-OBJECT    TO  APILST43-BODY 
               MOVE    "application/xml"   TO  APILST43-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
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
      *
      *****************************************************************
      *    日付変換編集処理（年月）
      *****************************************************************
       802-DAYHEN022-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD(1:6)       =   "999912"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN022-EXT.
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
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "基準日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "メモ２の登録はありませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "メモ１の登録はありませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "メモ２の登録が１０件以上あります"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "メモ１の登録が１０件以上あります"
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
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
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
      *    患者番号マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
               MOVE    MCPDATA-REC         TO  PTNUM-REC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                      PTNUM-REC
           END-IF
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
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
      *
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付読込処理
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
               MOVE    ZERO                TO  FLG-UKETUKE
           ELSE
               INITIALIZE                      UKETUKE-REC
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
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
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1                   TO  FLG-SYSKANRI
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
