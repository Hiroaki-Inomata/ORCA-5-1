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
       PROGRAM-ID.         ORAPI012R6V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者　保険組合せ取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/04/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/06/XX  公費確認日付追加
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
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-NYUIN               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
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
           03  WRK-ZZ9                 PIC ZZ9.
      *
           03  WRK-CNT                 PIC 9(03).
           03  WRK-ERR-CNT             PIC 9(03).
      *
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-START-YMD.
               05  WRK-START-YM            PIC X(06).
               05  WRK-START-DD            PIC X(02).
           03  WRK-END-YMD.
               05  WRK-END-YM              PIC X(06).
               05  WRK-END-DD              PIC X(02).
           03  WRK-BASE-YMD            PIC X(08).
      *
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
      *    COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張）
      **   COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
      **   COPY    "CPORCSSTRING.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPPATIENTLST6V2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPPATIENTLST6V2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    全保険組合せ取得領域
           COPY    "CPORCSAPIHKNALL.INC".
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
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
               MOVE   404                  TO  APILST31-HTTPSTATUS
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
           INITIALIZE                      XML-PATIENTLST6RES
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
      *        患者保険組合せ処理
               PERFORM 2002-HKNCOMBI-LIST-SEC
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
           IF      APILST31-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
                ",APILST31-BODY:"  APILST31-BODY
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst6v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST31-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst6req"    TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientlst6v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST31-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst6req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-PATIENTLST6REQ
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
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    基準日付チェック
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-BASE-YMD    NOT =   SPACE )
               PERFORM 20011-BASEYMD-CHK-SEC
           END-IF
      *    開始年月チェック
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-START-YM    NOT =   SPACE )
               PERFORM 20011-STARTDATE-CHK-SEC
           END-IF
      *    終了年月チェック
           IF     (WRK-END-YM          =   SPACE
                                       OR  "999999"  )
               CONTINUE
           ELSE
               IF     (WRK-ERRCD           =   SPACE )
                   PERFORM 20011-ENDDATE-CHK-SEC
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
           MOVE    PTREQ-PATIENTID     TO  WRK-PTNUM
      *    基準日付
           IF      PTREQ-BASE-DATE     =   SPACE
               MOVE    SPACE               TO  WRK-BASE-YMD
           ELSE
               MOVE    PTREQ-BASE-DATE     TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-BASE-YMD
           END-IF
      *    開始年月
           IF      PTREQ-START-DATE    =   SPACE
               MOVE    SPACE               TO  WRK-START-YM
           ELSE
               MOVE    PTREQ-START-DATE    TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN022-SEC
               MOVE    WRK-SYMD(1:6)       TO  WRK-START-YM
           END-IF
      *    終了年月
           IF      PTREQ-END-DATE      =   SPACE
               MOVE    SPACE               TO  WRK-END-YM
           ELSE
               MOVE    PTREQ-END-DATE      TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN022-SEC
               MOVE    WRK-SYMD(1:6)       TO  WRK-END-YM
           END-IF
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
           .
       20011-BASEYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始年月チェック
      *****************************************************************
       20011-STARTDATE-CHK-SEC    SECTION.
      *
           MOVE    "01"                TO  WRK-START-DD
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-START-YMD       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-STARTDATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了年月チェック
      *****************************************************************
       20011-ENDDATE-CHK-SEC           SECTION.
      *
           MOVE    "01"                TO  WRK-END-DD
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-END-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
      *    開始日とのチェック
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-START-YM    >   WRK-END-YM
                   MOVE    "14"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20011-ENDDATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険組合せ一覧処理
      *****************************************************************
       2002-HKNCOMBI-LIST-SEC          SECTION.
      *
      *    患者情報編集
           PERFORM 20021-PTINF-HEN-SEC
      *
      *    保険組合せ編集処理
           MOVE    ZERO                TO  IDX
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
           .
       2002-HKNCOMBI-LIST-EXT.
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
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC              SECTION.
      *
           INITIALIZE                  ORCSAPIHKNALLAREA
      *
           IF      WRK-BASE-YMD        =   SPACE
      *        開始年月・終了年月編集
               MOVE    WRK-START-YM        TO  ORCSAPIHKN-START-YM
               MOVE    WRK-END-YM          TO  ORCSAPIHKN-END-YM
           ELSE
      *        基準日があれば、基準日のみを対象
               MOVE    "1"                 TO  ORCSAPIHKN-KBN
               MOVE    WRK-BASE-YMD        TO  ORCSAPIHKN-SYSYMD
      *        生年月日（処理区分＝１のみ）
               MOVE    PTINF-BIRTHDAY      TO  ORCSAPIHKN-BIRTHDAY
           END-IF
           CALL    "ORCSAPIHKNALL"     USING
                                       SPA-AREA
                                       ORCSAPIHKNALLAREA
      *
      *    出力領域保険情報編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200  )
                         OR   (IDX     >   ORCSAPIHKN-OT-MAX)
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
      *        保険組合せ内容編集
               PERFORM 200231-HKNCOMBI-HENSYU-SEC
           END-PERFORM
           IF      ORCSAPIHKN-OT-MAX   =   ZERO
      *        対象なし
               MOVE    "20"                TO  WRK-ERRCD
           ELSE
      *        対象オーバー
               IF      ORCSAPIHKN-ERRCD    =   1
                   MOVE    "21"            TO  WRK-KEICD
               END-IF
           END-IF
      *
           .
       20023-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200231-HKNCOMBI-HENSYU-SEC              SECTION.
      *
      *    保険組合せ
           MOVE    ORCSAPIHKN-THKNCOMBI(IDX)
                                   TO  PTRES-COMB-NUMBER (IDX)
      *    保険組合せ負担割合（外来）
           MOVE    ORCSAPIHKN-COMB-RATEG(IDX)
                                   TO PTRES-COMB-RATE-OUT (IDX)
      *    保険組合せ負担割合（入院）
           MOVE    ORCSAPIHKN-COMB-RATEN(IDX)
                                   TO PTRES-COMB-RATE-ADM (IDX)
      *    保険組合せ有効期間
           MOVE    ORCSAPIHKN-COMB-TEKSTYMD(IDX)
                                   TO  PTRES-COMB-START-DATE (IDX)
           MOVE    ORCSAPIHKN-COMB-TEKEDYMD(IDX)
                                   TO  PTRES-COMB-END-DATE (IDX)
      *    保険組合せ非表示
           EVALUATE    ORCSAPIHKN-HYOJIKBN(IDX)
               WHEN    "2"
      *            入院非表示
                   MOVE    "I"         TO  PTRES-COMBINATION-HYOJI
                                                            (IDX)
               WHEN    "3"
      *            外来非表示
                   MOVE    "O"         TO  PTRES-COMBINATION-HYOJI
                                                            (IDX)
               WHEN    OTHER
      *            非表示なし
                   MOVE    "N"         TO  PTRES-COMBINATION-HYOJI
                                                            (IDX)
           END-EVALUATE
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  PTRES-MAIN-INSURANCE-CLASS (IDX)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  PTRES-MAIN-INSURANCE-NUMBER(IDX)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  PTRES-MAIN-INSURANCE-NAME(IDX)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  PTRES-MAIN-MARK (IDX)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  PTRES-MAIN-NUMBER  (IDX)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  PTRES-MAIN-CONTINUATION (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  PTRES-MAIN-ASSISTANCE  (IDX)
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  PTRES-MAIN-ASSISTANCE-NAME(IDX)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  PTRES-MAIN-FAMILY  (IDX)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  PTRES-MAIN-POLICYHOLDER(IDX)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  PTRES-MAIN-START-DATE (IDX)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  PTRES-MAIN-END-DATE   (IDX)
      *    資格取得日
           MOVE    ORCSAPIHKN-SKKGETYMD (IDX)
                                   TO  PTRES-MAIN-GET-DATE   (IDX)
      *    確認年月日
           MOVE    ORCSAPIHKN-KAKUNINYMD  (IDX)
                                   TO  PTRES-MAIN-CHECK-DATE (IDX)
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  PTRES-INSURANCE-CLASS
                                                        (IDX IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  PTRES-INSURANCE-NAME
                                                        (IDX IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  PTRES-PROVIDER-NUMBER
                                                        (IDX IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  PTRES-RECIPIENT-NUMBER
                                                        (IDX IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  PTRES-INSURANCE-START-DATE
                                                        (IDX IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  PTRES-INSURANCE-END-DATE
                                                        (IDX IDY2)
      *R01.6
      *        確認年月日
               MOVE    ORCSAPIHKN-KOH-KAKUNINYMD (IDX IDY2)
                                       TO  PTRES-INSURANCE-CHECK-DATE
                                                        (IDX IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  PTRES-RATE-ADMISSION
                                                        (IDX IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  PTRES-MONEY-ADMISSION
                                                        (IDX IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  PTRES-RATE-OUTPATIENT
                                                        (IDX IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  PTRES-MONEY-OUTPATIENT
                                                        (IDX IDY2)
           END-PERFORM
      *    労災・自賠責・第三者行為編集
           IF     (ORCSAPIHKN-HKNNUM(IDX)      =   "971"
                                               OR  "973"  )
               OR (ORCSAPIHKN-KOHNUM(IDX 1)    =   "970"  )
               PERFORM 2002311-PTRSIINF-HEN-SEC
           END-IF
      *
           .
       200231-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災・自賠責・第三者行為編集処理
      *****************************************************************
       2002311-PTRSIINF-HEN-SEC        SECTION.
      *
      *    労災保険名称
           MOVE    ORCSAPIHKN-ACCIDENT-NAME (IDX)
                                       TO  PTRES-ACCIDENT-NAME(IDX)
      *    傷病の部位（番号と同じ内容）
           MOVE    ORCSAPIHKN-DISEASE-LOCATION (IDX)
                                       TO  PTRES-DISEASE-LOCATION(IDX)
      *    傷病年月日
           MOVE    ORCSAPIHKN-DISEASE-DATE (IDX)
                                       TO  PTRES-DISEASE-DATE(IDX)
      *労災
      *    労働保険番号
           MOVE    ORCSAPIHKN-ACCIDENT-NUMBER (IDX)
                                       TO  PTRES-ACCIDENT-NUMBER(IDX)
      *    年金証書番号
           MOVE    ORCSAPIHKN-PENSIONCERTIFICATE (IDX)
                                       TO  PTRES-PENSIONCERTIFICATE(IDX)
      *    災害区分
           MOVE    ORCSAPIHKN-ACCIDENT-CLASS (IDX)
                                       TO  PTRES-ACCIDENT-CLASS(IDX)
           MOVE    ORCSAPIHKN-ACCIDENT-CLASS-NAME (IDX)
                                       TO  PTRES-ACCIDENT-CLASS-NAME
                                                                (IDX)
      *    労働基準監督署コード
           MOVE    ORCSAPIHKN-LABOR-ST-CODE (IDX)
                                       TO  PTRES-LABOR-ST-CODE(IDX)
      *    労働基準監督署コード
           MOVE    ORCSAPIHKN-LABOR-ST-NAME (IDX)
                                       TO  PTRES-LABOR-ST-NAME(IDX)
      *
      *    新継再別区分
           MOVE    ORCSAPIHKN-SINKEI  (IDX)
                                       TO  PTRES-SINKEI (IDX)
           MOVE    ORCSAPIHKN-SINKEI-NAME  (IDX)
                                       TO  PTRES-SINKEI-NAME (IDX)
      *    転帰事由
           MOVE    ORCSAPIHKN-TENKI  (IDX)
                                       TO  PTRES-TENKI (IDX)
           MOVE    ORCSAPIHKN-TENKI-NAME  (IDX)
                                       TO  PTRES-TENKI-NAME (IDX)
      *    四肢特例区分
           MOVE    ORCSAPIHKN-SISIKBN  (IDX)
                                       TO  PTRES-SISIKBN (IDX)
           MOVE    ORCSAPIHKN-SISIKBN-NAME  (IDX)
                                       TO  PTRES-SISIKBN-NAME (IDX)
      *
      *    事業所名称
           MOVE    ORCSAPIHKN-RSI-JIGYOUNAME (IDX)
                                       TO  PTRES-RSI-JIGYOUNAME(IDX)
      *    所在地都道府県名
           MOVE    ORCSAPIHKN-RSI-PREFNAME (IDX)
                                       TO  PTRES-RSI-PREFNAME(IDX)
      *    所在地都道府県区分
           MOVE    ORCSAPIHKN-RSI-PREFKBN (IDX)
                                       TO  PTRES-RSI-PREFKBN(IDX)
      *    所在地都道府県区分名称
           MOVE    ORCSAPIHKN-RSI-PREFKBNNAME (IDX)
                                       TO  PTRES-RSI-PREFKBNNAME(IDX)
      *    所在地郡市区名
           MOVE    ORCSAPIHKN-RSI-CITYNAME (IDX)
                                       TO  PTRES-RSI-CITYNAME(IDX)
      *    所在地郡市区区分
           MOVE    ORCSAPIHKN-RSI-CITYKBN(IDX)
                                       TO  PTRES-RSI-CITYKBN(IDX)
      *    所在地郡市区名称
           MOVE    ORCSAPIHKN-RSI-CITYKBNNAME (IDX)
                                       TO  PTRES-RSI-CITYKBNNAME(IDX)
      *    労災レセ回数記載 基準年月
           MOVE    ORCSAPIHKN-KIJYUNYM (IDX)
                                       TO  PTRES-RSI-KIJYUNYM(IDX)
      *    労災レセ回数記載 回数
           MOVE    ORCSAPIHKN-RESEPUTO-COUNT(IDX)
                                       TO  PTRES-RESEPUTO-COUNT(IDX)
      *自賠責
      *    自賠責請求区分
           MOVE    ORCSAPIHKN-JIBAISEIKBN (IDX)
                                       TO  PTRES-JIBAISEIKBN (IDX)
           MOVE    ORCSAPIHKN-JIBAISEIKBN-NAME (IDX)
                                       TO  PTRES-JIBAISEIKBN-NAME (IDX)
      *    自賠責保険会社名
           MOVE    ORCSAPIHKN-RSI-HOKENNAME (IDX)
                                       TO  PTRES-RSI-HOKENNAME(IDX)
      *    自賠責担当医
           MOVE    ORCSAPIHKN-RSI-DRCD (IDX)
                                       TO  PTRES-RSI-DRCD(IDX)
      *    自賠責担当医名称
           MOVE    ORCSAPIHKN-RSI-DRCD-NAME (IDX)
                                       TO  PTRES-RSI-DRCD-NAME(IDX)
      *アフターケア
      *    健康管理手帳番号
           MOVE    ORCSAPIHKN-RSI-KANRITCHO-NUMBER (IDX)
                                       TO  PTRES-RSI-KANRITCHO-NUMBER
                                                             (IDX)
      *    損傷区分
           MOVE    ORCSAPIHKN-DAMAGE-CLASS (IDX)
                                       TO  PTRES-DAMAGE-CLASS(IDX)
      *    損傷区分名称
           MOVE    ORCSAPIHKN-DAMAGE-CLASS-NAME (IDX)
                                       TO  PTRES-DAMAGE-CLASS-NAME(IDX)
      *第三者行為
      *    現物支給区分
           MOVE    ORCSAPIHKN-DS-GENBTKBN (IDX)
                                       TO  PTRES-DS-GENBTKBN (IDX)
           MOVE    ORCSAPIHKN-DS-GENBTKBN-NAME (IDX)
                                       TO  PTRES-DS-GENBTKBN-NAME (IDX)
      *    特記事項区分
           MOVE    ORCSAPIHKN-DS-TOKKIKBN (IDX)
                                       TO  PTRES-DS-TOKKIKBN (IDX)
           MOVE    ORCSAPIHKN-DS-TOKKIKBN-NAME (IDX)
                                       TO  PTRES-DS-TOKKIKBN-NAME(IDX)
           .
       2002311-PTRSIINF-HEN-EXT.
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
           IF      APILST31-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst6v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTRES-MODE
           MOVE    "patientlst2res"    TO  PTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST6RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_patientlst6v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTRES-MODE
           MOVE    "patientlst2res"    TO  PTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST6RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    PTRES-OBJECT    TO  APILST31-BODY 
               MOVE    "application/xml"   TO  APILST31-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
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
               MOVE    "開始年月が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "終了年月が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "開始年月＞終了年月です"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "対象の保険組合せが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "対象の保険組合せが２００件以上となります。"
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
