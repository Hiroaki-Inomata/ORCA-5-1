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
       PROGRAM-ID.         ORAPI012R5V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 保険番号マスタ（保険公費の種類）、補助区分取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/11/01  公費200件対応
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
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
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
      *
           03  WRK-YMD-01.
               05  WRK-YY-01               PIC 9(04).
               05  WRK-MM-01               PIC 9(02).
               05  WRK-DD-01               PIC 9(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-ZZ9                 PIC ZZ9.
      *
           03  WRK-CNT                 PIC 9(03).
           03  WRK-ERR-CNT             PIC 9(03).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
           03  WRK-HOJOKBN             PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-BASE-DATE           PIC X(10).
           03  WRK-BASE-YMD            PIC X(08).
      *
       01  WRK-HENSYU-AREA.
      *
           03  WRK-HJO-TBL.
               05  WRK-HJO-MAX         PIC 9(04).
      *        船員（前期高齢者）
               05  WRK-HJO-002-REC-G.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
                   07  FILLER          PIC X(10) VALUE "G ３割職務".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
                   07  FILLER          PIC X(10) VALUE "I ３割通勤".
               05  WRK-HJO-002-REC-R   REDEFINES   WRK-HJO-002-REC-G.
                   07  WRK-HJO-002-REC     OCCURS  15.
                       09  WRK-HOJO-002        PIC X(01).
                       09  WRK-HOJO-002F       PIC X(01).
                       09  WRK-HOJO-002MEI     PIC X(08).
               05  WRK-HOJO02-MAX          PIC 9(02)   VALUE  15.
      *
      *        国保
               05  WRK-HJO-060-REC-G.
                   07  FILLER          PIC X(16) VALUE "0 無し".
                   07  FILLER          PIC X(16) VALUE "1 １割".
                   07  FILLER          PIC X(16) VALUE "2 ２割".
                   07  FILLER          PIC X(16) VALUE "3 ３割".
                   07  FILLER          PIC X(16) VALUE
                                                 "4 外１割 入０割".
                   07  FILLER          PIC X(16) VALUE
                                                 "5 外２割 入１割".
                   07  FILLER          PIC X(16) VALUE
                                                 "6 外３割 入２割".
               05  WRK-HJO-060-REC-R   REDEFINES   WRK-HJO-060-REC-G.
                   07  WRK-HJO-060-REC     OCCURS  7.
                       09  WRK-HOJO-060        PIC X(01).
                       09  WRK-HOJO-060F       PIC X(01).
                       09  WRK-HOJO-060MEI     PIC X(14).
               05  WRK-HOJ060-MAX          PIC 9(02)   VALUE  7.
      *
      *        自費 
               05  WRK-HJO-980-REC-G.
                   07  FILLER          PIC X(10) VALUE "1 課税".
                   07  FILLER          PIC X(10) VALUE "2 非課".
               05  WRK-HJO-980-REC-R   REDEFINES   WRK-HJO-980-REC-G.
                   07  WRK-HJO-980-REC     OCCURS  2.
                       09  WRK-HOJO-980        PIC X(01).
                       09  WRK-HOJO-980F       PIC X(01).
                       09  WRK-HOJO-980MEI     PIC X(08).
               05  WRK-HOJ980-MAX          PIC 9(02)   VALUE  2.
      *
      *        前期高齢者(協会他）
               05  WRK-HJO-009-REC-G.
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
               05  WRK-HJO-009-REC-R   REDEFINES   WRK-HJO-009-REC-G.
                   07  WRK-HJO-009-REC     OCCURS  3.
                       09  WRK-HOJO-009        PIC X(01).
                       09  WRK-HOJO-009F       PIC X(01).
                       09  WRK-HOJO-009MEI     PIC X(08).
               05  WRK-HOJ009-MAX          PIC 9(02)   VALUE  3.
      *
      *        後期高齢者の補助区分（Ｈ２０．４以降）
               05  WRK-HJO-039-REC-G.
                   07  FILLER          PIC X(10) VALUE "1 １割".
                   07  FILLER          PIC X(10) VALUE "3 ３割".
               05  WRK-HJO-039-REC-R   REDEFINES   WRK-HJO-039-REC-G.
                   07  WRK-HJO-039-REC     OCCURS  2.
                       09  WRK-HOJO-039        PIC X(01).
                       09  WRK-HOJO-039F       PIC X(01).
                       09  WRK-HOJO-039MEI     PIC X(08).
               05  WRK-HOJ039-MAX          PIC 9(02)   VALUE  2.
      *
      *        共済の下船後補助区分追加（前期高齢者）
               05  WRK-HJO-032-REC-G.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
               05  WRK-HJO-032-REC-R   REDEFINES   WRK-HJO-032-REC-G.
                   07  WRK-HJO-032-REC     OCCURS  7.
                       09  WRK-HOJO-032        PIC X(01).
                       09  WRK-HOJO-032F       PIC X(01).
                       09  WRK-HOJO-032MEI     PIC X(08).
               05  WRK-HOJ032-MAX          PIC 9(02)   VALUE  7.
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    画面日付変換サブ
      *    COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張）
      *    COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
      *    COPY    "CPORCSSTRING.INC".
      *    補助区分名称パラメタ
           COPY    "CPORAPI012SUB1.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPINSURANCEINF1V2REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPINSURANCEINF1V2RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
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
               MOVE   404                  TO  APILST21-HTTPSTATUS
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
           INITIALIZE                      XML-INSURANCEINFRES
           INITIALIZE                      XML-APIREQ
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  INSUINFS-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  INSUINFS-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  INSUINFS-INFORMATION-TIME
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
           MOVE    ZERO                TO  INSUINFS-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    INSUINFQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    返却処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-HKNNM-LIST-SEC
           END-IF
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
           IF      APILST21-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
                ",APILST21-BODY:"  APILST21-BODY
           END-IF
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_insuranceinf1v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST21-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "insuranceinfreq"  TO  APIREQ-RECORDNAME
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
           MOVE    "xml_insuranceinf1v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST21-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "insuranceinfreq"   TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-INSURANCEINFREQ
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
           MOVE    INSUINFQ-BASE-DATE  TO  WRK-BASE-DATE
      *    処理日
           MOVE    INSUINFQ-BASE-DATE  TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-BASE-YMD
      *    開始日未設定はシステム日付
           IF      WRK-BASE-DATE       =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-BASE-YMD
               MOVE    WRK-BASE-YMD        TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-BASE-DATE
           END-IF
           MOVE    WRK-BASE-DATE       TO  INSUINFS-BASE-DATE
      *
      *    基準日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BASEDATE-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日チェック
      *****************************************************************
       20011-BASEDATE-CHK-SEC    SECTION.
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
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-BASEDATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ一覧処理
      *****************************************************************
       2002-HKNNM-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
      *
      *    基準日付＋１月
           MOVE    WRK-BASE-YMD        TO  WRK-YMD-01
           COMPUTE WRK-MM-01           =   WRK-MM-01    +   1
           IF      WRK-MM-01           >   12
               COMPUTE WRK-YY-01       =   WRK-YY-01    +   1
               MOVE    01                  TO  WRK-MM-01
           END-IF
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 930-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL       FLG-HKNNUM  =   1
      *      支払区分のないものは対象外とする
             IF      HKN-PAYKBN        =   SPACE
               CONTINUE
             ELSE
      *        基準日付から＋１までを範囲とする
               IF    ((HKN-TEKSTYMD    <=  WRK-BASE-YMD ) AND
                      (HKN-TEKEDYMD    >=  WRK-BASE-YMD ))
                 OR  ((HKN-TEKSTYMD    <=  WRK-YMD-01   ) AND
                      (HKN-TEKEDYMD    >=  WRK-YMD-01   ))
                   IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                               OR  "8"  OR  "9"
      *                保険
                       PERFORM 20021-HKNNUM-HEN-SEC
                   ELSE
      *                公費
                       PERFORM 20022-KOHNUM-HEN-SEC
                   END-IF
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 930-HKNNUM-READ-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "12"            TO  WRK-ERRCD
           END-IF
           .
       2002-HKNNM-LIST-EXT.
           EXIT.
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       20021-HKNNUM-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *    同じ保険は基準日有効を対象
           IF     (IDX                 >   ZERO )  AND
                  (HKN-HKNNUM          =   INSUINFS-INSURANCE-CLASS
                                                    (IDX))
               IF     (HKN-TEKSTYMD    <=  WRK-BASE-YMD ) AND
                      (HKN-TEKEDYMD    >=  WRK-BASE-YMD )
                   MOVE    1               TO  FLG-OK
               END-IF
           ELSE
               MOVE    1               TO  FLG-OK
               ADD     1               TO  IDX
           END-IF
      *
           IF     (FLG-OK              =   1   )
              AND (IDX                 <=  99  )
      *        保険情報編集
               MOVE    HKN-HKNNUM      TO  INSUINFS-INSURANCE-CLASS
                                                        (IDX)
               MOVE    HKN-TANSEIDONAME
                                       TO  INSUINFS-INSURANCE-NAME
                                                        (IDX)
      *        保険法別番号
               MOVE    HKN-HBTNUM      TO  INSUINFS-INSURANCE-HKNHBTNUM
                                                        (IDX)
      *        補助区分
               MOVE    HKN-HKNNUM      TO  WRK-HKNNUM
               PERFORM 200211-HOJOKBN-HEN-SEC
           END-IF
           .
       20021-HKNNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分編集処理
      *****************************************************************
       200211-HOJOKBN-HEN-SEC     SECTION.
      *
      *
           EVALUATE    WRK-HKNNUM
           WHEN    "002"
      *        船員
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   WRK-HOJO02-MAX
                   MOVE    WRK-HOJO-002 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                   MOVE    WRK-HOJO-002MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
                   IF     WRK-HOJO-002 (IDH)   =   "1"
                                               OR  "2"
                                               OR  "3"
      *                   一般
                       CONTINUE
                   ELSE
      *                高齢者区分
                       MOVE    "1"    TO  INSUINFS-ASSISTANCE-MODE
                                                              (IDX IDH)
                   END-IF
               END-PERFORM
      *    後期高齢者
           WHEN    "039"
           WHEN    "040"
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   WRK-HOJ039-MAX
                   MOVE    WRK-HOJO-039 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                   MOVE    WRK-HOJO-039MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
      *            高齢者区分
                   MOVE    "1"         TO  INSUINFS-ASSISTANCE-MODE
                                                              (IDX IDH)
               END-PERFORM
      *    共済
           WHEN    "031"
           WHEN    "032"
           WHEN    "033"
           WHEN    "034"
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   WRK-HOJ032-MAX
                   MOVE    WRK-HOJO-032 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                   MOVE    WRK-HOJO-032MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
                   IF     WRK-HOJO-032 (IDH)   =   "2"
      *                   一般
                       CONTINUE
                   ELSE
      *                高齢者区分
                       MOVE    "1"    TO  INSUINFS-ASSISTANCE-MODE
                                                              (IDX IDH)
                   END-IF
               END-PERFORM
      *    国保
           WHEN    "060"
           WHEN    "068"
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   WRK-HOJ060-MAX
                   MOVE    WRK-HOJO-060 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                   MOVE    WRK-HOJO-060MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
               END-PERFORM
      *    治験
           WHEN    "900"
           WHEN    "901"
      *    自費
           WHEN    "980"   THRU   "989"
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   WRK-HOJ980-MAX
                   MOVE    WRK-HOJO-980 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                   MOVE    WRK-HOJO-980MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
               END-PERFORM
      *
           WHEN    OTHER
               IF      WRK-HKNNUM(1:1)     =   "9"
                   CONTINUE
               ELSE
      *        協会等
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL       IDH     >   WRK-HOJ009-MAX
                       MOVE    WRK-HOJO-009 (IDH)
                                       TO  INSUINFS-ASSISTANCE
                                                              (IDX IDH)
                       MOVE    WRK-HOJO-009MEI(IDH)
                                       TO  INSUINFS-ASSISTANCE-NAME
                                                              (IDX IDH)
      *                高齢者区分
                       MOVE    "1"     TO  INSUINFS-ASSISTANCE-MODE
                                                              (IDX IDH)
                   END-PERFORM
               END-IF
           END-EVALUATE
           .
       200211-HOJOKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20022-KOHNUM-HEN-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *    同じ保険は基準日有効を対象
           IF     (IDY                 >   ZERO )  AND
                  (HKN-HKNNUM          =   INSUINFS-INSURANCE-CLASS
                                                    (IDY))
               IF     (HKN-TEKSTYMD    <=  WRK-BASE-YMD ) AND
                      (HKN-TEKEDYMD    >=  WRK-BASE-YMD )
                   MOVE    1               TO  FLG-OK
               END-IF
      *        老人の時
               IF    ((HKN-HKNNUM      =   "027"  )  AND
                      (HKN-PAYKBN      =   "01"  OR
                                           "02"  OR
                                           "03"   ))
                AND   (FLG-OK          =   1      )
                AND   (HKN-PAYKBN  NOT =   INSUINFS-PUBLIC-PAYKBN
                                                     (IDY))
                   ADD     1               TO  IDY
               END-IF
           ELSE
               MOVE    1               TO  FLG-OK
               ADD     1               TO  IDY
           END-IF
      *
           IF     (FLG-OK              =   1   )
      ******* AND (IDY                 <=  99  )
              AND (IDY                 <=  200 )
      *
               MOVE    HKN-HKNNUM      TO  INSUINFS-PUBLIC-CLASS
                                                          (IDY)
               MOVE    HKN-TANSEIDONAME
                                       TO  INSUINFS-PUBLIC-CLASS-NAME
                                                          (IDY)
      *        法別番号
               MOVE    HKN-HBTNUM      TO  INSUINFS-PUBLIC-HKNHBTNUM
                                                          (IDY)
      *        支払区分（老人のみ（現在廃止））
               IF     (HKN-HKNNUM      =   "027"  )
                   MOVE    HKN-PAYKBN      TO  INSUINFS-PUBLIC-PAYKBN
                                                          (IDY)
               END-IF
           END-IF
           .
      *
       20022-KOHNUM-HEN-EXT.
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
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  INSUINFS-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  INSUINFS-API-RESULT
               MOVE    WRK-ERRMSG          TO  INSUINFS-API-RESULT-MSG
           END-IF
      *
           IF      APILST21-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_insuranceinf1v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  INSUINFS-MODE
           MOVE    "insuranceinfres"  TO  INSUINFS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-INSURANCEINFRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_insuranceinf1v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  INSUINFS-MODE
           MOVE    "insuranceinfres"   TO  INSUINFS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-INSURANCEINFRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    INSUINFS-OBJECT     TO  APILST21-BODY 
               MOVE    "application/xml"   TO  APILST21-CONTENT-TYPE
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
               MOVE    "基準日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "該当内容が存在しません。"
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
      *    公費履歴読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       930-HKNNUM-READ-EXT.
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
