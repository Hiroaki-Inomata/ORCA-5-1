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
       PROGRAM-ID.         ORAPI012R3V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者情報一覧取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/29    NACL−多々納　新規作成
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
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
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
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPPATIENTLST2V2REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPPATIENTLST2V2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    保険組合せセット領域
      **   COPY    "CPORCSAPIHKN.INC".
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
      *    COPY    "CPAPIPTAV2.INC".
      *    COPY    "CPAPILSTV2.INC".
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST1-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST2-//.
      *    COPY    "CPAPILSTV2.INC"  REPLACING
      *                              //APILST-// BY //APILST3-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST4-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST5-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST6-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST7-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                                //APILST-// BY //APILST8-//.
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
           DISPLAY   "* patientlst2 start*"
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
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST3-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* patientlst2 end *"
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
           INITIALIZE                      XML-PATIENTLST2RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  PATLSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PATLSTRES-INFORMATION-TIME
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
           MOVE    ZERO                TO  PATLSTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST3-CLASS
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-MAIN-SYORI-SEC
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
           IF      APILST3-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
                ",APILST3-BODY:"  APILST3-BODY
           END-IF
           INITIALIZE                  XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst2v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST3-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst2req"    TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientlst2v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST3-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst2req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-PATIENTLST2REQ
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
      *    患者番号毎の処理
      *****************************************************************
       2001-MAIN-SYORI-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-ERR-CNT
           MOVE    SPACE               TO  WRK-ERRMSG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   100
               IF      PATLSTREQ-PATIENTID (IDX)   NOT =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    ZERO                TO  FLG-ERR
      *            入力項目チェック処理
                   PERFORM 20011-INPUT-CHK-SEC
      *            患者一覧処理
                   IF      FLG-ERR             =   ZERO
                       ADD     1               TO  WRK-CNT
                       PERFORM 20012-PTINF-HEN-SEC
                   ELSE
                       ADD     1               TO  WRK-ERR-CNT
                       MOVE    WRK-PTNUM       TO  PATLSTRES-PATIENTID
                                                                  (IDY)
                       MOVE    WRK-ERRMSG      TO  PATLSTRES-NAME (IDY)
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定内容チェック
      *****************************************************************
       20011-INPUT-CHK-SEC      SECTION.
      *
           MOVE    PATLSTREQ-PATIENTID (IDX)
                                       TO  WRK-PTNUM
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "【患者番号がありません】"
                                           TO  WRK-ERRMSG
               MOVE    1                   TO  FLG-ERR
               GO      TO                  20011-INPUT-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  PATLSTRES-PATIENTID (IDY)
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "【患者番号がありません】"
                                           TO  WRK-ERRMSG
               MOVE    1                   TO  FLG-ERR
           END-IF
           .
       20011-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20012-PTINF-HEN-SEC     SECTION.
      *
           MOVE    WRK-PTNUM           TO  PATLSTRES-PATIENTID (IDY)
           MOVE    PTINF-NAME          TO  PATLSTRES-NAME      (IDY)
           MOVE    PTINF-KANANAME      TO  PATLSTRES-KANANAME  (IDY)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-BIRTHDAY  (IDY)
           MOVE    PTINF-SEX           TO  PATLSTRES-SEX       (IDY)
      *
           MOVE    PTINF-HOME-POST     TO  PATLSTRES-HOME-ZIP-CODE
                                                               (IDY)
           MOVE    PTINF-HOME-ADRS     TO  PATLSTRES-HOME-ADDRESSES1
                                                               (IDY)
           MOVE    PTINF-HOME-BANTI    TO  PATLSTRES-HOME-ADDRESSES2
                                                               (IDY)
           MOVE    PTINF-HOME-TEL1     TO  PATLSTRES-HOME-HPONE1
                                                               (IDY)
           MOVE    PTINF-HOME-TEL2     TO  PATLSTRES-HOME-HPONE2
                                                               (IDY)
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    SPA-PTID            TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *    入院中は病棟で判断
           IF      ORCSLASTYMD-BRMNUM  NOT =   SPACE
               MOVE    "1"             TO  PATLSTRES-OUTPATIENT-CLASS
                                                               (IDY)
           END-IF
      *    保険・公費編集処理
           PERFORM 200210-HKNKOH-HEN-SEC
           .
       20012-PTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険・公費編集処理
      *****************************************************************
       200210-HKNKOH-HEN-SEC     SECTION.
      *
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDH
           PERFORM     UNTIL   FLG-PTHKNINF    =   1
               IF     (PTHKN-SAKJOKBN      =   SPACE )
                AND   (PTHKN-TEKSTYMD      <=  SPA-SYSYMD)
                AND   (PTHKN-TEKEDYMD      >=  SPA-SYSYMD)
                   ADD     1               TO  IDH
                   IF      IDH             <=  3
                       PERFORM 2002101-PTHKN-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTKOH-HOSPNUM
           MOVE    PTINF-PTID          TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDH
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
               IF     (PTKOH-SAKJOKBN      =   SPACE     )
                AND   (PTKOH-TEKSTYMD      <=  SPA-SYSYMD)
                AND   (PTKOH-TEKEDYMD      >=  SPA-SYSYMD)
                   ADD     1               TO  IDH
                   IF      IDH             <=  4
                       PERFORM 2002102-PTKOHINF-HEN-SEC
                   END-IF
             END-IF
      *
             MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200210-HKNCOMBI-HEN-EXT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-PTHKN-HEN-SEC              SECTION.
      *
      *    保険の種類
           MOVE    PTHKN-HKNNUM    TO  PATLSTRES-MAIN-INSURANCE-CLASS
                                                            (IDY IDH)
      *    保険者番号
           MOVE    PTHKN-HKNJANUM 
                                   TO  PATLSTRES-MAIN-INSURANCE-NUMBER
                                                            (IDY IDH)
      *    保険の名称
           MOVE    PTHKN-HKNNUM    TO  WRK-HKNNUM
           MOVE    ZERO            TO  WRK-PAYKBN
           MOVE    PTHKN-TEKSTYMD  TO  WRK-TEKSTYMD
           MOVE    PTHKN-TEKEDYMD  TO  WRK-TEKEDYMD
           PERFORM 2900-HKNNUM-HEN-SEC
           MOVE    HKN-TANSEIDONAME
                                   TO  PATLSTRES-MAIN-INSURANCE-NAME
                                                            (IDY IDH)
      *    記号
           MOVE    PTHKN-KIGO      TO  PATLSTRES-MAIN-MARK  (IDY IDH)
      *    番号
           MOVE    PTHKN-NUM       TO  PATLSTRES-MAIN-NUMBER(IDY IDH)
      *    継続区分
           MOVE    PTHKN-CONTKBN   TO  PATLSTRES-MAIN-CONTINUATION
                                                            (IDY IDH)
      *    補助区分
           MOVE    PTHKN-HOJOKBN   TO  PATLSTRES-MAIN-ASSISTANCE
                                                            (IDY IDH)
      *    本人家族区分
           MOVE    PTHKN-HONKZKKBN TO  PATLSTRES-MAIN-FAMILY 
                                                            (IDY IDH)
      *    被保険者名
           MOVE    PTHKN-HIHKNJANAME
                                   TO  PATLSTRES-MAIN-POLICYHOLDER
                                                            (IDY IDH)
      *    有効開始日
           MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-MAIN-START-DATE
                                                            (IDY IDH)
      *    有効終了日
           MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-MAIN-END-DATE
                                                            (IDY IDH)
           .
       2002101-PTHKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号処理
      *****************************************************************
       2900-HKNNUM-HEN-SEC              SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKEDYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2900-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    出力領域公費情報編集処理
      *****************************************************************
       2002102-PTKOHINF-HEN-SEC              SECTION.
      *
      *    公費の種類
           MOVE    PTKOH-KOHNUM        TO  PATLSTRES-INSURANCE-CLASS
                                                          (IDY IDH)
      *    公費の種類名称
           MOVE    PTKOH-KOHNUM        TO  WRK-HKNNUM
           MOVE    PTKOH-PAYKBN        TO  WRK-PAYKBN
           MOVE    PTKOH-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  WRK-TEKEDYMD
           PERFORM 2900-HKNNUM-HEN-SEC
           MOVE    HKN-TANSEIDONAME    TO  PATLSTRES-INSURANCE-NAME
                                                           (IDY IDH)
      *    負担者番号
           MOVE    PTKOH-FTNJANUM      TO  PATLSTRES-PROVIDER-NUMBER
                                                           (IDY IDH)
      *    受給者番号
           MOVE    PTKOH-JKYSNUM        TO  PATLSTRES-RECIPIENT-NUMBER
                                                           (IDY IDH)
      *    有効開始日
           MOVE    PTKOH-TEKSTYMD  TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  PATLSTRES-INSURANCE-START-DATE
                                                           (IDY IDH)
      *    有効終了日
           MOVE    PTKOH-TEKEDYMD  TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-INSURANCE-END-DATE
                                                           (IDY IDH)
           .
       2002101-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
      *    処理対象件数
           MOVE    WRK-CNT             TO  PATLSTRES-PATIENTID-CNT
           MOVE    WRK-ERR-CNT         TO  PATLSTRES-PATIENTID-ERR
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  PATLSTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PATLSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PATLSTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST3-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst2v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PATLSTRES-MODE
           MOVE    "patientlst2res"    TO  PATLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_patientlst2v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PATLSTRES-MODE
           MOVE    "patientlst2res"    TO  PATLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    PATLSTRES-OBJECT    TO  APILST3-BODY 
               MOVE    "application/xml"   TO  APILST3-CONTENT-TYPE
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
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
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
