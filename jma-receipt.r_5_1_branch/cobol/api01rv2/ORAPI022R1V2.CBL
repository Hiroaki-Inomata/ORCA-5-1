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
       PROGRAM-ID.         ORAPI022R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者病名返却
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/07/24  保険病名フラグ追加対応
      *
      *  04.08.01    NACL-藤原    16/02/29  合併症に係わる情報追加対応
      *                                     原疾患区分、合併症区分
      *  04.08.02    NACL-藤原    16/09/08  全ての患者病名情報の取得対応
      *  04.08.03    NACL-藤原    17/05/15  補足コメントコード返却対応
      *
      *  05.00.01    NACL-藤原    19/05/14  退院証明書返却対応
      *  05.00.02    NACL-藤原    19/06/17  第三者行為返却対応
      *  05.00.03    NACL-藤原    19/09/10  エラーコード修正
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
           03  FLG-HKNCOMBI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
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
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
       01  WRK-XML-AREA.
           03  WRK-BASE-DATE.
               05  WRK-BASE-DATE1       PIC X(07).
               05  WRK-BASE-DATE2       PIC X(03).
           03  WRK-BASE-YMD.
               05  WRK-BASE-YM          PIC X(06).
               05  WRK-BASE-DD          PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SELECT-MODE         PIC X(03).
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPDISEASEINFV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPDISEASEINFV2RES.INC".
      *
      *    病名一覧返却用パラメタ
           COPY    "CPORCSBYOINFO.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
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
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*********************"
           DISPLAY   "* disease_info start*"
           DISPLAY   "*********************"
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
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST7-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*********************"
           DISPLAY   "* disease_info end  *"
           DISPLAY   "*********************"
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
           INITIALIZE                      XML-DISEASE-INFRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  DISERES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DISERES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  DISERES-INFORMATION-TIME
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
           EVALUATE    APILST7-CLASS
           WHEN    "01"
      *        患者病名情報
               MOVE    1                   TO  FLG-SYORI
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
           EVALUATE    APILST7-CLASS
           WHEN    "01"
      *        患者病名情報
               PERFORM 2002-SYORI-01-SEC
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
           IF      APILST7-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_disease_infov2req" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST7-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "disease_inforeq"   TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-DISEASE-INFREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
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
               MOVE    ZERO                TO  DISERES-API-RESULT
               MOVE    "処理終了"          TO  DISERES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  DISERES-API-RESULT
               MOVE    WRK-ERRMSG          TO  DISERES-API-RESULT-MSG
           END-IF
      *
      *    XML情報書き込み処理
           PERFORM 3001-MDC01XMLMAKE-SEC
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-MDC01XMLMAKE-SEC        SECTION.
      *
           IF      APILST7-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_disease_infov2res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  DISERES-MODE
           MOVE    "disease_infores"   TO  DISERES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DISEASE-INFRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    DISERES-OBJECT      TO  APILST7-BODY 
               MOVE    "application/xml"   TO  APILST7-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-MDC01XMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      DISEREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  DISEREQ-PATIENTID
           END-IF
           IF     DISEREQ-BASE-DATE     =   LOW-VALUE
               MOVE  SPACE             TO  DISEREQ-BASE-DATE
           END-IF
           IF     DISEREQ-SELECT-MODE   =   LOW-VALUE
               MOVE  SPACE             TO  DISEREQ-SELECT-MODE
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
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    基準日付チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BASEDATE-CHK-SEC
           END-IF
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
           MOVE    DISEREQ-PATIENTID       TO  WRK-PTNUM
           MOVE    WRK-PTNUM               TO  DISERES-PATIENTID
      *    基準日付
           MOVE    DISEREQ-BASE-DATE       TO  DISERES-BASE-DATE
           MOVE    DISEREQ-BASE-DATE       TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
           IF      WRK-BASE-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
               MOVE    DISERES-INFORMATION-DATE
                                           TO  DISEREQ-BASE-DATE
           END-IF
      *    出力設定
           MOVE    DISEREQ-SELECT-MODE     TO  WRK-SELECT-MODE
           IF      WRK-SELECT-MODE         =   "All"
               CONTINUE
           ELSE
               MOVE    SPACE                   TO  WRK-SELECT-MODE
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
      *    診療日付チェック
      *****************************************************************
       20011-BASEDATE-CHK-SEC    SECTION.
      *
           MOVE    "01"                TO  WRK-BASE-DD
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
           MOVE    WRK-BASE-YMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-BASE-DATE
      *
           .
       20011-BASEDATE-CHK-EXT.
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
           MOVE    WRK-PTNUM           TO  DISERES-PATIENTID
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
      *    患者病名編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
           PERFORM 20021-DISERES-HEN-SEC
      *    患者病名一覧取得サブ
           INITIALIZE                  ORCSBYOINFOAREA
      *    患者番号
           MOVE    WRK-PTNUM           TO  SBYOINFO-PTNUM
      *    患者ＩＤ
           MOVE    SPA-PTID            TO  SBYOINFO-PTID
      *    診療年月
           MOVE    WRK-BASE-YM         TO  SBYOINFO-SRYYM
      *    出力設定
           MOVE    WRK-SELECT-MODE     TO  SBYOINFO-SELECT-MODE
      *
           CALL    "ORCSBYOINFO"   USING   ORCSBYOINFOAREA
                                           SPA-AREA
      *
           MOVE    ZERO                TO  TBL-MAX
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   200  )
                        OR   (SBYOINFO-SRYYMD (IDX)  =   SPACE )
               ADD     1                   TO  TBL-MAX
               PERFORM 20021-BYOMEI-HEN-SEC
           END-PERFORM
      *
           IF      SBYOINFO-RC    NOT =   ZERO
      *        200件以上あり
               MOVE    "True"             TO
                                          DISERES-INFORMATION-OVERFLOW
               MOVE    "20"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           ELSE
               MOVE    "False"           TO
                                          DISERES-INFORMATION-OVERFLOW
           END-IF
      *
           
           IF      TBL-MAX             =   ZERO
      *        対象なし
               MOVE    "21"                TO  WRK-ERRCD
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-DISERES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PTNUM           TO  DISERES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  DISERES-NAME
           MOVE    PTINF-KANANAME      TO  DISERES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DISERES-BIRTHDAY
           MOVE    PTINF-SEX           TO  DISERES-SEX
      *    基準日
           MOVE    WRK-BASE-DATE1      TO  DISERES-BASE-DATE
           .
       20021-DISERES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       20021-BYOMEI-HEN-SEC                SECTION.
      *
      *    入外区分
      *    MOVE    SBYOINFO-NYUGAIKBN(IDX) TO  DISERES-NYUGAIKBN
      *                                                       (IDX)
      *ver.4.7
           EVALUATE    SBYOINFO-NYUGAIKBN(IDX)
               WHEN    "1"
      *            入院
                   MOVE    "I"             TO  DISERES-NYUGAIKBN
                                                              (IDX)
               WHEN    "2"
      *            外来
                   MOVE    "O"             TO  DISERES-NYUGAIKBN
                                                              (IDX)
           END-EVALUATE
      *    診療科
           MOVE    SBYOINFO-SRYKA (IDX)    TO  DISERES-SRYKA (IDX)
      *    保険組合せ番号
           IF      SBYOINFO-HKNCOMBI (IDX) NOT =   ZERO
               MOVE    SBYOINFO-HKNCOMBI (IDX) TO  DISERES-HKNCOMBI
                                                              (IDX)
           END-IF
      *    第三者行為
           IF      SBYOINFO-KOHID (IDX)    >   ZERO
               INITIALIZE                      HKNCOMBI-REC
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SBYOINFO-PTID       TO  COMB-PTID
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key9"              TO  MCP-PATHNAME
                   PERFORM 900-COMB-READ-N-SEC
               ELSE
                   INITIALIZE                      HKNCOMBI-REC
                   MOVE    1                   TO  FLG-HKNCOMBI
               END-IF
               PERFORM            UNTIL    FLG-HKNCOMBI =   1
                   IF   ( SBYOINFO-KOHID  (IDX)    =   COMB-KOHID (1) )
                   AND  ( SBYOINFO-SRYYMD (IDX)    <=  COMB-TEKEDYMD  )
                       MOVE    COMB-HKNCOMBINUM    TO
                                           DISERES-HKNCOMBI        (IDX)
                       MOVE    "True"              TO
                                           DISERES-THIRD-PARTY-MARK(IDX)
                       MOVE    1                   TO  FLG-HKNCOMBI
                   ELSE
                       MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                       MOVE    "key9"              TO  MCP-PATHNAME
                       PERFORM 900-COMB-READ-N-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *    病名
           MOVE    SBYOINFO-BYOMEI (IDX)   TO  DISERES-DISEASE-NAME
                                                              (IDX)
      *
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >   21  )
                       OR    (SBYOINFO-BYOMEICD (IDX IDY)
                                               =   SPACE )
      *        病名コード
               MOVE    SBYOINFO-BYOMEICD (IDX IDY)
                                           TO  DISERES-SINGLE-CODE
                                                           (IDX IDY)
      *        病名
               MOVE    SBYOINFO-BYOMEI-T (IDX IDY)
                                           TO  DISERES-SINGLE-NAME
                                                           (IDX IDY)
      *        状態コード
               MOVE    SBYOINFO-FLG (IDX IDY)
                                           TO  DISERES-SINGLE-FLG
                                                           (IDX IDY)
           END-PERFORM
      *    主病名フラグ
           IF      SBYOINFO-SYUBYOFLG (IDX)    =   "1"
               MOVE    "PD"                TO  DISERES-CATEGORY (IDX)
           END-IF
      *    疑いフラグ
           EVALUATE    SBYOINFO-UTAGAIFLG (IDX)
      *        疑い
               WHEN    "1"
                   MOVE    "S"         TO  DISERES-SUSPECTEDFLAG (IDX)
      *        急性
               WHEN    "2"
                   MOVE    "A"         TO  DISERES-SUSPECTEDFLAG (IDX)
      *        急性かつ疑い
               WHEN    "3"
                   MOVE    "SA"        TO  DISERES-SUSPECTEDFLAG (IDX)
           END-EVALUATE
      *    診療開始日
           MOVE    SBYOINFO-SRYYMD (IDX)   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DISERES-START-DATE (IDX)
      *    転帰日
           IF      SBYOINFO-TENKIYMD (IDX) =   SPACE
               CONTINUE
           ELSE
               MOVE    SBYOINFO-TENKIYMD(IDX)  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  DISERES-END-DATE (IDX)
           END-IF
      *    転帰区分
           EVALUATE    SBYOINFO-TENKIKBN (IDX)
      *        治癒
               WHEN    "1"
                   MOVE    "F"             TO  DISERES-OUTCOME (IDX)
      *        死亡
               WHEN    "2"
                   MOVE    "D"             TO  DISERES-OUTCOME (IDX)
      *        中止
               WHEN    "3"
                   MOVE    "C"             TO  DISERES-OUTCOME (IDX)
      *        移行
               WHEN    "8"
                   MOVE    "S"             TO  DISERES-OUTCOME (IDX)
           END-EVALUATE
      *    補足コメント
           MOVE    SBYOINFO-HOSOKU-COMMENT (IDX)
                                       TO  DISERES-SUPPLEMENT-NAME(IDX)
      *    補足コメントコード
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       3
                   OR      SBYOINFO-HOSOKUCD (IDX IDY)
                                   =       SPACE
               MOVE    SBYOINFO-HOSOKUCD (IDX IDY)
                                           TO
                                       DISERES-SUPPLEMENT-SINGLE-CODE
                                                               (IDX IDY)
               MOVE    SBYOINFO-HOSOKUCD-COMMENT (IDX IDY)
                                           TO
                                       DISERES-SUPPLEMENT-SINGLE-NAME
                                                               (IDX IDY)
           END-PERFORM
      *    カルテ病名
           MOVE    SBYOINFO-CHARTBYOMEI (IDX)
                                       TO  DISERES-KARTE-NAME (IDX)
      *    疾患区分
           IF      SBYOINFO-MANSEIKBN(IDX) NOT =   ZERO
               MOVE    SBYOINFO-MANSEIKBN (IDX)
                                       TO  DISERES-DISEASE-CLASS (IDX)
           END-IF
      *    レセプト表示フラグ
           MOVE    SBYOINFO-REZEFLG (IDX)
                                       TO  DISERES-RECEIPT-PRINT (IDX)
      *    レセプト表示月数
           IF      SBYOINFO-REZEMM (IDX)   NOT =   ZERO
               MOVE    SBYOINFO-REZEMM (IDX)
                                       TO  DISERES-RECEIPT-PERIOD (IDX)
           END-IF
      *    保険病名フラグ
           IF      SBYOINFO-HKNBYOMEIFLG (IDX) =   "1"
               MOVE    "True"          TO  DISERES-INSURANCE-DISEASE
                                                                  (IDX)
           ELSE
               MOVE    "False"         TO  DISERES-INSURANCE-DISEASE
                                                                  (IDX)
           END-IF
      *    原疾患区分
           IF      SBYOINFO-CLASS1(IDX) NOT =   ZERO
               MOVE    SBYOINFO-CLASS1 (IDX)
                                       TO
                                   DISERES-CLASSIFICATION-NUMBER-MASTER
                                                                 (IDX)
           END-IF
      *    合併症区分
           IF      SBYOINFO-CLASS2(IDX) NOT =   ZERO
               MOVE    SBYOINFO-CLASS2 (IDX)
                                       TO
                                   DISERES-CLASSIFICATION-NUMBER-SERVANT
                                                                  (IDX)
           END-IF
      *    退院証明書
           MOVE    SBYOINFO-DISCHARGEFLG (IDX)
                                       TO  DISERES-DISCHARGEFLG   (IDX)
           .
       20021-BYOMEI-HEN-EXT.
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
           WHEN    "20"
               MOVE    "対象病名が２００件以上存在します。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "対象病名がありません"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "基準日を設定しました"
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
      *
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       900-COMB-READ-N-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-COMB-READ-N-EXT.
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
