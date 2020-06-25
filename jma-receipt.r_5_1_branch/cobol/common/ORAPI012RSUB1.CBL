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
       PROGRAM-ID.         ORAPI012RSUB1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者情報編集サブ
      *                      （地域連携など）
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  18/10/XX  妊婦設定から患者禁忌薬剤追加
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
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTPUBLIC            PIC 9(01).
           03  FLG-PTCARE-HKNINF       PIC 9(01).
           03  FLG-PTCARE-NINTEI       PIC 9(01).
           03  FLG-PTCONF              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-LABOR-SIO           PIC 9(01).
           03  FLG-PTMYNUMBER          PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTKINKI             PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CONFCHK             PIC 9(01).
           03  FLG-TENSU-OK            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
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
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
      *    割引率
           03  WRK-RATE-EDIT.
               05  WRK-RATE-EDITZ      PIC ZZZZZZZ.
               05  WRK-RATE-TANI       PIC X(04).
      *
      *    点数マスタ名称
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TNS-NAME            PIC X(80).
           03  WRK-TNS-YUKOEDYMD       PIC X(08).
           03  WRK-TNS-ENDDATE         PIC X(10).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    患者介護保険情報テーブル
       01  PTCARE-HKNINF-REC.
           COPY    "CPPTCARE-HKNINF.INC".
      *    患者介護認定者情報テーブル
       01  PTCARE-NINTEI-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *    患者個人番号テーブル
       01  PTMYNUMBER-REC.
           COPY    "CPPTMYNUMBER.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *H30.10
      *    患者禁忌薬剤マスタ
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *    API XML読み込み用定義体
           COPY    "CPORAPI012RSUB1.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORAPI012RSUB1AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    地域連携編集処理
           PERFORM 20022-PTNUMPUBLIC-HEN-SEC
      *
      *H26.5
      *    患者介護保険情報編集処理
           PERFORM 20022-PTCAREHKNINF-HEN-SEC
      *H26.5
      *    患者要介護情報編集処理
           PERFORM 20022-PTCARENINTEI-HEN-SEC
      *H26.5
      *    患者個別情報編集
           PERFORM 20022-PTCONF-HEN-SEC
      *H26.11
      *    最終来院日他編集
           PERFORM 20023-LASTYMDHEN-SEC
      *H26.12
      *    患者個人番号編集
           PERFORM 20023-PTMYNUMBER-HEN-SEC
      *
      *H30.10
      *    患者禁忌薬剤編集
           PERFORM 20024-KINKIYKZ-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    地域連携編集処理
      *****************************************************************
       20022-PTNUMPUBLIC-HEN-SEC       SECTION.
      *
           INITIALIZE                  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTPUBLIC-PTID
           PERFORM 920-PTPUBLIC-READ-SEC
      *
      *    地域連携ID
           MOVE    PTPUBLIC-PATIENT-ID1    TO  API12SUB-COMMUNITY-CID
      *    情報提供同意有無
           IF      PTPUBLIC-AGREEMENT  =   "1"
      *        同意する
               MOVE    "True"          TO  API12SUB-COMMUNITY-CID-AGREE
           ELSE
      *        同意しない
               MOVE    "False"         TO  API12SUB-COMMUNITY-CID-AGREE
           END-IF
           .
       20022-PTNUMPUBLIC-HEN-EXT.
           EXIT.
      *ver.4.7
      *H26.5
      *****************************************************************
      *    患者介護保険情報編集処理
      *****************************************************************
       20022-PTCAREHKNINF-HEN-SEC       SECTION.
      *
      *    患者介護保険情報
           MOVE    SPACE               TO  PTCARE-HKNINF-REC
           INITIALIZE                  PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTCRHKN-PTID
      *
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-HKNINF   =   1  )
                          OR  (IDX                 >=  10 )
               ADD     1                   TO  IDX
      *        保険者番号
               MOVE    PTCRHKN-HKNJANUM    TO
                                       API12SUB-CARE-INSURANCE-NUMBER
                                                             (IDX)
      *        被保険者番号
               MOVE    PTCRHKN-HIHKNJANUM  TO
                                       API12SUB-CARE-POLICYHOLDER
                                                             (IDX)
      *        開始日
               MOVE    PTCRHKN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-CARE-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRHKN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-CARE-END-DATE
                                                             (IDX)
      *
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20022-PTCAREHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者要介護情報編集処理
      *****************************************************************
       20022-PTCARENINTEI-HEN-SEC       SECTION.
      *
      *    患者介護保険情報
           INITIALIZE                  PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTCRNIN-PTID
      *
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCARE-NINTEI
               INITIALIZE                  PTCARE-NINTEI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-NINTEI   =   1  )
                          OR  (IDX                 >=  50 )
               ADD     1                   TO  IDX
      *        要介護状態
               MOVE    PTCRNIN-CAREJOTAICD TO
                                           API12SUB-NEED-CARE-STATE-CODE
                                                             (IDX)
      *        要介護状態名称編集
               PERFORM 200221-PTCARE-CARESTATE-SEC
      *        認定年月日
               MOVE    PTCRNIN-NINTEIYMD   TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-NEED-DATE
                                                             (IDX)
      *        開始日
               MOVE    PTCRNIN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-NEED-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRNIN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-NEED-END-DATE
                                                             (IDX)
      *
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20022-PTCARENINTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    要介護状態名称編集処理
      *****************************************************************
       200221-PTCARE-CARESTATE-SEC       SECTION.
      *
      *    要介護状態区分
           EVALUATE    PTCRNIN-CAREJOTAICD
               WHEN    "01"
                   MOVE    "自立"      TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "11"
                   MOVE    "要支援"    TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "12"
                   MOVE    "要支援１"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "13"
                   MOVE    "要支援２"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "21"
                   MOVE    "要介護１"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "22"
                   MOVE    "要介護２"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "23"
                   MOVE    "要介護３"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "24"
                   MOVE    "要介護４"  TO  API12SUB-NEED-CARE-STATE(IDX)
               WHEN    "25"
                   MOVE    "要介護５"  TO  API12SUB-NEED-CARE-STATE(IDX)
      *H30.9
               WHEN    "31"
                   MOVE    "事業対象者"
                                       TO  API12SUB-NEED-CARE-STATE(IDX)
           END-EVALUATE
           .
       200221-PTCARE-CARESTATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報編集処理
      *****************************************************************
       20022-PTCONF-HEN-SEC       SECTION.
      *
      *    地域包括診療対象疾病検索
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTCONF-PTID
           MOVE    "TIIKIHOKATU"       TO  PTCONF-CKEY
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-CONFCHK
           IF      FLG-PTCONF          =   ZERO
      *        地域包括診療対象疾病
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      PTCONF-CDATA(IDX:1) =   "1"
                       MOVE    "True"      TO  API12SUB-TARGET-DISEASE
                                                              (IDX)
                       MOVE    1           TO  FLG-CONFCHK
                   ELSE
                       MOVE    "False"     TO  API12SUB-TARGET-DISEASE
                                                              (IDX)
                   END-IF
               END-PERFORM
               IF      FLG-CONFCHK         =   ZERO
                   MOVE    SPACE       TO  API12SUB-COMMUNITY-DISEASE
               END-IF
           END-IF
      *
      *    患者個別情報
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTCONF-PTID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           PERFORM UNTIL       FLG-PTCONF      =   1
               EVALUATE    PTCONF-CKEY
               WHEN    "NINTIHOKATU"
      *            認知症地域包括診療
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    "True"      TO  API12SUB-NINTIHOKATU
                   END-IF
      *
               WHEN    "SYOUNIKAKARI"
      *            小児かかりつけ診療
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    "True"      TO  API12SUB-SYOUNIKAKARI
                   END-IF
               WHEN    "NINPUKBN"
      *            妊婦区分
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    "True"      TO  API12SUB-NINPUKBN
                   END-IF
      *
      *        指導料等自動算定
               WHEN    "JIDOSANTEI-1"
                   MOVE    PTCONF-CDATA(1:9)
                                           TO  API12SUB-AUTO-SRYCD(1)
                   MOVE    1               TO  IDX
                   PERFORM 200221-JIDOUSANTEI-HEN-SEC
      *
               WHEN    "JIDOSANTEI-2"
                   MOVE    PTCONF-CDATA(1:9)
                                           TO  API12SUB-AUTO-SRYCD(2)
                   MOVE    2               TO  IDX
                   PERFORM 200221-JIDOUSANTEI-HEN-SEC
               WHEN    "JIDOSANTEI-3"
                   MOVE    PTCONF-CDATA(1:9)
                                           TO  API12SUB-AUTO-SRYCD(3)
                   MOVE    3               TO  IDX
                   PERFORM 200221-JIDOUSANTEI-HEN-SEC
               END-EVALUATE
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20022-PTCONF-HEN-EXT.
           EXIT.
      *
      *H30.10
      *****************************************************************
      *    指導料等自動算定名称など編集処理
      *****************************************************************
       200221-JIDOUSANTEI-HEN-SEC               SECTION.
      *
           MOVE    API12SUB-AUTO-SRYCD(IDX)    TO  WRK-SRYCD
           PERFORM 400-TENSU-HENSYU-SEC
           MOVE    WRK-TNS-NAME        TO  API12SUB-AUTO-NAME(IDX)
           MOVE    WRK-TNS-ENDDATE     TO  API12SUB-AUTO-ENDYMD(IDX)
      *
           .
       200221-JIDOUSANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ名称など編集処理
      *****************************************************************
       400-TENSU-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    SPACE               TO  WRK-TNS-YUKOEDYMD
           MOVE    SPACE               TO  WRK-TNS-ENDDATE
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *    開始日の降順に検索
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           END-IF
           MOVE    ZERO                TO  FLG-TENSU-OK
           PERFORM UNTIL  (FLG-TENSU       =   1 ) OR
                          (FLG-TENSU-OK    =   1 )
               MOVE    TNS-NAME            TO  WRK-TNS-NAME
      *        最終履歴の終了日
               IF      WRK-TNS-YUKOEDYMD   =   SPACE
                   MOVE    TNS-YUKOEDYMD       TO  WRK-TNS-YUKOEDYMD
               END-IF
               IF     (TNS-YUKOSTYMD       <=  SPA-SYSYMD )
                   MOVE    1                   TO  FLG-TENSU-OK
               END-IF
      *
               IF      FLG-TENSU-OK        =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 910-TENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    有効終了日
           IF      WRK-TNS-YUKOEDYMD   NOT =  SPACE
               MOVE    WRK-TNS-YUKOEDYMD   TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-TNS-ENDDATE
           END-IF
      *
           .
       400-TENSU-HENSYU-EXT.
           EXIT.
      *
      *H26.11
      *****************************************************************
      *    最終来院日他編集処理
      *****************************************************************
       20023-LASTYMDHEN-SEC               SECTION.
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    API12SUB-PTID       TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *    入院中は病棟で判断
           IF      ORCSLASTYMD-BRMNUM  =   SPACE
               IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
      *            最終来院日
                   MOVE    ORCSLASTYMD-LASTYMD TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  API12SUB-LAST-DATE
               END-IF
           ELSE
      *        入院中
               MOVE    "1"                 TO  API12SUB-OUTPATIENT-CLASS
      *        入院日付
               MOVE    ORCSLASTYMD-NYUINYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-ADMISSION-DATE
      *        退院日
               IF      ORCSLASTYMD-LASTYMD     =   SPA-SYSYMD
                   MOVE    ORCSLASTYMD-LASTYMD TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO
                                           API12SUB-DISCHARGE-DATE
               END-IF
           END-IF
      *    初回来院日（受診履歴の１件目）
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    API12SUB-PTID       TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-FIRST-DATE
           END-IF
      *
           .
       20023-LASTYMDHEN-EXT.
           EXIT.
      *H26.12
      *****************************************************************
      *    患者個人番号編集処理
      *****************************************************************
       20023-PTMYNUMBER-HEN-SEC       SECTION.
      *
      *    地域包括診療対象疾病検索
           INITIALIZE                  PTMYNUMBER-REC
           MOVE    SPA-HOSPNUM         TO  PTMYNUM-HOSPNUM
           MOVE    API12SUB-PTID       TO  PTMYNUM-PTID
      *
           MOVE    PTMYNUMBER-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTMYNUMBER-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTMYNUMBER
               INITIALIZE                  PTMYNUMBER-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTMYNUMBER      =   1  )
                          OR  (IDX                 >=  20 )
               ADD     1                   TO  IDX
               MOVE    PTMYNUM-ID-KEY      TO  API12SUB-ID-KEY
                                                             (IDX)
               MOVE    PTMYNUM-MY-NUMBER   TO  API12SUB-MY-NUMBER
                                                             (IDX)
               MOVE    PTMYNUM-DESCRIPTION TO  API12SUB-DESCRIPTION
                                                             (IDX)
      *
               MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTMYNUMBER-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20023-PTMYNUMBER-HEN-EXT.
           EXIT.
      *
      *H30.10
      *****************************************************************
      *    患者禁忌薬剤編集処理
      *****************************************************************
       20024-KINKIYKZ-HEN-SEC                SECTION.
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTKINKI-PTID
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                      PTKINKI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  ( FLG-PTKINKI    =   1  )
                   OR     ( IDX            >   100)
               ADD    1                    TO  IDX
               IF     IDX                  <=  100
                   PERFORM 200131-PTKINKI-HEN-SEC
               END-IF
      *
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20024-KINKIYKZ-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    患者禁忌薬剤編集処理
      **************************************************************
       200131-PTKINKI-HEN-SEC           SECTION.
      *
      *    薬剤コード
           MOVE    PTKINKI-SRYCD       TO  API12SUB-DRUGCD-CODE
                                                             (IDX)
      *    点数マスタ検索
           MOVE    PTKINKI-SRYCD       TO  WRK-SRYCD
           PERFORM 400-TENSU-HENSYU-SEC
           MOVE    WRK-TNS-NAME        TO  API12SUB-DRUGCD-NAME
                                                             (IDX)
      *    有効終了日（マスタ終了日）
           MOVE    WRK-TNS-ENDDATE     TO  API12SUB-DRUGCD-END-DATE
                                                             (IDX)
      *    禁忌開始日
           IF      PTKINKI-KINKSTYMD   NOT =   SPACE
               MOVE    PTKINKI-KINKSTYMD   TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  API12SUB-START-DATE
                                                             (IDX)
           END-IF
           .
       20013-PTCARE-HKNINF-HENSYU-EXT.
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
      *    地域連携編情報取得
      *****************************************************************
       920-PTPUBLIC-READ-SEC              SECTION.
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
                   MOVE    ZERO                TO  FLG-PTPUBLIC
               ELSE
                   MOVE    1                   TO  FLG-PTPUBLIC
                   INITIALIZE                  PTPUBLIC-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTPUBLIC-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報データ読込処理
      *****************************************************************
       930-PTCONF-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCONF
               MOVE    MCPDATA-REC         TO  PTCONF-REC
           ELSE
               MOVE    1                   TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
      *
           .
       930-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護保険情報データデータ読込処理
      *****************************************************************
       930-PTCAREHKNINF-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-HKNINF
               MOVE    MCPDATA-REC         TO  PTCARE-HKNINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           .
       930-PTCAREHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護者認定情報データ読込処理
      *****************************************************************
       930-PTCARENINTEI-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-NINTEI
               MOVE    MCPDATA-REC         TO  PTCARE-NINTEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-NINTEI
               INITIALIZE                 PTCARE-NINTEI-REC
           END-IF
      *
           .
       930-PTCARENINTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
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
      *    患者個人番号情報データ読込処理
      *****************************************************************
       930-PTMYNUMBER-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTMYNUMBER
               MOVE    MCPDATA-REC         TO  PTMYNUMBER-REC
           ELSE
               MOVE    1                   TO  FLG-PTMYNUMBER
               INITIALIZE                  PTMYNUMBER-REC
           END-IF
      *
           .
       930-PTMYNUMBER-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者禁忌薬剤マスター読込処理
      *****************************************************************
       900-PTKINKI-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTKINKI
               MOVE    MCPDATA-REC         TO  PTKINKI-REC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                  PTKINKI-REC
           END-IF
      *
           .
       900-PTKINKI-READ-EXT.
           EXIT.
      *
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
