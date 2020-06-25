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
       PROGRAM-ID.         ORAPI012R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者基本情報取得
      *                      （version2　対応）
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  13/12/11  地域連携ＩＤ等対応
      * 04.07.00     NACL-多々納  14/05/13  介護情報他追加対応
      * 04.07.00     NACL-多々納  14/11/XX  初回最終来院日他追加対応
      * 04.07.00     NACL-多々納  15/01/XX  労災保険編集追加対応
      * 04.08.00     NACL-多々納  16/10/XX  勤務先から帰省先を追加対応
      * 04.08.00     NACL-多々納  16/12/22  患者個人番号追加対応
      * 04.08.00     NACL-多々納  17/04/XX  入金方法、状態等追加
      *                                     保険組合せ非表示追加
      * 04.08.00     NACL-多々納  17/05/XX  テスト患者・死亡区分追加
      * 04.08.00     NACL-多々納  18/03/XX  妊婦設定追加
      * 05.00.00     NACL-多々納  18/10/XX  認知症地域包括設定など追加
      * 05.00.00     NACL-多々納  19/06/XX  保険資格取得日など追加
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTPUBLIC            PIC 9(01).
           03  FLG-PTCARE-HKNINF       PIC 9(01).
           03  FLG-PTCARE-NINTEI       PIC 9(01).
           03  FLG-PTCONF              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-LABOR-SIO           PIC 9(01).
           03  FLG-PTMYNUMBER          PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CONFCHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDD                     PIC 9(04).
           03  IDK                     PIC 9(04).
           03  IDH                     PIC 9(04).
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
      *    API XML読み込み用定義体
           COPY    "CPPATIENTXMLV2RES.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    年齢・負担割合サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *H30.10
      *    患者地域連携等編集パラメタ
           COPY    "CPORAPI012RSUB1.INC".
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
           COPY  "CPSK4002.INC".
      *    状態情報
           COPY    "CPSK1018.INC".
      *    減免事由情報
           COPY    "CPSK1026.INC".
      *    割引率情報
           COPY    "CPSK1027.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *    患者個人番号テーブル
       01  PTMYNUMBER-REC.
           COPY    "CPPTMYNUMBER.INC".
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
               MOVE   404                  TO  APIPTA-HTTPSTATUS
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
           INITIALIZE                      XML-PATIENTINFORES
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
           MOVE    APIPTA-PTNUM       TO  WRK-PTNUM
      *    XML情報取り出し
      *    PERFORM 1000-PTIXMLREAD-SEC
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      200-MAIN-EXT
      *    END-IF
      *
           MOVE    ZERO                TO  PTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
      *
      *    患者情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-PTINF-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           .
       2001-INPUT-CHK-EXT.
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
           MOVE    SPTID-PTNUM         TO  PTRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
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
      *    患者基本情報取得
      *****************************************************************
       2002-PTINF-HENSYU-SEC           SECTION.
      *
           MOVE    PTINF-NAME          TO  PTRES-NAME
           MOVE    PTINF-KANANAME      TO  PTRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  PTRES-SEX
           MOVE    PTINF-HOME-POST     TO  PTRES-HOME-ZIP-CODE
           MOVE    PTINF-HOME-ADRS     TO  PTRES-HOME-ADDRESSES1
           MOVE    PTINF-HOME-BANTI    TO  PTRES-HOME-ADDRESSES2
           MOVE    PTINF-HOME-TEL1     TO  PTRES-HOME-HPONE1
           MOVE    PTINF-HOME-TEL2     TO  PTRES-HOME-HPONE2
      *
      *    世帯主名
           MOVE    PTINF-SETAINUSI     TO  PTRES-SETAI-NAME
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  PTRES-ZOKUGARA
      *    禁忌
           MOVE    PTINF-TABOO-1       TO  PTRES-TABOO-1
           MOVE    PTINF-TABOO-2       TO  PTRES-TABOO-2
      *    アレルギー
           MOVE    PTINF-ALLERGY-1     TO  PTRES-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  PTRES-ALLERGY-2
      *    感染症
           MOVE    PTINF-KANSENSYO-1   TO  PTRES-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  PTRES-KANSENSYO-2
      *    コメント
           MOVE    PTINF-COMMENT-1     TO  PTRES-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  PTRES-COMMENT-2
      *H28.10
      *    勤務先情報
           MOVE    PTINF-OFFICE-NAME   TO  PTRES-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  PTRES-OFFICE-ZIP-CODE
           MOVE    PTINF-OFFICE-ADRS   TO  PTRES-OFFICE-ADDRESSES1
           MOVE    PTINF-OFFICE-BANTI  TO  PTRES-OFFICE-ADDRESSES2
           MOVE    PTINF-OFFICE-TEL    TO  PTRES-OFFICE-HPONE
      *    連絡先情報
           MOVE    PTINF-RENRAKU-NAME  TO  PTRES-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  PTRES-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  PTRES-RENRAKU-ZIP-CODE
           MOVE    PTINF-RENRAKU-ADRS  TO  PTRES-RENRAKU-ADDRESSES1
           MOVE    PTINF-RENRAKU-BANTI TO  PTRES-RENRAKU-ADDRESSES2
           MOVE    PTINF-RENRAKU-TEL1  TO  PTRES-RENRAKU-HPONE1
           MOVE    PTINF-RENRAKU-TEL2  TO  PTRES-RENRAKU-HPONE2
      *    帰省先
           MOVE    PTINF-KISEI-NAME    TO  PTRES-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  PTRES-KISEI-ZIP-CODE
           MOVE    PTINF-KISEI-ADRS    TO  PTRES-KISEI-ADDRESSES1
           MOVE    PTINF-KISEI-BANTI   TO  PTRES-KISEI-ADDRESSES2
           MOVE    PTINF-KISEI-TEL     TO  PTRES-KISEI-HPONE
      *H29.5
      *    テスト患者番号区分
           MOVE    PTINF-TSTPTNUMKBN   TO  PTMODRES-TEST-PATIENT
           IF      PTINF-TSTPTNUMKBN   =   SPACE
               MOVE    "0"                 TO  PTMODRES-TEST-PATIENT
           END-IF
      *    死亡区分
           MOVE    PTINF-DEATHKBN      TO  PTMODRES-DEATHKBN
      *H29.4
      *    通称名
           MOVE    PTINF-NICKNAME      TO  PTRES-NICKNAME
      *    職業
           MOVE    PTINF-JOB           TO  PTRES-JOB
      *    ＦＡＸ番号
           MOVE    PTINF-FAX           TO  PTRES-FAX
      *    携帯
           MOVE    PTINF-KEITAI-TEL    TO  PTRES-KEITAI-TEL
      *    メール
           MOVE    PTINF-EMAIL         TO  PTRES-EMAIL
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  PTRES-DISCOUNTKBN
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1026"              TO  SYS-KANRICD
           MOVE    PTINF-DISCOUNT-KBN  TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1026-REC
               MOVE    SYS-1026-DISCOUNT-BODY
                                           TO  PTRES-DISCOUNTKBN-NAME
           END-IF
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  PTRES-DISCOUNT
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1027"              TO  SYS-KANRICD
           MOVE    PTINF-DISCOUNT      TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEYREAD-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1027-REC
               MOVE    SPACE               TO  WRK-RATE-EDIT
               MOVE    SYS-1027-DISCOUNT-RATE
                                       TO  WRK-RATE-EDITZ
               EVALUATE    SYS-1027-TEIRITU
               WHEN    "0"
                   MOVE    "該当なし"      TO  PTRES-DISCOUNT-NAME
               WHEN    "1"
                   MOVE    "(%)"           TO  WRK-RATE-TANI
                   MOVE    WRK-RATE-EDIT   TO  PTRES-DISCOUNT-NAME
               WHEN    "2"
                   MOVE    "(円)"          TO  WRK-RATE-TANI
                   MOVE    WRK-RATE-EDIT   TO  PTRES-DISCOUNT-NAME
               END-EVALUATE
           END-IF
      *    状態１〜３
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3        )
               INITIALIZE                      SYSKANRI-REC
               EVALUATE    IDX
               WHEN    1
                   MOVE    "1018"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-1   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-1   TO  PTRES-CONDITION-KBN
                                                          (IDX)
               WHEN    2
                   MOVE    "1019"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-2   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-2   TO  PTRES-CONDITION-KBN
                                                          (IDX)
               WHEN    3
                   MOVE    "1020"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-3   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-3   TO  PTRES-CONDITION-KBN
                                                          (IDX)
               END-EVALUATE
               MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1018-REC
                   MOVE    SYS-1018-CONDITION-BODY
                                               TO  PTRES-CONDITION-NAME
                                                           (IDX)
               END-IF
           END-PERFORM
      *    入金方法
           MOVE    PTINF-NYUKIN-HOHO   TO  PTRES-IC-CODE
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    PTINF-NYUKIN-HOHO   TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1041-REC
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  PTRES-IC-CODE-NAME
           END-IF
      *
      *    保険種類変換処理
           PERFORM 200210-HKNCOMBI-HEN-SEC
      *
      *H30.10
      *    地域連携編集から禁忌薬剤まで編集
           PERFORM 2002-KYOUTU-HEN-SEC
      *
           GO      TO  2002-PTINF-HENSYU-EXT
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
           .
       2002-PTINF-HENSYU-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    地域連携から禁忌薬剤まで編集処理
      *****************************************************************
        2002-KYOUTU-HEN-SEC       SECTION.
      *
      *
           INITIALIZE                      ORAPI012RSUB1AREA
           MOVE    PTINF-PTID          TO  API12SUB-PTID
           CALL    "ORAPI012RSUB1"     USING
                                       SPA-AREA
                                       ORAPI012RSUB1AREA
      *
      *    地域連携ID
           MOVE    API12SUB-COMMUNITY-CID
                                       TO  PTRES-COMMUNITY-CID
      *    情報提供同意有無
           MOVE    API12SUB-COMMUNITY-CID-AGREE
                                       TO  PTRES-COMMUNITY-CID-AGREE
      *
      *    初回来院日（受診履歴の１件目）
           MOVE    API12SUB-FIRST-DATE TO  PTRES-FIRST-DATE
      *    最終来院日（退院日）
           MOVE    API12SUB-LAST-DATE  TO  PTRES-LAST-DATE
      *    入院中区分（１：入院中）
           MOVE    API12SUB-OUTPATIENT-CLASS
                                       TO  PTRES-OUTPATIENT-CLASS
      *    入院日（入院中のみ）
           MOVE    API12SUB-ADMISSION-DATE
                                       TO  PTRES-ADMISSION-DATE
      *    退院日（当日退院時のみ）
           MOVE    API12SUB-DISCHARGE-DATE
                                       TO  PTRES-DISCHARGE-DATE
      *
      *    患者介護保険情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    API12SUB-CARE-INSURANCE-NUMBER (IDX)
                                       TO  PTRES-CARE-INSURANCE-NUMBER
                                                              (IDX)
               MOVE    API12SUB-CARE-POLICYHOLDER     (IDX)
                                       TO  PTRES-CARE-POLICYHOLDER
                                                              (IDX)
               MOVE    API12SUB-CARE-START-DATE       (IDX)
                                       TO  PTRES-CARE-START-DATE
                                                              (IDX)
               MOVE    API12SUB-CARE-END-DATE         (IDX)
                                       TO  PTRES-CARE-END-DATE
                                                              (IDX)
           END-PERFORM
      *
      *    要介護認定情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
      *        要介護状態区分
               MOVE    API12SUB-NEED-CARE-STATE-CODE (IDX)
                                       TO  PTRES-NEED-CARE-STATE-CODE
                                                               (IDX)
      *        要介護状態名称
               MOVE    API12SUB-NEED-CARE-STATE (IDX)
                                       TO  PTRES-NEED-CARE-STATE(IDX)
      *        認定日付
               MOVE    API12SUB-NEED-DATE (IDX)
                                       TO  PTRES-NEED-DATE     (IDX)
      *        有効開始日
               MOVE    API12SUB-NEED-START-DATE (IDX)
                                       TO  PTRES-NEED-START-DATE(IDX)
      *        有効終了日
               MOVE    API12SUB-NEED-END-DATE (IDX)
                                       TO  PTRES-NEED-END-DATE (IDX)
           END-PERFORM
      *
      *    地域包括診療対象疾病
           MOVE    API12SUB-COMMUNITY-DISEASE
                                       TO  PTRES-COMMUNITY-DISEASE
      *    患者個人番号情報
           MOVE    API12SUB-PTMYNUMBER-G
                                       TO  PTRES-PTMYNUMBER-G
      *
      *    妊婦加算
           MOVE    API12SUB-NINPUKBN   TO  PTRES-NINPUKBN
      *    認知症地域包括診療
           MOVE    API12SUB-NINTIHOKATU    TO  PTRES-NINTIHOKATU
      *    小児かかりつけ診療
           MOVE    API12SUB-SYOUNIKAKARI
                                       TO  PTRES-SYOUNIKAKARI
      *
      *    管理料等自動算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    API12SUB-AUTO-SRYCD(IDX)
                                       TO  PTRES-AUTO-SRYCD (IDX)
               MOVE    API12SUB-AUTO-NAME(IDX)
                                       TO  PTRES-AUTO-NAME  (IDX)
               MOVE    API12SUB-AUTO-ENDYMD(IDX)
                                       TO  PTRES-AUTO-ENDYMD(IDX)
           END-PERFORM
      *
      *    患者禁忌薬剤
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100  )
                           OR (API12SUB-DRUGCD-CODE (IDX)  =   SPACE )
      *        禁忌薬剤コード
               MOVE    API12SUB-DRUGCD-CODE (IDX)
                                       TO  PTRES-DRUGCD-CODE(IDX)
               MOVE    API12SUB-DRUGCD-NAME (IDX)
                                       TO  PTRES-DRUGCD-NAME(IDX)
      *        有効終了日（マスタ終了日）
               MOVE    API12SUB-DRUGCD-END-DATE (IDX)
                                       TO  PTRES-DRUGCD-END-DATE
                                                            (IDX)
      *        禁忌開始日
               MOVE    API12SUB-START-DATE (IDX)
                                       TO  PTRES-START-DATE
                                                            (IDX)
           END-PERFORM
      *
           .
        2002-KYOUTU-HEN-EXT.
           EXIT.
      *
      *!!!!!!!!!!!!!! 以下　不使用（H30.10 から）
      *ver.4.7
      *****************************************************************
      *    地域連携編集処理
      *****************************************************************
       20022-PTNUMPUBLIC-HEN-SEC       SECTION.
      *
           INITIALIZE                  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    PTINF-PTID          TO  PTPUBLIC-PTID
           PERFORM 920-PTPUBLIC-READ-SEC
      *
      *    地域連携ID
           MOVE    PTPUBLIC-PATIENT-ID1    TO  PTRES-COMMUNITY-CID
      *    情報提供同意有無
           IF      PTPUBLIC-AGREEMENT  =   "1"
      *        同意する
               MOVE    "True"              TO  PTRES-COMMUNITY-CID-AGREE
           ELSE
      *        同意しない
               MOVE    "False"             TO  PTRES-COMMUNITY-CID-AGREE
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
           MOVE    PTINF-PTID          TO  PTCRHKN-PTID
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
               ADD     1                       TO  IDX
      *        保険者番号
               MOVE    PTCRHKN-HKNJANUM    TO
                                           PTRES-CARE-INSURANCE-NUMBER
                                                             (IDX)
      *        被保険者番号
               MOVE    PTCRHKN-HIHKNJANUM  TO  PTRES-CARE-POLICYHOLDER
                                                             (IDX)
      *        開始日
               MOVE    PTCRHKN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-CARE-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRHKN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-CARE-END-DATE
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
           MOVE    PTINF-PTID          TO  PTCRNIN-PTID
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
                                           PTRES-NEED-CARE-STATE-CODE
                                                             (IDX)
      *        要介護状態名称編集
               PERFORM 200221-PTCARE-CARESTATE-SEC
      *        認定年月日
               MOVE    PTCRNIN-NINTEIYMD   TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-NEED-DATE
                                                             (IDX)
      *        開始日
               MOVE    PTCRNIN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-NEED-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRNIN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-NEED-END-DATE
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
                   MOVE    "自立"      TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "11"
                   MOVE    "要支援"    TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "12"
                   MOVE    "要支援１"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "13"
                   MOVE    "要支援２"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "21"
                   MOVE    "要介護１"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "22"
                   MOVE    "要介護２"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "23"
                   MOVE    "要介護３"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "24"
                   MOVE    "要介護４"  TO  PTRES-NEED-CARE-STATE (IDX)
               WHEN    "25"
                   MOVE    "要介護５"  TO  PTRES-NEED-CARE-STATE (IDX)
      *H30.7
               WHEN    "31"
                   MOVE    "事業対象者"
                                       TO  PTRES-NEED-CARE-STATE (IDX)
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
           MOVE    PTINF-PTID          TO  PTCONF-PTID
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
                       MOVE    "True"      TO  PTRES-TARGET-DISEASE
                                                              (IDX)
                       MOVE    1           TO  FLG-CONFCHK
                   ELSE
                       MOVE    "False"     TO  PTRES-TARGET-DISEASE
                                                              (IDX)
                   END-IF
               END-PERFORM
               IF      FLG-CONFCHK         =   ZERO
                   MOVE    SPACE           TO  PTRES-COMMUNITY-DISEASE
               END-IF
           END-IF
      *
      *H30.4
      *    妊婦設定
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCONF-PTID
           MOVE    "NINPUKBN"          TO  PTCONF-CKEY
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
           IF      FLG-PTCONF          =   ZERO
               IF      PTCONF-CDATA(1:1)   =   "1"
                   MOVE    "True"          TO  PTRES-NINPUKBN
               END-IF
           END-IF
      *
           .
       20022-PTCONF-HEN-EXT.
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
           MOVE    SPA-PTID            TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *    入院中は病棟で判断
           IF      ORCSLASTYMD-BRMNUM  =   SPACE
               IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
      *            最終来院日
                   MOVE    ORCSLASTYMD-LASTYMD TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  PTRES-LAST-DATE
               END-IF
           ELSE
      *        入院中
               MOVE    "1"                 TO  PTRES-OUTPATIENT-CLASS
      *        入院日付
               MOVE    ORCSLASTYMD-NYUINYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-ADMISSION-DATE
      *        退院日
               IF      ORCSLASTYMD-LASTYMD     =   SPA-SYSYMD
                   MOVE    ORCSLASTYMD-LASTYMD TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  PTRES-DISCHARGE-DATE
               END-IF
           END-IF
      *    初回来院日（受診履歴の１件目）
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
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
               MOVE    WRK-HEN-DATE        TO  PTRES-FIRST-DATE
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
           MOVE    PTINF-PTID          TO  PTMYNUM-PTID
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
               MOVE    PTMYNUM-ID-KEY      TO  PTRES-ID-KEY
                                                             (IDX)
               MOVE    PTMYNUM-MY-NUMBER   TO  PTRES-MY-NUMBER
                                                             (IDX)
               MOVE    PTMYNUM-DESCRIPTION TO  PTRES-DESCRIPTION
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
      *!!!! ここまで　　!!!!
      *
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
               MOVE    "処理終了"          TO  PTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PTRES-API-RESULT-MSG
           END-IF
      *
           IF      APIPTA-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientinfov2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "patientinfores"    TO  PTRES-RECORDNAME
      *ver.5.0
      **** MOVE    1                   TO  PTRES-MODE
           MOVE    2                   TO  PTRES-MODE
      *ver.5.0
           IF      APIPTA-FORMAT      =   "json"
               MOVE    3                   TO  PTRES-MODE
               MOVE    "application/json"  TO  APIPTA-CONTENT-TYPE
           END-IF
      *
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTINFORES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_patientinfov2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTINFORES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
      *
               MOVE    PTRES-OBJECT        TO  APIPTA-BODY 
               MOVE    "application/xml"   TO  APIPTA-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       200210-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
           MOVE    "1"                 TO  ORCSAPIHKN-KBN3
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
      *    出力領域保険情報編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
               PERFORM 2002101-HKNHEN-TBL-SEC
           END-PERFORM
           .
       200210-HKNCOMBI-HEN-EXT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
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
      *    枝番
           MOVE    ORCSAPIHKN-EDABAN  (IDX)
                                   TO  PTRES-MAIN-EDABAN  (IDX)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  PTRES-MAIN-CONTINUATION (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  PTRES-MAIN-ASSISTANCE  (IDX)
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
      *R01.6
      *    資格取得日
           MOVE    ORCSAPIHKN-SKKGETYMD (IDX)
                                   TO  PTRES-MAIN-GET-DATE   (IDX)
      *    確認年月日
           MOVE    ORCSAPIHKN-KAKUNINYMD  (IDX)
                                   TO  PTRES-MAIN-CHECK-DATE (IDX)
      *
      *    保険組合せ
           MOVE    ORCSAPIHKN-THKNCOMBI  (IDX)
                                   TO  PTRES-COMBINATION-NUMBER (IDX)
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  PTRES-MAIN-ASSISTANCE-NAME (IDX)
      *H26.11
      *    保険組合せ負担割合
           PERFORM 20021011-HKNCOMBI-FTNHEN-SEC
      *H29.4
      *    保険組合せ非表示
           EVALUATE    ORCSAPIHKN-HYOJIKBN (IDX)
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
      *
      *H27.1
      *    労災・自賠責・第三者行為編集
           IF     (ORCSAPIHKN-HKNNUM(IDX)      =   "971"  OR  "973" )
               OR (ORCSAPIHKN-KOHNUM(IDX 1)    =   "970"    )
               PERFORM 20021011-RSIINF-HEN-SEC
           END-IF
           .
       2002101-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ負担割合編集処理
      *****************************************************************
       20021011-HKNCOMBI-FTNHEN-SEC                SECTION.
      *
      *    入院
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           MOVE    ORCSAPIHKN-THKNCOMBI  (IDX)
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           COMPUTE WRK-FTNRATE         =   AGECHK-FTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  PTRES-COMBINATION-RATE-ADM
                                                        (IDX)
      *    外来
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           MOVE    ORCSAPIHKN-THKNCOMBI  (IDX)
                                       TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           COMPUTE WRK-FTNRATE         =   AGECHK-FTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  PTRES-COMBINATION-RATE-OUT
                                                        (IDX)
      *
           .
       20021011-HKNCOMBI-FTNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災・自賠責・第三者行為編集処理
      *****************************************************************
       20021011-RSIINF-HEN-SEC                SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           IF      ORCSAPIHKN-HKNNUM(IDX)  =   "971"  OR  "973"
               MOVE    ORCSAPIHKN-NAI-HKNID (IDX)
                                           TO  PTRSI-HKNID
           ELSE
               MOVE    ORCSAPIHKN-NAI-KOHID (IDX 1)
                                           TO  PTRSI-KOHID
           END-IF
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTRSIINF        =   1
               GO      TO    20021011-RSIINF-HEN-EXT
           END-IF
      *    労災保険名称
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短期給付"          TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "2"
                   MOVE    "傷病年金"          TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "3"
                   MOVE    "アフターケア"      TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "4"
                   MOVE    "自賠責保険"        TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "5"
                   MOVE    "公務災害"          TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "6"
                   MOVE    "第三者行為"        TO  PTRES-ACCIDENT-NAME
                                                                  (IDX)
           END-EVALUATE
      *    傷病の部位
           MOVE    PTRSI-COMMENT       TO  PTRES-DISEASE-LOCATION(IDX)
      *    傷病年月日
           IF      PTRSI-SHOBYOYMD NOT =   SPACE
               MOVE    PTRSI-SHOBYOYMD     TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTRES-DISEASE-DATE (IDX)
           END-IF
      *
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
      *    労働保険番号
                   MOVE    PTRSI-KOFUNUM   TO  PTRES-ACCIDENT-NUMBER
                                                                (IDX)
               WHEN    "2"
      *    年金証書番号
                   MOVE    PTRSI-KOFUNUM   TO  PTRES-PENSIONCERTIFICATE
                                                                (IDX)
               WHEN    "3"
      *    健康管理手帳番号
                   MOVE    PTRSI-KOFUNUM   TO
                                           PTRES-RSI-KANRITCHO-NUMBER
                                                                (IDX)
           END-EVALUATE
      *    災害区分（名称）
           EVALUATE    PTRSI-SAIGAIKBN
               WHEN    "1"
                   MOVE    "業務中の災害"      TO  PTRES-ACCIDENT-CLASS
                                                                (IDX)
               WHEN    "2"
                   MOVE    "通勤途上の災害"    TO  PTRES-ACCIDENT-CLASS
                                                                (IDX)
           END-EVALUATE
      *    労働基準監督署コード
           MOVE    PTRSI-SYOCD         TO  PTRES-LABOR-ST-CODE  (IDX)
           IF      PTRSI-SYOCD     NOT =   SPACE
               PERFORM 20021012-SYOCD-HEN-SEC
           END-IF
      *
      *    事業所名称
           MOVE    PTRSI-JIGYOUNAME    TO  PTRES-RSI-JIGYOUNAME (IDX)
      *    所在地都道府県名
           MOVE    PTRSI-PREFNAME      TO  PTRES-RSI-PREFNAME (IDX)
      *    所在地都道府県区分
           MOVE    PTRSI-PREFKBN       TO  PTRES-RSI-PREFKBN (IDX)
           EVALUATE    PTRSI-PREFKBN
               WHEN    "1"
                   MOVE    "都"        TO  PTRES-RSI-PREFKBNNAME (IDX)
               WHEN    "2"
                   MOVE    "道"        TO  PTRES-RSI-PREFKBNNAME (IDX)
               WHEN    "3"
                   MOVE    "府"        TO  PTRES-RSI-PREFKBNNAME (IDX)
               WHEN    "4"
                   MOVE    "県"        TO  PTRES-RSI-PREFKBNNAME (IDX)
           END-EVALUATE
      *    所在地郡市区名
           MOVE    PTRSI-CITYNAME      TO  PTRES-RSI-CITYNAME (IDX)
      *    所在地郡市区区分
           MOVE    PTRSI-CITYKBN       TO  PTRES-RSI-CITYKBN (IDX)
           EVALUATE    PTRSI-CITYKBN
               WHEN    "1"
                   MOVE    "郡"        TO  PTRES-RSI-CITYKBNNAME (IDX)
               WHEN    "2"
                   MOVE    "市"        TO  PTRES-RSI-CITYKBNNAME (IDX)
               WHEN    "3"
                   MOVE    "区"        TO  PTRES-RSI-CITYKBNNAME (IDX)
           END-EVALUATE
      *    自賠責保険会社名
           MOVE    PTRSI-HOKENNAME     TO  PTRES-RSI-HOKENNAME (IDX)
      *
      *    アフターケア
           EVALUATE    PTRSI-HKNKBN
           WHEN    "3"
      *        損傷区分
               STRING  PTRSI-SONSHOUKBN
                       PTRSI-SONSHOUKBN-EDANUM
                                           DELIMITED  BY  SIZE
                                           INTO   PTRES-DAMAGE-CLASS
                                                                (IDX)
               END-STRING
      *        損傷区分名称取得
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "4002"              TO  SYS-KANRICD
               MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    PTRES-DAMAGE-CLASS (IDX)
                                           TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-4002-REC
                   MOVE    SYS-4002-SONSHOUNAME
                                           TO  PTRES-DAMAGE-CLASS-NAME
                                                                (IDX)
               END-IF
           END-EVALUATE
           .
       20021011-RSIINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労働基準監督署名称編集処理
      *****************************************************************
       20021012-SYOCD-HEN-SEC                SECTION.
      *
           INITIALIZE                      LABOR-SIO-REC
           MOVE    PTRSI-SYOCD         TO  LABOR-SIO-SYOCD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOSTYMD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOEDYMD
      *
           MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_labor_sio"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  LABOR-SIO-REC
                   MOVE    ZERO                TO  FLG-LABOR-SIO
               ELSE
                   MOVE    1                   TO  FLG-LABOR-SIO
                   INITIALIZE                  LABOR-SIO-REC
               END-IF
           ELSE
               INITIALIZE                  LABOR-SIO-REC
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LABOR-SIO       =   ZERO
               MOVE    LABOR-SIO-NAME      TO  PTRES-LABOR-ST-NAME (IDX)
           END-IF
           .
       20021012-SYOCD-HEN-EXT.
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
           WHEN    "01"
      ******** MOVE    "患者番号未設定"
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
     ********* MOVE    "該当患者番号なし"
               MOVE    "患者番号に該当する患者が存在しません"
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
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC        SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
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
