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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSP04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者登録画面編集、他院患者情報複写サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/01/26    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
      *
           03  FLG-HEN-OK          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PTID            PIC 9(10).
           03  WRK-PTNUM           PIC X(20).
      *
           03  WRK-RATE-AREA.
               05  WRK-RATE-KBN        PIC X(02).
               05  WRK-RATE-SP         PIC X(01).
               05  WRK-RATE-EDIT.
                   07  WRK-RATE-EDITZ  PIC ZZZZZZZ.
                   07  WRK-RATE-TANI   PIC X(04).
               05  WRK-RATE-EDIT-R  REDEFINES    WRK-RATE-EDIT.
                   07  WRK-RATE-EDITR  PIC X(11).
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
           03  WRK-KBNCD               PIC X(02).
      *   年齢表示
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ−
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
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    パラメタ
           COPY    "CPORCSP04.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSP04AREA
           SPA-P02SCRAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
      *
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *
      *    主処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC        SECTION.
      *
      *    患者情報検索
           MOVE    ORCSP04-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    ORCSP04-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "1001"              TO  SPA-ERRCD
           END-IF
      *    パラメタファイルのクリア
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-PARA-DEL-SEC
           END-IF
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       100-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       100-PARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号発行処理
      *****************************************************************
       200-MAIN-SYORI-SEC               SECTION.
      *
      *    患者情報編集
           PERFORM 2001-PTINF-SET-SEC
      *    他の情報編集
           PERFORM 2002-PARA-SET-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-NAI-GRPAREA-G
               MOVE    ORCSP04-HOSPNUM     TO  SPA-NAI-GRPHOSPNUM
               MOVE    ORCSP04-PTID        TO  SPA-NAI-GRPPTID
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       2001-PTINF-SET-SEC          SECTION.
      *
      *    複写しない項目をクリアする
      *    減免事由
           MOVE    SPACE               TO  PTINF-DISCOUNT-KBN
           MOVE    SPACE               TO  PTINF-DISCOUNT
      *    状態
           MOVE    SPACE               TO  PTINF-CONDITION-1
           MOVE    SPACE               TO  PTINF-CONDITION-2
           MOVE    SPACE               TO  PTINF-CONDITION-3
      *    入金方法
           MOVE    SPACE               TO  PTINF-NYUKIN-HOHO
      *    請求書発行区分
           MOVE    SPACE               TO  PTINF-SKYPRTFLG
      *
      *    画面編集
           MOVE    PTINF-NAME          TO  SPA-GMN-NAMEH
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEXH
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEXH
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-BIRTHDAYH
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
      *
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *    変更確認用氏名保存
           MOVE    SPA-GMN-KANANAME    TO  SPA-GMN-OLD-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-GMN-OLD-NAME
      *    氏名変更
           MOVE    PTINF-NAME          TO  SPA-HZN-NAME
           MOVE    ZERO                TO  SPA-HEN-FLG
      *    旧姓
           MOVE    PTINF-NAME          TO  SPA-OLD-NAME
           MOVE    PTINF-KANANAME      TO  SPA-OLD-KANANAME
           MOVE    PTINF-NICKNAME      TO  SPA-OLD-NICKNAME
      *
           MOVE    SPA-GMN-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYW
      *    性別
           MOVE    PTINF-SEX           TO  SPA-GMN-SEX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-SEX     =   SPA-GMN-SEX-LST(IDX)(1:1)
                   MOVE    SPA-GMN-SEX-LST (IDX)   TO  SPA-GMN-SEX-G
                   MOVE    2                       TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-NICKNAME      TO  SPA-GMN-NICKNAME
           MOVE    PTINF-DEATHKBN      TO  SPA-GMN-DEATHKBN
           IF      SPA-GMN-DEATHKBN    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-DEATHKBN
           END-IF
      *    テスト患者
           MOVE    PTINF-TSTPTNUMKBN   TO  SPA-GMN-TSTPTNUMKBN
           IF      SPA-GMN-TSTPTNUMKBN =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TSTPTNUMKBN
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-TSTPTNUMKBN =   SPA-GMN-TSTPTNUMKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-TSTPTNUMKBN-LST (IDX)
                                           TO  SPA-GMN-TSTPTNUMKBN-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-HOME-POST     TO  SPA-GMN-POST
           MOVE    PTINF-HOME-ADRS     TO  SPA-GMN-EDITADRS-NAME
           MOVE    PTINF-HOME-BANTI    TO  SPA-GMN-BANTI
           MOVE    PTINF-HOME-TEL1     TO  SPA-GMN-TEL1
           MOVE    PTINF-HOME-TEL2     TO  SPA-GMN-TEL2
           MOVE    PTINF-SETAINUSI     TO  SPA-GMN-SETAINUSI
           MOVE    PTINF-ZOKUGARA      TO  SPA-GMN-ZOKUGARA
           MOVE    PTINF-COMMENT-1     TO  SPA-GMN-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  SPA-GMN-COMMENT-2
      *    年齢計算
           IF      SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO
               MOVE    SPACE               TO  SPA-GMN-NENREI
           ELSE
               PERFORM 410121-NENREI-SET-SEC
           END-IF
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  SPA-GMN-DISCOUNTKBN
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
      *    状態１〜３
           MOVE    PTINF-CONDITION-1   TO  SPA-GMN-CONDITION(1)
           MOVE    PTINF-CONDITION-2   TO  SPA-GMN-CONDITION(2)
           MOVE    PTINF-CONDITION-3   TO  SPA-GMN-CONDITION(3)
      *    入金方法
           MOVE    PTINF-NYUKIN-HOHO   TO  SPA-GMN-NYUKIN-HOHO
      *    請求書発行区分
           MOVE    PTINF-SKYPRTFLG     TO  SPA-GMN-SKYPRTFLG
      *
      *    連絡先
           MOVE    PTINF-FAX           TO  SPA-GMN21-FAX
           MOVE    PTINF-KEITAI-TEL    TO  SPA-GMN21-KEITAI-TEL
           MOVE    PTINF-EMAIL         TO  SPA-GMN21-EMAIL
           MOVE    PTINF-JOB           TO  SPA-GMN21-JOB
           MOVE    PTINF-RENRAKU-NAME  TO  SPA-GMN21-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  SPA-GMN21-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  SPA-GMN21-RENRAKU-POST
           MOVE    PTINF-RENRAKU-ADRS  TO  SPA-GMN21-RENRAKU-ADRS
           MOVE    PTINF-RENRAKU-BANTI TO  SPA-GMN21-RENRAKU-BANTI
           MOVE    PTINF-RENRAKU-TEL1  TO  SPA-GMN21-RENRAKU-TEL1
           MOVE    PTINF-RENRAKU-TEL2  TO  SPA-GMN21-RENRAKU-TEL2
           MOVE    PTINF-OFFICE-NAME   TO  SPA-GMN21-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  SPA-GMN21-OFFICE-POST
           MOVE    PTINF-OFFICE-ADRS   TO  SPA-GMN21-OFFICE-ADRS
           MOVE    PTINF-OFFICE-BANTI  TO  SPA-GMN21-OFFICE-BANTI
           MOVE    PTINF-OFFICE-TEL    TO  SPA-GMN21-OFFICE-TEL
           MOVE    PTINF-KISEI-NAME    TO  SPA-GMN21-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  SPA-GMN21-KISEI-POST
           MOVE    PTINF-KISEI-ADRS    TO  SPA-GMN21-KISEI-ADRS
           MOVE    PTINF-KISEI-BANTI   TO  SPA-GMN21-KISEI-BANTI
           MOVE    PTINF-KISEI-TEL     TO  SPA-GMN21-KISEI-TEL
      *
      *    禁忌情報
           MOVE    PTINF-TABOO-1       TO  SPA-GMN-TABOO-1
           MOVE    PTINF-TABOO-2       TO  SPA-GMN-TABOO-2
           MOVE    PTINF-ALLERGY-1     TO  SPA-GMN-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  SPA-GMN-ALLERGY-2
           MOVE    PTINF-KANSENSYO-1   TO  SPA-GMN-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  SPA-GMN-KANSENSYO-2
      *
           IF  SPA-GMN-DISCOUNTKBN     =   SPACE
               MOVE    ZERO               TO  SPA-GMN-DISCOUNTKBN
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
               IF  SPA-GMN-DISCOUNTKBN
                                     =   SPA-GMN-DISCOUNTKBN-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNTKBN-BODY (IDX)
                                     TO  SPA-GMN-DISCOUNTKBN-MEI
               END-IF
           END-PERFORM
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
           IF  SPA-GMN-DISCOUNT (1:2)      =   SPACE
               MOVE    ZERO                TO  SPA-GMN-DISCOUNT (1:2)
           END-IF
           MOVE    SPACE                   TO  WRK-RATE-AREA
           MOVE    SPA-GMN-DISCOUNT        TO  WRK-RATE-KBN
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX         >   SPA-DISCOUNT-MAX )
                     OR      ( SPA-GMN-DISCOUNT-KBNCD(IDX) = SPACE )
                     OR      ( WRK-RATE-EDIT-R         NOT = SPACE )
               IF  WRK-RATE-KBN        =   SPA-GMN-DISCOUNT-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNT-RITU (IDX)
                                           TO  WRK-RATE-EDITZ
                   EVALUATE    SPA-GMN-DISCOUNT-TEIRITU (IDX)
                   WHEN    "0"
                       MOVE    "該当なし"  TO  WRK-RATE-EDIT-R
                   WHEN    "1"
                       MOVE    "(%)"       TO  WRK-RATE-TANI
                   WHEN    "2"
                       MOVE    "(円)"      TO  WRK-RATE-TANI
                   END-EVALUATE
               END-IF
           END-PERFORM
           MOVE    WRK-RATE-AREA       TO  SPA-GMN-DISCOUNT
      *    状態１〜３
           MOVE    PTINF-CONDITION-1   TO  SPA-GMN-CONDITION(1)
           IF      SPA-GMN-CONDITION(1)(1:2)  =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(1)(1:2)
           END-IF
           MOVE    1                   TO  IDX
           PERFORM 20011-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-2   TO  SPA-GMN-CONDITION(2)
           IF      SPA-GMN-CONDITION(2)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(2)(1:2)
           END-IF
           MOVE    2                   TO  IDX
           PERFORM 20011-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-3   TO  SPA-GMN-CONDITION(3)
           IF      SPA-GMN-CONDITION(3)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(3)(1:2)
           END-IF
           MOVE    3                   TO  IDX
           PERFORM 20011-CONDITIONH-SPASET-SEC
      *    入金方法
           IF      SPA-GMN-NYUKIN-HOHO     =   SPACE
                                           OR  ZERO
               MOVE    SPA-GMN-NYUKIN-LST (01)
                                       TO  SPA-GMN-NYUKIN-HOHO-G
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   SPA-NYUKIN-MAX
               IF      SPA-GMN-NYUKIN-HOHO
                                       =   SPA-GMN-NYUKIN-HOHOL(IDX)
                   MOVE    SPA-GMN-NYUKIN-LST (IDX)
                                       TO  SPA-GMN-NYUKIN-HOHO-G
               END-IF
           END-PERFORM
      *    請求書発行区分
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG
                                       =   SPA-GMN-SKYPRTFLG-LST(IDX)
                                                           (1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                       TO  SPA-GMN-SKYPRTFLG-G
               END-IF
           END-PERFORM
      *
      *    入力の有無判定用
           MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-MAE-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-MAE-BIRTHDAY
           MOVE    SPA-GMN-DISCOUNTKBN TO  SPA-MAE-DISCOUNTKBN
      *
           MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           MOVE    SPA-GMN21-RENRAKU-ADRS  TO  SPA-MAE-RENRAKU-ADRS
           MOVE    SPA-GMN21-OFFICE-ADRS   TO  SPA-MAE-OFFICE-ADRS
           MOVE    SPA-GMN21-KISEI-ADRS    TO  SPA-MAE-KISEI-ADRS
      *
           .
       2001-PTINF-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    状態名称スパセット処理
      ****************************************************************
       20011-CONDITIONH-SPASET-SEC         SECTION.
      *
           MOVE    SPA-GMN-CONDITION (IDX)(1:2)    TO  WRK-KBNCD
      *
           PERFORM   VARYING   IDY     FROM    1   BY  1
                     UNTIL   ( IDY     >   SPA-CONDITION-MAX(IDX))
                     OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
               IF      WRK-KBNCD       =   SPA-GMN-CONDITION-KBNCD
                                                           (IDX IDY)
                   MOVE    SPA-GMN-CONDITION-BODY (IDX IDY)
                                         TO  SPA-GMN-CONDITION(IDX)(4:)
                   MOVE    SPA-GMN-CONDITION-WORD (IDX IDY)
                                         TO  SPA-GMN-CONDITIONH (IDX)
               END-IF
           END-PERFORM
      *
           .
       20011-CONDITIONH-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       410121-NENREI-SET-SEC             SECTION.
      *
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *
           MOVE    WRK-NENREI-YY       TO  SPA-NAI-NENREI-YY
           MOVE    WRK-NENREI-MM       TO  SPA-NAI-NENREI-MM
      *
           .
       410121-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     他の情報編集処理
      *****************************************************************
       2002-PARA-SET-SEC          SECTION.
      *
      *    保険者編集
           INITIALIZE                  SPA-PTHKNINF-AREA
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
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-PTNUM-HKNID
           PERFORM     UNTIL   FLG-PTHKNINF    =   1
               MOVE    ZERO                TO  FLG-HEN-OK
               IF     (PTHKN-SAKJOKBN      =   SPACE       )  AND
                     ((PTHKN-TEKSTYMD      <=  SPA-SYSYMD ) AND
                      (PTHKN-TEKEDYMD      >=  SPA-SYSYMD ))
      *            労災・自賠責・自費は複写なし
                   IF     (PTHKN-HKNNUM        =   "971" 
                                               OR  "973" )
                      OR  (PTHKN-HKNNUM(1:2)   =   "98"  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-HEN-OK
                   END-IF
               END-IF
               IF      FLG-HEN-OK          =   1
      *            内容の変更
                   MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
      *
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "PTHKNINF"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   ADD     1                   TO  SPA-PTNUM-HKNID
                   MOVE    SPA-PTNUM-HKNID     TO  PTHKN-HKNID
                   MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *            ＳＰＡにも編集する
                   IF      IDX               >  40
                       CONTINUE
                   ELSE
                       PERFORM 100221-PTHKNINF-SPA-SEC
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
           INITIALIZE                  SPA-PTKOHINF-AREA
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
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-PTNUM-KOHID
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
               IF    ( PTKOH-SAKJOKBN      =   SPACE      )  AND
                     ((PTKOH-TEKSTYMD      <=  SPA-SYSYMD ) AND
                      (PTKOH-TEKEDYMD      >=  SPA-SYSYMD ))
      *H25.11
      *            第三者行為対象外
                  AND (PTKOH-KOHNUM    NOT =   "970"      )
      *
      *            内容の変更
                   MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
      *
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "PTKOHINF"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   ADD     1                   TO  SPA-PTNUM-KOHID
                   MOVE    SPA-PTNUM-KOHID     TO  PTKOH-KOHID
                   MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *            ＳＰＡにも編集する
                   IF      IDX               >  60
                       CONTINUE
                   ELSE
                       PERFORM 100222-PTKOHINF-SPA-SEC
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
      *    受付
           IF      SPA-P02-UKEID   NOT =   ZERO
               PERFORM 100229-UKETUKE-SET-SEC
           END-IF
      *
      *    低所得より
           MOVE    ZERO                TO  SPA-NAI-TSYRRK
           INITIALIZE                      TSYRRK-REC
           MOVE    ORCSP04-HOSPNUM     TO  TSYRRK-HOSPNUM
           MOVE    ORCSP04-PTID        TO  TSYRRK-PTID
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TSYRRK
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM   UNTIL    FLG-TSYRRK    =   1
      *        当日に有効
               IF     (TSYRRK-NINSTYMD     <=  SPA-SYSYMD )
                  AND (TSYRRK-NINEDYMD     >=  SPA-SYSYMD )
      *            内容の変更
                   MOVE    SPA-HOSPNUM         TO  TSYRRK-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  TSYRRK-PTID
      *
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "TSYRRK"            TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
      *            当日に有効
                   MOVE    1                   TO  SPA-NAI-TSYRRK
      *
                   MOVE    TSYRRK-REC          TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               END-IF
      *
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    年金履歴より
           MOVE    ZERO                TO  SPA-NAI-TNKRRK
           INITIALIZE                      TNKRRK-REC
           MOVE    ORCSP04-HOSPNUM     TO  TNKRRK-HOSPNUM
           MOVE    ORCSP04-PTID        TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TNKRRK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM   UNTIL    FLG-TNKRRK   =   1
               IF     (TNKRRK-STYMD        <=  SPA-SYSYMD )
                  AND (TNKRRK-EDYMD        >=  SPA-SYSYMD )
      *            内容の変更
                   MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  TNKRRK-PTID
      *
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "TNKRRK"            TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
      *            当日に有効
                   MOVE    1                   TO  SPA-NAI-TNKRRK
      *
                   MOVE    TNKRRK-REC          TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               END-IF
      *
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2002-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報ＳＰＡ編集処理
      *****************************************************************
       100221-PTHKNINF-SPA-SEC           SECTION.
      *
           MOVE    PTHKN-HKNID         TO  SPA-PTHKN-HKNID      (IDX)
           MOVE    PTHKN-HKNNUM        TO  SPA-PTHKN-HKNNUM     (IDX)
           MOVE    PTHKN-HKNJANUM      TO  SPA-PTHKN-HKNJANUM   (IDX)
           MOVE    PTHKN-HIHKNJANAME   TO  SPA-PTHKN-HIHKNJANAME(IDX)
           MOVE    PTHKN-HONKZKKBN     TO  SPA-PTHKN-HONKZKKBN  (IDX)
           MOVE    PTHKN-HOJOKBN       TO  SPA-PTHKN-HOJOKBN    (IDX)
           MOVE    PTHKN-CONTKBN       TO  SPA-PTHKN-CONTKBN    (IDX)
           MOVE    PTHKN-KIGO          TO  SPA-PTHKN-KIGO       (IDX)
           MOVE    PTHKN-NUM           TO  SPA-PTHKN-NUM        (IDX)
           MOVE    PTHKN-SKKGETYMD     TO  SPA-PTHKN-SKKGETYMD  (IDX)
           MOVE    PTHKN-TEKSTYMD      TO  SPA-PTHKN-TEKSTYMD   (IDX)
           MOVE    PTHKN-TEKEDYMD      TO  SPA-PTHKN-TEKEDYMD   (IDX)
           MOVE    PTHKN-KAKUNINYMD    TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    PTHKN-SAKJOKBN      TO  SPA-PTHKN-SAKJOKBN   (IDX)
      *
           MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-SKKGETYMDW (IDX)
      *
           MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKSTYMDW (IDX)
      *
           MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW (IDX)
      *
           MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-KAKUNINYMDW (IDX)
      *
      *    保険公費区分編集
           MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           MOVE    PTHKN-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTHKN-TEKEDYMD      TO  WRK-TEKEDYMD
           PERFORM 1002211-HKNNUM-SET-SEC
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTHKN-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTHKN-HKNNUM-MEI (IDX)
      *    新規分
           MOVE    "1"                 TO  SPA-PTHKN-SINKIKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-PTHKN-MAX
      *
           .
       100221-PTHKNINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報ＳＰＡ編集処理
      *****************************************************************
       100222-PTKOHINF-SPA-SEC           SECTION.
      *
           MOVE    PTKOH-KOHID         TO  SPA-PTKOH-KOHID      (IDX)
           MOVE    PTKOH-KOHNUM        TO  SPA-PTKOH-KOHNUM     (IDX)
           MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN     (IDX)
           MOVE    PTKOH-FTNJANUM      TO  SPA-PTKOH-FTNJANUM   (IDX)
           MOVE    PTKOH-JKYSNUM       TO  SPA-PTKOH-JKYSNUM    (IDX)
           MOVE    PTKOH-TEKSTYMD      TO  SPA-PTKOH-TEKSTYMD   (IDX)
           MOVE    PTKOH-TEKEDYMD      TO  SPA-PTKOH-TEKEDYMD   (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-PTKOH-KAKUNINYMD (IDX)
           MOVE    PTKOH-SAKJOKBN      TO  SPA-PTKOH-SAKJOKBN   (IDX)
           MOVE    PTKOH-TEKEDCHKKBN   TO  SPA-PTKOH-TEKEDCHKKBN   (IDX)
      *
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKSTYMDW (IDX)
      *
           MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKEDYMDW (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-KAKUNINYMDW (IDX)
      *
           IF     (PTKOH-KOHNUM        =   "027") AND
                  (PTKOH-TEKSTYMD      >=  "20010101" )  AND
                  (PTKOH-TEKEDYMD      <=  "20020930" )
               IF     (PTKOH-PAYKBN    NOT =   ZERO   )
                   MOVE    ZERO                TO  PTKOH-PAYKBN
                   MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN
                                                                (IDX)
               END-IF
           END-IF
      *    保険公費区分編集
           MOVE    PTKOH-KOHNUM        TO  WRK-HKNNUM
           MOVE    PTKOH-PAYKBN        TO  WRK-PAYKBN
           MOVE    PTKOH-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  WRK-TEKEDYMD
      *
           IF     (WRK-HKNNUM          =   "027") AND
                  (WRK-PAYKBN          =   ZERO OR SPACE)
      *        Ｈ１２．１２．３１までの支払区分は００
               IF      WRK-TEKEDYMD        >=  "20010101"
                   MOVE    SPA-NAI-HOSPSBT         TO  WRK-PAYKBN(1:1)
                   MOVE    SPA-NAI-ROUPAYKBN       TO  WRK-PAYKBN(2:1)
               END-IF
           END-IF
           PERFORM 1002211-HKNNUM-SET-SEC
           IF     (PTKOH-KOHNUM        =   "027" )  AND
                  (PTKOH-PAYKBN        =   ZERO  )
               MOVE    "老人保健"          TO  WRK-TANSEIDONAME
           END-IF
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTKOH-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTKOH-KOHNUM-MEI (IDX)
      *    新規分
           MOVE    "1"                 TO  SPA-PTKOH-SINKIKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-PTKOH-MAX
      *
           .
       100222-PTKOHINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費区分編集処理
      *****************************************************************
       1002211-HKNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           IF      FLG-HKNNUM          =   ZERO
               IF     (WRK-TEKSTYMD        >=  HKN-TEKSTYMD )  AND
                      (WRK-TEKEDYMD        <=  HKN-TEKEDYMD )
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
      *        対象を開始日のみとする
               IF      WRK-TEKSTYMD        >=  HKN-TEKSTYMD
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
           END-IF
      *
           .
       1002211-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付作成処理
      *****************************************************************
       100229-UKETUKE-SET-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    SPACE               TO  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-P02-UKEID       TO  UKE-UKEID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC                  =   ZERO
                   MOVE    MCPDATA-REC         TO  UKETUKE-REC
      *
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "UKETUKE"           TO  PARA-FILEMEI
                   MOVE    1                   TO  PARA-EDANUM
                   MOVE    UKETUKE-REC         TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               ELSE
                   MOVE    SPACE               TO  UKETUKE-REC
               END-IF
           END-IF
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10022-PARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
      *
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
           PERFORM 920-DBFETCH-SEC
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
           PERFORM 920-DBFETCH-SEC
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
      *
      *****************************************************************
      *    受付読み込み
      *****************************************************************
       930-UKETUKE-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       930-UKETUKE-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       990-TSYRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       990-TSYRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       991-TNKRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       991-TNKRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNMST-NEXT-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
      *********MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
      *********INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *
