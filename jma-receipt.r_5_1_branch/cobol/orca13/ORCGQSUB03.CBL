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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQSUB03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 患者照会サブ（コンボボックス情報取得）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SHOUKAIKANRI                PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNNUM                      PIC 9(01).
           03  FLG-USERBYOMEI                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDXK                            PIC 9(05).
           03  IDXL                            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CD99-G.
               05  WRK-CD99                    PIC 9(02).
           03  WRK-KANRICD                     PIC X(04).
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-YMD                         PIC X(10). 
           03  WRK-ZOGENPTN                    PIC X(01).
           03  WRK-ZOGEN                       PIC S9(05).
      *
       01  CONST-AREA.
           03  CONST-H200401               PIC X(08)
                                               VALUE   "20080401".
           03  CONST-SELPATTERN-MAX        PIC 9(05)
                                               VALUE   50.
           03  CONST-DRCDLST-MAX             PIC 9(05)
                                               VALUE   99.
           03  CONST-SRYKALST-MAX          PIC 9(05)
                                               VALUE   50.
           03  CONST-CONDITIONLST-MAX      PIC 9(05)
                                               VALUE  100.
           03  CONST-GENMENLST-MAX         PIC 9(05)
                                               VALUE   99.
           03  CONST-HKNNUMLST-MAX         PIC 9(05)
                                               VALUE   50.
           03  CONST-KOHNUMLST-MAX         PIC 9(05)
                                               VALUE   150.
           03  CONST-UBYOMEILST-MAX        PIC 9(05)
                                               VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者状態コメント
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK1018.INC".
           COPY    "CPSK1026.INC".
           COPY    "CPSK4001.INC".
      *
      *    診療科
           COPY    "CPSK1005.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者照会検索条件管理情報
       01  SHOUKAIKANRI-REC.
           COPY    "CPSHOUKAIKANRI.INC".
      *
      *    自院病名マスター
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPQSUB03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           QSUB03-LNK
           SPA-AREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
                                   QSUB03-ITEM-LIST-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    QSUB03-REQUEST
           WHEN    SPACE
               PERFORM 2000-MAIN-SEC
           WHEN    "1"
               PERFORM 2001-MAIN-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       2000-MAIN-SEC                       SECTION.
      *
      *    検索条件コンボ初期処理
           PERFORM 2001-SELPATTERNLST-SEC
      *
      *    性別コンボ初期処理
           PERFORM 2001-SEXLST-SEC
      *
      *    ドクターコンボ初期処理
           PERFORM 2001-DRCDLST-SEC
      *
      *    入外区分コンボ初期処理
           PERFORM 2001-NYUGAIKBNLST-SEC
      *
      *    診療科コンボ初期処理
           PERFORM 2001-SRYKALST-SEC
      *
      *    死亡コンボ初期処理
           PERFORM 2001-DEATHLST-SEC
      *
      *    地域連携情報提供コンボ初期処理
           PERFORM 2001-AGREEMENTLST-SEC
      *
      *    テスト患者コンボ初期処理
           PERFORM 2001-TSTPTLST-SEC
      *
      *    状態１コンボ初期処理
           MOVE    "1018"          TO  WRK-KANRICD
           PERFORM 2001-CONDITIONLST-SEC
      *
      *    状態２コンボ初期処理
           MOVE    "1019"          TO  WRK-KANRICD
           PERFORM 2001-CONDITIONLST-SEC
      *
      *    状態３コンボ初期処理
           MOVE    "1020"          TO  WRK-KANRICD
           PERFORM 2001-CONDITIONLST-SEC
      *
      *    減免コンボ初期処理
           PERFORM 2001-GENMENLST-SEC
      *
      *    コメント選択コンボ初期処理
           PERFORM 2001-SELCOMMENTLST-SEC
      *
      *    低所得コンボ初期処理
           PERFORM 2001-TSYLST-SEC
      *
      *    低所得期間開始識別コンボ初期処理
           PERFORM 2001-STTSYSKBTLST-SEC
      *
      *    電話先コンボ初期処理
           PERFORM 2001-TEL-TOLST-SEC
      *
      *    特記事項コンボ初期処理
           PERFORM 2001-TOKKIINFOLST-SEC
      *
      *    災害該当コンボ初期処理
           PERFORM 2001-SAIGAIKBNLST-SEC
      *
      *    点検用レセプトコンボ初期処理
           PERFORM 2001-TENKENRECELST-SEC
      *
      *    本人家族コンボ初期処理
           PERFORM 2001-HONKZKKBNLST-SEC
      *
      *    負担割合コンボ初期処理
           PERFORM 2001-FTNRATELST-SEC
      *
      *    保険の種類・公費の種類コンボ初期処理
           PERFORM 2001-HKNKOHLST-SEC
      *
      *    疑い病名区分コンボ初期処理
           PERFORM 2001-UTAGAIKBNLST-SEC
      *
      *    急性病名区分コンボ初期処理
           PERFORM 2001-KYUSEIKBNLST-SEC
      *
      *    転帰区分コンボ初期処理
           PERFORM 2001-TENKIKBNLST-SEC
      *
      *    主病名区分コンボ初期処理
           PERFORM 2001-SYUBYOKBNLST-SEC
      *
      *    病名検索コンボ初期処理
           PERFORM 2001-BYOMEILST-SEC
      *
      *    病名比較条件コンボ初期処理
           PERFORM 2001-BYOCOMPLST-SEC
      *
      *    診療区分コンボ初期処理
           PERFORM 2001-SRYKBNLST-SEC
      *
      *    診療種別区分コンボ初期処理
           PERFORM 2001-SRYSYUKBNLST-SEC
      *
      *    診療行為　項目区分（回数・数量・合計）
           PERFORM 2001-SRYVALITEMLST-SEC
      *
      *    包括区分コンボ初期処理
           PERFORM 2001-HOKATSUKBNLST-SEC
      *
      *    AND/OR/NOT条件コンボ初期処理
           PERFORM 2001-JKNLST-SEC
      *
      *    診療行為　比較区分（回数・数量・合計）
           PERFORM 2001-SRYVALCOMPLST-SEC
      *
      *    介護保険期間識別コンボ初期処理
           PERFORM 2001-KHSKBTLST-SEC
      *
      *    介護認定期間識別コンボ初期処理
           PERFORM 2001-KNSKBTLST-SEC
      *
      *    要介護状態コンボボックス初期値
           PERFORM 2001-KNJOTAILST-SEC
      *
      *    検索項目（基本情報）コンボボックス初期処理
           PERFORM 2001-PTSORTLST-SEC
      *
      *    検索項目（点数情報）コンボボックス初期処理
           PERFORM 2001-TENSORTLST-SEC
      *
      *    検索項目（診療行為情報）コンボボックス初期処理
           PERFORM 2001-SRYSORTLST-SEC
      *
           .
       2000-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主処理（ドクターコンボボックス初期処理）
      *****************************************************************
       2001-MAIN-SEC                       SECTION.
      *
      *    ドクターコンボボックス初期処理
           PERFORM 2001-DRCDLST-SEC
      *
           .
       2001-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    検索条件コンボ初期処理
      *****************************************************************
       2001-SELPATTERNLST-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           PERFORM 900-SHOUKAIKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SHOUKAIKANRI    NOT =   ZERO )
                    OR   ( IDXL       >=  CONST-SELPATTERN-MAX )
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL        TO  QSUB03-SELPATTERNLST-MAX
                                       WRK-CD99
      *
               MOVE    WRK-CD99-G  TO  QSUB03-SELPATTERNL (IDXL)
               MOVE    SHOKANRI-NAME
                                   TO  QSUB03-SELPATTERNMEIL (IDXL)
      *
               MOVE    SHOKANRI-UID
                                   TO  QSUB03-SELPATTERN-UIDL (IDXL)
               MOVE    SHOKANRI-TITLEKBN 
                                   TO  QSUB03-SELPATTERN-TTLKBNL (IDXL)
      *
               PERFORM 900-SHOUKAIKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SELPATTERNLST-EXT.
           EXIT.
      *****************************************************************
      *    性別コンボ初期処理
      *****************************************************************
       2001-SEXLST-SEC                     SECTION.
      *
           MOVE    3                   TO  QSUB03-SEXLST-MAX
      *
           MOVE    SPACE               TO  QSUB03-SEXL (1)
           MOVE    "すべて"            TO  QSUB03-SEXMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-SEXL (2)
           MOVE    "男性のみ"          TO  QSUB03-SEXMEIL (2)
      *
           MOVE    "2"                 TO  QSUB03-SEXL (3)
           MOVE    "女性のみ"          TO  QSUB03-SEXMEIL (3)
      *
           .
       2001-SEXLST-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ初期処理
      *****************************************************************
       2001-NYUGAIKBNLST-SEC               SECTION.
      *
           MOVE    2               TO  QSUB03-NYUGAIKBNLST-MAX
      *
           MOVE    "1"             TO  QSUB03-NYUGAIKBNL    (1)
           MOVE    " 入院"         TO  QSUB03-NYUGAIKBNMEIL (1)
      *
           MOVE    "2"             TO  QSUB03-NYUGAIKBNL    (2)
           MOVE    " 外来"         TO  QSUB03-NYUGAIKBNMEIL (2)
      *
           .
       2001-NYUGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    ドクターコンボ初期処理
      *****************************************************************
       2001-DRCDLST-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    "1%"                TO  SYS-KBNCD
           MOVE    QSUB03-EDSRYYMD     TO  SYS-STYUKYMD
           MOVE    QSUB03-STSRYYMD     TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY21-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-DRCDLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  QSUB03-DRCDLST-MAX
      *
               MOVE    SYS-1010-KBNCD (2:4)
                                       TO  QSUB03-DRCDL (IDXL)
               MOVE    SYS-1010-NAME   TO  QSUB03-DRCDMEIL (IDXL)
      *
               PERFORM 900-SYSKANRI-KEY21-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-DRCDLST-EXT.
           EXIT.
      *****************************************************************
      *    診療科コンボ初期処理
      *****************************************************************
       2001-SRYKALST-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    QSUB03-BASEYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-SRYKALST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  QSUB03-SRYKALST-MAX
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                       TO  QSUB03-SRYKAL (IDXL)
               MOVE    SYS-1005-SRYKANAME
                                       TO  QSUB03-SRYKAMEIL (IDXL)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SRYKALST-EXT.
           EXIT.
      *****************************************************************
      *    死亡コンボ初期処理
      *****************************************************************
       2001-DEATHLST-SEC                   SECTION.
      *
           MOVE    2                   TO  QSUB03-DEATHLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-DEATHL    (1)
           MOVE    "未設定"            TO  QSUB03-DEATHMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-DEATHL    (2)
           MOVE    "死亡"              TO  QSUB03-DEATHMEIL (2)
      *
           .
       2001-DEATHLST-EXT.
           EXIT.
      *****************************************************************
      *    地域連携情報提供コンボ初期処理
      *****************************************************************
       2001-AGREEMENTLST-SEC               SECTION.
      *
           MOVE    2                   TO  QSUB03-AGREEMENTLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-AGREEMENTL    (1)
           MOVE    "同意しない"        TO  QSUB03-AGREEMENTMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-AGREEMENTL    (2)
           MOVE    "同意する"          TO  QSUB03-AGREEMENTMEIL (2)
      *
           .
       2001-AGREEMENTLST-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者コンボ初期処理
      *****************************************************************
       2001-TSTPTLST-SEC                   SECTION.
      *
           MOVE    3                   TO  QSUB03-TSTPTLST-MAX
      *
           MOVE    "a"                 TO  QSUB03-TSTPTL    (1)
           MOVE    "テスト患者を除く"  TO  QSUB03-TSTPTMEIL (1)
      *
           MOVE    "b"                 TO  QSUB03-TSTPTL    (2)
           MOVE    "テスト患者を含める" TO QSUB03-TSTPTMEIL (2)
      *
           MOVE    "1"                 TO  QSUB03-TSTPTL    (3)
           MOVE    "テスト患者のみ"    TO  QSUB03-TSTPTMEIL (3)
      *
           .
       2001-TSTPTLST-EXT.
           EXIT.
      *****************************************************************
      *    状態コンボ初期処理
      *****************************************************************
       2001-CONDITIONLST-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    QSUB03-BASEYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=   CONST-CONDITIONLST-MAX )
      *
               MOVE    SYSKANRI-REC
                                   TO  SYS-1018-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               EVALUATE    WRK-KANRICD
               WHEN    "1018"
                   MOVE    IDXL    TO  QSUB03-CONDITION1LST-MAX
                   MOVE    SYS-1018-KBNCD (1:2)
                                   TO  QSUB03-CONDITION1L (IDXL)
                   MOVE    SYS-1018-CONDITION-BODY
                                   TO  QSUB03-CONDITION1MEIL (IDXL)
               WHEN    "1019"
                   MOVE    IDXL    TO  QSUB03-CONDITION2LST-MAX
                   MOVE    SYS-1018-KBNCD (1:2)
                                   TO  QSUB03-CONDITION2L (IDXL)
                   MOVE    SYS-1018-CONDITION-BODY
                                   TO  QSUB03-CONDITION2MEIL (IDXL)
               WHEN    "1020"
                   MOVE    IDXL    TO  QSUB03-CONDITION3LST-MAX
                   MOVE    SYS-1018-KBNCD (1:2)
                                   TO  QSUB03-CONDITION3L (IDXL)
                   MOVE    SYS-1018-CONDITION-BODY
                                   TO  QSUB03-CONDITION3MEIL (IDXL)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-CONDITIONLST-EXT.
           EXIT.
      *****************************************************************
      *    減免コンボ初期処理
      *****************************************************************
       2001-GENMENLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1026"              TO  SYS-KANRICD
           MOVE    QSUB03-BASEYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL       >=   CONST-GENMENLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1026-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL        TO  QSUB03-GENMENLST-MAX
      *
               MOVE    SYS-1026-KBNCD (1:2)
                                   TO  QSUB03-GENMENL (IDXL)
               MOVE    SYS-1026-DISCOUNT-BODY
                                   TO  QSUB03-GENMENMEIL (IDXL)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-GENMENLST-EXT.
           EXIT.
      *****************************************************************
      *    コメント選択コンボ初期処理
      *****************************************************************
       2001-SELCOMMENTLST-SEC              SECTION.
      *
           MOVE    2                   TO  QSUB03-SELCOMMENTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-SELCOMMENTL   (1)
           MOVE    "患者コメント"      TO  QSUB03-SELCOMMENTMEIL(1)
      *
           MOVE    "2"                 TO  QSUB03-SELCOMMENTL   (2)
           MOVE    "患者禁忌・アレルギー・感染症"
                                       TO  QSUB03-SELCOMMENTMEIL(2)
      *
           .
       2001-SELCOMMENTLST-EXT.
           EXIT.
      *****************************************************************
      *    低所得コンボ初期処理
      *****************************************************************
       2001-TSYLST-SEC                     SECTION.
      *
           MOVE    2                   TO  QSUB03-TSYLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-TSYL    (1)
           MOVE    "低所得者１"        TO  QSUB03-TSYMEIL (1)
           MOVE    "2"                 TO  QSUB03-TSYL    (2)
           MOVE    "低所得者２"        TO  QSUB03-TSYMEIL (2)
      *
           .
       2001-TSYLST-EXT.
           EXIT.
      *****************************************************************
      *    低所得期間開始識別コンボ初期処理
      *****************************************************************
       2001-STTSYSKBTLST-SEC               SECTION.
      *
           MOVE    4                   TO  QSUB03-STTSYSKBTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-STTSYSKBTL    (1)
           MOVE    "有効月"            TO  QSUB03-STTSYSKBTMEIL (1)
           MOVE    "2"                 TO  QSUB03-STTSYSKBTL    (2)
           MOVE    "開始月"            TO  QSUB03-STTSYSKBTMEIL (2)
           MOVE    "3"                 TO  QSUB03-STTSYSKBTL    (3)
           MOVE    "終了月"            TO  QSUB03-STTSYSKBTMEIL (3)
           MOVE    "4"                 TO  QSUB03-STTSYSKBTL    (4)
           MOVE    "有効日"            TO  QSUB03-STTSYSKBTMEIL (4)
      *
           .
       2001-STTSYSKBTLST-EXT.
           EXIT.
      *****************************************************************
      *    電話先コンボ初期処理
      *****************************************************************
       2001-TEL-TOLST-SEC                  SECTION.
      *
           MOVE    3                   TO  QSUB03-TEL-TOLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-TEL-TOL    (1)
           MOVE    "自宅"              TO  QSUB03-TEL-TOMEIL (1)
           MOVE    "2"                 TO  QSUB03-TEL-TOL    (2)
           MOVE    "連絡先"            TO  QSUB03-TEL-TOMEIL (2)
           MOVE    "3"                 TO  QSUB03-TEL-TOL    (3)
           MOVE    "自宅・連絡先"      TO  QSUB03-TEL-TOMEIL (3)
      *
           .
       2001-TEL-TOLST-EXT.
           EXIT.
      *****************************************************************
      *    特記事項コンボ初期処理
      *****************************************************************
       2001-TOKKIINFOLST-SEC               SECTION.
      *
           MOVE    37              TO  QSUB03-TOKKIINFOLST-MAX
      *
           MOVE    "01"            TO  QSUB03-TOKKIINFOL    (01)
           MOVE    "公"            TO  QSUB03-TOKKIINFOMEIL (01)
      *
           MOVE    "02"            TO  QSUB03-TOKKIINFOL    (02)
           MOVE    "長"            TO  QSUB03-TOKKIINFOMEIL (02)
      *
           MOVE    "03"            TO  QSUB03-TOKKIINFOL    (03)
           MOVE    "長処"          TO  QSUB03-TOKKIINFOMEIL (03)
      *
           MOVE    "04"            TO  QSUB03-TOKKIINFOL    (04)
           MOVE    "後保"          TO  QSUB03-TOKKIINFOMEIL (04)
      *
           MOVE    "07"            TO  QSUB03-TOKKIINFOL    (05)
           MOVE    "老併"          TO  QSUB03-TOKKIINFOMEIL (05)
      *
           MOVE    "08"            TO  QSUB03-TOKKIINFOL    (06)
           MOVE    "老健"          TO  QSUB03-TOKKIINFOMEIL (06)
      *
           MOVE    "09"            TO  QSUB03-TOKKIINFOL    (07)
           MOVE    "施"            TO  QSUB03-TOKKIINFOMEIL (07)
      *
           MOVE    "10"            TO  QSUB03-TOKKIINFOL    (08)
           MOVE    "第三"          TO  QSUB03-TOKKIINFOMEIL (08)
      *
           MOVE    "11"            TO  QSUB03-TOKKIINFOL    (09)
           MOVE    "薬治"          TO  QSUB03-TOKKIINFOMEIL (09)
      *
           MOVE    "12"            TO  QSUB03-TOKKIINFOL    (10)
           MOVE    "器治"          TO  QSUB03-TOKKIINFOMEIL (10)
      *
           MOVE    "13"            TO  QSUB03-TOKKIINFOL    (11)
           MOVE    "先進"          TO  QSUB03-TOKKIINFOMEIL (11)
      *
           MOVE    "14"            TO  QSUB03-TOKKIINFOL    (12)
           MOVE    "制超"          TO  QSUB03-TOKKIINFOMEIL (12)
      *
           MOVE    "15"            TO  QSUB03-TOKKIINFOL    (13)
           MOVE    "経過"          TO  QSUB03-TOKKIINFOMEIL (13)
      *
           MOVE    "16"            TO  QSUB03-TOKKIINFOL    (14)
           MOVE    "長２"          TO  QSUB03-TOKKIINFOMEIL (14)
      *
           MOVE    "17"            TO  QSUB03-TOKKIINFOL    (15)
           MOVE    "上位"          TO  QSUB03-TOKKIINFOMEIL (15)
      *
           MOVE    "18"            TO  QSUB03-TOKKIINFOL    (16)
           MOVE    "一般"          TO  QSUB03-TOKKIINFOMEIL (16)
      *
           MOVE    "19"            TO  QSUB03-TOKKIINFOL    (17)
           MOVE    "低所"          TO  QSUB03-TOKKIINFOMEIL (17)
      *
           MOVE    "20"            TO  QSUB03-TOKKIINFOL    (18)
           MOVE    "二割"          TO  QSUB03-TOKKIINFOMEIL (18)
      *
           MOVE    "21"            TO  QSUB03-TOKKIINFOL    (19)
           MOVE    "高半"          TO  QSUB03-TOKKIINFOMEIL (19)
      *
           MOVE    "22"            TO  QSUB03-TOKKIINFOL    (20)
           MOVE    "多上"          TO  QSUB03-TOKKIINFOMEIL (20)
      *
           MOVE    "23"            TO  QSUB03-TOKKIINFOL    (21)
           MOVE    "多一"          TO  QSUB03-TOKKIINFOMEIL (21)
      *
           MOVE    "24"            TO  QSUB03-TOKKIINFOL    (22)
           MOVE    "多低"          TO  QSUB03-TOKKIINFOMEIL (22)
      *
           MOVE    "25"            TO  QSUB03-TOKKIINFOL    (23)
           MOVE    "出産"          TO  QSUB03-TOKKIINFOMEIL (23)
      *
           MOVE    "26"            TO  QSUB03-TOKKIINFOL    (24)
           MOVE    "区ア"          TO  QSUB03-TOKKIINFOMEIL (24)
      *
           MOVE    "27"            TO  QSUB03-TOKKIINFOL    (25)
           MOVE    "区イ"          TO  QSUB03-TOKKIINFOMEIL (25)
      *
           MOVE    "28"            TO  QSUB03-TOKKIINFOL    (26)
           MOVE    "区ウ"          TO  QSUB03-TOKKIINFOMEIL (26)
      *
           MOVE    "29"            TO  QSUB03-TOKKIINFOL    (27)
           MOVE    "区エ"          TO  QSUB03-TOKKIINFOMEIL (27)
      *
           MOVE    "30"            TO  QSUB03-TOKKIINFOL    (28)
           MOVE    "区オ"          TO  QSUB03-TOKKIINFOMEIL (28)
      *
           MOVE    "31"            TO  QSUB03-TOKKIINFOL    (29)
           MOVE    "多ア"          TO  QSUB03-TOKKIINFOMEIL (29)
      *
           MOVE    "32"            TO  QSUB03-TOKKIINFOL    (30)
           MOVE    "多イ"          TO  QSUB03-TOKKIINFOMEIL (30)
      *
           MOVE    "33"            TO  QSUB03-TOKKIINFOL    (31)
           MOVE    "多ウ"          TO  QSUB03-TOKKIINFOMEIL (31)
      *
           MOVE    "34"            TO  QSUB03-TOKKIINFOL    (32)
           MOVE    "多エ"          TO  QSUB03-TOKKIINFOMEIL (32)
      *
           MOVE    "35"            TO  QSUB03-TOKKIINFOL    (33)
           MOVE    "多オ"          TO  QSUB03-TOKKIINFOMEIL (33)
      *
           MOVE    "36"            TO  QSUB03-TOKKIINFOL    (34)
           MOVE    "加治"          TO  QSUB03-TOKKIINFOMEIL (34)
      *
           MOVE    "37"            TO  QSUB03-TOKKIINFOL    (35)
           MOVE    "申出"          TO  QSUB03-TOKKIINFOMEIL (35)
      *
           MOVE    "38"            TO  QSUB03-TOKKIINFOL    (36)
           MOVE    "医併"          TO  QSUB03-TOKKIINFOMEIL (36)
      *
           MOVE    "39"            TO  QSUB03-TOKKIINFOL    (37)
           MOVE    "医療"          TO  QSUB03-TOKKIINFOMEIL (37)
      *
           .
       2001-TOKKIINFOLST-EXT.
           EXIT.
      *****************************************************************
      *    災害該当コンボ初期処理
      *****************************************************************
       2001-SAIGAIKBNLST-SEC               SECTION.
      *
           MOVE    2               TO  QSUB03-SAIGAIKBNLST-MAX
      *
           MOVE    "1"             TO  QSUB03-SAIGAIKBNL    (01)
           MOVE    "未確定分"      TO  QSUB03-SAIGAIKBNMEIL (01)
      *
           MOVE    "2"             TO  QSUB03-SAIGAIKBNL    (02)
           MOVE    "確定分"        TO  QSUB03-SAIGAIKBNMEIL (02)
      *
           .
       2001-SAIGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    点検用レセプトコンボ初期処理
      *****************************************************************
       2001-TENKENRECELST-SEC              SECTION.
      *
           MOVE    1               TO  QSUB03-TENKENRECELST-MAX
      *
           MOVE    "1"             TO  QSUB03-TENKENRECEL    (01)
           MOVE    "０点公費記載分" TO QSUB03-TENKENRECEMEIL (01)
      *
           .
       2001-TENKENRECELST-EXT.
           EXIT.
      *****************************************************************
      *    本人家族コンボ初期処理
      *****************************************************************
       2001-HONKZKKBNLST-SEC               SECTION.
      *
           MOVE    2                   TO  QSUB03-HONKZKKBNLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-HONKZKKBNL    (1)
           MOVE    "本人"              TO  QSUB03-HONKZKKBNMEIL (1)
           MOVE    "2"                 TO  QSUB03-HONKZKKBNL    (2)
           MOVE    "家族"              TO  QSUB03-HONKZKKBNMEIL (2)
      *
           .
       2001-HONKZKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    負担割合コンボ初期処理
      *****************************************************************
       2001-FTNRATELST-SEC                 SECTION.
      *
           MOVE    5                   TO  QSUB03-FTNRATELST-MAX
      *
           MOVE    "1"                 TO  QSUB03-FTNRATEL    (1)
           MOVE    "前期高齢者１割"    TO  QSUB03-FTNRATEMEIL (1)
           MOVE    "2"                 TO  QSUB03-FTNRATEL    (2)
           MOVE    "前期高齢者２割"    TO  QSUB03-FTNRATEMEIL (2)
           MOVE    "3"                 TO  QSUB03-FTNRATEL    (3)
           MOVE    "前期高齢者３割"    TO  QSUB03-FTNRATEMEIL (3)
           MOVE    "4"                 TO  QSUB03-FTNRATEL    (4)
           MOVE    "後期高齢者１割"    TO  QSUB03-FTNRATEMEIL (4)
           MOVE    "5"                 TO  QSUB03-FTNRATEL    (5)
           MOVE    "後期高齢者３割"    TO  QSUB03-FTNRATEMEIL (5)
      *
           .
       2001-FTNRATELST-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類コンボ初期処理
      *****************************************************************
       2001-HKNKOHLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
           MOVE    ZERO                TO  IDXK
      *
      *    保険番号検索
           PERFORM 900-HKNNUM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
      *            保険
                   IF    ( IDXL            >   ZERO )
                       IF    ( HKN-HKNNUM
                                   NOT =   QSUB03-HKNNUML (IDXL))
      *                    保険情報編集
                           PERFORM 20011-HKNNUMLST-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 20011-HKNNUMLST-SEC
                   END-IF
      *
               ELSE
      *            公費情報編集
                   PERFORM 20011-KOHNUMLST-SEC
               END-IF
      *
               PERFORM 900-HKNNUM-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    "SYA"           TO  QSUB03-HKNNUML   (IDXL + 1)
           MOVE    "社保"          TO  QSUB03-HKNNUMMEIL(IDXL + 1)
           MOVE    "KOK"           TO  QSUB03-HKNNUML   (IDXL + 2)
           MOVE    "国保"          TO  QSUB03-HKNNUMMEIL(IDXL + 2)
           IF    ( QSUB03-BASEYMD      >=  CONST-H200401 )
               MOVE    "KOU"       TO  QSUB03-HKNNUML   (IDXL + 3)
               MOVE    "広域"      TO  QSUB03-HKNNUMMEIL(IDXL + 3)
               COMPUTE IDXL    =   IDXL + 3
           ELSE
               COMPUTE IDXL    =   IDXL + 2
           END-IF
      *
           MOVE    "JB0"           TO  QSUB03-HKNNUML   (IDXL + 1)
           MOVE    "自賠責（システム設定）"
                                   TO  QSUB03-HKNNUMMEIL(IDXL + 1)
           MOVE    "JB1"           TO  QSUB03-HKNNUML   (IDXL + 2)
           MOVE    "自賠責（患者労災設定）"
                                   TO  QSUB03-HKNNUMMEIL(IDXL + 2)
           MOVE    "JB2"           TO  QSUB03-HKNNUML   (IDXL + 3)
           MOVE    "自賠責（患者健保設定）"
                                   TO  QSUB03-HKNNUMMEIL(IDXL + 3)
           COMPUTE IDXL    =   IDXL + 3
      *
           MOVE    IDXL            TO QSUB03-HKNNUMLST-MAX
      *
           .
       2001-HKNKOHLST-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       20011-HKNNUMLST-SEC             SECTION.
      *
           IF    ( IDXL            <  CONST-HKNNUMLST-MAX )
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    HKN-HKNNUM          TO  QSUB03-HKNNUML    (IDXL)
               MOVE    HKN-TANSEIDONAME    TO  QSUB03-HKNNUMMEIL (IDXL)
      *
           END-IF
      *
           .
       20011-HKNNUMLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20011-KOHNUMLST-SEC          SECTION.
      *
           IF    ( IDXK            >   ZERO )
               IF    ( HKN-HKNNUM  =   QSUB03-KOHNUML (IDXK))
                   IF    ( HKN-HKNNUM   =   "027"  )
                     AND ( HKN-PAYKBN   =   "01" OR "02" OR "03")
                       IF    ( HKN-PAYKBN
                                    NOT =   QSUB03-PAYKBNL (IDXK))
      *                    公費情報編集
                           PERFORM 200111-KOHNUMLST-SEC
                       END-IF
                   END-IF
               ELSE
      *            公費情報編集
                   PERFORM 200111-KOHNUMLST-SEC
               END-IF
           ELSE
      *        公費情報編集
               PERFORM 200111-KOHNUMLST-SEC
           END-IF
      *
           .
       20011-KOHNUMLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       200111-KOHNUMLST-SEC            SECTION.
      *
           IF    ( IDXK            <  CONST-KOHNUMLST-MAX )
               PERFORM 2001111-KOHNUMLST-SEC
           END-IF
      *
           .
       200111-KOHNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       2001111-KOHNUMLST-SEC            SECTION.
      *
           COMPUTE IDXK    =   IDXK    +   1
      *
           MOVE    IDXK                TO  QSUB03-KOHNUMLST-MAX
      *
           MOVE    HKN-HKNNUM          TO  QSUB03-KOHNUML (IDXK)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" AND "03" )
               MOVE    "老人保健"      TO  QSUB03-KOHNUMMEIL (IDXK)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  QSUB03-KOHNUMMEIL (IDXK)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" OR "03")
               MOVE    HKN-PAYKBN (2:1)
                                       TO  QSUB03-PAYKBNL (IDXK)
           ELSE
               MOVE    SPACE           TO  QSUB03-PAYKBNL (IDXK)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "940" )
               CONTINUE
           ELSE
               IF    ( HKN-HKNNUM (1:2)  =   "94" OR "95" OR "96" )
                OR   ( HKN-HKNNUM        =   "976" OR "977" OR "978"
                                          OR "979" )
                   MOVE    "1"           TO  QSUB03-KOHNUM-TYPEL (IDXK)
               END-IF
           END-IF
      *
           .
       2001111-KOHNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    疑い病名区分コンボ初期処理
      *****************************************************************
       2001-UTAGAIKBNLST-SEC               SECTION.
      *
           MOVE    2                   TO  QSUB03-UTAGAIKBNLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-UTAGAIKBNL (1)
           MOVE    "含まない"          TO  QSUB03-UTAGAIKBNMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-UTAGAIKBNL (2)
           MOVE    "疑い病名のみ"      TO  QSUB03-UTAGAIKBNMEIL (2)
      *
           .
       2001-UTAGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    急性病名区分コンボ初期処理
      *****************************************************************
       2001-KYUSEIKBNLST-SEC               SECTION.
      *
           MOVE    2                   TO  QSUB03-KYUSEIKBNLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-KYUSEIKBNL (1)
           MOVE    "含まない"          TO  QSUB03-KYUSEIKBNMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-KYUSEIKBNL (2)
           MOVE    "急性病名のみ"      TO  QSUB03-KYUSEIKBNMEIL (2)
      *
           .
       2001-KYUSEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    転帰区分コンボ初期処理
      *****************************************************************
       2001-TENKIKBNLST-SEC                SECTION.
      *
           MOVE    6                   TO  QSUB03-TENKIKBNLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-TENKIKBNL (1)
           MOVE    "未入力分のみ"      TO  QSUB03-TENKIKBNMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-TENKIKBNL (2)
           MOVE    "治ゆ"              TO  QSUB03-TENKIKBNMEIL (2)
      *
           MOVE    "2"                 TO  QSUB03-TENKIKBNL (3)
           MOVE    "死亡"              TO  QSUB03-TENKIKBNMEIL (3)
      *
           MOVE    "3"                 TO  QSUB03-TENKIKBNL (4)
           MOVE    "中止"              TO  QSUB03-TENKIKBNMEIL (4)
      *
           MOVE    "8"                 TO  QSUB03-TENKIKBNL (5)
           MOVE    "移行"              TO  QSUB03-TENKIKBNMEIL (5)
      *
           MOVE    "9"                 TO  QSUB03-TENKIKBNL (6)
           MOVE    "転帰すべて"        TO  QSUB03-TENKIKBNMEIL (6)
      *
           .
       2001-TENKIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    主病名区分コンボ初期処理
      *****************************************************************
       2001-SYUBYOKBNLST-SEC               SECTION.
      *
           MOVE    2                   TO  QSUB03-SYUBYOKBNLST-MAX
      *
           MOVE    "0"                 TO  QSUB03-SYUBYOKBNL (1)
           MOVE    "主病名でない"      TO  QSUB03-SYUBYOKBNMEIL (1)
      *
           MOVE    "1"                 TO  QSUB03-SYUBYOKBNL (2)
           MOVE    "主病名である"      TO  QSUB03-SYUBYOKBNMEIL (2)
      *
           .
       2001-SYUBYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    病名検索コンボ初期処理
      *****************************************************************
       2001-BYOMEILST-SEC                  SECTION.
      *
           MOVE    ZERO            TO  IDXL
      *
      *    自院病名マスタ−検索処理
           PERFORM 900-USERBYOMEI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( IDXL   >=  CONST-UBYOMEILST-MAX - 2 )
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    USBYO-BYOMEIINPUTCD
                               TO  QSUB03-UBYOMEIL (IDXL)
               MOVE    USBYO-BYOMEI
                               TO  QSUB03-UBYOMEIMEIL (IDXL)
      *
               PERFORM 900-USERBYOMEI-KEY4-FET-SEC
      *
           END-PERFORM
      *
      *    自院病名マスタ−クローズ処理
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE IDXL        =   IDXL    +   1
           MOVE    "NON"           TO  QSUB03-UBYOMEIL    (IDXL)
           MOVE    "分類なし"      TO  QSUB03-UBYOMEIMEIL (IDXL)
      *
           COMPUTE IDXL        =   IDXL    +   1
           MOVE    "ALL"           TO  QSUB03-UBYOMEIL    (IDXL)
           MOVE    "すべて"        TO  QSUB03-UBYOMEIMEIL (IDXL)
      *
           MOVE    IDXL            TO  QSUB03-UBYOMEILST-MAX
      *
           .
       2001-BYOMEILST-EXT.
           EXIT.
      *****************************************************************
      *    病名比較条件コンボ初期処理
      *****************************************************************
       2001-BYOCOMPLST-SEC                 SECTION.
      *
           MOVE    4                   TO  QSUB03-BYOCOMPLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-BYOCOMPL (1)
           MOVE    "前方"              TO  QSUB03-BYOCOMPMEIL (1)
      *
           MOVE    "2"                 TO  QSUB03-BYOCOMPL (2)
           MOVE    "部分"              TO  QSUB03-BYOCOMPMEIL (2)
      *
           MOVE    "3"                 TO  QSUB03-BYOCOMPL (3)
           MOVE    "後方"              TO  QSUB03-BYOCOMPMEIL (3)
      *
           MOVE    "4"                 TO  QSUB03-BYOCOMPL (4)
           MOVE    "完全"              TO  QSUB03-BYOCOMPMEIL (4)
      *
           .
       2001-BYOCOMPLST-EXT.
           EXIT.
      *****************************************************************
      *    診療区分コンボ初期処理
      *****************************************************************
       2001-SRYKBNLST-SEC                  SECTION.
      *
           MOVE    "11"            TO  QSUB03-SRYKBNL    (1)
           MOVE    "初診"          TO  QSUB03-SRYKBNMEIL (1)
      *
           MOVE    "12"            TO  QSUB03-SRYKBNL    (2)
           MOVE    "再診"          TO  QSUB03-SRYKBNMEIL (2)
      *
           MOVE    "13"            TO  QSUB03-SRYKBNL    (3)
           MOVE    "医学管理"      TO  QSUB03-SRYKBNMEIL (3)
      *
           MOVE    "14"            TO  QSUB03-SRYKBNL    (4)
           MOVE    "在宅"          TO  QSUB03-SRYKBNMEIL (4)
      *
           MOVE    "20"            TO  QSUB03-SRYKBNL    (5)
           MOVE    "投薬"          TO  QSUB03-SRYKBNMEIL (5)
      *
           MOVE    "30"            TO  QSUB03-SRYKBNL    (6)
           MOVE    "注射"          TO  QSUB03-SRYKBNMEIL (6)
      *
           MOVE    "40"            TO  QSUB03-SRYKBNL    (7)
           MOVE    "処置"          TO  QSUB03-SRYKBNMEIL (7)
      *
           MOVE    "50"            TO  QSUB03-SRYKBNL    (8)
           MOVE    "手術"          TO  QSUB03-SRYKBNMEIL (8)
      *
           MOVE    "54"            TO  QSUB03-SRYKBNL    (9)
           MOVE    "麻酔"          TO  QSUB03-SRYKBNMEIL (9)
      *
           MOVE    "60"            TO  QSUB03-SRYKBNL    (10)
           MOVE    "検査"          TO  QSUB03-SRYKBNMEIL (10)
      *
           MOVE    "64"            TO  QSUB03-SRYKBNL    (11)
           MOVE    "病理診断"      TO  QSUB03-SRYKBNMEIL (11)
      *
           MOVE    "70"            TO  QSUB03-SRYKBNL    (12)
           MOVE    "画像診断"      TO  QSUB03-SRYKBNMEIL (12)
      *
           MOVE    "80"            TO  QSUB03-SRYKBNL    (13)
           MOVE    "リハビリ・その他"
                                   TO  QSUB03-SRYKBNMEIL (13)
      *
           MOVE    "83"            TO  QSUB03-SRYKBNL    (14)
           MOVE    "精神科専門"    TO  QSUB03-SRYKBNMEIL (14)
      *
           MOVE    "84"            TO  QSUB03-SRYKBNL    (15)
           MOVE    "放射線治療"    TO  QSUB03-SRYKBNMEIL (15)
      *
           MOVE    "90"            TO  QSUB03-SRYKBNL    (16)
           MOVE    "入院"          TO  QSUB03-SRYKBNMEIL (16)
      *
           MOVE    "95"            TO  QSUB03-SRYKBNL    (17)
           MOVE    "保険外（消費税なし）"
                                   TO  QSUB03-SRYKBNMEIL (17)
      *
           MOVE    "96"            TO  QSUB03-SRYKBNL    (18)
           MOVE    "保険外（消費税あり）"
                                   TO  QSUB03-SRYKBNMEIL (18)
      *
           MOVE    "97"            TO  QSUB03-SRYKBNL    (19)
           MOVE    "食事"          TO  QSUB03-SRYKBNMEIL (19)
      *
           MOVE    "98"            TO  QSUB03-SRYKBNL    (20)
           MOVE    "処方箋コメント"
                                   TO  QSUB03-SRYKBNMEIL (20)
      *
           MOVE    "99"            TO  QSUB03-SRYKBNL    (21)
           MOVE    "コメント"      TO  QSUB03-SRYKBNMEIL (21)
      *
           MOVE    21              TO  QSUB03-SRYKBNLST-MAX
      *
           .
       2001-SRYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療種別区分コンボ初期処理
      *****************************************************************
       2001-SRYSYUKBNLST-SEC           SECTION.
      *
           MOVE    "21"            TO  QSUB03-SRYSYUKBN20L    (1)
           MOVE    "内服"          TO  QSUB03-SRYSYUKBN20MEIL (1)
           MOVE    "22"            TO  QSUB03-SRYSYUKBN20L    (2)
           MOVE    "頓服"          TO  QSUB03-SRYSYUKBN20MEIL (2)
           MOVE    "23"            TO  QSUB03-SRYSYUKBN20L    (3)
           MOVE    "外用"          TO  QSUB03-SRYSYUKBN20MEIL (3)
           MOVE    "24"            TO  QSUB03-SRYSYUKBN20L    (4)
           MOVE    "入院調剤料"    TO  QSUB03-SRYSYUKBN20MEIL (4)
           MOVE    "25"            TO  QSUB03-SRYSYUKBN20L    (5)
           MOVE    "処方料"        TO  QSUB03-SRYSYUKBN20MEIL (5)
           MOVE    "26"            TO  QSUB03-SRYSYUKBN20L    (6)
           MOVE    "麻毒加算"      TO  QSUB03-SRYSYUKBN20MEIL (6)
           MOVE    "27"            TO  QSUB03-SRYSYUKBN20L    (7)
           MOVE    "調剤技術基本料" TO QSUB03-SRYSYUKBN20MEIL (7)
           MOVE    7               TO  QSUB03-SRYSYUKBN20LST-MAX
      *
           MOVE    "31"            TO  QSUB03-SRYSYUKBN30L    (1)
           MOVE    "皮下筋肉注射"  TO  QSUB03-SRYSYUKBN30MEIL (1)
           MOVE    "32"            TO  QSUB03-SRYSYUKBN30L    (2)
           MOVE    "静脈注射"      TO  QSUB03-SRYSYUKBN30MEIL (2)
           MOVE    "33"            TO  QSUB03-SRYSYUKBN30L    (3)
           MOVE    "点滴注射・その他"
                                   TO  QSUB03-SRYSYUKBN30MEIL (3)
           MOVE    3               TO  QSUB03-SRYSYUKBN30LST-MAX
      *
           MOVE    "90"            TO  QSUB03-SRYSYUKBN90L    (1)
           MOVE    "入院基本"      TO  QSUB03-SRYSYUKBN90MEIL (1)
           MOVE    "92"            TO  QSUB03-SRYSYUKBN90L    (2)
           MOVE    "特定入院・その他"
                                   TO  QSUB03-SRYSYUKBN90MEIL (2)
           MOVE    2               TO  QSUB03-SRYSYUKBN90LST-MAX
      *
           .
       2001-SRYSYUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療行為　項目区分（回数・数量・合計）
      *****************************************************************
       2001-SRYVALITEMLST-SEC              SECTION.
      *
           MOVE    "1"             TO  QSUB03-HOKATSUKBNL    (1)
           MOVE    "含まない"      TO  QSUB03-HOKATSUKBNMEIL (1)
           MOVE    1               TO  QSUB03-HOKATSUKBNLST-MAX
      *
           .
       2001-SRYVALITEMLST-EXT.
           EXIT.
      *****************************************************************
      *    包括区分コンボ初期処理
      *****************************************************************
       2001-HOKATSUKBNLST-SEC              SECTION.
      *
           MOVE    "1"             TO  QSUB03-SRYVALITEML    (1)
           MOVE    "数量を指定する"
                                   TO  QSUB03-SRYVALITEMMEIL (1)
      *
           MOVE    "2"             TO  QSUB03-SRYVALITEML    (2)
           MOVE    "回数を指定する"
                                   TO  QSUB03-SRYVALITEMMEIL (2)
      *
           MOVE    "3"             TO  QSUB03-SRYVALITEML    (3)
           MOVE    "合計を指定する"
                                   TO  QSUB03-SRYVALITEMMEIL (3)
      *
           MOVE    3               TO  QSUB03-SRYVALITEMLST-MAX
      *
           .
       2001-HOKATSUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    AND/OR/NOT条件コンボ初期処理
      *****************************************************************
       2001-JKNLST-SEC                     SECTION.
      *
           MOVE    2               TO  QSUB03-JKNHKNJANUMLST-MAX
           MOVE    "2"             TO  QSUB03-JKNHKNJANUML    (1)
           MOVE    "OR"            TO  QSUB03-JKNHKNJANUMMEIL (1)
           MOVE    "9"             TO  QSUB03-JKNHKNJANUML    (2)
           MOVE    "NOT"           TO  QSUB03-JKNHKNJANUMMEIL (2)
      *
           MOVE    2               TO  QSUB03-JKNHKNNUMLST-MAX
           MOVE    "2"             TO  QSUB03-JKNHKNNUML    (1)
           MOVE    "OR"            TO  QSUB03-JKNHKNNUMMEIL (1)
           MOVE    "9"             TO  QSUB03-JKNHKNNUML    (2)
           MOVE    "NOT"           TO  QSUB03-JKNHKNNUMMEIL (2)
      *
           MOVE    3               TO  QSUB03-JKNKOHNUMLST-MAX
           MOVE    "1"             TO  QSUB03-JKNKOHNUML    (1)
           MOVE    "AND"           TO  QSUB03-JKNKOHNUMMEIL (1)
           MOVE    "2"             TO  QSUB03-JKNKOHNUML    (2)
           MOVE    "OR"            TO  QSUB03-JKNKOHNUMMEIL (2)
           MOVE    "9"             TO  QSUB03-JKNKOHNUML    (3)
           MOVE    "NOT"           TO  QSUB03-JKNKOHNUMMEIL (3)
      *
           MOVE    2               TO  QSUB03-JKNHKNKOHLST-MAX
           MOVE    "1"             TO  QSUB03-JKNHKNKOHL    (1)
           MOVE    "AND"           TO  QSUB03-JKNHKNKOHMEIL (1)
           MOVE    "2"             TO  QSUB03-JKNHKNKOHL    (2)
           MOVE    "OR"            TO  QSUB03-JKNHKNKOHMEIL (2)
      *
           MOVE    3               TO  QSUB03-JKNBYOMEILST-MAX
           MOVE    "1"             TO  QSUB03-JKNBYOMEIL    (1)
           MOVE    "AND"           TO  QSUB03-JKNBYOMEIMEIL (1)
           MOVE    "2"             TO  QSUB03-JKNBYOMEIL    (2)
           MOVE    "OR"            TO  QSUB03-JKNBYOMEIMEIL (2)
           MOVE    "9"             TO  QSUB03-JKNBYOMEIL    (3)
           MOVE    "NOT"           TO  QSUB03-JKNBYOMEIMEIL (3)
      *
           MOVE    2               TO  QSUB03-JKNSRYACTLST-MAX
           MOVE    "1"             TO  QSUB03-JKNSRYACTL    (1)
           MOVE    "AND"           TO  QSUB03-JKNSRYACTMEIL (1)
           MOVE    "2"             TO  QSUB03-JKNSRYACTL    (2)
           MOVE    "OR"            TO  QSUB03-JKNSRYACTMEIL (2)
      *
           .
       2001-JKNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療行為　比較区分（回数・数量・合計）
      *****************************************************************
       2001-SRYVALCOMPLST-SEC              SECTION.
      *
           MOVE    "1"             TO  QSUB03-SRYVALCOMPL    (1)
           MOVE    ":"             TO  QSUB03-SRYVALCOMPFLL  (1)
           MOVE    "等しい"        TO  QSUB03-SRYVALCOMPMEIL (1)
      *
           MOVE    "2"             TO  QSUB03-SRYVALCOMPL    (2)
           MOVE    ":"             TO  QSUB03-SRYVALCOMPFLL  (2)
           MOVE    "以上"          TO  QSUB03-SRYVALCOMPMEIL (2)
      *
           MOVE    "3"             TO  QSUB03-SRYVALCOMPL    (3)
           MOVE    ":"             TO  QSUB03-SRYVALCOMPFLL  (3)
           MOVE    "以下"          TO  QSUB03-SRYVALCOMPMEIL (3)
      *
           MOVE    3               TO  QSUB03-SRYVALCOMPLST-MAX
      *
           .
       2001-SRYVALCOMPLST-EXT.
           EXIT.
      *****************************************************************
      *    介護保険期間識別コンボ初期処理
      *****************************************************************
       2001-KHSKBTLST-SEC                  SECTION.
      *
           MOVE    4                   TO  QSUB03-KHSKBTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-KHSKBTL    (1)
           MOVE    "有効月"            TO  QSUB03-KHSKBTMEIL (1)
           MOVE    "2"                 TO  QSUB03-KHSKBTL    (2)
           MOVE    "開始月"            TO  QSUB03-KHSKBTMEIL (2)
           MOVE    "3"                 TO  QSUB03-KHSKBTL    (3)
           MOVE    "終了月"            TO  QSUB03-KHSKBTMEIL (3)
           MOVE    "4"                 TO  QSUB03-KHSKBTL    (4)
           MOVE    "有効日"            TO  QSUB03-KHSKBTMEIL (4)
      *
           .
       2001-KHSKBTLST-EXT.
           EXIT.
      *****************************************************************
      *    介護認定期間識別コンボ初期処理
      *****************************************************************
       2001-KNSKBTLST-SEC                  SECTION.
      *
           MOVE    4                   TO  QSUB03-KNSKBTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-KNSKBTL    (1)
           MOVE    "有効月"            TO  QSUB03-KNSKBTMEIL (1)
           MOVE    "2"                 TO  QSUB03-KNSKBTL    (2)
           MOVE    "開始月"            TO  QSUB03-KNSKBTMEIL (2)
           MOVE    "3"                 TO  QSUB03-KNSKBTL    (3)
           MOVE    "終了月"            TO  QSUB03-KNSKBTMEIL (3)
           MOVE    "4"                 TO  QSUB03-KNSKBTL    (4)
           MOVE    "有効日"            TO  QSUB03-KNSKBTMEIL (4)
      *
           .
       2001-KNSKBTLST-EXT.
           EXIT.
      *****************************************************************
      *    要介護状態コンボボックス初期値
      *****************************************************************
       2001-KNJOTAILST-SEC                 SECTION.
      *
           MOVE    13                  TO  QSUB03-KNJOTAILST-MAX
      *
           MOVE    "01"                TO  QSUB03-KNJOTAIL    (1)
           MOVE    "自立"              TO  QSUB03-KNJOTAIMEIL (1)
           MOVE    "11"                TO  QSUB03-KNJOTAIL    (2)
           MOVE    "要支援"            TO  QSUB03-KNJOTAIMEIL (2)
           MOVE    "12"                TO  QSUB03-KNJOTAIL    (3)
           MOVE    "要支援１"          TO  QSUB03-KNJOTAIMEIL (3)
           MOVE    "13"                TO  QSUB03-KNJOTAIL    (4)
           MOVE    "要支援２"          TO  QSUB03-KNJOTAIMEIL (4)
           MOVE    "21"                TO  QSUB03-KNJOTAIL    (5)
           MOVE    "要介護１"          TO  QSUB03-KNJOTAIMEIL (5)
           MOVE    "22"                TO  QSUB03-KNJOTAIL    (6)
           MOVE    "要介護２"          TO  QSUB03-KNJOTAIMEIL (6)
           MOVE    "23"                TO  QSUB03-KNJOTAIL    (7)
           MOVE    "要介護３"          TO  QSUB03-KNJOTAIMEIL (7)
           MOVE    "24"                TO  QSUB03-KNJOTAIL    (8)
           MOVE    "要介護４"          TO  QSUB03-KNJOTAIMEIL (8)
           MOVE    "25"                TO  QSUB03-KNJOTAIL    (9)
           MOVE    "要介護５"          TO  QSUB03-KNJOTAIMEIL (9)
           MOVE    "31"                TO  QSUB03-KNJOTAIL    (10)
           MOVE    "事業対象者"        TO  QSUB03-KNJOTAIMEIL (10)
           MOVE    "1x"                TO  QSUB03-KNJOTAIL    (11)
           MOVE    "要支援"            TO  QSUB03-KNJOTAIMEIL (11)
           MOVE    "2x"                TO  QSUB03-KNJOTAIL    (12)
           MOVE    "要介護"            TO  QSUB03-KNJOTAIMEIL (12)
           MOVE    "xx"                TO  QSUB03-KNJOTAIL    (13)
           MOVE    "要支援・要介護"    TO  QSUB03-KNJOTAIMEIL (13)
      *
           .
       2001-KNJOTAILST-EXT.
           EXIT.
      *****************************************************************
      *    検索項目（基本情報）コンボボックス初期処理
      *****************************************************************
       2001-PTSORTLST-SEC                  SECTION.
      *
           MOVE    5                   TO  QSUB03-PTSORTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-PTSORTL (1)
           MOVE    "患者番号"          TO  QSUB03-PTSORTMEIL (1)
      *
           MOVE    "2"                 TO  QSUB03-PTSORTL (2)
           MOVE    "カナ氏名"          TO  QSUB03-PTSORTMEIL (2)
      *
           MOVE    "3"                 TO  QSUB03-PTSORTL (3)
           MOVE    "性別"              TO  QSUB03-PTSORTMEIL (3)
      *
           MOVE    "4"                 TO  QSUB03-PTSORTL (4)
           MOVE    "生年月日"          TO  QSUB03-PTSORTMEIL (4)
      *
           MOVE    "5"                 TO  QSUB03-PTSORTL (5)
           MOVE    "公費受給者番号"    TO  QSUB03-PTSORTMEIL (5)
      *
           .
       2001-PTSORTLST-EXT.
           EXIT.
      *****************************************************************
      *    検索項目（点数情報）コンボボックス初期処理
      *****************************************************************
       2001-TENSORTLST-SEC                  SECTION.
      *
           MOVE    6                   TO  QSUB03-TENSORTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-TENSORTL (1)
           MOVE    "点数"              TO  QSUB03-TENSORTMEIL (1)
      *
           MOVE    "2"                 TO  QSUB03-TENSORTL (2)
           MOVE    "入外区分"          TO  QSUB03-TENSORTMEIL (2)
      *
           MOVE    "3"                 TO  QSUB03-TENSORTL (3)
           MOVE    "患者番号"          TO  QSUB03-TENSORTMEIL (3)
      *
           MOVE    "4"                 TO  QSUB03-TENSORTL (4)
           MOVE    "カナ氏名"          TO  QSUB03-TENSORTMEIL (4)
      *
           MOVE    "5"                 TO  QSUB03-TENSORTL (5)
           MOVE    "性別"              TO  QSUB03-TENSORTMEIL (5)
      *
           MOVE    "6"                 TO  QSUB03-TENSORTL (6)
           MOVE    "生年月日"          TO  QSUB03-TENSORTMEIL (6)
      *
           .
       2001-TENSORTLST-EXT.
           EXIT.
      *****************************************************************
      *    検索項目（診療行為情報）コンボボックス初期処理
      *****************************************************************
       2001-SRYSORTLST-SEC                  SECTION.
      *
           MOVE    6                   TO  QSUB03-SRYSORTLST-MAX
      *
           MOVE    "1"                 TO  QSUB03-SRYSORTL (1)
           MOVE    "患者番号"          TO  QSUB03-SRYSORTMEIL (1)
      *
           MOVE    "2"                 TO  QSUB03-SRYSORTL (2)
           MOVE    "カナ氏名"          TO  QSUB03-SRYSORTMEIL (2)
      *
           MOVE    "3"                 TO  QSUB03-SRYSORTL (3)
           MOVE    "性別"              TO  QSUB03-SRYSORTMEIL (3)
      *
           MOVE    "4"                 TO  QSUB03-SRYSORTL (4)
           MOVE    "生年月日"          TO  QSUB03-SRYSORTMEIL (4)
      *
           MOVE    "5"                 TO  QSUB03-SRYSORTL (5)
           MOVE    "診療コード"        TO  QSUB03-SRYSORTMEIL (5)
      *
           MOVE    "6"                 TO  QSUB03-SRYSORTL (6)
           MOVE    "診療行為カナ名称"  TO  QSUB03-SRYSORTMEIL (6)
      *
           .
       2001-SRYSORTLST-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理 ZOGENPTN 1:日/2:月/3:年
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報検索処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           INITIALIZE                      SHOUKAIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SHOKANRI-HOSPNUM
           MOVE    "0"                 TO  SHOKANRI-NYUGAIKBN
           MOVE    SHOUKAIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索条件管理情報FETCH処理
      *****************************************************************
       900-SHOUKAIKANRI-KEY2-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOUKAIKANRI
      *
           MOVE    "tbl_shoukaikanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SHOUKAIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SHOUKAIKANRI
               INITIALIZE                  SHOUKAIKANRI-REC
           END-IF
           .
      *
       900-SHOUKAIKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタFETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタFETCH処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY21-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター検索処理
      *****************************************************************
       900-HKNNUM-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
      *
           MOVE    QSUB03-BASEYMD      TO  WRK-YMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-YMD             TO  HKN-TEKSTYMD
      *
           MOVE    QSUB03-BASEYMD      TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
      *
       900-HKNNUM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスター読込処理
      *****************************************************************
       900-HKNNUM-KEY3-FET-SEC       SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
      *
       900-HKNNUM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター検索処理
      *****************************************************************
       900-USERBYOMEI-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM          TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名マスター読込処理
      *****************************************************************
       900-USERBYOMEI-KEY4-FET-SEC     SECTION.
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
           .
      *
       900-USERBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
