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
       PROGRAM-ID.             ORCSAGECHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 保険算定用年齢・割合処理
      *  返却エラーコード  : 00:正常　99:パラメータ不正・対象データなし
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/10    NACL- 多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/10/01  生活保護追加
      * 01.00.02     NACL-多々納  02/10/07  船員本人追加
      *                                     ７５歳以上の老人公費有無追加
      * 01.00.03     NACL-多々納  02/11/21  共済下船３月追加
      * 01.00.04     NACL-多々納  03/02/05  原爆単独老人判定をしない
      * 01.00.05     NACL-多々納  03/03/06  ３歳未満判定に自費・地験をはずす
      * 01.00.06     NACL-多々納  03/04/14  労災の割合変更追加
      * 01.00.07     NACL-多々納  03/05/12  自賠責割合変更追加
      * 01.00.08     NACL-多々納  03/09/03  公費期限チェック追加
      * 01.00.09     NACL-多々納  03/11/13  老人区分追加
      * 01.00.10     NACL-多々納  04/01/06  包括まとめ入力追加
      * 01.00.11     NACL-多々納  04/04/21  老人区分２、保険区分追加
      *                                     実年齢追加
      * 01.00.12     NACL-多々納  05/09/15  地方公費単独区分追加
      * 01.00.13     NACL-多々納  05/09/22  長崎県被爆者医療（１８６）追加
      * 01.00.14     NACL-多々納  05/10/04  特別療養費（０６８）追加
      * 02.00.00     NACL-太田    05/10/25  MONFUNC 対応
      * 02.00.01     NACL-多々納  06/01/12  児童保護（０５３）追加
      * 03.00.01     NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      * 03.03.00     NACL-多々納  06/10/24  精神入院単独追加
      * 03.04.00     NACL-多々納  06/12/01  自賠責健保準拠追加
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.01.00     NACL-多々納  07/10/19  公務災害追加
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      *                                     （後期高齢者、就学前判定）
      *                                     （残留邦人）
      * 04.05.00     NACL-多々納  09/08/11  退職国保の６５歳以上判定追加
      * 04.05.00     NACL-多々納  10/08/24  高齢者非該当追加
      * 04.08.00     NACL-多々納  13/07/XX  第三者行為対応
      * 04.08.00     NACL-多々納  14/03/XX  前期高齢者２割対応
      * 04.07.00     NACL-多々納  14/08/08  高齢非該当２割対応
      * 04.08.00     NACL-多々納  17/05/XX  自賠責点数算定区分対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNNUM-OK       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-HKN-ERR         PIC 9(01).
           03  FLG-KOHCHK          PIC 9(01).
           03  FLG-KOH186          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-ERR2            PIC 9(01).
           03  FLG-KAKUNIN         PIC 9(01).
           03  FLG-CHK-OK          PIC 9(01).
           03  FLG-TAHKN           PIC 9(01).
           03  FLG-HI-ROUJIN       PIC 9(01).
           03  FLG-HI-ROUJIN2      PIC 9(01).
      *
           03  FLG-ROU-02          PIC 9(01).
           03  FLG-ROU-03          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM             PIC 9(02).
                   07  WRK-SDD             PIC 9(02).
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-NENREI-YY           PIC 9(03).
           03  WRK-NENREI-MM           PIC 9(02).
           03  WRK-NENREI-DD           PIC 9(02).
      *    ７０歳開始日
           03  WRK-BYMD-70.
               05  WRK-B70-YY          PIC 9(04).
               05  WRK-B70-MM          PIC 9(02).
               05  WRK-B70-DD          PIC 9(02).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
      *
           03  WRK-ZZ9                 PIC ZZZ.
      *
           03  WRK-SEIDONAME           PIC X(20).
      *
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-ROU-NEN             PIC 9(03).
      *
           03  WRK-N-STRYMD.
               05  WRK-N-SYY           PIC 9(04).
               05  WRK-N-SMD           PIC X(04).
      *
           03  WRK-KEY-KOHNUM          PIC X(03).
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *!!
      *    保険組み合わせ負担割合取得サブ
           COPY    "CPORCSFTNRATE.INC".
      *!!
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    基本情報
           COPY    "CPSK1001.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSAGECHKAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  AGECHK-OUT-AREA
           INITIALIZE                  AGECHK-RC
      *
           IF     (AGECHK-HOSPNUM      =   ZERO    ) OR
                  (AGECHK-PTID         NOT NUMERIC )
               MOVE    99              TO  AGECHK-RC
           END-IF
      *    年齢計算
           IF      AGECHK-RC           =   ZERO
               PERFORM 100-NENREI-SEC
           END-IF
      *
      *    保険組合番号があるとき、負担割合を算定する
           IF     (AGECHK-RC           =   ZERO  )
             AND  (AGECHK-HKNCOMBINUM  NOT =   SPACE)
               PERFORM 200-FTNRATE-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       100-NENREI-SEC                SECTION.
      *
      *    患者生年月日
           IF      AGECHK-BIRTHDAY     =   SPACE
      *        患者情報検索
               INITIALIZE                      PTINF-REC
               MOVE    AGECHK-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    AGECHK-PTID         TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   1
                   MOVE    99              TO  AGECHK-RC
                   GO      TO      100-NENREI-EXT
               END-IF
               MOVE    PTINF-BIRTHDAY      TO  WRK-BYMD
               MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           ELSE
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BYMD
               MOVE    AGECHK-BIRTHDAY     TO  WRK-BIRTHDAY
           END-IF
      *    診療日＜生年月日は０歳とする
           IF      AGECHK-SRYYMD       <   WRK-BIRTHDAY
               MOVE    WRK-BIRTHDAY        TO  WRK-SRYYMD
           ELSE
               MOVE    AGECHK-SRYYMD       TO  WRK-SRYYMD
           END-IF
      *
      *    当日の年齢を計算する
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF      WRK-BYMD(5:4)       >   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-SMM   +  12 )
                                           -   WRK-BMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-SMM
                                           -   WRK-BMM
           END-IF
           IF      WRK-BDD             >   WRK-SDD
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                       -   1
           END-IF
      *    年齢期間算定
           IF     (WRK-NENREI-MM       =   ZERO )
             AND  (WRK-NENREI-YY       =   ZERO )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    WRK-BIRTHDAY        TO  LNK-DAY5-START
               MOVE    WRK-SRYYMD          TO  LNK-DAY5-END
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     <=  31
                   MOVE    LNK-DAY5-NISSU2     TO  WRK-NENREI-DD
                   MOVE    ZERO                TO  WRK-NENREI-MM
               END-IF
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI2-YY
           MOVE    WRK-NENREI-MM       TO  AGECHK-NENREI2-MM
           MOVE    WRK-NENREI-DD       TO  AGECHK-NENREI2-DD
      *
      *保険年齢計算
      *    １日以外の誕生日の時は、翌月１日を基準とする
           IF      WRK-BDD             >   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY         =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01              TO  WRK-BDD
           END-IF
      *    ７０歳到達日（前期高齢者開始日）
           MOVE    WRK-BYMD            TO  WRK-BYMD-70
           COMPUTE WRK-B70-YY          =   WRK-B70-YY  +   70
      *
      *    保険年齢を計算する
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           COMPUTE WRK-NENREI-YY       =   WRK-SYY
                                       -   WRK-BYY
           IF     (WRK-BYMD(5:4)       >   WRK-SYMD(5:4))
            AND   (WRK-NENREI-YY       >   ZERO         )
               COMPUTE WRK-NENREI-YY       =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM       =  (WRK-BMM   +  12 )
                                           -   WRK-SMM
           ELSE
               COMPUTE WRK-NENREI-MM       =   WRK-BMM
                                           -   WRK-SMM
           END-IF
           MOVE    WRK-NENREI-YY       TO  AGECHK-NENREI
      *
      *    Ｈ２０．４改正
      *    就学前判定
           IF     (AGECHK-SRYYMD       >=   "20080401" )  AND
                  (WRK-BIRTHDAY        >=   "20020402" )
               PERFORM 1001-SYUGAKU-NEN-SEC
           END-IF
      *
      *    年齢での老人判定
           IF     (AGECHK-SRYYMD       >=   "20080401" )
      *        Ｈ２０．４から
               PERFORM 1003-ROUJIN-CHK2008-SEC
           ELSE
      *        Ｈ２０．４まで
               PERFORM 1002-ROUJIN-CHK2002-SEC
           END-IF
           .
       100-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    就学前判定処理
      *****************************************************************
       1001-SYUGAKU-NEN-SEC                SECTION.
      *
      *    システム日で就学前かの判断
           MOVE    AGECHK-SRYYMD       TO  WRK-SYMD
           IF      WRK-SMMDD           >=  "0401"
               COMPUTE WRK-N-SYY           =   WRK-SYY   -   6
           ELSE
               COMPUTE WRK-N-SYY           =   WRK-SYY   -   7
           END-IF
           MOVE    "0401"              TO  WRK-N-SMD
      *    システム年度−６年の４．１以降の誕生日が就学前
           IF      WRK-BIRTHDAY        >   WRK-N-STRYMD
               MOVE    1               TO  AGECHK-SYUGAKUKBN
           END-IF
      *
           .
       1001-SYUGAKU-NEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢での老人判定（Ｈ２０．３まで）
      *****************************************************************
       1002-ROUJIN-CHK2002-SEC         SECTION.
      *
      *    Ｈ１４．１０以降７０歳を７５歳とする
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           ELSE
      *    Ｈ１４．１０時点で７０歳未満の時
               MOVE    70                   TO  WRK-ROU-NEN
           END-IF
      *    老人年齢以上は老人とする（該当月１日に７０歳・７５歳）
      *    保険年齢計算判定
           IF      AGECHK-NENREI       >=  WRK-ROU-NEN
               MOVE    1               TO  AGECHK-ROUJIN
           END-IF
      *    ６５歳以上の時、老人公費をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
           .
       1002-ROUJIN-CHK2002-EXT.
           EXIT.
      *****************************************************************
      *    年齢での老人判定（Ｈ２０．４から）
      *****************************************************************
       1003-ROUJIN-CHK2008-SEC         SECTION.
      *
      *    ７５歳を迎える日
           MOVE    WRK-BIRTHDAY        TO  WRK-SYMD
           COMPUTE WRK-SYY             =   WRK-SYY     +   75
           IF      WRK-SYMD            <=  AGECHK-SRYYMD
               MOVE    1                   TO  AGECHK-ROUJIN
           END-IF
      *    ６５歳以上の時、後期高齢者をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20011-ROUJIN-CHK3-SEC
                   END-IF
               ELSE
      *            ７５歳以上で、後期高齢者以外保険
                   PERFORM 20011-ROUJIN-CHK3-SEC
                   IF     (AGECHK-ROUJIN2      =   ZERO )
                    AND  ((FLG-HI-ROUJIN       =   1    ) OR
                          (FLG-HI-ROUJIN2      =   1    ))
                    AND   (FLG-TAHKN           =   1    )
                       MOVE    ZERO                TO  AGECHK-ROUJIN2
                   ELSE
                       MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
                   END-IF
               END-IF
           END-IF
           IF     (AGECHK-SRYYMD       >=   "20021001" )  AND
                  (WRK-BIRTHDAY        >=   "19321001" )
               MOVE    75                   TO  WRK-ROU-NEN
           END-IF
           .
       1003-ROUJIN-CHK2008-EXT.
           EXIT.
      *****************************************************************
      *    負担割合を算定処理
      *****************************************************************
       200-FTNRATE-SEC                SECTION.
      *
      *    包括まとめ入力
           IF      AGECHK-HKNCOMBINUM  =   "9999"
               PERFORM 2009-HOUKATUHKN-SEC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    AGECHK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    AGECHK-PTID         TO  COMB-PTID
           MOVE    AGECHK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNCOMBI        =   1
               MOVE    99              TO  AGECHK-RC
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
           MOVE    ZERO               TO  AGECHK-HKNKBN
      *
      *    労災の時、割合の変更
           IF      COMB-HKNNUM         =   "971"
               MOVE    1               TO  AGECHK-HKNKBN
      *        労災情報検索
               PERFORM 2004-PTRSIINF-SELECT-SEC
               IF      PTRSI-HKNKBN        =   "5"
      *            公務災害の割合変更
                   PERFORM 2003-RSIKOMU-FTNRATE-SEC
               ELSE
                   PERFORM 2001-RSI-FTNRATE-SEC
               END-IF
           END-IF
      *
      *    自賠責の時、割合の変更
           IF      COMB-HKNNUM         =   "973"
               MOVE    2               TO  AGECHK-HKNKBN
               PERFORM 2002-JIBAI-FTNRATE-SEC
           END-IF
      *
      *    公害
           IF      COMB-HKNNUM         =   "975"
               MOVE    6               TO  AGECHK-HKNKBN
           END-IF
      *H25.7
      *    第三者行為
           IF     (COMB-KOH1HKNNUM     =   "970" )
               MOVE    7               TO  AGECHK-HKNKBN
           END-IF
      *
      *!!!!
           IF     (COMB-KOH1HKNNUM     =   "970" )
               OR (COMB-KOH1HKNNUM     =   SPACE )
               CONTINUE
           ELSE
      *        地方公費対応負担割合設定
               INITIALIZE                  ORCSFTNRATEAREA
               MOVE    AGECHK-HOSPNUM      TO  LNK-FTNRATE-HOSPNUM
               MOVE    AGECHK-PTID         TO  LNK-FTNRATE-PTID
               MOVE    AGECHK-SRYYMD       TO  LNK-FTNRATE-SRYYMD
               MOVE    AGECHK-HKNCOMBINUM  TO  LNK-FTNRATE-HKNCOMBINUM
               MOVE    "1"                 TO  LNK-FTNRATE-KBN
               CALL    "ORCSFTNRATE"       USING
                                           ORCSFTNRATEAREA
                                           SPA-AREA
                                           HKNCOMBI-REC
               IF      LNK-FTNRATE-RC      =   ZERO
                   MOVE    LNK-FTNRATE-NYUIN   TO  COMB-NYU-KFTNRATE
                   MOVE    LNK-FTNRATE-GAIRAI  TO  COMB-GAI-KFTNRATE
               END-IF
           END-IF
      *
      *!!!!
      *
           IF      AGECHK-NYUGAIKBN    =   "1"
               MOVE    COMB-NYU-KFTNRATE   TO  AGECHK-FTNRATE
           ELSE
               MOVE    COMB-GAI-KFTNRATE   TO  AGECHK-FTNRATE
           END-IF
           IF      AGECHK-FTNRATE      =   ZERO
               MOVE    SPACE               TO  AGECHK-FTNRATEMEI
           ELSE
               MOVE    AGECHK-FTNRATE      TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  AGECHK-FTNRATEMEI(1:3)
               MOVE    "%"                 TO  AGECHK-FTNRATEMEI(4:)
           END-IF
      *    Ｈ１４．１０改正後
      *    ３歳未満の時、２割を上限とする
           IF    ((AGECHK-SRYYMD       >=  "20021001") AND
                  (AGECHK-NENREI       <   03        )) 
      *    Ｈ２０．４ 改正
             OR  ((AGECHK-SRYYMD       >=  "20080401") AND
                  (AGECHK-SYUGAKUKBN   =   1         ))
      *        主保険が自費以外の時
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
      *        自費・治験・労災・自賠責
               IF     (HKN-HKNKOHSBTKBN    =   "8"  OR  "9" )  OR
                      (HKN-HKNKOHSBTKBN    =   "4"          )  OR
                      (COMB-HKNNUM         =   "971"  OR  "973")
      *                特別療養費
                   OR (COMB-HKNNUM         =   "068"  OR  "069")
                   CONTINUE
      *            自費
                   IF      HKN-HKNKOHSBTKBN    =   "8"
                       MOVE    3               TO  AGECHK-HKNKBN
                   END-IF
               ELSE
                   IF      AGECHK-FTNRATE      >   20
                       MOVE    20                  TO  AGECHK-FTNRATE
                       MOVE    " 20%"              TO  AGECHK-FTNRATEMEI
                   END-IF
               END-IF
           END-IF
      *
      *H26.4
      *    前期高齢者２割負担対応
      *         （S19.4.2生まれ以降）
           IF     (AGECHK-SRYYMD       >=  "20140501" )
              AND (WRK-BIRTHDAY        >=  "19440402" )
               MOVE    1                   TO  FLG-ROU-02
           ELSE
               MOVE    ZERO                TO  FLG-ROU-02
           END-IF
      *    老人判定
           IF      AGECHK-SRYYMD       <   "20080401"
               PERFORM 2009-ROUJIN-HANTEI-SEC
           ELSE
      *        Ｈ２０．４から後期高齢者
               PERFORM 20010-ROUJIN-HANTEI2-SEC
           END-IF
      *
      *    地方公費単独判定
           PERFORM 2010-KOHONLY-CHK-SEC
      *
      *    Ｈ１４．１０改正後以下のチェックを行う
           IF      AGECHK-SRYYMD       >=  "20021001"
               CONTINUE
           ELSE
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    Ｈ１５．４改正後以下のチェックを行う
           IF      AGECHK-SRYYMD       >=  "20030401"
               PERFORM 400-PTHKNCHK-SEC
      *        公費の期限切れチェック
               IF      AGECHK-HKNCHK       =   ZERO
                   PERFORM 410-PTKOHCHK-SEC
               END-IF
           END-IF
           IF      AGECHK-HKNCHK   NOT =   ZERO
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    長崎県の被爆者医療（１８６）単独はチェクなし
           IF      FLG-KOH186          =   1
               GO      TO      200-FTNRATE-EXT
           END-IF
      *
      *    ７０歳以上の時、老人であること
           IF      AGECHK-NENREI       >=  70
               PERFORM 2000-RJNCHK-SYORI-SEC
           END-IF
      *    Ｈ１５．４改正後
           IF     (AGECHK-SRYYMD       >=  "20061001")  AND
                  (AGECHK-NENREI       >=  65   AND
                                       <   70        )
      *        ６５歳以上老人公費の割合判定
               PERFORM 2001-RJNCHK65-SYORI-SEC
           END-IF
      *!!
      *    Ｈ２０．４月から退職国保の年齢判定
           IF     (AGECHK-SRYYMD       >=  "20080401")  AND
                  (COMB-HKNNUM         =   "067"  )  AND
                  (AGECHK-NENREI       >=  65     )
               MOVE    9               TO  AGECHK-RJNCHK
           END-IF
      *
           .
       200-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ６５歳以上老人公費の割合判定処理（Ｈ１８．１０から）
      *****************************************************************
       2001-RJNCHK65-SYORI-SEC         SECTION.
      *
      *    老人公費で、支払区分がある時
           PERFORM 294-KOH027-18-CHK-SEC
      *    老人２割エラー
           IF     (FLG-ERR2            =   1   )
               MOVE    3               TO  AGECHK-RJNCHK
           END-IF
           .
       2001-RJNCHK65-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人設定有無判定処理（７０歳以上）
      *****************************************************************
       2000-RJNCHK-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ERR2
           MOVE    ZERO                TO  FLG-KAKUNIN
      *    主保険判定
           IF      COMB-HKNNUM     NOT =   SPACE
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 3001-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM          =   ZERO
      *            労災・自費・治験
                   IF      HKN-HKNKOHSBTKBN    =   "4"  OR  "8"  OR  "9"
                       MOVE    3                   TO  FLG-OK
                   ELSE
                       IF      AGECHK-SRYYMD       >=  "20061001"
      *                    Ｈ１８．１０改正以降のチェック
                           PERFORM 291-HOJOKBN-CHK18-SEC
                       ELSE
      *                    Ｈ１４．１０改正のチェック
                           PERFORM 290-HOJOKBN-CHK14-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    生活保護のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    残留邦人等のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "025" )  OR
                  (COMB-KOH2HKNNUM     =   "025" )  OR
                  (COMB-KOH3HKNNUM     =   "025" )  OR
                  (COMB-KOH4HKNNUM     =   "025" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *
      *    原爆のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "018" )  OR
                  (COMB-KOH2HKNNUM     =   "018" )  OR
                  (COMB-KOH3HKNNUM     =   "018" )  OR
                  (COMB-KOH4HKNNUM     =   "018" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "019" )  OR
                  (COMB-KOH2HKNNUM     =   "019" )  OR
                  (COMB-KOH3HKNNUM     =   "019" )  OR
                  (COMB-KOH4HKNNUM     =   "019" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    結３５のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "011" )  OR
                  (COMB-KOH2HKNNUM     =   "011" )  OR
                  (COMB-KOH3HKNNUM     =   "011" )  OR
                  (COMB-KOH4HKNNUM     =   "011" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    更正のみ時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "015" )  OR
                  (COMB-KOH2HKNNUM     =   "015" )  OR
                  (COMB-KOH3HKNNUM     =   "015" )  OR
                  (COMB-KOH4HKNNUM     =   "015" ))
               IF      COMB-TEKSTYMD        >=   "20060401"
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *    精神入院のみ時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "020" )  OR
                  (COMB-KOH2HKNNUM     =   "020" )  OR
                  (COMB-KOH3HKNNUM     =   "020" )  OR
                  (COMB-KOH4HKNNUM     =   "020" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    精神通院のみ時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "021" )  OR
                  (COMB-KOH2HKNNUM     =   "021" )  OR
                  (COMB-KOH3HKNNUM     =   "021" )  OR
                  (COMB-KOH4HKNNUM     =   "021" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    児童保護措置及び知的障害者福祉のみ時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "053" )  OR
                  (COMB-KOH2HKNNUM     =   "053" )  OR
                  (COMB-KOH3HKNNUM     =   "053" )  OR
                  (COMB-KOH4HKNNUM     =   "053" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    結３４などのみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "010" )  OR
                  (COMB-KOH2HKNNUM     =   "010" )  OR
                  (COMB-KOH3HKNNUM     =   "010" )  OR
                  (COMB-KOH4HKNNUM     =   "010" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "030" )  OR
                  (COMB-KOH2HKNNUM     =   "030" )  OR
                  (COMB-KOH3HKNNUM     =   "030" )  OR
                  (COMB-KOH4HKNNUM     =   "030" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "024" )  OR
                  (COMB-KOH2HKNNUM     =   "024" )  OR
                  (COMB-KOH3HKNNUM     =   "024" )  OR
                  (COMB-KOH4HKNNUM     =   "024" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "079" )  OR
                  (COMB-KOH2HKNNUM     =   "079" )  OR
                  (COMB-KOH3HKNNUM     =   "079" )  OR
                  (COMB-KOH4HKNNUM     =   "079" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *H27.1
      *    難病のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "054" )  OR
                  (COMB-KOH2HKNNUM     =   "054" )  OR
                  (COMB-KOH3HKNNUM     =   "054" )  OR
                  (COMB-KOH4HKNNUM     =   "054" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *H28.7
      *    石綿のみの時
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "066" )  OR
                  (COMB-KOH2HKNNUM     =   "066" )  OR
                  (COMB-KOH3HKNNUM     =   "066" )  OR
                  (COMB-KOH4HKNNUM     =   "066" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *!!!!
      *    高齢者非該当あり
           IF      FLG-HI-ROUJIN2      =   1
               MOVE    1               TO  FLG-OK2
           END-IF
      *!!!!
      *    老人公費で、支払区分がある時
           IF      AGECHK-SRYYMD       >=  "20061001"
      *        Ｈ１８．１０改正以降のチェック
               PERFORM 294-KOH027-18-CHK-SEC
           ELSE
      *        Ｈ１４．１０改正以降のチェック
               PERFORM 293-KOH027-14-CHK-SEC
           END-IF
      *
      *    各公費が老人公費かの判定
           IF      FLG-OK2             =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
      *            公費
                   IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                       MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
                       PERFORM 3001-HKNNUM-CHK-SEC
                       IF      FLG-HKNNUM          =   ZERO
                           IF      HKN-HKNKOHSBTKBN    =   "3" OR "6"
                               PERFORM 2001-KOH-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    老人区分なし
           IF     (FLG-OK              =   ZERO )  AND
                  (FLG-OK2             =   ZERO )
               MOVE    1               TO  AGECHK-RJNCHK
           END-IF
      *
           IF     (AGECHK-RJNCHK       =   ZERO )  AND
                  (FLG-OK          NOT =   3    )
      *    ７５歳以上の時、老人公費があること
               IF      AGECHK-SRYYMD       <   "20080401"
                   IF     (AGECHK-NENREI       >=  WRK-ROU-NEN )  AND
                          (FLG-OK2             =   ZERO)
                       MOVE    2               TO  AGECHK-RJNCHK
                   END-IF
               ELSE
      *            Ｈ２０．４から
      *            ７５歳以上で後期高齢者がない時
                   IF     (AGECHK-ROUJIN       =   1   )  AND
                          (FLG-OK          NOT =   4   )  AND
                          (FLG-OK2             =   ZERO)
                       MOVE    4               TO  AGECHK-RJNCHK
                   END-IF
               END-IF
           END-IF
      *
      *    Ｈ１８．１０から、老人２割エラー
           IF    ((FLG-ERR             =   1   )  AND
                  (FLG-OK2             =   ZERO) )  OR
                  (FLG-ERR2            =   1   )
               IF      (AGECHK-RJNCHK   NOT =   4   )
                   MOVE    3               TO  AGECHK-RJNCHK
               END-IF
           END-IF
      *
      *    国保の前期高齢者以降の確認有無
           IF    (FLG-KAKUNIN          =   1 ) AND
                 (FLG-OK2              =   ZERO)
               MOVE    1               TO  AGECHK-KAKUNINCHK
           END-IF
      *
           .
       2000-RJNCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費支払区分判定処理（Ｈ１４．１０以降）
      *****************************************************************
       293-KOH027-14-CHK-SEC         SECTION.
      *
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "02" ))
               MOVE    1               TO  FLG-OK2
           END-IF
           .
       293-KOH027-14-CHK-EXT.
           EXIT.
      *****************************************************************
      *    老人公費支払区分判定処理（Ｈ１８．１０以降）
      *****************************************************************
       294-KOH027-18-CHK-SEC         SECTION.
      *
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "01" OR "03" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "01" OR "03"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "01" OR "03" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "01" OR "03" ))
               MOVE    1               TO  FLG-OK2
           END-IF
      *    ２割の判定
           IF    ((COMB-KOH1HKNNUM     =   "027" )  AND
                  (COMB-KOH1PAYKBN     =   "02" ))  OR
                 ((COMB-KOH2HKNNUM     =   "027" )  AND
                  (COMB-KOH2PAYKBN     =   "02"  ))  OR
                 ((COMB-KOH3HKNNUM     =   "027" )  AND
                  (COMB-KOH3PAYKBN     =   "02" ))  OR
                 ((COMB-KOH4HKNNUM     =   "027" )  AND
                  (COMB-KOH4PAYKBN     =   "02" ))
               MOVE    1               TO  FLG-ERR2
           END-IF
           .
       294-KOH027-18-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報処理
      *****************************************************************
       2001-KOH-CHK-SEC         SECTION.
      *
           MOVE    1                   TO  FLG-OK2
           .
       2001-KOH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｈ１４．１０改正のチェック処理
      *****************************************************************
       290-HOJOKBN-CHK14-SEC         SECTION.
      *
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTHKNINF        =   ZERO
               IF     (PTHKN-HOJOKBN       =   "8"  OR  "9") OR
                      ((COMB-HKNNUM        =   "060"
                                           OR  "068"       )  AND
                       (PTHKN-HOJOKBN      =   "1"  OR  "2"
                                           OR  "4"  OR  "5"))
                   MOVE    1               TO  FLG-OK
               END-IF
      *        船員で補助区分のある時ＯＫとする
               IF     (COMB-HKNNUM         =   "002" ) AND
                      (PTHKN-HOJOKBN   NOT =   SPACE )
                   IF      PTHKN-HOJOKBN       >=  "A" AND
                                               <=  "F"
                       MOVE    2               TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *        共済で老人区分のある時ＯＫとする
               IF     (COMB-HKNNUM         =   "031"  OR
                                               "032"  OR
                                               "033"  OR
                                               "034" ) AND
                      (PTHKN-HOJOKBN       =   "B"  OR  "E")
                   MOVE    2               TO  FLG-OK
               END-IF
      *        国保で老人区分のある時ＯＫとする
      *            前期高齢者開始日以降に確定がない時、警告
               IF     (COMB-HKNNUM         =   "060"
                                           OR  "068"       )
                   IF     (PTHKN-TEKSTYMD      <   WRK-BYMD-70 )
                      AND (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
                      AND (PTHKN-KAKUNINYMD    <   WRK-BYMD-70 )
                       MOVE    1               TO  FLG-KAKUNIN
                   END-IF
               END-IF
           END-IF
           .
       290-HOJOKBN-CHK14-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｈ１８．１０改正のチェック処理
      *****************************************************************
       291-HOJOKBN-CHK18-SEC         SECTION.
      *
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           EVALUATE    COMB-HKNNUM
               WHEN    "060"
               WHEN    "068"
      *            国保
      *            組合国保以外は、２割は警告
                   IF      PTHKN-HKNJANUM (3:1)    =   "3"
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                       IF      PTHKN-HOJOKBN       =   "2"
                           MOVE    1               TO  FLG-ERR
                       END-IF
      *                H26.5から２割対応
                       IF      FLG-ROU-02      =   1
                           MOVE    ZERO            TO  FLG-ERR
                           IF      PTHKN-HOJOKBN       =   "1"
                           AND     FLG-ROU-03          =   ZERO
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
      *
                   END-IF
      *            前期高齢者開始日以降に確定がない時、警告
                   IF     (PTHKN-TEKSTYMD      <   WRK-BYMD-70 )
                      AND (PTHKN-TEKEDYMD      >=  WRK-BYMD-70 )
                      AND (PTHKN-KAKUNINYMD    <   WRK-BYMD-70 )
                       MOVE    1               TO  FLG-KAKUNIN
                   END-IF
               WHEN    "002"
      *        船員
                   IF      PTHKN-HOJOKBN       =   "1"   OR  "2"
                                               OR  "3"   OR  SPACE
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
      *                ２割で継続以外はエラー
                       IF      PTHKN-HOJOKBN       =   "8"
                                                   OR  "D"
                                                   OR  "E"
                                                   OR  "F"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
      *                H26.5から２割対応
                       IF      FLG-ROU-02      =   1
                           MOVE    ZERO            TO  FLG-ERR
      *                    １割で継続以外はエラー
                           IF     (PTHKN-HOJOKBN   =   "9"
                                                   OR  "A"
                                                   OR  "B"
                                                   OR  "C" )
                           AND    (PTHKN-CONTKBN   =   SPACE)
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *            Ｈ２０．４から本人であれば後期高齢者
                   IF     (AGECHK-SRYYMD       >=  "20080401" ) AND
                          (AGECHK-ROUJIN       =   1          ) AND
                          (PTHKN-HONKZKKBN     =   "1"        ) AND
                          (PTHKN-HOJOKBN       >=  "A"  AND
                                               <=  "I"        )
                       MOVE    4               TO  FLG-OK
                   END-IF
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
      *            共済で老人区分のある時ＯＫとする
                   IF     PTHKN-HOJOKBN        =   SPACE
                                               OR  "2"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
      *                ２割で継続以外はエラー
                       IF      PTHKN-HOJOKBN       =   "8"
                                                   OR  "E"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       ELSE
                           MOVE    2               TO  FLG-OK
                       END-IF
      *                H26.5から２割対応
                       IF      FLG-ROU-02      =   1
                           MOVE    ZERO            TO  FLG-ERR
                           MOVE    1               TO  FLG-OK
      *                    １割で継続以外はエラー
                           IF      PTHKN-HOJOKBN       =   "9"
                                                       OR  "B"
                               IF      PTHKN-CONTKBN   =   SPACE
                                   MOVE    1               TO  FLG-ERR
                               END-IF
                           ELSE
                               MOVE    2               TO  FLG-OK
                           END-IF
                       END-IF
                   END-IF
      *        Ｈ２０．４から（後期高齢者）
               WHEN    "039"
               WHEN    "040"
                   MOVE    4                   TO  FLG-OK
               WHEN    OTHER
      *            その他
                   IF     PTHKN-HOJOKBN        =   "7"
                                               OR  "8"
                                               OR  "9"
                       MOVE    1               TO  FLG-OK
      *                ２割で継続以外はエラー
                       IF      PTHKN-HOJOKBN       =   "8"
                           IF      PTHKN-CONTKBN   =   SPACE
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
      *                H26.5から２割対応
                       IF      FLG-ROU-02      =   1
                           MOVE    ZERO            TO  FLG-ERR
      *                    １割で継続以外はエラー
                           IF      PTHKN-HOJOKBN       =   "9"
                               IF      PTHKN-CONTKBN   =   SPACE
                                   MOVE    1               TO  FLG-ERR
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
           .
       291-HOJOKBN-CHK18-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時、割合の変更処理
      *****************************************************************
       2001-RSI-FTNRATE-SEC         SECTION.
      *
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *
               IF      SYS-4001-RSIIRYOKBN     =   "1"
                   MOVE    ZERO                TO  COMB-GAI-KFTNRATE
                   MOVE    ZERO                TO  COMB-NYU-KFTNRATE
               ELSE
                   MOVE    100                 TO  COMB-GAI-KFTNRATE
                   MOVE    100                 TO  COMB-NYU-KFTNRATE
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-RSI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責割合の変更処理
      *****************************************************************
       2002-JIBAI-FTNRATE-SEC         SECTION.
      *
      *    患者保険情報検索
           PERFORM 2004-PTRSIINF-SELECT-SEC
      *    自賠責患者請求区分
           IF     (FLG-PTRSIINF        =   ZERO )  AND
                  (PTRSI-HKNKBN        =   "4"  )
               EVALUATE    PTRSI-JIBAISEIKBN
                   WHEN    "1"
                       MOVE    100             TO  COMB-GAI-KFTNRATE
                       MOVE    100             TO  COMB-NYU-KFTNRATE
                   WHEN    "2"
                       MOVE    ZERO            TO  COMB-GAI-KFTNRATE
                       MOVE    ZERO            TO  COMB-NYU-KFTNRATE
               END-EVALUATE
           END-IF
      *
      *H29.4
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
     *
      *    自賠責点数区分の設定があれば優先
           EVALUATE    PTRSI-JIBAIKBN
               WHEN    "1"
      *        労災準拠
                   MOVE    2               TO  AGECHK-HKNKBN
               WHEN    "2"
      *        健保準拠
                   MOVE    4               TO  AGECHK-HKNKBN
               WHEN    OTHER
      *        システム管理準拠
                   IF      SYS-4001-JIBAIKBN   =   "2"
      *                健保準拠
                       MOVE    4               TO  AGECHK-HKNKBN
                   END-IF
           END-EVALUATE
           .
      *
       2002-JIBAI-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公務災害の割合変更処理
      *****************************************************************
       2003-RSIKOMU-FTNRATE-SEC         SECTION.
      *
      *    患者請求区分
           EVALUATE    PTRSI-JIBAISEIKBN
               WHEN    "1"
                   MOVE    100             TO  COMB-GAI-KFTNRATE
                   MOVE    100             TO  COMB-NYU-KFTNRATE
               WHEN    "2"
                   MOVE    ZERO            TO  COMB-GAI-KFTNRATE
                   MOVE    ZERO            TO  COMB-NYU-KFTNRATE
           END-EVALUATE
      *
      *    公務災害区分の判定
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
      *        健保準拠
      *        公務災害区分
               IF      SYS-4001-KOMUKBN        =   "2"
                   MOVE    5               TO  AGECHK-HKNKBN
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-RSIKOMU-FTNRATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報検索処理
      *****************************************************************
       2004-PTRSIINF-SELECT-SEC         SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
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
           MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2004-PTRSIINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    老人・一般定処理
      *****************************************************************
       2009-ROUJIN-HANTEI-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-ROUJIN2
      *    生活保護で、６５歳以上で老人公費を持っている場合、老人
           IF     (COMB-HKNNUM         =   SPACE )  AND
                 ((COMB-KOH1HKNNUM     =   "012" )  OR
                  (COMB-KOH2HKNNUM     =   "012" )  OR
                  (COMB-KOH3HKNNUM     =   "012" )  OR
                  (COMB-KOH4HKNNUM     =   "012" ))
               IF     (AGECHK-NENREI       >=  65  )  AND
                      (AGECHK-ROUJIN       =   ZERO)
                   PERFORM 2009-ROUJIN-CHK-SEC
               END-IF
           ELSE
      *        老人公費を選択している場合、老人
               IF     (COMB-KOH1HKNNUM     =   "027" )  OR
                      (COMB-KOH2HKNNUM     =   "027" )  OR
                      (COMB-KOH3HKNNUM     =   "027" )  OR
                      (COMB-KOH4HKNNUM     =   "027" )
                   MOVE    1               TO  AGECHK-ROUJIN
               END-IF
           END-IF
      *    精神通院・更正で６５歳以上で老人公費を持っている場合、老人
           IF     (COMB-HKNNUM         =   SPACE )  AND
                  (AGECHK-NENREI       >=  65    )  AND
                  (AGECHK-ROUJIN       =   ZERO  )  AND
                  (AGECHK-SRYYMD       >=  "20060401" )
               IF     ((COMB-KOH1HKNNUM     =   "015" )  OR
                       (COMB-KOH2HKNNUM     =   "015" )  OR
                       (COMB-KOH3HKNNUM     =   "015" )  OR
                       (COMB-KOH4HKNNUM     =   "015" ))  OR
                      ((COMB-KOH1HKNNUM     =   "021" )  OR
                       (COMB-KOH2HKNNUM     =   "021" )  OR
                       (COMB-KOH3HKNNUM     =   "021" )  OR
                       (COMB-KOH4HKNNUM     =   "021" ))
      *            精神入院
                 OR  ((COMB-KOH1HKNNUM     =   "020" )  OR
                      (COMB-KOH2HKNNUM     =   "020" )  OR
                      (COMB-KOH3HKNNUM     =   "020" )  OR
                      (COMB-KOH4HKNNUM     =   "020" ))
                   PERFORM 2009-ROUJIN-CHK-SEC
               END-IF
           END-IF
      *    ６５歳以上の時、老人公費をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20010-ROUJIN-CHK2-SEC
                   END-IF
               ELSE
                   MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *
      *    労災・自賠責・公害の時、すべて一般へ
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
                                       OR  "975"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       2009-ROUJIN-HANTEI-EXT.
           EXIT.
      *****************************************************************
      *    老人・一般定（Ｈ２０．４から）処理
      *****************************************************************
       20010-ROUJIN-HANTEI2-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-ROUJIN2
      *    後期高齢者を選択している場合、老人
           IF      COMB-HKNNUM         =   "039"
                                       OR  "040"
               MOVE    1               TO  AGECHK-ROUJIN
           END-IF
      *    ６５歳以上の時、後期高齢者をもっているかの判定
           IF      AGECHK-NENREI       >=  65
               IF      AGECHK-ROUJIN       =   ZERO
                   IF      AGECHK-ROUJIN2      =   ZERO
                       PERFORM 20011-ROUJIN-CHK3-SEC
                   END-IF
               ELSE
                   IF     (COMB-HKNNUM         =   "039"
                                               OR  "040"
                                               OR  SPACE )
                   OR     (COMB-HKNNUM         >   "900" )
                       MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
                   ELSE
                       PERFORM 20011-ROUJIN-CHK3-SEC
                       IF     (AGECHK-ROUJIN2      =   ZERO )
                        AND  ((FLG-HI-ROUJIN       =   1    )  OR
                              (FLG-HI-ROUJIN2      =   1    ))
                        AND   (FLG-TAHKN           =   1    )
                           MOVE    ZERO            TO  AGECHK-ROUJIN2
                           MOVE    ZERO            TO  AGECHK-ROUJIN
                       END-IF
                       MOVE    AGECHK-ROUJIN       TO  AGECHK-ROUJIN2
      *H26.8
      *                後期非該当
                       IF     (FLG-HI-ROUJIN       =   1 )
                         AND  (AGECHK-SRYYMD       >=  "20140501" )
                           MOVE    1                   TO  FLG-ROU-02
                           MOVE    1                   TO  FLG-ROU-03
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    生活保護、公費単独で後期公費を持っている場合、老人
           IF     (COMB-HKNNUM         =   SPACE )  AND
                  (AGECHK-NENREI       >=  65    )  AND
                  (AGECHK-ROUJIN       =   ZERO  )
               PERFORM 20012-KOH-ROJIN-SEC
           END-IF
      *    労災・自賠責・公害の時、すべて一般へ
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
                                       OR  "975"
               MOVE    ZERO            TO  AGECHK-ROUJIN
           END-IF
      *
           .
       20010-ROUJIN-HANTEI2-EXT.
           EXIT.
      *****************************************************************
      *    老人公費判定処理
      *****************************************************************
       2009-ROUJIN-CHK-SEC         SECTION.
      *
      *    公費編集
           MOVE    "027"               TO  WRK-KEY-KOHNUM
           PERFORM 9001-PTKOHINF-KEY06-SEC
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       2009-ROUJIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費判定処理
      *****************************************************************
       20010-ROUJIN-CHK2-SEC         SECTION.
      *
      *    公費編集
           MOVE    "027"               TO  WRK-KEY-KOHNUM
           PERFORM 9001-PTKOHINF-KEY06-SEC
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  AGECHK-ROUJIN2
           END-IF
      *
           .
      *
       20010-ROUJIN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢者判定処理
      *****************************************************************
       20011-ROUJIN-CHK3-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TAHKN
      *    後期高齢者該当
           INITIALIZE                  PTHKNINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTHKN-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTHKN-PTID
           MOVE    AGECHK-SRYYMD       TO  PTHKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           PERFORM UNTIL      (FLG-PTHKNINF    =   1   )
                         OR   (AGECHK-ROUJIN2  =   1   )
               IF      PTHKN-SAKJOKBN      =   "1"
                   CONTINUE
               ELSE
                   IF      PTHKN-HKNNUM        =   "039"
                                               OR  "040"
                       MOVE    1               TO  AGECHK-ROUJIN2
                   ELSE
      *                後期高齢者以外の保険有無
                       IF      PTHKN-HKNNUM        <   "900"
                           MOVE    1               TO  FLG-TAHKN
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    後期高齢者該当
           IF      AGECHK-ROUJIN2      =   ZERO
               MOVE    "968"               TO  WRK-KEY-KOHNUM
               PERFORM 9001-PTKOHINF-KEY06-SEC
               IF      FLG-PTKOHINF        =   ZERO
                   MOVE    1                   TO  AGECHK-ROUJIN2
               END-IF
           END-IF
      *    後期高齢者非該当
           IF      AGECHK-ROUJIN2      =   ZERO
               MOVE    "977"               TO  WRK-KEY-KOHNUM
               PERFORM 9001-PTKOHINF-KEY06-SEC
               IF      FLG-PTKOHINF        =   ZERO
                   MOVE    1                   TO  FLG-HI-ROUJIN
               END-IF
           END-IF
      *!!!
      *    高齢者非該当(H22.9)
           MOVE    "976"               TO  WRK-KEY-KOHNUM
           PERFORM 9001-PTKOHINF-KEY06-SEC
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    1                   TO  FLG-HI-ROUJIN2
           END-IF
           .
       20011-ROUJIN-CHK3-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費単独老人判定処理
      *****************************************************************
       20012-KOH-ROJIN-SEC         SECTION.
      *
           IF     (COMB-HKNNUM         =   SPACE )
      *        生活保護・精神通院・更正
               IF    ((COMB-KOH1HKNNUM     =   "012" )  OR
                      (COMB-KOH2HKNNUM     =   "012" )  OR
                      (COMB-KOH3HKNNUM     =   "012" ))
                 OR  ((COMB-KOH1HKNNUM     =   "015" )  OR
                      (COMB-KOH2HKNNUM     =   "015" )  OR
                      (COMB-KOH3HKNNUM     =   "015" )  OR
                      (COMB-KOH4HKNNUM     =   "015" ))
                 OR  ((COMB-KOH1HKNNUM     =   "021" )  OR
                      (COMB-KOH2HKNNUM     =   "021" )  OR
                      (COMB-KOH3HKNNUM     =   "021" )  OR
                      (COMB-KOH4HKNNUM     =   "021" ))
      *            精神入院
                 OR  ((COMB-KOH1HKNNUM     =   "020" )  OR
                      (COMB-KOH2HKNNUM     =   "020" )  OR
                      (COMB-KOH3HKNNUM     =   "020" )  OR
                      (COMB-KOH4HKNNUM     =   "020" ))
      *            残留邦人
                 OR  ((COMB-KOH1HKNNUM     =   "025" )  OR
                      (COMB-KOH2HKNNUM     =   "025" )  OR
                      (COMB-KOH3HKNNUM     =   "025" )  OR
                      (COMB-KOH4HKNNUM     =   "025" ))
      *H27.1
      *            難病
                 OR  ((COMB-KOH1HKNNUM     =   "054" )  OR
                      (COMB-KOH2HKNNUM     =   "054" )  OR
                      (COMB-KOH3HKNNUM     =   "054" )  OR
                      (COMB-KOH4HKNNUM     =   "054" ))
                   MOVE    AGECHK-ROUJIN2      TO  AGECHK-ROUJIN
               END-IF
           END-IF
           .
       20012-KOH-ROJIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費単独判定処理
      *****************************************************************
       2010-KOHONLY-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH186
           MOVE    ZERO                TO  FLG-KOHCHK
      *
      *    長崎県の被爆者医療（１８６）は単独可能
           IF      (COMB-HKNNUM         =   SPACE )  AND
                   (COMB-KOH1HKNNUM     =   "186" )  AND
                   (COMB-KOH2HKNNUM     =   SPACE )  AND
                   (COMB-KOH3HKNNUM     =   SPACE )  AND
                   (COMB-KOH4HKNNUM     =   SPACE )
               PERFORM 20101-KOH186-CHK-SEC
           END-IF
           IF     (FLG-KOH186           =   ZERO  )  AND
                  (COMB-HKNNUM          =   SPACE )  AND
                  (COMB-KOH1HKNNUM NOT  =   SPACE )
               MOVE    ZERO             TO  FLG-CHK-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF    (COMB-KOHHKNNUM(IDX)  =   SPACE  )
                     OR  (COMB-KOHHKNNUM(IDX)  >=  "100"  AND
                                               <=  "899"  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK-OK
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   4   )
                             OR   (FLG-CHK-OK  =   1   )
      *            公費
                   IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                       MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                       MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
                       PERFORM 3001-HKNNUM-CHK-SEC
                       IF     (FLG-HKNNUM          =   ZERO  )
                          AND (HKN-HKNKOHSBTKBN    NOT =   "6" )
      *                    単独公費以外
                           MOVE    1               TO  FLG-KOHCHK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-KOHCHK          =   1
               MOVE    1                   TO  AGECHK-KOHCHK
           ELSE
               MOVE    ZERO                TO  AGECHK-KOHCHK
           END-IF
           .
      *
       2010-KOHONLY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長崎県の被爆者医療（１８６）処理
      *****************************************************************
       20101-KOH186-CHK-SEC         SECTION.
      *
      *    システム管理検索（基本情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    AGECHK-SRYYMD       TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    AGECHK-HOSPNUM      TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        県番号
               IF      SYS-1001-PREFNUM    =   "42"
                   MOVE    1                TO  FLG-KOH186
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20101-KOH186-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報処理
      *****************************************************************
       3001-HKNNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPNUM      TO  HKN-HOSPNUM
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の切り替えチェック処理
      *****************************************************************
       400-PTHKNCHK-SEC         SECTION.
      *
           IF      COMB-HKNNUM     NOT =   SPACE
      *
               MOVE    COMB-HKNNUM         TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               PERFORM 4101-KIKANCHK-CHK-SEC
               IF      FLG-HKNNUM-OK       =   ZERO
                   MOVE    1               TO  AGECHK-HKNCHK
               END-IF
           END-IF
      *
           .
       400-PTHKNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の切り替えチェック処理
      *****************************************************************
       410-PTKOHCHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                       OR     (AGECHK-HKNCHK   NOT =   ZERO )
      *        公費
               IF      COMB-KOHHKNNUM (IDX)    NOT =   SPACE
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  WRK-HKNNUM
                   MOVE    COMB-KOHPAYKBN (IDX)    TO  WRK-PAYKBN
      *            公費の期限切れチェック
                   PERFORM 4101-KIKANCHK-CHK-SEC
                   IF      FLG-HKNNUM-OK       =   ZERO
                       MOVE    2               TO  AGECHK-HKNCHK
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       410-PTKOHCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の切り替えチェック処理
      *****************************************************************
       4101-KIKANCHK-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    AGECHK-HOSPNUM      TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    AGECHK-SRYYMD       TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    ZERO                TO  FLG-HKNNUM-OK
           PERFORM
               UNTIL   (FLG-HKNNUM     =   1  )  OR
                       (FLG-HKNNUM-OK  =   1  )
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        >=  COMB-TEKEDYMD )
      *            保険期間対象あり
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               IF     (HKN-TEKSTYMD        <=  COMB-TEKSTYMD ) AND
                      (HKN-TEKEDYMD        <   COMB-TEKEDYMD ) AND
                      (HKN-TEKEDYMD        >=  AGECHK-SRYYMD )
      *            診療日に保険期間対象あり
                   MOVE    1                   TO  FLG-HKNNUM-OK
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4101-HKNNUM-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括まとめ入力処理
      *****************************************************************
       2009-HOUKATUHKN-SEC         SECTION.
      *
           MOVE    ZERO                TO  AGECHK-FTNRATE
           MOVE    SPACE               TO  AGECHK-FTNRATEMEI
      *
      *    老人判定
           IF     (AGECHK-NENREI       >=  65  )  AND
                  (AGECHK-ROUJIN       =   ZERO)
               PERFORM 2009-ROUJIN-CHK-SEC
           END-IF
      *
           .
       2009-HOUKATUHKN-EXT.
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
      *
      *****************************************************************
      *    患者公費検索処理（KEY6）
      *****************************************************************
       9001-PTKOHINF-KEY06-SEC         SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    AGECHK-HOSPNUM      TO  PTKOH-HOSPNUM
           MOVE    AGECHK-PTID         TO  PTKOH-PTID
           MOVE    WRK-KEY-KOHNUM      TO  PTKOH-KOHNUM
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKSTYMD
           MOVE    AGECHK-SRYYMD       TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-PTKOHINF-KEY06-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
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
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    AGECHK-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *
      *****************************************************************
      *    患者公費情報読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC        SECTION.
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込み処理
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
