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
       PROGRAM-ID.             ORCSCKTWKSRY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ入力
      *  コンポーネント名  : ワーク診療行為展開、登録処理サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/12    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/09/XX  '0086'コメントコード対応
      *  04.08.00    NACL-多々納  14/05/29  一時ファイル変更
      *  04.07.00    NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      *  04.07.00    NACL-多々納  15/03/XX  Ｈ２７年４月改正対応
      *  04.08.00    NACL-多々納  17/07/XX  心身療法２０未満加算等削除対応
      *  04.08.00    NACL-多々納  17/05/21  自費金額桁数チェック対応
      *  05.00.00    NACL-多々納  20/03/XX  コメントコード桁数対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-WKSRY           PIC 9(01).
           03  FLG-WKSRY02         PIC 9(01).
           03  FLG-OUTSYU          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-SYORIFLG        PIC 9(01).
           03  FLG-ZENKAI          PIC 9(01).
           03  FLG-HEN-CHK         PIC 9(01).
           03  FLG-AUTO-OK         PIC 9(01).
           03  FLG-INGAIKBN        PIC 9(01).
           03  FLG-INGAIKBN-ARI    PIC 9(01).
           03  FLG-SET             PIC 9(01).
      *
           03  FLG-SYUBETU         PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-FUKU            PIC 9(01).
           03  FLG-FIRUMU          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-AUTO-ARI        PIC 9(01).
           03  FLG-AUTO-NASI       PIC 9(01).
      *
           03  FLG-WKSYORIFLG      PIC 9(01).
      *
           03  FLG-TIME            PIC 9(01).
           03  FLG-HKN999          PIC 9(01).
      *
           03  FLG-DAY             PIC 9(01).
      *
           03  FLG-RSITEN          PIC 9(01).
           03  FLG-TEIGEN30-ZAI    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDW2                    PIC 9(04).
           03  IDW-END                 PIC 9(04).
           03  IDW-STR                 PIC 9(04).
      *
           03  IDX-TEI                  PIC 9(04).
           03  IDX-1                    PIC 9(04).
           03  IDX-K                    PIC 9(04).
           03  IDX-K2                   PIC 9(04).
      *
           03  IDX-F                    PIC 9(04).
           03  IDH                      PIC 9(04).
           03  IDH1                     PIC 9(04).
           03  IDH2                     PIC 9(04).
           03  IDH3                     PIC 9(04).
      *
           03  IDX-HZN                 PIC 9(04).
           03  IDX-STR                 PIC 9(04).
           03  IDX-END                 PIC 9(04).
      *
           03  IDT                     PIC 9(04).
           03  IDXW                    PIC 9(04).
           03  IDXW2                   PIC 9(04).
           03  IDXL                    PIC 9(04).
           03  IDD                     PIC 9(02).
           03  IDD2                    PIC 9(02).
      *
           03  IDX-KAI1            PIC 9(04).
           03  IDX-KAI2            PIC 9(02).
           03  IDX-KAI3            PIC 9(04).
           03  IDX-KAI             PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
      *!!!
           03  IDW2-H                  PIC 9(04).
      *
           03  IDG                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRCD1              PIC X(04).
           03  WRK-ERRCD2              PIC X(04).
           03  WRK-DAY                 PIC 9(02).
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD                PIC X(05).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-SYUTEN2-R           PIC  9(07).
               05  WRK-TENSUKEI            PIC 9(08).
               05  WRK-KAISU               PIC 9(04).
               05  WRK-KINGAK              PIC 9(08).
               05  WRK-KAISUKEI            PIC 9(08).
      *
               05  WRK-MEINUM              PIC 9(04).
               05  WRK-RENNUM              PIC 9(04).
      *
               05  WRK-SRYKBN              PIC X(02).
               05  WRK-SRYSYUKBN           PIC X(03).
      *
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-PARAEDABAN          PIC 9(04).
      *
           03  WRK-EDANUM              PIC 9(03).
      *
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *    複数保険
           03  WRK-FTNRATE         PIC 9(03).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-KOH1HKNNUM      PIC X(03).
           03  WRK-KOH2HKNNUM      PIC X(03).
           03  WRK-KOH3HKNNUM      PIC X(03).
           03  WRK-KOH4HKNNUM      PIC X(03).
      *    労災保険用
           03  WRK-TENSU           PIC S9(08).
           03  WRK-EN              PIC 9(08).
      *
           03  WRK-SYU-DENPNUM     PIC 9(07).
           03  WRK-TNSSRYSYUKBN        PIC X(03).
      *
           03  WRK-HOUKATU-ZAI         PIC 9(01).
      *
           03  WRK-YAKUHYO             PIC X(01).
           03  WRK-GENTEN              PIC X(01).
      *    回数・日数
           03  WRK-NYUIN-AREA.
               05  WRK-NYU-KAI        PIC 9(03).
               05  WRK-NYUINDAY       PIC 9(01)    OCCURS  31.
           03  WRK-NYUIN-CHK-AREA.
               05  WRK-NYUIN-KAI       PIC 9(03).
           03  IDX-N2                  PIC 9(02).
      *
           03  WRK-KANSURYO            PIC 9(05)V9(5).
      *
      *    労災施術加算等用
           03  WRK-ENTANKA             PIC S9(08).
           03  IDR-1                   PIC 9(04).
           03  IDR-2                   PIC 9(04).
           03  WRK-TENKEISAN           PIC 9(08)V99.
           03  WRK-TENSUKEI-9          PIC S9(08).
           03  WRK-TENSUKEI-G          PIC S9(08).
           03  WRK-TENSUKEI-K          PIC S9(08).
      *H29.2
      *    在医総管判定
           03  WRK-ZS-GRP                  PIC X(02).
           03  WRK-ZS-KBN                  PIC X(01).
           03  WRK-ZS-KBN2                 PIC X(01).
      *
           03  FLG-CHKAUTO                 PIC 9(01).
           03  FLG-CHKEND                  PIC 9(01).
           03  IDXZ                        PIC 9(04).
           03  WRK-ZKS-GRP             PIC X(02).
           03  WRK-ZKS-KBN             PIC X(01).
      *H30.5
      *    保険外の合計金額７桁オーバーチェック
           03  WRK-SUM-HKNTEN             PIC 9(08).
           03  WRK-SUM-TGMONEY            PIC 9(08).
           03  WRK-SUM-TGMONEY-TAX        PIC 9(08).
           03  WRK-SUM-JIHI-TOTAL         PIC 9(08).
           03  WRK-SUM-JIHI-TOTAL-TAX     PIC 9(08).
      *!!!
      *
      *    収納集計点数内容
       01  TBL-K02SCR-AREA.
           03  TBL-K02SCR-OCC          OCCURS   400.
               05  TBL-SYU-TENSU       PIC 9(07).
      *
      *    集計収納請求内容
       01  WRK-SUM-AREA.
           03  SUM-SYU-SKY-NAIYOU.
               05  SUM-SYU-SKY-TBL             OCCURS   17.
      *    保険点数
                   09  SUM-SYU-HKNTEN          PIC  S9(08).
      *
       01  TBL-KAISUU-AREA.
           03  TBL-KAISUU-OCCU         OCCURS   10.
               05  TBL-KAISU           PIC 9(03).
               05  TBL-DAY             PIC 9(02)
                                       OCCURS   31.
      *
      *    診療日毎の連番
       01  TBL-DAY-GRP-G.
           03  TBL-DAY-OCC             OCCURS   31.
               05  TBL-DAY-EDANUM      PIC 9(03).
               05  TBL-DAY-ZAINUM      PIC 9(04).
      *
      *
      *画面最大行
      *01  WRK-GYO-MAX                 PIC 9(04)   VALUE   400.
      *
      *    小児科外来診察料テーブル
           COPY   "CMSYONIKATBL.INC".
      *
      *H26.10
      * 向精神薬多剤投与減算コード
       01  CONS-TAZAI-SRYCD        PIC X(09)   VALUE    "630010005".
      * （精減）
       01  CONS-SGEN-SRYCD         PIC X(09)   VALUE    "820000166".
      *
      *H27.4
      * 低紹介率病院　減算コード
       01  CONS-TEIHP-SRYCD        PIC X(09)   VALUE    "630010006".
      * （減）（内服逓減と同じ）
       01  CONS-TGEN-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *H28.4
      * 湿布薬枚数減算
       01  CONS-SPGEN-SRYCD        PIC X(09)   VALUE    "630010007".
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *
      *H29.7
      *    在宅時医療総合管理料コードテーブル（H28.4分)
           COPY     "CMZISOTBL2016.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定用共通パラメタ
      *    COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    診察料自動発生パラメタ
      *    COPY    "CPORCSC10S.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
      *    附加情報
       01  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
      *    ワーク診療行為マスタ−（パラメタ）
       01  PARA-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //PARA-WKSRY-//.
      *    附加情報を追加
           03  PARA-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //PARA-WKSRYP-//.
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
      *
           COPY    "ORCA-SPA".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *v4.8
       01  HZN-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //HZN-WKSRY-//.
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *
           COPY    "CPORCSCKTWKSRY.INC".
      *    画面ＳＰＡ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KT01SCR-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSCKTWKSRYAREA
           SPA-AREA
           SPA-KT01
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      **** INITIALIZE                  STS-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           EVALUATE    ORCSCWKSRY-KBN
      *        展開処理
               WHEN    "1"
                   PERFORM 100-WKSRYACT-TENKAI-SEC
      *        ＤＯ展開処理
               WHEN    "4"
                   PERFORM 400-DOWKSRY-TENKAI-SEC
      *        ワーク診療行為マスタ登録（画面内）
               WHEN    "2"
                   PERFORM 200-GMN-WKSRYACT-SEC
      *        パラメタ登録（登録時）
               WHEN    "3"
                   PERFORM 300-TOUROKU-PARA-SEC
      *        パラメタ登録（他画面遷移）
               WHEN    "5"
                   PERFORM 400-SENI-PARA-SEC
           END-EVALUATE
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ワーク診療行為展開処理
      *****************************************************************
       100-WKSRYACT-TENKAI-SEC          SECTION.
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *    院内院外区分初期化
           MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
      *
      *    ワーク診療行為検索
           PERFORM 1001-WKSRYACT-INIT-SEC
      *
           IF      LNK-WKSRYACT-MAX    >   ZERO
               PERFORM 1002-SAIHEN-SYORI-SEC
           END-IF
      *
           .
       100-WKSRYACT-TENKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       1001-WKSRYACT-INIT-SEC          SECTION.
      *
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    MOVE    ORCSCWKSRY-DAY      TO  FILE-DAY
      *
      *    OPEN    INPUT               WKSRY-FILE
      *    IF      STS-WKSRY        NOT =  ZERO
      *******  DISPLAY "ORCSCWKSRY STS-WKSRY OPEN:"  STS-WKSRY  "]"
      *        MOVE    1                   TO  FLG-WKSRY
      *    ELSE
      *        MOVE    ZERO                TO  FLG-WKSRY
      *    END-IF
      *    IF      FLG-WKSRY           =   ZERO
      *        PERFORM 980-WKSRY-READ-SEC
      *    END-IF
      *
      *ver.4.8
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  FLG-WKSRY
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           STRING  "KT01WKSRY"
                   ORCSCWKSRY-DAY      DELIMITED  BY  SIZE
                                   INTO    PARA-FILEMEI
           END-STRING
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                 OR   (SPA-ERRCD       NOT =   SPACE )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "KY01 LNK-WKSRYACT-MAX 400 OVR "
                           LNK-WKSRYACT-MAX
                   MOVE    "9000"              TO  SPA-ERRCD
                   MOVE    1                   TO  FLG-WKSRY
               ELSE
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
                   MOVE    "WKSRYACT"          TO  LNK-WKSRY-TERMID
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
      ******** PERFORM 980-WKSRY-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      **** CLOSE                       WKSRY-FILE
      *
           IF    (LNK-WKSRYACT-MAX     =   ZERO )
      *        対象データがない時、データ削除
      *???     IF      (FLG-WKSRY        NOT =   2    )
      *            MOVE    ORCSCWKSRY-DAY      TO  WRK-DAY
      *            PERFORM 210-PARA-DEL-SEC
      *????    END-IF
               IF      ORCSCWKSRY-DAY      >   ZERO
                   MOVE    ZERO                TO  SPA-NAI-RRKDAY
                                                  (ORCSCWKSRY-DAY)
               END-IF
           END-IF
           .
       1001-WKSRYACT-INIT-EXT.
           EXIT.
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       1002-SAIHEN-SYORI-SEC          SECTION.
      *
           MOVE    ZERO               TO  FLG-SET
           MOVE    SPACE              TO  WRK-ERRCD1
           MOVE    SPACE              TO  WRK-ERRCD2
           MOVE    ORCSCWKSRY-STRIDX  TO  IDX2
      *
           MOVE    ZERO               TO  FLG-SYORIFLG
           MOVE    ZERO               TO  FLG-ZENKAI
      *
           MOVE    ZERO               TO  FLG-AUTO-ARI
           MOVE    ZERO               TO  FLG-AUTO-NASI
      *    ワーク診療行為からの時は、訂正と同様に編集する
           IF      LNK-WKSRY-TERMID (1)    =   "WKSRYACT"
               MOVE    1               TO  FLG-SYORIFLG
           END-IF
      *    ワーク診療行為からの時、剤毎に剤点数を編集し直す
           IF      LNK-WKSRY-TERMID (1)    =   "WKSRYACT"
               PERFORM 10021-WKSRY-ZAITENHEN-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-INGAIKBN
           MOVE    ZERO                TO  FLG-INGAIKBN-ARI
           MOVE    ZERO                TO  IDX-HZN
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX   >   LNK-WKSRYACT-MAX )  OR
                                (IDX2  >   SPA-MAX-LINE     )
      *
               MOVE    1               TO  IDX1
      *        約束セットの内容は対象外
               IF       FLG-SET            =   1
                   MOVE    6               TO  IDX1
               END-IF
      *
               MOVE    ZERO                TO  IDXS
               PERFORM     VARYING   IDY   FROM    IDX1   BY  1
                           UNTIL     IDY   >   5
                   MOVE    ZERO                TO  FLG-AUTO-OK
                   MOVE    ZERO                TO  FLG-HEN-CHK
      *
      *            診療コードのないものは対象外
                   IF      LNK-WKSRY-SRYCD  (IDX IDY) =   SPACE
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
      *            登録時自動発生分は編集しない
                   IF      LNK-WKSRY-AUTOKBN(IDX IDY)    =   "3"
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
                   IF     (FLG-SYORIFLG                  =   1  ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =  "3"
                                                         OR "6" )
      *                訂正自動作成で算定回数を超えている時対象とする
                       PERFORM 10023-AUTOCHK-SEC
                       IF      FLG-AUTO-OK         =   1
                           MOVE    ZERO            TO  FLG-HEN-CHK
                       END-IF
                   END-IF
      *            登録時自動発生分は編集しない
                   IF     (ORCSCWKSRY-SYORIFLG         =   ZERO ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "4"
                                                       OR "6" )
      *                訂正時の自動発生しない
                       IF     (SPA-TEISEI-FLG            =  "1")
                         AND  (FLG-AUTO-OK               =   ZERO)
                           MOVE    1               TO  FLG-HEN-CHK
                       ELSE
                           MOVE    ZERO            TO  FLG-HEN-CHK
                           MOVE    1               TO  FLG-AUTO-OK
                       END-IF
                   END-IF
      *            特定疾患処方管理加算編集判定
                   IF     (ORCSCWKSRY-SYORIFLG         =   ZERO ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =  "5")
      *                訂正時の自動発生しない
                       IF     (SPA-TEISEITOKU-FLG        =  "1")
                         AND  (FLG-AUTO-OK               =   ZERO)
                           MOVE    1               TO  FLG-HEN-CHK
                       ELSE
                           MOVE    ZERO            TO  FLG-HEN-CHK
                           MOVE    1               TO  FLG-AUTO-OK
                       END-IF
                   END-IF
      *            自動発生の有無判定
                   IF      ORCSCWKSRY-SYORIFLG     =   ZERO
                       IF     (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "4"
                                                           OR "5"
                                                           OR "6" )
                           MOVE    1               TO  FLG-AUTO-ARI
                       END-IF
                       IF     (LNK-WKSRY-AUTOKBN(IDX IDY)  =  "3" ) AND
                              (LNK-WKSRY-SRYKBN (IDX)      =  "13"
                                                           OR "27"
                                                           OR "60"
                                                           OR "70")
                           MOVE    1               TO  FLG-AUTO-NASI
                       END-IF
                   END-IF
      *
                   IF      FLG-HEN-CHK         =   ZERO
                       PERFORM 10020-SPA-HEN-SEC
                   END-IF
      *
      *            入力回数（入院のみ）
                   IF      LNK-WKSRY-INPUTCD(IDX IDY)(1:1) =   "*"
                       ADD     1               TO  IDX2
                       IF      IDX2            >   SPA-MAX-LINE
                           MOVE    "9100"          TO  WRK-ERRCD1
                       ELSE
                           MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
                           MOVE    LNK-WKSRY-SRYKBN (IDX)
                                               TO  SPA-COM-SRYKBN(IDX2)
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                               TO  SPA-COM-KAISU(IDX2)
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                               TO  SPA-COM-KAISU2(IDX2)
                           MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
                                               TO  SPA-GMN-INPUTCD(IDX2)
                           MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                       END-IF
                   END-IF
      *
      *            約束セットの時、以下を編集しない
                   IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
                       MOVE    5               TO  IDY
                       MOVE    1               TO  FLG-SET
                   END-IF
               END-PERFORM
      *
      *        剤の最後に回数入力をセット
               IF     (IDXS                >   ZERO  )  OR
                      (FLG-SET             =   1     ) 
                   IF     (IDX                 =   LNK-WKSRYACT-MAX ) OR
                          ((IDX                <   SPA-MAX-LINE  ) AND
                           (LNK-WKSRY-ZAINUM (IDX + 1)
                                           NOT =   LNK-WKSRY-ZAINUM
                                                                (IDX)))
                       IF      IDX2            <=  SPA-MAX-LINE
                           MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                               (IDX2)
                       END-IF
                       MOVE    ZERO                TO  FLG-SET
      *                院内・院外処方を点数より変更
                       IF      FLG-INGAIKBN-ARI    =   1
      *                院外
                           IF      FLG-INGAIKBN        =   1
                               MOVE    "1"             TO  SPA-INGAIKBN
                           END-IF
      *                    院内（入院は院内）
                          IF      FLG-INGAIKBN        =   2
                               MOVE    "0"             TO  SPA-INGAIKBN
                           END-IF
                       END-IF
      *
                       MOVE    ZERO                TO  FLG-INGAIKBN
                       MOVE    ZERO                TO  FLG-INGAIKBN-ARI
                   END-IF
               END-IF
           END-PERFORM
      *
      *    訂正時の算定有無登録の判定
           IF      FLG-AUTO-ARI        =   ZERO
               IF      FLG-AUTO-NASI       =   1
                   MOVE    "1"             TO  SPA-TEISEI-FLG
               END-IF
           END-IF
      *
           IF      IDX2                >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  IDX2
           END-IF
           MOVE    ZERO                TO  IDX-TEI
           COMPUTE IDX-STR             =   ORCSCWKSRY-STRIDX  +  1
      *    約束コード以外の画面再編集
           PERFORM VARYING     IDX-1 FROM    IDX-STR BY  1
                   UNTIL      (IDX-1     >   SPA-MAX-LINE )  OR
                             ((SPA-GMN-INPUTCD(IDX-1) =   SPACE )
                          AND (SPA-COM-INPUTCD(IDX-1) =   SPACE ))
               IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "S" OR ".") OR
                      (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"       ) OR
                      (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "*"       ) OR
                      (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "#"
                                                     OR  "$"       )
                   CONTINUE
               ELSE
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    IDX-1               TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX-1)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      **********   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
                   IF     (SPA-ERRCD           NOT =   SPACE )  OR
                          (LNK-SC92-ERRAREA    NOT =   SPACE )
                       PERFORM 10024-ERRCD-SET-SEC
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-1)
                   END-IF
      *            残量廃棄の判定
                   IF     (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "3" )  AND
                          (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" )
                       PERFORM 10025-HAIKI-CHKI-SEC
                   END-IF
      *            注射以下
                   IF     (SPA-HAIKIKBN-FLG    =   "2"   )   AND
                          (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" ) AND
                          (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "4"
                                                        OR  "5"
                                                        OR  "6"
                                                        OR  "7"
                                                        OR  "8" )
                       PERFORM 10025-HAIKI-CHKI-SEC
                   END-IF
      *
      *H29.7
      *            精神通院の２０歳加算の自動発生なし
                   IF     (SPA-SRYYMD              >=  "20140401")
                      AND (ORCSCWKSRY-SYORIFLG     =   ZERO      )
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "I"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "002")
                       IF      (SPA-COM-AUTOKBN(IDX-1) =  "2"   )
                           MOVE    SPACE          TO  SPA-COM-AUTOKBN
                                                             (IDX-1)
                       END-IF
                       IF      (SPA-COM-DATAKBN(IDX-1) =  "1"   )
                           MOVE    "1"            TO  SPA-COM-KSNAUTO
                                                             (IDX-1)
                       END-IF
                   END-IF
      *H29.7
      *            心身療法の２０歳加算の自動発生なし(H29/7から）
                   IF     (SPA-SRYYMD              >=  "20170701")
                      AND (ORCSCWKSRY-SYORIFLG      =   ZERO    )
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "I"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "004")
                       IF      (SPA-COM-AUTOKBN(IDX-1) =  "2"   )
                           AND (SPA-COM-DATAKBN(IDX-1) =  "2"   )
                           MOVE    SPACE          TO  SPA-COM-AUTOKBN
                                                             (IDX-1)
                       END-IF
                       IF      (SPA-COM-DATAKBN(IDX-1) =  "1"   )
                           MOVE    "1"            TO  SPA-COM-KSNAUTO
                                                             (IDX-1)
                       END-IF
                   END-IF
      *            在宅加算２の訂正時自動発生対応
                   IF     (SPA-SRYYMD              >=  "20160401")
                      AND (SPA-COM-CDKBN-KBN (IDX-1)   =   "C"  )
                      AND (SPA-COM-CDKBN-KBNNUM(IDX-1) =   "002")
                      AND (ORCSCWKSRY-SYORIFLG     =   ZERO      )
                       PERFORM 10026-ZAITAKU-AUTO-SEC
                   END-IF
      *H29.7
      *
      *            回数の入力（外来のみ）
                   IF     (SPA-NYUGAIKBN       =   "2" )  AND
                          (SPA-COM-KAIFLG (IDX-1)  =   "1" )
                       IF     (SPA-COM-KAISU(IDX-1)    >   1  )  OR
                              (SPA-COM-SRYKBN(IDX-1)(1:1)
                                                   =   "2"    )  OR
                              (IDX-1               =   SPA-MAX-LINE)
                           CONTINUE
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX-1 + 1)
                                                   =   SPACE   )  OR
                                  (SPA-GMN-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "."     )
                               MOVE    SPACE       TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                           ELSE
                               IF    ((SPA-COM-SRYKBN (IDX-1 + 1)
                                               NOT =  SPA-COM-SRYKBN
                                                       (IDX-1) )  AND
                                      (SPA-COM-SRYKBN (IDX-1)
                                                   >   "13"    )  AND
                                      (SPA-COM-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "6"     ))
      *                         コメントの時
                                OR   ((SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =  SPA-COM-SRYKBN
                                                       (IDX-1) )  AND
                                      (SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =   "99"
                                                   OR  "98"   ) AND
                                      (SPA-COM-SRYSYUKBN (IDX-1 + 1)
                                                   =   SPACE   ))
      *                         コメントの時
                                OR   ((SPA-COM-SRYKBN (IDX-1)
                                                   =   "95"
                                                   OR  "96"    )  AND
                                      (SPA-COM-SRYKBN (IDX-1 + 1)
                                                   =   "99"
                                                   OR  "98"   ) AND
                                      (SPA-COM-SRYSYUKBN (IDX-1 + 1)
                                                   =   SPACE   ))
                                   CONTINUE
                               ELSE
                                   MOVE    SPACE   TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
      *            入力コードの画面用再編集
                   MOVE    IDX-1               TO  IDX
                   PERFORM 10026-INPUTCD-HEN-SEC
                   IF     (SPA-COM-CUR (IDX-1) =   SPACE )  AND
                          (SPA-COM-CHKFLG (IDX-1)  =   "1"   )
      *                コメントの全角チェックをする
                     AND  (SPA-COM-MEIFLG (IDX-1)  =   SPACE )
                       MOVE    SPA-GMN-INPUTCD(IDX-1)
                                          TO  SPA-MAE-INPUTCD (IDX-1)
                   ELSE
                       MOVE    SPACE      TO  SPA-MAE-INPUTCD (IDX-1)
                       MOVE    SPACE      TO  SPA-COM-CHKFLG (IDX-1)
                   END-IF
               END-IF
      *        回数の画面用再編集
               PERFORM 10028-GAIRAI-KAISU-SEC
      *
      *        訂正の展開時はすべて自動発生済みとする
               IF      ORCSCWKSRY-SYORIFLG =   ZERO
                   MOVE    IDX-1           TO  IDX
                   MOVE    "1"             TO  SPA-COM-AUTOFLG (IDX)
      *            検査の逓減がある場合、フラグを変更する
                   IF     (SPA-COM-SRYKBN (IDX)    =   "60" )  AND
                          (SPA-COM-INPUTCD(IDX)(1:1)   =   "1")
                       IF      (SPA-COM-DATAKBN (IDX)  =   "1")  AND
                               (SPA-COM-TEIGENKBN(IDX) =   1  )
                           MOVE    IDX                 TO  IDX-TEI
                       END-IF
                       IF      (SPA-COM-DATAKBN (IDX)  =   "2")  AND
                               (IDX-TEI                >   ZERO) AND
                               (SPA-COM-AUTOKBN (IDX)  =   "2")  AND
                               (SPA-COM-TENSIKIBETU(IDX)   =  6 ) AND
                               (SPA-COM-KIJUNTEIGENKBN(IDX)   =  ZERO)
      *                    自動発生で％減算、施設基準逓減区分がないもの
                           MOVE    1           TO  SPA-COM-TEIGEN-FLG
                                                          (IDX-TEI)
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO                TO  IDX-TEI
               END-IF
      *        最終行
               MOVE    IDX-1               TO  IDX2
           END-PERFORM
      *
           MOVE    ZERO              TO  FLG-FIRUMU
           PERFORM VARYING     IDX-1 FROM    IDX2   BY  -1
                   UNTIL       IDX-1     <   1
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO            TO  FLG-FIRUMU
               END-IF
      *        画像診断のフィルム判定
               IF      SPA-COM-SRYKBN (IDX-1)  =   "70"
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "7" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "3" )
                       MOVE    1               TO  FLG-FIRUMU
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "1" )  AND
                          (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       IF      FLG-FIRUMU                =   ZERO
                           MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                       ELSE
                           IF      SPA-COM-SURFLG (IDX-1)
                                                       NOT =   "1"
                               MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                        入力コードの画面用再編集
                               MOVE    IDX-1               TO  IDX
                               PERFORM 10026-INPUTCD-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    約束コードの画面用再編集
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM VARYING     IDX-1   FROM    IDX-STR   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )  OR
                             ((SPA-COM-INPUTCD(IDX-1)    =   SPACE) AND
                              (SPA-GMN-INPUTCD(IDX-1)    =   SPACE))
               IF      SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"
                   MOVE    IDX-1               TO  IDX
                   MOVE    SPA-COM-INPUTCD (IDX-1)
                                               TO  SPA-GMN-INPUTCD
                                                              (IDX-1)
      *            入力コードの画面用再編集
                   PERFORM 10026-INPUTCD-HEN-SEC
      *?           MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
                   MOVE    IDX-1               TO  SPA-GMN-IDX
      *
                   MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                             (IDX-1)
                   MOVE    SPACE               TO  SPA-COM-KAIFLG
                                                             (IDX-1)
      *
                   INITIALIZE                  ORCSC97AREA
                   MOVE    "K02"               TO  LNK-SC97-PG
                   MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  LNK-SC97-INPUTCD
                   CALL    "ORCSC97"           USING
                                               SPA-AREA
                                               ORCSC97AREA
                                               SPA-SCR-REC
      *
                   PERFORM 10028-INPUTSET-SYORI-SEC
               END-IF
           END-PERFORM
      *    最大行数
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )
                          OR ((SPA-COM-INPUTCD(IDX-1)    =   SPACE)
                          AND (SPA-GMN-INPUTCD(IDX-1)    =   SPACE))
               MOVE    IDX-1               TO  SPA-GMN-MAX
           END-PERFORM
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  ORCSCWKSRY-ERRCD
           ELSE
               MOVE    WRK-ERRCD           TO  ORCSCWKSRY-ERRCD
           END-IF
           MOVE    WRK-ERRCD2          TO  ORCSCWKSRY-ERRCD2
      *
           .
       1002-SAIHEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーコード編集
      *****************************************************************
       10024-ERRCD-SET-SEC              SECTION.
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
               END-IF
           END-PERFORM
           MOVE    SPA-ERRCD           TO  WRK-ERRCD2
           .
       10024-ERRCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の判定処理
      *****************************************************************
       10025-HAIKI-CHKI-SEC              SECTION.
      *
           IF      (SPA-COM-YKZKBN (IDX-1)  =   "4"  )  AND
                   (SPA-COM-TANICD (IDX-1)  =   "014"
                                            OR  "022"
                                            OR  "046" )  AND
                   (SPA-COM-SURYO(IDX-1)(6:3)  >   ZERO)
           AND     (SPA-COM-YAKUHYO(IDX-1)  =   SPACE )
               IF     (IDX-1               <   SPA-MAX-LINE  )  AND
                      (SPA-COM-INPUTCD(IDX-1 + 1)  =   "099309901")
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  SPA-COM-AUTOFLG2(IDX-1)
               END-IF
           END-IF
      *
           .
       10025-HAIKI-CHKI-EXT.
           EXIT.
      *
      *!
      *H29.7
      *****************************************************************
      *    （在支診等以外）の加算処理
      *****************************************************************
       10026-ZAITAKU-AUTO-SEC              SECTION.
      *
      *    在宅時医学総合管理料等判定（在医総管・特医総管）
           SET     TBL-ZSIDX           TO  1
           SEARCH  TBL-ZISOSIN-OCC     VARYING   TBL-ZSIDX
              AT   END
                   MOVE    SPACE                   TO  WRK-ZS-KBN
                   MOVE    SPACE                   TO  WRK-ZS-KBN2
                   MOVE    SPACE                   TO  WRK-ZS-GRP
               WHEN   (TBL-ZS-SRYCD (TBL-ZSIDX)
                                           =   SPA-COM-INPUTCD (IDX-1)) 
                   MOVE    TBL-ZS-GRP (TBL-ZSIDX)  TO  WRK-ZS-GRP
                   MOVE    TBL-ZS-KBN (TBL-ZSIDX)  TO  WRK-ZS-KBN
                   MOVE    TBL-ZS-KBN2(TBL-ZSIDX)  TO  WRK-ZS-KBN2
           END-SEARCH
      *
           IF     (WRK-ZS-GRP(1:1)        =   "0"
                                          OR  "1" )
      *
               MOVE    ZERO              TO  FLG-CHKEND
               MOVE    ZERO              TO  FLG-CHKAUTO
               PERFORM VARYING     IDX   FROM    IDX-1   BY  1
                       UNTIL      (IDX   >   SPA-MAX-LINE )
                           OR   ((SPA-COM-INPUTCD(IDX) =   SPACE) AND
                                 (SPA-GMN-INPUTCD(IDX) =   SPACE))
                           OR   ( FLG-CHKEND           =    1   )
                   IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"  )
                      AND (SPA-COM-AUTOKBN(IDX)        =   "2"  )
      *                在宅実績加算のチェック
                       PERFORM 330811-ZISOKSN-CHK-SEC
      *                在宅実績加算が自動発生分
                       IF     (WRK-ZKS-GRP(1:1)    =   "0"
                                                   OR  "1"    )
                           MOVE    1               TO  FLG-CHKAUTO
                       END-IF
                   END-IF
                   IF      SPA-COM-ZAIEND (IDX)     =   "1"
                       MOVE    1                    TO  FLG-CHKEND
                   END-IF
               END-PERFORM
               IF      FLG-CHKAUTO        =   1
                   CONTINUE
               ELSE
      *            在宅実績加算が自動発生でない時自動発生なしとする
                   MOVE    "1"             TO  SPA-COM-KSNAUTO(IDX-1)
               END-IF
           END-IF
           .
       10026-ZAITAKU-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅実績加算処理
      *****************************************************************
       330811-ZISOKSN-CHK-SEC         SECTION.
      *
           SET     TBL-ZKSIDX          TO  1
           SEARCH  TBL-ZISOKSN-OCC     VARYING   TBL-ZKSIDX
              AT   END
                   MOVE    SPACE                   TO  WRK-ZKS-KBN
                   MOVE    SPACE                   TO  WRK-ZKS-GRP
              WHEN   (TBL-ZKS-SRYCD (TBL-ZKSIDX)
                                       =   SPA-COM-INPUTCD
                                                        (IDX))
                   MOVE    TBL-ZKS-GRP (TBL-ZKSIDX) TO  WRK-ZKS-GRP
                   MOVE    TBL-ZKS-KBN (TBL-ZKSIDX) TO  WRK-ZKS-KBN
           END-SEARCH
           .
       330811-ZISOKSN-CHK-EXT.
           EXIT.
      *!
      *
      *****************************************************************
      *    剤毎に剤点数を編集処理
      *****************************************************************
       10021-WKSRY-ZAITENHEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI
           PERFORM VARYING   IDX       FROM    LNK-WKSRYACT-MAX  BY  -1
                   UNTIL     IDX       <       1
               IF    ( IDX             =   LNK-WKSRYACT-MAX  )  OR
                     ((IDX             <   SPA-MAX-LINE  )    AND
                      (LNK-WKSRY-ZAINUM (IDX + 1)
                                       NOT =   LNK-WKSRY-ZAINUM(IDX)))
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                               TO  WRK-TENSUKEI
               END-IF
               MOVE    WRK-TENSUKEI        TO  LNK-WKSRY-ZAITENKEI(IDX)
           END-PERFORM
           .
       10021-WKSRY-ZAITENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       10020-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDXS                    =   ZERO)  AND
                  (LNK-WKSRY-RENNUM (IDX)  =   1   )
               IF      IDX2                <   SPA-MAX-LINE
                   IF      SPA-NYUGAIKBN       =   "2"
                       PERFORM 310112-SRYSYUKBN-GAI-SEC
                   ELSE
                       PERFORM 310111-SRYSYUKBN-NYU-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    投薬の判定
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 310113-INGAI-CHK-SEC
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           IF      IDX2                >   SPA-MAX-LINE
                MOVE    "9100"         TO  WRK-ERRCD1
                GO      TO      10020-SPA-HEN-EXT
           END-IF
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-COM-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-GMN-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  SPA-COM-SRYSYUKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  SPA-COM-INPUTCD (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO   (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO2  (IDX2)
           MOVE    LNK-WKSRY-AUTOKBN (IDX IDY)
                                       TO  SPA-COM-AUTOKBN  (IDX2)
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"  OR  "H"
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *!!
      *    器材商品名対応
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "T"
               MOVE    "1"             TO  SPA-COM-AUTOKBN2(IDX2)
               MOVE    SPACE           TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    訂正時登録時の区分
           IF      ORCSCWKSRY-SYORIFLG =   ZERO
               MOVE    SPA-COM-AUTOKBN (IDX2)
                                       TO  SPA-COM-MAE-AUTOKBN  (IDX2)
           END-IF
      *
           IF      FLG-AUTO-OK         =   1
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (IDX) =   "H"
               MOVE    1               TO  SPA-COM-HOUKATU-FLG(IDX2)
               MOVE    1               TO  SPA-COM-HOUKATU-ZAI(IDX2)
      *        包括診療行為チェックをオンにする
               IF      SPA-HKTSANTEI-FLG   =   ZERO
                   MOVE   1                TO  SPA-HKTSANTEI-FLG
               END-IF
           END-IF
      *@
      *
           MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDY)
                                       TO  SPA-COM-INPUTCHI-G  (IDX2)
      *
      *    回数を編集
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU(IDX2)
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU2(IDX2)
           MOVE    IDX2                TO  IDXS
      *    分画数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-BUNGSU (IDX2)
           END-IF
      *!!
      *    内服１種類区分
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-INPUTKBN(IDX IDY) =   "2"  )
                MOVE    "1"                TO  SPA-COM-INPUTNAIF (IDX2)
                MOVE    "1"                TO  SPA-COM-TANDKKBN  (IDX2)
           END-IF
      *    コメントの継続
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "8"  )  OR
                  (SPA-COM-INPUTCD (IDX2)(1:3) =   "008")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDY)    =   "1" )
                   MOVE    "1"             TO  SPA-COM-INPUTCONT (IDX2)
               END-IF
           END-IF
      *!!
      *!!
      *    外用の回数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-KAISU2(IDX2)
               IF      SPA-COM-KAISU2(IDX2)    =   ZERO
                   MOVE    1               TO  SPA-COM-KAISU2(IDX2)
               END-IF
      *        数量の再計算
               IF     (SPA-COM-KAISU2  (IDX2)  >   1    )  AND
                     ((SPA-COM-INPUTCD (IDX2)(1:1) =   "6"
                                                   OR  "7" ) OR
                      (SPA-COM-INPUTCD (IDX2)(1:3) =   "059") OR
                      (SPA-COM-INPUTCD (IDX2)(1:1) =   "S"  ))
                   COMPUTE SPA-COM-SURYO2  (IDX2)
                                       =   SPA-COM-SURYO2  (IDX2)
                                       /   SPA-COM-KAISU2(IDX2)
               END-IF
               IF      LNK-WKSRY-ZAIKAIKEI(IDX) >   1
                   COMPUTE SPA-COM-KAISU2  (IDX2)
                                       =   SPA-COM-KAISU2  (IDX2)
                                       *   LNK-WKSRY-ZAIKAIKEI
                                                           (IDX)
               END-IF
               MOVE    SPA-COM-KAISU2  (IDX2)
                                       TO  SPA-COM-KAISU (IDX2)
               MOVE    SPA-COM-SURYO2  (IDX2)
                                       TO  SPA-COM-SURYO (IDX2)
           END-IF
      *ver.4.5.0
      *    換算値入力
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-KANSURYO (IDX IDY) >   ZERO )
      *        点数マスタの換算値×換算値数量＝数量の時のみ設定
               MOVE    SPA-COM-INPUTCD (IDX2)  TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 9101-TENSUPLUS-SYORI-SEC
                   COMPUTE WRK-KANSURYO    =   TNSP-KANZANCHI
                                           *   LNK-WKSRY-KANSURYO
                                                          (IDX IDY)
                   IF      WRK-KANSURYO    =   LNK-WKSRY-SRYSURYO
                                                          (IDX IDY)
                        MOVE    LNK-WKSRY-KANSURYO (IDX IDY)
                                           TO  SPA-COM-KANSURYO (IDX2)
      *                外用薬は÷回数
                       IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
                           COMPUTE SPA-COM-KANSURYO (IDX2)
                                               =   LNK-WKSRY-KANSURYO
                                                          (IDX IDY)
                                               /   SPA-COM-KAISU2(IDX2)
                       END-IF
                   END-IF
      *           外来投薬で、１種類設定可能かの判定
                   IF      (SPA-COM-SRYKBN (IDX2)  =   "21"  )
                   AND     (SPA-NYUGAIKBN          =   "2"   )
                   AND     (SPA-COM-KANSURYO(IDX2) NOT =   ZERO  )
      *                ｇ、�咫，髻（顱　■蹌譟，髻）�
                       IF    ((TNS-TANICD          =   "032"
                                                   OR  "033")  AND 
                              (TNSP-KANZANTANICD   =  "018"))
                       OR    ((TNS-TANICD          =   "036") AND
                              (TNSP-KANZANTANICD   =   "007"))
                           MOVE    "1"             TO  SPA-COM-TANDKKBN
                                                                (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *!!
      *    薬評・減点
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   1
               MOVE    "1"                 TO  SPA-COM-YAKUHYO (IDX2)
           END-IF
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   2
               MOVE    "1"                 TO  SPA-COM-GENTEN  (IDX2)
           END-IF
      *!!
           IF      LNK-WKSRY-INPUTCD (IDX IDY) =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX2)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           ELSE
               MOVE    LNK-WKSRY-INPUTCD (IDX IDY)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           END-IF
           MOVE    SPA-GMN-INPUTCD (IDX2)  TO  SPA-MAE-INPUTCD (IDX2)
      *
      *    剤点数の設定（訂正前）
           MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                       TO  SPA-COM-MAE-TENSU (IDX2)
      *
      *    約束セットの時、以下処理なし
           IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
               GO      TO      10020-SPA-HEN-EXT
           END-IF
      *
      *    時間加算
           IF      (SPA-GMN-INPUTCD (IDX2)(1:1)  IS  NUMERIC) AND
                   (SPA-GMN-INPUTCD (IDX2)(2:1)  =   SPACE  ) AND
                   (SPA-GMN-INPUTCD (IDX2)(3:1)  NOT =   SPACE)
               MOVE    SPA-GMN-INPUTCD (IDX2)(1:1)
                                           TO  SPA-COM-TIMEFLG(IDX2)
      *        入院で訂正の時、設定なし
               IF     (SPA-NYUGAIKBN       =   "1"   )  AND
                      (ORCSCWKSRY-SYORIFLG =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    SPA-COM-TIMEFLG (IDX2)
                                           TO  SPA-NAI-TIMEFLG
               END-IF
           END-IF
      *
      *    コメントの時、コメント内容編集
           IF      SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8"
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDY)  NOT =   SPACE
                   IF      SPA-COM-INPUTCD (IDX2)       =   "810000001"
                                                        OR  "008500000"
                                                        OR  "008600000"
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO-G (IDX2)
                       MOVE    "1"         TO  SPA-COM-MEIFLG   (IDX2)
                   ELSE
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO   (IDX2)
                       IF     (SPA-COM-INPUTCD (IDX2)(1:2)  =   "83" )
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0083")
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0085")
                         OR   (SPA-COM-INPUTCD (IDX2)(1:4)  =   "0086")
                         OR   (SPA-COM-INPUTCD (IDX2)(1:2)  =   "85"  )
                           MOVE    "1"     TO  SPA-COM-MEIFLG   (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    金額入力時、金額へ
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
      *        自賠責の時、対象外
               IF     (SPA-HKNKBN          =   1    )  AND
      *************** (SPA-RSI-HKNKBN      =   "4"  )  AND
                      (SPA-COM-SRYKBN (IDX2)
                                           =   "80" )  AND
                      (SPA-COM-INPUTCD(IDX2)(1:5)
                                           =   "09591"  OR
                                               "09592"  OR
                                               "09593"  OR
                                               "09594"  )
                   MOVE    ZERO                TO  FLG-OK2
               ELSE
                   MOVE    1                   TO  FLG-OK2
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-OK2
           END-IF
           IF      FLG-OK2             =   1
               MOVE    SPA-COM-INPUTCD (IDX2)  TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      TNS-TEN                 =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
               END-IF
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
               MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
      *        剤点数の設定（訂正前）
               MOVE    SPA-COM-JIHIMONEY(IDX2)
                                           TO  SPA-COM-MAE-TENSU (IDX2)
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
      *            剤点数の設定（訂正前）
                   MOVE    SPA-COM-JIHIMONEY(IDX2)
                                           TO  SPA-COM-MAE-TENSU (IDX2)
               ELSE
                   MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
               END-IF
           END-IF
      *
           .
       10020-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別設定（外来）処理
      *****************************************************************
       310112-SRYSYUKBN-GAI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUBETU
      *    診察料以外
           IF    ((LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE )  AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)(1:2)
                                           NOT =   "11" AND
                                                   "12" AND
                                                   "13"  ))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *    診察料で前の剤が自費の時
           IF     (LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                       =   "11" OR
                                           "12" OR
                                           "13"  )   AND
                 ((IDX2                >   ZERO  ) AND
                  (SPA-COM-SRYKBN (IDX2)
                                       =   "96"  OR  "95"))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *
      *    診察料で前の剤が731の時
           IF     (LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                       =   "11" OR
                                           "12" OR
                                           "13"  )   AND
                 ((IDX2                >   ZERO  ) AND
                  (SPA-COM-SRYSYUKBN (IDX2)
                                       =   "731" ))
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *H28.1
      *    初再診加算
           IF     LNK-WKSRY-SRYSYUKBN(IDX)     =   "114"
                                               OR  "124"
                                               OR  "133"
               MOVE    1               TO  FLG-SYUBETU
           END-IF
      *
      *    訂正の時、再診料算定科を再診とする
           IF     (LNK-WKSRY-SRYKBN    (IDX)       =   "12" AND
                   LNK-WKSRY-SRYCD (IDX IDY)       =   "830000021")
               OR (LNK-WKSRY-SRYKBN    (IDX)       =   "11" AND
                   LNK-WKSRY-SRYCD (IDX IDY)       =   "830000020")
      *             訂正の時、ダミー診察の時
              OR ((LNK-WKSRY-SRYKBN    (IDX)       =   "11" OR
                                                       "12"   ) AND
                  (LNK-WKSRY-SRYCD (IDX IDY)       =   "820000003"
                                                   OR  "820000004"
                                                   OR  "820000005"
                                                   OR  "820000006"))
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
           IF      LNK-WKSRY-SRYKBN    (IDX)       =   "13"
      *        小児科外来診察料の加算のみ
               PERFORM 310119-SYONIKA-CHK-SEC
               IF      WRK-SYONIKA-KBN     =   "4"
                   MOVE    1                   TO  FLG-SYUBETU
               END-IF
           END-IF
      *    最初が手技料以外の時
           IF     (LNK-WKSRY-SRYKBN(IDX)   =   "11"
                                           OR  "12"
                                           OR  "13"  ) AND
                  (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                       NOT =   "1" )
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
      *
      *    薬評・減点分の時
           IF     (LNK-WKSRY-ZAIKBN(IDX)   NOT =   ZERO  )
               MOVE    1                   TO  FLG-SYUBETU
           END-IF
      *
      *    自動発生の展開分
           IF      FLG-SYUBETU         =   1
               IF     (FLG-AUTO-OK         =   1 ) AND
                      (LNK-WKSRY-SRYKBN (IDX)  NOT =   "70" )
      *            前の剤が自費以外
                   IF      (IDX2                >   ZERO  ) AND
                           (SPA-COM-SRYKBN (IDX2)
                                               =   "96"  OR  "95")
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  FLG-SYUBETU
                   END-IF
               END-IF
           END-IF
      *
      *    画像診断で部位から始まる場合
           IF     (LNK-WKSRY-SRYKBN (IDX)      =   "70" )  AND
                  (LNK-WKSRY-SRYKBN (IDX - 1 ) =   "70" )  AND
                  (LNK-WKSRY-SRYCD (IDX IDY)(1:3)  =   "002")  AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE )
                MOVE    1               TO  FLG-SYUBETU
                MOVE    "700"           TO  LNK-WKSRY-SRYSYUKBN (IDX)
            END-IF
      *
      *    診療種別のない投薬で前が検査の時
           IF     (FLG-SYUBETU         =   ZERO ) AND
                  (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE ) AND
                  (LNK-WKSRY-SRYKBN (IDX)(1:1) =   "2"   ) AND
                  (LNK-WKSRY-SRYKBN (IDX - 1 ) =   "60"  ) 
                MOVE    1               TO  FLG-SYUBETU
                MOVE    LNK-WKSRY-SRYKBN (IDX)
                                        TO  LNK-WKSRY-SRYSYUKBN (IDX)
                                                           (1:2)
                MOVE    "0"             TO  LNK-WKSRY-SRYSYUKBN (IDX)
                                                           (3:1)
            END-IF
      *
            IF      FLG-SYUBETU         =   1
               ADD     1                   TO  IDX2
               MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-COM-SRYKBN(IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-GMN-SRYKBN(IDX2)
           END-IF
      *
           .
       310112-SRYSYUKBN-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *   投薬の判定処理
      *****************************************************************
       310113-INGAI-CHK-SEC                  SECTION.
      *
      *    訂正の時、診療種別が未入力で院外の薬剤があった時
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE OR
                                                   "210" OR
                                                   "220" OR
                                                   "230" OR
                                                   "290" )  AND
                  (LNK-WKSRY-SRYKBN    (IDX)   =   "21" OR "22"
                                               OR  "23" )
      *        薬評以外
             AND  (LNK-WKSRY-ZAIKBN (IDX)  NOT =   1     )
               IF     (FLG-SYORIFLG            =   1    )  OR
                      (LNK-WKSRY-TERMID (IDX)  =   "WKSRYACT")
                   MOVE    1                   TO  FLG-INGAIKBN-ARI
                   IF      LNK-WKSRY-ZAITENKEI (IDX)   =   ZERO
      *                剤点数＝ゼロ　院外
      *                剤内に薬剤がある時のみ
                       IF      LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                               =   "6"
                           MOVE    1               TO  FLG-INGAIKBN
                       END-IF
                   ELSE
      *                剤点数≠ゼロ　院内
                       MOVE    2               TO  FLG-INGAIKBN
                   END-IF
               END-IF
           END-IF
           .
       310113-INGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別設定（入院）処理
      *****************************************************************
       310111-SRYSYUKBN-NYU-SEC                  SECTION.
      *    診療種別セット
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE )  AND
                 ((LNK-WKSRY-SRYCD (IDX 01)(1:1)   NOT =   "1") OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)(1:1)  =   "9"  OR
                                                       "3"  OR
                                                       "2"   )  OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)(3:1)  NOT =   "0") OR
                  (LNK-WKSRY-SRYSYUKBN (IDX)       =   "610"  )
      *           減点分の時
             OR   (LNK-WKSRY-ZAIKBN  (IDX)     NOT =   ZERO   ))
               ADD     1                       TO  IDX2
               MOVE    "."                     TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                             (IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SPA-COM-SRYKBN(IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SPA-GMN-SRYKBN(IDX2)
           END-IF
      *
           .
       310111-SRYSYUKBN-NYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   小児科外来診察料科チェックク処理
      *****************************************************************
       310119-SYONIKA-CHK-SEC                  SECTION.
      *
      *    診察料検索
           SET     TBL-SYO             TO  1
           SEARCH  TBL-SYONIKA-OCC     VARYING   TBL-SYO
               AT  END
                   MOVE    SPACE           TO  WRK-SYONIKA-KBN
                WHEN   LNK-WKSRY-SRYCD (IDX IDY)
                                           =   TBL-SYONIKA-SRYCD
                                                      (TBL-SYO)
                   MOVE    TBL-SYONIKA-KBN (TBL-SYO)
                                           TO  WRK-SYONIKA-KBN
           END-SEARCH
           .
       310119-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   自動算定分編集処理
      *****************************************************************
       10023-AUTOCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTO-OK
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    LNK-WKSRY-SRYCD  (IDX IDY)
                                       TO  LNK-SC91-SRYCD
           MOVE    "4"                 TO  LNK-SC91-KBN
           MOVE    ZERO                TO  LNK-SC91-GMN-IDX
      ***  MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "K004"
              OR  (LNK-SC91-ERRCD2     =   "K004"  )
               MOVE    1               TO  FLG-AUTO-OK
               MOVE    SPACE           TO  LNK-SC91-ERRCD
           END-IF
      *
      *    薬剤情報で毎回回の算定
           IF     (SPA-YAKJYO-1        =   2 )  AND
                  (FLG-AUTO-OK         =   1 )  AND
                  (LNK-WKSRY-SRYCD  (IDX IDY)   =   "120002370")
             AND  (SPA-NAI-ROUJIN      =   0 )
               MOVE    ZERO            TO  FLG-AUTO-OK
           END-IF
           IF    ((SPA-YAKJYO-2        =   2 )  OR
                  (SPA-YAKJYO-3        =   2 )) AND
                  (FLG-AUTO-OK         =   1 )  AND
                  (LNK-WKSRY-SRYCD  (IDX IDY)   =   "113701410"
                                                OR  "113701310"
                                                OR  "120002370")
             AND  (SPA-NAI-ROUJIN      =   1 )
               MOVE    ZERO            TO  FLG-AUTO-OK
           END-IF
      *
           .
       10023-AUTOCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   自動算定分編集処理
      *****************************************************************
      *10022-ZENKAICHK-SEC                  SECTION.
      *
      *    算定履歴チェック処理
      *    INITIALIZE                  ORCSC91AREA
      *    MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
      *    MOVE    LNK-WKSRY-SRYCD  (IDX IDY)
      *                                TO  LNK-SC91-SRYCD
      *    MOVE    "2"                 TO  LNK-SC91-KBN
      *    MOVE    ZERO                TO  LNK-SC91-GMN-IDX
      *    MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
      *    CALL    "ORCSC91"           USING    MCPAREA
      *                                         ORCSC91AREA
      *                                         SPA-SCR-REC
      *                                         SPA-AREA
      *                                         TNS-TENSU-REC
      *    IF      LNK-SC91-ERRCD      =   "K004" 
      *        MOVE    SPACE           TO  LNK-SC91-ERRCD
      *    END-IF
      *    IF      LNK-SC91-ERRCD  NOT =   SPACE
      *        MOVE    1               TO  FLG-HEN-CHK
      *    END-IF
      *    併用算定チェック
      *    INITIALIZE                  ORCSC92AREA
      *    MOVE    "4"                 TO  LNK-SC92-KBN
      *    MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
      *    MOVE    "K02"               TO  LNK-SC92-PG
      *    COMPUTE LNK-SC92-GMN-IDX    =   IDX2    +   1
      *    MOVE    LNK-WKSRY-SRYCD(IDX IDY)
      *                                TO  LNK-SC92-SRYCD
      *    MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
      *    MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
      *    MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
      *    MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
      *    MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      *    MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *    CALL    "ORCSC92"           USING
      *                                MCPAREA
      *                                ORCSC92AREA
      *                                SPA-SCR-REC
      *                                SPA-AREA
      *    IF      LNK-SC92-ERRCD6 NOT =   SPACE
      *        MOVE    1               TO  FLG-HEN-CHK
      *    END-IF
      *
      *    処方料・処方せん料は対象外とする
      *    IF      LNK-WKSRY-SRYSYUKBN (IDX)  =   "250"  OR
      *                                           "820"
      *        EVALUATE    LNK-WKSRY-SRYCD(IDX IDY)
      *            長期投与加算（処方せん）
      *            WHEN    "120003270"
      *            特定疾患処方箋
      *            WHEN    "120002570"
      *            特定疾患処方料
      *            WHEN    "120002270"
      *            長期投与加算（処方）
      *            WHEN    "120003170"
      *                MOVE    1               TO  FLG-HEN-CHK
      *        END-EVALUATE
      *    END-IF
      *    .
      *10022-ZENKAICHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       10026-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    前へ編集
           MOVE    SPA-GMN-INPUTCD (IDX)   TO  SPA-MAE-INPUTCD (IDX)
      *
           .
       10026-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    回数の画面用再編集
      *****************************************************************
       10028-GAIRAI-KAISU-SEC            SECTION.
      *
      *    診療日を日に設定
           INITIALIZE                  SPA-COM-NYUIN-AREA (IDX-1)
      *    日付
      *
           MOVE    ORCSCWKSRY-DAY      TO  WRK-DAY
           IF      ORCSCWKSRY-DAY      =   ZERO
               MOVE    SPA-SRYYMD(7:2)     TO  WRK-DAY
           END-IF
      *
           MOVE    1                   TO  SPA-COM-NYUINDAY
                                           (IDX-1 WRK-DAY)
           ADD     1                   TO  SPA-COM-NYU-KAI
                                                       (IDX-1)
           .
       10028-GAIRAI-KAISU-EXT.
           EXIT.
      *****************************************************************
      *     入院回数の画面用再編集
      *****************************************************************
       10028-NYUIN-KAISU-SEC            SECTION.
      *
      *    入力コードの画面用再編集
           IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "*" )  AND
                  (SPA-GMN-INPUTCD(IDX-1)(2:)    =   SPACE)
               MOVE    IDX-1               TO  IDX
               PERFORM 10026-INPUTCD-HEN-SEC
               MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
           END-IF
      *    回数入力の画面用再編集
           IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)     =   "*"  ) AND
                  (SPA-GMN-INPUTCD(IDX-1)(2:)  NOT =   SPACE)
               MOVE    IDX-1               TO  SPA-GMN-IDX
               INITIALIZE                  ORCSC97AREA
               MOVE    "KT01"              TO  LNK-SC97-PG
               MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  LNK-SC97-INPUTCD
               CALL    "ORCSC97"           USING
                                           SPA-AREA
                                           ORCSC97AREA
                                           SPA-SCR-REC
      *
      *        変換文字列編集
               PERFORM 100281-KAISUU-CHK-SEC
      *
               IF     (SPA-COM-CUR (IDX-1) =   SPACE )  AND
                      (SPA-COM-CHKFLG (IDX-1)  =   "1"   )
      *            コメントの全角チェックをする
                 AND  (SPA-COM-MEIFLG (IDX-1)  =   SPACE )
                   MOVE    SPA-GMN-INPUTCD(IDX-1)
                                          TO  SPA-MAE-INPUTCD (IDX-1)
               ELSE
                   MOVE    SPACE      TO  SPA-MAE-INPUTCD (IDX-1)
                   MOVE    SPACE      TO  SPA-COM-CHKFLG (IDX-1)
               END-IF
           END-IF
      *
           .
       10028-NYUIN-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    回数入力チェック処理
      *****************************************************************
       100281-KAISUU-CHK-SEC           SECTION.
      *
      *    回数に変更があった時、剤内の警告はクリアする
      *    IF     SPA-GMN-INPUTCD(IDX-K)
      *                            NOT =   SPA-MAE-INPUTCD(IDX-K)
      ******** COMPUTE IDX-K           =   SPA-GMN-IDX   -   1
      *        PERFORM VARYING     IDX-K2  FROM   (IDX-K - 1)  BY  -1
      *                UNTIL       IDX-K2  <   1
      *           IF      SPA-COM-ZAIEND (IDX-K2)  =   "1"
      *               MOVE    1                TO  IDX-K2
      *           ELSE
      *               IF      SPA-COM-SRYKBN (IDX-K2)  =  "80"
      *                    MOVE    SPACE           TO  SPA-COM-KZMFLG
      *                                                     (IDX-K2)
      *                    MOVE    SPACE           TO  SPA-COM-KZMFLG2
      *                                                     (IDX-K2)
      *               END-IF
      *           END-IF
      *        END-PERFORM
      **** END-IF
      *
      *    クリア
           MOVE    SPACE           TO  SPA-COM-INPUTCD(IDX-K)
           INITIALIZE                  SPA-COM-TBLREC (IDX-K)
      *
      *    回数（未入力時、１回）
           IF      LNK-SC97-KAISUCNT   =   ZERO
               MOVE    1               TO  SPA-COM-KAISU (IDX-K)
               MOVE    "1"             TO  SPA-COM-KAIFLG(IDX-K)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(IDX-K)
               MOVE    LNK-SC97-KAISU  TO  SPA-COM-KAISU (IDX-K)
               IF      SPA-COM-KAISU  (IDX-K)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (IDX-K)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU (IDX-K)
                                       TO  SPA-COM-KAISU2 (IDX-K)
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (IDX-K)
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
              IF      LNK-SC97-NYU-DAY (IDD)   NOT =   ZERO
                   IF      SPA-K02-NYUDAY (IDD)    =   ZERO
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (IDX-K IDD)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (IDX-K)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100281-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       10028-INPUTSET-SYORI-SEC      SECTION.
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "K02"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-SYORIFLG        TO  ORCSCSETHEN-SYORIFLG
           MOVE    SPA-NENREI          TO  ORCSCSETHEN-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  ORCSCSETHEN-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  ORCSCSETHEN-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  ORCSCSETHEN-LSTSRYKAMEI
      **** MOVE    SPA-K02-TEISEI-AREA TO  ORCSCSETHEN-TEISEI-AREA
      *
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-SCR-REC
      *    展開後の位置
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
      *
           .
       10028-INPUTSET-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    セット展開の空白行削除処理
      *****************************************************************
       100282-SYOKYO-SEC         SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-HZN-SCR-AREA
           MOVE    ZERO                TO  SPA-GMN-IDX
           INITIALIZE                      SPA-SCR-REC
      *
           PERFORM VARYING     IDX-1     FROM    1   BY  1
                   UNTIL       IDX-1     >=  SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD(IDX-1)    =   SPACE ) AND
                      (WRK-HZN-COM-INPUTCD(IDX-1)    =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    WRK-HZN-COM-TBLREC (IDX-1) 
                                           TO  SPA-COM-TBLREC
                                                    (SPA-GMN-IDX)
                   MOVE    WRK-HZN-GMN-TBLREC (IDX-1) 
                                           TO  SPA-GMN-TBLREC
                                                    (SPA-GMN-IDX)
               END-IF
           END-PERFORM
           .
       100282-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為展開処理
      *****************************************************************
       400-DOWKSRY-TENKAI-SEC          SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *    ワーク診療行為検索
           PERFORM 4001-WKSRYACT-INIT-SEC
      *
           IF      LNK-WKSRYACT-MAX    >   ZERO
               PERFORM 1002-SAIHEN-SYORI-SEC
           END-IF
      *
           .
       400-DOWKSRY-TENKAI-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       4001-WKSRYACT-INIT-SEC         SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    2               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO
      *        IF      PARA-FILEMEI        =   "WKSRYACT"
      *            IF      PARA-DATA-REC   NOT =   SPACE
      *                ADD     1                   TO  LNK-WKSRYACT-MAX
      *                IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
      *                    DISPLAY "KT01 LNK-WKSRYACT-MAX 400 OVR"
      *                    MOVE    "9000"          TO  SPA-ERRCD
      *                ELSE
      *                    MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
      *                                               (LNK-WKSRYACT-MAX)
      *                END-IF
      *            END-IF
      *        END-IF
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      *****CLOSE                       PARA-FILE
      *
      *ver.4.8
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                  OR  (SPA-ERRCD       NOT =   SPACE )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "KT01 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "9000"              TO  SPA-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4001-WKSRYACT-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除集処理
      *****************************************************************
       210-PARA-DEL-SEC         SECTION.
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    MOVE    WRK-DAY             TO  FILE-DAY
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME3          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ****                             ORCSFDELAREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           STRING  "KT01WKSRY"
                   WRK-DAY             DELIMITED  BY  SIZE
                                   INTO    PARA-FILEMEI
           END-STRING
      *
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 8003-PARA-DBDELETET-SEC
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為テーブル編集処理
      *****************************************************************
       200-GMN-WKSRYACT-SEC         SECTION.
      *
      *    画面の診療日分は削除
           MOVE    SPA-NAI-SRYDD       TO  WRK-DAY
           MOVE    ZERO                TO  SPA-NAI-RRKDAY (WRK-DAY)
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                  TBL-DAY-GRP-G
      *
      **   MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    MOVE    WRK-DAY             TO  FILE-DAY
      *    OPEN    INPUT               WKSRY-FILE
      *    IF      STS-WKSRY           =   ZERO
      *        CLOSE                       WKSRY-FILE
      *        PERFORM 210-PARA-DEL-SEC
      *    ELSE
      *        CLOSE                       WKSRY-FILE
      *****END-IF
      *
      *
           MOVE    ZERO                TO  IDX-STR
           MOVE    ZERO                TO  IDX-END
           PERFORM VARYING     IDXW    FROM    1   BY  1
                   UNTIL      (IDXW    >   SPA-MAX-LINE      )   OR
                             ((SPA-GMN-INPUTCD (IDXW)  =   SPACE )
                          AND (SPA-COM-INPUTCD (IDXW)  =   SPACE ))
               IF     (IDXW                    =   SPA-MAX-LINE )  OR
                     ((SPA-GMN-INPUTCD (IDXW + 1)   =   SPACE )
                  AND (SPA-COM-INPUTCD (IDXW + 1)   =   SPACE ))
                   MOVE    "1"             TO  SPA-COM-ZAIEND (IDXW)
               END-IF
               IF      SPA-COM-ZAIEND (IDXW)   =   "1"
                   IF      IDX-STR             =   ZERO
                       MOVE    IDXW            TO  IDX-STR
                   END-IF
                   MOVE    IDXW                TO  IDX-END
                   PERFORM 2001-WRKSRYTBL-HEN-SEC
      *            日毎編集
                   PERFORM 2003-DAYTBL-SET-SEC
                   MOVE    ZERO                TO  IDX-STR
               ELSE
                   IF      IDX-STR             =   ZERO
                       MOVE    IDXW            TO  IDX-STR
                   END-IF
               END-IF
           END-PERFORM
           .
       200-GMN-WKSRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日毎編集編集処理
      *****************************************************************
       2003-DAYTBL-SET-SEC           SECTION.
      *
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL       (IDX-KAI1   >   10  )   OR
                               (TBL-KAISU(IDX-KAI1)    =   ZERO )
               PERFORM VARYING      IDX-KAI2    FROM    1   BY  1
                       UNTIL        IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2)  NOT =   ZERO
                       MOVE    IDX-KAI2        TO  WRK-DAY
                       MOVE    TBL-KAISU (IDX-KAI1)
                                               TO  WRK-KAISUKEI
                       PERFORM 2002-DAYTBL-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       2003-DAYTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為テーブル編集処理
      *****************************************************************
       2001-WRKSRYTBL-HEN-SEC            SECTION.
      *
      *    回数再編集
           PERFORM 51011-KAISU-SET1-SEC
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDXL
           MOVE    ZERO                TO  WRK-PARAEDABAN
           MOVE    1                   TO  FLG-SYORI
           PERFORM 2002-WKSRYACT-SYORI-SEC
           .
       2001-WRKSRYTBL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    回数再編集処理
      *****************************************************************
       51011-KAISU-SET1-SEC             SECTION.
      *
           INITIALIZE                      TBL-KAISUU-AREA
           MOVE    ZERO                TO  IDX-KAI
           MOVE    ZERO                TO  WRK-TENSUKEI
      *    回数再編集
           PERFORM VARYING     IDX-K   FROM    IDX-END  BY  -1
                   UNTIL       IDX-K   <   IDX-STR
      *        点数再編集
               IF      SPA-COM-TENSU  (IDX-K)  NOT =   ZERO
                   MOVE    SPA-COM-TENSU  (IDX-K)  TO  WRK-TENSUKEI
               END-IF
               MOVE    WRK-TENSUKEI    TO  SPA-COM-TENSU  (IDX-K)
      *        回数・日数
               IF      SPA-GMN-INPUTCD(IDX-K)(1:1)   =   "*"
                   PERFORM 1001-KAISUU-SYORI-SEC
               END-IF
           END-PERFORM
      *    ’＊’の行がなかった時、１回の診療日とする
           IF      TBL-KAISU (01)          =   ZERO
      *        回数計
      *        外用薬の時、入力回数を
               IF     (SPA-COM-SRYKBN(IDX-END) =  "23"  )  AND
                      (SPA-COM-KAISU2(IDX-END) >   ZERO ) AND
                      (SPA-COM-KAISU (IDX-END) =   1    )
      ********     MOVE    SPA-COM-KAISU2 (IDX-END) TO  WRK-KAISU
                   MOVE    1                        TO  WRK-KAISU
               ELSE
                   MOVE    SPA-COM-KAISU  (IDX-END) TO  WRK-KAISU
               END-IF
               IF      WRK-KAISU      =   ZERO
                   MOVE    1              TO  WRK-KAISU
               END-IF
               MOVE    WRK-KAISU           TO  TBL-KAISU (01)
               IF      TBL-KAISU (01)      =   ZERO
                   MOVE    1               TO  TBL-KAISU (01)
               END-IF
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-KAI2
               MOVE    1                   TO  TBL-DAY   (01 IDX-KAI2)
           END-IF
      *    算定日が重複した時、前の回数とする
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL       IDX-KAI1    >   10
               PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                       UNTIL       IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2) >   ZERO
                       COMPUTE IDX-KAI3    =   IDX-KAI1    +  1
                       PERFORM VARYING     IDY FROM   IDX-KAI3  BY  1 
                                UNTIL      IDY    >   10
                           MOVE    ZERO           TO  TBL-DAY
                                                      (IDY IDX-KAI2)
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       51011-KAISU-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *     回数決定処理
      *****************************************************************
       1001-KAISUU-SYORI-SEC              SECTION.
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    "KT01"              TO  LNK-SC97-PG
           MOVE    IDX-K               TO  LNK-SC97-GMN-IDX
           MOVE    SPA-GMN-INPUTCD (IDX-K)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *    変換文字列編集
           PERFORM 100281-KAISUU-CHK-SEC
      *
           MOVE    SPA-COM-KAISU (IDX-K)   TO  WRK-KAISU
      *
      *    外用薬の時、回数は１回とする
           IF      SPA-COM-SRYKBN (IDX-STR)    =   "23"
               MOVE    1               TO  WRK-KAISU
           END-IF
           MOVE    ZERO                TO  IDX-KAI1
           MOVE    ZERO                TO  IDX-KAI2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   10   )  OR
                              (IDX-KAI1    >   ZERO )
               IF      TBL-KAISU (IDX2)     =   ZERO
                   MOVE    IDX2            TO  IDX-KAI1
                   MOVE    WRK-KAISU       TO  TBL-KAISU (IDX-KAI1)
               ELSE
                   IF      WRK-KAISU       =   TBL-KAISU (IDX2)
                       MOVE    IDX2            TO  IDX-KAI1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定日
           MOVE    ZERO                TO  FLG-DAY
           PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                   UNTIL       IDX-KAI2    >   31
               IF      SPA-COM-NYUINDAY (IDX-K IDX-KAI2) NOT =   ZERO
                   MOVE    1               TO  TBL-DAY
                                                   (IDX-KAI1 IDX-KAI2)
                   MOVE    1               TO  FLG-DAY
               END-IF
           END-PERFORM
           IF      FLG-DAY             =   ZERO
               MOVE    SPA-SRYYMD(7:2) TO  IDD
               MOVE    1               TO  TBL-DAY(IDX-KAI1 IDD)
           END-IF
           .
       1001-KAISUU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為テーブル編集処理
      *****************************************************************
       20021-WKSRYACT-TBL-SEC          SECTION.
      *
           MOVE    SPA-COM-SRYSYUKBN (IDX)
                                           TO  WRK-SRYSYUKBN
           MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
           MOVE    SPA-COM-HKNCOMBI(IDX)   TO  WRK-HKNCOMBI
           IF      SPA-COM-HKNCOMBI(IDX)   =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           END-IF
           MOVE    SPA-COM-SRYKA   (IDX)   TO  WRK-SRYKA
           IF      SPA-COM-SRYKA   (IDX)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM 
           MOVE    SPA-PTID            TO  WKSRY-PTID 
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN 
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA 
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD 
           MOVE    ZERO                TO  WKSRY-ZAINUM 
           MOVE    ZERO                TO  WKSRY-RENNUM 
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *    診療会計マスタ作成の為、剤点数計、回数を保存する
           MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
           MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *    各計
           MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
           MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
           MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
           MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *    同時診療連番
           MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *    継続区分
           MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *@
      *    包括剤は継続区分に'H'を編集
           IF      SPA-COM-HOUKATU-ZAI(IDX)    =   1
               MOVE    "H"                 TO  WKSRY-CONTKBN
           END-IF
      *    薬評・器評
           IF      SPA-COM-YAKUHYO (IDX)   =   "1"
               MOVE    1                   TO  WKSRY-ZAIKBN
           END-IF
      *    減点
           IF      SPA-COM-GENTEN (IDX)    =   "1"
               MOVE    2                   TO  WKSRY-ZAIKBN
           END-IF
      *    ドクター
           MOVE    SPA-DRCD            TO  WKSRY-DRCD
      *
           ADD     1                   TO  IDXL
           MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC (IDXL)
      *
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
      *
           .
       20021-WKSRYACT-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為テーブル編集処理
      *****************************************************************
       2002-DAYTBL-HEN-SEC            SECTION.
      *
      *    診療日
           MOVE    1                   TO  SPA-NAI-RRKDAY (WRK-DAY)
      *ver.4.8
           IF     (WRK-DAY         NOT =   SPA-NAI-SRYDD )
             AND  (TBL-DAY-EDANUM (WRK-DAY)
                                       =   ZERO          )
      *        画面診療日以外は、一時ファイル検索
               PERFORM 20021-DAYPARA-SYORI-SEC
           END-IF
      *
           MOVE    TBL-DAY-EDANUM (WRK-DAY)    TO  WRK-EDANUM
           MOVE    TBL-DAY-ZAINUM (WRK-DAY)    TO  WRK-ZAINUM
      *
           ADD     1                   TO  WRK-ZAINUM
           PERFORM VARYING     IDXW2   FROM    1   BY  1
                   UNTIL      (IDXW2   >   IDXL  )
                         OR   (SPA-ERRCD   NOT =   SPACE)
               MOVE    WRK-ZAINUM          TO  LNK-WKSRY-ZAINUM 
                                                              (IDXW2)
               MOVE    IDXW2               TO  LNK-WKSRY-RENNUM 
                                                              (IDXW2)
               MOVE    WRK-KAISUKEI        TO  LNK-WKSRY-ZAIKAIKEI
                                                              (IDXW2)
      *
               INITIALIZE                      PARA-REC
               MOVE    "KTDA"              TO  PARA-GYOUMUID
               STRING  "KT01WKSRY"
                       WRK-DAY             DELIMITED  BY  SIZE
                                       INTO    PARA-FILEMEI
               END-STRING
               ADD     1                   TO  WRK-EDANUM
               MOVE    WRK-EDANUM          TO  PARA-EDANUM
               MOVE    LNK-WKSRYACT-REC (IDXW2)   TO  PARA-DATA-REC
      *
               PERFORM 8002-PARA-DBINSERT-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-EDANUM          TO  TBL-DAY-EDANUM (WRK-DAY)
           MOVE    WRK-ZAINUM          TO  TBL-DAY-ZAINUM (WRK-DAY)
      *
      ***  MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    MOVE    WRK-DAY             TO  FILE-DAY
      *    MOVE    ZERO                TO  FLG-WKSRY
      *    MOVE    ZERO                TO  WRK-ZAINUM
      *
      *    OPEN    INPUT               WKSRY-FILE
      *    IF      STS-WKSRY        NOT =  ZERO
      *******  DISPLAY "ORCSCWKSRY STS-WKSRY OPEN:"  STS-WKSRY  "]"
      *        MOVE    1                   TO  FLG-WKSRY
      *    END-IF
      *
      *    IF      STS-WKSRY           =  ZERO
      *        INITIALIZE                      WKSRY-F-REC
      *        START   WKSRY-FILE
      *            KEY IS      >=   WKSRY-F-KEY
      *        INVALID
      *            MOVE    1               TO  FLG-WKSRY
      *        NOT INVALID
      *            PERFORM 980-WKSRY-READ-SEC
      *        END-START
      *        PERFORM UNTIL     FLG-WKSRY     =   1
      *            IF      WRK-ZAINUM          <   WKSRY-F-ZAINUM
      *                MOVE    WKSRY-F-ZAINUM      TO  WRK-ZAINUM
      *            END-IF
      *            PERFORM 980-WKSRY-READ-SEC
      *        END-PERFORM
      *        CLOSE                           WKSRY-FILE
      *    ELSE
      *        CLOSE                       WKSRY-FILE
      *        OPEN    OUTPUT              WKSRY-FILE
      *        CLOSE                       WKSRY-FILE
      *    END-IF
      *
      *    OPEN    I-O                 WKSRY-FILE
      *    IF      STS-WKSRY        NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-WKSRY OPEN I-O:"  STS-WKSRY  "]"
      *    END-IF
      *
      *    ADD     1                   TO  WRK-ZAINUM
      *    PERFORM VARYING     IDXW2   FROM    1   BY  1
      *            UNTIL       IDXW2   >   IDXL
      *        MOVE    WRK-ZAINUM          TO  LNK-WKSRY-ZAINUM 
      *                                                       (IDXW2)
      *        MOVE    IDXW2               TO  LNK-WKSRY-RENNUM 
      *                                                       (IDXW2)
      *        MOVE    WRK-KAISUKEI        TO  LNK-WKSRY-ZAIKAIKEI
      *                                                       (IDXW2)
      *
      *        MOVE    SPACE               TO  WKSRY-F-REC
      *        MOVE    WRK-ZAINUM          TO  WKSRY-F-ZAINUM
      *        MOVE    IDXW2               TO  WKSRY-F-RENNUM
      *
      *        MOVE    LNK-WKSRYACT-REC (IDXW2)   TO  WKSRY-DATA-REC
      *        WRITE   WKSRY-F-REC
      *        IF      STS-WKSRY        NOT =  ZERO
      *            DISPLAY "ORCSCWKSRY STS-WKSRY OUT:"  STS-WKSRY  "]"
      *              ",WKSRY-F-KEY:" WKSRY-F-KEY
      *        END-IF
      *    END-PERFORM
      *
      *****CLOSE                           WKSRY-FILE
           .
       2002-DAYTBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日毎の一時テーブル検索処理
      *****************************************************************
       20021-DAYPARA-SYORI-SEC            SECTION.
      *
      *    当日分に追加
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           STRING  "KT01WKSRY"
                   WRK-DAY             DELIMITED  BY  SIZE
                                   INTO    PARA-FILEMEI
           END-STRING
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           IF      FLG-PARA            =   ZERO
               MOVE    PARA-DATA-REC       TO  HZN-WKSRYACT-REC
               MOVE    PARA-EDANUM         TO  TBL-DAY-EDANUM (WRK-DAY)
               MOVE    HZN-WKSRY-ZAINUM    TO  TBL-DAY-ZAINUM (WRK-DAY)
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20021-DAYPARA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴テーブル編集処理
      *****************************************************************
       2003-SANTEI-HEN-SEC            SECTION.
      *
           .
       2003-SANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       2002-WKSRYACT-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                  WRK-WKSRY-AREA
           MOVE    1                   TO  WRK-ZAINUM
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
           MOVE    ZERO                TO  IDW-STR
           PERFORM VARYING     IDX     FROM    IDX-STR  BY  1
                   UNTIL       IDX     >   IDX-END
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                                                   OR  "*"
                   IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                       MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                       MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
                   END-IF
               ELSE
      *
                   ADD     1                       TO  IDY
                   IF      IDY                     =   1
                       MOVE    IDX                     TO  IDW-STR
                   END-IF
      *            入力コード
                   MOVE    SPA-GMN-INPUTCD(IDX)    TO  WKSRY-INPUTCD
                                                                 (IDY)
      *            行為コード
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WKSRY-SRYCD
                                                                 (IDY)
      *            セットの時、セットコードを行為コードへ
                   IF     (SPA-COM-INPUTCD(IDX)    =   SPACE )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                       MOVE    SPA-GMN-INPUTCD(IDX)(1:6)
                                                    TO  WKSRY-SRYCD
                                                                 (IDY)
                   END-IF
      *
      *!!          数量入力のできないコードは数量編集なし
                   IF     (SPA-COM-SURYO  (IDX)    >   1   )  AND
                          (SPA-COM-INPUTCD(IDX)(1:3)   =   "058"
                                                       OR  "001"
                                                       OR  "002"
                                                       OR  "099")
                       IF      SPA-COM-INPUTCD(IDX)(1:7)   =   "0992000"
                           CONTINUE
                       ELSE
                           MOVE    ZERO          TO  SPA-COM-SURYO(IDX)
                           MOVE    SPACE         TO  SPA-COM-SURFLG(IDX)
                       END-IF
                   END-IF
      *!!
      *            数量
                   IF      SPA-COM-SURYO  (IDX)    =   ZERO
                       MOVE    1                   TO  SPA-COM-SURYO
                                                                 (IDX)
                   END-IF
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WKSRY-SRYSURYO
                                                                 (IDY)
      *            回数
                   IF      SPA-COM-DATAKBN (IDX)   =   "3"
      *                画像診断のみ
                    AND   (SPA-COM-SRYKBN  (IDX)   =   "70" )
                       MOVE    SPA-COM-BUNGSU(IDX) TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *!!
      *            外用の個別回数を設定
                   IF      SPA-COM-SRYKBN  (IDX)   =   "23"
                       MOVE    SPA-COM-KAISU2 (IDX) TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *!!
      *            自動算定区分
                   MOVE    SPA-COM-AUTOKBN (IDX)    TO  WKSRY-AUTOKBN
                                                                 (IDY)
      *            訂正で前回がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-MAE-AUTOKBN(IDX) =   "4"
                                                    OR  "5"
                                                    OR  "6"   )
                      MOVE    SPA-COM-MAE-AUTOKBN(IDX)
                                                   TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *            数量の入力がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-SURFLG  (IDX)    =   "1"   )
                       MOVE    "S"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *!!
      *            器材の商品名入力がある時
                   IF     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7"   )
                     AND  (SPA-COM-AUTOKBN2(IDX)    NOT =   SPACE )
                       MOVE    "T"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *!!
      *            時間外区分入力がある時、英字変換後セット
                   IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                       PERFORM 20021-JIKANGAI-SET-SEC
                   END-IF
      *
      *            自費金額
                   IF      SPA-COM-SRYKBN (IDX)     =   "96"  OR  "95"
                       MOVE    SPA-COM-JIHIMONEY (IDX)
                                                    TO  WKSRY-JIHIMONEY
                                                                 (IDY)
                   END-IF
      *            入力コメント番号
                   IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                     OR  "84" )
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                     OR  "0084")
                      OR  (SPA-COM-INPUTCD(IDX)      =   "810000001"
                                                     OR  "008500000"
                                                     OR  "008600000" )
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0085" AND
                           SPA-COM-MEIFLG (IDX)      =   "1"   )
                      OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0086" AND
                           SPA-COM-MEIFLG (IDX)      =   "1"   )
      *               用法コメント
                      OR  (SPA-COM-INPUTCD(IDX)(1:3) =   "001"  AND
                           SPA-COM-YCOMKBN(IDX)      =   1     )
      *H30.4
                      OR  (SPA-COM-INPUTCD(IDX)(1:7) =   "0992081")
      *R02.4
                      OR  (SPA-COM-INPUTCD(IDX)(1:2) =   "85"     )
      *
                       ADD     1                   TO  WRK-MEINUM
                       MOVE    WRK-MEINUM          TO  WKSRY-INPUTNUM
                                                                 (IDY)
      *
                       MOVE    SPA-COM-ATAI1(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 1)
                       MOVE    SPA-COM-ATAI2(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 2)
      *                入力値
                       MOVE    SPA-COM-ATAI3(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 3)
      *                入力値
                       MOVE    SPA-COM-ATAI4(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 4)
      *                入力値
                       MOVE    SPA-COM-ATAI5(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 5)
      *                入力コメント
                       IF      SPA-COM-MEIFLG (IDX)    =   "1"
                           IF   (SPA-COM-INPUTCD(IDX)  =   "810000001"
                                                       OR  "008500000"
                                                       OR  "008600000" )
                               MOVE    SPA-GMN-MEISYO-G (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           ELSE
                               IF  (SPA-COM-INPUTCD(IDX)(1:4) =  "0085"
                                                              OR "0086")
                                AND (SPA-GMN-MEISYO (IDX)      =
                                                    SPA-COM-NAME (IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    SPA-GMN-MEISYO (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                               END-IF
                           END-IF
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                             OR  "84")
                              OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                             OR  "0084")
      *H30.4
                              OR  (SPA-COM-INPUTCD(IDX)(1:7)
                                                       =   "0992081")
      *R02.4
                              OR  (SPA-COM-INPUTCD(IDX)(1:2) =   "85" )
                               MOVE    SPA-GMN-MEISYO(IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
      *                    用法コメント
                           IF     (SPA-COM-INPUTCD(IDX)(1:3) =   "001" )
                               MOVE    SPA-COM-NAME  (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       END-IF
                   END-IF
      *ver.4.5
      *            継続コメント
                   IF      SPA-COM-INPUTCONT (IDX)     =   "1"
                       MOVE    "1"             TO  WKSRY-INPUTKBN(IDY)
                   END-IF
      *            換算値数量入力
                   IF      SPA-COM-KANSURYO (IDX)  NOT =   ZERO
                       MOVE    SPA-COM-KANSURYO (IDX)
                                               TO  WKSRY-KANSURYO(IDY)
      *                外用の個別回数を設定
                       IF      SPA-COM-SRYKBN  (IDX)   =   "23"
                           COMPUTE WKSRY-KANSURYO(IDY)
                                               =   SPA-COM-KANSURYO(IDX)
                                               *   WKSRY-SRYKAISU
                                                                 (IDY)
                       END-IF
                   END-IF
      *            内服１種類区分
                   IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
                       MOVE    "2"             TO  WKSRY-INPUTKBN(IDY)
                   END-IF
      *
                   MOVE    SPA-COM-KAISU  (IDX)   TO  WRK-KAISUKEI
      *            画面の計を集計する（１剤で１件）
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                      OR  (SPA-COM-TENSU  (IDX)    >  ZERO )
                       MOVE    SPA-COM-TENSU  (IDX)   TO  WRK-TENSUKEI
      *                各計を編集する
                       MOVE    SPA-COM-SYUTEN1(IDX)   TO  WRK-SYUTEN1
                       MOVE    SPA-COM-SYUTEN2 (IDX)  TO  WRK-SYUTEN2
                       MOVE    SPA-COM-YKZTEN  (IDX)  TO  WRK-YKZTEN
                       MOVE    SPA-COM-KIZAITEN(IDX)  TO  WRK-KIZAITEN
                   END-IF
      *
                   IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
                       ADD      SPA-COM-JIHIMONEY (IDX)
                                                  TO  WRK-KINGAK
                   END-IF
      ****         算定履歴編集
      *            IF      FLG-SYORI           =   1
      *                IF      SPA-COM-SANTEIRRKKBN (IDX)  NOT =   ZERO
      *                    PERFORM 2003-SANTEI-HEN-SEC
      *                END-IF
      ***          END-IF
      *
                   IF     (IDX                     <   SPA-GMN-MAX) AND
                          (SPA-GMN-INPUTCD (IDX + 1)(1:1)
                                                   =   "."    )
                       MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   END-IF
                   IF     (SPA-COM-ZAIEND (IDX)    =   "1"   )  OR
                          (IDY                     =   5     )
                       IF      FLG-SYORI           =   1
                           PERFORM 20021-WKSRYACT-TBL-SEC
                       ELSE
                           PERFORM 30021-WKSRYPARA-OUT-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 >   ZERO
               MOVE    IDX-STR             TO  IDX
               IF      FLG-SYORI           =   1
                   PERFORM 20021-WKSRYACT-TBL-SEC
               ELSE
                   PERFORM 30021-WKSRYPARA-OUT-SEC
               END-IF
           END-IF
      *
           .
       2002-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分 編集処理
      *****************************************************************
       20021-JIKANGAI-SET-SEC            SECTION.
      *
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)       NUMERIC   ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)       =   SPACE ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)   NOT =   SPACE )
               PERFORM 20021-JIKANGAI-CHK-SEC
           ELSE
               IF     (SPA-NYUGAIKBN           =   "1" ) AND
                      (SPA-COM-DATAKBN (IDX)   =   "1" ) AND
      *               時間加算区分
                      (SPA-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                   EVALUATE    SPA-COM-TIMEFLG (IDX)
                       WHEN    1
                           MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    2
                           MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    3
                           MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    4
                           MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    5
                           MOVE    "E"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    6
                           MOVE    "F"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    7
                           MOVE    "G"      TO  WKSRY-AUTOKBN  (IDY)
                       WHEN    8
                           MOVE    "H"      TO  WKSRY-AUTOKBN  (IDY)
                   END-EVALUATE
               END-IF
           END-IF
           .
       20021-JIKANGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     時間外区分チェック処理
      *****************************************************************
       20021-JIKANGAI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TIME
           PERFORM VARYING     IDT     FROM    IDX   BY  1
                   UNTIL      (IDT     >   SPA-GMN-MAX )  OR
                              (FLG-TIME    =   1       )
      *        自動発生の時間外加算
               IF     (SPA-COM-DATAKBN(IDT)    =   "2" )  AND
                      (SPA-COM-AUTOKBN(IDT)    =   "2" )
      *********  AND  (SPA-COM-TIMEKSNKBN(IDT) NOT =   ZERO )
                   MOVE    1               TO  FLG-TIME
               END-IF
               IF      SPA-COM-ZAIEND (IDT)    =   "1"
                   MOVE    SPA-GMN-MAX         TO  IDT
               END-IF
           END-PERFORM
      *
           IF      FLG-TIME             =   1
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "2"
                       MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "3"
                       MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "4"
                       MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "5"
                       MOVE    "E"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "6"
                       MOVE    "F"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "7"
                       MOVE    "G"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "8"
                       MOVE    "H"      TO  WKSRY-AUTOKBN  (IDY)
               END-EVALUATE
           END-IF
           .
       20021-JIKANGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタ登録（登録時）処理
      *****************************************************************
       300-TOUROKU-PARA-SEC         SECTION.
      *
      *    保険毎の科をテーブルへ
           PERFORM 45021-HKNCOMBI-SRYKA-SEC
      *
      **   MOVE    SPA-TERMID          TO  FILE-TERMID5
      **   MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM5
      **   MOVE    ORCSCWKSRY-DAY      TO  FILE-DAY5
           MOVE    ZERO                TO  FLG-WKSRY02
           MOVE    ZERO                TO  WRK-ZAINUM
      *
      **** OPEN    OUTPUT              WKSRY-FILE02
      *    IF      STS-WKSRY02      NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-WKSRY OPEN:"  STS-WKSRY02  "]"
      *        MOVE    1                   TO  FLG-WKSRY02
      **** END-IF
      *ver.4.8
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           STRING  "KT02WKSRY"
                   ORCSCWKSRY-DAY      DELIMITED  BY  SIZE
                                       INTO    PARA-FILEMEI
           END-STRING
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 8003-PARA-DBDELETET-SEC
      *
           MOVE    ZERO                TO  WRK-EDANUM
      *
      *    診療行為マスタのパラメタを作成する
           MOVE    1                   TO  IDX-STR
           MOVE    SPA-GMN-MAX         TO  IDX-END
           MOVE    ZERO                TO  WRK-PARAEDABAN
           MOVE    2                   TO  FLG-SYORI
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *    自動作成の診療行為作成処理
           PERFORM 3004-JIDOU-SAKU-SEC
      *
      *****CLOSE                       WKSRY-FILE02
      *
      *    収納マスタ処理 
           PERFORM 30021-SYUNOU-NYU-SYORI-SEC
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARA-SYORI-SEC
           .
       300-TOUROKU-PARA-EXT.
           EXIT.
      *****************************************************************
      *    複数保険毎の複数科をテーブル編集処理
      *****************************************************************
       45021-HKNCOMBI-SRYKA-SEC             SECTION.
      *
           INITIALIZE                  SPA-TBL-HKNCOMBI-AREA
           MOVE    1                   TO  IDH
      *    主保険・主科
           MOVE    SPA-HKNCOMBINUM     TO  SPA-TBL-HKNCOMBI (IDH)
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-TBL-HKNCOMBIMEI(IDH)
           MOVE    SPA-GMN-FTNRATE     TO  SPA-TBL-FTNRATE   (IDH)
           MOVE    SPA-GMN-FTNRATEMEI  TO  SPA-TBL-FTNRATEMEI(IDH)
           MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA (IDH 1)
           MOVE    SPA-DRCD            TO  SPA-TBL-DRCD  (IDH 1)
      *
           .
       45021-HKNCOMBI-SRYKA-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為パラメタ出力処理
      *****************************************************************
       30021-WKSRYPARA-OUT-SEC              SECTION.
      *
           MOVE    SPA-COM-SRYSYUKBN (IDX)
                                           TO  WRK-SRYSYUKBN
           MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
           MOVE    SPA-COM-HKNCOMBI(IDX)   TO  WRK-HKNCOMBI
           IF      SPA-COM-HKNCOMBI(IDX)   =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           END-IF
           MOVE    SPA-COM-SRYKA   (IDX)   TO  WRK-SRYKA
           IF      SPA-COM-SRYKA   (IDX)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           END-IF
      *    各附加情報（１件目に附加する）
           IF      WRK-RENNUM          =   ZERO
               PERFORM 30022-WKSRYACTPLUS-SEC
           END-IF
      *
           MOVE    SPA-COM-HOUKATU-ZAI (IDX)   TO  WRK-HOUKATU-ZAI
           MOVE    SPA-COM-YAKUHYO     (IDX)   TO  WRK-YAKUHYO
           MOVE    SPA-COM-GENTEN      (IDX)   TO  WRK-GENTEN
           PERFORM 300211-WKSRYPARA-WRITE-SEC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               MOVE    ZERO                TO  IDY
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
           .
       30021-WKSRYPARA-OUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       300211-WKSRYPARA-WRITE-SEC          SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM 
           MOVE    SPA-PTID            TO  WKSRY-PTID 
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN 
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA 
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD 
           MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM 
           MOVE    WRK-RENNUM          TO  WKSRY-RENNUM 
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *    診療会計マスタ作成の為、剤点数計、回数を保存する
           MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
           MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *    各計
           MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
           MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
           MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
           MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *    同時診療連番
           MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *    継続区分
           MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *@
      *    包括剤は継続区分に'H'を編集
           IF      WRK-HOUKATU-ZAI      =   1
               MOVE    "H"              TO  WKSRY-CONTKBN
           END-IF
      *    薬評・器評
           IF      WRK-YAKUHYO         =   "1"
               MOVE    1               TO  WKSRY-ZAIKBN
           END-IF
      *    減点
           IF      WRK-GENTEN          =   "1"
               MOVE    2               TO  WKSRY-ZAIKBN
           END-IF
      *@
      *    ドクター
           MOVE    SPA-DRCD            TO  WKSRY-DRCD
      *
      ***  MOVE    SPACE               TO  WKSRY-F02-REC
      *    MOVE    WRK-ZAINUM          TO  WKSRY-F02-ZAINUM
      **** MOVE    WRK-RENNUM          TO  WKSRY-F02-RENNUM
      *    ＤＢからの作成でない
           MOVE    ALL "1"             TO  WKSRY-TERMID
      *
           MOVE    WKSRYACT-REC        TO  PARA-WKSRYACT-REC
           MOVE    WKSRYACTPLUS-REC    TO  PARA-WKSRYACTPLUS-REC
      **** MOVE    SPACE               TO  PARA-DATA-REC
      *    MOVE    PARA-WKSRYACT-REC   TO  WKSRY02-DATA-REC
      **
      *    WRITE   WKSRY-F02-REC
      *    IF      STS-WKSRY02     NOT =   ZERO
      *        DISPLAY "ORCSCWKSRY PARA WRITE ERR:"   STS-WKSRY02    "."
      **** END-IF
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           STRING  "KT02WKSRY"
                   ORCSCWKSRY-DAY      DELIMITED  BY  SIZE
                                       INTO    PARA-FILEMEI
           END-STRING
           ADD     1                   TO  WRK-EDANUM
           MOVE    WRK-EDANUM          TO  PARA-EDANUM
           MOVE    PARA-WKSRYACT-REC   TO  PARA-DATA-REC
           PERFORM 8002-PARA-DBINSERT-SEC
      *
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
      *
           .
       300211-WKSRYPARA-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30022-WKSRYACTPLUS-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDW
           MOVE    ZERO                TO  IDW-END
           MOVE    SPACE               TO  WKSRYACTPLUS-REC
           INITIALIZE                  WKSRYACTPLUS-REC
      *
           MOVE    ZERO                TO  FLG-TEIGEN30-ZAI
      *
           PERFORM VARYING     IDW2    FROM    IDW-STR   BY  1
                   UNTIL       IDW2    >   SPA-GMN-MAX
      *H27.4
      *        ３０日減対象判定
               IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                               OR  "22" )
                 AND  (SPA-COM-INPUTCD(IDW2)(1:1)  =   "6" )
                 AND  (SPA-COM-YOBI2 (IDW2)(1:1)   =   SPACE)
                   MOVE    1                   TO  FLG-TEIGEN30-ZAI
               END-IF
      *
               IF      SPA-COM-ZAIEND(IDW2)    =   "1"
                   MOVE    IDW2                TO  IDW-END
                   MOVE    SPA-GMN-MAX         TO  IDW2
               END-IF
           END-PERFORM
           IF      IDW-END             =   ZERO
               MOVE    SPA-GMN-MAX     TO  IDW-END
           END-IF
           PERFORM VARYING     IDW2    FROM    IDW-STR   BY  1
                   UNTIL       IDW2    >   IDW-END
               PERFORM 30022-WKSRYACTPLUS-HEN-SEC
           END-PERFORM
      *!!
      *    労災円・点数の混在
           IF     (WKSRYP-RSIKINGAKU   NOT =   ZERO )
             AND  (WKSRYP-RSI-TENSU    NOT =   ZERO )
               IF      IDW                 =   ZERO
      *            明細がない時、１件目に点数のみ記載
                   MOVE    1               TO  IDW
                   MOVE    SPA-COM-INPUTCD(IDW2-H)
                                               TO  WKSRYP-SRYCD (IDW)
                   MOVE    WKSRYP-RSI-TENSU    TO  WKSRYP-SYUTEN (IDW)
               END-IF
           END-IF
           .
       30022-WKSRYACTPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30022-WKSRYACTPLUS-HEN-SEC          SECTION.
      *
      *    労災の円
           IF      SPA-HKNKBN          =   1
               IF      SPA-COM-RSIKIN(IDW2) >   ZERO
                   MOVE    SPA-COM-RSIKIN(IDW2)    TO  WKSRYP-RSIKINGAKU
                   MOVE    "1"              TO  WKSRYP-ADDKBN
               END-IF
      *!!!!!!
      *TEST
      *        剤の収納集計用点数
               IF      TBL-SYU-TENSU (IDW2)    >   ZERO
                   ADD     TBL-SYU-TENSU (IDW2)    TO  WKSRYP-RSI-TENSU
                   IF      IDW2-H              =   ZERO
                       MOVE    IDW2                TO  IDW2-H
                   END-IF
               END-IF
      *!!!!!!
           ELSE
               MOVE    ZERO                 TO  WKSRYP-RSIKINGAKU
           END-IF
      *    点数区分
           IF     (SPA-COM-TENSIKIBETU(IDW2)   =   7
                                               OR  8  )  AND
                  (SPA-COM-KISOTEN (IDW2)  =   SPA-COM-TENSU(IDW-END))
               MOVE    1                   TO  WKSRYP-PLUSKBN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    手技毎の内容（診察・手術・画像診断・処置）
           IF     (SPA-COM-SYUFLG(IDW2)    =   1  )  AND
                  (SPA-COM-SYUTEN-M(IDW2)  NOT =  SPA-COM-SYUTEN1
                                                       (IDW-END))
               MOVE    "1"                 TO  WKSRYP-ADDKBN
               ADD     1                   TO  IDW
      *        開始手技コード
               MOVE    SPA-COM-INPUTCD(IDW2)   TO  WKSRYP-SRYCD (IDW)
      *        手技点数
               MOVE    SPA-COM-SYUTEN-M(IDW2)  TO  WKSRYP-SYUTEN (IDW)
      *        薬剤点数
               MOVE    SPA-COM-YKZTEN-M(IDW2)  TO  WKSRYP-YKZTEN (IDW)
      *        その他器材点数（フィルム・酸素・窒素以外）
               MOVE    SPA-COM-KIZTEN-M(IDW2)  TO  WKSRYP-KIZAITEN (IDW)
      *        フィルム点数
               MOVE    SPA-COM-FIRTEN-M(IDW2)  TO  WKSRYP-FIRTEN (IDW)
           ELSE
      *        フィルム点数のみ
               IF      SPA-COM-FIRTEN-M(IDW2)  NOT =  ZERO
                   ADD     1                   TO  IDW
                   MOVE    "1"                 TO  WKSRYP-ADDKBN
      *            その他器材点数（フィルム・酸素・窒素以外）
                   MOVE    SPA-COM-KIZTEN-M(IDW2)
                                               TO  WKSRYP-KIZAITEN (IDW)
                   MOVE    SPA-COM-FIRTEN-M(IDW2)
                                               TO  WKSRYP-FIRTEN (IDW)
               END-IF
           END-IF
      *    酸素点数
           IF      SPA-COM-SANSOTEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-SANSOTEN(IDW2)   TO  WKSRYP-SANSOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    窒素点数
           IF      SPA-COM-CHISOTEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-CHISOTEN(IDW2)  TO  WKSRYP-CHISOTEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *ver.4.7
      *    院外処方点数
           IF      SPA-COM-INGAITEN(IDW2)  NOT =   ZERO
               MOVE    SPA-COM-INGAITEN(IDW2)  TO  WKSRYP-INGAITEN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *    内分泌負荷試験用
           IF     (SPA-COM-SRYKBN(IDW2)    =   "60" )  AND
                  (SPA-COM-DATAKBN(IDW2)   =   "1"   )  AND
                  (SPA-COM-HOUKNSKBN(IDW2) =   08    )
      *        当月算定フラグ
               MOVE    1                   TO  WKSRYP-MSANTEITFLG
      *        当月算定点数区分
               IF      SPA-COM-TEN (IDW2)  =   SPA-COM-KISOTEN(IDW2)
                   MOVE    1               TO  WKSRYP-MSANTEITENKBN
               ELSE
                   MOVE    2               TO  WKSRYP-MSANTEITENKBN
               END-IF
      *        当月算定点数
               MOVE    SPA-COM-KISOTEN(IDW2)   TO WKSRYP-MSANTEITEN
           END-IF
      *
      *    内服７種類逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   "820000047")
      *H27.4
            AND   (SPA-NAIFUKU-OK          =   1     )
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *H26.10
      *    向精神薬多剤投与逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                           OR  "22"
                                           OR  "23" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   CONS-SGEN-SRYCD )
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *H27.4
      *    ３０日投薬逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "21"
                                           OR  "22" )
             AND  (SPA-COM-INPUTCD(IDW2)   =   CONS-TGEN-SRYCD )
             AND  (SPA-TEIGEN30-OK         =   1    )
             AND  (WRK-KAISUKEI            >=  30   )
      *        対象の剤判定
             AND  (FLG-TEIGEN30-ZAI        =   1    )
               MOVE    1                   TO  WKSRYP-TEI-NKBN30
           END-IF
      *
      *H28.4
      *    湿布薬７０枚以上逓減対象
           IF     (SPA-COM-SRYKBN(IDW2)    =   "23" )  AND
                  (SPA-COM-INPUTCD(IDW2)   =   CONS-SPCM-SRYCD )
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *
           .
       30022-WKSRYACTPLUS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       3001-PARA-SYORI-SEC          SECTION.
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KT01"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *    SPA-K01KYOUTU 保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KT01"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "KT01PARA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-K01KYOUTU       TO  SPATMP-DATA-REC
           PERFORM 30011-SPATMP-INSERT-SEC
      *
      *    SPA-KT01  保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KT01"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAKT01"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-KT01            TO  SPATMP-DATA-REC
           PERFORM 30011-SPATMP-INSERT-SEC
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    OPEN    OUTPUT              PARA-FILE
      *    IF      STS-PARA        NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-PARA OPEN:"  STS-PARA  "]"
      *    END-IF
      *
      *    共通ＳＰＡをパラメタへ出力
      *    MOVE    SPACE               TO  PARA-REC
      *    MOVE    "KT01PARA"          TO  PARA-FILEMEI
      *    MOVE    1                   TO  PARA-EDANUM
      *    MOVE    SPA-K01KYOUTU       TO  PARA-DATA-REC
      *    WRITE                       PARA-REC
      *
      *    CLOSE                       PARA-FILE
      *
      *    ＫＴ０１ＳＰＡをパラメタへ出力
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *
      *    OPEN    OUTPUT              PARA-FILE2
      *    IF      STS-PARA2       NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-PARA2 OPEN:"  STS-PARA2 "]"
      *    END-IF
      *    MOVE    SPA-KT01            TO  PARA-SPA-KT01
      *    WRITE                       PARA-SPA-KT01
      *
      **** CLOSE                       PARA-FILE2
      *
           .
       3001-PARA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       30011-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "KT01 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       30011-SPATMP-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   負担金算定・収納マスタ処理
      *****************************************************************
       30021-SYUNOU-NYU-SYORI-SEC          SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID4
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM4
      *    MOVE    ORCSCWKSRY-DAY      TO  FILE-DAY4
      *
      *    OPEN    OUTPUT              OUTSYU-FILE
      *    IF      STS-OUTSYU        NOT =  ZERO
      *        DISPLAY "ORCSCWKSRY STS-OUTSYU OPEN:"  STS-OUTSYU  "]"
      *        MOVE    1                   TO  FLG-OUTSYU
      *****END-IF
      *ver.4.8
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           STRING  "KT01SYUNO"
                   ORCSCWKSRY-DAY      DELIMITED  BY  SIZE
                                       INTO    PARA-FILEMEI
           END-STRING
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 8003-PARA-DBDELETET-SEC
      *
           MOVE    ZERO                TO  WRK-EDANUM
      *
      *    負担金算定・収納マスタ処理 
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *    保険情報
           MOVE    SPA-FTNRATE         TO  WRK-FTNRATE
           MOVE    SPA-HKNNUM          TO  WRK-HKNNUM
           MOVE    SPA-KOH1HKNNUM      TO  WRK-KOH1HKNNUM
           MOVE    SPA-KOH2HKNNUM      TO  WRK-KOH2HKNNUM
           MOVE    SPA-KOH3HKNNUM      TO  WRK-KOH3HKNNUM
           MOVE    SPA-KOH4HKNNUM      TO  WRK-KOH4HKNNUM
      *
           MOVE    ZERO                TO  WRK-PARAEDABAN
      *
      *    診療行為より、収納マスタのパラメタを作成する 
           PERFORM 4520101-SYUNOU-HENSYU-SEC
      *
      *
      *****CLOSE                       OUTSYU-FILE
           .
       30021-SYUNOU-NYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為より、収納マスタの請求額の計算処理
      *****************************************************************
       4520101-SYUNOU-HENSYU-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    ZERO                TO  SYU-DENPNUM
           MOVE    WRK-SRYKA           TO  SYU-SRYKA
           MOVE    SPA-SRYYMD          TO  SYU-SRYYMD
           MOVE    SPA-SRYYMD          TO  SYU-SKYSTYMD
           MOVE    ALL "9"             TO  SYU-SKYEDYMD
           MOVE    "1"                 TO  SYU-DENPJTIKBN
      *
           MOVE    WRK-HKNCOMBI        TO  SYU-HKNCOMBINUM
           MOVE    WRK-FTNRATE         TO  SYU-PTFTNRATE
      *    保険情報
           MOVE    WRK-HKNNUM          TO  SYU-SYUHKNNUM
           MOVE    WRK-KOH1HKNNUM      TO  SYU-KOH1HKNNUM
           MOVE    WRK-KOH2HKNNUM      TO  SYU-KOH2HKNNUM
           MOVE    WRK-KOH3HKNNUM      TO  SYU-KOH3HKNNUM
           MOVE    WRK-KOH4HKNNUM      TO  SYU-KOH4HKNNUM
      *    同時診療伝票番号
           MOVE    ZERO                TO  SYU-DOUJI-DENPNUM
      *    院外処方区分
           MOVE    SPA-INGAIKBN        TO  SYU-INGAISHOHOKBN
      *    ドクター
           MOVE    WRK-DRCD            TO  SYU-DRCD-G
      *    収納点数集計
           INITIALIZE                  WRK-SUM-AREA
      *TEST
           INITIALIZE                  TBL-K02SCR-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "." 
                                                   OR  "#" 
                                                   OR  "$" 
                   CONTINUE
               ELSE
                   PERFORM 4501-SEIKYU-KEI-SEC
               END-IF
           END-PERFORM
      *
      *    自動計算処理
           PERFORM 4504-JIDOUKEI-SEC
      *    各合計計算処理
           PERFORM 4506-TOTALKEI-SEC
      *
      **** MOVE    SYUNOU-REC          TO  OUTSYU-DATA-REC
      *****WRITE   OUTSYU-REC
      *
      *ver.4.8
      *    一時データ出力
           INITIALIZE                      PARA-REC
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           STRING  "KT01SYUNO"
                   ORCSCWKSRY-DAY      DELIMITED  BY  SIZE
                                       INTO    PARA-FILEMEI
           END-STRING
           ADD     1                   TO  WRK-EDANUM
           MOVE    WRK-EDANUM          TO  PARA-EDANUM
           MOVE    SYUNOU-REC          TO  PARA-DATA-REC
           PERFORM 8002-PARA-DBINSERT-SEC
           .
       4520101-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為より、収納マスタの請求額の計算処理
      *****************************************************************
       4501-SEIKYU-KEI-SEC             SECTION.
      *
      *    包括剤は対象外
           IF     (SPA-COM-HOUKATU-ZAI (IDX)   =   1 )  OR
                  (SPA-COM-HKNCOMBI    (IDX)   =   "9999")
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *   自費の集計（診療コードが'09500'で始まるもの）
          IF     (SPA-COM-INPUTCD(IDX)(1:5)   =   "09500"  OR
                                                  "09600" )  OR
                ((SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"  OR
                                                  "09592"  OR
                                                  "09593"  OR
                                                  "09594" )  AND
                 (SPA-COM-SRYKBN (IDX)    NOT =   "80"   ))
      *          '09691'の追加
            OR   (SPA-COM-INPUTCD(IDX)(1:5)   =   "09691"  OR
                                                  "09692"  OR
                                                  "09693"  OR
                                                  "09694" )
               PERFORM 45010-JIHI-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *   保険適用外の集計（診療コードが'095XX'で始まるもの）または、
      *                     自費の診療種別のもの
          IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096")  OR
                 (SPA-COM-SRYSYUKBN (IDX)     =   "950"  OR  "960")
               PERFORM 45011-TGMONEY-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *    労災保険は円と点数に分ける
           IF      SPA-HKNKBN          =   1
               PERFORM 45011-RSI-SYUNOU-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *    保険適用分は剤の最後の計を集計
           IF      SPA-COM-ZAIEND (IDX)    NOT =   "1"
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (14)
      *        その他
               WHEN    "80"
                   PERFORM 45011-HKNTEN-SONOTA-SEC
           END-EVALUATE
      *
           .
       4501-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他個別集計処理
      *****************************************************************
       45011-HKNTEN-SONOTA-SEC               SECTION.
      *
           EVALUATE    SPA-COM-SRYSYUKBN  (IDX)
      *        リハビリ
               WHEN    "800"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (11)
      *        処方せん料
               WHEN    "820"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        処方せん料再掲
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-SHOHOU-SAI
      *        精神科
               WHEN    "830"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (12)
      *        放射線治療
               WHEN    "840"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (13)
      *        療養担当手当て
               WHEN    "850"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (16)
      *        入院料
               WHEN    "890"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (15)
      *        その他
               WHEN    OTHER
                  ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (11)
           END-EVALUATE
           .
       45011-HKNTEN-SONOTA-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費セット集計処理
      *****************************************************************
       45010-JIHI-SET-SEC               SECTION.
      *
           IF      SPA-NYUGAIKBN          =   "1"
               MOVE    SPA-COM-NYUTENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           ELSE
               MOVE    SPA-COM-GAITENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           END-IF
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
      *H30.5   桁数チェック
               IF     (SPA-COM-KEI (IDX) + SYU-JIHI-TAXNASI
                                             (WRK-TENTTLKBN))
                                         >   9999999
                   MOVE    "1089"        TO  SPA-ERRCD
                   MOVE    "1"           TO  SPA-COM-CUR (IDX)
               END-IF
      *
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5
                   WHEN    6
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-6
                   WHEN    7
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-7
                   WHEN    8
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-8
                   WHEN    9
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-9
                   WHEN    10
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-10
               END-EVALUATE
      *        消費税あり
               WHEN    "960"
      *H26.10      追加
               WHEN    "961"
      *
      *H30.5   桁数チェック
               IF     (SPA-COM-KEI (IDX) + SYU-JIHI-TAXARI
                                             (WRK-TENTTLKBN))
                                         >   9999999
                   MOVE    "1089"        TO  SPA-ERRCD
                   MOVE    "1"           TO  SPA-COM-CUR(IDX)
               END-IF
      *
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1-TAX
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2-TAX
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3-TAX
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4-TAX
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5-TAX
                   WHEN    6
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-6-TAX
                   WHEN    7
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-7-TAX
                   WHEN    8
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-8-TAX
                   WHEN    9
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-9-TAX
                   WHEN    10
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-10-TAX
               END-EVALUATE
           END-EVALUATE
      *
           .
       45010-JIHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理
      *****************************************************************
       45011-TGMONEY-SET-SEC               SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-HKNKBN          =   1   )  AND
                  (SPA-COM-SRYKBN (IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"
                                               OR  "09592"
                                               OR  "09593"
                                               OR  "09594")
               PERFORM 450112-JIBAISEKI-SET-SEC
               GO      TO      45011-TGMONEY-SET-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096"
               MOVE    SPA-COM-INPUTCD(IDX)(4:2)   TO  WRK-SRYKBN
               MOVE    SPACE                       TO  WRK-TNSSRYSYUKBN
           ELSE
               MOVE    SPA-COM-TNSSRYKBN(IDX)      TO  WRK-SRYKBN
               MOVE    SPA-COM-TNSSRYSYUKBN(IDX)   TO  WRK-TNSSRYSYUKBN
           END-IF
      *
      *    薬剤の時、投薬へ
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
      *        注射薬の時、注射へ
               IF      SPA-COM-YKZKBN (IDX)    =   "4"
                   MOVE    "31"                        TO  WRK-SRYKBN
               ELSE
                   MOVE    "21"                        TO  WRK-SRYKBN
               END-IF
           END-IF
      *    器材の時、その他へ
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "7" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:3)   =   "059")
               MOVE    "80"                        TO  WRK-SRYKBN
           END-IF
      *!!!
      *H30.5
      *    桁数チェック
           PERFORM 450113-TGMONE-CHK-SEC
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
                   PERFORM 450111-TGMONEY-SEC
      *        消費税あり
               WHEN    "960"
                   PERFORM 450111-TGMONEY-TAX-SEC
           END-EVALUATE
           .
       45011-TGMONEY-SET-EXT.
           EXIT.
      *
      *H30.5
      *****************************************************************
      *    保険適用外の桁数チェック処理
      *****************************************************************
       450113-TGMONE-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDG
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   MOVE    1                   TO  IDG
      *        指導
               WHEN    "13"
                   MOVE    2                   TO  IDG
      *        在宅
               WHEN    "14"
                   MOVE    3                   TO  IDG
      *        投薬
               WHEN    "21"   THRU  "27"
                   MOVE    4                   TO  IDG
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    5                   TO  IDG
      *        処置
               WHEN    "40"
                   MOVE    6                   TO  IDG
      *        手術
               WHEN    "50"
                   MOVE    7                   TO  IDG
      *        麻酔
               WHEN    "54"
                   MOVE    8                   TO  IDG
      *        検査
               WHEN    "60"
                   MOVE    9                   TO  IDG
      *        Ｘ線
               WHEN    "70"
                   MOVE    10                  TO  IDG
      *        病理診断
               WHEN    "64"
                   MOVE    14                  TO  IDG
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       MOVE    11                  TO  IDG
      *            処方せん料
                   WHEN    "820"
                       MOVE    04                  TO  IDG
      *            精神科
                   WHEN    "830"
                       MOVE    12                  TO  IDG
      *            放射線治療
                   WHEN    "840"
                       MOVE    13                  TO  IDG
      *            入院料
                   WHEN    "890"
                       MOVE    15                  TO  IDG
      *            療養担当手当て
                   WHEN    "850"
                       MOVE    16                  TO  IDG
      *            その他
                   WHEN    OTHER
                       MOVE    11                  TO  IDG
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   MOVE    12                  TO  IDG
      *        放射線治療
               WHEN    "84"
                   MOVE    13                  TO  IDG
      *        入院料
               WHEN    "89"
                   MOVE    15                  TO  IDG
      *        療養担当手当て
               WHEN    "85"
                   MOVE    16                  TO  IDG
      *        その他
               WHEN    OTHER
                   MOVE    11                  TO  IDG
           END-EVALUATE
      *
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
                   IF     (SPA-GMN-KEI (IDX)   +   SYU-TGMONEY (IDG))
                                               >   9999999
                       MOVE    "1089"        TO  SPA-ERRCD
                       MOVE    "1"           TO  SPA-COM-CUR(IDX)
                   END-IF
      *        消費税あり
               WHEN    "960"
               WHEN    "961"
                   IF     (SPA-GMN-KEI (IDX)   +   SYU-TGMONEY-TAX
                                                                (IDG))
                                               >   9999999
                       MOVE    "1089"        TO  SPA-ERRCD
                       MOVE    "1"           TO  SPA-COM-CUR(IDX)
                   END-IF
           END-EVALUATE
           .
       450113-TGMONE-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険適用外の集計処理(消費税なし)
      *****************************************************************
       450111-TGMONEY-SEC               SECTION.
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (14)
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
      *            処方せん料
                   WHEN    "820"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (04)
      *            精神科
                   WHEN    "830"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (12)
      *            放射線治療
                   WHEN    "840"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (13)
      *            入院料
                   WHEN    "890"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (15)
      *            療養担当手当て
                   WHEN    "850"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (16)
      *            その他
                   WHEN    OTHER
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (12)
      *        放射線治療
               WHEN    "84"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (13)
      *        入院料
               WHEN    "89"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (15)
      *        療養担当手当て
               WHEN    "85"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (16)
      *        その他
               WHEN    OTHER
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (11)
           END-EVALUATE
      *
           .
       450111-TGMONEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理(消費税あり)
      *****************************************************************
       450111-TGMONEY-TAX-SEC               SECTION.
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (14)
      *        その他
               WHEN    "80"
                   EVALUATE    WRK-TNSSRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (11)
      *            処方せん料
                   WHEN    "820"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (04)
      *            精神科
                   WHEN    "830"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (12)
      *            放射線治療
                   WHEN    "840"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (13)
      *            療養担当手当て
                   WHEN    "850"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (16)
      *            入院料
                   WHEN    "890"
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (15)
      *            その他
                   WHEN    OTHER
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX
                                                                   (11)
                   END-EVALUATE
      *        精神科
               WHEN    "83"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (12)
      *        放射線治療
               WHEN    "84"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (13)
      *        療養担当手当て
               WHEN    "85"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (16)
      *        入院料
               WHEN    "89"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (15)
      *        その他
               WHEN    OTHER
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (11)
           END-EVALUATE
      *
           .
       450111-TGMONEY-TAX-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自賠責診断料・明細料その他の編集処理
      *****************************************************************
       450112-JIBAISEKI-SET-SEC        SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
      *    労災保険−その他
           COMPUTE WRK-EN              =   SPA-COM-TEN (IDX)
                                       *   SPA-COM-KAISU (IDX)
           ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *    労災の円
           MOVE    WRK-EN              TO  SPA-COM-RSIKIN(IDX)
      *
           .
       450112-JIBAISEKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険計算処理
      *****************************************************************
       45011-RSI-SYUNOU-SEC               SECTION.
      *
      *    労災保険は円と点数に分ける
           IF     (SPA-COM-TENSIKIBETU (IDX)   =   1  )  AND
                  (SPA-COM-RSIKBN      (IDX)   =   1  )  AND
                  (SPA-COM-INPUTCD (IDX)(1:1)  =   "1")
      *        円
      *H26.2
      *        労災金額％加算再計算対応
               PERFORM 450112-RSI-SKASAN-SEC
      *******  COMPUTE WRK-EN          =   SPA-COM-TEN (IDX)
               COMPUTE WRK-EN          =   WRK-ENTANKA
                                       *   SPA-COM-KAISU (IDX)
               MOVE    ZERO            TO  WRK-TENSU
      *        労災の円
      ******   MOVE    WRK-EN          TO  SPA-COM-RSIKIN(IDX)
               MOVE    WRK-ENTANKA     TO  SPA-COM-RSIKIN(IDX)
               MOVE    1               TO  FLG-RSITEN
           ELSE
               IF     (SPA-COM-TENSIKIBETU (IDX)   =   3  )
      *            点数
                   COMPUTE WRK-TENSU       =   SPA-COM-KISOTEN (IDX)
                                           *   SPA-COM-KAISU (IDX)
                   IF      SPA-COM-PLUSKBN(IDX)    =   1
                       COMPUTE WRK-TENSU       =  (SPA-COM-KISOTEN (IDX)
                                               *   SPA-COM-KAISU (IDX) )
                                               *   -1
                   END-IF
                   MOVE    ZERO            TO  WRK-EN
      *            労災の円
                   MOVE    ZERO            TO  SPA-COM-RSIKIN(IDX)
      *
               ELSE
                   MOVE    ZERO            TO  WRK-EN
                   MOVE    ZERO            TO  WRK-TENSU
               END-IF
           END-IF
      *
           IF     (SPA-COM-SRYKBN  (IDX)   =   "11"
                                           OR  "12"
                                           OR  "13" ) OR
                 ((SPA-COM-SRYKBN (IDX)    =   "80" ) AND
                  (FLG-RSITEN              =   1    ))
               CONTINUE
           ELSE
               IF      SPA-COM-ZAIEND (IDX)    NOT =   "1"
                   GO      TO      45011-RSI-SYUNOU-EXT
               END-IF
           END-IF
      *
      *TEST
      *    収納　円・点数
           IF     (WRK-TENSU           >   ZERO)
              AND (FLG-RSITEN          =   1  )
               MOVE    WRK-TENSU           TO  TBL-SYU-TENSU (IDX)
           END-IF
      *TEST
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        初診
               WHEN    "11"
      *
      *        労災保険−初診料
                   ADD     WRK-EN              TO  SYU-RSISHOSHIN-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (01)
      *        再診
               WHEN    "12"
      *            *労災保険−再診料
                   ADD     WRK-EN              TO  SYU-RSISAISHIN-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
      *            労災保険−指導料
                   ADD     WRK-EN              TO  SYU-RSISDO-MONEY
                   ADD     WRK-TENSU           TO  SUM-SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (07)
      *        麻酔
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (08)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (09)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (10)
      *        病理診断
               WHEN    "64"
                   ADD     SPA-GMN-KEI (IDX)   TO  SUM-SYU-HKNTEN (14)
      *        その他
               WHEN    "80"
      *            労災保険−その他
                   ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *            その他の円と点数は剤内で混在しないこととする(H14.8)
                   IF      WRK-EN              =   ZERO
                       PERFORM 45011-HKNTEN-SONOTA-SEC
                   END-IF
           END-EVALUATE
           .
       45011-RSI-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災金額％加算再計算対応処理
      *****************************************************************
       450112-RSI-SKASAN-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-ENTANKA
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               COMPUTE IDR-1           =   SPA-GMN-MAX +   1
           ELSE
               COMPUTE IDR-1           =   IDX     +   1
           END-IF
           PERFORM VARYING     IDR-2   FROM    IDR-1   BY  1
                   UNTIL      (IDR-2   >   SPA-GMN-MAX  )
      *
               IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "5"  )  AND
                      (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *************％加算　円未満切り捨て
      *            ％加算　円未満切り上げ（労災加算と合わせる）
                   MOVE    ZERO                TO  WRK-TENKEISAN
                   COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
                                           *   SPA-COM-KISOTEN (IDR-2)
                                           /   100
                                           +   0.9
                   MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *
      *対象なし
      *        IF     (SPA-COM-TENSIKIBETU (IDR-2) =   "6"  )  AND
      *               (SPA-COM-INPUTCD(IDR-2)(1:3) =   "101")
      *            ％減算　円未満切り捨て
      *            MOVE    ZERO                TO  WRK-TENKEISAN
      *            COMPUTE WRK-TENKEISAN   =   SPA-COM-TEN (IDX)
      *                                    *   SPA-COM-KISOTEN (IDR-2)
      *                                    /   100
      *            MOVE    WRK-TENKEISAN       TO  WRK-TENSUKEI-9
      *            ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
      ***      END-IF
               IF      (SPA-COM-ZAIEND (IDR-2)  =   "1" )
                   MOVE    SPA-GMN-MAX     TO  IDR-2
               END-IF
           END-PERFORM
           COMPUTE WRK-ENTANKA         =   SPA-COM-TEN (IDX)
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
           .
       450112-RSI-SKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動計算処理
      *****************************************************************
       4504-JIDOUKEI-SEC               SECTION.
      *
      *    自動加算分集計
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   ORCSCWKSRY-KSN-IDX
      *    包括剤は対象外
           IF     (ORCSCWKSRY-KSN-HKTKBN  (IDX)   =   1 )  OR
                  (ORCSCWKSRY-KSN-HKNCOMBI(IDX)   =   "9999")
               CONTINUE
           ELSE
      *
            IF   ( ORCSCWKSRY-KSN-HKNCOMBI (IDX)  =   WRK-HKNCOMBI) AND
                 ((WRK-SRYKA               =   ZERO   )   OR
                  (ORCSCWKSRY-KSN-SRYKA    (IDX)  =   WRK-SRYKA  ))
               EVALUATE    ORCSCWKSRY-KSN-SRYKBN  (IDX)
      *            診療
                   WHEN    "11"
                   WHEN    "12"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (01)
      *            指導
                   WHEN    "13"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (02)
      *            在宅
                   WHEN    "14"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (03)
      *            投薬
                   WHEN    "21"   THRU  "27"
      *                薬剤料逓減　を減算
                       IF      ORCSCWKSRY-KSN-SRYCD (IDX)
                                               =   "630010002"
      *H26.10
                                               OR  CONS-TAZAI-SRYCD
      *H27.4                   低紹介率病院　減算コード
                                               OR  CONS-TEIHP-SRYCD
      *H28.4                   湿布薬減算
                                               OR  CONS-SPGEN-SRYCD
                           COMPUTE SUM-SYU-HKNTEN (04)
                                               =   SUM-SYU-HKNTEN(04)
                                               -   ORCSCWKSRY-KSN-TENSU
                                                                 (IDX)
                       ELSE
                           ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                                    TO  SUM-SYU-HKNTEN
                                                                  (04)
                       END-IF
      *            注射
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(05)
      *            処置
                   WHEN    "40"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(06)
      *            手術
                   WHEN    "50"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(07)
      *            麻酔
                   WHEN    "54"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(08)
      *            検査
                   WHEN    "60"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(09)
      *            Ｘ線
                   WHEN    "70"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(10)
      *            病理診断
                   WHEN    "64"
                       ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN(14)
      *            その他
                   WHEN    "80"
                       PERFORM 4504-JIDOUKEI-SONOTA-SEC
               END-EVALUATE
             END-IF
      *
           END-IF
      *
           END-PERFORM
      *
           .
       4504-JIDOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    その他個別集計処理
      *****************************************************************
       4504-JIDOUKEI-SONOTA-SEC               SECTION.
      *
           EVALUATE    ORCSCWKSRY-KSN-SRYSYUKBN (IDX)
      *        リハビリ
               WHEN    "800"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (11)
      *        処方せん料
               WHEN    "820"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (04)
      *        処方せん料再掲
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SYU-SHOHOU-SAI
      *        精神科
               WHEN    "830"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (12)
      *        放射線治療
               WHEN    "840"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (13)
      *        療養担当手当て
               WHEN    "850"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (16)
      *        入院料
               WHEN    "890"
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                               TO  SUM-SYU-HKNTEN (15)
      *        その他
               WHEN    OTHER
                   ADD     ORCSCWKSRY-KSN-TENSU (IDX)
                                              TO  SUM-SYU-HKNTEN (11)
           END-EVALUATE
           .
       4504-JIDOUKEI-SONOTA-EXT.
           EXIT.
      *
      *****************************************************************
      *    各合計計算処理
      *****************************************************************
       4506-TOTALKEI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   16
      *        保険点数
               IF      SUM-SYU-HKNTEN(IDX)  <   ZERO
                   MOVE    "0224"               TO  SPA-ERRCD
               ELSE
                   MOVE    SUM-SYU-HKNTEN (IDX) TO  SYU-HKNTEN(IDX)
              END-IF
      *!!!
      *H30.5
      *       桁オーバーチェック
              IF     (SUM-SYU-HKNTEN(IDX)  +   SUM-SYU-HKNTEN(17))
                                           >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1090"           TO  SPA-ERRCD
                  END-IF
              END-IF
              ADD     SUM-SYU-HKNTEN(IDX)  TO  SUM-SYU-HKNTEN(17)
              IF     (SYU-TGMONEY(IDX) +   SYU-TGMONEY(17))
                                       >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1089"           TO  SPA-ERRCD
                  END-IF
              END-IF
              IF     (SYU-TGMONEY-TAX(IDX) +   SYU-TGMONEY-TAX(17))
                                       >   9999999
                  IF      SPA-ERRCD        =   SPACE
                      MOVE    "1089"           TO  SPA-ERRCD
                  END-IF
              END-IF
      *H30.5
      *
      *       保険点数
              ADD     SYU-HKNTEN(IDX)      TO  SYU-HKNTEN(17)
      *       金額
              ADD     SYU-MONEY (IDX)      TO  SYU-MONEY (17)
      *       適用外金額
              ADD     SYU-TGMONEY(IDX)     TO  SYU-TGMONEY(17)
      *       適用外金額（消費税あり）
              ADD     SYU-TGMONEY-TAX(IDX) TO  SYU-TGMONEY-TAX(17)
      *
           END-PERFORM
      *!!!
      *H30.5
      *    自費合計桁数７桁オーバーチェック
           PERFORM 45069-TOTALOVER-CHK-SEC
      *
           .
       4506-TOTALKEI-EXT.
           EXIT.
      *****************************************************************
      *    自費合計桁数７桁オーバーチェック処理
      *****************************************************************
       45069-TOTALOVER-CHK-SEC         SECTION.
      *
      *!!!
      *H30.5
      *    合計桁数７桁オーバーチェック
           IF     (SYU-HKNTEN(17)      +   WRK-SUM-HKNTEN )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
           IF     (SYU-TGMONEY(17)     +   WRK-SUM-TGMONEY )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
           IF     (SYU-TGMONEY-TAX(17) +   WRK-SUM-TGMONEY-TAX )
                                       >   9999999
               MOVE    "1089"              TO  SPA-ERRCD
           END-IF
      *
           ADD     SYU-HKNTEN(17)      TO  WRK-SUM-HKNTEN
           ADD     SYU-TGMONEY(17)     TO  WRK-SUM-TGMONEY
           ADD     SYU-TGMONEY-TAX(17) TO  WRK-SUM-TGMONEY-TAX
      *!!!
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )
      *        消費税なし
               IF     (SYU-JIHI-TAXNASI (IDX)  +  WRK-SUM-JIHI-TOTAL)
                                           >   9999999
                   MOVE    "1089"          TO  SPA-ERRCD
               END-IF
      *        消費税あり
               IF     (SYU-JIHI-TAXARI  (IDX)
                                           +  WRK-SUM-JIHI-TOTAL-TAX)
                                           >   9999999
                   MOVE    "1089"          TO  SPA-ERRCD
               END-IF
               ADD     SYU-JIHI-TAXNASI(IDX)  TO  WRK-SUM-JIHI-TOTAL
               ADD     SYU-JIHI-TAXARI (IDX)  TO  WRK-SUM-JIHI-TOTAL-TAX
           END-PERFORM
      *
           .
       45069-TOTALOVER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動作成の診療行為作成処理
      *****************************************************************
       3004-JIDOU-SAKU-SEC         SECTION.
      *
      *    自動加算料
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  WRK-HOUKATU-ZAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCSCWKSRY-KSN-IDX
               IF      IDY             =   ZERO
                   MOVE    SPACE               TO  WKSRYACT-REC
                   INITIALIZE                  WKSRYACT-REC
                   MOVE    SPACE               TO  WKSRYACTPLUS-REC
                   INITIALIZE                  WKSRYACTPLUS-REC
                   ADD     1                   TO  WRK-ZAINUM
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    ORCSCWKSRY-KSN-HKNCOMBI (IDX)
                                               TO  WRK-HKNCOMBI
                   MOVE    ORCSCWKSRY-KSN-SRYKA (IDX) TO  WRK-SRYKA
                   MOVE    ORCSCWKSRY-KSN-SRYSYUKBN(IDX)
                                               TO  WRK-SRYSYUKBN
                   MOVE    ORCSCWKSRY-KSN-SRYKBN(IDX) TO  WRK-SRYKBN
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   MOVE    ZERO                TO  WRK-KAISUKEI
                   MOVE    ZERO                TO  WRK-KINGAK
                   MOVE    ZERO                TO  WRK-MEINUM
               END-IF
               ADD     1                       TO  IDY
               MOVE    ORCSCWKSRY-KSN-SRYCD  (IDX)
                                               TO  WKSRY-SRYCD (IDY)
               MOVE    ORCSCWKSRY-KSN-INPUTCD(IDX)
                                               TO  WKSRY-INPUTCD (IDY)
               MOVE    1                       TO  WKSRY-SRYSURYO(IDY)
               IF      ORCSCWKSRY-KSN-SURYO(IDX)   >   1
                   MOVE    ORCSCWKSRY-KSN-SURYO (IDX)
                                               TO  WKSRY-SRYSURYO(IDY)
               END-IF
               MOVE    ZERO                    TO  WKSRY-ZAIKAISU(IDY)
               MOVE    ORCSCWKSRY-KSN-AUTOKBN (IDX)
                                               TO  WKSRY-AUTOKBN (IDY)
               IF      ORCSCWKSRY-KSN-AUTOKBN(IDX) =   SPACE
                    MOVE    "3"                TO  WKSRY-AUTOKBN (IDY)
               END-IF
               IF      ORCSCWKSRY-KSN-AUTOKBN(IDX) =   "1"
                    MOVE    SPACE              TO  WKSRY-AUTOKBN (IDY)
               END-IF
               IF     (ORCSCWKSRY-KSN-SRYCD  (IDX)(1:1)
                                                       =   "8"  )  AND
                      (ORCSCWKSRY-KSN-ATAI-G (IDX) NOT =   SPACE )
                   PERFORM 45022-COMMNT-SET-SEC
               END-IF
               ADD     ORCSCWKSRY-KSN-TENSU(IDX)      TO  WRK-TENSUKEI
               ADD     ORCSCWKSRY-KSN-TENSU(IDX)      TO  WRK-SYUTEN1
               IF      ORCSCWKSRY-KSN-HKTKBN(IDX)  =  1
                   MOVE    ORCSCWKSRY-KSN-HKTKBN(IDX)
                                               TO  WRK-HOUKATU-ZAI
               END-IF
               MOVE    1                       TO  WRK-KAISUKEI
      *        各附加情報編集処理
               PERFORM 30041-WKSRYACTPLUS-SEC
      *
               IF      ORCSCWKSRY-KSN-ZAIEND(IDX)     =   "1"
      *            診療行為パラメタ出力
      ***          MOVE    ORCSCWKSRY-KSN-HKTKBN(IDX)
      ******                                   TO  WRK-HOUKATU-ZAI
                   MOVE    SPACE               TO  WRK-YAKUHYO
                   MOVE    SPACE               TO  WRK-GENTEN
                   PERFORM 300211-WKSRYPARA-WRITE-SEC
                   ADD     1                   TO  WRK-ZAINUM
                   MOVE    ZERO                TO  IDY
                   INITIALIZE                  WRK-WKSRY-AREA
                   MOVE    ZERO                TO  WRK-HOUKATU-ZAI
               END-IF
           END-PERFORM
           .
       3004-JIDOU-SAKU-EXT.
           EXIT.
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       30041-WKSRYACTPLUS-SEC          SECTION.
      *
      *    薬剤料逓減　を減算
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   "630010002"
                                               OR  CONS-TAZAI-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
      *H27.4
      *    低紹介率病院　減算
      *    薬剤料逓減　を減算
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   CONS-TEIHP-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN30
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
      *H28.4
      *    湿布薬減点
           IF      ORCSCWKSRY-KSN-SRYCD (IDX)  =   CONS-SPGEN-SRYCD
               MOVE    1                   TO  WKSRYP-PLUSKBN
      *        内服７種類逓減対象
               MOVE    2                   TO  WKSRYP-TEI-NKBN30
               MOVE    "1"                 TO  WKSRYP-ADDKBN
           END-IF
      *
           .
       30041-WKSRYACTPLUS-EXT.
           EXIT.
      *****************************************************************
      *    追加のコメントの日数変換編集処理
      *****************************************************************
       45022-COMMNT-SET-SEC              SECTION.
      *
           INITIALIZE                  WRK-SCR-REC
           MOVE    WKSRY-SRYCD (IDY)   TO  WRK-COM-INPUTCD (01)
           MOVE    ORCSCWKSRY-KSN-ATAI-G(IDX) TO  WRK-COM-INPUTCHI-G(01)
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    1                   TO  LNK-SC92-GMN-IDX
           MOVE    WKSRY-SRYCD (IDY)   TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      **** MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    入力コメント番号
           MOVE    1                   TO  WKSRY-INPUTNUM  (IDY)
      *
           MOVE    WRK-COM-ATAI1(01)   TO  WKSRY-INPUTCHI  (IDY 1)
           MOVE    WRK-COM-ATAI2(01)   TO  WKSRY-INPUTCHI  (IDY 2)
           MOVE    WRK-COM-ATAI3(01)   TO  WKSRY-INPUTCHI  (IDY 3)
           MOVE    WRK-COM-ATAI4(01)   TO  WKSRY-INPUTCHI  (IDY 4)
           MOVE    WRK-COM-ATAI5(01)   TO  WKSRY-INPUTCHI  (IDY 5)
           MOVE    WRK-GMN-MEISYO(01)  TO  WKSRY-INPUTCOMENT (IDY)
      *
           .
       45022-COMMNT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタ登録（他画面遷移）処理
      *****************************************************************
       400-SENI-PARA-SEC         SECTION.
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 3001-PARA-SYORI-SEC
      *
           .
       400-SENI-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ一時ファイル書き込み処理
      *****************************************************************
       8002-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "KT01 PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       8002-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ一時ファイル削除処理
      *****************************************************************
       8003-PARA-DBDELETET-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       8003-PARA-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    ZERO                TO  FLG-TENSUPLUS
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *ver.4.8
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-WKSRY-READ-SEC         SECTION.
      *
      *    READ    WKSRY-FILE           NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-WKSRY
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-WKSRY
      *    END-READ
      *
      *    .
      *980-WKSRY-READ-EXT.
      *    EXIT.
      *
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      **   EXIT.
      *
