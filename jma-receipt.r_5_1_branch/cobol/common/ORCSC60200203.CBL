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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC60200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（検査）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01.04.18  更新前チェックのバグ対応
      *  01.00.02    MCC−多々納  01.05.29  血液学的検査判断料のバグ対応
      *  01.00.03    MCC−多々納  01.06.26  乳幼児加算の自動算定追加
      *  01.00.14    NACL-多々納  05/12/08  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-JIKASAN         PIC 9(01).
      *
           03  FLG-ADD             PIC 9(01).
      *
      *    静脈区分
           03  FLG-BV              PIC 9(01).
      *    動脈区分
           03  FLG-BA              PIC 9(01).
      *    抹消区分 
           03  FLG-BC              PIC 9(01).
      *    採血料算定不可
           03  FLG-FUKA            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-ACT             PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(04).
      *----(01.00.01) LINE ADD START ----------------------------------
           03  IDX-CHK             PIC 9(04).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  IDX-JIKAN           PIC 9(04).
           03  IDX-NYU             PIC 9(04).
      *
           03  IDX-TEIGEN          PIC 9(04).
           03  IDX-TEIGEN2         PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-ERRCD           PIC X(04). 
           03  WRK-SRYCD           PIC X(09). 
      *
           03  WRK-IDX             PIC 9(03).
      *    乳幼児の自動加算コード
           03  WRK-NYU-SRYCD       PIC X(09).
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-K      PIC 9(08).
           03  WRK-TENSUKEI-TEI    PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
      *    フィルム点数計
           03  WRK-FIRTEN-KEI      PIC 9(08).
           03  WRK-FIRTEN-ADD      PIC 9(08).
           03  WRK-TENSUKEIS       PIC 9(08)V99.
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *
           03  WRK-KEN-HOU         PIC X(02).
           03  WRK-SANTEI-CNT      PIC 9(04).
      *
      *    検査　判断グループ
       01  WRK-KENSA-AREA.
           03  WRK-KENSA-TBL                       OCCURS   20.
               05  WRK-KENSAGRP        PIC 9(02).
               05  WRK-KEN-KBN         PIC 9(01).
               05  WRK-KEN-SRYCD       PIC X(09).
               05  WRK-KEN-TENSU       PIC 9(05)V9(03).
           03  WRK-KENIDX              PIC 9(04).
      *
      *    包括対象検査
       01  WRK-HOUKNS-AREA.
           03  WRK-HOUKNS-TBL          OCCURS   10.
               05  WRK-HOUKNS-CNT      PIC 9(04).
               05  WRK-HOUKNS-TENSU    PIC 9(07)V9(03).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *****************************************************************
      *    逓減検査包括テーブル
      *****************************************************************
      *
       01  TBL-KENSA-HOU-AREA.
      *      心カテ（右心）
           03  FILLER          PIC X(12)  VALUE    "01 160064610".
      *      肺臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065850".
      *      肺臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065850".
      *      肝臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065950".
      *      膵臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160166950".
      * 02
      *    ＥＣＧ１２
           03  FILLER          PIC X(12)  VALUE    "02 160068410".
      *    ＥＣＧ（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "02 160068510".
      *    体表ヒス束心電図
           03  FILLER          PIC X(12)  VALUE    "02 160068750".
      *    ＶＤＧ
           03  FILLER          PIC X(12)  VALUE    "02 160068850".
      *    バリストカルジオグラフ（１方向）
           03  FILLER          PIC X(12)  VALUE    "02 160069050".
      *    バリストカルジオグラフ（２方向以上）
           03  FILLER          PIC X(12)  VALUE    "02 160069130".
      *    ＥＣＧ１２
           03  FILLER          PIC X(12)  VALUE    "02 160704210".
      *    ＥＣＧ（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "02 160704310".
      *    ＶＤＧ
           03  FILLER          PIC X(12)  VALUE    "02 160708710".
      *    体表ヒス束心電図
           03  FILLER          PIC X(12)  VALUE    "02 160708810".
      *    携帯型発作時心電図記憶伝送装置使用心電図
           03  FILLER          PIC X(12)  VALUE    "02 160150650".
      * 03
      *    ＥＣＧ負荷１２
           03  FILLER          PIC X(12)  VALUE    "03 160069210".
      *    ＥＣＧ負荷（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "03 160069310".
      * 04
      *    ホルター型心電図
           03  FILLER          PIC X(12)  VALUE    "04 160069510".
      *    ホルター型心電図（８時間以上）
           03  FILLER          PIC X(12)  VALUE    "04 160069610".
      *    体表面心電図
           03  FILLER          PIC X(12)  VALUE    "04 160069750".
      *    心外膜マッピング
           03  FILLER          PIC X(12)  VALUE    "04 160069850".
      *    ホルター型心電図(３０分まで）
           03  FILLER          PIC X(12)  VALUE    "04 160128430".
      *    ホルター型心電図(３０分超１時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128530".
      *    ホルター型心電図(１時間超１時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160128630".
      *    ホルター型心電図(１時間３０分超２時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128730".
      *    ホルター型心電図(２時間超１時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160128830".
      *    ホルター型心電図(２時間３０分超３時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128930".
      *    ホルター型心電図(３時間超３時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129030".
      *    ホルター型心電図(３時間３０分超４時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129130".
      *    ホルター型心電図(４時間超４時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129230".
      *    ホルター型心電図(４時間３０分超５時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129330".
      *    ホルター型心電図(５時間超５時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129430".
      *    ホルター型心電図(５時間３０分超６時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129530".
      *    ホルター型心電図(６時間超６時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129630".
      *    ホルター型心電図(６時間３０分超７時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129730".
      *    ホルター型心電図(７時間超７時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129830".
      *    ホルター型心電図(７時間３０分超８時間未満）
           03  FILLER          PIC X(12)  VALUE    "04 160129930".
      * 05
      *    ＰＧＣ
           03  FILLER          PIC X(12)  VALUE    "05 160071910".
      *    亜硝酸アミル吸入心音図
           03  FILLER          PIC X(12)  VALUE    "05 160072030".
      * 06
      *    気管支鏡
           03  FILLER          PIC X(12)  VALUE    "06 160092610".
      *    気管支カメラ
           03  FILLER          PIC X(12)  VALUE    "06 160092750".
      * 07
      *    食道鏡
           03  FILLER          PIC X(12)  VALUE    "07 160093210".
      *    食道カメラ
           03  FILLER          PIC X(12)  VALUE    "07 160093350".
      * 08
      *    胃鏡
           03  FILLER          PIC X(12)  VALUE    "08 160093510".
      *    ガストロカメラ
           03  FILLER          PIC X(12)  VALUE    "08 160093650".
      * 09
      *    直腸鏡
           03  FILLER          PIC X(12)  VALUE    "09 160093510".
      *    コロンブラッシュ法（沈)
           03  FILLER          PIC X(12)  VALUE    "09 160094330".
      *    コロンブラッシュ法（組織）
           03  FILLER          PIC X(12)  VALUE    "09 160094430".
      * 10
      *    ＥＦ−Ｓ状結腸
           03  FILLER          PIC X(12)  VALUE    "10 160094710".
      *    ＥＦ−下行結腸及び横行結腸
           03  FILLER          PIC X(12)  VALUE    "10 160094810".
      *    ＥＦ−下行結腸及び盲腸
           03  FILLER          PIC X(12)  VALUE    "10 160094910".
      * 11
      *    リアルタイム解析型心電図
           03  FILLER          PIC X(12)  VALUE    "11 160125550".
      *    携帯型発作時心電図記録計使用心電図
           03  FILLER          PIC X(12)  VALUE    "11 160125950".
      *
       01  TBL-KENSA-HOU-AREA-R    REDEFINES   TBL-KENSA-HOU-AREA.
           03  TBL-KENSA-HOU-OCC       OCCURS  54  INDEXED  KEN-IDX.
               05  TBL-KEN-HOU         PIC X(02).
               05  TBL-FILLER          PIC X(01).
               05  TBL-KEN-SRYCD       PIC X(09).
       01  KEN-IDX-MAX                 PIC 9(04)   VALUE   54.
      *
      *****************************************************************
      *    重複算定不可テーブル
      *****************************************************************
      *
       01  TBL-KENSA-JYU-AREA.
      *どれか２つ
      *    ＨＤＬコレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160023410".
      *    総コレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160022410".
      *    ＬＤＬコレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160167250".
      *どれか２つ
      *    ＴＩＢＣ
           03  FILLER          PIC X(14)  VALUE    "02 2 160023610".
      *    ＵＩＢＣ
           03  FILLER          PIC X(14)  VALUE    "02 2 160023710".
      *    鉄
           03  FILLER          PIC X(14)  VALUE    "02 2 160022110".
      *どれか１つ
      *    Ｃａ
           03  FILLER          PIC X(14)  VALUE    "03 1 160021510".
      *    イオン化カルシウム
           03  FILLER          PIC X(14)  VALUE    "03 1 160021610".
      *
      *
       01  TBL-KENSA-JYU-AREA-R    REDEFINES   TBL-KENSA-JYU-AREA.
           03  TBL-KENSA-JYU-OCC       OCCURS  8   INDEXED  JYU-IDX.
               05  TBL-JYU-KBN         PIC X(02).
               05  TBL-FILLER1         PIC X(01).
               05  TBL-JYU-KEN         PIC X(01).
               05  TBL-FILLER2         PIC X(01).
               05  TBL-JYU-SRYCD       PIC X(09).
       01  KEN-JYU-MAX                 PIC 9(04)   VALUE   8.
      *
       01  WRK-JYUFUKU-CHK.
           03  WRK-JYU-CNT             PIC 9(02).
           03  WRK-JYU-IDX             PIC 9(04).
           03  WRK-JYU-KBN             PIC X(02).
           03  WRK-JYU-KEN             PIC 9(01).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (WRK-GMN-INPUTCD(IDX-MAX)
                                       =   SPACE )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-TEIGEN
           MOVE    ZERO                TO  IDX-TEIGEN2
           INITIALIZE                  WRK-HOUKNS-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    逓減加算算定チェック（診療行為入力時のみ）
           IF     (IDX-TEIGEN          >   ZERO )  AND
                  (LNK-ORCSC-PG        =   "K02" )
               PERFORM 1008-TEIGEN-SYORI-SEC
           END-IF
      *
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            手技料 加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *            逓減加算算定チェック
                   IF     (WRK-COM-DATAKBN (IDX-Z)     =   "1" )  AND
                          (WRK-COM-TEIGENKBN (IDX-Z)   =   1   )
                       MOVE    IDX-Z             TO  IDX-TEIGEN
                   END-IF
                   IF     (WRK-COM-DATAKBN (IDX-Z)     =   "1" )  AND
                          (WRK-COM-TEIGENKBN (IDX-Z)   =   1   )
                       MOVE    IDX-Z             TO  IDX-TEIGEN2
                   END-IF
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *            年齢加算算定チェック
                   MOVE    SPACE               TO  WRK-NYU-SRYCD
                   PERFORM 1007-NENREI-SYORI-SEC
      *----(01.00.03) LINE ADD END   ----------------------------------
           END-EVALUATE
      *
           .
       1001-SYORI-SET-EXT.
           EXIT.
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       1007-NENREI-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-NYU
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                          =   SPACE  OR  ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
               MOVE    IDX-Z               TO  IDX-S
               PERFORM 2001-JIDOTUIKA-SEC
               IF      FLG-ARI             =   1
                   MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                               (IDX-T)
                   MOVE    4                   TO  IDX2
                   MOVE    WRK-SRYCD           TO  WRK-NYU-SRYCD
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    逓減加算算定チェック加算処理
      *****************************************************************
       1008-TEIGEN-SYORI-SEC           SECTION.
      *
      *    逓減加算算定チェック
           IF     (WRK-COM-TEIGEN-FLG(IDX-TEIGEN)  =   ZERO )  AND
                  (WRK-COM-TEIGEN-CHK(IDX-TEIGEN)  =   ZERO )
               MOVE    1           TO  WRK-COM-TEIGEN-CHK(IDX-TEIGEN)
               PERFORM 100812-TEIGEN-CHK-SEC
           END-IF
      *
      *    逓減加算自動追加
           IF      WRK-COM-TEIGEN-FLG(IDX-TEIGEN)  =   1
               MOVE    "160000190"         TO  WRK-SRYCD
               MOVE    IDX-TEIGEN2         TO  IDX-S
               PERFORM 2001-JIDOTUIKA-SEC
               MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                           (IDX-T)
               MOVE    IDX-T               TO  IDX-Z
           END-IF
      *
           .
       1008-TEIGEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     逓減加算算定チェック処理
      *****************************************************************
       100812-TEIGEN-CHK-SEC           SECTION.
      *
      *    算定歴チェック
           MOVE    WRK-COM-INPUTCD (IDX-TEIGEN) TO  WRK-SRYCD
           PERFORM 108121-SANTEI-CHK-SEC
      *    逓減加算自動追加あり
           IF      WRK-SANTEI-CNT      >   ZERO
               MOVE    1           TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
               GO      TO      100812-TEIGEN-CHK-EXT
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      100812-TEIGEN-CHK-EXT
           END-IF
      *
      *    同一検査算定有無の判定
           SET     KEN-IDX             TO  1
           SEARCH      TBL-KENSA-HOU-OCC   VARYING   KEN-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KEN-HOU
                   WHEN    WRK-COM-INPUTCD (IDX-TEIGEN)
                                               =   TBL-KEN-SRYCD
                                                           (KEN-IDX)
                       MOVE    TBL-KEN-HOU  (KEN-IDX)
                                                TO  WRK-KEN-HOU
           END-SEARCH 
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-IDX-MAX)  OR
                              (WRK-KEN-HOU     =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               SET      KEN-IDX           TO  IDX
               IF      TBL-KEN-HOU  (KEN-IDX)  =   WRK-KEN-HOU
                   MOVE    TBL-KEN-SRYCD (KEN-IDX) TO  WRK-SRYCD
                   PERFORM 108121-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      WRK-SANTEI-CNT      >   ZERO
               MOVE    1           TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
               GO      TO      100812-TEIGEN-CHK-EXT
           END-IF
           .
      *
       100812-TEIGEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックコードの算定チェック
      *****************************************************************
       108121-SANTEI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
      *    点数マスタ読込
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      108121-SANTEI-CHK-EXT
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
           MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
           IF      SANTEI-FSANTEIYMD   >   SPA-SRYYMD
               MOVE    "0056"          TO  SPA-ERRCD
           END-IF
      *
      *    訂正の時、訂正前までカウントする
           IF      LNK-ORCSC-SYORIFLG  =   1
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    ZERO                TO  WRK-SANTEI-CNT
               PERFORM VARYING     IDX-Y3      FROM    1   BY  1
                       UNTIL       IDX-Y3      >=  IDX-Y2
                   ADD     SANTEI-DAY (IDX-Y3) TO  WRK-SANTEI-CNT
               END-PERFORM
           END-IF
           .
       108121-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
      *
           INITIALIZE                  WRK-HOUKNS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    薬剤点数計算
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-MAX             TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    手技料点数合計計算
           PERFORM 1004-HOUKNS-SET-SEC
      *    手技料の合計点数をセット
           COMPUTE LNK-SC90-TENSUKEI   =   WRK-TENSUKEI
                                       +   WRK-TENSUKEI-TEI
      *
      *    割合加算・減算
           PERFORM 1006-KASAN-SET-SEC
      *
      *    特定器材点数計算
           PERFORM 1005-KIZAI-TENSU-SEC
      *
      *    フィルム料合計計算
           IF      WRK-FIRTEN-ADD      >   ZERO
               COMPUTE WRK-TENSUKEIS   =   WRK-FIRTEN-ADD
                                           /   10
                                           +   0.5
               MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
               ADD     WRK-FIRTEN          TO  WRK-FIRTEN-KEI
           END-IF
      *
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           COMPUTE WRK-COM-KIZAITEN(IDX-MAX)
                                       =   LNK-SC90-KIZTEN
                                       +   WRK-FIRTEN-KEI
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
                                           +   WRK-FIRTEN-KEI
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            手技料点数加算
                   PERFORM 1003-HOUKNS-HEN-SEC
      *        器材
               WHEN    "7"
      *            フィルムの時、点数計算
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                上限点数判定
                       IF      WRK-COM-JGNTEN (IDX-Z)  =   ZERO
                           ADD     LNK-SC90-FIRTEN   TO  WRK-FIRTEN-ADD
                       ELSE
                           COMPUTE WRK-TENSUKEIS   =   LNK-SC90-FIRTEN
                                                   /   10
                                                   +   0.5
                           MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
                           IF      WRK-FIRTEN      >   WRK-COM-JGNTEN
                                                             (IDX-Z)
                               MOVE    WRK-COM-JGNTEN(IDX-Z)
                                                       TO  WRK-FIRTEN
                           END-IF
                           ADD     WRK-FIRTEN      TO  WRK-FIRTEN-KEI
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括チェック・点数集計処理
      *****************************************************************
       1003-HOUKNS-HEN-SEC                  SECTION.
      *
      *    ％加算・減算は集計しない
           IF      WRK-COM-TENSIKIBETU (IDX-Z) =   5
                                               OR  6
               CONTINUE
           ELSE
               EVALUATE    WRK-COM-HOUKNSKBN(IDX-Z)
                   WHEN    01   THRU   08
                       MOVE    WRK-COM-HOUKNSKBN(IDX-Z)    TO  IDX
                       ADD     1               TO  WRK-HOUKNS-CNT (IDX)
                       ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-HOUKNS-TENSU
                                                                 (IDX)
                   WHEN    OTHER
      *                逓減あり点数計算
                       IF      WRK-COM-TEIGENKBN (IDX-Z)   =   1
                          ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-TENSUKEI-TEI
                       ELSE
                           ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-TENSUKEI
                       END-IF
               END-EVALUATE
           END-IF
      *
           .
       1003-HOUKNS-HEN-EXT.
           EXIT.
      *----(01.00.01) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *    包括・点数集計処理
      *****************************************************************
       1004-HOUKNS-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    IDX
                   WHEN    1
      *                血液化学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    5   THRU  7
                               MOVE    "099600101"     TO  WRK-SRYCD
                           WHEN    8   THRU  9
                               MOVE    "099600102"     TO  WRK-SRYCD
                           WHEN    10  THRU  9999
                               MOVE    "099600103"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    2
      *                内分泌学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  5
                               MOVE    "099600201"     TO  WRK-SRYCD
                           WHEN    6   THRU  7
                               MOVE    "099600202"     TO  WRK-SRYCD
                           WHEN    8   THRU  9999
                               MOVE    "099600203"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    3
      *                肝炎ウイルス関連の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3
                               MOVE    "099600301"     TO  WRK-SRYCD
                           WHEN    4
                               MOVE    "099600302"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600303"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    4
      *                腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2   THRU  9999
                               MOVE    "099600401"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    5
      *                ４以外の腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600501"     TO  WRK-SRYCD
                           WHEN    3
                               MOVE    "099600502"     TO  WRK-SRYCD
                           WHEN    4   THRU  9999
                               MOVE    "099600503"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    6
      *                出血・凝固検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  4
                               MOVE    "099600601"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600602"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    7
      *                自己抗体検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600701"     TO  WRK-SRYCD
                           WHEN    3   THRU  9999
                               MOVE    "099600702"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    8
      *                内部泌負荷試験の包括
                       ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
               END-EVALUATE
           END-PERFORM
      *
           .
       1004-HOUKNS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括点数算定処理
      *****************************************************************
       10041-HOUKNS-ADD-SEC                  SECTION.
      *
           IF      FLG-ERR             =   1
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      10041-HOUKNS-ADD-EXT
           END-IF
           MOVE    1                   TO  FLG-ERR
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               ADD     TNS-TEN             TO  WRK-TENSUKEI
           END-IF
           .
       10041-HOUKNS-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定器材点数計算
      *****************************************************************
       1005-KIZAI-TENSU-SEC         SECTION.
      *
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =   LNK-SC90-KIZKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
      *
      *
           .
       1005-KIZAI-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算・減算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        加算
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        減算
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
      *            検査逓減の時、逓減分の合計より逓減する
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSUKEI-TEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
      *****************************************+   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    同一検査をエラーとする
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "60"  )  AND
                      (WRK-COM-INPUTCD(IDX-Z)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"   )
                   PERFORM VARYING     IDX-Z2  FROM    1   BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE) OR
                                      (SPA-ERRCD   NOT =   SPACE )  OR
                                  (WRK-GMN-INPUTCD (IDX-Z2) =   SPACE )
                       IF     (IDX-Z2      NOT =   IDX-Z )  AND
                              (WRK-COM-INPUTCD(IDX-Z)
                                               =   WRK-COM-INPUTCD
                                                        (IDX-Z2))
                           MOVE    "0064"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  FLG-JIKASAN
           MOVE    ZERO                TO  IDX-JIKAN
           MOVE    ZERO                TO  IDX-TEIGEN
           MOVE    ZERO                TO  IDX-TEIGEN2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        時間外緊急院内画像診断加算
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "160000210"
                   MOVE    IDX-Z           TO  IDX-JIKAN
               END-IF
      *
      *        病理診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160155110"
                   PERFORM 1006-SINDAN-CHK-SEC
               END-IF
      *
      *        病理学的検査診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160062310"
                   PERFORM 1007-KENSA-CHK-SEC
               END-IF
      *
      *        時間外加算チェック（診察、加算、時間加算のもの）
               IF     (WRK-COM-SRYKBN  (IDX-Z)     =   "11"  OR
                                                       "12"     ) AND
                      (WRK-COM-DATAKBN (IDX-Z)     =   "2"      ) AND
                      (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =   ZERO AND
                                                           5    )
                   MOVE    1               TO  FLG-JIKASAN
               END-IF
      *        逓減加算算定チェック
               IF     (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
                      (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )  AND
                      (WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO )
                   PERFORM 2008-TEIGEN-INSYORI-SEC
               END-IF
               IF     (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
                      (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )
                   MOVE    IDX-Z               TO  IDX-TEIGEN2
               END-IF
      *
      *        重複算定不可チェック
               IF     (WRK-COM-SRYKBN    (IDX-Z)   =   "60"  )  AND
                      (WRK-COM-DATAKBN   (IDX-Z)   =   "1"   )  AND
                      (WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO )
                   PERFORM 20011-JYUFUKU-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    逓減加算算定あり
           IF      IDX-TEIGEN          >   ZERO
               PERFORM 2009-TEIGEN-TUIKA-SEC
           END-IF
      *
      *    時間外緊急院内画像診断加算有り
           IF      IDX-JIKAN           >   ZERO
               MOVE    IDX-JIKAN           TO  IDX-Z
               IF     (FLG-JIKASAN         =   ZERO         )  AND
                      (WRK-COM-KZMFLG (IDX-Z)  =   SPACE    )
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
      *        時間入力追加
               MOVE    "840000044"         TO  WRK-SRYCD
               IF      WRK-COM-INPUTCD(IDX-Z + 1)  =   WRK-SRYCD
                   CONTINUE
               ELSE
                   MOVE    IDX-Z           TO  IDX-S
                   PERFORM 2001-JIDOTUIKA-SEC
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     逓減加算算定チェック処理
      *****************************************************************
       2008-TEIGEN-INSYORI-SEC           SECTION.
      *
      *    同一検査算定有無の判定
           SET     KEN-IDX             TO  1
           SEARCH      TBL-KENSA-HOU-OCC   VARYING   KEN-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KEN-HOU
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-KEN-SRYCD
                                                           (KEN-IDX)
                       MOVE    TBL-KEN-HOU  (KEN-IDX)
                                                TO  WRK-KEN-HOU
           END-SEARCH
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-IDX-MAX)  OR
                              (WRK-KEN-HOU     =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               SET     KEN-IDX             TO  IDX
               IF     (TBL-KEN-HOU  (KEN-IDX)  =   WRK-KEN-HOU ) AND
                      (TBL-KEN-SRYCD(KEN-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL-KEN-SRYCD (KEN-IDX) TO  WRK-SRYCD
                   PERFORM 20081-SANTEI-TBLCHK-SEC
               END-IF
           END-PERFORM
      *    逓減加算自動追加
           IF      WRK-SANTEI-CNT      >   ZERO
               MOVE    IDX-Z               TO  IDX-TEIGEN
           END-IF
           .
      *
       2008-TEIGEN-INSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減加算算定入力テーブル追加処理
      *****************************************************************
       2009-TEIGEN-TUIKA-SEC                  SECTION.
      *
           MOVE    1                   TO  WRK-COM-TEIGEN-FLG
                                                        (IDX-TEIGEN)
           MOVE    "160000190"         TO  WRK-SRYCD
           MOVE    IDX-TEIGEN2         TO  IDX-S
           PERFORM 2001-JIDOTUIKA-SEC
           MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                           (IDX-T)
      *
           .
       2009-TEIGEN-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減加算算定入力テーブルチェック処理
      *****************************************************************
       20081-SANTEI-TBLCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >=  IDX-Z      )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               IF      WRK-SRYCD           =   WRK-COM-INPUTCD
                                                         (IDX2)
                   ADD     1               TO  WRK-SANTEI-CNT
               END-IF
           END-PERFORM
           .
      *
       20081-SANTEI-TBLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病理診断料チェック処理
      *****************************************************************
       1006-SINDAN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *T   同月内の検査区分番号のチェック
      *    PERFORM VARYING IDX-ACT     FROM    1   BY  1
      *            UNTIL  (IDX-ACT     >   200  )  OR
      *                   (LNK-ACCT-SRYCD (IDX-ACT)    =   SPACE )
      *        IF      LNK-ACCT-CDKBN (IDX-ACT)    =   "D100" OR
      *                                                "D101"
      *            MOVE    1               TO  FLG-OK
      *        END-IF
      *T   END-PERFORM
      *
      *    今回入力の検査区分番号のチェック
           PERFORM VARYING IDX-ACT     FROM    1   BY  1
                   UNTIL  (IDX-ACT     >   200  )  OR
                          (WRK-COM-INPUTCD (IDX-ACT)  =   SPACE )
               IF     (IDX-ACT         NOT =   IDX-Z  )  AND
                      (WRK-COM-CDKBN (IDX-ACT) =   "D100" OR
                                                   "D101"  )
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    同月内の検査区分番号のチェック
           IF      FLG-OK              =   ZERO
               IF      LNK-ACCT-SRYCD (01) =   SPACE
                   PERFORM 10060-SINDAN-ACCT-HEN-SEC
               END-IF
               PERFORM VARYING IDX-ACT     FROM    1   BY  1
                       UNTIL  (IDX-ACT     >   100  )  OR
                              (LNK-ACCT-SRYCD (IDX-ACT)    =   SPACE )
                   IF      LNK-ACCT-CDKBN (IDX-ACT)    =   "D100" OR
                                                           "D101"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0026"          TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    病理学的検査診断料の算定チェック
           INITIALIZE                      ORCSC91AREA
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    "160062310"         TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-CNT        >   ZERO
      *        算定あり
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
       1006-SINDAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病理診断料チェック処理
      *****************************************************************
       1007-KENSA-CHK-SEC                  SECTION.
      *
      *    病理学的診断料の算定チェック
           INITIALIZE                      ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    "160155110"         TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-CNT        >   ZERO
      *        算定あり
               MOVE    "0018"              TO  SPA-ERRCD
      ******** MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
       1007-SINDAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     同月内の検査区分番号のチェック処理
      *****************************************************************
       10060-SINDAN-ACCT-HEN-SEC           SECTION.
      *
           INITIALIZE                  LNK-NAIACCT-AREA
           MOVE    ZERO                TO  IDX-ACT
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "60"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-HKNCOMBI     =   "9999" ) OR
                      (ACCT-ZAIKAISU     =   ZERO   )
                   CONTINUE
               ELSE
      *            診療行為マスター編集
                   PERFORM 100601-SRYACT-HEN-SEC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-ACT             =   ZERO
               MOVE    "999999999"     TO  LNK-ACCT-SRYCD (1)
           END-IF
      *
           .
       10060-SINDAN-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       100601-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "1"
      *                点数マスタより、区分番号編集
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF     (TNS-DATAKBN         =   "1" )  AND
                              (TNS-CDKBN-KBN       =   "D" )  AND
                              (TNS-CDKBN-KBNNUM    =   "100"
                                                   OR  "101" )
                           ADD     1               TO  IDX-ACT
                           IF      IDX-ACT         >   100
                               MOVE    100         TO  IDX-ACT
                           END-IF
                           MOVE    ACCT-SRYKBN     TO  LNK-ACCT-SRYKBN
                                                      (IDX-ACT)
                           MOVE    SRY-SRYCD (IDX2)
                                                   TO  LNK-ACCT-SRYCD
                                                           (IDX-ACT)
                           MOVE    TNS-CDKBN-KBN
                                               TO  LNK-ACCT-CDKBN-KBN
                                                          (IDX-ACT)
                           MOVE    TNS-CDKBN-KBNNUM
                                               TO  LNK-ACCT-CDKBN-KBNNUM
                                                          (IDX-ACT)
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100601-SRYACT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     重複算定不可チェック処理
      *****************************************************************
       20011-JYUFUKU-CHK-SEC           SECTION.
      *
      *    同一検査算定有無の判定
           SET     JYU-IDX             TO  1
           SEARCH      TBL-KENSA-JYU-OCC   VARYING   JYU-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-JYU-KBN
                       MOVE    ZERO            TO  WRK-JYU-KEN
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-JYU-SRYCD
                                                           (JYU-IDX)
                       MOVE    TBL-JYU-KBN  (JYU-IDX)
                                                TO  WRK-JYU-KBN
                       MOVE    TBL-JYU-KEN  (JYU-IDX)
                                                TO  WRK-JYU-KEN
           END-SEARCH
      *
           IF      WRK-JYU-KBN         =   SPACE
               GO      TO      20011-JYUFUKU-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-JYU-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-JYU-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               SET     JYU-IDX             TO  IDX
               IF     (TBL-JYU-KBN  (JYU-IDX)  =   WRK-JYU-KBN ) AND
                      (TBL-JYU-SRYCD(JYU-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL-JYU-SRYCD (JYU-IDX) TO  WRK-SRYCD
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   SPA-MAX-LINE)  OR
                                      (WRK-COM-INPUTCD (IDX2)
                                                   =   SPACE  AND
                                       WRK-GMN-INPUTCD (IDX2)
                                                   =   SPACE  )
                       IF      WRK-COM-INPUTCD (IDX2)  =   WRK-SRYCD
                           ADD     1               TO  WRK-JYU-CNT
                           MOVE    IDX2            TO  WRK-JYU-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      WRK-JYU-KEN         <=  WRK-JYU-CNT
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (WRK-JYU-IDX)
           END-IF
           .
      *
       20011-JYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       2001-JIDOTUIKA-SEC     SECTION.
      *    手技料の次に追加する
           COMPUTE IDX-T               =   IDX-S   +  1
           IF      WRK-COM-INPUTCD (IDX-T) =   WRK-SRYCD
               MOVE    IDX-T               TO  WRK-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IDX
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           IF      WRK-IDX             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-T)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-T               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               MOVE    IDX-S               TO  IDX-T
               GO      TO      2001-JIDOTUIKA-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ARI
      *
           MOVE    WRK-COM-SRYKBN (IDX-T - 1)
                                       TO  WRK-COM-SRYKBN(IDX-T)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-T)
           MOVE    WRK-COM-SRYSYUKBN (IDX-T - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-T)
      *
           MOVE    "1"                 TO  WRK-COM-AUTOKBN (IDX-T)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           .
       2001-JIDOTUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
     *
           MOVE    ZERO                TO  IDXS
     *
           MOVE    ZERO                TO  FLG-BV
           MOVE    ZERO                TO  FLG-BA
           MOVE    ZERO                TO  FLG-BC
           MOVE    ZERO                TO  FLG-FUKA
     *
           INITIALIZE                  WRK-KENSA-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *
      *        検査の時、検査等実施判断を行う
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1"  )  AND
                      (WRK-COM-SRYKBN (IDX)        =   "60" )
                   IF      WRK-COM-KNSJISGRPKBN (IDX)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    WRK-COM-KNSJISGRPKBN (IDX)
                                           TO  WRK-KENIDX
                       ADD     1           TO  WRK-KENSAGRP(WRK-KENIDX)
      *                判断料の判定
      *---(01.00.02)   LINE UPD START  ---------------------------------
      *****************IF      WRK-COM-KNSJISGRPKBN (IDX)  =   2
                       IF      WRK-COM-KNSJISKBN    (IDX)  =   2
      *---(01.00.02)   LINE UPD END    ---------------------------------
                           ADD     1       TO  WRK-KEN-KBN (WRK-KENIDX)
                       END-IF
                   END-IF
      *            採血料区分判定
                   EVALUATE    WRK-COM-SAIKETUKBN (IDX)
      *                抹消採血
                       WHEN    1
                           MOVE    1           TO  FLG-BC
      *                静脈採血
                       WHEN    2
                           MOVE    1           TO  FLG-BV
      *                動脈採血
                       WHEN    3
                           MOVE    1           TO  FLG-BA
                   END-EVALUATE
               END-IF
      *
      *        採血料算定不可のチェック
      *        （悪性腫瘍特異物質治療管理料を算定時、自動算定なし）
               IF      WRK-COM-INPUTCD (IDX)      =   "113001210"  OR
                                                      "113001310"  OR
                                                      "113002110"
                   MOVE    1               TO  FLG-FUKA
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF     (WRK-KENSAGRP (IDX)  NOT =   ZERO )  AND
                      (WRK-KEN-KBN  (IDX)      =   ZERO )
                   EVALUATE    IDX
                       WHEN    01
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160061710" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160700510" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
                       WHEN    02
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160061810" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160700610" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
                       WHEN    03
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160061910" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160700710" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
                       WHEN    04
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160062010" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160700810" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
                       WHEN    05
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160062110" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160700910" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
                       WHEN    06
                           IF      LNK-ORCSC-ROUJIN      =   ZERO
                               MOVE    "160062210" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           ELSE
                               MOVE    "160701010" TO  WRK-KEN-SRYCD
                                                                (IDX)
                           END-IF
      *                病理学的検査診断料
                       WHEN    07
                           MOVE    "160062310"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                呼吸機能等検査診断料
                       WHEN    11
                           MOVE    "160146910"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                脈波図、心機図、ポリグラフ
                       WHEN    12
                           MOVE    "160147010"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                脳波検査
                       WHEN    13
                           MOVE    "160147610"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                神経・筋検査
                       WHEN    14
                           MOVE    "160147710"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                ラジオアイソトープ検査
                       WHEN    15
                           MOVE    "160147910"     TO  WRK-KEN-SRYCD
                                                                (IDX)
                   END-EVALUATE
                END-IF
           END-PERFORM
      *
      *    ＳＰＡを保存する
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   20    )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-KEN-SRYCD (IDX) NOT =   SPACE
      *
      *            共通チェック処理
                   MOVE    WRK-KEN-SRYCD (IDX) TO  WRK-SRYCD
                   PERFORM 3001-KYOUTU-CHEK-SEC
      *
                   MOVE    WRK-KEN-SRYCD (IDX)     TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            病理学的検査診断料チェック
                   IF    (FLG-ERR              =   ZERO  )  AND
                         (WRK-KEN-SRYCD (IDX)  =   "160062310" )
                       PERFORM 1007-KENSA-CHK-SEC
      *
                       MOVE    WRK-KEN-SRYCD (IDX) TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *
                       IF      LNK-SC91-ERRCD      =   SPACE
      *                    追加あり
                           MOVE    ZERO            TO  FLG-ERR
                       ELSE
      *                    追加なし
                           MOVE    1               TO  FLG-ERR
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR         =   ZERO
                       ADD     1               TO  IDXS
                       MOVE    WRK-KEN-SRYCD (IDX)
                                               TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
                       MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
                       MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *                乳幼児加算セット
                       PERFORM 30051-NENREI-SET-SEC
      *                併用チェック用
                       ADD     1               TO  WRK-GMN-MAX
                       MOVE    WRK-KEN-SRYCD (IDX) TO  WRK-COM-INPUTCD
                                                      (WRK-GMN-MAX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    採血料自動発生なし
           IF      FLG-FUKA            =   1
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
      *    採血料自動発生
           IF      FLG-BA              =   1
      *        動脈血採血
               MOVE    "160101210"         TO  WRK-SRYCD
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
           IF      FLG-BV              =   1
      *        静脈血採血
               MOVE    "160095710"         TO  WRK-SRYCD
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
           IF      FLG-BC              =   1
      *        抹消採血
               MOVE    "160095810"         TO  WRK-SRYCD
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
      *3001-SANTEI-CHK-SEC         SECTION.
      *
      *----(01.00.01) LINE UPD START ----------------------------------
      *    今回入力にないこと
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
      *                       (FLG-ERR              =   1    )   OR
      *                       (WRK-GMN-INPUTCD(IDX) =   SPACE)
      *        IF      WRK-GMN-INPUTCD(IDX)    =   WRK-KEN-SRYCD (IDX)
      *            MOVE    1                   TO  FLG-ERR
      *        END-IF
      *****END-PERFORM
      *    MOVE    ZERO                TO  FLG-ERR
      *
      *    PERFORM VARYING     IDX-CHK FROM    1   BY  1
      *            UNTIL      (IDX-CHK >   SPA-MAX-LINE  )  OR
      *                       (FLG-ERR              =   1    )   OR
      *                       (WRK-GMN-INPUTCD(IDX-CHK) =   SPACE)
      *        IF      WRK-GMN-INPUTCD(IDX-CHK)    =   WRK-SRYCD
      *            MOVE    1                   TO  FLG-ERR
      *        END-IF
      *    END-PERFORM
      *----(01.00.01) LINE UPD END   ----------------------------------
      *    IF      FLG-ERR             =   1
      *        GO      TO      3001-SANTEI-CHK-EXT
      *    END-IF
      *
      *
      *    INITIALIZE                      ORCSC91AREA
      *    MOVE    "2"                 TO  LNK-SC91-KBN
      *    MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
      *    MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
      *    CALL    "ORCSC91"       USING
      *                                MCPAREA
      *                                ORCSC91AREA
      *                                WRK-SCR-REC
      *                                SPA-AREA
      *                                TNS-TENSU-REC
      *
      *    IF      LNK-SC91-ERRCD      =   SPACE
      *        追加あり
      *        MOVE    ZERO            TO  FLG-ERR
      *    ELSE
      *        追加なし
      *        MOVE    1               TO  FLG-ERR
      *    END-IF
      *
      *    .
      *3001-SANTEI-CHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    共通チェック処理
      *****************************************************************
       3001-KYOUTU-CHEK-SEC         SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
      *
           .
       3001-KYOUTU-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    採血料自動発生処理
      *****************************************************************
       3005-AUTO-SAIKETU-SEC         SECTION.
      *
      *    共通チェック処理
           PERFORM 3001-KYOUTU-CHEK-SEC
      *
           IF      FLG-ERR         =   ZERO
      *
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
               MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
               MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *        併用チェック用
               ADD     1               TO  WRK-GMN-MAX
               MOVE    WRK-SRYCD       TO  WRK-COM-INPUTCD
                                              (WRK-GMN-MAX)
      *        乳幼児加算セット
               PERFORM 30051-NENREI-SET-SEC
           END-IF
      *
           .
       3005-AUTO-SAIKETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       30051-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX2)   =   SPACE  OR
                                                           ZERO )
               MOVE    ZERO                TO  FLG-ERR
      *        加算追加
               MOVE    TNS-AGEKSNSRYCD (IDX2)  TO  WRK-SRYCD
      *        共通チェック処理
               PERFORM 3001-KYOUTU-CHEK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SRYCD           TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
      *                算定あり
                       MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD-KS
                                                              (07 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                              (07 IDXS)
                       MOVE    4               TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           .
       30051-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       300511-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-AGEKSNKGN(IDX2) TO  WRK-KGNAGE(2:2)
           MOVE    TNS-AGEKSNJGN(IDX2) TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-AGEKSNKGN(IDX2)     =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      LNK-ORCSC-NENREI-YY     >=  WRK-KGNAGE
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-AGEKSNJGN(IDX2) =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-AGEKSNJGN(IDX2)     =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <=  28   )
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY     <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
      *
           .
       300511-NENREI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
       990-DBCLOSE-SEC         SECTION.
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
