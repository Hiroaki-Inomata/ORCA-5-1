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
       PROGRAM-ID.                 ORCSC30201604.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（注射）
      *                      Ｈ２８年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/03/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  １診療行為内で、点滴は１回とする
      * 01.00.02     MCC−多々納  01/05/16  点滴注射の判定を診療種別とする
      * 01.00.03     NACL−多々納 02/11/22  点滴注射同時重複エラー追加
      * 01.00.04     NACL−多々納 02/12/09  動脈注射エラー追加
      * 01.00.05     NACL−多々納 03/01/07  入院点滴処理追加
      * 01.00.06     NACL−多々納 03/04/01  中心静脈自動発生追加
      * 01.00.07     NACL−多々納 03/04/10  皮下筋注射変換なし(312)追加
      * 01.00.08     NACL−多々納 03/09/04  手術料との算定エラー追加
      * 01.00.09     NACL−多々納 03/10/03  手術料との算定エラーを警告へ
      * 01.00.10     NACL−多々納 05/05/13  332点滴（手術以外）
      * 01.00.11     NACL−多々納 05/10/25  外来静脈注射チェック追加
      * 01.00.12     NACL-多々納  05/12/14  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/07/18  皮下筋肉注射変換なし対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  10/03/XX  Ｈ２２．４改正対応
      *  04.07.00    NACL-多々納  11/11/25  同日再入院対応
      *  04.08.00    NACL-多々納  16/06/08  332　の外来対応追加
      *  04.08.00    NACL-多々納  16/06/XX  G006の点滴同時入力対応
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
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBUTU         PIC 9(01).
           03  FLG-SYUGI           PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-TENTEKI         PIC 9(01).
      *
           03  FLG-SYUGIADD        PIC 9(01).
      *    動脈注射
           03  FLG-DOUCHU          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ZAIARI          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-GAZO-ARI        PIC 9(01).
      *
           03  FLG-OUT-OK          PIC 9(01).
      *    中心静脈あり 
           03  FLG-SYUGI-CHU       PIC 9(01).
           03  FLG-SYUGI-CHU6      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-KBN             PIC 9(04).
           03  IDX-IDY             PIC 9(04).
      *
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-HAI             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD-2         PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-CHUSYA          PIC 9(09)V9(5).
      *
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-SRYSYUKBNMEI    PIC X(40).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-GEN    PIC 9(08).
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-TENSU-GAI       PIC 9(08)V99.
               05  WRK-TENSU-KEI       PIC 9(08)V99.
      *
      *    入院日＋１年
           03  WRK-NYMD.
               05  WRK-NYY             PIC 9(04).
               05  WRK-NMM             PIC 9(02).
               05  WRK-NDD             PIC 9(02).
      *    ６歳到達日
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
      *    手技別日付
           03  WRK-TEN-SRYCD       PIC X(09).
           03  WRK-TEN-SRYCD1      PIC X(09).
           03  WRK-TEN-SRYCD2      PIC X(09).
           03  WRK-TEN-SRYCD3      PIC X(09).
           03  WRK-TEN-SRYCD6      PIC X(09).
           03  WRK-NISUU-HEN-AREA.
               05  WRK-NIDAY           PIC 9(01)   OCCURS   31.
      *
           03  WRK-TEN-AUTO        PIC X(01).
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *    入院の回数
           03  WRK-NISUU-AREA.
               05  WRK-NISUU-TBL           OCCURS   31.
                   07  WRK-DAY             PIC 9(02).
                   07  WRK-SEL             PIC X(01).
           03  IDN1                    PIC 9(02).
           03  IDN2                    PIC 9(04).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  FLG-DAY                 PIC 9(01).
           03  WRK-H-DAY               PIC 9(02).
           03  WRK-H-SEL               PIC X(01).
      *
      *    最大液量
           03  WRK-MAX-RYOU            PIC 9(04).
           03  WRK-EKIRYOU             PIC 9(09)V9(5).
           03  WRK-GOK-RYOU            PIC 9(09)V9(5).
      *    診療種別
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
      *
      *    手術チェック
           03  FLG-SYUJUTU             PIC 9(01).
           03  IDX-CHU                 PIC 9(04).
      *
      *ver.4.7
      *    同日再入院分
           03  WRK-D-DAY               PIC 9(03).
           03  IDXX                    PIC 9(02).
           03  IDX-D                   PIC 9(02).
      *
       01  TBL-NISUU-AREA.
           03  TBL-NISUU-OCC           OCCURS   31.
               05  TBL-DAY             PIC 9(01).
               05  TBL-CHUSYA          PIC 9(09)V9(5).
      *
               05  TBL-TEN-SRYCD       PIC X(09).
               05  TBL-TEN-SRYCD2      PIC X(09).
               05  TBL-AUTO-SRYCD      PIC X(09).
               05  TBL-TEN-IDX         PIC 9(04).
      *        手術有無
               05  TBL-SYU-DAY         PIC 9(01).
               05  TBL-SYU-IDX         PIC 9(04).
      *        画像診断点滴有無
               05  TBL-GAZ-DAY         PIC 9(01).
               05  TBL-GAZ-IDX         PIC 9(04).
      *        生物学的製剤
               05  TBL-SEIBUTU         PIC 9(01).
               05  TBL-SEIBUTU-IDX     PIC 9(04).
               05  TBL-SEIBUTU-ARI     PIC X(09).
      *        麻毒加算
               05  TBL-MADOKU          PIC 9(01).
               05  TBL-MADOKU-IDX      PIC 9(04).
               05  TBL-MADOKU-ARI      PIC X(09).
      *        手術以外液量
               05  TBL-CHUSYA-2        PIC 9(09)V9(5).
               05  TBL-SEIBUTU-2       PIC 9(01).
               05  TBL-MADOKU-2        PIC 9(01).
      *
      *   中心静脈
       01  TBL-IVH-NISUU-AREA.
           03  TBL-IVH-NISUU-OCC           OCCURS   31.
               05  TBL-IVH-DAY             PIC 9(01).
               05  TBL-IVH-CHUSYA          PIC 9(09)V9(5).
               05  TBL-IVH-SRYCD           PIC X(09).
               05  TBL-IVH-IDX             PIC 9(04).
               05  TBL-IVH-CHUIDX          PIC 9(04).
      *
               05  TBL-IVHA-SRYCD          PIC X(09).
      *        手術有無
               05  TBL-IVH-SYU-DAY         PIC 9(01).
      *        中心静脈（カテーテル）
               05  TBL-IVH6-SRYCD           PIC X(09).
               05  TBL-IVH6-IDX             PIC 9(04).
      *
      *        生物学的製剤
               05  TBL-IVH-SEIBUTU         PIC 9(01).
      *        麻毒加算
               05  TBL-IVH-MADOKU          PIC 9(01).
      *    前回算定分
               05  TBL-IVH2-CHUSYA         PIC 9(09)V9(5).
      *        生物学的製剤
               05  TBL-IVH2-SEIBUTU        PIC 9(01).
      *        麻毒加算
               05  TBL-IVH2-MADOKU         PIC 9(01).
      *    手術以外液量
               05  TBL-IVH-CHUSYA-2        PIC 9(09)V9(5).
               05  TBL-IVH-SEIBUTU-2       PIC 9(01).
               05  TBL-IVH-MADOKU-2        PIC 9(01).
      *
       01  WRK-SONOTA.
           03  WRK-ERRCD           PIC X(04). 
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   07  WRK-SRT-KNKMEI      PIC X(40).
                   07  WRK-SRT-SRYYM       PIC X(06).
                   07  WRK-SRT-SRYYMW      PIC X(07).
                   07  WRK-SRT-SRYCD       PIC X(09).
           03  WRK-KEY-SRYYM               PIC X(06).
           03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *    中心静脈注射
       01  CON-CHUJYO              PIC X(09)   VALUE   "130004410".
      *
      *    中心静脈注射(植入型カテーテル）
       01  CON-CHUJYO6             PIC X(09)   VALUE   "130008510".
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
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN2.INC".
      *
      *    入院通算日数編集パラメタ
           COPY    "CPORCSTUSAN.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *DDD COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
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
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   IF     (SPA-HKNCOMBINUM     =   "9999" )  OR
                          (LNK-ORCSC-PG        =   "KT01" )
                       CONTINUE
                   ELSE
                       PERFORM 200-ALL-SYORI-SEC
                   END-IF
      *        自動算定
               WHEN    "3"
                   IF     (SPA-HKNCOMBINUM     =   "9999" )  OR
                          (LNK-ORCSC-PG    NOT =   "K02"  )
                       CONTINUE
                   ELSE
                       PERFORM 300-AUTO-SYORI-SEC
                   END-IF
               WHEN    OTHER
                   MOVE    "1099"          TO  SPA-ERRCD
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
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
           MOVE    ZERO                TO  FLG-SYUGI
           MOVE    ZERO                TO  FLG-SYUGI-CHU
           MOVE    ZERO                TO  FLG-SYUGI-CHU6
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-SEIBUTU
           INITIALIZE                  WRK-AREA
           MOVE    SPACE               TO  WRK-ERRCD
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
      *
                   IF      WRK-COM-INPUTCD(IDX-Z)  =   "099303301"
                       MOVE    "1"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
      ************ WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
                   WHEN    "."
                       IF      WRK-GMN-INPUTCD(IDX-Z)  =   ".350"
                                                       OR  ".332"
                                                       OR  ".352"
      *H28.6
      *                    H28.4 から、.332 入力可
                           IF    (SPA-SRYYMD           >=  "20160401")
                             AND (WRK-GMN-INPUTCD(IDX-Z)
                                                       =   ".332" )
                               CONTINUE
                           ELSE
      *
                           IF     SPA-NYUGAIKBN        =   "2"
                               MOVE    "9024"          TO  WRK-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                               (IDX-Z)
                           END-IF
      *
                           END-IF
                       END-IF
                       IF      WRK-GMN-INPUTCD(IDX-Z)  =   ".312"
                                                       OR  ".334"
                           IF     SPA-NYUGAIKBN        =   "1"
                               MOVE    "9028"          TO  WRK-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                               (IDX-Z)
                           END-IF
                       END-IF
      *
                   WHEN    OTHER
      *                編集チェックへ
      *                薬評以外
                       IF      WRK-COM-YAKUHYO (IDX-Z) =   SPACE
                           PERFORM 10011-CHK-SEC
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    点滴の時、手技料の自動修正
           IF     (WRK-COM-SRYSYUKBN(01)   =   "330" )
           AND    (WRK-COM-YAKUHYO  (01)   =   SPACE)
               PERFORM 1002-TENTEKI-CHK-SEC
           END-IF
      *
      *    皮下注射の時、手技料の自動修正
           IF     (WRK-COM-SRYSYUKBN(01)   =   "310")
           AND    (WRK-COM-YAKUHYO  (01)   =   SPACE)
      *        皮下筋肉注射料変換選択
               IF     LNK-ORCSC-CHG310FLG     =   "1"
                   CONTINUE
               ELSE
                   PERFORM 1002-CHUSYA-CHK-SEC
               END-IF
           END-IF
      *
      *    種別のみの時、手技料の自動発生
           IF     (FLG-SYUGI           =   ZERO  )  AND
                  (WRK-ERRCD           =   SPACE )
           AND    (WRK-COM-YAKUHYO  (01)   =   SPACE)
      *        薬剤のみの時対象外
               IF      WRK-COM-SRYSYUKBN(01)(3:1)  =   "1"
                   CONTINUE
               ELSE
                   PERFORM 1003-SYUGIJIDOU-SEC
               END-IF
           END-IF
      *
           IF      IDX-SYU             >   ZERO
      *        自動加算を追加する
               PERFORM 1004-JIDO-KASAN-SEC
           END-IF
      *
      *!   IF      WRK-ERRCD(1:1)  NOT =   "K"
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *!   END-IF
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
      *        手技料
               WHEN    "1"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       IF      FLG-SYUGI           =   1
                           MOVE    "0019"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                               (IDX-Z)
                       END-IF
                       MOVE    1                   TO  FLG-SYUGI
      *                入力が有った時、自動加算を入力とする
                       MOVE    SPACE                TO  WRK-COM-AUTOKBN
                                                                (IDX-Z)
      *                手技料の位置セット
                       MOVE    IDX-Z               TO  IDX-SYU
      *!!
      *                中心静脈注射判定
                       IF      WRK-COM-INPUTCD (IDX-Z) =   CON-CHUJYO
                           MOVE    1               TO  FLG-SYUGI-CHU
                       END-IF 
      *!!
      *                中心静脈注射判定
                       IF      WRK-COM-INPUTCD (IDX-Z) =   CON-CHUJYO6
                           MOVE    1               TO  FLG-SYUGI-CHU6
                       END-IF 
      *!!
                   END-IF
      *            入院時の剤内容のチェック
                   IF     (WRK-COM-DATAKBN (IDX-Z) =   "2" ) AND
                          (SPA-NYUGAIKBN           =   "1"   )
                       PERFORM 1005-ZAI-CHK-SEC
                   END-IF
      *            外来の同時算定チェック
                   IF    ((SPA-HKNCOMBINUM     =   "9999" )  AND
                          (LNK-ORCSC-PG        =   "K02"  ))  OR
                         ( LNK-ORCSC-PG        =   "J04"   )
                   OR    ( LNK-ORCSC-PG        =   "KT01"  )
                       CONTINUE
                   ELSE
                       IF     (SPA-NYUGAIKBN           =   "2" ) AND
                              (WRK-COM-DATAKBN (IDX-Z) =   "1" ) AND
                              (WRK-COM-SRYSYUKBN(IDX-Z)
                                                       =   "320" OR
                                                           "330" OR
                                                           "340" )
                         AND  (WRK-COM-HKNCOMBI (IDX-Z)
                                                   NOT =   "9999")
                          PERFORM 1006-DOUJI-CHK-SEC
                       END-IF
                   END-IF
      *
      *        薬剤
               WHEN    "6"
      *            麻毒区分
                   IF      WRK-COM-MADOKUKBN (IDX-Z)   =   1
                       MOVE   1                TO  FLG-MADOKU
                   END-IF
      *            生物学的製剤区分
                   IF      WRK-COM-SEIBUTUKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-SEIBUTU
                   END-IF
      *            注射液量加算
                   COMPUTE WRK-CHUSYA      =  (WRK-COM-CSYYOURYO(IDX-Z)
                                           *   WRK-COM-SURYO  (IDX-Z))
                                           +   WRK-CHUSYA
      *            入院時の薬剤と手技料を分ける
                   IF    (SPA-NYUGAIKBN        =   "1" ) AND
                         (FLG-SYUGI            =   1   ) AND
                         (WRK-COM-SRYSYUKBN (IDX-Z)    =   "330" OR
                                                           "331")
                       MOVE    "9020"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *!!!!
      *            入院時の中心静脈注射と薬剤は分ける
                   IF    (SPA-NYUGAIKBN        =   "1" )   AND
                         (FLG-SYUGI-CHU        =   1   )   AND
                         (WRK-COM-HKNCOMBI (IDX-Z)
                                           NOT =   "9999")
                       MOVE    "9040"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *!!!!
      *            残量廃棄
                   PERFORM 1006-HAIKI-SYORI-SEC
      *
      *        器材
               WHEN    "7"
      *                フィルムの時エラー
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
                       MOVE    "0047"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *
      *            酸素・窒素はエラー
                   IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                       MOVE    "0039"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *!!!!
      *H27.1       入院時の中心静脈注射と器材は分ける
                   IF    (SPA-NYUGAIKBN        =   "1" )   AND
                         (FLG-SYUGI-CHU        =   1   )   AND
                         (WRK-COM-HKNCOMBI (IDX-Z)
                                           NOT =   "9999")
                       MOVE    "9042"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *!!!!
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    残量廃棄コメントの有無
      *****************************************************************
       1006-HAIKI-SYORI-SEC           SECTION.
      *
      *    アンプルの時、小数以下判定
           IF      WRK-COM-TANICD (IDX-Z)  =   "014" OR "022" OR "046"
               MOVE    WRK-COM-SURYO(IDX-Z)    TO  WRK-SURYO
               IF     (WRK-SURYO-S2                >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)    =   SPACE   )
                   MOVE    IDX-Z                   TO  IDX-HAI
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC             SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-HAI + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    IDX-HAI             TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
      *
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の剤内容チェック処理
      *****************************************************************
       1005-ZAI-CHK-SEC           SECTION.
      *
      *
           .
       1005-ZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の同時算定チェック処理
      *****************************************************************
       1006-DOUJI-CHK-SEC           SECTION.
      *
           EVALUATE    WRK-COM-SRYSYUKBN (IDX-Z)
               WHEN    "320"
      *            静脈注射
                   IF     (LNK-ACCT2-SRYCD-JYO2    =   SPACE )  AND
                          (LNK-ACCT2-SRYCD         =   SPACE )  AND
                          (LNK-ORCSC-TEN           =   SPACE )  AND
                          (LNK-ORCSC-JYO2          =   SPACE )
                       MOVE    "1"                 TO  LNK-ORCSC-JYO1
                   ELSE
                       MOVE    "0301"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               WHEN    "330"
      *            点滴注射
                   IF     (LNK-ACCT2-SRYCD-JYO1    =   SPACE )  AND
                          (LNK-ACCT2-SRYCD-JYO2    =   SPACE )  AND
                          (LNK-ORCSC-JYO1          =   SPACE )  AND
                          (LNK-ORCSC-JYO2          =   SPACE )
                       MOVE    "1"                 TO  LNK-ORCSC-TEN
                   ELSE
                       MOVE    "0302"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               WHEN    "340"
      *            中心静脈注射
                   IF      WRK-COM-INPUTCD (IDX-Z) =   CON-CHUJYO
                                                   OR  CON-CHUJYO6
                       IF     (LNK-ACCT2-SRYCD-JYO1    =   SPACE )  AND
                              (LNK-ACCT2-SRYCD-JYO2    =   SPACE )  AND
                              (LNK-ACCT2-SRYCD         =   SPACE )  AND
                              (LNK-ORCSC-TEN           =   SPACE )  AND
                              (LNK-ORCSC-JYO1          =   SPACE )  AND
                              (LNK-ORCSC-JYO2          =   SPACE )
                           MOVE    "1"             TO  LNK-ORCSC-JYO2
                       ELSE
                           MOVE    "0303"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                                (IDX-Z)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       1006-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料自動発生成処理
      *****************************************************************
       1003-SYUGIJIDOU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    外来の同日重複チェック
           IF     (SPA-NYUGAIKBN       =   "1"  )  OR
                 ((SPA-HKNCOMBINUM     =   "9999") AND
                  (LNK-ORCSC-PG        =   "K02" ))  OR
                 ( LNK-ORCSC-PG        =   "J04"   )
      *        外来まとめは変更なし
           OR    ( LNK-ORCSC-PG        =   "KT01"  )
               CONTINUE
           ELSE
               EVALUATE    WRK-COM-SRYSYUKBN (01)
      *            静脈注射
                   WHEN    "320"
                       IF     (LNK-ACCT2-SRYCD-JYO2    =   SPACE )  AND
                              (LNK-ACCT2-SRYCD         =   SPACE )  AND
                              (LNK-ORCSC-TEN           =   SPACE )  AND
                              (LNK-ORCSC-JYO2          =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "1"         TO  WRK-COM-SRYSYUKBN(01)
                                                            (3:1)
                           MOVE    "1"         TO  WRK-GMN-INPUTCD(01)
                                                            (4:1)
      *                    診療種別名称
                           PERFORM 10021-SRYKBN-MEI-SEC 
                       END-IF
      *            点滴注射
                   WHEN    "330"
                       IF     (LNK-ACCT2-SRYCD-JYO1    =   SPACE ) AND
                              (LNK-ACCT2-SRYCD-JYO2    =   SPACE ) AND
                              (LNK-ORCSC-TEN           =   SPACE ) AND
                              (LNK-ORCSC-JYO1          =   SPACE ) AND
                              (LNK-ORCSC-JYO2          =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "1"         TO  WRK-COM-SRYSYUKBN(01)
                                                            (3:1)
                           MOVE    "1"         TO  WRK-GMN-INPUTCD(01)
                                                            (4:1)
      *                    診療種別名称
                           PERFORM 10021-SRYKBN-MEI-SEC 
                       END-IF
               END-EVALUATE
           END-IF
      *
           EVALUATE    WRK-COM-SRYSYUKBN (01)
      *        皮下筋肉注射
               WHEN    "310"
                   MOVE    "130000510"         TO  WRK-SRYCD
      *        皮下筋肉注射
               WHEN    "312"
                   MOVE    "130000510"         TO  WRK-SRYCD
      *        静脈注射
               WHEN    "320"
                   MOVE    "130003510"         TO  WRK-SRYCD
      *        点滴注射
               WHEN    "330"
      *********    IF      LNK-ORCSC-NENREI-YY     <   6
      *                MOVE    "130003710"     TO  WRK-SRYCD
      *            ELSE
      *                MOVE    "130003810"     TO  WRK-SRYCD
      ***********  END-IF
      *            低い点数とする
                   MOVE    "130009310"         TO  WRK-SRYCD
      *            入院の時、発生しない
                   IF      SPA-NYUGAIKBN           =   "1"
                       MOVE    SPACE               TO  WRK-SRYCD
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  IDX-SYU
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    1                   TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    IDX-S           TO  IDX-SYU
                   ADD     1               TO  IDX-MAX
               END-IF
           END-IF
      *
      *    同日２回目警告
           IF     (LNK-ACCT2-SRYCD NOT =   SPACE   )  AND
                  (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-SRYSYUKBN (01)  =   "330")
               IF     (WRK-COM-KZMFLG (IDX-SYU)    =   SPACE )
                  AND (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K006"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-SYU)
                   MOVE    IDX-SYU             TO  LNK-ORCSC-KEI-IDX
               END-IF
           END-IF
      *
           .
       1003-SYUGIJIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴の時、液料により手技料を変更
      *****************************************************************
       1002-TENTEKI-CHK-SEC                  SECTION.
      *
      *    同日２回目以降は変更なし
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                       =   "099303301")
               COMPUTE WRK-CHUSYA      =   LNK-ACCT2-CSYYOURYO
                                       +   WRK-CHUSYA
           END-IF
      *
      *    液量により、手技料変更
           MOVE    "130009310"         TO  WRK-SRYCD
      *    入院の時なし
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPACE               TO  WRK-SRYCD
           END-IF
      *
           IF      LNK-ORCSC-NENREI-YY     <   6
      *        ６歳未満で　100ml 以上
               IF      WRK-CHUSYA      >=  100
                   MOVE    "130003710"     TO  WRK-SRYCD
               END-IF
           ELSE
      *        ６歳以上で　500ml 以上
               IF      WRK-CHUSYA      >=  500
                   MOVE    "130003810"     TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    同日２回目以降は変更なし
      *****IF     (IDX-SYU             >   ZERO    )  AND
      *           (WRK-COM-INPUTCD(IDX-SYU)
      *                                =   "099303301")
      *        IF      WRK-SRYCD       NOT =   LNK-ACCT2-SRYCD
      *            IF      WRK-COM-KZMFLG (IDX-SYU)    =   SPACE
      *               AND (LNK-ORCSC-KEI-IDX        =   ZERO )
      *                MOVE    "K006"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-KZMFLG
      *                                                       (IDX-SYU)
      *                MOVE    IDX-SYU         TO  LNK-ORCSC-KEI-IDX
      *            END-IF
      *        END-IF
      *        GO    TO      1002-TENTEKI-CHK-EXT
      *****END-IF
      *
           IF      WRK-SRYCD           =   SPACE
               GO    TO      1002-TENTEKI-CHK-EXT
           END-IF
      *
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                   NOT =   WRK-SRYCD )
               MOVE    IDX-SYU             TO  IDX-T
               MOVE    1                   TO  FLG-ARI
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *            診療種別再編集
                   IF      WRK-COM-SRYSYUKBN(IDX-SYU) 
                                           NOT =   WRK-COM-SRYSYUKBN
                                                             (01)
                       MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                             (01)
                       MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-GMN-INPUTCD
                                                            (01)
                                                            (2:3)
      *
                       PERFORM VARYING     IDX     FROM    1   BY  1
                                UNTIL     (IDX         >   SPA-MAX-LINE)
                                      OR  (WRK-GMN-INPUTCD (IDX)
                                                      =   SPACE )
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                            (IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-IF
      *
      *    同日２回目警告
           IF     (LNK-ACCT2-SRYCD NOT =   SPACE   )  AND
                  (IDX-SYU             >   ZERO    )
               IF     (WRK-COM-KZMFLG (IDX-SYU)    =   SPACE )
                  AND (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K006"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-SYU)
                   MOVE    IDX-SYU             TO  LNK-ORCSC-KEI-IDX
               END-IF
           END-IF
           .
       1002-TENTEKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    皮下筋の時、液料により手技料を変更
      *****************************************************************
       1002-CHUSYA-CHK-SEC                  SECTION.
      *
      *    液量が、5ML以上の時、静脈注射へ変更
           MOVE    SPACE               TO  WRK-SRYCD
           IF      WRK-CHUSYA          >=  5
               MOVE    "130003510"     TO  WRK-SRYCD
           END-IF
      *
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                   NOT =   WRK-SRYCD )
      *        皮下筋肉注射の時のみ
             AND  (WRK-COM-INPUTCD(IDX-SYU)
                                       =   "130000510" )
               MOVE    IDX-SYU             TO  IDX-T
               MOVE    1                   TO  FLG-ARI
               MOVE    1                   TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *            診療種別再編集
                   IF      WRK-COM-SRYSYUKBN(IDX-SYU) 
                                           NOT =   WRK-COM-SRYSYUKBN
                                                             (01)
                       IF      WRK-GMN-INPUTCD(01)(1:1)   =   "."
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                             (01)
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-GMN-INPUTCD
                                                            (01)(2:3)
      *                    診療種別名称
                           PERFORM 10021-SRYKBN-MEI-SEC 
                       END-IF
      *
                       PERFORM VARYING     IDX     FROM    1   BY  1
                                UNTIL     (IDX         >   SPA-MAX-LINE)
                                       OR  (WRK-GMN-INPUTCD (IDX)
                                                      =   SPACE )
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                            (IDX)
                           MOVE    WRK-COM-TNSSRYKBN(IDX-SYU)
                                               TO  WRK-COM-SRYKBN
                                                            (IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       1002-CHUSYA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別名称処理
      *****************************************************************
       10021-SRYKBN-MEI-SEC           SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                   WHEN    WRK-COM-SRYSYUKBN(1)
                                               =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH
      *
           MOVE    WRK-SRYSYUKBNMEI    TO  WRK-GMN-MEISYO (01)
           .
       10021-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算作成処理
      *****************************************************************
       1004-JIDO-KASAN-SEC           SECTION.
      *
      *    手技料の直下に追加する
           MOVE    IDX-SYU             TO  IDX-S
      *
      *    年齢加算セット処理
           PERFORM 1007-NYUYOJI-SET-SEC
      *
      *    生物学的製剤
           IF      FLG-SEIBUTU         =   1
               MOVE    "130000110"         TO  WRK-SRYCD
      *        加算チェック処理
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
      *
      *    麻毒加算
           IF      FLG-MADOKU          =   1
               MOVE    "130000310"         TO  WRK-SRYCD
      *        加算チェック処理
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
           .
       1004-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       1007-NYUYOJI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-SYU IDX2)
                                           =   SPACE  OR  ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-SYU IDX2)
                                           TO  WRK-SRYCD
               MOVE    IDX-SYU             TO  IDX-S
      *        加算チェック処理
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       1100-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1100-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       1100-JIDOTUIKA-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
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
               MOVE    HZN-SCR-REC     TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S   -   1
               END-IF
               GO      TO      1100-JIDOTUIKA-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ADD
      *    手技料の変更の時、診療区分のセットはしない
           IF     (FLG-SYUGIADD         =   1    )  OR
                  (IDX-S               =   1  OR  ZERO )
              GO      TO      1100-JIDOTUIKA-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           .
        1100-JIDOTUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
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
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                                OR  "*"
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
                   WHEN    "*"
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
      *    割合計算加算
           PERFORM 1006-KASAN-SET-SEC
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *H31.3
      *    桁数チェック
      *    他の診療と合わせる為、2018/4から
           IF      SPA-SRYYMD          >=  "20180401"
      *
           IF      LNK-SC90-ERRCD  NOT =   SPACE
               MOVE    LNK-SC90-ERRCD      TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-MAX)
           END-IF
      *        最大７桁
           IF     (WRK-COM-TENSU  (IDX-MAX)    *   LNK-ORCSC-KAISU)
                                           >   9999999
               MOVE    "0249"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-MAX)
           END-IF
      *
           END-IF
      *
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
      *    セット用合計点数
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
      *    手技点数計算（１剤１手技料とする）
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *****************************************************************
      *    手技点数計算処理
      *****************************************************************
       100211-SYUGITEN-KEI-SEC              SECTION.
      *
           IF      WRK-COM-TENSIKIBETU (IDX-Z)     =   5
                                                   OR  6
               CONTINUE
           ELSE
      *        通則加算対象外区分判定
               IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "1"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-GAI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-TENSU-KEI
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-GEN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        通則対象の点数を所定点数とする
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-GEN
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-TENSU-KEI       =   WRK-TENSU-KEI
                                       +   WRK-TENSUKEI-G
                                       -   WRK-TENSUKEI-GEN
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-TENSU-KEI
                                       +   WRK-TENSU-GAI
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
     *
      *****************************************************************
      *    点滴注射加算作成処理
      *****************************************************************
      *保留
       510242-TEN-KASAN-SEC           SECTION.
      *
      *    液量により、手技料追加
           MOVE    SPACE               TO  WRK-SRYCD
           IF      LNK-ORCSC-NENREI-YY   <   6
      *        ６歳未満で　100ml 以上
               IF      WRK-CHUSYA      >=  100
                   MOVE    "130003710"     TO  WRK-SRYCD
               END-IF
           ELSE
      *        ６歳以上で　500ml 以上
               IF      WRK-CHUSYA      >=  500
                   MOVE    "130003810"     TO  WRK-SRYCD
               END-IF
           END-IF
      *
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
           .
       510242-TEN-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    ZERO                TO  FLG-TENTEKI
           MOVE    ZERO                TO  FLG-DOUCHU
      *
           MOVE    ZERO                TO  FLG-SYUJUTU
           MOVE    ZERO                TO  IDX-CHU
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE)
             IF   (WRK-COM-HKNCOMBI (IDX) NOT =   "9999")
      *        点滴は１回のみとする（外来のみ）
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
      **              (WRK-COM-SRYSYUKBN(IDX)  NOT =   "950"  AND
      **                                               "960") AND
                      (WRK-COM-SRYKBN   (IDX)  NOT =   "95"   AND
                                                       "96" ) AND
                      (WRK-COM-INPUTCD  (IDX)      =   "130009310" OR
                                                       "130003710" OR
                                                       "130003810")
                   IF      FLG-TENTEKI         =   1
                       MOVE    "0057"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-CUR(IDX)
                   END-IF
                   MOVE    1               TO  FLG-TENTEKI
               END-IF
      *        動脈注射は１回のみとする
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
      ***             (WRK-COM-SRYSYUKBN(IDX)  NOT =   "950"  AND
      ***                                              "960") AND
                      (WRK-COM-SRYKBN   (IDX)  NOT =   "95"   AND
                                                       "96" ) AND
                      (WRK-COM-INPUTCD  (IDX)      =   "130006110" OR
                                                       "130006210")
                   IF      FLG-DOUCHU          =   1
                       MOVE    "0157"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-CUR(IDX)
                   END-IF
                   MOVE    1               TO  FLG-DOUCHU
               END-IF
      *        手術がある時、点滴手技料は算定できない
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
                      (WRK-COM-SRYSYUKBN(IDX)      =   "500" )  AND
                      (WRK-COM-INPUTCD  (IDX)(1:1) =   "1"   )  AND
                      (WRK-COM-DATAKBN  (IDX)      =   "1"   )
                   MOVE    1                   TO  FLG-SYUJUTU
               END-IF
      *        注射の判定（外来のみ）
               IF     (SPA-NYUGAIKBN           =   "2"   )  AND
                      (WRK-COM-SRYKBN (IDX)    =   "31" OR
                                                   "32" OR
                                                   "33"  )  AND
                      (WRK-COM-INPUTCD(IDX)(1:1)  =  "1" )  AND
                      (WRK-COM-DATAKBN(IDX)    =   "1"   )
      *          警告表示なしの場合のみ
                 AND  (WRK-COM-KZMFLG (IDX)    =   SPACE )
                   IF      IDX-CHU             =   ZERO
                       MOVE    IDX                 TO  IDX-CHU
                   END-IF
               END-IF
      *****    点滴は１回のみとする
      *        IF     (WRK-GMN-INPUTCD (IDX)(1:1)   =   "."  )  AND
      *               (WRK-COM-SRYSYUKBN (IDX)      =   "330")
      *            IF      FLG-TENTEKI         =   1
      *                MOVE    "0057"              TO  SPA-ERRCD
      *                MOVE    "1"                 TO  WRK-COM-CUR(IDX)
      *            END-IF
      **           MOVE    1               TO  FLG-TENTEKI
      ******   END-IF
             END-IF
           END-PERFORM
      *----(01.00.01) LINE ADD START ----------------------------------
      *
      *    手術がある時、注射の手技料は警告とする
           IF     (FLG-SYUJUTU         =   1  )  AND
                  (IDX-CHU             >   ZERO )
               IF      WRK-COM-KZMFLG (IDX-CHU)    =   SPACE
                   MOVE    "K140"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-KZMFLG
                                                           (IDX-CHU)
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-CHU)
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
      *    入院の時、自動発生する
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、点滴液量チェック
               PERFORM 3001-GAIRAI-TENTEKI-SEC
           ELSE
      *        入院の時、点滴手技料自動発生
               PERFORM 3002-NYUIN-TENTEKI-SEC
           END-IF
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の時、点滴手技料自動発生処理
      *****************************************************************
       3002-NYUIN-TENTEKI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           INITIALIZE                  TBL-NISUU-AREA
           INITIALIZE                  TBL-IVH-NISUU-AREA
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-MAX-LINE )  OR
                             ((WRK-GMN-INPUTCD(IDX)  =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX)  =   SPACE ))
      *    薬評以外
             IF      WRK-COM-YAKUHYO (IDX) =   "1"
               CONTINUE
             ELSE
      *        点滴の時のみ
               IF      WRK-COM-SRYSYUKBN (IDX)     =   "330"
                                                   OR  "332"
                   MOVE    1                   TO  FLG-OK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    WRK-COM-NYUINDAY(IDX IDY)
                                                   TO  TBL-DAY(IDY)
                        IF      WRK-COM-SRYSYUKBN (IDX)     =   "330"
      *                    液量集計
                           IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
                               COMPUTE TBL-CHUSYA (IDY)
                                               = (WRK-COM-CSYYOURYO(IDX)
                                               *   WRK-COM-SURYO  (IDX))
                                               +   TBL-CHUSYA (IDY)
      *                        生物学的製剤区分
                               IF      WRK-COM-SEIBUTUKBN(IDX)   =   1
                                   MOVE    1           TO  TBL-SEIBUTU
                                                                   (IDY)
                               END-IF
      *                        麻毒
                               IF      WRK-COM-MADOKUKBN(IDX)  =   1
                                   MOVE    1           TO  TBL-MADOKU
                                                                   (IDY)
                               END-IF
                           END-IF
                        ELSE
      *                 手術以外
      *                    液量集計
                           IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
                               COMPUTE TBL-CHUSYA-2 (IDY)
                                               = (WRK-COM-CSYYOURYO(IDX)
                                               *   WRK-COM-SURYO  (IDX))
                                               +   TBL-CHUSYA-2 (IDY)
      *                        生物学的製剤区分
                               IF      WRK-COM-SEIBUTUKBN(IDX)   =   1
                                   MOVE    1           TO  TBL-SEIBUTU-2
                                                                   (IDY)
                               END-IF
      *                        麻毒
                               IF      WRK-COM-MADOKUKBN(IDX)  =   1
                                   MOVE    1           TO  TBL-MADOKU-2
                                                                   (IDY)
                               END-IF
                           END-IF
                        END-IF
      *                    手技
                           IF     (WRK-COM-INPUTCD(IDX)(1:1)  =   "1")
                           AND    (WRK-COM-DATAKBN(IDX)       =   "1")
                               MOVE    WRK-COM-INPUTCD(IDX)
                                               TO  TBL-TEN-SRYCD(IDY)
                               MOVE    IDX     TO  TBL-TEN-IDX  (IDY)
                           END-IF
      *                    麻毒
                           IF      WRK-COM-INPUTCD(IDX)  =   "130000310"
                               MOVE    IDX     TO  TBL-MADOKU-IDX (IDY)
                           END-IF
      *                    生物学的製剤
                           IF      WRK-COM-INPUTCD(IDX)  =   "130000110"
                               MOVE    IDX     TO  TBL-SEIBUTU-IDX (IDY)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        中心静脈の薬剤
               IF      WRK-COM-SRYSYUKBN (IDX)     =   "350"
                                                   OR  "352"
                   MOVE    1                   TO  FLG-OK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    WRK-COM-NYUINDAY(IDX IDY)
                                                   TO  TBL-IVH-DAY(IDY)
                         IF      WRK-COM-SRYSYUKBN (IDX)     =   "350"
      *                    液量集計
                           IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
                               COMPUTE TBL-IVH-CHUSYA (IDY)
                                               =   WRK-COM-SURYO  (IDX)
                                               +   TBL-IVH-CHUSYA (IDY)
                               IF      TBL-IVH-CHUIDX (IDY)    =   ZERO
                                   MOVE    IDX       TO  TBL-IVH-CHUIDX
                                                                 (IDY)
                               END-IF
      *                        生物学的製剤区分
                               IF      WRK-COM-SEIBUTUKBN(IDX)   =   1
                                   MOVE    1       TO  TBL-IVH-SEIBUTU
                                                                   (IDY)
                               END-IF
      *                        麻毒
                               IF      WRK-COM-MADOKUKBN(IDX)  =   1
                                   MOVE    1       TO  TBL-IVH-MADOKU
                                                                   (IDY)
                               END-IF
                           END-IF
                         ELSE
      *                    手術以外
                           IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
                               COMPUTE TBL-IVH-CHUSYA-2 (IDY)
                                               =   WRK-COM-SURYO  (IDX)
                                               +   TBL-IVH-CHUSYA-2(IDY)
                               IF      TBL-IVH-CHUIDX (IDY)    =   ZERO
                                   MOVE    IDX       TO  TBL-IVH-CHUIDX
                                                                 (IDY)
                               END-IF
      *                        生物学的製剤区分
                               IF      WRK-COM-SEIBUTUKBN(IDX)   =   1
                                   MOVE    1       TO  TBL-IVH-SEIBUTU-2
                                                                   (IDY)
                               END-IF
      *                        麻毒
                               IF      WRK-COM-MADOKUKBN(IDX)  =   1
                                   MOVE    1       TO  TBL-IVH-MADOKU-2
                                                                   (IDY)
                               END-IF
                           END-IF
                         END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        中心静脈の時
               IF      WRK-COM-INPUTCD (IDX)   =   CON-CHUJYO
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    1                   TO  FLG-OK
      *                    手技
                           MOVE    WRK-COM-INPUTCD(IDX)
                                               TO  TBL-IVH-SRYCD(IDY)
                           MOVE    IDX         TO  TBL-IVH-IDX  (IDY)
                       END-IF
                   END-PERFORM
               END-IF
      *H28.6
      *        中心静脈（カテーテル）の時
               IF      WRK-COM-INPUTCD (IDX)   =   CON-CHUJYO6
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    1                   TO  FLG-OK
      *                    手技
                           MOVE    WRK-COM-INPUTCD(IDX)
                                               TO  TBL-IVH6-SRYCD(IDY)
                           MOVE    IDX         TO  TBL-IVH6-IDX  (IDY)
                       END-IF
                   END-PERFORM
               END-IF
      *        手術
               IF     (WRK-COM-SRYSYUKBN(IDX)  =   "500" )  AND
                      (WRK-COM-INPUTCD (IDX)(1:1)  =   "1")
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    WRK-COM-NYUINDAY(IDX IDY)
                                                   TO  TBL-SYU-DAY(IDY)
                           MOVE    IDX             TO  TBL-SYU-IDX(IDY)
                           MOVE    1                   TO  FLG-OK
                       END-IF
                   END-PERFORM
               END-IF
      *        画像診断の点滴
               IF     (WRK-COM-SRYSYUKBN(IDX)  =   "731" )  AND
                      (WRK-COM-TNSSRYSYUKBN(IDX)   =   "330" )  AND
                      (WRK-COM-DATAKBN  (IDX)  =   "1"   )
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF      WRK-COM-NYUINDAY (IDX IDY)  >   ZERO
                           MOVE    WRK-COM-NYUINDAY(IDX IDY)
                                                   TO  TBL-GAZ-DAY(IDY)
                           MOVE    IDX             TO  TBL-GAZ-IDX(IDY)
                           MOVE    1                   TO  FLG-OK
                       END-IF
                   END-PERFORM
               END-IF
      *
            END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               CONTINUE
           ELSE
      *        当日のすべての点滴液量を集計する
               PERFORM 3001-EKIRYO-SYUKEI-SEC
      *        当日のすべての手術の有無を判定
               PERFORM 3003-SYUJYU-CHK-SEC
      *        当日のすべての画像点滴手技の有無を判定
               PERFORM 3003-GAZOU-CHK-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDXS
      *
      *    手術に関係ない点滴の編集
           IF     (FLG-OK              =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               CONTINUE
           ELSE
               PERFORM 3007-TENTEKI-HEN-SEC
           END-IF
      *
      *    中心静脈発生チェック
           IF     (FLG-OK              =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               CONTINUE
           ELSE
               PERFORM 3004-IVH-AUTO-SEC
           END-IF
      *
      *    点滴注射発生チェック
           IF     (FLG-OK              =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               CONTINUE
           ELSE
               PERFORM 3005-TENTEKI-AUTO-SEC
           END-IF
      *
           IF     (FLG-OK              =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               CONTINUE
           ELSE
               PERFORM 30021-NYUIN-AUTO-SEC
           END-IF
      *
           .
       3002-NYUIN-TENTEKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院手技料自動発生処理
      *****************************************************************
       30021-NYUIN-AUTO-SEC         SECTION.
      *
      *    点滴手技料自動発生
           INITIALIZE                  WRK-NISUU-HEN-AREA
           MOVE    SPACE               TO  WRK-TEN-SRYCD
           MOVE    SPACE               TO  WRK-TEN-SRYCD2
           MOVE    SPACE               TO  WRK-TEN-AUTO
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-SEIBUTU
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
               IF      TBL-AUTO-SRYCD (IDY)    NOT =   SPACE
                   IF     (WRK-TEN-SRYCD           =   SPACE )  OR
                          (TBL-AUTO-SRYCD (IDY)    =   WRK-TEN-SRYCD)
                       MOVE    TBL-AUTO-SRYCD (IDY)    TO  WRK-TEN-SRYCD
                       MOVE    1                   TO  WRK-NIDAY (IDY)
                   ELSE
                       IF      WRK-TEN-SRYCD   NOT =   SPACE
                           MOVE    "330"           TO  WRK-SRYSYUKBN
                           MOVE    "33"            TO  WRK-SRYKBN
                           PERFORM 3002-AUTO-HENSYU-SEC
                           INITIALIZE               WRK-NISUU-HEN-AREA
      *
                           MOVE    TBL-AUTO-SRYCD (IDY)
                                                   TO  WRK-TEN-SRYCD
                           MOVE    1               TO  WRK-NIDAY (IDY)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-TEN-SRYCD   NOT =   SPACE
                MOVE    "330"           TO  WRK-SRYSYUKBN
                MOVE    "33"            TO  WRK-SRYKBN
                PERFORM 3002-AUTO-HENSYU-SEC
           END-IF
      *
      *    生物学的製剤加算自動発生
           INITIALIZE                  WRK-NISUU-HEN-AREA
           MOVE    SPACE               TO  WRK-TEN-SRYCD
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
               IF     (TBL-AUTO-SRYCD (IDY)    NOT =   SPACE ) OR
                      (TBL-TEN-SRYCD  (IDY)    NOT =   SPACE ) OR
                      (TBL-IVH-SRYCD  (IDY)    NOT =   SPACE ) OR
                      (TBL-IVH-SRYCD  (IDY)    NOT =   SPACE ) OR
                      (TBL-IVHA-SRYCD (IDY)    NOT =   SPACE )
                 OR   (TBL-IVH6-SRYCD (IDY)    NOT =   SPACE )
                   IF    ((TBL-SEIBUTU    (IDY)    =   1    ) OR
                          (TBL-IVH-SEIBUTU(IDY)    =   1    )) AND
                          (TBL-SEIBUTU-IDX(IDY)    =   ZERO ) AND
                          (TBL-SEIBUTU-ARI(IDY)    =   SPACE)
                       MOVE    "130000110"         TO  WRK-TEN-SRYCD
                       MOVE    1                   TO  WRK-NIDAY (IDY)
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-TEN-SRYCD   NOT =   SPACE
                MOVE    "330"           TO  WRK-SRYSYUKBN
                MOVE    "33"            TO  WRK-SRYKBN
               PERFORM 3002-AUTO-HENSYU-SEC
           END-IF
      *
      *    麻毒加算自動発生
           INITIALIZE                  WRK-NISUU-HEN-AREA
           MOVE    SPACE               TO  WRK-TEN-SRYCD
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
               IF     (TBL-AUTO-SRYCD (IDY)    NOT =   SPACE ) OR
                      (TBL-TEN-SRYCD  (IDY)    NOT =   SPACE ) OR
                      (TBL-IVH-SRYCD  (IDY)    NOT =   SPACE ) OR
                      (TBL-IVHA-SRYCD (IDY)    NOT =   SPACE )
                 OR   (TBL-IVH6-SRYCD (IDY)    NOT =   SPACE )
                   IF    ((TBL-MADOKU     (IDY)    =   1    ) OR
                          (TBL-IVH-MADOKU (IDY)    =   1    ))  AND
                          (TBL-MADOKU-IDX (IDY)    =   ZERO ) AND
                          (TBL-MADOKU-ARI (IDY)    =   SPACE)
                       MOVE    "130000310"         TO  WRK-TEN-SRYCD
                       MOVE    1                   TO  WRK-NIDAY (IDY)
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-TEN-SRYCD   NOT =   SPACE
                MOVE    "330"           TO  WRK-SRYSYUKBN
                MOVE    "33"            TO  WRK-SRYKBN
                PERFORM 3002-AUTO-HENSYU-SEC
           END-IF
      *
           .
       30021-NYUIN-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の時、点滴液量チェック処理
      *****************************************************************
       3001-GAIRAI-TENTEKI-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-CHU
           MOVE    ZERO                TO  WRK-CHUSYA
           MOVE    ZERO                TO  FLG-SYUJUTU
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE)
             IF   (WRK-COM-HKNCOMBI (IDX) NOT =   "9999")
             AND  (WRK-COM-YAKUHYO  (IDX) NOT =   "1"   )
      *H27.8 包括剤対象外
             AND  (WRK-COM-HOUKATU-ZAI(IDX)   =   ZERO  )
      *
      *        点滴は１回のみとする（外来のみ）
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
      **              (WRK-COM-SRYSYUKBN(IDX)  NOT =   "950"  AND
      **                                               "960") AND
                      (WRK-COM-SRYKBN   (IDX)  NOT =   "95"   AND
                                                       "96" ) AND
                      (WRK-COM-INPUTCD  (IDX)      =   "130009310" OR
                                                       "130003710" OR
                                                       "130003810")
                   MOVE    IDX             TO  IDX-SYU
                   IF      IDX-CHU         =   ZERO
                       MOVE    IDX             TO  IDX-CHU
                   END-IF
               END-IF
      *        注射液量加算
               IF     (SPA-NYUGAIKBN           =   "2"   )  AND
                      (WRK-COM-SRYSYUKBN(IDX)  =   "330"
                                               OR  "331"  )  AND
                      (WRK-COM-INPUTCD(IDX)(1:1)   =  "6")
                   COMPUTE WRK-CHUSYA      =  (WRK-COM-CSYYOURYO(IDX)
                                           *   WRK-COM-SURYO   (IDX))
                                           +   WRK-CHUSYA
                   IF      IDX-CHU         =   ZERO
                       MOVE    IDX             TO  IDX-CHU
                   END-IF
               END-IF
      *        手術
               IF     (SPA-NYUGAIKBN               =   "2"   )  AND
                      (WRK-COM-SRYSYUKBN(IDX)      =   "500" )  AND
                      (WRK-COM-INPUTCD  (IDX)(1:1) =   "1"   )  AND
                      (WRK-COM-DATAKBN  (IDX)      =   "1"   )
                   MOVE    1                   TO  FLG-SYUJUTU
               END-IF
      *
             END-IF
           END-PERFORM
      *    今回と前回の両方に手技がある場合は、チェックしない
      *    手術がある時対象外
           IF    ((IDX-SYU             >   ZERO ) AND
                  (LNK-ACCT2-SRYCD NOT =   SPACE) )  OR
                  (FLG-SYUJUTU         =   1      )  OR
                  (WRK-CHUSYA          =   ZERO   )  OR
      *           前回に点滴がない時
                 ((LNK-ACCT2-CSYYOURYO =   ZERO   ) AND
                  (LNK-ACCT2-SRYCD     =   SPACE  ))
               CONTINUE
           ELSE
               PERFORM 30011-GAIRAI-TEN-CHK-SEC
           END-IF
           .
       3001-GAIRAI-TENTEKI-EXT.
           EXIT.
      *****************************************************************
      *    外来点滴液量チェック処理
      *****************************************************************
       30011-GAIRAI-TEN-CHK-SEC         SECTION.
      *
      *    外来の当日の合計液量判定
           COMPUTE WRK-CHUSYA      =   WRK-CHUSYA
                                   +   LNK-ACCT2-CSYYOURYO
           IF      LNK-ORCSC-NENREI-YY     <   6
               MOVE    "130003710"     TO  WRK-SRYCD
               MOVE    100             TO  WRK-MAX-RYOU
           ELSE
               MOVE    "130003810"     TO  WRK-SRYCD
               MOVE    500             TO  WRK-MAX-RYOU
           END-IF
           IF      WRK-CHUSYA          >=  WRK-MAX-RYOU
               IF    ((IDX-SYU             >   ZERO ) AND
                      (WRK-COM-INPUTCD(IDX-SYU)
                                           =   WRK-SRYCD))  OR
                     ((LNK-ACCT2-SRYCD NOT =   SPACE    ) AND
                      (LNK-ACCT2-SRYCD     =   WRK-SRYCD))
                   CONTINUE
               ELSE
                   IF      (LNK-ORCSC-KEI-IDX  =   ZERO ) AND
                           (WRK-COM-KZMFLG (IDX-CHU)    =   SPACE )
                       MOVE    "K009"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-CHU)
                       MOVE    IDX-CHU             TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
           .
       30011-GAIRAI-TEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点滴液量集計処理
      *****************************************************************
       3001-EKIRYO-SYUKEI-SEC         SECTION.
      *
      *    同日再入院対応
           MOVE    ZERO                TO  IDX-D
           MOVE    1                   TO  IDXX
           IF     (LNK-ORCSC-SYORIFLG =   "1"    )
      *        訂正の時のみ対象回数変更
               IF     (SPA-K02N-DOUKBN     =   "1"  )
                   MOVE    SPA-K02N-DOUDAY     TO  IDX-D
                   MOVE    2                   TO  IDXX
               ELSE
      *        診療日付＝同日再入院日
                   IF     (SPA-SRYYMD             =   SPA-K02N-TAIINYMD)
                       MOVE    SPA-K02N-TAIINYMD(7:2)  TO  IDX-D
                       MOVE    3                   TO  IDXX
                   END-IF
               END-IF
           END-IF
      *!!!
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "33"                TO  ACCT-SRYKBN
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
           PERFORM UNTIL       (FLG-SRYACCT        =   1       )
                           OR  (SPA-ERRCD      NOT =   SPACE )
               MOVE    ZERO                TO  FLG-ZAIARI
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   31
                   IF      ACCT-DAY (1 IDY)    >   ZERO
      **********       IF     (TBL-DAY (IDY)   >   ZERO )  OR
      *                       (TBL-IVH-DAY(IDY)    >   ZERO )  OR
      *                       (TBL-SYU-DAY(IDY)    >   ZERO )  OR
      *******                 (TBL-GAZ-DAY(IDY)    >   ZERO )
                           MOVE    1           TO  FLG-ZAIARI
      ***              END-IF
                   END-IF
               END-PERFORM
      *        訂正の時、集計なし
               IF     (LNK-ORCSC-SYORIFLG =   "1"    )  AND
                      (ACCT-SRYKA         =   LNK-ORCSC-OLD-SRYKA) AND
                      (ACCT-HKNCOMBI      =   LNK-ORCSC-OLD-HKNCOMBI)
                   MOVE    ZERO               TO  FLG-ZAIARI
                   IF     (IDX-D              >   ZERO )  AND
                          (ACCT-DAY (IDXX IDX-D)  >  ZERO )
                       MOVE    1                  TO  FLG-ZAIARI
                   END-IF
               END-IF
      *        包括まとめは対象外
               IF      ACCT-HKNCOMBI           =   "9999"
                   MOVE    ZERO               TO  FLG-ZAIARI
               END-IF
      *        薬評は対象外
               IF      ACCT-ZAIKBN             =   1
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        科・保険判定
               IF     (ACCT-SRYKA         =   SPA-SRYKA       ) AND
                      (ACCT-HKNCOMBI      =   SPA-HKNCOMBINUM )
                   MOVE    1                  TO  FLG-OUT-OK
               ELSE
                   MOVE    ZERO               TO  FLG-OUT-OK
               END-IF
      *
      *        回数があるもの
               IF      FLG-ZAIARI          =   1
      *            診療行為より液量集計
                   PERFORM 30011-SRYACT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3001-EKIRYO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       30011-SRYACT-HEN-SEC            SECTION.
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
           MOVE    ZERO                TO  WRK-EKIRYOU
           MOVE    ZERO                TO  WRK-GOK-RYOU
           MOVE    SPACE               TO  WRK-TEN-SRYCD
           MOVE    SPACE               TO  WRK-TEN-SRYCD1
           MOVE    SPACE               TO  WRK-TEN-SRYCD2
           MOVE    SPACE               TO  WRK-TEN-SRYCD3
           MOVE    SPACE               TO  WRK-TEN-SRYCD6
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-SEIBUTU
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "6" )  AND
                          (SRY-SRYSYUKBN           =   "330"
                                                   OR  "332"
                                                   OR  "350"
                                                   OR  "353" )
      *                点数マスタより、液量取得
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           IF      TNS-CSYYOURYO   =   ZERO
                               MOVE    SRY-SRYSURYO(IDX2)
                                                       TO  WRK-EKIRYOU
                           ELSE
                               COMPUTE WRK-EKIRYOU     =   TNS-CSYYOURYO
                                                       *   SRY-SRYSURYO
                                                                  (IDX2)
                           END-IF
                           ADD     WRK-EKIRYOU         TO  WRK-GOK-RYOU
      *                    生物学的製剤区分
                           IF      TNS-SEIBUTUGAKUKBN  =   1
                               MOVE    1               TO  FLG-SEIBUTU
                           END-IF
      *                    麻毒
                           IF      TNS-MADOKUKBN       =   1
                               MOVE    1               TO  FLG-MADOKU
                           END-IF
                       END-IF
                   END-IF
      *            手技料（点滴）
                   IF      SRY-SRYCD (IDX2)        =   "130003710"
                                                   OR  "130003810"
                                                   OR  "130700310"
                                                   OR  "130700410"
                                                   OR  "130702910"
                                                   OR  "130701010"
                                                   OR  "130702810"
                       MOVE    SRY-SRYCD (IDX2)    TO  WRK-TEN-SRYCD
                       MOVE    SRY-AUTOKBN (IDX2)  TO  WRK-TEN-AUTO
                   END-IF
      *            手技料（中心静脈）
                   IF      SRY-SRYCD (IDX2)        =   CON-CHUJYO
                       MOVE    SRY-SRYCD (IDX2)    TO  WRK-TEN-SRYCD1
                   END-IF
      *            手技料（麻毒）
                   IF      SRY-SRYCD (IDX2)        =   "130000310"
                       MOVE    SRY-SRYCD (IDX2)    TO  WRK-TEN-SRYCD3
                   END-IF
      *            手技料（生物学的）
                   IF      SRY-SRYCD (IDX2)        =   "130000110"
                       MOVE    SRY-SRYCD (IDX2)    TO  WRK-TEN-SRYCD2
                   END-IF
      *H28.4       手技料（中心静脈カテーテル）
                   IF      SRY-SRYCD (IDX2)        =   CON-CHUJYO6
                       MOVE    SRY-SRYCD (IDX2)    TO  WRK-TEN-SRYCD6
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
      *    日別の液量集計
           IF      WRK-SRYSYUKBN       =   "350"
                                       OR  "353"
               PERFORM 300112-IVH-TBL-SEC
           ELSE
               PERFORM 300111-TENTEKI-TBL-SEC
           END-IF
           .
       30011-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     点滴の日別の液量集計処理
      *****************************************************************
       300111-TENTEKI-TBL-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
      *ver.4.7
      *同日再入院日
               IF      IDX-D           =   IDY
                   MOVE    ACCT-DAY (IDXX IDY)     TO  WRK-D-DAY
               ELSE
                   MOVE    ACCT-DAY (1    IDY)     TO  WRK-D-DAY
               END-IF
      *
      ******   IF     (ACCT-DAY (1 IDY)    >   ZERO )  AND
               IF     (WRK-D-DAY           >   ZERO )  AND
                      (TBL-DAY  (IDY)      >   ZERO )
                 IF      WRK-SRYSYUKBN       =   "330"
                   ADD     WRK-GOK-RYOU    TO  TBL-CHUSYA (IDY)
                 ELSE
                   ADD     WRK-GOK-RYOU    TO  TBL-CHUSYA-2 (IDY)
                 END-IF
               END-IF
      ******** IF      ACCT-DAY (1 IDY)    >   ZERO
               IF      WRK-D-DAY           >   ZERO
                   IF      WRK-TEN-SRYCD   NOT =   SPACE
                       IF     (TBL-TEN-SRYCD(IDY)  NOT =  SPACE )
                           MOVE    WRK-TEN-SRYCD
                                               TO  TBL-TEN-SRYCD2
                                                                 (IDY)
                       ELSE
                           MOVE    WRK-TEN-SRYCD
                                               TO  TBL-TEN-SRYCD(IDY)
                       END-IF
                   END-IF
      *            中心静脈
                   IF      WRK-TEN-SRYCD1  NOT =   SPACE
                       IF    ((TBL-IVH-SRYCD (IDY) NOT =   SPACE )  AND
                              (TBL-IVH-IDX   (IDY)     >   ZERO  ))
                         OR  ((TBL-IVH6-SRYCD(IDY) NOT =   SPACE ) AND
                              (TBL-IVH6-IDX  (IDY)     >   ZERO  ))
                           MOVE    "9026"              TO  SPA-ERRCD
                           IF      TBL-IVH-IDX (IDY)   >   ZERO
                               MOVE    TBL-IVH-IDX (IDY)   TO  IDX-IDY
                           ELSE
                               MOVE    TBL-IVH6-IDX (IDY)  TO  IDX-IDY
                           END-IF
                           MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-IDY)
                       ELSE
                           MOVE    WRK-TEN-SRYCD1  TO  TBL-IVH-SRYCD
                                                                 (IDY)
                       END-IF
                   END-IF
      *
      *            中心静脈
                   IF      WRK-TEN-SRYCD6  NOT =   SPACE
                       IF    ((TBL-IVH-SRYCD (IDY) NOT =   SPACE )  AND
                              (TBL-IVH-IDX   (IDY)     >   ZERO  ))
                         OR  ((TBL-IVH6-SRYCD(IDY) NOT =   SPACE ) AND
                              (TBL-IVH6-IDX  (IDY)     >   ZERO  ))
                           MOVE    "9026"              TO  SPA-ERRCD
                           IF      TBL-IVH-IDX (IDY)   >   ZERO
                               MOVE    TBL-IVH-IDX (IDY)   TO  IDX-IDY
                           ELSE
                               MOVE    TBL-IVH6-IDX (IDY)  TO  IDX-IDY
                           END-IF
                           MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-IDY)
                       ELSE
                           MOVE    WRK-TEN-SRYCD6  TO  TBL-IVH6-SRYCD
                                                                 (IDY)
                       END-IF
                   END-IF
      *
      *            生物学的製剤
                   IF      WRK-TEN-SRYCD2  NOT =   SPACE
                       MOVE    WRK-TEN-SRYCD2  TO  TBL-SEIBUTU-ARI(IDY)
                   END-IF
      *            麻薬注射
                   IF      WRK-TEN-SRYCD3  NOT =   SPACE
                       MOVE    WRK-TEN-SRYCD3  TO  TBL-MADOKU-ARI(IDY)
                   END-IF
      *
      *            訂正は今回入力分のみ
                   IF     ((LNK-ORCSC-SYORIFLG =   "1" )
                      AND  (TBL-DAY  (IDY)     =   ZERO))
                     OR   ( FLG-OUT-OK         =   ZERO )
                       CONTINUE
                   ELSE
      *              生物学的製剤区分
                     IF     (FLG-SEIBUTU         =   1 )
                       IF      WRK-SRYSYUKBN       =   "330"
                           MOVE    1           TO  TBL-SEIBUTU (IDY)
                       ELSE
                           MOVE    1           TO  TBL-SEIBUTU-2(IDY)
                       END-IF
                     END-IF
      *              麻毒
                     IF     (FLG-MADOKU          =   1 )
                       IF      WRK-SRYSYUKBN       =   "330"
                           MOVE    1               TO  TBL-MADOKU (IDY)
                       ELSE
                           MOVE    1               TO  TBL-MADOKU-2(IDY)
                        END-IF
                     END-IF
      *
                   END-IF
               END-IF
           END-PERFORM
           .
       300111-TENTEKI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     中心静脈の日別の液量集計処理
      *****************************************************************
       300112-IVH-TBL-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
      *ver.4.7
      *同日再入院日
               IF      IDX-D           =   IDY
                   MOVE    ACCT-DAY (IDXX IDY)     TO  WRK-D-DAY
               ELSE
                   MOVE    ACCT-DAY (1    IDY)     TO  WRK-D-DAY
               END-IF
      *********IF      ACCT-DAY (1 IDY)    >   ZERO
               IF      WRK-D-DAY           >   ZERO
      *            生物学的製剤区分
                   IF      FLG-SEIBUTU         =   1
                       MOVE    1               TO  TBL-IVH2-SEIBUTU(IDY)
                   END-IF
      *            麻毒
                   IF      FLG-MADOKU          =   1
                       MOVE    1               TO  TBL-IVH2-MADOKU(IDY)
                   END-IF
               END-IF
           END-PERFORM
           .
       300112-IVH-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     当日のすべての手術の有無を判定処理
      *****************************************************************
       3003-SYUJYU-CHK-SEC         SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "50"                TO  ACCT-SRYKBN
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
               MOVE    1                   TO  FLG-ZAIARI
      *        訂正の時、集計なし
               IF     (LNK-ORCSC-SYORIFLG  =   "1"    )  AND
                      (ACCT-SRYKA          =   LNK-ORCSC-OLD-SRYKA) AND
                      (ACCT-HKNCOMBI       =   LNK-ORCSC-OLD-HKNCOMBI)
                   MOVE    ZERO                TO  FLG-ZAIARI
      *            同日再日
                   IF     (IDX-D              >   ZERO )  AND
                          (ACCT-DAY (IDXX IDX-D)  >  ZERO )
                       MOVE    1                  TO  FLG-ZAIARI
                   END-IF
               END-IF
      *        手技料点数がないものは対象外
               IF      ACCT-SYUTEN1        =   ZERO
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        包括まとめは対象外
               IF      ACCT-HKNCOMBI           =   "9999"
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        薬評は対象外
               IF      ACCT-ZAIKBN             =   1
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        輸血の判定
               IF      FLG-ZAIARI          =   1
      *            診療行為より
                   PERFORM 30031-SRYACT-YUKETU-SEC
               END-IF
      *
      *        回数編集
               IF      FLG-ZAIARI          =   1
                  PERFORM VARYING     IDY     FROM    1   BY  1
                          UNTIL       IDY     >   31
      *ver.4.7
      *同日再入院日
                       IF      IDX-D           =   IDY
                           MOVE    ACCT-DAY (IDXX IDY)     TO  WRK-D-DAY
                       ELSE
                           MOVE    ACCT-DAY (1    IDY)     TO  WRK-D-DAY
                       END-IF
      *
      **************** IF      ACCT-DAY (1 IDY)    >   ZERO
      ***************      MOVE    ACCT-DAY (1 IDY)
                       IF      WRK-D-DAY       >   ZERO
                           MOVE    WRK-D-DAY
                                               TO  TBL-SYU-DAY(IDY)
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3003-SYUJYU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為より輸血の判定処理
      *****************************************************************
       30031-SRYACT-YUKETU-SEC         SECTION.
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
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACT          =   ZERO
      *        輸血の時対象外
               IF      SRY-SRYSYUKBN       =   "510"
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
           END-IF
      *
           .
       30031-SRYACT-YUKETU-EXT.
           EXIT.
      *
      *****************************************************************
      *     当日のすべての画像診断点滴の有無を判定処理
      *****************************************************************
       3003-GAZOU-CHK-SEC         SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "70"                TO  ACCT-SRYKBN
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
               MOVE    1                   TO  FLG-ZAIARI
      *        訂正の時、集計なし
               IF     (LNK-ORCSC-SYORIFLG  =   "1"    )  AND
                      (ACCT-SRYKA          =   LNK-ORCSC-OLD-SRYKA) AND
                      (ACCT-HKNCOMBI       =   LNK-ORCSC-OLD-HKNCOMBI)
                   MOVE    ZERO                TO  FLG-ZAIARI
      *ver.4.7     同日再日
                   IF     (IDX-D              >   ZERO )  AND
                          (ACCT-DAY (IDXX IDX-D)  >  ZERO )
                       MOVE    1                  TO  FLG-ZAIARI
                   END-IF
               END-IF
      *        手技料点数がないものは対象外
               IF      ACCT-SYUTEN1       =   ZERO
                   MOVE    ZERO               TO  FLG-ZAIARI
               END-IF
      *        包括まとめは対象外
               IF      ACCT-HKNCOMBI           =   "9999"
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        薬評は対象外
               IF      ACCT-ZAIKBN             =   1
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
               IF      FLG-ZAIARI         =   1
      *            診療行為より液量集計
                   PERFORM 30031-SRYACT-HEN-SEC
      *
      *            回数編集
                   IF      FLG-GAZO-ARI        =   1
                       PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
      *ver.4.7
      *同日再入院日
                           IF      IDX-D           =   IDY
                               MOVE    ACCT-DAY (IDXX IDY) TO  WRK-D-DAY
                           ELSE
                               MOVE    ACCT-DAY (1    IDY) TO  WRK-D-DAY
                           END-IF
                           IF      WRK-D-DAY        >   ZERO
                               MOVE    WRK-D-DAY
                                               TO  TBL-GAZ-DAY(IDY)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3003-GAZOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理（画像診断）
      *****************************************************************
       30031-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
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
           MOVE    ZERO                TO  FLG-GAZO-ARI
           PERFORM UNTIL     (FLG-SRYACT   =   1   )  OR
                             (FLG-GAZO-ARI =   1   )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF     (SRY-SRYCD (IDX2)(1:2)   =   "13")  AND
                          (SRY-SRYSYUKBN           =   "731")
      *                点数マスタより
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF     (TNS-SRYSYUKBN   =   "330" )  AND
                              (TNS-DATAKBN     =   "1"   )
                           MOVE    1               TO  FLG-GAZO-ARI
                           MOVE    5               TO  IDX2
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
           .
       30031-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術に関係ない点滴の編集処理
      *****************************************************************
       3007-TENTEKI-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
      *        手術日をＩＶＨ用に編集
               MOVE    TBL-SYU-DAY(IDY)    TO  TBL-IVH-SYU-DAY(IDY)
               IF      TBL-SYU-DAY   (IDY)     =   ZERO
      *            手術がない時
                   COMPUTE TBL-CHUSYA (IDY)    =   TBL-CHUSYA (IDY)
                                               +   TBL-CHUSYA-2 (IDY)
                   IF      TBL-SEIBUTU-2(IDY)  =   1
                       MOVE    TBL-SEIBUTU-2(IDY)  TO  TBL-SEIBUTU(IDY)
                   END-IF
                   IF      TBL-MADOKU-2(IDY)   =   1
                       MOVE    TBL-MADOKU-2(IDY)   TO  TBL-MADOKU (IDY)
                   END-IF
               ELSE
      *            手術があり、手術外の点滴がある時
                   IF     (TBL-CHUSYA   (IDY)  >   ZERO ) AND
                          (TBL-CHUSYA-2 (IDY)  =   ZERO )
                       CONTINUE
                   ELSE
      *                手術はないものとする
                       MOVE    ZERO                TO  TBL-SYU-DAY(IDY)
                   END-IF
                   IF      TBL-CHUSYA-2 (IDY)  >   ZERO
                       MOVE    TBL-CHUSYA-2 (IDY)  TO  TBL-CHUSYA (IDY)
                       MOVE    TBL-SEIBUTU-2(IDY)  TO  TBL-SEIBUTU(IDY)
                       MOVE    TBL-MADOKU-2(IDY)   TO  TBL-MADOKU (IDY)
                   END-IF
               END-IF
      *        中心静脈
               IF      TBL-IVH-SYU-DAY (IDY)   =   ZERO
      *            手術がない時
                   COMPUTE TBL-IVH-CHUSYA(IDY) =   TBL-IVH-CHUSYA (IDY)
                                               +   TBL-IVH-CHUSYA-2(IDY)
                   IF      TBL-IVH-SEIBUTU-2(IDY)  =   1
                       MOVE    TBL-IVH-SEIBUTU-2(IDY)
                                               TO  TBL-IVH-SEIBUTU(IDY)
                   END-IF
                   IF      TBL-IVH-MADOKU-2(IDY)   =   1
                       MOVE    TBL-IVH-MADOKU-2(IDY)
                                               TO  TBL-IVH-MADOKU (IDY)
                   END-IF
               ELSE
      *            手術があり、手術外の点滴がある時
                   IF     (TBL-IVH-CHUSYA   (IDY)  >   ZERO ) AND
                          (TBL-IVH-CHUSYA-2 (IDY)  =   ZERO )
                       CONTINUE
                   ELSE
      *                手術はないものとする
                       MOVE    ZERO            TO  TBL-IVH-SYU-DAY(IDY)
                   END-IF
                   IF      TBL-IVH-CHUSYA-2 (IDY)  >   ZERO
                       MOVE    TBL-IVH-CHUSYA-2 (IDY)
                                               TO  TBL-IVH-CHUSYA (IDY)
                       MOVE    TBL-IVH-SEIBUTU-2(IDY)
                                               TO  TBL-IVH-SEIBUTU(IDY)
                       MOVE    TBL-IVH-MADOKU-2(IDY)
                                               TO  TBL-IVH-MADOKU (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3007-TENTEKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    中心静脈発生チェック処理
      *****************************************************************
       3004-IVH-AUTO-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
      *        中心静脈の薬剤があり、手術がない時
               IF     (TBL-IVH-CHUSYA (IDY)    >   ZERO )  AND
                      (TBL-IVH-SRYCD  (IDY)    =   SPACE )  AND
                      (TBL-IVH-DAY    (IDY)    >   ZERO )  AND
                      (TBL-IVH-SYU-DAY(IDY)    =   ZERO )
                AND   (TBL-IVH6-SRYCD (IDY)    =   SPACE )
      *            点滴がない時
                   IF    ((TBL-TEN-IDX   (IDY)     >   ZERO  )  OR
                          (TBL-TEN-SRYCD (IDY)     =   SPACE )) AND
                          (TBL-TEN-SRYCD2(IDY)     =   SPACE )
                       MOVE    CON-CHUJYO          TO  TBL-IVHA-SRYCD
                                                               (IDY)
                   ELSE
                       IF      TBL-IVH-CHUIDX (IDY)   >   ZERO
                           MOVE    TBL-IVH-CHUIDX (IDY)   TO  IDX-KBN
                           MOVE    IDY                    TO  IDX-IDY
                           IF      WRK-COM-NYU-KZMFLG (IDX-KBN IDY)
                                                           =   SPACE
                               MOVE    "K014"          TO  WRK-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-KBN)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        手術が存在する時、点滴手技は算定できない
               IF     (TBL-IVH-SYU-DAY (IDY)   >   ZERO )  AND
                      (TBL-IVH-SRYCD (IDY) NOT =   SPACE)
                   IF      TBL-IVH-IDX (IDY)   >   ZERO
      *                手技料エラー
                       MOVE    "9027"              TO  SPA-ERRCD
                       MOVE    TBL-IVH-IDX (IDY)   TO  IDX-K
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                   END-IF
               END-IF
      *        手術が存在する時、点滴手技は算定できない
               IF     (TBL-SYU-IDX (IDY)   >   ZERO )  AND
                      (TBL-IVH-SRYCD (IDY) NOT =   SPACE)
                   IF      TBL-SYU-IDX (IDY)   >   ZERO
      *                手技料エラー
                       MOVE    TBL-SYU-IDX (IDY)   TO  IDX-KBN
                       MOVE    IDY                 TO  IDX-IDY
                       IF      WRK-COM-NYU-KZMFLG (IDX-KBN IDY)
                                                       =   SPACE
                           MOVE    "K015"              TO  WRK-ERRCD
                           MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-KBN)
                       END-IF
                   END-IF
               END-IF
      *        点滴の手技が存在する時、エラーとする
               IF     (TBL-TEN-IDX   (IDY)     >   ZERO )  AND
                     ((TBL-IVH-SRYCD (IDY) NOT =   SPACE)  OR
                      (TBL-IVHA-SRYCD(IDY) NOT =   SPACE)  OR
                      (TBL-IVH6-SRYCD(IDY) NOT =   SPACE))
                   IF      TBL-TEN-IDX  (IDY)      >   ZERO
      *                手技料エラー
                       MOVE    "9025"              TO  SPA-ERRCD
                       MOVE    TBL-TEN-IDX  (IDY)  TO  IDX-K
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                   END-IF
               END-IF
      *!!!!
      *  中心静脈（植入型カテーテル）
      *        手術が存在する時、注射手技は算定できない
               IF     (TBL-IVH-SYU-DAY (IDY)   >   ZERO  )
                 AND  (TBL-IVH6-SRYCD(IDY) NOT =   SPACE )
                 AND  (SPA-ERRCD               =   SPACE )
                   IF      TBL-IVH6-IDX (IDY)   >   ZERO
      *                手技料エラー
                       MOVE    "9027"              TO  SPA-ERRCD
                       MOVE    TBL-IVH6-IDX (IDY)   TO  IDX-K
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                   END-IF
               END-IF
      *        中心静脈が存在する時、エラーとする
               IF     (TBL-IVH6-SRYCD(IDY) NOT =   SPACE)
                 AND  (SPA-ERRCD               =   SPACE )
                   IF    (TBL-IVH-SRYCD (IDY) NOT =   SPACE)  OR
                         (TBL-IVHA-SRYCD(IDY) NOT =   SPACE)
                       IF      TBL-IVH6-IDX (IDY)   >   ZERO
      *                    手技料エラー
                           MOVE    "9026"              TO  SPA-ERRCD
                           MOVE    TBL-IVH6-IDX (IDY)  TO  IDX-K
                           MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                       END-IF
                   END-IF
               END-IF
      *!!!!
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    WRK-ERRCD       TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-NYU-KZMFLG
                                                      (IDX-KBN IDX-IDY)
                   MOVE    "1"             TO  WRK-COM-KZMFLG
                                                   (IDX-KBN)
               END-IF
           END-IF
      *
      *    中心静脈自動発生
           INITIALIZE                  WRK-NISUU-HEN-AREA
           MOVE    SPACE               TO  WRK-TEN-SRYCD
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
               IF     (TBL-IVHA-SRYCD(IDY) NOT =   SPACE ) AND
                      (TBL-IVH-DAY   (IDY)     >   ZERO  )
                   MOVE    TBL-IVHA-SRYCD(IDY) TO  WRK-TEN-SRYCD
                   MOVE    1                   TO  WRK-NIDAY (IDY)
               END-IF
           END-PERFORM
           IF      WRK-TEN-SRYCD   NOT =   SPACE
               MOVE    "340"           TO  WRK-SRYSYUKBN
               MOVE    "33"            TO  WRK-SRYKBN
               PERFORM 3002-AUTO-HENSYU-SEC
           END-IF
      *
           .
       3004-IVH-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射発生チェック処理
      *****************************************************************
       3005-TENTEKI-AUTO-SEC         SECTION.
      *
      *    ６歳到達日
           MOVE    SPA-BIRTHDAY        TO  WRK-BYMD
           COMPUTE WRK-BYY             =   WRK-BYY
                                       +   6
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   31            )
                           OR (SPA-ERRCD   NOT =   SPACE )
      *        ６歳未満
               IF      WRK-BYMD(1:6)       >   SPA-SRYYMD(1:6)
                   MOVE    "130003710"     TO  WRK-SRYCD
                   MOVE    100             TO  WRK-MAX-RYOU
               END-IF
      *        ６歳
               IF      WRK-BYMD(1:6)       =  SPA-SRYYMD(1:6)
                   IF      WRK-BDD         <=  IDY
                       MOVE    "130003810"     TO  WRK-SRYCD
                       MOVE    500             TO  WRK-MAX-RYOU
                   ELSE
                       MOVE    "130003710"     TO  WRK-SRYCD
                       MOVE    100             TO  WRK-MAX-RYOU
                   END-IF
               END-IF
      *        ６歳以上
               IF      WRK-BYMD(1:6)       <   SPA-SRYYMD(1:6)
                   MOVE    "130003810"     TO  WRK-SRYCD
                   MOVE    500             TO  WRK-MAX-RYOU
               END-IF
      *
               IF      TBL-CHUSYA (IDY)    >=  WRK-MAX-RYOU
                   IF     (TBL-TEN-SRYCD(IDY)  =   SPACE )  AND
                          (TBL-SYU-DAY (IDY)   =   ZERO  )
      *                すでに、画像診断等の点滴が算定されているとき
                       IF     ((TBL-GAZ-DAY (IDY)  >   ZERO  )  AND
                               (TBL-GAZ-IDX (IDY)  =   ZERO  ))  OR
                              ((TBL-IVH-SRYCD(IDY) NOT =   SPACE )  OR
                               (TBL-IVHA-SRYCD(IDY)    NOT =   SPACE)
                           OR  (TBL-IVH6-SRYCD(IDY)    NOT =   SPACE ))
                           CONTINUE
                       ELSE
      *                手技料自動算定
                           MOVE    WRK-SRYCD       TO  TBL-AUTO-SRYCD
                                                                (IDY)
                       END-IF
                   END-IF
               ELSE
                   IF     (TBL-TEN-IDX  (IDY)  >   ZERO ) AND
                          (TBL-TEN-SRYCD(IDY)  NOT =   CON-CHUJYO)
      *                液量エラー
                       MOVE    TBL-TEN-IDX (IDY)   TO  IDX-KBN
                       MOVE    IDY                 TO  IDX-IDY
                       IF      WRK-COM-NYU-KZMFLG (IDX-KBN IDY)
                                                       =   SPACE
                           MOVE    "K012"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-KBN)
                       END-IF
                   END-IF
               END-IF
      *        麻毒・生物のチェック
               IF     (TBL-SEIBUTU   (IDY)     =   ZERO )  AND
                      (TBL-IVH-SEIBUTU (IDY)   =   ZERO )  AND
                      (TBL-SEIBUTU-IDX(IDY)    >   ZERO )
                   MOVE    "9017"                  TO  SPA-ERRCD
                   MOVE    TBL-SEIBUTU-IDX (IDY)   TO  IDX-K
                   MOVE    "1"                     TO  WRK-COM-CUR
                                                              (IDX-K)
               END-IF
               IF     (TBL-MADOKU    (IDY)     =   ZERO )  AND
                      (TBL-IVH-MADOKU (IDY)    =   ZERO )  AND
                      (TBL-MADOKU-IDX(IDY)     >   ZERO )
                   MOVE    "9018"                  TO  SPA-ERRCD
                   MOVE    TBL-MADOKU-IDX (IDY)    TO  IDX-K
                   MOVE    "1"                     TO  WRK-COM-CUR
                                                              (IDX-K)
               END-IF
      *        点滴手技の変更エラー
               IF     (TBL-TEN-SRYCD (IDY) NOT =   SPACE)  AND
                      (TBL-TEN-SRYCD2(IDY) NOT =   SPACE)  AND
                      (TBL-TEN-IDX   (IDY)     >   ZERO )
      ***          MOVE    TBL-TEN-IDX (IDY)   TO  IDX-KBN
      *            IF      WRK-COM-KZMFLG (IDX-KBN)    =   SPACE
      *                MOVE    "K011"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR
      *                                                   (IDX-KBN)
      ****         END-IF
                   MOVE    "9019"              TO  SPA-ERRCD
                   MOVE    TBL-TEN-IDX (IDY)   TO  IDX-K
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
               END-IF
      *        手術が存在する時、点滴手技は算定できない
               IF     (TBL-SYU-DAY (IDY)       >   ZERO )  AND
                      (TBL-TEN-SRYCD (IDY) NOT =   SPACE)
                   IF      TBL-TEN-IDX (IDY)   >   ZERO
      *                手技料エラー
                       MOVE    "9016"              TO  SPA-ERRCD
                       MOVE    TBL-TEN-IDX (IDY)   TO  IDX-K
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                   ELSE
      *                手技料エラー（警告）
                       IF      TBL-SYU-IDX (IDY)   >   ZERO
                           MOVE    TBL-SYU-IDX (IDY)   TO  IDX-KBN
                           MOVE    IDY                 TO  IDX-IDY
                           IF      WRK-COM-NYU-KZMFLG (IDX-KBN IDY)
                                                       =   SPACE
                              MOVE    "K010"       TO  WRK-ERRCD
                              MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-KBN)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        画像診断の点滴手技が存在する時
               IF     (TBL-GAZ-DAY (IDY)   >   ZERO )
                 AND  (SPA-ERRCD           =   SPACE)
                   IF     (TBL-TEN-SRYCD (IDY) NOT =   SPACE) OR
                          (TBL-TEN-SRYCD2(IDY) NOT =   SPACE)
                       MOVE    "9022"              TO  SPA-ERRCD
                   ELSE
                       IF     (TBL-GAZ-IDX (IDY)   >   ZERO  )  AND
                              (TBL-CHUSYA (IDY)    >=  WRK-MAX-RYOU)
                           MOVE    "9023"              TO  SPA-ERRCD
                       END-IF
                   END-IF
                   IF      SPA-ERRCD       NOT =   SPACE
      *
                   IF      TBL-GAZ-IDX (IDY)   >   ZERO
      *                手技料エラー
      *?????           MOVE    "9022"              TO  SPA-ERRCD
                       MOVE    TBL-GAZ-IDX (IDY)   TO  IDX-K
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-K)
                   ELSE
      *                手技料エラー（警告）
                       MOVE    SPACE               TO  SPA-ERRCD
                       IF      TBL-TEN-IDX (IDY)   >   ZERO
                           MOVE    TBL-TEN-IDX (IDY)   TO  IDX-KBN
                           MOVE    IDY                 TO  IDX-IDY
                           IF      WRK-COM-NYU-KZMFLG (IDX-KBN IDY)
                                                       =   SPACE
                              MOVE    "K013"       TO  WRK-ERRCD
                              MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-KBN)
                           END-IF
                       END-IF
                   END-IF
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    WRK-ERRCD       TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-NYU-KZMFLG
                                                   (IDX-KBN IDX-IDY)
                   MOVE    "1"             TO  WRK-COM-KZMFLG
                                                   (IDX-KBN)
               END-IF
           END-IF
      *
           .
       3005-TENTEKI-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴より入院開始日決定処理
      *****************************************************************
       30051-NYUINREKI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-NYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    SPA-HOSPNUM         TO  STUSAN-HOSPNUM
           MOVE    SPA-PTID            TO  STUSAN-PTID
           MOVE    SPA-SRYYMD          TO  STUSAN-KJNYMD
           CALL    "ORCSTUSAN"         USING   ORCSTUSANAREA
                                               SPA-AREA
      *
      *    通算入院日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           COMPUTE LNK-DAY6-ZOGEN      =  (STUSAN-NISSU03 -  1 )
                                       *   -1
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           MOVE    LNK-DAY6-KEISAN     TO  WRK-NYMD
      *
           IF      WRK-NYMD            =   SPACE
               MOVE    LNK-ORCSC-NYUINYMD  TO  WRK-NYMD
           END-IF
      *    入院日より１年後
           COMPUTE WRK-NYY             =   WRK-NYY
                                       +   1
      *
      *
           .
       30051-NYUINREKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴手技料追加処理
      *****************************************************************
       3002-AUTO-HENSYU-SEC         SECTION.
      *
           MOVE    WRK-TEN-SRYCD       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      3002-AUTO-HENSYU-EXT
           END-IF
      *
           ADD     1                   TO  IDXS
           MOVE    WRK-TEN-SRYCD       TO  LNK-ORCSC-SRYCD (03 IDXS)
           MOVE    TNS-TEN             TO  LNK-ORCSC-TENSU (03 IDXS)
      *    年齢加算処理
           PERFORM 30021-NENREI-KASAN-SEC
           IF      FLG-ERR             =   ZERO
               PERFORM 30022-NISUU-SET-SEC
               MOVE    WRK-SRYSYUKBN   TO  LNK-ORCSC-SRYSYUKBN
                                                         (03 IDXS)
               MOVE    WRK-SRYKBN      TO  LNK-ORCSC-SRYKBN
                                                         (03 IDXS)
           END-IF
      *
           .
       3002-AUTO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       30021-NENREI-KASAN-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX2)   =   SPACE  OR
                                                           ZERO )
               MOVE    TNS-AGEKSNSRYCD(IDX2)   TO  WRK-SRYCD
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            年齢チェック
                   PERFORM 30021-NENREI-CHK-SEC
                   IF      FLG-CHK              =   ZERO
      *
      *                加算料計算
                       MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD-KS
                                                       (03 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                       (03 IDXS)
                       MOVE    4               TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       30021-NENREI-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       30021-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
           MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD   <=  28   )
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   >=  WRK-KGNAGE
                       CONTINUE
                    ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <=  28   )
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
           .
       30021-NENREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の日数自動算定処理
      *****************************************************************
       30022-NISUU-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDN1
           MOVE    ZERO                TO  IDN2
      *
           INITIALIZE                      WRK-NISUU-AREA
           MOVE    ZERO                TO  WRK-H-DAY
           MOVE    SPACE               TO  WRK-H-SEL
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   32
               IF     (IDN1                =   32  )  OR
                      (WRK-NIDAY (IDN1)    =   ZERO)
                   IF      WRK-H-DAY       NOT =   ZERO
                       IF      WRK-H-SEL   NOT =   SPACE
                           ADD     1               TO  IDN2
                           MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                           MOVE    WRK-H-SEL       TO  WRK-SEL(IDN2 - 1)
                       END-IF
                       MOVE    ZERO            TO  WRK-H-DAY
                       MOVE    SPACE           TO  WRK-H-SEL
                   END-IF
               ELSE
                   IF      WRK-H-DAY           =   ZERO
                       MOVE    IDN1            TO  WRK-H-DAY
                       ADD     1               TO  IDN2
                       MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                   ELSE
                       IF      WRK-H-DAY       =  (IDN1  -  1 )
                           MOVE    "-"             TO  WRK-H-SEL
                       END-IF
                       MOVE    IDN1            TO  WRK-H-DAY
                   END-IF
               END-IF
           END-PERFORM
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF     (WRK-SEL (IDN1)      =   SPACE )  AND
                      (IDN1                <   31    )
                   IF      WRK-DAY (IDN1 + 1 )     NOT =   ZERO
                       MOVE    ","          TO  WRK-SEL (IDN1)
                   END-IF
               ELSE
                   IF     (IDN1                    =   31    )  OR
                          (WRK-DAY (IDN1 + 1 )     =   ZERO  )
                       MOVE    SPACE        TO  WRK-SEL (IDN1)
                   END-IF
               END-IF
           END-PERFORM
           INITIALIZE                      LNK-ORCSC-INPUTCD-G (03 IDXS)
           MOVE    1                   TO  IDN2
           MOVE    1                   TO  IDN3
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )  OR
                              (IDN2            >=  5  )  OR
                              (WRK-DAY (IDN1)  =   ZERO )
               IF      IDN3                >   19
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
      *        '-' の次判定
               IF     (WRK-SEL (IDN1)      =   "-" )
                  AND ((IDN3   +   3 )     >=  19  )
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
      *
               IF      LNK-ORCSC-INPUTCD(03 IDXS IDN2)   =   SPACE
                   MOVE    "*/"            TO  LNK-ORCSC-INPUTCD
                                                   (03 IDXS IDN2) (1:2)
                   ADD     2               TO  IDN3
               END-IF
               IF      WRK-SEL (IDN1)      =   SPACE
                   MOVE    ","             TO  WRK-SEL (IDN1)
               END-IF
               MOVE    WRK-NISUU-TBL(IDN1) TO  LNK-ORCSC-INPUTCD
                                                  (03 IDXS IDN2)(IDN3:3)
               ADD     3                   TO  IDN3
           END-PERFORM
      *
           .
       30022-NISUU-SET-EXT.
           EXIT.
      *
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
      *
           .
       910-TENSU-READ-EXT.
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
      *****************************************************************
