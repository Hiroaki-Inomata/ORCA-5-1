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
       PROGRAM-ID.                 ORCSC30200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（注射）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  １診療行為内で、点滴は１回とする
      * 01.00.02     MCC−多々納  01/05/16  点滴注射の判定を診療種別とする
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
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-SYUGI           PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-TENTEKI         PIC 9(01).
      *
           03  FLG-SYUGIADD        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
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
           03  WRK-CHUSYA          PIC 9(09).
      *
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
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
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-SEIBETU
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
                   WHEN    "."
                   WHEN    "S"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    点滴の時、手技料の自動修正
           IF      WRK-COM-SRYSYUKBN(01)   =   "330"
               PERFORM 1002-TENTEKI-CHK-SEC
           END-IF
      *
      *    皮下注射の時、手技料の自動修正
           IF      WRK-COM-SRYSYUKBN(01)   =   "310"
               PERFORM 1002-CHUSYA-CHK-SEC
           END-IF
      *
      *    種別のみの時、手技料の自動発生
           IF     (FLG-SYUGI           =   ZERO  )  AND
                  (WRK-ERRCD           =   SPACE )
               PERFORM 1003-SYUGIJIDOU-SEC
           END-IF
      *
           IF      IDX-SYU             >   ZERO
      *        自動加算を追加する
               PERFORM 1004-JIDO-KASAN-SEC
           END-IF
      *
           IF      WRK-ERRCD(1:1)  NOT =   "K"
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
                       MOVE   1                TO  FLG-SEIBETU
                   END-IF
      *            注射液量加算
                   COMPUTE WRK-CHUSYA      =  (WRK-COM-CSYYOURYO(IDX-Z)
                                           *   WRK-COM-SURYO  (IDX-Z))
                                           +   WRK-CHUSYA
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
               IF     (WRK-SURYO-S2              >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)  =   SPACE   )
                   MOVE    IDX-Z                 TO  IDX-HAI
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    手技料自動発生成処理
      *****************************************************************
       1003-SYUGIJIDOU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           EVALUATE    WRK-COM-SRYSYUKBN (01)
      *        皮下筋肉注射
               WHEN    "310"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       MOVE    "130000510"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700510"     TO  WRK-SRYCD
                   END-IF
      *        静脈注射
               WHEN    "320"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       MOVE    "130003510"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700610"     TO  WRK-SRYCD
                   END-IF
      *        点滴注射
               WHEN    "330"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       IF      LNK-ORCSC-NENREI-YY     <   6
                           MOVE    "130003710"     TO  WRK-SRYCD
                       ELSE
                           MOVE    "130003810"     TO  WRK-SRYCD
                       END-IF
                   ELSE
                       MOVE    "130700710"     TO  WRK-SRYCD
                   END-IF
      *            同日２回目以降
      *????        IF      LNK-ACCT2-SRYCD     NOT =   SPACE
      *????            MOVE    "099303301"         TO  WRK-SRYCD
      *????        END-IF
           END-EVALUATE
      *
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
           IF      LNK-ORCSC-ROUJIN    =   ZERO
               MOVE    "130009310"         TO  WRK-SRYCD
           ELSE
               MOVE    "130700950"         TO  WRK-SRYCD
           END-IF
           IF      LNK-ORCSC-NENREI-YY     <   6
      *        ６歳未満で　100ml 以上
               IF      WRK-CHUSYA      >=  100
                   MOVE    "130003710"     TO  WRK-SRYCD
               END-IF
           ELSE
      *        ６歳以上で　500ml 以上
               IF      WRK-CHUSYA      >=  500
                   IF      LNK-ORCSC-ROUJIN    =   ZERO
                       MOVE    "130003810"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700710"     TO  WRK-SRYCD
                   END-IF
               END-IF
           END-IF
      *
      *
      *    同日２回目以降は変更なし
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                       =   "099303301")
               IF      WRK-SRYCD       NOT =   LNK-ACCT2-SRYCD
                   IF      WRK-COM-KZMFLG (IDX-SYU)    =   SPACE
                       MOVE    "K006"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-SYU)
                   END-IF
               END-IF
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
                                       OR (WRK-GMN-INPUTCD (IDX)
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
               IF      LNK-ORCSC-ROUJIN      =   ZERO
                   MOVE    "130003510"     TO  WRK-SRYCD
               ELSE
                   MOVE    "130700610"     TO  WRK-SRYCD
               END-IF
           END-IF
      *
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                   NOT =   WRK-SRYCD )
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
      *                    診療種別名称（固定とする）
                           MOVE    "静脈注射"
                                               TO  WRK-GMN-MEISYO (01)
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
       1003-TENTEKI-CHK-EXT.
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
           IF      FLG-SEIBETU         =   1
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
                   UNTIL      (IDX1        >   SPA-MAX-LINE   ) OR
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
           MOVE    LNK-ORCSC-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
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
           IF      FLG-SYUGIADD         =   1
              GO      TO      1100-JIDOTUIKA-EXT
           END-IF
           IF      IDX-S               =   1  OR  ZERO
              GO      TO      1100-JIDOTUIKA-EXT
           END-IF
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
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
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
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
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
           MOVE    ZERO                TO  LNK-SC90-HAIKI
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *    各チェック処理
      **** EVALUATE    WRK-MOJI
      *        薬剤
      *        WHEN    "6"
      *            廃棄が有った時、破棄コメントを追加する
      *            IF      LNK-SC90-HAIKI          =   1
      *                MOVE    IDX-Z               TO  IDX-HAI
      *                PERFORM 10011-HAIKI-ADD-SEC
      *            END-IF
      **** END-EVALUATE
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC           SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-HAI + 1) =   WRK-SRYCD
               GO      TO      10011-HAIKI-ADD-EXT
           END-IF
      *
           MOVE    IDX-HAI             TO  IDX-S
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  FLG-SYUGIADD
           PERFORM 1100-JIDOTUIKA-SEC
      *
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *    自動加算以外の加算（時間外、年齢以外）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        手技料の点数を所定点数とする
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
      *    自動加算の加算（時間外、年齢）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
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
      *
           MOVE    ZERO                TO  FLG-TENTEKI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE)
      *        点滴は１回のみとする
               IF     (WRK-GMN-INPUTCD (IDX)(1:1)   =   "."  )  AND
                      (WRK-COM-SRYSYUKBN (IDX)      =   "330")
                   IF      FLG-TENTEKI         =   1
                       MOVE    "0057"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-CUR(IDX)
                   END-IF
                   MOVE    1               TO  FLG-TENTEKI
               END-IF
           END-PERFORM
      *----(01.00.01) LINE ADD START ----------------------------------
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *
