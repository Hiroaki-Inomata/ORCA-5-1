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
       PROGRAM-ID.                 ORCGJ021.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会
      *  コンポーネント名  : 保険組合せ変更（Ｊ０２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/24    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/11/10  レセプトコメント更新追加
      *                                     生保単独チェック追加
      * 01.00.02     NACL-多々納  04/10/20  変更期間追加
      * 01.00.03     NACL-多々納  05/04/20  診療会計附加情報追加
      * 01.00.04     NACL-多々納  05/11/29  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正
      * 04.06.00     NACL-多々納  10/09/06  自費保険入院中入力対応
      * 04.08.00     NACL-多々納  13/01/15  診療回数テーブル１０対応
      * 04.08.00     NACL-多々納  14/01/08  第三者行為対応
      * 04.08.00     NACL-多々納  15/06/04  削除分対応
      * 04.08.00     NACL-多々納  17/02/XX  HAORI対応
      *****************************************************************
      *
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-JYURRK-ATO      PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
           03  FLG-ATO-ROUJIN      PIC 9(01).
           03  FLG-MAE-ROUJIN      PIC 9(01).
           03  FLG-ATO-SEIHO       PIC 9(01).
           03  FLG-MAE-SEIHO       PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
      *
           03  FLG-MAE-HEN         PIC 9(01).
           03  FLG-ACCT-END        PIC 9(01).
      *
           03  FLG-JYURRK-NASI     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDJ                 PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
      *
           03  IDX-N               PIC 9(02).
      *
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-PTFTNRATE       PIC 9(03).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
           03  WRK-DEL-MEI         PIC X(14).
      *
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-STDAY           PIC X(02).
           03  WRK-ENDAY           PIC X(02).
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-HZN-ZAINUM      PIC 9(08).
      *
           03  WRK-DAY-G.
               05  WRK-DAY         PIC 9(03)   OCCURS  4.
      *
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-ZZ              PIC ZZ.
      *
           03  WRK-DBPATH          PIC X(20).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-JIDMSG          PIC X(80).
      *
           03  WRK-GRP-ZAINUM-AREA.
               05  WRK-GRP-ZAINUM      PIC 9(08)   OCCURS   50.
               05  IDX-GEN             PIC 9(04).
               05  IDX-GEN2            PIC 9(04).
               05  IDX-STR             PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
       01  HZN-SRYACCT-REC.
           COPY    "CPSRYACCT.INC"   REPLACING
                                   //ACCT-// BY //HZN-ACCT-//.
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
       01  HZN-SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC"   REPLACING
                                   //ACCTP-// BY //HZN-ACCTP-//.
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"      REPLACING
                                   //SANTEI-// BY //HZN-SANTEI-//.
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプトコメント
       01  HZN-RECECOM-AREA.
         02  HZN-RECECOM-REC           OCCURS  40.
           COPY    "CPRECECOM.INC"     REPLACING
                                   //RECECOM-// BY //HZN-RECECOM-//.
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険算定年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *    収納更新差額取得パラメタ
           COPY    "CPORCSS002.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      **** COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA24SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
      *HAORI対応
               WHEN    "API"           ALSO    ANY
                   PERFORM 300-HAORI-SYORI-SEC
      *            画面表示なし
                   MOVE    1                   TO  FLG-END
      *
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           EVALUATE    TRUE
      *         終了フラグのある時、終了とする
               WHEN    SPA-J021-ENDFLG    =   1
                   PERFORM 210-BACK
      *        更新確認メッセージ表示
               WHEN    SPA-J021-ENDFLG    =   9
                   IF      SPA-NYUGAIKBN       =   "2"
                       MOVE    1               TO  SPA-J021-ENDFLG
      *                処理終了のメッセージを表示する
                       MOVE    "0010"          TO  SPA-ERRCD
                       PERFORM 510-ERRSET-SEC
                   ELSE
                       PERFORM 210-BACK
                   END-IF
               WHEN    OTHER
                   PERFORM 1001-INIT-SYORI-SEC
           END-EVALUATE
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       1001-INIT-SYORI-SEC              SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "J021"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       1001-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG(1:4)
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-JIDCD
      *
               WHEN    OTHER
      *            初期表示
                   PERFORM                     310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-J021-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
      *            変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 30031-HENKOU-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-J021-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-J021-AREA
      *
      *     保険組合せ
            PERFORM 3101-HKNCOMBI-HEN-SEC
      *     変更期間
            MOVE    01                 TO  SPA-J021-DAY1
            MOVE    SPA-SRYMATU        TO  SPA-J021-DAY2
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC        SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI       TO  SPA-HKN-NENREI
           MOVE    ZERO                TO  SPA-J021-KOHCHK
           IF     (AGECHK-NENREI       >=  65 )
             AND  (SPA-J02-ROUJIN      =   ZERO )
      *        ６５歳以上で老人でない時、老人公費の有無判定
               PERFORM 31011-ROUJIN-CHK-SEC
           END-IF
      *
           INITIALIZE                  SPA-J021-TBL
           MOVE    ZERO                TO  SPA-J021-MAX
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           PERFORM
                   UNTIL      (FLG-HKNCOMBI        =   1      )
                          OR  (SPA-J021-MAX        >=   40     )
      *
      *        対象の保険組合せのみ編集
               MOVE    ZERO                TO  FLG-HKNCOMBICHK
      *****    IF      COMB-DELKBN         =   "1"
      *            CONTINUE
      *****    ELSE
                   IF      (SPA-NAI-SRYYM  >=  COMB-TEKSTYMD(1:6)) AND
                           (SPA-NAI-SRYYM  <=  COMB-TEKEDYMD(1:6))
                       MOVE    1                   TO  FLG-HKNCOMBICHK
                   END-IF
      *******  END-IF
      *        労災・自賠責は対象外
               IF     (COMB-HKNNUM         =   "971"  OR
                                               "973"
                                           OR  "975"  )
      *           第三者行為
                  OR  (COMB-KOH1HKNNUM     =   "970"  )
                  MOVE    ZERO                 TO  FLG-HKNCOMBICHK
               END-IF
      *H27.6
      *        削除分判定
               IF     (FLG-HKNCOMBICHK     =    1 )
                 AND  (COMB-DELKBN         =   "1")
                   PERFORM 31012-DELKBN-CHK-SEC
               END-IF
      *
               IF      FLG-HKNCOMBICHK    =    1
                   ADD     1                   TO  SPA-J021-MAX
      *
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-J021-THKNCOMBI
                                                    (SPA-J021-MAX)
                   MOVE    COMB-TEKSTYMD       TO  SPA-NAI-TEKSTYMD
                                                    (SPA-J021-MAX)
                   MOVE    COMB-TEKEDYMD       TO  SPA-NAI-TEKEDYMD
                                                    (SPA-J021-MAX)
                   MOVE    COMB-DELKBN         TO  SPA-NAI-TDELKBN
                                                    (SPA-J021-MAX)
                   MOVE    COMB-HYOJIKBN       TO  SPA-NAI-THYOJIKBN
                                                    (SPA-J021-MAX)
                   MOVE    COMB-HKNNUM         TO  SPA-J021-THKNNUM
                                                    (SPA-J021-MAX)
      *            老人判定
                   IF    ((COMB-KOH1HKNNUM     =   "027"  )  OR
                          (COMB-KOH2HKNNUM     =   "027"  )  OR
                          (COMB-KOH3HKNNUM     =   "027"  )  OR
                          (COMB-KOH4HKNNUM     =   "027"  ) )  OR
                         ( SPA-J02-ROUJIN      =   1        )
                       MOVE    1               TO  SPA-NAI-TROUJIN
                                                      (SPA-J021-MAX)
                   END-IF
      *            後期該当判定
                   IF    ((COMB-KOH1HKNNUM     =   "698"  )  OR
                          (COMB-KOH2HKNNUM     =   "698"  )  OR
                          (COMB-KOH3HKNNUM     =   "698"  )  OR
                          (COMB-KOH4HKNNUM     =   "698"  ) )  OR
                         ( SPA-J02-ROUJIN      =   1        )
                       MOVE    1               TO  SPA-NAI-TROUJIN
                                                      (SPA-J021-MAX)
                   END-IF
      *
      *            保険算定年齢チェック等
                   INITIALIZE                  ORCSAGECHKAREA
                   MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
                   MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
                   MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
                   MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
                   MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
                   MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
                                               SPA-AREA
      *!!!!!!!     IF      AGECHK-FTNRATE  NOT =   ZERO
                   IF      AGECHK-RC           =   ZERO
      *                負担率
                       MOVE    AGECHK-FTNRATE      TO  SPA-NAI-FTNWARI
                                                         (SPA-J021-MAX)
                       MOVE    AGECHK-FTNRATEMEI   TO  SPA-J021-TFTNRATE
                                                         (SPA-J021-MAX)
                   END-IF
      *            老人判定
                   IF      AGECHK-ROUJIN       =   1
                       MOVE    1               TO  SPA-NAI-TROUJIN
                                                      (SPA-J021-MAX)
                   END-IF
      *            生保等単独の場合
                   IF     (COMB-HKNNUM         =   SPACE )  AND
                          ((COMB-KOH1HKNNUM    =   "012" )  OR
                           (COMB-KOH2HKNNUM    =   "012" )  OR
                           (COMB-KOH3HKNNUM    =   "012" )  OR
                           (COMB-KOH4HKNNUM    =   "012" ))
                       IF     (SPA-HKN-NENREI      >=  65 ) AND
                              (SPA-J02-ROUJIN      =   ZERO)  AND
                              (SPA-J021-KOHCHK     =   1  )
      *                    老人年齢でなく、老人公費のある時
                           MOVE    1           TO  SPA-NAI-TSEIHO
                                                      (SPA-J021-MAX)
                       END-IF
                   END-IF
      *            精神通院・更正
                   IF     (COMB-HKNNUM         =   SPACE )  AND
                         (((COMB-KOH1HKNNUM    =   "015" )  OR
                           (COMB-KOH2HKNNUM    =   "015" )  OR
                           (COMB-KOH3HKNNUM    =   "015" )  OR
                           (COMB-KOH4HKNNUM    =   "015" ))  OR
                          ((COMB-KOH1HKNNUM    =   "021" )  OR
                           (COMB-KOH2HKNNUM    =   "021" )  OR
                           (COMB-KOH3HKNNUM    =   "021" )  OR
                           (COMB-KOH4HKNNUM    =   "021" )))
                       IF     (COMB-TEKSTYMD       >=   "20060401") AND
                              (SPA-HKN-NENREI      >=  65 ) AND
                              (SPA-J02-ROUJIN      =   ZERO)  AND
                              (SPA-J021-KOHCHK     =   1  )
      *                    老人年齢でなく、老人公費のある時
                           MOVE    1           TO  SPA-NAI-TSEIHO
                                                      (SPA-J021-MAX)
                       END-IF
                   END-IF
      *
                   MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-J021-TTEKSTYMD
                                                        (SPA-J021-MAX)
      *
                   MOVE    COMB-TEKEDYMD       TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-J021-TTEKEDYMD
                                                         (SPA-J021-MAX)
                   PERFORM 31011-HKNCOMBI-HEN-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-J021-THKNCOMBIMEI
                                                         (SPA-J021-MAX)
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除分判定処理
      *****************************************************************
       31012-DELKBN-CHK-SEC             SECTION.
      *
      *    受診履歴が存在すること
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    COMB-HKNCOMBINUM    TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
      *        受診有
               MOVE    1                   TO  FLG-OK
           ELSE
               MOVE    ZERO                TO  FLG-OK
           END-IF
      *
      *   入院の時、診療会計から判定
          IF     (SPA-NYUGAIKBN       =   "1" )
             AND (FLG-OK              =   ZERO )
      *        診療会計
               INITIALIZE                  SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  ACCT-PTID
               MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
               MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
               MOVE    COMB-HKNCOMBINUM    TO  ACCT-HKNCOMBI
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 980-SRYACCT-READ-SEC
                   IF      FLG-SRYACCT         =   ZERO
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *    今月算定なし
           IF      FLG-OK              =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBICHK
           END-IF
           .
       31012-DELKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費チェック処理
      *****************************************************************
       31011-ROUJIN-CHK-SEC             SECTION.
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
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
           MOVE    ZERO                TO  SPA-J021-KOHCHK
           PERFORM
                UNTIL     (FLG-PTKOHINF    =   1  )
                      OR  (SPA-J021-ROUJIN =   1  )
      *        老人公費
               IF      PTKOH-KOHNUM        =   "027"
                                           OR  "968"
                   IF      (SPA-NAI-SRYYM  >=  PTKOH-TEKSTYMD(1:6)) AND
                           (SPA-NAI-SRYYM  <=  PTKOH-TEKEDYMD(1:6))
                       MOVE    1               TO  SPA-J021-KOHCHK
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
           .
       31011-ROUJIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       31011-HKNCOMBI-HEN-SEC             SECTION.
      *
           IF      COMB-DELKBN         =   "1"
               MOVE    "（削除）"          TO  WRK-DEL-MEI
           ELSE
               MOVE    SPACE               TO  WRK-DEL-MEI
      *
               EVALUATE    COMB-HYOJIKBN
                   WHEN    "1"
                       MOVE    "【非表示】"        TO  WRK-DEL-MEI
                   WHEN    "2"
                       MOVE    "【入非表示】"      TO  WRK-DEL-MEI
                   WHEN    "3"
                       MOVE    "【外非表示】"      TO  WRK-DEL-MEI
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   WRK-DEL-MEI             DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       WRK-DEL-MEI             DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
           .
       31011-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  J021
      *
      *    画面編集
           INITIALIZE                      J021
           IF      SPA-J021-MAE-HKNCOMBI   =   ZERO
               CONTINUE
           ELSE
               MOVE    SPA-J021-MAE-HKNCOMBI   TO  J021-MAE-HKNCOMBI
           END-IF
           MOVE    SPA-J021-MAE-HKNCOMBIMEI 
                                           TO  J021-MAE-HKNCOMBIMEI
           MOVE    SPA-J021-MAE-FTNRATE    TO  J021-MAE-FTNRATE
           IF      SPA-J021-ATO-HKNCOMBI   =   ZERO
               CONTINUE
           ELSE
               MOVE    SPA-J021-ATO-HKNCOMBI   TO  J021-ATO-HKNCOMBI
           END-IF
           MOVE    SPA-J021-ATO-HKNCOMBIMEI 
                                           TO  J021-ATO-HKNCOMBIMEI
           MOVE    SPA-J021-ATO-FTNRATE    TO  J021-ATO-FTNRATE
      *
      *    保険組合せ
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-J021-MAX
               MOVE    SPA-J021-THKNCOMBI (IDX) TO  J021-NUM     (IDX)
               MOVE    SPA-J021-THKNCOMBIMEI (IDX)
                                                TO  J021-HKNKUMI (IDX)
               MOVE    SPA-J021-TTEKSTYMD (IDX) TO  J021-TEKSTYMD(IDX)
               MOVE    SPA-J021-TTEKEDYMD (IDX) TO  J021-TEKEDYMD(IDX)
               MOVE    SPA-J021-TFTNRATE  (IDX) TO  J021-FTNRATE (IDX)
           END-PERFORM
           MOVE    SPA-J021-MAX        TO  J021-HKNLIST-COUNT
      *
           MOVE    SPA-J021-DAY1       TO  J021-DAY1
           MOVE    SPA-J021-DAY2       TO  J021-DAY2
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-J021-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "MAE_HKNCOMBI"
                       MOVE    2           TO  SPA-J021-CUR
                   WHEN    "ATO_HKNCOMBI"
                       MOVE    3           TO  SPA-J021-CUR
                   WHEN    "DAY1"
                       MOVE    4           TO  SPA-J021-CUR
                   WHEN    "DAY2"
                       MOVE    10          TO  SPA-J021-CUR
                   WHEN    OTHER
                       MOVE    1           TO  SPA-J021-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-J021-CUR
               WHEN    1
                   MOVE    "MAE_HKNCOMBI"      TO  MCP-WIDGET
               WHEN    2
                   MOVE    "ATO_HKNCOMBI"      TO  MCP-WIDGET
               WHEN    3
                   MOVE    "DAY1"              TO  MCP-WIDGET
               WHEN    4
                   MOVE    "DAY2"              TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEA-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 212-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           MOVE    ZERO                TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-J021-CUR
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        保険組合せ
               WHEN    ANY             ALSO    "HKNLIST"
                   PERFORM 410-HKNCOMBI-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       410-HKNCOMBI-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-J021-MAX
               IF      J021-HKNLIST-SELECT (IDX)   =  "T"
                   MOVE    SPA-J021-THKNCOMBI  (IDX)  TO  WRK-HKNCOMBI
               END-IF
           END-PERFORM
           IF      WRK-HKNCOMBI    NOT =   ZERO
               IF     (SPA-J021-MAE-HKNCOMBI   =   SPACE  OR  ZERO ) OR
                      (SPA-J021-MAE-ERR        =   "1"             )
                   MOVE    WRK-HKNCOMBI        TO  J021-MAE-HKNCOMBI
                   MOVE    "MAE_HKNCOMBI"      TO  WRK-MCP-WIDGET
               ELSE
                   IF     (SPA-J021-ATO-HKNCOMBI   =   SPACE OR  ZERO )
                       OR (SPA-J021-ATO-ERR        =   "1"            )
                       MOVE    WRK-HKNCOMBI    TO  J021-ATO-HKNCOMBI
                       MOVE    "ATO_HKNCOMBI"  TO  WRK-MCP-WIDGET
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       410-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           IF     (J021-MAE-HKNCOMBI   NOT =   SPA-J021-MAE-HKNCOMBI)
              OR  (J021-ATO-HKNCOMBI   NOT =   SPA-J021-ATO-HKNCOMBI)
               MOVE    ZERO            TO  SPA-J021-KEIFLG
           END-IF
      *
           IF      J021-MAE-HKNCOMBI   =   SPA-J021-MAE-HKNCOMBI
               MOVE    ZERO            TO  FLG-MAE-HEN
           ELSE
               MOVE    1               TO  FLG-MAE-HEN
           END-IF
           MOVE    J021-MAE-HKNCOMBI   TO  SPA-J021-MAE-HKNCOMBI
           MOVE    J021-ATO-HKNCOMBI   TO  SPA-J021-ATO-HKNCOMBI
      *
           MOVE    J021-DAY1           TO  SPA-J021-DAY1
           MOVE    J021-DAY2           TO  SPA-J021-DAY2
      *
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    前保険組合せ
           PERFORM 4001-MAE-HKNCOMBI-SEC
      *    後保険組合せ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-ATO-HKNCOMBI-SEC
           END-IF
      *    期間
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-DAY1-CHK-SEC
           END-IF
      *    期間
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4002-DAY2-CHK-SEC
           END-IF
      *    登録時
           IF      FLG-KOUSIN          =   1
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4003-KANREI-CHK-SEC
              END-IF
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前保険組合せチェック処理
      *****************************************************************
       4001-MAE-HKNCOMBI-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-J021-MAE-ERR
           IF      SPA-J021-MAE-HKNCOMBI   =   SPACE
               MOVE    SPACE               TO  SPA-J021-MAE-HKNCOMBIMEI
               MOVE    SPACE               TO  SPA-J021-MAE-HKNNUM
               MOVE    SPACE               TO  SPA-J021-MAE-FTNRATE
           ELSE
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-J021-CUR
                   MOVE    "1"                 TO  SPA-J021-MAE-ERR
               ELSE
                   MOVE    SNUM-OUTEDT(12:4)   TO  SPA-J021-MAE-HKNCOMBI
      *            保険組合せ内容チェック
                   PERFORM 40011-MAE-HKNCOMBI-CHK-SEC
               END-IF
           END-IF
      *
           .
       4001-MAE-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ内容チェック編集処理
      *****************************************************************
       40011-MAE-HKNCOMBI-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-MAE-ROUJIN
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-J021-MAX )  OR
                             (FLG-OK       =   1        )
               IF      SPA-J021-MAE-HKNCOMBI   =
                                           SPA-J021-THKNCOMBI (IDX)
                   MOVE    SPA-J021-THKNCOMBIMEI  (IDX)
                                           TO  SPA-J021-MAE-HKNCOMBIMEI
                   MOVE    SPA-J021-THKNNUM       (IDX)
                                           TO  SPA-J021-MAE-HKNNUM
                   MOVE    SPA-J021-TFTNRATE (IDX)
                                           TO  SPA-J021-MAE-FTNRATE
                   MOVE    SPA-NAI-TROUJIN (IDX)
                                           TO  FLG-MAE-ROUJIN
                   MOVE    SPA-NAI-TSEIHO  (IDX)
                                           TO  FLG-MAE-SEIHO
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    受診履歴が存在すること
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  WRK-STDAY
           MOVE    ZERO                TO  WRK-ENDAY
           MOVE    ZERO                TO  WRK-DENPNUM
           PERFORM UNTIL    FLG-JYURRK     =   1
               IF      WRK-ENDAY           =   ZERO
                   MOVE    JYURRK-SRYYMD(7:2)  TO  WRK-STDAY
                   MOVE    JYURRK-SRYYMD(7:2)  TO  WRK-ENDAY
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
               END-IF
               IF      JYURRK-SRYYMD(7:2)  <   WRK-STDAY
                   MOVE    JYURRK-SRYYMD(7:2)  TO  WRK-STDAY
               END-IF
               IF      JYURRK-SRYYMD(7:2)  >   WRK-ENDAY
                   MOVE    JYURRK-SRYYMD(7:2)  TO  WRK-ENDAY
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (WRK-STDAY           =   ZERO )  AND
                  (WRK-ENDAY           =   ZERO )
              MOVE    1                TO  FLG-JYURRK-NASI
           ELSE
              MOVE    ZERO             TO  FLG-JYURRK-NASI
           END-IF
      *    入院の時、診療会計から判定
           MOVE    ZERO                TO  SPA-J021-NAI-ACCT
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 40013-SRYACCT-CHK-SEC
               IF      FLG-JYURRK-NASI     =   1
                   IF      SPA-J021-NAI-ACCT   =   1
                       MOVE    2               TO  SPA-J021-NAI-ACCT
                   END-IF
               END-IF
           END-IF
      *
           MOVE    WRK-STDAY           TO  SPA-J021-STDAY
           MOVE    WRK-ENDAY           TO  SPA-J021-ENDAY
      *
           IF      FLG-MAE-HEN         =   1
               MOVE    SPA-J021-STDAY      TO  SPA-J021-DAY1
               MOVE    SPA-J021-ENDAY      TO  SPA-J021-DAY2
           END-IF
      *
           IF      WRK-STDAY           =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-J021-CUR
               MOVE    "1"                 TO  SPA-J021-MAE-ERR
           ELSE
               IF      FLG-OK              =   ZERO
      *        今回対象外の時、保険組合せより保険を判定する
                   PERFORM 40012-MAE-HKNCHK-SEC
               END-IF
           END-IF
      *
           .
       40011-MAE-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    後保険組合せチェック処理
      *****************************************************************
       40012-MAE-HKNCHK-SEC               SECTION.
      *
      *
      *    保険組合検索
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  COMB-HKNCOMBINUM
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
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-J021-CUR
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *    労災・自賠責は対象外
           IF     (COMB-HKNNUM         =   "971"  OR
                                           "973"
      *        公害
                                       OR  "975"  )
      *           第三者行為
              OR  (COMB-KOH1HKNNUM     =   "970"  )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-J021-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 31011-HKNCOMBI-HEN-SEC
               MOVE    WRK-HKNCOMBIMEI     TO  SPA-J021-MAE-HKNCOMBIMEI
      *H31.1   自費保険情報編集
               MOVE    COMB-HKNNUM         TO  SPA-J021-MAE-HKNNUM
      *
      *        収納から負担割合を編集する
               INITIALIZE                  SYUNOU-REC
               MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   MOVE    SPACE               TO  SYUNOU-REC
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-SYUNOU          =   ZERO
                   MOVE    SYU-PTFTNRATE       TO  COMB-GAI-KFTNRATE
               END-IF
               IF      COMB-GAI-KFTNRATE    >   ZERO
                   MOVE    COMB-GAI-KFTNRATE   TO  WRK-ZZZ
                   MOVE    WRK-ZZZ             TO  SPA-J021-MAE-FTNRATE
                   MOVE    "%"                 TO  SPA-J021-MAE-FTNRATE
                                                                 (4:)
               END-IF
      *
      *        後期高齢者を選択している場合、老人
               IF      COMB-HKNNUM         =   "039"
                                           OR  "040"
                   MOVE    1               TO  FLG-MAE-ROUJIN
               END-IF
      *        老人判定
               IF    ((COMB-KOH1HKNNUM     =   "027"  )  OR
                      (COMB-KOH2HKNNUM     =   "027"  )  OR
                      (COMB-KOH3HKNNUM     =   "027"  )  OR
                      (COMB-KOH4HKNNUM     =   "027"  ) )  OR
                      ( SPA-J02-ROUJIN     =   1        )
                   MOVE    1               TO  FLG-MAE-ROUJIN
               END-IF
      *        生保単独の場合
               IF     (COMB-HKNNUM         =   SPACE )  AND
                      ((COMB-KOH1HKNNUM    =   "012" )  OR
                       (COMB-KOH2HKNNUM    =   "012" )  OR
                       (COMB-KOH3HKNNUM    =   "012" )  OR
                       (COMB-KOH4HKNNUM    =   "012" ))
                   IF     (SPA-HKN-NENREI      >=  65 ) AND
                          (SPA-J02-ROUJIN      =   ZERO)  AND
                          (SPA-J021-KOHCHK     =   1  )
                       MOVE    1               TO  FLG-MAE-SEIHO
                   END-IF
               END-IF
      *        精神通院・更正
               IF     (COMB-HKNNUM         =   SPACE )  AND
                     (((COMB-KOH1HKNNUM    =   "015" )  OR
                       (COMB-KOH2HKNNUM    =   "015" )  OR
                       (COMB-KOH3HKNNUM    =   "015" )  OR
                       (COMB-KOH4HKNNUM    =   "015" ))  OR
                      ((COMB-KOH1HKNNUM    =   "021" )  OR
                       (COMB-KOH2HKNNUM    =   "021" )  OR
                       (COMB-KOH3HKNNUM    =   "021" )  OR
                       (COMB-KOH4HKNNUM    =   "021" )))
                   IF     (COMB-TEKSTYMD       >=   "20060401") AND
                          (SPA-HKN-NENREI      >=  65 ) AND
                          (SPA-J02-ROUJIN      =   ZERO)  AND
                          (SPA-J021-KOHCHK     =   1  )
                       MOVE    1               TO  FLG-MAE-SEIHO
                   END-IF
               END-IF
           END-IF
           .
       40012-MAE-HKNCHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計判定処理
      *****************************************************************
       40013-SRYACCT-CHK-SEC           SECTION.
      *
      *    調剤料
           MOVE    "24"                TO  WRK-SRYKBN
           PERFORM 400131-SRYACCT-CHK-SYORI-SEC
      *    麻毒加算
           MOVE    "26"                TO  WRK-SRYKBN
           PERFORM 400131-SRYACCT-CHK-SYORI-SEC
      *
           .
       40013-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計判定処理
      *****************************************************************
       400131-SRYACCT-CHK-SYORI-SEC     SECTION.
      *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key40"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM
               UNTIL   FLG-SRYACCT     =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   31
                   IF      ACCT-DAY (1 IDX2)   >   ZERO
                       IF     (WRK-STDAY       =   ZERO )  OR
                              (WRK-STDAY       >   IDX2 )
                           MOVE    IDX2            TO  WRK-STDAY
                           MOVE    1               TO  SPA-J021-NAI-ACCT
                       END-IF
                       IF     (WRK-ENDAY       =   ZERO )  OR
                              (WRK-ENDAY       <   IDX2 )
                           MOVE    IDX2            TO  WRK-ENDAY
                           MOVE    1               TO  SPA-J021-NAI-ACCT
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key40"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key40"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       400131-SRYACCT-CHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後保険組合せチェック処理
      *****************************************************************
       4002-ATO-HKNCOMBI-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-J021-ATO-ERR
           IF      SPA-J021-ATO-HKNCOMBI   =   SPACE
               MOVE    SPACE               TO  SPA-J021-ATO-HKNCOMBIMEI
               MOVE    SPACE               TO  SPA-J021-ATO-FTNRATE
               MOVE    SPACE               TO  SPA-J021-ATO-HKNNUM
           ELSE
               PERFORM 40021-ATO-HKNCOMBI-CHK-SEC
           END-IF
           .
       4002-ATO-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後保険組合せチェック処理
      *****************************************************************
       40021-ATO-HKNCOMBI-CHK-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-J021-CUR
               MOVE    "1"                 TO  SPA-J021-ATO-ERR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-J021-ATO-HKNCOMBI
           END-IF
      *
           MOVE    ZERO                TO  FLG-ATO-ROUJIN
           MOVE    ZERO                TO  FLG-ATO-SEIHO
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX          >   SPA-J021-MAX)  OR
                             (FLG-OK       =   1    )   OR
                             (SPA-ERRCD    NOT =   SPACE )
               IF      SPA-J021-ATO-HKNCOMBI   =
                                           SPA-J021-THKNCOMBI (IDX)
                   MOVE    SPA-J021-THKNCOMBIMEI  (IDX)
                                           TO  SPA-J021-ATO-HKNCOMBIMEI
                   MOVE    SPA-J021-THKNNUM       (IDX)
                                           TO  SPA-J021-ATO-HKNNUM
                   MOVE    SPA-J021-TFTNRATE (IDX)
                                           TO  SPA-J021-ATO-FTNRATE
                   MOVE    SPA-NAI-TROUJIN (IDX)
                                           TO  FLG-ATO-ROUJIN
                   MOVE    SPA-NAI-TSEIHO  (IDX)
                                           TO  FLG-ATO-SEIHO
      *
                   IF     (SPA-NAI-TEKSTYMD (IDX)(1:6)
                                           <=  SPA-NAI-SRYYM  )  AND
                          (SPA-NAI-TEKEDYMD (IDX)(1:6)
                                           >=  SPA-NAI-SRYYM  )
      *                保険開始日が診療日より前であること
                       IF     (SPA-NAI-TEKSTYMD (IDX)(1:6)
                                           =   SPA-NAI-SRYYM )  AND
                              (SPA-NAI-TEKSTYMD (IDX)(7:2)
                                           >   SPA-J021-DAY1 )
                           IF      SPA-NAI-TEKSTYMD (IDX)(7:2)
                                                   >   SPA-J021-DAY2
                               MOVE    "0009"          TO  SPA-ERRCD
                               MOVE    4               TO  SPA-J021-CUR
                           ELSE
                               IF      SPA-NAI-TEKSTYMD (IDX)(7:2)
                                                   >   SPA-J021-STDAY
                                   MOVE    SPA-NAI-TEKSTYMD (IDX)(7:2)
                                                   TO  SPA-J021-DAY1
                               END-IF
                           END-IF
                       END-IF
      *                保険終了日が診療日より後であること
                       IF     (SPA-NAI-TEKEDYMD (IDX)(1:6)
                                           =   SPA-NAI-SRYYM )  AND
                              (SPA-NAI-TEKEDYMD (IDX)(7:2)
                                           <   SPA-J021-DAY2 )
                           MOVE    "0009"              TO  SPA-ERRCD
                           MOVE    4                   TO  SPA-J021-CUR
                       END-IF
                   ELSE
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
      *            削除でないこと
                   IF      SPA-NAI-TDELKBN (IDX)   =   "1"
                       MOVE    "0012"              TO  SPA-ERRCD
                   END-IF
      *            非表示でないこと
                   IF      SPA-NAI-THYOJIKBN (IDX) =   "1"
                       MOVE    "0022"              TO  SPA-ERRCD
                   END-IF
      *            非表示でないこと
                   IF     (SPA-NAI-THYOJIKBN (IDX) =   "2" )
                      AND (SPA-GMN-NYUGAIKBN       =   "1" )
                       MOVE    "0022"              TO  SPA-ERRCD
                   END-IF
      *            非表示でないこと
                   IF     (SPA-NAI-THYOJIKBN (IDX) =   "3" )
                      AND (SPA-GMN-NYUGAIKBN       =   "2" )
                       MOVE    "0022"              TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO ) AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-MAE-ROUJIN      =   FLG-ATO-ROUJIN
      *            どちらかがが生保単独ので場合
                   IF    ((FLG-MAE-SEIHO       =   1   )  OR
                          (FLG-ATO-SEIHO       =   1   ))  AND
                          (FLG-MAE-ROUJIN      =   1   )   AND
                          (SPA-J02-ROUJIN      =   ZERO)
                       MOVE    "0024"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
      *        Ｈ２０．４から変更可能として、警告表示とする
               IF      (SPA-NAI-SRYYM      >=  "200804" )   AND
                       (SPA-ERRCD      NOT =   SPACE    )
                   MOVE    SPACE               TO  SPA-ERRCD
                   IF      SPA-J021-KEIFLG     =   ZERO
                       MOVE    "K999"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-J021-KEIFLG
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-J021-ATO-ERR
               IF      SPA-ERRCD           =   "0009"
                   CONTINUE
               ELSE
                   MOVE    "1"                 TO  SPA-J021-ATO-ERR
                   MOVE    2                   TO  SPA-J021-CUR
               END-IF
           END-IF
      *
           .
       40021-ATO-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       4002-DAY1-CHK-SEC                    SECTION.
      *
           IF      SPA-J021-DAY1       =   ZERO
               MOVE    SPA-J021-STDAY      TO  SPA-J021-DAY1
           END-IF
           IF      SPA-J021-DAY1       >   SPA-J021-ENDAY
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-J021-CUR
           END-IF
      *
           .
       4002-DAY1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       4002-DAY2-CHK-SEC                    SECTION.
      *
           IF      SPA-J021-DAY2       =   ZERO
               MOVE    SPA-J021-ENDAY      TO  SPA-J021-DAY2
           END-IF
           IF      SPA-J021-DAY1       >   SPA-J021-DAY2
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-J021-CUR
           END-IF
           .
       4002-DAY2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4003-KANREI-CHK-SEC                    SECTION.
      *
           IF      SPA-J021-MAE-HKNCOMBI   =   SPACE  OR  ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-J021-CUR
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-J021-ATO-HKNCOMBI   =   SPACE  OR  ZERO)
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-J021-CUR
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-J021-ATO-HKNCOMBI   =   SPA-J021-MAE-HKNCOMBI)
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-J021-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 40031-DAY-KIKAN-CHK-SEC
           END-IF
      *
           .
       4003-KANREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間チェック処理
      *****************************************************************
       40031-DAY-KIKAN-CHK-SEC                   SECTION.
      *
           IF      (SPA-J021-DAY1      <   SPA-J021-STDAY)  OR
                   (SPA-J021-DAY1      >   SPA-J021-ENDAY)
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-J021-CUR
           END-IF
      *
           IF      (SPA-ERRCD          =   SPACE         )  AND
                   (SPA-J021-DAY2      <   SPA-J021-STDAY)
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-J021-CUR
           END-IF
      *
      *    後の期間チェック
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX          >   SPA-J021-MAX)  OR
                             (SPA-ERRCD    NOT =   SPACE   )  OR
                             (FLG-OK       =   1    )
               IF      SPA-J021-ATO-HKNCOMBI   =
                                           SPA-J021-THKNCOMBI (IDX)
                   IF      SPA-NAI-TEKSTYMD (IDX)(1:6)
                                            =  SPA-NAI-SRYYM
                       IF     (SPA-NAI-TEKSTYMD (IDX)(7:2)
                                                   <=  SPA-J021-DAY1)
                           CONTINUE
                       ELSE
                           MOVE    "0017"              TO  SPA-ERRCD
                           MOVE    3                   TO  SPA-J021-CUR
                       END-IF
                   END-IF
                   IF      SPA-NAI-TEKEDYMD (IDX)(1:6)
                                            =  SPA-NAI-SRYYM
                       IF     (SPA-NAI-TEKEDYMD (IDX)(7:2)
                                                   >=  SPA-J021-DAY2)
                           CONTINUE
                       ELSE
                           MOVE    "0017"              TO  SPA-ERRCD
                           MOVE    4                   TO  SPA-J021-CUR
                       END-IF
                   END-IF
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF     (SPA-NYUGAIKBN       =   "2"   )  AND
                  (SPA-ERRCD           =   SPACE )
      *        外来の入院期間チェック
               IF      (SPA-NAI-NYUINYMD(1:6)  <=   SPA-NAI-SRYYM ) 
                  AND  (SPA-NAI-TAIINYMD(1:6)  >=   SPA-NAI-SRYYM ) 
                   PERFORM 400312-GAINYUIN-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 400311-JYURRK-CHK-SEC
           END-IF
           .
       40031-DAY-KIKAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の入院期間チェック処理
      *****************************************************************
       400312-GAINYUIN-CHK-SEC              SECTION.
      *
      *    外来の入院期間チェック
           PERFORM VARYING     IDX-N   FROM    SPA-J021-DAY1   BY  1
                   UNTIL      (IDX-N   >       SPA-J021-DAY2 )
                          OR  (SPA-ERRCD   NOT =   SPACE  )
               IF      SPA-NAI-NYUIN (IDX-N)   =   1
                   IF      (SPA-J021-MAE-HKNNUM(1:2)   =   "98" )  AND
                           (SPA-J021-ATO-HKNNUM(1:2)   =   "98" )
                       CONTINUE
                   ELSE
                       MOVE    "0023"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           .
       400312-GAINYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更後の保険組合せチェック処理
      *****************************************************************
       400311-JYURRK-CHK-SEC                   SECTION.
      *    後の保険で受診履歴の検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    SPA-J021-DAY1       TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    SPA-J021-DAY2       TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      SPA-J021-NAI-ACCT   =   2
                   CONTINUE
               ELSE
                   MOVE    "0018"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-J021-CUR
               END-IF
           END-IF
          .
       400311-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEA-SEC                   SECTION.
      *
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                  TO  SPA-J021-CUR
           .
       220-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
           MOVE    ZERO                TO  SPA-J021-CUR
           MOVE    SPACE               TO  WRK-MCP-WIDGET
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           MOVE    1                   TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-JIDCD
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ一括変更処理
      *****************************************************************
       30031-HENKOU-SYORI-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    保険組合検索
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  COMB-HKNCOMBINUM
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
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *    負担率
           IF      SPA-ERRCD           =   SPACE
      *        保険算定年齢チェック等
               MOVE    COMB-GAI-KFTNRATE   TO  WRK-PTFTNRATE
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
               MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
               MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-J021-ATO-HKNCOMBI
                                           TO  AGECHK-HKNCOMBINUM
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
      *?????   IF      AGECHK-FTNRATE  NOT =   ZERO
               IF      AGECHK-RC           =   ZERO
                   MOVE    AGECHK-FTNRATE      TO  WRK-PTFTNRATE
               END-IF
      *
      *        更新処理
               PERFORM 30032-DB-KOUSIN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NYUGAIKBN       =   "2"
                   MOVE    1               TO  SPA-J021-ENDFLG
      *            処理終了のメッセージを表示する
                   MOVE    "0010"          TO  SPA-ERRCD
                   PERFORM 510-ERRSET-SEC
               ELSE
                   PERFORM 210-BACK
               END-IF
           ELSE
      *        レセプトコメントを更新しない場合
               IF      SPA-ERRCD       =   "0020"
                   MOVE    9               TO  SPA-J021-ENDFLG
                   PERFORM 510-ERRSET-SEC
               ELSE
                   MOVE    ZERO            TO  SPA-J021-ENDFLG
                   PERFORM 510-ERRSET-SEC
               END-IF
           END-IF
           .
       30031-HENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間一括変更処理
      *****************************************************************
       30032-DB-KOUSIN-SEC          SECTION.
      *
      *    後の保険で受診履歴の検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    1                   TO  FLG-JYURRK-ATO
           ELSE
               MOVE    ZERO                TO  FLG-JYURRK-ATO
           END-IF
      *
      *    受診履歴
           PERFORM 300311-JYURRK-UPD-SEC
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-NYUGAIKBN       =   "1"   )
      *****   AND (SPA-J021-NAI-ACCT   =   1     )
      *        入院調剤料のみの診療会計（受診履歴なし）
               PERFORM 300315-SRYACCT-UPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        算定履歴
               PERFORM 300314-SANTEI-UPD-SEC
           END-IF
      *
      *
           IF     (SPA-J021-DAY1       =   SPA-J021-STDAY )  AND
                  (SPA-J021-DAY2       =   SPA-J021-ENDAY )  AND
                  (SPA-ERRCD           =   SPACE )
      *        外来時のみとする
             AND  (SPA-NYUGAIKBN       =   "2"   )
      *        レセプトコメント
               PERFORM 300315-RECECOM-UPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        主科設定
               PERFORM 300325-ORCSSYUKAMAIN-SEC
           END-IF
           .
       30032-DB-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴変更処理
      *****************************************************************
       300311-JYURRK-UPD-SEC          SECTION.
      *
      *    受診履歴
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    SPA-J021-DAY1       TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    SPA-J021-DAY2       TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           INITIALIZE                  WRK-GRP-ZAINUM-AREA
           INITIALIZE                  HZN-SRYACCTPLUS-REC
           PERFORM UNTIL    (FLG-JYURRK    =   1  )  OR
                            (SPA-ERRCD     NOT =   SPACE )
      *        内服逓減の剤
               IF      JYURRK-EDANUM       =   1
                   IF      HZN-ACCTP-ZAINUM    NOT =   ZERO
                       PERFORM 3003110-GRPZAINUM-SET-SEC
                   END-IF
                   INITIALIZE                  WRK-GRP-ZAINUM-AREA
                   INITIALIZE                  HZN-SRYACCTPLUS-REC
               END-IF
      *        診療会計更新
               PERFORM VARYING     IDJ     FROM    1   BY  1
                       UNTIL      (IDJ     >   15  )  OR
                                  (JYURRK-ZAINUM (IDJ) =   ZERO )
                   PERFORM 300321-SRYACCT-CHK-SEC
               END-PERFORM
      *
               IF     (SPA-ERRCD           =   SPACE ) AND
                      (JYURRK-NYUGAIKBN    =   "2"   )
      *            収納変更（伝票番号より）
      *            伝票番号がゼロの時、エラー
                   IF      JYURRK-DENPNUM      =   ZERO
                       MOVE    "0021"              TO  SPA-ERRCD
                   ELSE
                       PERFORM 300313-SYUNOU-UPD-SEC
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
      *            保険組合せ変更
                   MOVE    SPA-J021-ATO-HKNCOMBI
                                               TO  JYURRK-HKNCOMBINUM
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
                   MOVE    SPA-OPID            TO  JYURRK-OPID
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J021 JYURRK UPD ERR:"   MCP-RC
                               ",KEY:" JYURRK-KEY
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *    内服逓減の剤
           IF      HZN-ACCTP-ZAINUM    NOT =   ZERO
               PERFORM 3003110-GRPZAINUM-SET-SEC
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300311-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計変更処理
      *****************************************************************
       300321-SRYACCT-CHK-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  ACCT-HKNCOMBI
           MOVE    JYURRK-ZAINUM(IDJ)  TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           IF      (FLG-SRYACCT        =   ZERO )  AND
                   (SPA-ERRCD          =   SPACE)
               IF      SPA-J021-MAE-HKNCOMBI   =  ACCT-HKNCOMBI
                   PERFORM 3003211-SRYACCT-UPD-SEC
               END-IF
           END-IF
      *
      *    剤番号変更
           IF     (WRK-ZAINUM      NOT =   ZERO )  AND
                  (WRK-ZAINUM      NOT =   JYURRK-ZAINUM(IDJ))
               MOVE    WRK-ZAINUM          TO  JYURRK-ZAINUM(IDJ)
           END-IF
      *
           .
       300321-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計変更処理
      *****************************************************************
       3003211-SRYACCT-UPD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      ACCT-DAY (1 IDX2)   >   ZERO
                   IF     (IDX2            >=  SPA-J021-DAY1 ) AND
                          (IDX2            <=  SPA-J021-DAY2 )
                       MOVE    1           TO  FLG-CHK2
                   ELSE
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-IF
           END-PERFORM
      *
      *    対象の期間にあること
           MOVE    SRYACCT-REC         TO  HZN-SRYACCT-REC
      *    変更期間のみ算定
           MOVE    ZERO                TO  FLG-ACCT-END
           MOVE    ZERO                TO  WRK-ZAINUM
           IF     (FLG-CHK             =   ZERO )  AND
                  (FLG-CHK2            =   1    )
               IF      FLG-JYURRK-ATO      =   1
      *            同一剤の有無判定
                   PERFORM 30032111-ZAICHK-SEC
                   IF      WRK-ZAINUM          =   ZERO
      *                保険組合せのみの変更
                       PERFORM 30032110-HKNCOMBI-HEN-SEC
                       MOVE    ACCT-ZAINUM         TO  WRK-ZAINUM
                       MOVE    1                   TO  FLG-ACCT-END
                   END-IF
               ELSE
      *            保険組合せのみの変更
                   PERFORM 30032110-HKNCOMBI-HEN-SEC
                   MOVE    ACCT-ZAINUM         TO  WRK-ZAINUM
                   MOVE    1                   TO  FLG-ACCT-END
               END-IF
           END-IF
           IF     (FLG-CHK2            =   1    )  AND
                  (FLG-ACCT-END        =   ZERO )
      *        算定日削除
               MOVE    JYURRK-SRYYMD(7:2)  TO  IDX2
               COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                           -   ACCT-DAY (1 IDX2)
               PERFORM VARYING    IDZ      FROM    1   BY  1
                       UNTIL      IDZ      >   10
                   MOVE    ZERO            TO  ACCT-DAY (IDZ IDX2)
               END-PERFORM
      ***      MOVE    ZERO                TO  ACCT-DAY (1 IDX2)
      *        MOVE    ZERO                TO  ACCT-DAY (2 IDX2)
      *        MOVE    ZERO                TO  ACCT-DAY (3 IDX2)
      ****     MOVE    ZERO                TO  ACCT-DAY (4 IDX2)
               IF      ACCT-ZAIKAISU       =   ZERO
                   PERFORM 30032114-SRYACCT-DELETE-SEC
               ELSE
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J021 SRYACCT UPD ERR:"   MCP-RC
                               ",KEY:" ACCT-KEY
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        変更期間のみ算定
               IF      WRK-ZAINUM          =   ZERO
      *            同一剤の有無判定
                   PERFORM 30032111-ZAICHK-SEC
               END-IF
               IF      WRK-ZAINUM              =   ZERO
      *            同一剤なし
                   PERFORM 30032112-SRYACCT-INS-SEC
               ELSE
      *            同一剤あり
                   PERFORM 30032113-SRYACCT-UPD-SEC
               END-IF
           END-IF
      *
           IF      HZN-ACCT-ZAIREQFLG  =   "2"
               ADD     1               TO  IDX-GEN
               MOVE    WRK-ZAINUM      TO  WRK-GRP-ZAINUM(IDX-GEN)
           END-IF
      *
           .
       3003211-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せのみの変更処理
      *****************************************************************
       30032110-HKNCOMBI-HEN-SEC          SECTION.
      *
      *    保険組合せの変更
           MOVE    SPA-J021-ATO-HKNCOMBI  TO  ACCT-HKNCOMBI
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACCT UPD ERR:"   MCP-RC
                           ",KEY:" ACCT-KEY
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
           .
       30032110-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤の削除処理
      *****************************************************************
       30032114-SRYACCT-DELETE-SEC          SECTION.
      *
      *    診療行為
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACT DEL ERR:" MCP-RC
                       ",KEY:" SRY-KEY
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *    コメント
           INITIALIZE                  PTCOM-REC
           MOVE    ACCT-HOSPNUM        TO  PTCOM-HOSPNUM
           MOVE    ACCT-PTID           TO  PTCOM-PTID
           MOVE    ACCT-ZAINUM         TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 PTCOM  DEL ERR:" MCP-RC
                       ",KEY:" PTCOM-KEY
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *    診療会計
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACCT DEL ERR:" MCP-RC
                       ",KEY:"  ACCT-KEY
           END-IF
      *
      *    附加情報削除
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
           END-IF
      *
           .
       30032114-SRYACCT-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一剤の判定処理
      *****************************************************************
       30032111-ZAICHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX
      *    診療会計剤チェックサブ
           MOVE    SPA-GMN-PTID            TO  SPA-PTID
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    ACCT-SRYKA              TO  ORCSCZAICHK-SRYKA
           MOVE    ACCT-SRYYM              TO  ORCSCZAICHK-SRYYM
           MOVE    ACCT-SRYKBN             TO  ORCSCZAICHK-SRYKBN
           MOVE    SPA-J021-ATO-HKNCOMBI   TO  ORCSCZAICHK-HKNCOMBI
           MOVE    ACCT-SRYCDTOTAL         TO  ORCSCZAICHK-SRYCDTOTAL
           MOVE    ACCT-ZAITEN             TO  ORCSCZAICHK-ZAITEN
           MOVE    ACCT-MEISAISU           TO  ORCSCZAICHK-MEISAISU
           MOVE    ACCT-SURYOUTOTAL        TO  ORCSCZAICHK-SURYOUTOTAL
           MOVE    1                       TO  ORCSCZAICHK-ZAIKAISU
           MOVE    ACCT-JIHOKBN            TO  ORCSCZAICHK-JIHOKBN
      *
           PERFORM 300321111-SRYACT-HEN-SEC
      *
           CALL    "ORCSCZAICHK"       USING   SPA-AREA
                                               ORCSCZAICHKAREA
           MOVE    ORCSCZAICHK-ZAINUM  TO  WRK-ZAINUM
      *
           .
       30032111-ZAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       300321111-SRYACT-HEN-SEC          SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
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
               PERFORM 910-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               MOVE    SRY-SRYSYUKBN          TO  ORCSCZAICHK-SRYSYUKBN
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL  (IDX2        >   5   )  OR
                              (SRY-SRYCD(IDX2)     =   SPACE ) OR
                              (IDX                 >=  50 )
                   ADD     1                   TO  IDX
                   MOVE    SRY-SRYCD   (IDX2)  TO  ORCSCZAICHK-SRYCD
                                                             (IDX)
                   MOVE    SRY-SRYSURYO(IDX2)  TO  ORCSCZAICHK-SRYSURYO
                                                             (IDX)
                   MOVE    SRY-SRYKAISU(IDX2)  TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX)
      *            自費金額
                   MOVE    SRY-JIHIMONEY(IDX2) TO  ORCSCZAICHK-JIHIMONEY
                                                             (IDX)
      *            換算値数量
                   MOVE    SRY-KANSURYO(IDX2)  TO  ORCSCZAICHK-KANSURYO
                                                             (IDX)
      *            多剤投与・関連コメント
                   EVALUATE    SRY-MEISKYFLG (IDX2)
                       WHEN    "2"
                           MOVE    "1"         TO  ORCSCZAICHK-INPUTKBN
                                                             (IDX)
                       WHEN    "3"
                           MOVE    "2"         TO  ORCSCZAICHK-INPUTKBN
                                                             (IDX)
      *H25.3
      *            労災読み替え手技
                       WHEN    "R"
                           MOVE    "R"         TO  ORCSCZAICHK-INPUTKBN
                                                             (IDX)
                   END-EVALUATE
      *
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "8" ) OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "008")
                       MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                       MOVE    SRY-PTID            TO  PTCOM-PTID
                       MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                       MOVE    SRY-SRYCD (IDX2)    TO  PTCOM-SRYCD
                       MOVE    SRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
                       PERFORM 950-PTCOM-READ-SEC
                       IF      FLG-PTCOM           =   ZERO
                           MOVE    PTCOM-INPUTCOMENT   TO
                                               ORCSCZAICHK-INPUTCOMENT
                                                            (IDX)
                           MOVE    PTCOM-INPUTCHI-AREA TO
                                               ORCSCZAICHK-INPUTCHI-G
                                                            (IDX)
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300321111-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計追加処理
      *****************************************************************
       30032112-SRYACCT-INS-SEC          SECTION.
      *
           PERFORM 30032112-ZAINUM-SET-SEC
      *
           MOVE    HZN-SRYACCT-REC     TO  SRYACCT-REC
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    ZERO                TO  ACCT-ZAIKAISU
           IF      JYURRK-SRYYMD       =   SPACE
             PERFORM VARYING     IDX3    FROM    1   BY  1
                     UNTIL       IDX3    >   31
               IF     (IDX3            >=  SPA-J021-DAY1 ) AND
                      (IDX3            <=  SPA-J021-DAY2 )
                   ADD     ACCT-DAY (1 IDX3)   TO  ACCT-ZAIKAISU
               ELSE
                   PERFORM VARYING    IDZ      FROM    1   BY  1
                           UNTIL      IDZ      >   10
                       MOVE    ZERO        TO  ACCT-DAY (IDZ IDX3)
                   END-PERFORM
      **           MOVE    ZERO        TO  ACCT-DAY (1 IDX3)
      *            MOVE    ZERO        TO  ACCT-DAY (2 IDX3)
      *            MOVE    ZERO        TO  ACCT-DAY (3 IDX3)
      ***          MOVE    ZERO        TO  ACCT-DAY (4 IDX3)
               END-IF
             END-PERFORM
           ELSE
             MOVE    JYURRK-SRYYMD(7:2)  TO  IDX2
             PERFORM VARYING     IDX3    FROM    1   BY  1
                     UNTIL       IDX3    >   31
               IF      IDX3            =   IDX2
                   ADD     ACCT-DAY (1 IDX2)   TO  ACCT-ZAIKAISU
               ELSE
                   PERFORM VARYING    IDZ      FROM    1   BY  1
                           UNTIL      IDZ      >   10
                       MOVE    ZERO        TO  ACCT-DAY (IDZ IDX3)
                   END-PERFORM
      **           MOVE    ZERO        TO  ACCT-DAY (1 IDX3)
      *            MOVE    ZERO        TO  ACCT-DAY (2 IDX3)
      *            MOVE    ZERO        TO  ACCT-DAY (3 IDX3)
      ***          MOVE    ZERO        TO  ACCT-DAY (4 IDX3)
               END-IF
             END-PERFORM
           END-IF
      *    保険組合せの変更
           MOVE    SPA-J021-ATO-HKNCOMBI  TO  ACCT-HKNCOMBI
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACCT INS ERR:"   MCP-RC
                           ",KEY:" ACCT-KEY
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *    診療会計マスターから診療行為マスターを検索する
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    HZN-ACCT-ZAINUM     TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL  (IDX2        >   5   )  OR
                              (SRY-SRYCD(IDX2)     =   SPACE ) OR
                              (IDX                 >=  50 )
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "8" ) OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "008")
                       INITIALIZE                      PTCOM-REC
                       MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                       MOVE    SRY-PTID            TO  PTCOM-PTID
                       MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                       MOVE    SRY-SRYCD (IDX2)    TO  PTCOM-SRYCD
                       MOVE    SRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
                       PERFORM 950-PTCOM-READ-SEC
                       IF      FLG-PTCOM           =   ZERO
                           MOVE    ACCT-ZAINUM         TO  PTCOM-ZAINUM
      *
                           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
                           MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
                           MOVE    SPA-OPID            TO  PTCOM-OPID
      *
                           MOVE    PTCOM-REC           TO  MCPDATA-REC
                           MOVE    "DBINSERT"          TO  MCP-FUNC
                           MOVE    "tbl_ptcom"         TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                               SPA-AREA
                          IF      MCP-RC          NOT =   ZERO
                               DISPLAY "J021 PTCOM INS ERR:"   MCP-RC
                                       ",KEY:" PTCOM-KEY
                              MOVE    "0011"              TO  SPA-ERRCD
                          END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
               MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
               MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
               MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
               MOVE    SPA-OPID            TO  SRY-OPID
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 SRYACT INS ERR:"   MCP-RC
                           ",KEY:" SRY-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    附加情報追加
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    HZN-ACCT-ZAINUM     TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           PERFORM UNTIL       FLG-SRYACCTPLUS       =   1
      *
               MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
               IF      ACCTP-GRP-ZAINUM-G  NOT =   ZERO
                   MOVE    SRYACCTPLUS-REC     TO  HZN-SRYACCTPLUS-REC
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  ACCTP-CREYMD
               MOVE    SMCNDATE-YMD        TO  ACCTP-UPYMD
               MOVE    SMCNDATE-HMS        TO  ACCTP-UPHMS
               MOVE    SPA-OPID            TO  ACCTP-OPID
      *
               MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 SRYACCTPLUS INS ERR:"   MCP-RC
                           ",KEY:" ACCTP-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30032112-SRYACCT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計変更処理
      *****************************************************************
       30032112-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0019"          TO  SPA-ERRCD
               DISPLAY "J021 ZAINUM-SET ERR:" SPA-ERRCD
           ELSE
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
           MOVE    SPA-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 PTINF UPD ERR:"   MCP-RC
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF 
      *
           END-IF
      *
           .
       30032112-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計追加更新処理
      *****************************************************************
       30032113-SRYACCT-UPD-SEC          SECTION.
     *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT         =   ZERO
               IF      SPA-J021-ATO-HKNCOMBI   =   ACCT-HKNCOMBI
                   PERFORM 300321131-SRYACCT-HENUPD-SEC
               END-IF
           END-IF
      *
           .
       30032113-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計変更追加処理
      *****************************************************************
       300321131-SRYACCT-HENUPD-SEC          SECTION.
      *
           IF      JYURRK-SRYYMD       =   SPACE
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL       IDX3    >   31
                   IF     (IDX3            >=  SPA-J021-DAY1 ) AND
                          (IDX3            <=  SPA-J021-DAY2 )
      **v.4.8          ADD     HZN-ACCT-DAY (1 IDX3)
      *                                        TO  ACCT-DAY (1 IDX3)
      *                ADD     HZN-ACCT-DAY (2 IDX3)
      *                                        TO  ACCT-DAY (2 IDX3)
      *                ADD     HZN-ACCT-DAY (3 IDX3)
      *                                        TO  ACCT-DAY (3 IDX3)
      *                ADD     HZN-ACCT-DAY (4 IDX3)
      *****                                    TO  ACCT-DAY (4 IDX3)
                       ADD     HZN-ACCT-DAY (1 IDX3)
                                               TO  ACCT-ZAIKAISU
                       PERFORM VARYING    IDZ      FROM    1   BY  1
                               UNTIL      IDZ      >   10
                           ADD     HZN-ACCT-DAY (IDZ IDX3)
                                               TO  ACCT-DAY (IDZ IDX3)
                       END-PERFORM
                   END-IF
               END-PERFORM
           ELSE
               MOVE    JYURRK-SRYYMD(7:2)  TO  IDX2
      **v4.8   ADD     HZN-ACCT-DAY (1 IDX2)   TO  ACCT-DAY (1 IDX2)
      *        ADD     HZN-ACCT-DAY (2 IDX2)   TO  ACCT-DAY (2 IDX2)
      *        ADD     HZN-ACCT-DAY (3 IDX2)   TO  ACCT-DAY (3 IDX2)
      ****     ADD     HZN-ACCT-DAY (4 IDX2)   TO  ACCT-DAY (4 IDX2)
               ADD     HZN-ACCT-DAY (1 IDX2)   TO  ACCT-ZAIKAISU
               PERFORM VARYING    IDZ      FROM    1   BY  1
                       UNTIL      IDZ      >   10
                   ADD     HZN-ACCT-DAY (IDZ IDX2)
                                              TO  ACCT-DAY (IDZ IDX2)
               END-PERFORM
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SRYACCT UPD ERR:"   MCP-RC
                           ",KEY:" ACCT-KEY
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
           .
       300321131-SRYACCT-HENUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納変更処理
      *****************************************************************
       300313-SYUNOU-UPD-SEC          SECTION.
      *
      *    収納
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-SYUNOU          =   ZERO  )  AND
                  (SYU-HKNCOMBINUM     =   SPA-J021-MAE-HKNCOMBI)
               PERFORM 3003131-SYUNOU-UPDATE-SEC
           ELSE
      *        収納が無い時、エラー
               IF      FLG-SYUNOU          =   1
                   MOVE    "0021"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       300313-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納変更処理
      *****************************************************************
       3003131-SYUNOU-UPDATE-SEC          SECTION.
      *    会計変更区分
           MOVE    "1"                 TO  SYU-ACCT-UPDKBN
      *
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  SYU-HKNCOMBINUM
           MOVE    WRK-PTFTNRATE       TO  SYU-PTFTNRATE
      *H28.12
           IF     (SYU-SYUHKNNUM   NOT =   SPACE )
              AND (SYU-KOH1HKNNUM      =   SPACE )
              AND (COMB-HKNNUM     NOT =   SPACE )
              AND (COMB-KOH1HKNNUM NOT =   SPACE )
               MOVE    SPACE               TO  SYU-SAIKEISANKBN
           END-IF
      *
      *    保険情報
           MOVE    COMB-HKNNUM         TO  SYU-SYUHKNNUM
           MOVE    COMB-KOH1HKNNUM     TO  SYU-KOH1HKNNUM
           MOVE    COMB-KOH2HKNNUM     TO  SYU-KOH2HKNNUM
           MOVE    COMB-KOH3HKNNUM     TO  SYU-KOH3HKNNUM
           MOVE    COMB-KOH4HKNNUM     TO  SYU-KOH4HKNNUM
      *
           MOVE    ZERO                TO  SYU-SYUHKNFTNMONEY
                                           SYU-SYUCOMPFTN
                                           SYU-SYUCOMPFTN-ENTANI
                                           SYU-SYUCOMPYKZFTN
                                           SYU-SYUCOMPTOTAL
           MOVE    SPACE               TO  SYU-SYUYKZFTNKBN
      *
           PERFORM   VARYING   IDX4   FROM   1    BY   1
                      UNTIL    IDX4    >     4
             MOVE  ZERO     TO   SYU-KOH-HKNFTNMONEY(IDX4)
                                 SYU-KOH-COMPFTN(IDX4)
                                 SYU-KOH-COMPFTN-ENTANI(IDX4)
                                 SYU-KOH-COMPYKZFTN(IDX4)
                                 SYU-KOH-COMPTOTAL(IDX4)
             MOVE  SPACE    TO   SYU-KOH-YKZFTNKBN(IDX4)
           END-PERFORM
      *
      *    収納履歴処理
           PERFORM 3003131-SRYURRK-HEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
           MOVE    SPA-OPID            TO  SYU-OPID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J021 SYUNOU UPD ERR:"   MCP-RC
                       ",KEY:" SYU-KEY
                MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
           .
       3003131-SYUNOU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納履歴処理
      *****************************************************************
       3003131-SRYURRK-HEN-SEC           SECTION.
      *
           INITIALIZE                  SS002-AREA
           MOVE    "1"                 TO  SS002-UPDKBN
           CALL    "ORCSS002"          USING    SS002-AREA
                                                SYUNOU-REC
                                                SPA-AREA
      *
           .
       3003131-SRYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服逓減の剤処理
      *****************************************************************
       3003110-GRPZAINUM-SET-SEC          SECTION.
      *
      *    附加情報追加
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    HZN-ACCTP-HOSPNUM   TO  ACCTP-HOSPNUM
           MOVE    HZN-ACCTP-NYUGAIKBN TO  ACCTP-NYUGAIKBN
           MOVE    HZN-ACCTP-PTID      TO  ACCTP-PTID
           MOVE    HZN-ACCTP-SRYKA     TO  ACCTP-SRYKA
           MOVE    HZN-ACCTP-SRYYM     TO  ACCTP-SRYYM
           MOVE    HZN-ACCTP-ZAINUM    TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    1                   TO  IDX-STR
           PERFORM UNTIL       FLG-SRYACCTPLUS       =   1
               INITIALIZE                  ACCTP-GRP-ZAINUM-G
               MOVE    ZERO                TO  IDX-GEN
               PERFORM VARYING     IDX-GEN2    FROM    IDX-STR BY  1
                       UNTIL      (IDX-GEN2    >   50 )  OR
                                  (WRK-GRP-ZAINUM(IDX-GEN2)  =  ZERO) OR
                                  (IDX-GEN     >=  15  )
                   ADD     1               TO  IDX-GEN
                   MOVE    WRK-GRP-ZAINUM(IDX-GEN2)
                                           TO  ACCTP-GRP-ZAINUM(IDX-GEN)
               END-PERFORM
               MOVE    IDX-GEN2            TO  IDX-STR
      *
               MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 SRYACCTPLUS UPD ERR:"   MCP-RC
                           ",KEY:" ACCTP-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3003110-GRPZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調剤料のみの診療会計（受診履歴なし）処理
      *****************************************************************
       300315-SRYACCT-UPD-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM
               UNTIL  (FLG-SRYACCT     =   1   )  OR
                      (SPA-ERRCD   NOT =   SPACE)
               PERFORM 3003151-SRYACCT-ONLY-SEC
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       300315-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計変更処理
      *****************************************************************
       3003151-SRYACCT-ONLY-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      ACCT-DAY (1 IDX2)   >   ZERO
                   IF     (IDX2            >=  SPA-J021-DAY1 ) AND
                          (IDX2            <=  SPA-J021-DAY2 )
                       MOVE    1           TO  FLG-CHK2
                   ELSE
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-IF
           END-PERFORM
      *
      *    対象の期間にあること
           MOVE    SRYACCT-REC         TO  HZN-SRYACCT-REC
      *    変更期間のみ算定
           MOVE    ZERO                TO  FLG-ACCT-END
           MOVE    ZERO                TO  WRK-ZAINUM
           IF     (FLG-CHK             =   ZERO )  AND
                  (FLG-CHK2            =   1    )
               IF      FLG-JYURRK-ATO      =   1
      *            同一剤の有無判定
                   PERFORM 30032111-ZAICHK-SEC
                   IF      WRK-ZAINUM          =   ZERO
      *                保険組合せのみの変更
                       PERFORM 30032110-HKNCOMBI-HEN-SEC
                       MOVE    ACCT-ZAINUM         TO  WRK-ZAINUM
                       MOVE    1                   TO  FLG-ACCT-END
                   END-IF
               ELSE
      *            保険組合せのみの変更
                   PERFORM 30032110-HKNCOMBI-HEN-SEC
                   MOVE    ACCT-ZAINUM         TO  WRK-ZAINUM
                   MOVE    1                   TO  FLG-ACCT-END
               END-IF
           END-IF
           IF     (FLG-CHK2            =   1    )  AND
                  (FLG-ACCT-END        =   ZERO )
      *        算定日削除
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   31
                   IF      ACCT-DAY (1 IDX2)   >   ZERO
                      IF     (IDX2            >=  SPA-J021-DAY1 ) AND
                             (IDX2            <=  SPA-J021-DAY2 )
                           COMPUTE ACCT-ZAIKAISU   =   ACCT-ZAIKAISU
                                                   -   ACCT-DAY (1 IDX2)
      **v.4.8              MOVE    ZERO            TO  ACCT-DAY (1 IDX2)
      *                    MOVE    ZERO            TO  ACCT-DAY (2 IDX2)
      *                    MOVE    ZERO            TO  ACCT-DAY (3 IDX2)
      ****                 MOVE    ZERO            TO  ACCT-DAY (4 IDX2)
                           PERFORM VARYING    IDZ      FROM    1   BY  1
                                   UNTIL      IDZ      >   10
                               MOVE    ZERO            TO  ACCT-DAY
                                                            (IDZ IDX2)
                           END-PERFORM
                       END-IF
                   END-IF
               END-PERFORM
               IF      ACCT-ZAIKAISU       =   ZERO
                   PERFORM 30032114-SRYACCT-DELETE-SEC
               ELSE
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J021 SRYACCT UPD ERR:"   MCP-RC
                               ",KEY:" ACCT-KEY
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        変更期間のみ算定
               IF      WRK-ZAINUM          =   ZERO
      *            同一剤の有無判定
                   PERFORM 30032111-ZAICHK-SEC
               END-IF
               MOVE    SPACE               TO  JYURRK-SRYYMD
               IF      WRK-ZAINUM          =   ZERO
      *            同一剤なし
                   PERFORM 30032112-SRYACCT-INS-SEC
               ELSE
      *            同一剤あり
                   PERFORM 30032113-SRYACCT-UPD-SEC
               END-IF
           END-IF
      *
           .
       3003211-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴変更処理
      *****************************************************************
       300314-SANTEI-UPD-SEC          SECTION.
      *
      *    算定履歴
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  SANTEI-HKNCOMBINUM 
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SANTEI-REC
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
           PERFORM
               UNTIL   (FLG-SANTEI     =   1    )  OR
                       (SPA-ERRCD  NOT =   SPACE )
               IF    ((SANTEI-SRYKA        NOT =   SPACE )   AND
                      (SPA-J01-SRYKA       NOT =   SANTEI-SRYKA )) OR
                     ((SANTEI-NYUGAIKBN    NOT =   SPACE ) AND
                      (SPA-NYUGAIKBN       NOT =   SANTEI-NYUGAIKBN ))
                   CONTINUE
               ELSE
      *            期間外がないこと
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       IF      SANTEI-DAY (IDX2)   >   ZERO
                           IF     (IDX2        >=  SPA-J021-DAY1 ) AND
                                  (IDX2        <=  SPA-J021-DAY2 )
                               CONTINUE
                           ELSE
                               MOVE    1           TO  FLG-CHK
                          END-IF
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK             =   ZERO
                       MOVE    SPA-J021-ATO-HKNCOMBI
                                               TO  SANTEI-HKNCOMBINUM
                   ELSE
                       MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                       PERFORM VARYING     IDX2    FROM    SPA-J021-DAY1
                                                   BY  1
                               UNTIL       IDX2    >   SPA-J021-DAY2
                           COMPUTE SANTEI-MSANTEICNT
                                               =   SANTEI-MSANTEICNT
                                               -   SANTEI-DAY (IDX2)
                           MOVE    ZERO        TO  SANTEI-DAY (IDX2)
                       END-PERFORM
                   END-IF
      *
                   MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
                   MOVE    SPA-OPID            TO  SANTEI-OPID
      *
                   MOVE    SANTEI-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J021 SANTEI UPD ERR:"   MCP-RC
                               ",KEY:" SANTEI-KEY
                       MOVE    "0011"              TO  SPA-ERRCD
                   END-IF
                   IF      FLG-CHK             =   ZERO
                       PERFORM 3003141-SANTEI-INS-SEC
                   END-IF
      *
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300314-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント変更処理
      *****************************************************************
       3003141-SANTEI-INS-SEC          SECTION.
      *
      *    期間外のクリア
           MOVE    ZERO                TO  HZN-SANTEI-MSANTEICNT
           MOVE    ZERO                TO  HZN-SANTEI-MSANTEID
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      HZN-SANTEI-DAY (IDX2)   >   ZERO
                   IF     (IDX2        >=  SPA-J021-DAY1 ) AND
                          (IDX2        <=  SPA-J021-DAY2 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  HZN-SANTEI-DAY (IDX2)
                   END-IF
               END-IF
               IF      SANTEI-DAY (IDX2)   >   ZERO
                   COMPUTE HZN-SANTEI-MSANTEICNT
                                           =   HZN-SANTEI-MSANTEICNT
                                           +   HZN-SANTEI-DAY (IDX2)
                   IF      HZN-SANTEI-MSANTEID     =   ZERO
                       MOVE    IDX2        TO  HZN-SANTEI-MSANTEID
                   END-IF
               END-IF
           END-PERFORM
      *    算定履歴
           INITIALIZE                  SANTEI-REC
           MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  SANTEI-HKNCOMBINUM 
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
               MOVE    SPACE               TO  SANTEI-REC
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SANTEI          =   1
               MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
               MOVE    SPA-J021-ATO-HKNCOMBI
                                           TO  SANTEI-HKNCOMBINUM 
      *
               MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
               MOVE    SPA-OPID            TO  SANTEI-OPID
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 SANTEI UPD ERR:"   MCP-RC
                           ",KEY:" SANTEI-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
           ELSE
               PERFORM VARYING IDX2    FROM    SPA-J021-DAY1 BY  1
                       UNTIL   IDX2    >   SPA-J021-DAY2
                   COMPUTE SANTEI-MSANTEICNT   =   SANTEI-MSANTEICNT
                                               +   HZN-SANTEI-DAY (IDX2)
                   ADD     HZN-SANTEI-DAY (IDX2)
                                               TO  SANTEI-DAY (IDX2)
                   IF      SANTEI-DAY (IDX2)   >   ZERO
                       IF      SANTEI-MSANTEID     >   IDX2
                           MOVE    IDX2        TO  SANTEI-MSANTEID
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
               MOVE    SPA-OPID            TO  SANTEI-OPID
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 SANTEI UPD ERR:"   MCP-RC
                           ",KEY:" SANTEI-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       3003141-SANTEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント変更処理
      *****************************************************************
       300315-RECECOM-UPD-SEC          SECTION.
      *
           INITIALIZE                  HZN-RECECOM-AREA
           MOVE    ZERO                TO  IDX-R
      *
      *    算定履歴
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  RECECOM-HKNCOMBI
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           PERFORM
               UNTIL   (FLG-RECECOM    =   1   )  OR
                       (SPA-ERRCD  NOT =   SPACE )
               ADD     1                   TO  IDX-R
               IF      IDX-R               >   40
                   MOVE    "0011"              TO  SPA-ERRCD
               ELSE
                   MOVE    RECECOM-REC     TO  HZN-RECECOM-REC(IDX-R)
               END-IF
      *
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (IDX-R               >   1  )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
      *
      *    診療科が同じか、ゼロのもの
           IF     (IDX-R               =   1    )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                 ((HZN-RECECOM-SRYKA(1)    =   ZERO )  OR
                  (HZN-RECECOM-SRYKA(1)    =   SPA-J01-SRYKA))
               PERFORM 3003150-ATO-RECECOM-SEC
               IF      FLG-RECECOM         =   1
                   PERFORM 3003151-RECECOM-UPDATE-SEC
               END-IF
           END-IF
      *
           .
       300315-RECECOM-UPD-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント変更処理
      *****************************************************************
       3003150-ATO-RECECOM-SEC           SECTION.
      *
      *    変更後の判定
           MOVE    HZN-RECECOM-REC(1)  TO  RECECOM-REC
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  RECECOM-HKNCOMBI
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3003150-ATO-RECECOM-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメント変更処理
      *****************************************************************
       3003151-RECECOM-UPDATE-SEC           SECTION.
      *
      *    １件だけの時、更新する
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM          TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-J021-MAE-HKNCOMBI
                                       TO  RECECOM-HKNCOMBI
      *
           MOVE    HZN-RECECOM-REC(1)  TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-RECECOM         =   ZERO
      *        削除
               MOVE    HZN-RECECOM-REC(1)  TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        保険変更
               MOVE    HZN-RECECOM-REC(1)  TO  RECECOM-REC
               MOVE    SPA-J021-ATO-HKNCOMBI
                                           TO  RECECOM-HKNCOMBI
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J021 RECECOM UPD ERR:"   MCP-RC
                           ",KEY:" RECECOM-KEY
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       3003151-RECECOM-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       300325-ORCSSYUKAMAIN-SEC             SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    "03"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-NAI-SRYYM       TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           .
      *
       300325-ORCSSYUKAMAIN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J021"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE   "J02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *H29.2
      *****************************************************************
      *    HAORI対応処理
      *****************************************************************
       300-HAORI-SYORI-SEC             SECTION.
      *
           EVALUATE    MCP-EVENT
               WHEN    "INIT"
      *            初期設定
                   INITIALIZE                  SPA-J021-AREA
      *            保険組合せ
                   PERFORM 3101-HKNCOMBI-HEN-SEC
               WHEN    "CHK"
      *            チェック
                   MOVE    1                   TO  FLG-KOUSIN
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
               WHEN    "UPD"
      *            保険組合せ一括変更処理
                   PERFORM 3001-HORI-UPD-SYORI-SEC
           END-EVALUATE
           .
      *
       300-HAORI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   （HAORI）保険組合せ一括変更処理
      *****************************************************************
       3001-HORI-UPD-SYORI-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    保険組合検索
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    SPA-J021-ATO-HKNCOMBI
                                       TO  COMB-HKNCOMBINUM
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
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *    負担率
           IF      SPA-ERRCD           =   SPACE
      *        保険算定年齢チェック等
               MOVE    COMB-GAI-KFTNRATE   TO  WRK-PTFTNRATE
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
               MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
               MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-J021-ATO-HKNCOMBI
                                           TO  AGECHK-HKNCOMBINUM
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      AGECHK-RC           =   ZERO
                   MOVE    AGECHK-FTNRATE      TO  WRK-PTFTNRATE
               END-IF
      *
      *        更新処理
               PERFORM 30032-DB-KOUSIN-SEC
           END-IF
           .
       3001-HORI-UPD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           IF     (SPA-JIDCD       NOT =   SPACE ) AND
                  (FLG-END             =   ZERO  )
               PERFORM 520-JIDSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    "CURRENT"           TO  MCP-PUTTYPE
               MOVE    "J021"              TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
           END-IF
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "変更前の保険組合せがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "この保険組合せでは受診はありません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "変更後の保険組合せがありません"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   STRING  "変更前と後の一般・老人判定が違います。"
                                               DELIMITED  BY  SIZE
                           "診療行為の訂正で変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   MOVE    "生活保護単独は一般・老人の判別ができま"
                                       TO  SPA-ERRMSG
                   MOVE    "せん。診療行為の訂正で変更して下さい。"
                                       TO  SPA-ERRMSG(39:)
               WHEN    "0024"
                   STRING  "一般・老人の判別ができない保険組合せです。"
                                               DELIMITED  BY  SIZE
                           "診療行為の訂正で変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   MOVE    "変更前保険組合せを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "変更後保険組合せを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "変更前と変更後が同じです。変更して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "変更後の保険組合せは対象外です。"
                                       TO  SPA-ERRMSG
                   MOVE    "変更して下さい"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0009"
                   MOVE    "変更後の保険組合せは受診日に対象では"
                                       TO  SPA-ERRMSG
                   MOVE    "ありません。変更して下さい"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0012"
                   MOVE    "変更後の保険組合せは削除分です。"
                                       TO  SPA-ERRMSG
                   MOVE    "変更して下さい"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0022"
                   STRING  "変更後の保険組合せは非表示です。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   MOVE    "保険組合せの変更が終了しました。"
                                       TO  SPA-ERRMSG
                   MOVE    "収納更新で請求額の変更して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0011"
                   MOVE    "変換エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "開始日での受診がありません。"
                                       TO  SPA-ERRMSG
      ******** WHEN    "0014"
      *            MOVE    "終了日が違います。"
      ********                         TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "開始日＜終了日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "期間内に受診はありません。変更できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "変更後の保険組合せが期間内にありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "変更して下さい。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "0018"
                   MOVE    "変更後の保険組合せで既に受診があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "診療行為で変更して下さい。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "0019"
                   MOVE    "剤番号が取得できませんでした。"
                                       TO  SPA-ERRMSG
                   MOVE    "患者情報を確認して下さい。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "0020"
                   MOVE    "レセプトコメントが複数あります。"
                                       TO  SPA-ERRMSG
                   MOVE    "レセプトコメントは更新しませんでした。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0021"
                   MOVE    "受診履歴の伝票番号がゼロのものがあります。"
                                       TO  SPA-ERRMSG
                   MOVE    "変更できません。"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "0023"
                   STRING  "入院期間中です。"
                           "外来は自費保険以外の変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K999"
                   STRING  "警告！！"
                           "変更前と後の一般・老人判定が違います。"
                           "変更後、診療内容を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J021"              TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   MOVE    "保険組合せをすべて変更します。"
                                       TO  WRK-JIDMSG
                   MOVE
                   "変更後の取消しはできません。よろしいですか？"
                                       TO  WRK-JIDMSG(31:)
               WHEN    OTHER
                   MOVE    SPA-JIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J021"              TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    診療会計マスター次読込
      *****************************************************************
       980-SRYACCT-READ-SEC         SECTION.
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
       980-SRYACCT-READ-EXT.
           EXIT.
      *
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
       910-SRYACT-READ-SEC         SECTION.
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
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスター読込
      *****************************************************************
       950-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTCOM-REC
                   MOVE    ZERO               TO  FLG-PTCOM
               ELSE
                   INITIALIZE                      PTCOM-REC
                   MOVE    1                  TO  FLG-PTCOM
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント読込
      *****************************************************************
       920-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RECECOM-REC
               MOVE    ZERO            TO  FLG-RECECOM
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           .
       920-RECECOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
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
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計附加マスター次読込
      *****************************************************************
       910-SRYACCTPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       910-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
