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
       PROGRAM-ID.             ORCSCH30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 薬剤情報印刷用パラメタ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/11    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/09/04  種別区分の追加
      * 01.00.02     NACL-多々納  02/09/17  処方のみの時院内処方とする
      * 01.00.03     NACL-多々納  02/10/04  逓減を対象外とする
      * 01.00.04     NACL-多々納  02/10/25  入院対応
      * 01.00.05     NACL-多々納  04/03/04  用法複数対応
      * 01.00.06     NACL-多々納  04/10/14  複数科保険対応
      * 01.00.07     NACL-多々納  05/09/28  ドクター複数科対応
      * 01.00.08     NACL-多々納  05/11/08  再印刷他対応
      *                                     MONFUNC対応
      * 01.00.09     NACL-多々納  06/01/24  用量割合コード追加
      * 04.01.00     NACL-多々納  07/12/11  起床追加
      * 04.03.00     NACL-多々納  08/10/31  包括保険を対象とする
      * 04.05.00     NACL-多々納  09/07/23  在宅薬剤、コメント対象対応
      * 04.05.00     NACL-多々納  09/09/XX  '0086'コメントコード対応他
      * 04.05.00     NACL-多々納  09/10/XX  換算値数量対応
      * 04.06.00     NACL-多々納  11/02/22  用法コメント対応
      * 04.07.00     NACL-多々納  12/11/02  薬剤毎のコメント対応
      * 04.07.00     NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      * 04.08.00     NACL-多々納  16/10/XX  用法コード服用時点対応
      * 04.08.00     NACL-多々納  17/02/XX  自費コード薬剤対応
      * 05.01.01     NMED-太田    19/09/19  API-PDF印刷対応
      * 05.00.00     NACL-多々納  20/02/XX  コメント、入力値桁数変更
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-YSET            PIC 9(01).
           03  FLG-YKJOKUKBN       PIC 9(01).
           03  FLG-INNAI           PIC 9(01).
      *
           03  FLG-NO              PIC 9(01).
           03  FLG-INPUTCHI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXY2               PIC 9(04).
           03  IDT                 PIC 9(04).
           03  IDXC                PIC 9(04).
           03  IDXD                PIC 9(04).
           03  IDXC2               PIC 9(04).
      *
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
      *
           03  IDX-SST             PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDXS2               PIC 9(04).
      *
           03  YOHO-MAX            PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
      *    剤毎の編集内容
           03  WRK-TBL-AREA.
               05  WRK-TBL-OCC             OCCURS   99.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-SURYO           PIC 9(05)V9(05).
                   07  TBL-KANSURYO        PIC 9(05)V9(05).
                   07  TBL-YOURYOU         PIC 9(05)V9(05)
                                           OCCURS   5.
                   07  TBL-SRYKAISU        PIC 9(03).
      *            用法コメント
                   07  TBL-YCOMMENT-OCC         OCCURS   20.
                       09  TBL-YCOMMENT             PIC X(09).
      *R02.4           09  TBL-YINPUTCOMENT         PIC X(80).
                       09  TBL-YINPUTCOMENT         PIC X(140).
                       09  TBL-YCOM-CHK             PIC X(01).
      *H28.10          服用数値
                       09  TBL-YCOM-SURYO-G.
                           11  TBL-YCOM-SURYO       PIC 9(05)V9(5)
                                                    OCCURS  5.
               05  WRK-YSRYCD              PIC X(09).
      *R02.4   05  WRK-YSRYCD-NAME         PIC X(80).
               05  WRK-YSRYCD-NAME         PIC X(140).
      *H28.10  服用数値
               05  WRK-YSRYCD-SURYO-G.
                   07  WRK-YSRYCD-SURYO     PIC 9(05)V9(5)
                                                   OCCURS  5.
               05  WRK-YOHOU-G.
                   07  WRK-YOHOU-TBL       OCCURS  10.
                       09  WRK-YOHOU           PIC X(09).
      *R02.4           09  WRK-YOHOU-NAME      PIC X(80).
                       09  WRK-YOHOU-NAME      PIC X(140).
                       09  WRK-YOHOU-KBN       PIC X(01).
      *H28.10          服用数値
                       09  WRK-YOHOU-SURYO-G.
                           11  WRK-YOHOU-SURYO     PIC 9(05)V9(5)
                                                   OCCURS  5.
      *        コメント
               05  TBL-COMMENT-G           OCCURS   20.
                   07  TBL-COMMENT             PIC X(09).
      *R02.4       07  TBL-INPUTCOMENT         PIC X(80).
                   07  TBL-INPUTCOMENT         PIC X(140).
                   07  TBL-IDX-S               PIC 9(04).
                   07  TBL-IDXD                PIC 9(04).
      *H28.10
               05  TBL-CDCNT            PIC 9(04).
      *
           03  WRK-SRYNAME              PIC X(80).
      *
           03  WRK-ZAITENKEI            PIC 9(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-KAISU           PIC 9(03).
      *    保険組合せ
           03  WRK-HKNCOMBINUM         PIC X(04).
      *    プログラム名
           03  WRK-YAKJYO-PG       PIC X(12).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-ORC-DBPATH      PIC X(64).
      *
           03  WRK-DRCD            PIC X(05).
           03  WRK-HZN-DRCD        PIC X(05).
           03  WRK-WKSRY-DRCD      PIC X(05).
      *H28.10
           03  WRK-INPUTCHI-AREA.
      *R02     05  WRK-INPUTCHI        PIC X(08)
               05  WRK-INPUTCHI        PIC X(10)
                                       OCCURS   5.
           03  WRK-INPUTCHISURYO-AREA.
               05  WRK-INPUTCHI-SURYO  PIC 9(5)V9(05)
                                       OCCURS   5.
           03  WRK-INPUTCHISURYO-AREA2.
               05  WRK-INPUTCHI-SURYO2 PIC 9(5)V9(05)
                                       OCCURS   5.
      *
           03  WRK-SSTKIJUNCD-AREA.
               05  WRK-SSTKIJUNCD      PIC 9(04)
                                       OCCURS 10.
      *
      *    剤内容テーブル
       01  TBL-SRYACT-GRP-AREA.
           03  TBL-ACCT-ZAITEN         PIC 9(07).
           03  TBL-ACCT-KAISU          PIC 9(03).
           03  TBL-ACCT-ZAINUM         PIC 9(08).
           03  TBL-SRYACT-TBL          OCCURS   50.
      *診療コード
               05  TBL-SRY-SRYCD           PIC  X(09).
      *数量
               05  TBL-SRY-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  TBL-SRY-SRYKAISU        PIC  9(03).
      *明細請求区分
               05  TBL-SRY-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  TBL-SRY-AUTOKBN         PIC  X(01).
      *換算値数量
               05  TBL-SRY-KANSURYO        PIC  9(05)V9(05).
      *名称入力番号
               05  TBL-SRY-INPUTNUM        PIC  9(03).
      *名称
      *R02.4   05  TBL-SRY-COM-NAME        PIC  X(80).
               05  TBL-SRY-COM-NAME        PIC  X(140).
      *用法数量
               05  TBL-SRY-INPUTCHI-G.
      *R02.4       07  TBL-SRY-INPUTCHI    PIC X(08)
                   07  TBL-SRY-INPUTCHI    PIC X(10)
                                           OCCURS   5.
      *
           03  TBL-SRY-MAX                 PIC 9(03).
      *
      *H26.10
      * 向精神薬多剤投与減算コード
       01  CONS-TAZAI-SRYCD        PIC X(09)   VALUE    "630010005".
      * （精減）
       01  CONS-SGEN-SRYCD         PIC X(09)   VALUE    "820000166".
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *
      *    診療行為管理情報 
           COPY    "CPSK1038.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療行為マスタワーク
       01  HZN-SRYACT-REC.
           COPY    "CPSRYACT.INC"  REPLACING
                                     //SRY-// BY //HZN-SRY-//.
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
      *    薬情報出力パラメタ
           COPY    "CPORCHC30.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    パラメタ
           COPY    "CPORCSCH30.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCH30AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCHC30AREA
      *
           PERFORM 100-INIT-SYORI-SEC
      *
           EVALUATE    ORCSCH30-KBN
               WHEN    "1"
               WHEN    "2"
      *            更新時発行
                   PERFORM 100-SRYACT-SYORI-SEC
               WHEN    "3"
               WHEN    "4"
      *            中途終了時発行
                   PERFORM 200-WKSRYACT-SYORI-SEC
               WHEN    "5"
      *            内容変更発行
                   PERFORM 300-ORCSCH30-SYORI-SEC
           END-EVALUATE
      *
      *TEST
      *    DISPLAY "ORCSCH30-KBN:" ORCSCH30-KBN
      *    DISPLAY "ORCHC30-MAX:" ORCHC30-MAX
      *    PERFORM VARYING    IDX   FROM    1  BY  1
      *            UNTIL      IDX   >  ORCHC30-MAX
      *      DISPLAY "ORCHC30-SRYCD:" ORCHC30-SRYCD (IDX)
      *              ",ORCHC30-SURYO:" ORCHC30-SURYO (IDX)
      *              ",ORCHC30-YSRYCD:" ORCHC30-YSRYCD (IDX)
      *     DISPLAY "ORCHC30-INPUTSURYOU(1):"  ORCHC30-INPUTSURYOU
      *                                                 (IDX 1)
      *            ",(2):"  ORCHC30-INPUTSURYOU    (IDX 2)
      *            ",(3):"  ORCHC30-INPUTSURYOU    (IDX 3)
      *            ",(4):"  ORCHC30-INPUTSURYOU    (IDX 4)
      *            ",(5):"  ORCHC30-INPUTSURYOU    (IDX 5)
      *     DISPLAY  "ORCHC30-YOURYOU 1:" ORCHC30-YOURYOU (IDX 1)
      *              ",2:" ORCHC30-YOURYOU (IDX 2)
      *              ",3:" ORCHC30-YOURYOU (IDX 3)
      *              ",4:" ORCHC30-YOURYOU (IDX 4)
      *              ",ORCHC30-SRYKAISU:" ORCHC30-SRYKAISU (IDX)
      *      DISPLAY "ORCHC30-YSRYCD-NAME:"  ORCHC30-YSRYCD-NAME (IDX)
      *      DISPLAY "ORCHC30-YOHOU-NAME:"  ORCHC30-YOHOU-NAME (IDX 1)
      *              ",CD:" ORCHC30-YOHOU (IDX 1)
      *      DISPLAY "ORCHC30-COMMENT1:"  ORCHC30-COMMENT-CD (IDX 1)
      *                 "," ORCHC30-COMMENT-MEI (IDX 1)
      *                 "," ORCHC30-COMHENKBN (IDX 1)
      *      IF      ORCHC30-COMMENT-CD (IDX 2) NOT =   SPACE
      *      DISPLAY "ORCHC30-COMMENT2:"  ORCHC30-COMMENT-CD (IDX 2)
      *                 "," ORCHC30-COMMENT-MEI (IDX 2)
      *      END-IF
      *    END-PERFORM 
      *TEST
      *
      *    印刷処理へ
           IF      ORCHC30-MAX         >   ZERO
               PERFORM 300-HC30-PRINT-SEC
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期印刷処理
      *****************************************************************
       100-INIT-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-YKJOKUKBN
      *    システム管理検索（診療行為一括入金管理情報）
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1038-REC
      *            包括保険を対象とする
                   IF      SYS-1038-YKJOKUKBN  =   "1"
                       MOVE    1               TO  FLG-YKJOKUKBN
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       300-HC30-PRINT-SEC             SECTION.
      *
      *H29.6
      *HAORI対応
      *    IF      SPA-API-FLG         =   1
      *        ドクター変更
      *        MOVE    SPA-DRCD            TO  WRK-HZN-DRCD
      *        IF      WRK-DRCD        NOT =   SPACE
      *            MOVE    WRK-DRCD            TO  SPA-DRCD
      *        END-IF
      *        CALL    "ORCHC30"           USING
      *                                    SPA-AREA
      *                                    ORCHC30AREA
      *        MOVE    WRK-HZN-DRCD        TO  SPA-DRCD
      *        GO      TO      300-HC30-PRINT-EXT
      *    END-IF
      *
      *    薬情報出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000004"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               GO      TO      300-HC30-PRINT-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-YAKJYO-PG
      *
      *    ドクター変更
           MOVE    SPA-DRCD            TO  WRK-HZN-DRCD
           IF      WRK-DRCD        NOT =   SPACE
               MOVE    WRK-DRCD            TO  SPA-DRCD
           END-IF
      *
           CALL    WRK-YAKJYO-PG           USING
                                           SPA-AREA
                                           ORCHC30AREA
      *
           MOVE    WRK-HZN-DRCD        TO  SPA-DRCD
      *
           .
       300-HC30-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新時発行処理
      *****************************************************************
       100-SRYACT-SYORI-SEC             SECTION.
      *
           IF      ORCSCH30-DENPNUM    =   ZERO
               MOVE    ZERO                TO  WRK-RENNUM
               PERFORM 1001-RENNUM-SET-SEC
           ELSE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    ORCSCH30-DENPNUM    TO  WRK-DENPNUM
      *        編集
               PERFORM 1002-SYOHOU-SYORI-SEC
           END-IF
      *
           .
       100-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       1001-RENNUM-SET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-PRINT
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
               MOVE    ZERO                TO  FLG-SYORI
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   IF      SPA-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               END-IF
      *        包括保険対象外
               IF     (WRK-HKNCOMBINUM     =   "9999")
               AND    (FLG-YKJOKUKBN       =   ZERO  )
                   MOVE    ZERO                TO  FLG-SYORI
               END-IF
      *        編集
               IF      FLG-SYORI           =   1
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
                   PERFORM 1002-SYOHOU-SYORI-SEC
               END-IF
               IF      FLG-PRINT           =   1
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   MOVE    ZERO            TO  FLG-JYURRK
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-RENNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       1002-SYOHOU-SYORI-SEC          SECTION.
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
           IF      JYURRK-GRP-DENPNUM  =   ZERO
      *         単独分
                PERFORM 10021-ONLY-SYORI-SEC
           ELSE
      *         複数分
                PERFORM 10022-FUKU-SYORI-SEC
           END-IF
      *
           .
       1002-SYOHOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    単独分編集処理
      *****************************************************************
       10021-ONLY-SYORI-SEC          SECTION.
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *        包括保険対象外
               IF     (JYURRK-HKNCOMBINUM      =   "9999")
               AND    (FLG-YKJOKUKBN           =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 100211-JYURRK-CHK-SEC
               END-IF
      *
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10021-ONLY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    複数分編集処理
      *****************************************************************
       10022-FUKU-SYORI-SEC          SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    受診履歴検索
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
      *        包括保険対象外
               IF     (JYURRK-HKNCOMBINUM      =   "9999")
               AND    (FLG-YKJOKUKBN       =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 100211-JYURRK-CHK-SEC
               END-IF
      *
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10022-FUKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       100211-JYURRK-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU)
                                               =   ZERO )
               MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
      *ver.4.8
      ******   IF      IDX-REN         >   4
      *            MOVE    4               TO  IDX-REN
      ******   END-IF
               PERFORM 2002-SRYACT-SYORI-SEC
           END-PERFORM
      *
           .
       100211-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    WRK-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           INITIALIZE                  TBL-SRYACT-GRP-AREA
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-HEN
      *    投薬・在宅を対象
           IF      SRY-SRYKBN          =   "14"
                                       OR  "21"
                                       OR  "22"
                                       OR  "23"
      *H29.02
                                       OR  "95"
                                       OR  "96"
               MOVE    SRYACT-REC          TO  HZN-SRYACT-REC
               PERFORM
                   UNTIL   FLG-SRYACT      =   1
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )  OR
                                      (SRY-SRYCD (IDXA)   =   SPACE)
      *H26.10
                       MOVE    ZERO                TO  FLG-NO
                       IF     (SRY-SRYCD (IDXA)(1:1)      =   "6")
      *                    点数マスタを検索
                           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      TNS-TENSIKIBETU     =   7
      *                        減点は対象外
                               MOVE    1               TO  FLG-NO
                           ELSE
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
      *H29.2           自費薬剤のみ対象
                       IF      SRY-SRYKBN          =   "95"
                                                   OR  "96"
                           IF     (SRY-SRYCD (IDXA)(1:3)   =   "095"
                                                           OR  "096")
      *                        点数マスタを検索
                               MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                               PERFORM 910-TENSU-READ-SEC
                               IF      TNS-YKZKBN          =   ZERO
                                   MOVE    1               TO  FLG-NO
                               ELSE
                                   MOVE    1               TO  FLG-OK
                               END-IF
                           END-IF
                       END-IF
      *
                       IF     (SRY-SRYCD (IDXA)    =   "630010002"
                                                   OR  "630010004"
      *H25.4               （減）対象外
                                                   OR  "820000047"
      *H26.10               (精減）
                                                   OR  CONS-SGEN-SRYCD
                                                   OR  CONS-SPCM-SRYCD)
                       OR     (FLG-NO              =   1           )
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX
                           MOVE    SRY-SRYCD (IDXA)
                                                   TO  TBL-SRY-SRYCD
                                                                (IDX)
                           MOVE    SRY-SRYSURYO (IDXA)
                                                   TO  TBL-SRY-SRYSURYO
                                                                (IDX)
                           MOVE    SRY-SRYKAISU (IDXA)
                                                   TO  TBL-SRY-SRYKAISU
                                                                (IDX)
                           MOVE    SRY-MEISKYFLG(IDXA)
                                                   TO  TBL-SRY-MEISKYFLG
                                                                (IDX)
                           MOVE    SRY-AUTOKBN(IDXA)
                                                   TO  TBL-SRY-AUTOKBN
                                                                (IDX)
                           MOVE    SRY-INPUTNUM(IDXA)
                                                   TO  TBL-SRY-INPUTNUM
                                                                (IDX)
                           MOVE    SRY-KANSURYO (IDXA)
                                                   TO  TBL-SRY-KANSURYO
                                                                (IDX)
                           MOVE    IDX             TO  TBL-SRY-MAX
                       END-IF
                   END-PERFORM
      *
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    投薬・在宅で薬剤のある剤のみ対象
           IF      FLG-OK              =   1
               MOVE    HZN-SRYACT-REC      TO  SRYACT-REC
      *        診療会計テーブル編集
               PERFORM 20021-SRYACCT-SYORI-SEC
           END-IF
           IF      FLG-OK              =   1
               MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
               MOVE    SRY-ZAINUM          TO  TBL-ACCT-ZAINUM
               IF      ORCSCH30-KBN        =   "1"
      *            院内
                   MOVE    1               TO  FLG-INNAI
               ELSE
      *            院外
                   MOVE    2               TO  FLG-INNAI
               END-IF
      *        剤ごとのチェック・編集
               PERFORM 20022-ZAICHEK-SYORI-SEC
           END-IF
           IF      FLG-HEN             =   1
      *        最初のドクター
               IF      WRK-DRCD        =   SPACE
                   MOVE    JYURRK-DRCD     TO  WRK-DRCD
               END-IF
           END-IF
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (ACCT-ZAIKBN         =   1  )
              OR  (FLG-OK              =   ZERO )
      *        薬評は対象外
               MOVE    ZERO                TO  FLG-OK
           ELSE
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-ACT
               MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                           TO  TBL-ACCT-KAISU
               MOVE    ACCT-ZAITEN         TO  TBL-ACCT-ZAITEN
           END-IF
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎のチェック・テーブル編集処理
      *****************************************************************
       20022-ZAICHEK-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HEN
      *    在宅
           IF      WRK-SRYKBN          =   "14"
      *        院内
               IF      FLG-INNAI           =   1
                   IF      WRK-SRYSYUKBN       =   "148"
                                               OR  "149"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-HEN
                   END-IF
               ELSE
      *        院外
                   IF      WRK-SRYSYUKBN       =   "148"
                                               OR  "149"
                       MOVE    1               TO  FLG-HEN
                   END-IF
               END-IF
           ELSE
      *    投薬
      *        院内
               IF      FLG-INNAI           =   1
                   IF     (WRK-SRYSYUKBN       =   "213"
                                               OR  "223"
                                               OR  "233"
      *H28.4
                                               OR  "211"
                                               OR  "221"
                                               OR  "231"
                                               OR  "294" )  OR
                          (TBL-ACCT-ZAITEN NOT =   ZERO  )
                       MOVE    1               TO  FLG-HEN
                   END-IF
               ELSE
      *        院外
                   IF     (WRK-SRYSYUKBN       =   "213"
                                               OR  "223"
                                               OR  "233"
      *H28.4
                                               OR  "211"
                                               OR  "221"
                                               OR  "231"
                                               OR  "294" )  OR
                          (TBL-ACCT-ZAITEN NOT =   ZERO  )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-HEN
                   END-IF
               END-IF
           END-IF
      *H29.2
      *    自費は院内とする
           IF      WRK-SRYKBN          =   "95"
                                       OR  "96"
      *        院内
               IF      FLG-INNAI           =   1
                   MOVE    1               TO  FLG-HEN
               ELSE
                   MOVE    ZERO            TO  FLG-HEN
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-TBL-AREA
           INITIALIZE                      WRK-TBL-AREA
           MOVE    TBL-ACCT-KAISU      TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDXY
           MOVE    ZERO                TO  IDXC
           MOVE    SPACE               TO  WRK-YSRYCD
           MOVE    SPACE               TO  WRK-YSRYCD-NAME
           INITIALIZE                      WRK-YOHOU-G
           MOVE    ZERO                TO  IDXD
      *
           IF      FLG-HEN             =   1
               PERFORM VARYING     IDXA    FROM    1   BY  1
                        UNTIL     (IDXA    >   TBL-SRY-MAX   ) 
      *            コード毎の処理
                   PERFORM 20022-TBLSET-SEC
               END-PERFORM
      *        パラメタ編集
               PERFORM 2003-HC30TBL-SET-SEC
           END-IF
      *
           .
       20022-ZAICHEK-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       20022-TBLSET-SEC         SECTION.
      *
      *    薬剤編集
           IF     (TBL-SRY-SRYCD (IDXA)(1:1)   =   "6")
             OR   (TBL-SRY-SRYCD (IDXA)(1:3)   =   "095"
                                               OR  "096") 
               ADD     1               TO  IDX-S
               IF      IDX-S           >   99
                   DISPLAY "ORCSHC30 TBL OVER !!"
                   MOVE    99              TO  IDX-S
               END-IF
               MOVE    TBL-SRY-SRYCD (IDXA)    TO  TBL-SRYCD (IDX-S)
               MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-SURYO (IDX-S)
               MOVE    TBL-SRY-KANSURYO (IDXA) TO  TBL-KANSURYO (IDX-S)
      *        外用の日数
               IF     (WRK-SRYKBN          =   "23"  )  AND
                      (TBL-SRY-SRYKAISU (IDXA) >   1     )
                   MOVE    TBL-SRY-SRYKAISU (IDXA)
                                               TO  TBL-SRYKAISU (IDX-S)
               END-IF
               MOVE    ZERO                TO  IDXD
           END-IF
      *    用法編集
           IF      TBL-SRY-SRYCD (IDXA)(1:3)   =   "001"
      *H28.10
               ADD     1                   TO  TBL-CDCNT
      *
               MOVE    ZERO                TO  FLG-YSET
               INITIALIZE                  WRK-INPUTCHISURYO-AREA
      *        点数マスタを検索して、用法時点を判定する
               MOVE    TBL-SRY-SRYCD (IDXA)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (FLG-TENSU           =   ZERO      )
      *         コメントは対象外
               AND    (TNS-KENTAICOMMENT   =   0  OR  1  )
      *            コメント名称編集
                   IF     (TBL-SRY-INPUTNUM (IDXA) NOT =   ZERO )
                       PERFORM 200221-PTCOM-HEN-SEC
                       MOVE    PTCOM-INPUTCOMENT       TO  WRK-SRYNAME
      *H28.10          埋め込み数値変換
                       MOVE    PTCOM-INPUTCHI-AREA
                                                   TO  WRK-INPUTCHI-AREA
                       PERFORM 200221-PTCOM-INPUTCHI-SEC
                   ELSE
                       MOVE    TBL-SRY-COM-NAME (IDXA)
                                                       TO  WRK-SRYNAME
      *H28.10          埋め込み数値変換
                       MOVE    TBL-SRY-INPUTCHI-G (IDXA)
                                                   TO  WRK-INPUTCHI-AREA
                       PERFORM 200221-PTCOM-INPUTCHI-SEC
                   END-IF
      *
                   IF    ((TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(4)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(5)   =   ZERO ))
                       MOVE    ZERO                TO  FLG-YSET
                   ELSE
                       MOVE    1                   TO  FLG-YSET
                   END-IF
                   ADD     1                   TO  IDXY
                   IF      IDXY                <=  10
                       MOVE    TBL-SRY-SRYCD(IDXA) TO  WRK-YOHOU
                                                               (IDXY)
                       MOVE    WRK-SRYNAME         TO  WRK-YOHOU-NAME
                                                               (IDXY)
                       MOVE    FLG-YSET            TO  WRK-YOHOU-KBN
                                                               (IDXY)
      *H28.10          埋め込み数量
                       MOVE    WRK-INPUTCHISURYO-AREA
                                                   TO  WRK-YOHOU-SURYO-G
                                                               (IDXY)
                   END-IF
               END-IF
           END-IF
      *    用量割合コード
           IF     (TBL-SRY-SRYCD (IDXA)(1:7)   =   "0992000" ) AND
                  (IDX-S                   >   ZERO      )
               EVALUATE    TBL-SRY-SRYCD (IDXA)
      *            朝
                   WHEN    "099200011"
                       MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU
                                                          (IDX-S 1)
      *            昼
                   WHEN    "099200012"
                       MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU
                                                          (IDX-S 2)
      *            夜
                   WHEN    "099200013"
                       MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU
                                                          (IDX-S 3)
      *            寝
                   WHEN    "099200014"
                       MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU
                                                          (IDX-S 4)
      *            起床
                   WHEN    "099200015"
                       MOVE    TBL-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU
                                                          (IDX-S 5)
               END-EVALUATE
           END-IF
      *
      *    コメント
           IF     (TBL-SRY-SRYCD (IDXA)(1:1)   =   "8"   )   OR
                  (TBL-SRY-SRYCD (IDXA)(1:4)   =   "0082"
                                               OR  "0083"
                                               OR  "0084"
                                               OR  "0085" )
      *H28.10
               ADD     1                   TO  TBL-CDCNT
      *ver.4.7
               IF     (IDX-S                   >   ZERO  )
                 AND  (IDXD                    <   20    )
      *            薬剤の下のコメント
                   ADD     1                       TO  IDXD
                   MOVE    TBL-SRY-SRYCD (IDXA)    TO  TBL-YCOMMENT
                                                       (IDX-S IDXD)
                   IF     (TBL-SRY-INPUTNUM (IDXA) NOT =   ZERO )
                       PERFORM 200221-PTCOM-HEN-SEC
                       MOVE    PTCOM-INPUTCOMENT   TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
                   ELSE
                       MOVE    TBL-SRY-COM-NAME (IDXA)
                                                   TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
                   END-IF
               END-IF
      *ver.4.7
      *
      *        すべての薬剤
               IF      IDXC                <   20
                   ADD     1                       TO  IDXC
                   MOVE    TBL-SRY-SRYCD (IDXA)    TO  TBL-COMMENT(IDXC)
                   IF     (TBL-SRY-INPUTNUM (IDXA) NOT =   ZERO )
                        PERFORM 200221-PTCOM-HEN-SEC
                       MOVE    PTCOM-INPUTCOMENT   TO  TBL-INPUTCOMENT
                                                              (IDXC)
                   ELSE
                       MOVE    TBL-SRY-COM-NAME (IDXA)
                                                   TO  TBL-INPUTCOMENT
                                                             (IDXC)
                   END-IF
               END-IF
      *ver.4.7
               IF      IDX-S                   >   ZERO
      *            薬剤あり
                   MOVE    IDX-S               TO  TBL-IDX-S (IDXC)
                   MOVE    IDXD                TO  TBL-IDXD (IDXC)
               END-IF
      *ver.4.7
           END-IF
      *    用法コメント
           IF     (TBL-SRY-SRYCD (IDXA)(1:3)   =   "001" ) AND
                  (TNS-KENTAICOMMENT           =   2     ) AND
                  (IDX-S                       >   ZERO  )
            AND   (IDXD                        <   20    )
               ADD     1                       TO  IDXD
               MOVE    TBL-SRY-SRYCD (IDXA)    TO  TBL-YCOMMENT
                                                       (IDX-S IDXD)
               IF     (TBL-SRY-INPUTNUM (IDXA) NOT =   ZERO )
                   PERFORM 200221-PTCOM-HEN-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
      *H28.10      埋め込み数値変換
                   MOVE    PTCOM-INPUTCHI-AREA TO  WRK-INPUTCHI-AREA
                   PERFORM 200221-PTCOM-INPUTCHI-SEC
                   MOVE    WRK-INPUTCHISURYO-AREA
                                               TO  TBL-YCOM-SURYO-G
                                                       (IDX-S IDXD)
               ELSE
                   MOVE    TBL-SRY-COM-NAME (IDXA) TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
      *H28.10      埋め込み数値変換
                   MOVE    TBL-SRY-INPUTCHI-G (IDXA)
                                                   TO  WRK-INPUTCHI-AREA
                   PERFORM 200221-PTCOM-INPUTCHI-SEC
                   MOVE    WRK-INPUTCHISURYO-AREA
                                               TO  TBL-YCOM-SURYO-G
                                                       (IDX-S IDXD)
               END-IF
           END-IF
           .
       20022-TBLSET-EXT.
           EXIT.
      *****************************************************************
      *    コメントテーブル編集処理
      *****************************************************************
       200221-PTCOM-HEN-SEC         SECTION.
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SPA-PTID            TO  PTCOM-PTID
           MOVE    TBL-ACCT-ZAINUM     TO  PTCOM-ZAINUM
           MOVE    TBL-SRY-SRYCD    (IDXA)
                                       TO  PTCOM-SRYCD
           MOVE    TBL-SRY-INPUTNUM (IDXA)
                                       TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           .
       200221-PTCOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントテーブル埋め込み数値変換編集処理
      *****************************************************************
       200221-PTCOM-INPUTCHI-SEC         SECTION.
      *
           INITIALIZE                  WRK-SSTKIJUNCD-AREA
           MOVE    1                   TO  IDXS
      *    起床(5)を先頭へ移動する
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   5
               IF      IDX-SST             <   5
                   ADD     1               TO  IDXS
               ELSE
                   MOVE    1               TO  IDXS
               END-IF
               MOVE    TNS-SSTKIJUNCD (IDX-SST)
                                       TO  WRK-SSTKIJUNCD (IDXS)
           END-PERFORM
      *
           MOVE    ZERO                TO  IDXS
           INITIALIZE                  WRK-INPUTCHISURYO-AREA
      *
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   5
              IF      WRK-SSTKIJUNCD (IDX-SST) NOT =   ZERO
                   ADD     1                   TO  IDXS
      *            数値変換
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-INPUTCHI (IDXS)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE ) 
                       MOVE    ZERO            TO  WRK-INPUTCHI-SURYO
                                                        (IDX-SST)
                   ELSE
                       MOVE    SNUM-OUTNUM     TO  WRK-INPUTCHI-SURYO
                                                        (IDX-SST)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    起床を（５）へ
           INITIALIZE                  WRK-INPUTCHISURYO-AREA2
           MOVE    ZERO                TO  IDXS
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   5
               IF      IDX-SST             =   1
                   MOVE    5               TO  IDXS
               ELSE
                   ADD     1               TO  IDXS
               END-IF
               MOVE    WRK-INPUTCHI-SURYO (IDX-SST)
                                       TO  WRK-INPUTCHI-SURYO2 (IDXS)
               IF      IDXS            =   5
                   MOVE    ZERO            TO  IDXS
               END-IF
           END-PERFORM
           MOVE    WRK-INPUTCHISURYO-AREA2 TO  WRK-INPUTCHISURYO-AREA
           .
       200221-PTCOM-INPUTCHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情報パラメタ編集処理
      *****************************************************************
       2003-HC30TBL-SET-SEC         SECTION.
      *
      *    用法は最後の用法コードとする
           MOVE    ZERO                TO  IDXY2
           MOVE    ZERO                TO  WRK-YSRYCD-SURYO-G
           PERFORM VARYING     IDXY    FROM    10  BY  -1
                   UNTIL      (IDXY        <   1      )
               IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
               AND    (WRK-YOHOU-KBN(IDXY)     =   "1"   )
                   MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   MOVE    WRK-YOHOU-NAME(IDXY)
                                               TO  WRK-YSRYCD-NAME
      *H28.10      服用数値
                   MOVE    WRK-YOHOU-SURYO-G (IDXY)
                                               TO  WRK-YSRYCD-SURYO-G
                   MOVE    SPACE               TO  WRK-YOHOU(IDXY)
                   MOVE    1                   TO  IDXY
               ELSE
                   IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
      ***********  AND    (IDXY2               =   ZERO      )
      *                １件目
                       MOVE    IDXY            TO  IDXY2
                   END-IF
               END-IF
           END-PERFORM
      *H25.4
      *    服用時点がなければ用法コードの１件目
           IF     (WRK-YSRYCD              =   SPACE )
              AND (IDXY2                   >   ZERO  )
               MOVE    WRK-YOHOU(IDXY2)    TO  WRK-YSRYCD
               MOVE    WRK-YOHOU-NAME(IDXY2)
                                           TO  WRK-YSRYCD-NAME
               MOVE    SPACE               TO  WRK-YOHOU(IDXY2)
               MOVE    4                   TO  YOHO-MAX
           ELSE
               MOVE    5                   TO  YOHO-MAX
           END-IF
      *
      *ver.4.7
      *    用法コードがないとき最後のコメントをすべてに
           IF     (WRK-YSRYCD          =   SPACE )
              AND (IDXC                >   ZERO  )
              AND (IDXY2               =   ZERO  )
      *        コメント
               IF     (TBL-COMMENT (IDXC)  NOT =   SPACE)
               AND    (TBL-IDX-S   (IDXC)  NOT =   ZERO )
                   MOVE    TBL-IDX-S   (IDXC)      TO  IDX
                   MOVE    TBL-IDXD    (IDXC)      TO  IDXC2
                   MOVE    SPACE           TO  TBL-YCOMMENT (IDX IDXC2)
                   MOVE    SPACE           TO  TBL-YINPUTCOMENT
                                                            (IDX IDXC2)
                   MOVE    ZERO            TO  TBL-IDX-S   (IDXC)
                   MOVE    ZERO            TO  TBL-IDXD    (IDXC)
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-S   )   OR
                              (ORCHC30-MAX     >=  99 )
               ADD     1                   TO  ORCHC30-MAX
               MOVE    ORCHC30-MAX         TO  IDY
               MOVE    TBL-SRYCD (IDX)     TO  ORCHC30-SRYCD (IDY)
               MOVE    TBL-SURYO (IDX)     TO  ORCHC30-SURYO (IDY)
               MOVE    TBL-KANSURYO (IDX)  TO  ORCHC30-KANSURYO (IDY)
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL       IDXY        >   5
                   MOVE    TBL-YOURYOU (IDX IDXY)
                                           TO  ORCHC30-YOURYOU
                                                            (IDY IDXY)
               END-PERFORM
               MOVE    TBL-SRYKAISU (IDX)  TO  ORCHC30-SRYKAISU (IDY)
               MOVE    WRK-KAISU           TO  ORCHC30-KAISU (IDY)
               MOVE    WRK-YSRYCD          TO  ORCHC30-YSRYCD(IDY)
               MOVE    WRK-YSRYCD-NAME     TO  ORCHC30-YSRYCD-NAME(IDY)
      *H28.10
      *        用法コードの数量
               MOVE    ZERO                TO  FLG-INPUTCHI
               PERFORM VARYING     IDXC2   FROM    1   BY  1
                       UNTIL      (IDXC2       >   10   )
                              OR  (FLG-INPUTCHI    =   1  )
                   IF     (TBL-YCOMMENT (IDX IDXC2)(1:3)   =   "001")
                    AND   (TBL-YCOM-SURYO-G (IDX IDXC2)
                                                       NOT =   ZERO)
                        MOVE    TBL-YCOM-SURYO-G (IDX IDXC2)
                                           TO  ORCHC30-INPUTSURYOU-TBL
                                                              (IDY)
                        MOVE    1          TO  FLG-INPUTCHI
                        MOVE    "1"        TO  TBL-YCOM-CHK (IDX IDXC2)
                        MOVE    10         TO  IDXC2
                   END-IF
               END-PERFORM
               IF     (TBL-CDCNT           =   1     )
                 AND  (FLG-INPUTCHI        =   ZERO  )
                 AND  (WRK-YSRYCD      NOT =   SPACE )
      *            用法レコードが１件のみの時、数量を全部編集
                   MOVE    WRK-YSRYCD-SURYO-G
                                           TO  ORCHC30-INPUTSURYOU-TBL
                                                              (IDY)
               END-IF
      *
               MOVE    ZERO                TO  IDXY2
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL      (IDXY        >   10       )
                              OR  (IDXY2       >=  YOHO-MAX )
                   IF      WRK-YOHOU(IDXY) NOT =   SPACE
                       ADD     1                   TO  IDXY2
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHC30-YOHOU
                                                            (IDY IDXY2)
                       MOVE    WRK-YOHOU-NAME(IDXY)
                                                TO  ORCHC30-YOHOU-NAME
                                                            (IDY IDXY2)
                   END-IF
               END-PERFORM
      *        用法コメント
               MOVE    ZERO                TO  IDXD
               PERFORM VARYING     IDXC2   FROM    1   BY  1
                       UNTIL      (IDXC2       >   10   )
                 IF      TBL-YCOMMENT (IDX IDXC2)  =   SPACE
                       CONTINUE
                 ELSE
                   ADD     1                    TO  IDXD
                   MOVE    TBL-YCOMMENT (IDX IDXC2)
                                                TO  ORCHC30-COMMENT-CD
                                                            (IDY IDXD)
                   MOVE    TBL-YINPUTCOMENT (IDX IDXC2)
                                               TO  ORCHC30-COMMENT-MEI
                                                            (IDY IDXD)
                   MOVE    TBL-YCOM-CHK (IDX IDXC2)
                                               TO  ORCHC30-COMHENKBN
                                                            (IDY IDXD)
                 END-IF
               END-PERFORM
      *        コメント
               PERFORM VARYING     IDXC2    FROM   1   BY  1
                       UNTIL      (IDXC2       >  10   )
                            OR    (IDXD        >=  10   )
                 IF     (TBL-COMMENT (IDXC2)   NOT =   SPACE)
                 AND    (TBL-IDX-S   (IDXC2)       =   ZERO )
      *
                   ADD     1                   TO  IDXD
                   MOVE    TBL-COMMENT (IDXC2) TO  ORCHC30-COMMENT-CD
                                                            (IDY IDXD)
                   MOVE    TBL-INPUTCOMENT (IDXC2)
                                               TO  ORCHC30-COMMENT-MEI
                                                            (IDY IDXD)
                 END-IF
               END-PERFORM
      *
               EVALUATE    WRK-SRYKBN
      *            内服
                   WHEN    "21"
                       MOVE    "1"         TO  ORCHC30-ZAIKBN(IDY)
      *            頓服
                   WHEN    "22"
                       MOVE    "2"         TO  ORCHC30-ZAIKBN(IDY)
      *            外用
                   WHEN    "23"
                       MOVE    "3"         TO  ORCHC30-ZAIKBN(IDY)
      *            在宅
                   WHEN    "14"
                       MOVE    "4"         TO  ORCHC30-ZAIKBN(IDY)
      *H29.2       自費
                   WHEN    "95"
                   WHEN    "96"
                       MOVE    "9"         TO  ORCHC30-ZAIKBN(IDY)
               END-EVALUATE
           END-PERFORM
      *
           .
       2003-HC30TBL-SET-EXT.
           EXIT.
      *****************************************************************
      *    中途終了時の発行処理
      *****************************************************************
       200-WKSRYACT-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
      *
           MOVE    SPACE               TO  WRK-TBL-AREA
           INITIALIZE                      WRK-TBL-AREA
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDXY
           MOVE    SPACE               TO  WRK-YSRYCD
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    ZERO                TO  WRK-ZAITENKEI
           MOVE    SPACE               TO  WRK-YOHOU-G
           MOVE    SPACE               TO  WRK-HKNCOMBINUM
           INITIALIZE                  TBL-SRYACT-GRP-AREA
           MOVE    ZERO                TO  IDXD
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
      *
               IF      WKSRY-RENNUM        =   1
                   IF      TBL-SRY-MAX         >   ZERO
                       PERFORM 2002-TBLWKSRYACT-HEN-SEC
                   END-IF
                   IF     (WKSRY-INPUTCD(1)(1:1)   =   "#" )
                       MOVE    WKSRY-INPUTCD(1)(2:4)
                                               TO  WRK-HKNCOMBINUM
                   END-IF
      *
                   INITIALIZE                  TBL-SRYACT-GRP-AREA
                   MOVE    ZERO                TO  IDX
                   MOVE    ZERO                TO  FLG-OK
                   MOVE    ZERO                TO  FLG-HEN
                   MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
                   MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
                   MOVE    WKSRY-DRCD          TO  WRK-WKSRY-DRCD
               END-IF
      *        投薬を対象とする
               IF     (WKSRY-SRYKBN        =   "21"  OR
                                               "22"  OR
                                               "23"  OR
                                               "14"
      *H29.2
                                           OR  "95"
                                           OR  "96"    )
      *            包括保険は対象外
                   IF    ((WRK-HKNCOMBINUM     =   "9999")
                   AND    (FLG-YKJOKUKBN       =   ZERO  ) )
      *                薬評は対象外
                   OR    ( WKSRY-ZAIKBN        =   1     )
      *                減点
                   OR    ( WKSRY-ZAIKBN        =   2     )
                       CONTINUE
                   ELSE
                       PERFORM 2001-WKSRYACT-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-SRY-MAX         >   ZERO
               PERFORM 2002-TBLWKSRYACT-HEN-SEC
           END-IF
      *
           .
       200-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為内容を診療行為へ
      *****************************************************************
       2002-TBLWKSRYACT-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  TBL-ACCT-ZAINUM
           IF      ORCSCH30-KBN        =   "3"
      *        院内
               MOVE    1               TO  FLG-INNAI
           ELSE
      *        院外
               MOVE    2               TO  FLG-INNAI
           END-IF
      *    剤ごとのチェック・編集
           PERFORM 20022-ZAICHEK-SYORI-SEC
           IF      FLG-HEN             =   1
      *        最初のドクター
               IF      WRK-DRCD        =   SPACE
                   MOVE    WRK-WKSRY-DRCD      TO  WRK-DRCD
               END-IF
           END-IF
           .
       2002-TBLWKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為内容を診療行為へ
      *****************************************************************
       2001-WKSRYACT-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )  OR
                              (WKSRY-SRYCD (IDXA)  =   SPACE)
      *H26.10
               MOVE    ZERO            TO  FLG-NO
               IF     (WKSRY-SRYCD (IDXA)(1:1) =   "6" )
      *            点数マスタを検索
                   MOVE    WKSRY-SRYCD (IDXA)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      TNS-TENSIKIBETU     =   7
      *                減点は対象外
                       MOVE    1               TO  FLG-NO
                   END-IF
               END-IF
      *H29.2
      *        自費コード
               IF     WKSRY-SRYKBN         =   "95"
                                           OR  "96"
                   IF    (WKSRY-SRYCD (IDXA)(1:3) =   "095"
                                                  OR  "096" )
      *                点数マスタを検索
                       MOVE    WKSRY-SRYCD (IDXA)  TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-YKZKBN          =   ZERO
      *                    対象外
                           MOVE    1               TO  FLG-NO
                       END-IF
                   END-IF
      *            薬剤・用法・コメントのみ対象
                   IF     (WKSRY-SRYCD (IDXA)(1:1) =   "6"
                                                   OR  "0"
                                                   OR  "8" )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-NO
                   END-IF
               END-IF
      *
               IF     (WKSRY-SRYCD (IDXA)      =   "630010002" )
                 OR   (WKSRY-SRYCD (IDXA)(1:4)     =   "0086" )
      *          （減）は対象外
                 OR   (WKSRY-SRYCD (IDXA)      =   "820000047" )
                 OR   (WKSRY-SRYCD (IDXA)      =   CONS-SGEN-SRYCD)
                 OR   (FLG-NO                  =   1           )
                   CONTINUE
               ELSE
                   ADD    1                        TO  IDX
                   MOVE    WKSRY-SRYCD (IDXA)      TO  TBL-SRY-SRYCD
                                                                (IDX)
                   MOVE    WKSRY-SRYSURYO (IDXA)   TO  TBL-SRY-SRYSURYO
                                                                (IDX)
                   MOVE    WKSRY-SRYKAISU (IDXA)   TO  TBL-SRY-SRYKAISU
                                                                (IDX)
                   MOVE    WKSRY-AUTOKBN (IDXA)    TO  TBL-SRY-AUTOKBN
                                                                (IDX)
                   MOVE    WKSRY-KANSURYO (IDXA)   TO  TBL-SRY-KANSURYO
                                                                (IDX)
                   MOVE    ZERO                    TO  TBL-SRY-INPUTNUM
                                                                (IDX)
      *            名称
                   IF    ((WKSRY-SRYCD (IDXA)(1:1) =    "8")   OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "008") OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "001")) AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  TBL-SRY-COM-NAME
                                                           (IDX)
      *H28.10          数量
                       IF      WKSRY-SRYCD (IDXA)(1:3) =    "001"
                           MOVE    WKSRY-INPUTCHI-G (IDXA)
                                               TO  TBL-SRY-INPUTCHI-G
                                                           (IDX)
                       END-IF
                   END-IF
                   MOVE    IDX                 TO  TBL-SRY-MAX
               END-IF
           END-PERFORM
           IF      WKSRY-ZAIKAIKEI     >   ZERO
               MOVE    WKSRY-ZAITENKEI     TO  TBL-ACCT-ZAITEN
               MOVE    WKSRY-ZAIKAIKEI     TO  TBL-ACCT-KAISU
           END-IF
      *H29.2
      *    自費
           IF     (WKSRY-SRYKBN        =   "95"
                                       OR  "96" )
             AND  (WKSRY-JIHIMONEYTOTAL    >   ZERO )
               MOVE    WKSRY-JIHIMONEYTOTAL    TO  TBL-ACCT-ZAITEN
               MOVE    WKSRY-ZAIKAIKEI         TO  TBL-ACCT-KAISU
           END-IF
      *
           .
       2001-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    内容変更発行処理
      *****************************************************************
       300-ORCSCH30-SYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   ORCSCH30-ZAI-MAX)
      *
               MOVE    SPACE               TO  WRK-TBL-AREA
               INITIALIZE                      WRK-TBL-AREA
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  IDXY
               MOVE    ZERO                TO  IDXC
               MOVE    SPACE               TO  WRK-YSRYCD
               MOVE    SPA-DRCD            TO  WRK-DRCD
               MOVE    ORCSCH30-ZAIKAIKEI(IDT)
                                           TO  WRK-KAISU
               MOVE    ORCSCH30-SRYKBN (IDT)
                                           TO  WRK-SRYKBN
               MOVE    ORCSCH30-SRYSYUKBN (IDT)
                                           TO  WRK-SRYSYUKBN
               MOVE    SPACE               TO  WRK-YOHOU-G
               MOVE    ZERO                TO  IDXD
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   50   )  OR
                                  (ORCSCH30-SRYCD(IDT IDXA)  =   SPACE)
                   PERFORM 3001-TBLSET-SEC
               END-PERFORM
      *
               IF      IDX-S               >   ZERO
      *            パラメタ編集
                   PERFORM 2003-HC30TBL-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       200-ORCSCH30-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       3001-TBLSET-SEC         SECTION.
      *
      *    薬剤編集
           IF     (ORCSCH30-SRYCD(IDT IDXA)(1:1)   =   "6" )
      *H29.2
             OR   (ORCSCH30-SRYCD(IDT IDXA)(1:3)   =   "095"
                                                   OR  "096")
               ADD     1               TO  IDX-S
               IF      IDX-S           >   99
                   DISPLAY "ORCSHC30 TBL OVER !!"
                   MOVE    99              TO  IDX-S
               END-IF
               MOVE    ORCSCH30-SRYCD(IDT IDXA)    TO  TBL-SRYCD (IDX-S)
               MOVE    ORCSCH30-SRYSURYO(IDT IDXA) TO  TBL-SURYO (IDX-S)
               MOVE    ORCSCH30-KANSURYO(IDT IDXA) TO  TBL-KANSURYO
                                                                 (IDX-S)
      *        外用の日数
               IF      (ORCSCH30-SRYKBN (IDT)          =   "23" )  AND
                       (ORCSCH30-SRYKAISU(IDT IDXA)    >   1 )
                   MOVE    ORCSCH30-SRYKAISU(IDT IDXA)
                                                   TO  TBL-SRYKAISU
                                                                 (IDX-S)
               END-IF
               MOVE    ZERO                TO  IDXD
           END-IF
      *    用法編集
           IF      ORCSCH30-SRYCD(IDT IDXA)(1:3)   =   "001"
      *H28.10
               ADD     1                   TO  TBL-CDCNT
               MOVE    ZERO                TO  FLG-YSET
      *        点数マスタを検索して、用法時点を判定する
               MOVE    ORCSCH30-SRYCD(IDT IDXA)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (FLG-TENSU           =   ZERO      )
      *         コメントは対象外
               AND    (TNS-KENTAICOMMENT   =   0  OR  1  )
                   IF    ((TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(4)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(5)   =   ZERO ))
                       MOVE    ZERO                TO  FLG-YSET
                   ELSE
                       MOVE    1                   TO  FLG-YSET
                   END-IF
                   ADD     1                   TO  IDXY
                   IF      IDXY                <=  10
                       MOVE    ORCSCH30-SRYCD(IDT IDXA)
                                                   TO  WRK-YOHOU
                                                               (IDXY)
                       MOVE    ORCSCH30-INPUTCOMENT(IDT IDXA)
                                                   TO  WRK-YOHOU-NAME
                                                               (IDXY)
                       MOVE    FLG-YSET            TO  WRK-YOHOU-KBN
                                                               (IDXY)
      *H28.10          埋め込み数値変換
                       MOVE    ORCSCH30-INPUTCHI-AREA(IDT IDXA)
                                                   TO  WRK-INPUTCHI-AREA
                       PERFORM 200221-PTCOM-INPUTCHI-SEC
                       MOVE    WRK-INPUTCHISURYO-AREA
                                                   TO  WRK-YOHOU-SURYO-G
                                                               (IDXY)
                   END-IF
               END-IF
           END-IF
      *    用量割合コード
           IF     (ORCSCH30-SRYCD(IDT IDXA)(1:7)
                                           =   "0992000" ) AND
                  (IDX-S                   >   ZERO      )
               EVALUATE    ORCSCH30-SRYCD(IDT IDXA)
      *            朝
                   WHEN    "099200011"
                       MOVE    ORCSCH30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 1)
      *            昼
                   WHEN    "099200012"
                       MOVE    ORCSCH30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 2)
      *            夜
                   WHEN    "099200013"
                       MOVE    ORCSCH30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 3)
      *            寝
                   WHEN    "099200014"
                       MOVE    ORCSCH30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 4)
      *            起床
                   WHEN    "099200015"
                       MOVE    ORCSCH30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 5)
               END-EVALUATE
           END-IF
      *
      *    コメント
           IF     (ORCSCH30-SRYCD(IDT IDXA)(1:1)   =   "8"   )   OR
                  (ORCSCH30-SRYCD(IDT IDXA)(1:4)
                                               =   "0082"
                                               OR  "0083"
                                               OR  "0084"
                                               OR  "0085" )
      *H28.10
               ADD     1                   TO  TBL-CDCNT
      *ver.4.7
               IF     (IDX-S                   >   ZERO  )
                 AND  (IDXD                    <   20    )
      *            薬剤の下のコメント
                   ADD     1                       TO  IDXD
                   MOVE    ORCSCH30-SRYCD(IDT IDXA)
                                                   TO  TBL-YCOMMENT
                                                       (IDX-S IDXD)
                   MOVE    ORCSCH30-INPUTCOMENT(IDT IDXA)
                                                   TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
               END-IF
      *ver.4.7
      *
      *        すべての薬剤
               IF      IDXC                    <   20
                   ADD     1                       TO  IDXC
                   MOVE    ORCSCH30-SRYCD(IDT IDXA)
                                               TO  TBL-COMMENT (IDXC)
                   MOVE    ORCSCH30-INPUTCOMENT(IDT IDXA)
                                               TO  TBL-INPUTCOMENT
                                                             (IDXC)
               END-IF
      *ver.4.7
               IF      IDX-S                   >   ZERO
      *            薬剤あり
                   MOVE    IDX-S               TO  TBL-IDX-S (IDXC)
                   MOVE    IDXD                TO  TBL-IDXD (IDXC)
               END-IF
      *ver.4.7
           END-IF
      *    用法コメント
           IF     (ORCSCH30-SRYCD(IDT IDXA)(1:3)
                                               =   "001" ) AND
                  (TNS-KENTAICOMMENT           =   2     ) AND
                  (IDX-S                       >   ZERO  )
            AND   (IDXD                        <   20    )
               ADD     1                       TO  IDXD
               MOVE    ORCSCH30-SRYCD(IDT IDXA)
                                               TO  TBL-YCOMMENT
                                                       (IDX-S IDXD)
               MOVE    ORCSCH30-INPUTCOMENT(IDT IDXA)
                                               TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
      *H28.10  埋め込み数値変換
               MOVE    ORCSCH30-INPUTCHI-AREA(IDT IDXA)
                                               TO  WRK-INPUTCHI-AREA
               PERFORM 200221-PTCOM-INPUTCHI-SEC
               MOVE    WRK-INPUTCHISURYO-AREA  TO  TBL-YCOM-SURYO-G
                                                       (IDX-S IDXD)
           END-IF
      *
           .
       3001-TBLSET-EXT.
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
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
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
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
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
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
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
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
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
