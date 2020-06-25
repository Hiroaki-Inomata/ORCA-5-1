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
       PROGRAM-ID.             ORCSCHN30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 入院薬剤情報印刷用パラメタ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/11    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.00    NACL-太田   08/10/31   オン帳票バッチ起動対応
      *  04.05.00    NACL-多々納 09/07/23   在宅・コメント対象対応
      *  04.05.00    NACL-多々納 09/09/XX   表示コメントコード対応
      *  04.06.00    NACL-多々納 11/02/22   用法コメント対応
      *  04.07.00    NACL-多々納 12/11/02   薬剤毎のコメント対応
      *  04.08.00    NACL-多々納 16/11/XX   用法コード数量追加
      *  05.00.00    NACL-多々納  20/03/XX  コメント、入力値桁数変更
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
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-YSET            PIC 9(01).
      *
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
           03  IDXC                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXY2               PIC 9(04).
           03  IDT                 PIC 9(04).
           03  IDXD                PIC 9(04).
           03  IDXC2               PIC 9(04).
      *
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
      *
           03  YOHO-MAX            PIC 9(04).
      *
           03  IDX-SST             PIC 9(04).
           03  IDXS                PIC 9(04).
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
      *H28.10          服用数値
                       09  TBL-YCOM-CHK             PIC X(01).
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
               05  WRK-SRYKBN              PIC X(02).
               05  WRK-SRYSYUKBN           PIC X(03).
      *
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
           03  WRK-KAISU           PIC 9(05).
      *    プログラム名
           03  WRK-YAKJYO-PG       PIC X(20).
      *H28.10
           03  WRK-INPUTCHI-AREA.
               05  WRK-INPUTCHI        PIC X(08)
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
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
      *    入院薬情報出力パラメタ
           COPY    "CPORCHCN30.INC".
      *
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "MCPDATA.INC".
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
           COPY    "CPORCSCHN30.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCHN30AREA
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
           INITIALIZE                  ORCHCN30AREA
           INITIALIZE                  ONPRTDATA
      *
           EVALUATE    ORCSHCN30-KBN
               WHEN    "N"
      *            入院印刷画面発行
                   PERFORM 100-ORCSHCN30-SYORI-SEC
           END-EVALUATE
      *
      *TEST
      *    DISPLAY "ORCSHCN30-KBN:" ORCSHCN30-KBN
      *    DISPLAY "ORCHCN30-MAX:" ORCHCN30-MAX
      *    PERFORM VARYING    IDX   FROM    1  BY  1
      *            UNTIL      IDX   >  ORCHCN30-MAX
      *      DISPLAY "ORCHCN30-SRYCD:" ORCHCN30-SRYCD (IDX)
      *              ",ORCHCN30-ZAIKBN:" ORCHCN30-ZAIKBN (IDX)
      *              ",ORCHCN30-SURYO:" ORCHCN30-SURYO (IDX)
      *              ",ORCHCN30-YSRYCD:" ORCHCN30-YSRYCD (IDX)
      *                 "," ORCHCN30-YSRYCD-NAME (IDX)
      *              ",ORCHCN30-YOURYOU 1:" ORCHCN30-YOURYOU (IDX 1)
      *              ",2:" ORCHCN30-YOURYOU (IDX 2)
      *              ",3:" ORCHCN30-YOURYOU (IDX 3)
      *              ",4:" ORCHCN30-YOURYOU (IDX 4)
      *              ",5:" ORCHCN30-YOURYOU (IDX 5)
      *              ",ORCHCN30-SRYKAISU:" ORCHCN30-SRYKAISU (IDX)
      *     DISPLAY "ORCHCN30-INPUTSURYOU 1:" ORCHCN30-INPUTSURYOU
      *                                             (IDX 1)
      *           ",2:" ORCHCN30-INPUTSURYOU (IDX 2)
      *           ",3:" ORCHCN30-INPUTSURYOU (IDX 3)
      *           ",4:" ORCHCN30-INPUTSURYOU (IDX 4)
      *           ",5:" ORCHCN30-INPUTSURYOU (IDX 5)
      *      DISPLAY "ORCHCN30-YOHOU:" ORCHCN30-YOHOU (IDX 1)
      *       "," ORCHCN30-YOHOU-NAME(IDX 1)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 1)
      *       "," ORCHCN30-COMMENT-MEI (IDX 1)
      *        ",KBN:"  ORCHCN30-COMHENKBN  (IDX 1)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 2)
      *       "," ORCHCN30-COMMENT-MEI (IDX 2)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 3)
      *       "," ORCHCN30-COMMENT-MEI (IDX 3)
      *      DISPLAY "ORCHCN30-COMMENT-CD:" ORCHCN30-COMMENT-CD (IDX 4)
      *       "," ORCHCN30-COMMENT-MEI (IDX 4)
      *    END-PERFORM 
      *TEST
      *
      *    印刷処理へ
           IF      ORCHCN30-MAX         >   ZERO
               PERFORM 300-HCN30-PRINT-SEC
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       300-HCN30-PRINT-SEC            SECTION.
      *
      *    薬情報出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000009"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-YAKJYO-PG
           END-IF
      *
      *    交付日
           MOVE    ORCSHCN30-SRYYMD    TO  SPA-SRYYMD
      *    ドクター
           MOVE    ORCSHCN30-DRCD      TO  ORCHCN30-DRCD
      *    病棟
           MOVE    ORCSHCN30-BTUNAME   TO  ORCHCN30-BTUNAME
      *    病室
           MOVE    ORCSHCN30-ROOMNO    TO  ORCHCN30-ROOMNO
      *API(GINBEE)対応
      *    病棟番号
           MOVE    ORCSHCN30-BTUNUM    TO  ORCHCN30-BTUNUM
      *
           IF      WRK-YAKJYO-PG   NOT =   SPACE
      *ver.4.7 プレビューの為、直接印刷へ変更
               MOVE    ORCHCN30AREA    TO  ONPRTDATA-DATA
               CALL    WRK-YAKJYO-PG       USING
                                           SPA-AREA
                                           ONPRTDATA
      *****    MOVE    WRK-YAKJYO-PG   TO  ONPRTDATA-PGID
      *        MOVE    ORCHCN30AREA    TO  ONPRTDATA-DATA
      *        CALL    "ORCHCOMMON"        USING
      *                                    SPA-AREA
      *****                                ONPRTDATA
           END-IF
      *
           .
       300-HCN30-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容変更発行処理
      *****************************************************************
       100-ORCSHCN30-SYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   ORCSHCN30-ZAI-MAX)
      *
               MOVE    SPACE               TO  WRK-TBL-AREA
               INITIALIZE                      WRK-TBL-AREA
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  IDXY
               MOVE    ZERO                TO  IDXC
               MOVE    SPACE               TO  WRK-YSRYCD
               MOVE    ORCSHCN30-ZAIKAIKEI(IDT)
                                           TO  WRK-KAISU
               MOVE    ORCSHCN30-SRYKBN (IDT)
                                           TO  WRK-SRYKBN
               MOVE    ORCSHCN30-SRYSYUKBN (IDT)
                                           TO  WRK-SRYSYUKBN
               INITIALIZE                      WRK-YOHOU-G
               MOVE    ZERO                TO  IDXD
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   50   )  OR
                                  (ORCSHCN30-SRYCD(IDT IDXA)  =   SPACE)
                   PERFORM 3001-TBLSET-SEC
               END-PERFORM
      *
               IF      IDX-S               >   ZERO
      *            パラメタ編集
                   PERFORM 1001-HCN30TBL-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       100-ORCSHCN30-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       3001-TBLSET-SEC         SECTION.
      *    薬剤編集
           IF      ORCSHCN30-SRYCD(IDT IDXA)(1:1)   =   "6"
               ADD     1               TO  IDX-S
               IF      IDX-S           >   99
                   DISPLAY "ORCSHC30 TBL OVER !!"
                   MOVE    99              TO  IDX-S
               END-IF
               MOVE    ORCSHCN30-SRYCD(IDT IDXA)
                                           TO  TBL-SRYCD (IDX-S)
               MOVE    ORCSHCN30-SRYSURYO(IDT IDXA)
                                           TO  TBL-SURYO (IDX-S)
               MOVE    ORCSHCN30-KANSURYO(IDT IDXA)
                                           TO  TBL-KANSURYO (IDX-S)
      *        外用の日数
               IF      (ORCSHCN30-SRYKBN (IDT)          =   "23" )  AND
                       (ORCSHCN30-SRYKAISU(IDT IDXA)    >   1 )
                   MOVE    ORCSHCN30-SRYKAISU(IDT IDXA)
                                                   TO  TBL-SRYKAISU
                                                                 (IDX-S)
               END-IF
               MOVE    ZERO                TO  IDXD
           END-IF
      *    用法編集
           IF      ORCSHCN30-SRYCD(IDT IDXA)(1:3)   =   "001"
      *H28.10
               ADD     1                   TO  TBL-CDCNT
               MOVE    ZERO                TO  FLG-YSET
      *        点数マスタを検索して、用法時点を判定する
               MOVE    ORCSHCN30-SRYCD(IDT IDXA)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (FLG-TENSU           =   ZERO   )
      *         コメントは対象外
               AND    (TNS-KENTAICOMMENT   =   0  OR  1  )
                   IF     (TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(4)   =   ZERO )  AND
                          (TNS-SSTKIJUNCD(5)   =   ZERO )
                       MOVE    ZERO                TO  FLG-YSET
                   ELSE
                       MOVE    1                   TO  FLG-YSET
                   END-IF
                   ADD     1                   TO  IDXY
                   IF      IDXY                <=  10
                       MOVE    ORCSHCN30-SRYCD(IDT IDXA)
                                                   TO  WRK-YOHOU
                                                               (IDXY)
                       MOVE    ORCSHCN30-INPUTCOMENT(IDT IDXA)
                                                   TO  WRK-YOHOU-NAME
                                                               (IDXY)
                       MOVE    FLG-YSET            TO  WRK-YOHOU-KBN
                                                               (IDXY)
      *H28.10          埋め込み数値変換
                       MOVE    ORCSHCN30-INPUTCHI-G (IDT IDXA)
                                                   TO  WRK-INPUTCHI-AREA
                       PERFORM 200221-PTCOM-INPUTCHI-SEC
                       MOVE    WRK-INPUTCHISURYO-AREA
                                                   TO  WRK-YOHOU-SURYO-G
                                                               (IDXY)
                   END-IF
               END-IF
           END-IF
      *    用量割合コード
           IF     (ORCSHCN30-SRYCD(IDT IDXA)(1:7)
                                           =   "0992000" ) AND
                  (IDX-S                   >   ZERO      )
               EVALUATE    ORCSHCN30-SRYCD(IDT IDXA)
      *            朝
                   WHEN    "099200011"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 1)
      *            昼
                   WHEN    "099200012"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 2)
      *            夜
                   WHEN    "099200013"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 3)
      *            寝
                   WHEN    "099200014"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 4)
      *            起床
                   WHEN    "099200015"
                       MOVE    ORCSHCN30-SRYSURYO (IDT IDXA)
                                           TO  TBL-YOURYOU (IDX-S 5)
               END-EVALUATE
           END-IF
      *
      *    コメント
           IF     (ORCSHCN30-SRYCD(IDT IDXA)(1:1)  =   "8"   )   OR
                  (ORCSHCN30-SRYCD(IDT IDXA)(1:4)
                                               =   "0082"
                                               OR  "0083"
                                               OR  "0084"
                                               OR  "0085" )
      *H28.10
               ADD     1                   TO  TBL-CDCNT
      *ver.4.7
               IF     (IDX-S                   >   ZERO  )
                  AND (IDXD                    <   20    )
      *            薬剤の下のコメント
                   ADD     1                       TO  IDXD
                   MOVE    ORCSHCN30-SRYCD (IDT IDXA)
                                                   TO  TBL-YCOMMENT
                                                       (IDX-S IDXD)
                   MOVE    ORCSHCN30-INPUTCOMENT (IDT IDXA)
                                                   TO  TBL-YINPUTCOMENT
                                                       (IDX-S IDXD)
               END-IF
      *ver.4.7
               IF      IDXC                    <   20
                   ADD     1                       TO  IDXC
                   MOVE    ORCSHCN30-SRYCD (IDT IDXA)
                                               TO  TBL-COMMENT (IDXC)
                   MOVE    ORCSHCN30-INPUTCOMENT (IDT IDXA)
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
           IF     (ORCSHCN30-SRYCD(IDT IDXA)(1:3)
                                           =   "001" ) AND
                  (TNS-KENTAICOMMENT       =   2     ) AND
                  (IDX-S                   >   ZERO  )
             AND  (IDXD                    <   20    )
               ADD     1                       TO  IDXD
               MOVE    ORCSHCN30-SRYCD (IDT IDXA)
                                               TO  TBL-YCOMMENT
                                                          (IDX-S IDXD)
               MOVE    ORCSHCN30-INPUTCOMENT (IDT IDXA)
                                               TO  TBL-YINPUTCOMENT
                                                          (IDX-S IDXD)
      *H28.10  埋め込み数値変換
               MOVE    ORCSHCN30-INPUTCHI-G (IDT IDXA)
                                               TO  WRK-INPUTCHI-AREA
               PERFORM 200221-PTCOM-INPUTCHI-SEC
               MOVE    WRK-INPUTCHISURYO-AREA  TO  TBL-YCOM-SURYO-G
                                                       (IDX-S IDXD)
           END-IF
      *
           .
       3001-TBLSET-EXT.
           EXIT.
      *
      *H28.11
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
       1001-HCN30TBL-SET-SEC         SECTION.
      *
      *    用法は最後の用法コードとする
           MOVE    ZERO                TO  IDXY2
           PERFORM VARYING     IDXY    FROM    10  BY  -1
                   UNTIL      (IDXY        <   1      )
               IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
               AND    (WRK-YOHOU-KBN(IDXY)     =   "1"       )
                   MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   MOVE    WRK-YOHOU-NAME(IDXY)
                                               TO  WRK-YSRYCD-NAME
      *H28.11      服用数値
                   MOVE    WRK-YOHOU-SURYO-G (IDXY)
                                               TO  WRK-YSRYCD-SURYO-G
                   MOVE    SPACE               TO  WRK-YOHOU(IDXY)
                   MOVE    1                   TO  IDXY
               ELSE
                   IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
      ************ AND    (IDXY2               =   ZERO      )
                       MOVE    IDXY            TO  IDXY2
                   END-IF
               END-IF
           END-PERFORM
      *H25.3
      *    服用時点がなければ用法の１件目となる
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
                              (ORCHCN30-MAX     >=  99 )
               ADD     1                   TO  ORCHCN30-MAX
               MOVE    ORCHCN30-MAX        TO  IDY
               MOVE    TBL-SRYCD (IDX)     TO  ORCHCN30-SRYCD (IDY)
               MOVE    TBL-SURYO (IDX)     TO  ORCHCN30-SURYO (IDY)
               MOVE    TBL-KANSURYO (IDX)  TO  ORCHCN30-KANSURYO (IDY)
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL       IDXY        >   5
                   MOVE    TBL-YOURYOU (IDX IDXY)
                                           TO  ORCHCN30-YOURYOU
                                                            (IDY IDXY)
               END-PERFORM
               MOVE    TBL-SRYKAISU (IDX)  TO  ORCHCN30-SRYKAISU (IDY)
      *
               MOVE    WRK-YSRYCD          TO  ORCHCN30-YSRYCD(IDY)
               MOVE    WRK-YSRYCD-NAME     TO  ORCHCN30-YSRYCD-NAME(IDY)
               MOVE    WRK-KAISU           TO  ORCHCN30-KAISU (IDY)
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
                                           TO  ORCHCN30-INPUTSURYOU-TBL
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
                                           TO  ORCHCN30-INPUTSURYOU-TBL
                                                              (IDY)
               END-IF
      *
      *
               MOVE    ZERO                TO  IDXY2
               PERFORM VARYING     IDXY    FROM    1   BY  1
                       UNTIL      (IDXY        >   10   )   OR
                                  (IDXY2       >=  YOHO-MAX  )
                   IF      WRK-YOHOU(IDXY)     NOT =   SPACE
                       ADD     1                   TO  IDXY2
                       MOVE    WRK-YOHOU(IDXY)     TO  ORCHCN30-YOHOU
                                                            (IDY IDXY2)
                       MOVE    WRK-YOHOU-NAME(IDXY)
                                               TO  ORCHCN30-YOHOU-NAME
                                                            (IDY IDXY2)
                   END-IF
               END-PERFORM
      *        用法コメント
               MOVE    ZERO                TO  IDXD
               PERFORM VARYING     IDXC    FROM    1   BY  1
                       UNTIL      (IDXC        >   10   )
                 IF      (TBL-YCOMMENT (IDX IDXC)  NOT =   SPACE)
                   ADD     1                   TO  IDXD
                   MOVE    TBL-YCOMMENT (IDX IDXC)
                                               TO  ORCHCN30-COMMENT-CD
                                                            (IDY IDXD)
                   MOVE    TBL-YINPUTCOMENT (IDX IDXC)
                                               TO  ORCHCN30-COMMENT-MEI
                                                            (IDY IDXD)
      *H28.11      数量編集済み
                   MOVE    TBL-YCOM-CHK (IDX IDXC)
                                               TO  ORCHCN30-COMHENKBN
                                                            (IDY IDXD)
                 END-IF
               END-PERFORM
      *        コメント
               PERFORM VARYING     IDXC    FROM    1   BY  1
                       UNTIL      (IDXC        >   10   )
                             OR   (IDXD        >=  10   )
                   IF     (TBL-COMMENT (IDXC)  NOT =   SPACE)
                     AND  (TBL-IDX-S   (IDXC)      =   ZERO )
                       ADD     1                   TO  IDXD
                       MOVE    TBL-COMMENT (IDXC)
                                               TO  ORCHCN30-COMMENT-CD
                                                            (IDY IDXD)
                       MOVE    TBL-INPUTCOMENT (IDXC)
                                               TO  ORCHCN30-COMMENT-MEI
                                                            (IDY IDXD)
                   END-IF
               END-PERFORM
      *
               EVALUATE    WRK-SRYKBN
      *            内服
                   WHEN    "21"
                       MOVE    "1"         TO  ORCHCN30-ZAIKBN(IDY)
      *            頓服
                   WHEN    "22"
                       MOVE    "2"         TO  ORCHCN30-ZAIKBN(IDY)
      *            外用
                   WHEN    "23"
                       MOVE    "3"         TO  ORCHCN30-ZAIKBN(IDY)
      *            在宅
                   WHEN    "14"
                       MOVE    "4"         TO  ORCHCN30-ZAIKBN(IDY)
               END-EVALUATE
           END-PERFORM
      *
           .
       1001-HCN30TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ORCSHCN30-SRYYMD     TO  TNS-YUKOSTYMD
           MOVE    ORCSHCN30-SRYYMD     TO  TNS-YUKOEDYMD
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
