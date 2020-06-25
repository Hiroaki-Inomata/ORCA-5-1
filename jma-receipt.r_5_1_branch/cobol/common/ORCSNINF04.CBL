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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNINF04.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院医療区分・ＡＤＬ点数情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/05/26    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RYOCOM              PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXM                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-ZAISKBKBN           PIC X(01).
           03  WRK-MEDKBN-ID-G.
               05  WRK-MEDKBN-ID       PIC 9(02).
           03  WRK-ADLTEN-G.
               05  WRK-ADLTEN          PIC 9(02).
           03  WRK-MEDKBN              PIC X(1).
           03  WRK-PTEVAL              PIC X(1).
           03  WRK-Z9-G.
               05  WRK-Z9              PIC Z9.
      *
       01  KEY-AREA.
           03  KEY-SRYYM               PIC X(06).
      *
       01  TDAY-AREA.
           03  TDAY-POS                PIC 9(03).
           03  TDAY-DAY-G.
               05  TDAY-DAY            PIC 9(02)
                                       OCCURS      31.
           03  TDAY-CSV                PIC X(128).
      *
       01  TCOMB-AREA.
           COPY    "CPNYUINACCT.INC"   REPLACING   //NACCT//
                                       BY          //TCOMB//.
      *
       01  CONST-AREA.
           03  CONST-H240401           PIC X(08) VALUE "20120401".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    医療区分・ＡＤＬ点数取得サブ
           COPY    "CPORCHCN68S01.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院診療会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPORCSNINF04.INC".
           COPY    "CPORCHCN68S01.INC"         REPLACING //HCN68S01-//
                                               BY    //LNK-HCN68S01-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNINF04-AREA
           LNK-HCN68S01-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TCOMB-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    LNK-HCN68S01-SRYYM
                                   TO  KEY-SRYYM
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-STYUKYMD
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           INITIALIZE                  ORCSNTYMDAREA
           MOVE    SPA-HOSPNUM     TO  SNTYMD-HOSPNUM
           MOVE    SPA-PTID        TO  SNTYMD-PTID
           MOVE    "2"             TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                       SPA-AREA
      *
           MOVE    "5"                 TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           MOVE    NYUINACCT-REC   TO  TCOMB-AREA
      *
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    LNK-HCN68S01-AREA
                                   TO  HCN68S01-AREA
           CALL    "ORCHCN68S01"       USING
                                       SPA-AREA
                                       HCN68S01-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-SRYYM-EDIT-SEC
      *
           PERFORM 2001-MEDKBN-EDIT-SEC
      *
           PERFORM 2001-ADLTEN-EDIT-SEC
      *
           PERFORM 2001-PTEVAL-EDIT-SEC
      *
           PERFORM 2001-DESIGNR-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-PTNUM           TO  SNINF04-PTNUM
           MOVE    PTINF-NAME          TO  SNINF04-NAME
           MOVE    PTINF-KANANAME      TO  SNINF04-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  SNINF04-BIRTHDAY
           MOVE    PTINF-SEX           TO  SNINF04-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月編集処理
      *****************************************************************
       2001-SRYYM-EDIT-SEC             SECTION.
      *
           MOVE    KEY-SRYYM       TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:2)
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE (1:7)  TO  SNINF04-SRYYM
      *
           MOVE    ZERO            TO      IDXA
           PERFORM VARYING IDX0    FROM    SNTYMD-MAX   BY  -1
                   UNTIL ( IDXA    >   5 )
                    OR   ( IDX0    <   1)
      *
               IF    ( KEY-SRYYM   >=  SNTYMD-NYUINYMD (IDX0)(1:6))
                AND  ( KEY-SRYYM   <=  SNTYMD-TAIINYMD (IDX0)(1:6))
                   COMPUTE IDXA    =   IDXA    +   1
                   MOVE    SNTYMD-NYUINYMD (IDX0)
                                   TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  SNINF04-NYUINYMD (IDXA)
      *
                   MOVE    SNTYMD-TAIINYMD (IDX0)
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  SNINF04-TAIINYMD (IDXA)
               END-IF
      *
           END-PERFORM
      *
          .
       2001-SRYYM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分編集処理
      *****************************************************************
       2001-MEDKBN-EDIT-SEC     SECTION.
      *
           MOVE    ZERO                TO  SNINF04-MEDKBN-MAX
      *
           PERFORM VARYING IDXM    FROM    1    BY  1
                   UNTIL ( IDXM    >   HCN68S01-RYOLIST-MAX )
      *
               IF    ( HCN68S01-TZAISKBKBN (IDXM)  =   "2" )
                   EVALUATE    TRUE
                   WHEN  ( HCN68S01-TALLKBN (IDXM) =   1 )
                    AND  ( HCN68S01-TDAYALL (IDXM) =   1 )
                   WHEN  ( HCN68S01-TALLKBN (IDXM) =   ZERO )
                    AND  ( HCN68S01-TDAY-G  (IDXM) NOT = ZERO )
                       PERFORM 20011-MEDKBN-EDIT-SEC
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           IF    ( HCN68S01-MEDKBN-DAYALL (1)  =   1 )
            OR   ( HCN68S01-MEDKBN-DAY-G  (1)  NOT =   ZERO )
               MOVE    1       TO   IDXM
               PERFORM 20011-MEDKBN-TUKI-HYOKA-EDIT-SEC
           END-IF
      *
           IF    ( HCN68S01-MEDKBN-DAYALL (2)  =   1 )
            OR   ( HCN68S01-MEDKBN-DAY-G  (2)  NOT =   ZERO )
               MOVE    2       TO   IDXM
               PERFORM 20011-MEDKBN-TUKI-HYOKA-EDIT-SEC
           END-IF
      *
           INITIALIZE      TDAY-AREA
           PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
               EVALUATE    TRUE
               WHEN  ( HCN68S01-RYOYO-CD (IDXD)        NOT =   SPACE )
                   IF    ( HCN68S01-MEDKBN-DAYALL (1)      =   ZERO )
                    AND  ( HCN68S01-MEDKBN-DAY (1 IDXD)    =   ZERO )
                    AND  ( HCN68S01-MEDKBN-DAYALL (2)      =   ZERO )
                    AND  ( HCN68S01-MEDKBN-DAY (2 IDXD)    =   ZERO )
                       MOVE    1       TO  TDAY-DAY (IDXD)
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           IF    ( TDAY-DAY-G          NOT =   ZERO )
               COMPUTE SNINF04-MEDKBN-MAX  =   SNINF04-MEDKBN-MAX  +   1
               MOVE    SNINF04-MEDKBN-MAX  TO  IDXA
               MOVE    "1"         TO  SNINF04-MEDKBN-LV (IDXA)
               MOVE    "M1"        TO  SNINF04-MEDKBN-ID (IDXA)
               MOVE    "医療区分３・２いずれも０（医療区分１）"
                                   TO  SNINF04-MEDKBN-NAME (IDXA)
               PERFORM 20111-DAY-CSV-SEC
               MOVE    TDAY-CSV    TO  SNINF04-MEDKBN-DAY(IDXA)
           END-IF
      *
           INITIALIZE                  TDAY-AREA
           MOVE    1              TO   TDAY-POS
           PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
               IF    ( IDXD        =   1 )
                   CONTINUE
               ELSE
                   STRING    ","   DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
               EVALUATE    TRUE
               WHEN  ( HCN68S01-RYOYO-CD (IDXD)        NOT =   SPACE )
                   MOVE    SPACE       TO  WRK-MEDKBN
                   EVALUATE    TRUE
                   WHEN  ( HCN68S01-MEDKBN-DAYALL (1)      =   1 )
                   WHEN  ( HCN68S01-MEDKBN-DAY (1 IDXD)    >   ZERO )
                       MOVE    "3"     TO  WRK-MEDKBN
                   WHEN  ( HCN68S01-MEDKBN-DAYALL (2)      =   1 )
                   WHEN  ( HCN68S01-MEDKBN-DAY (2 IDXD)    >   ZERO )
                       MOVE    "2"     TO  WRK-MEDKBN
                   WHEN  OTHER
                       MOVE    "1"     TO  WRK-MEDKBN
                   END-EVALUATE
                   IF    ( WRK-MEDKBN  NOT =    SPACE )
                       STRING    WRK-MEDKBN     DELIMITED BY SIZE
                       INTO  TDAY-CSV
                       WITH  POINTER   TDAY-POS
                       END-STRING
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
           MOVE    TDAY-CSV            TO  SNINF04-MEDKBN-LV-DAY
      *
          .
       2001-MEDKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分編集処理
      *****************************************************************
       20011-MEDKBN-EDIT-SEC     SECTION.
      *
           COMPUTE SNINF04-MEDKBN-MAX  =   SNINF04-MEDKBN-MAX  +   1
           MOVE    SNINF04-MEDKBN-MAX  TO  IDXA
      *
           IF    ( HCN68S01-TMEDKBN (IDXM) =   "2" OR "3" )
               MOVE    HCN68S01-TMEDKBN (IDXM)
                                   TO  SNINF04-MEDKBN-LV (IDXA)
           END-IF
           MOVE    HCN68S01-TID (IDXM)
                                   TO  SNINF04-MEDKBN-ID (IDXA)
           MOVE    HCN68S01-TNAME (IDXM)
                                   TO  SNINF04-MEDKBN-NAME (IDXA)
           IF    ( SNINF04-MEDKBN-ID (IDXA)(1:1)   =   "0" )
               MOVE    SPACE       TO  SNINF04-MEDKBN-ID (IDXA)(1:1)
           END-IF
           IF    ( HCN68S01-TDAYALL (IDXM)
                                   =   1 )
               MOVE    "1"         TO  SNINF04-MEDKBN-MON (IDXA)
           END-IF
      *
           IF    ( HCN68S01-TDAY-G (IDXM)  NOT =   ZERO )
               INITIALIZE      TDAY-AREA
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    HCN68S01-TDAY (IDXM IDXD)
                                   TO  TDAY-DAY (IDXD)
               END-PERFORM
               PERFORM 20111-DAY-CSV-SEC
               MOVE    TDAY-CSV    TO  SNINF04-MEDKBN-DAY(IDXA)
           END-IF
      *
          .
       20011-MEDKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療区分編集処理
      *****************************************************************
       20011-MEDKBN-TUKI-HYOKA-EDIT-SEC     SECTION.
      *
           COMPUTE SNINF04-MEDKBN-MAX  =   SNINF04-MEDKBN-MAX  +   1
           MOVE    SNINF04-MEDKBN-MAX  TO  IDXA
      *
           EVALUATE    IDXM
           WHEN    1
               MOVE    "3"     TO  SNINF04-MEDKBN-LV (IDXA)
               MOVE    "M3"    TO  SNINF04-MEDKBN-ID (IDXA)
               MOVE    "医療区分３の該当有無"
                               TO  SNINF04-MEDKBN-NAME (IDXA)
           WHEN    2
               MOVE    "2"     TO  SNINF04-MEDKBN-LV (IDXA)
               MOVE    "M2"    TO  SNINF04-MEDKBN-ID (IDXA)
               MOVE    "医療区分２の該当有無"
                               TO  SNINF04-MEDKBN-NAME (IDXA)
           END-EVALUATE
      *
           IF    ( HCN68S01-MEDKBN-DAYALL (IDXM)   =   1 )
               MOVE    "1"         TO  SNINF04-MEDKBN-MON (IDXA)
           END-IF
      *
           INITIALIZE      TDAY-AREA
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    HCN68S01-MEDKBN-DAY (IDXM IDXD)
                                   TO  TDAY-DAY (IDXD)
           END-PERFORM
           PERFORM 20111-DAY-CSV-SEC
           MOVE    TDAY-CSV        TO  SNINF04-MEDKBN-DAY(IDXA)
      *
          .
       20011-MEDKBN-TUKI-HYOKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ点数編集処理
      *****************************************************************
       2001-ADLTEN-EDIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDXM    FROM    1   BY  1
                   UNTIL ( IDXM    >   HCN68S01-RYOLIST-MAX )
      *
               IF    ( HCN68S01-TZAISKBKBN (IDXM)  =   "1" )
      *
                   COMPUTE IDXA    =   IDXA    +   1
      *
                   MOVE    HCN68S01-TID (IDXM)
                                   TO  SNINF04-ADLKBN-ID (IDXA)
                   MOVE    HCN68S01-TNAME (IDXM)
                                   TO  SNINF04-ADLKBN-NAME (IDXA)
                   INITIALIZE          TDAY-AREA
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       MOVE    HCN68S01-TDAY (IDXM IDXD)
                                       TO  TDAY-DAY (IDXD)
                   END-PERFORM
                   PERFORM 20112-DAY-CSV-SEC
                   MOVE    TDAY-CSV    TO  SNINF04-ADLKBN-DAY (IDXA)
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE          TDAY-AREA
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    HCN68S01-ADLTEN-DAY (IDXD)
                               TO  TDAY-DAY (IDXD)
           END-PERFORM
           PERFORM 20112-DAY-CSV-SEC
           MOVE    TDAY-CSV    TO  SNINF04-ADLKBN-TTL-DAY
      *
          .
       2001-ADLTEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者の状態像評価編集処理
      *****************************************************************
       2001-PTEVAL-EDIT-SEC                SECTION.
      *
           INITIALIZE              TDAY-AREA
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    HCN68S01-PTSTS-DAY3 (IDXD)
                                   TO  TDAY-DAY (IDXD)
           END-PERFORM
           PERFORM 20113-DAY-CSV-SEC
           MOVE    TDAY-CSV        TO  SNINF04-PTEVAL-DAY
      *
           INITIALIZE              TDAY-AREA
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               IF    ( HCN68S01-PTSTS-DAY (IDXD)
                               NOT =   HCN68S01-PTSTS-DAY2 (IDXD))
                   MOVE    HCN68S01-PTSTS-DAY (IDXD)
                                   TO  TDAY-DAY (IDXD)
               END-IF
           END-PERFORM
           IF    ( TDAY-DAY-G      NOT =   ZERO )
               INITIALIZE              TDAY-AREA
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                   MOVE    HCN68S01-PTSTS-DAY   (IDXD)
                                   TO  TDAY-DAY (IDXD)
               END-PERFORM
               PERFORM 20113-DAY-CSV-SEC
               MOVE    TDAY-CSV    TO  SNINF04-PTEVAL-DIFF
           END-IF
      *
          .
       2001-PTEVAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＥＳＩＧＮ−Ｒ合計点編集処理
      *****************************************************************
       2001-DESIGNR-EDIT-SEC                SECTION.
      *
           PERFORM VARYING IDXM
                   FROM    HCN68S01-RYOLIST-DESIGNR-STIDX
                   BY      1
                   UNTIL ( IDXM    >   HCN68S01-RYOLIST-MAX )
      *
               IF    ( HCN68S01-TZAISKBKBN (IDXM)  =   "A" )
                   INITIALIZE              TDAY-AREA
                   MOVE    1       TO      TDAY-POS
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
      *
                       IF    ( IDXD        =   1 )
                           CONTINUE
                       ELSE
                           STRING    ","   DELIMITED BY SIZE
                           INTO  TDAY-CSV
                           WITH  POINTER   TDAY-POS
                           END-STRING
                       END-IF
      *
                       IF    ( HCN68S01-DESIGNR-EVAL-DAY (IDXD)
                                           >   ZERO )
                           MOVE    HCN68S01-TDAY (IDXM IDXD)
                                           TO  WRK-Z9
                           STRING  FUNCTION TRIM (WRK-Z9-G)
                                           DELIMITED BY SIZE
                           INTO  TDAY-CSV
                           WITH  POINTER   TDAY-POS
                           END-STRING
                       END-IF
                   END-PERFORM
                   MOVE    TDAY-CSV        TO  SNINF04-DESIGNR-DAY
               END-IF
      *
           END-PERFORM
      *
          .
       2001-DESIGNR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    評価日情報ＣＳＶ編集処理（医療区分）
      *****************************************************************
       20111-DAY-CSV-SEC                   SECTION.
      *
           MOVE    SPACE               TO  TDAY-CSV
      *
           MOVE    1                   TO  TDAY-POS
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( IDXD        =   1 )
                   CONTINUE
               ELSE
                   STRING    ","   DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
               IF    ( TDAY-DAY (IDXD) >   ZERO )
                   STRING    "1"   DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
          .
       20111-DAY-CSV-EXT.
           EXIT.
      *****************************************************************
      *    評価日情報ＣＳＶ編集処理（ＡＤＬ点数）
      *****************************************************************
       20112-DAY-CSV-SEC                   SECTION.
      *
           MOVE    SPACE               TO  TDAY-CSV
      *
           MOVE    1                   TO  TDAY-POS
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( IDXD        =   1 )
                   CONTINUE
               ELSE
                   STRING    ","   DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
               EVALUATE    TRUE
               WHEN  ( HCN68S01-RYOYO-CD (IDXD)        NOT =   SPACE )
               WHEN  ( HCN68S01-RANGE                      =   "0" )
                AND  ( HCN68S01-NYUIN-DAY (IDXD)           >   ZERO )
                AND  ( TDAY-DAY (IDXD)                     >   ZERO )
                   MOVE    TDAY-DAY (IDXD)
                                   TO  WRK-ADLTEN
                   IF    ( WRK-ADLTEN (1:1)    =   "0" )
                       STRING    WRK-ADLTEN-G(2:) 
                                               DELIMITED BY SIZE
                       INTO  TDAY-CSV
                       WITH  POINTER   TDAY-POS
                       END-STRING
                   ELSE
                       STRING    WRK-ADLTEN-G  DELIMITED BY SIZE
                       INTO  TDAY-CSV
                       WITH  POINTER   TDAY-POS
                       END-STRING
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
      *
          .
       20112-DAY-CSV-EXT.
           EXIT.
      *****************************************************************
      *    評価日情報ＣＳＶ編集処理（患者状態）
      *****************************************************************
       20113-DAY-CSV-SEC                   SECTION.
      *
           MOVE    SPACE               TO  TDAY-CSV
      *
           MOVE    1                   TO  TDAY-POS
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               IF    ( IDXD        =   1 )
                   CONTINUE
               ELSE
                   STRING    ","   DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
               MOVE    SPACE       TO  WRK-PTEVAL
               EVALUATE    TDAY-DAY (IDXD)
               WHEN    1
                   MOVE    "A"     TO  WRK-PTEVAL
               WHEN    2
                   MOVE    "B"     TO  WRK-PTEVAL
               WHEN    3
                   MOVE    "C"     TO  WRK-PTEVAL
               WHEN    4
                   MOVE    "D"     TO  WRK-PTEVAL
               WHEN    5
                   MOVE    "E"     TO  WRK-PTEVAL
               WHEN    6
                   MOVE    "F"     TO  WRK-PTEVAL
               WHEN    7
                   MOVE    "G"     TO  WRK-PTEVAL
               WHEN    8
                   MOVE    "H"     TO  WRK-PTEVAL
               WHEN    9
                   MOVE    "I"     TO  WRK-PTEVAL
               END-EVALUATE
      *
               IF    ( WRK-PTEVAL  NOT =   SPACE )
                   STRING    WRK-PTEVAL    DELIMITED BY SIZE
                   INTO  TDAY-CSV
                   WITH  POINTER   TDAY-POS
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
          .
       20113-DAY-CSV-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    KEY-SRYYM           TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    WRK-ZAISKBKBN       TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    KEY-SRYYM       TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
