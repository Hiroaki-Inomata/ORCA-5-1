      ******************************************************************
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
      ******************************************************************
      *    入院点滴集計サブ（会計カード用）   COPY (CPORCSTENTEK3.INC)
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
       01  ORCSTENTEK3AREA.
      *
      *医療機関ＩＤ
           03  LNK-ORCSTEN3-HOSPNUM                  PIC  9(02).
      *入外区分
           03  LNK-ORCSTEN3-NYUGAIKBN                PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSTEN3-PTID                     PIC  9(10).
      *診療年月
           03  LNK-ORCSTEN3-SRYYM                    PIC  X(06).
      *保険組合せ
           03  LNK-ORCSTEN3-HKNCOMBI                 PIC  9(04).
      *
      *返却項目
           03  LNK-ORCSTEN3-OUT-AREA.
      *
      *       点滴まとめ（日別）
              04  LNK-ORCSTEN3-TABLE.
                05  LNK-ORCSTEN3-MAX                 PIC  9(05).
                05  LNK-ORCSTEN3-OCC                 OCCURS   150.
                  06  LNK-ORCSTEN3-SBT               PIC  X(01).
                  06  LNK-ORCSTEN3-OCC2-MAX          PIC  9(05).
                  06  LNK-ORCSTEN3-OCC2              OCCURS   350.
                    07  LNK-ORCSTEN3-SRYCD           PIC  X(09).
                    07  LNK-ORCSTEN3-SURYO           PIC  9(05)V9(05).
                    07  LNK-ORCSTEN3-YUKOKETA        PIC  9(03).
                    07  LNK-ORCSTEN3-COM             PIC  X(200).
                  06  LNK-ORCSTEN3-ZAITEN            PIC  9(07).
                  06  LNK-ORCSTEN3-ZAIKAISU          PIC  9(03).
                  06  LNK-ORCSTEN3-DAY-G.
                    07  LNK-ORCSTEN3-DAY             PIC  9(03)
                                                     OCCURS   31.
                  06  LNK-ORCSTEN3-MEISAISU          PIC  9(03).
                  06  LNK-ORCSTEN3-SRYCDTOTAL        PIC  9(14).
                  06  LNK-ORCSTEN3-SURYOUTOTAL       PIC  9(07)V9(05).
                  06  LNK-ORCSTEN3-RUIKEITOTAL       PIC  9(13)V9(05).
                  06  LNK-ORCSTEN3-COMCNT            PIC  9(03).
      *
      *       以下内訳
      *       点滴注射の手技・加算（日別）
              04  LNK-ORCSTEN3-SYU-TABLE.
                05  LNK-ORCSTEN3-SYU-OCC             OCCURS   31.
                  06  LNK-ORCSTEN3-SYU-OCC2          OCCURS   30.
                    07  LNK-ORCSTEN3-SYU-SRYCD       PIC  X(09).
                    07  LNK-ORCSTEN3-SYU-SURYO       PIC  9(05)V9(05).
                    07  LNK-ORCSTEN3-SYU-YUKOKETA    PIC  9(03).
                    07  LNK-ORCSTEN3-SYU-COM         PIC  X(200).
                  06  LNK-ORCSTEN3-SYU-ZAITEN        PIC  9(07).
                  06  LNK-ORCSTEN3-SYU-ZAIKAISU      PIC  9(03).
                  06  LNK-ORCSTEN3-SYU-DAY-G.
                    07  LNK-ORCSTEN3-SYU-DAY         PIC  9(03)
                                                     OCCURS   31.
                  06  LNK-ORCSTEN3-SYU-MEISAISU      PIC  9(03).
                  06  LNK-ORCSTEN3-SYU-SRYCDTOTAL    PIC  9(14).
                  06  LNK-ORCSTEN3-SYU-SURYOUTOTAL   PIC  9(07)V9(05).
                  06  LNK-ORCSTEN3-SYU-RUIKEITOTAL   PIC  9(13)V9(05).
                  06  LNK-ORCSTEN3-SYU-COMCNT        PIC  9(03).
      *
      *       中心静脈注射の手技・加算（日別）
              04  LNK-ORCSTEN3-SYU2-TABLE.
                05  LNK-ORCSTEN3-SYU2-OCC            OCCURS   31.
                  06  LNK-ORCSTEN3-SYU2-OCC2         OCCURS   30.
                    07  LNK-ORCSTEN3-SYU2-SRYCD      PIC  X(09).
                    07  LNK-ORCSTEN3-SYU2-SURYO      PIC  9(05)V9(05).
                    07  LNK-ORCSTEN3-SYU2-YUKOKETA   PIC  9(03).
                    07  LNK-ORCSTEN3-SYU2-COM        PIC  X(200).
                  06  LNK-ORCSTEN3-SYU2-ZAITEN       PIC  9(07).
                  06  LNK-ORCSTEN3-SYU2-ZAIKAISU     PIC  9(03).
                  06  LNK-ORCSTEN3-SYU2-DAY-G.
                    07  LNK-ORCSTEN3-SYU2-DAY        PIC  9(03)
                                                     OCCURS   31.
                  06  LNK-ORCSTEN3-SYU2-MEISAISU     PIC  9(03).
                  06  LNK-ORCSTEN3-SYU2-SRYCDTOTAL   PIC  9(14).
                  06  LNK-ORCSTEN3-SYU2-SURYOUTOTAL  PIC  9(07)V9(05).
                  06  LNK-ORCSTEN3-SYU2-RUIKEITOTAL  PIC  9(13)V9(05).
                  06  LNK-ORCSTEN3-SYU2-COMCNT       PIC  9(03).
      *
      *       薬剤まとめ（日別）
              04  LNK-ORCSTEN3-YAK-TABLE.
                05  LNK-ORCSTEN3-YAK-OCC             OCCURS   31.
                  06  LNK-ORCSTEN3-YAK-OCC2          OCCURS  350.
                    07  LNK-ORCSTEN3-YAK-SRYCD       PIC  X(09).
                    07  LNK-ORCSTEN3-YAK-SURYO       PIC  9(05)V9(05).
                    07  LNK-ORCSTEN3-YAK-YUKOKETA    PIC  9(03).
                    07  LNK-ORCSTEN3-YAK-COM         PIC  X(200).
                  06  LNK-ORCSTEN3-YAK-ZAITEN        PIC  9(07).
                  06  LNK-ORCSTEN3-YAK-ZAIKAISU      PIC  9(03).
                  06  LNK-ORCSTEN3-YAK-DAY-G.
                    07  LNK-ORCSTEN3-YAK-DAY         PIC  9(03)
                                                     OCCURS   31.
                  06  LNK-ORCSTEN3-YAK-MEISAISU      PIC  9(03).
                  06  LNK-ORCSTEN3-YAK-SRYCDTOTAL    PIC  9(14).
                  06  LNK-ORCSTEN3-YAK-SURYOUTOTAL   PIC  9(07)V9(05).
                  06  LNK-ORCSTEN3-YAK-RUIKEITOTAL   PIC  9(13)V9(05).
                  06  LNK-ORCSTEN3-YAK-COMCNT        PIC  9(03).
      *
      *       器材まとめ（剤別）
              04  LNK-ORCSTEN3-KIZ-TABLE.
                05  LNK-ORCSTEN3-KIZ-OCC             OCCURS   50.
                  06  LNK-ORCSTEN3-KIZ-OCC2          OCCURS   60.
                    07  LNK-ORCSTEN3-KIZ-SRYCD       PIC  X(09).
                    07  LNK-ORCSTEN3-KIZ-SURYO       PIC  9(05)V9(05).
                    07  LNK-ORCSTEN3-KIZ-YUKOKETA    PIC  9(03).
                    07  LNK-ORCSTEN3-KIZ-COM         PIC  X(200).
                  06  LNK-ORCSTEN3-KIZ-ZAITEN        PIC  9(07).
                  06  LNK-ORCSTEN3-KIZ-ZAIKAISU      PIC  9(03).
                  06  LNK-ORCSTEN3-KIZ-DAY-G.
                    07  LNK-ORCSTEN3-KIZ-DAY         PIC  9(03)
                                                     OCCURS   31.
      *
      *       まとめ対象剤番号
              04  LNK-ORCSTEN3-ZAINUM-TABLE.
                05  LNK-ORCSTEN3-ZAINUM              PIC  9(08)
                                                     OCCURS  100.
      *
      *復帰コード
           03  LNK-ORCSTEN3-RC                       PIC  9(02).
      *
