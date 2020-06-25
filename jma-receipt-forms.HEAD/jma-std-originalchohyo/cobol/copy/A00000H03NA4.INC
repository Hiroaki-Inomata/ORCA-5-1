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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      * 01.00.08     @@@@-門間    14/02/07  プログラムオプション対応（訂正印字）
      * 01.00.09     @@@@-門間    14/03/24  消費税対応
      ********************************************************************
        01  HCN03V02.
            02  HCN03V02-TYPE-A.
              03  HCN03V02-NO PIC X(7).
              03  HCN03V02-NYUINSTRYMD    PIC X(22).
              03  HCN03V02-NYUINENDYMD    PIC X(22).
              03  HCN03V02-HAKYMD PIC X(22).
              03  HCN03V02-KANANAME   PIC X(50).
              03  HCN03V02-KNJNAMW    PIC X(30).
              03  HCN03V02-KNJNO  PIC X(20).
              03  HCN03V02-ROOM   PIC X(50).
              03  HCN03V02-HOKEN  PIC X(20).
              03  HCN03V02-FUTAN  PIC X(6).
              03  HCN03V02-MIDASI PIC X(20).
              03  HCN03V02-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
              03  HCN03V02-HONKZKKBN    PIC X(4).
              03  HCN03V02-TANJYOYMD    PIC X(24).
              03  HCN03V02-SEX    PIC X(02).
              03  HCN03V02-HKNRYO
                      PIC X(18)   OCCURS  17 TIMES.
              03  HCN03V02-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCN03V02-SKJRYO     PIC X(18).
              03  HCN03V02-LIFERYO    PIC X(18).
              03  HCN03V02-HOKENBUN   PIC X(18).
              03  HCN03V02-HOKENGAI   PIC X(18).
              03  HCN03V02-HOKENZEI   PIC X(18).
              03  HCN03V02-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCN03V02-JIHIMSG
                      PIC X(20)   OCCURS  10 TIMES.
              03  HCN03V02-SYOKUJI    PIC X(18).
              03  HCN03V02-ROUJIN PIC X(18).
              03  HCN03V02-KOUHI  PIC X(18).
              03  HCN03V02-RMSAGAKU   PIC X(18).
              03  HCN03V02-RMZEI  PIC X(18).
              03  HCN03V02-CHOSEI PIC X(18).
              03  HCN03V02-KONSEIKYU  PIC X(18).
              03  HCN03V02-KONZEI     PIC X(18).
              03  HCN03V02-KONNYUKIN  PIC X(18).
              03  HCN03V02-IRYOADRS   PIC X(100).
              03  HCN03V02-IRYOMEI    PIC X(100).
              03  HCN03V02-IRYOTEL    PIC X(15).
              03  HCN03V02-MIDASHI    PIC X(10).
              03  HCN03V02-RSIBUN     PIC X(18).
              03  HCN03V02-RSIEN      PIC X(02).
              03  HCN03V02-RSIMIDASI  PIC X(16).
              03  HCN03V02-CHUI       PIC X(100).
            02  HCN03V02-TYPE-B.
              03  HCN03V02-B-NO PIC X(7).
              03  HCN03V02-B-NYUINSTRYMD    PIC X(22).
              03  HCN03V02-B-NYUINENDYMD    PIC X(22).
              03  HCN03V02-B-HAKYMD PIC X(22).
              03  HCN03V02-B-KANANAME   PIC X(50).
              03  HCN03V02-B-KNJNAMW    PIC X(30).
              03  HCN03V02-B-KNJNO  PIC X(20).
              03  HCN03V02-B-ROOM   PIC X(50).
              03  HCN03V02-B-HOKEN  PIC X(20).
              03  HCN03V02-B-FUTAN  PIC X(6).
              03  HCN03V02-B-MIDASI PIC X(20).
              03  HCN03V02-B-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
              03  HCN03V02-B-HONKZKKBN    PIC X(4).
              03  HCN03V02-B-TANJYOYMD    PIC X(24).
              03  HCN03V02-B-SEX    PIC X(02).
              03  HCN03V02-B-HKNRYO
                      PIC X(18)   OCCURS  17 TIMES.
              03  HCN03V02-B-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCN03V02-B-SKJRYO     PIC X(18).
              03  HCN03V02-B-LIFERYO    PIC X(18).
              03  HCN03V02-B-HOKENBUN   PIC X(18).
              03  HCN03V02-B-HOKENGAI   PIC X(18).
              03  HCN03V02-B-HOKENZEI   PIC X(18).
              03  HCN03V02-B-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCN03V02-B-JIHIMSG
                      PIC X(20)   OCCURS  10 TIMES.
              03  HCN03V02-B-SYOKUJI    PIC X(18).
              03  HCN03V02-B-ROUJIN PIC X(18).
              03  HCN03V02-B-KOUHI  PIC X(18).
              03  HCN03V02-B-RMSAGAKU   PIC X(18).
              03  HCN03V02-B-RMZEI  PIC X(18).
              03  HCN03V02-B-CHOSEI PIC X(18).
              03  HCN03V02-B-KONSEIKYU  PIC X(18).
              03  HCN03V02-B-KONZEI  PIC X(18).
              03  HCN03V02-B-KONNYUKIN  PIC X(18).
              03  HCN03V02-B-IRYOADRS   PIC X(100).
              03  HCN03V02-B-IRYOMEI    PIC X(100).
              03  HCN03V02-B-IRYOTEL    PIC X(15).
              03  HCN03V02-B-MIDASHI    PIC X(10).
              03  HCN03V02-B-RSIBUN     PIC X(18).
              03  HCN03V02-B-RSIEN      PIC X(02).
              03  HCN03V02-B-RSIMIDASI  PIC X(16).
              03  HCN03V02-B-CHUI       PIC X(100).
            02  HCN03V02-TYPE-C.
              03  HCN03V02-LBL-G-MISYU    PIC X(20).
              03  HCN03V02-LBL-G-MISYU-EN PIC X(2).
              03  HCN03V02-GAI-MISYUKIN   PIC X(18).
              03  HCN03V02-LBL-N-MISYU    PIC X(20).
              03  HCN03V02-LBL-N-MISYU-EN PIC X(2).
              03  HCN03V02-NYU-MISYUKIN   PIC X(18).
