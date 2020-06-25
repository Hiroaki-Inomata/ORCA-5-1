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
      *                                     open-cobol1.0対応
      * 01.00.06     @@@@-門間    14/03/24  消費税対応
      * 02.00.01     @@@@-門間    17/03/15  負担割合４桁へ修正
      ********************************************************************
        01  HCM03V02B.
            02  HCM03V02B-TYPE-A.
              03  HCM03V02B-NO         PIC X(7).
              03  HCM03V02B-LBLSRYYMD  PIC X(10).
              03  HCM03V02B-SRYYMD     PIC X(50).
              03  HCM03V02B-KNJNAMW    PIC X(30).
              03  HCM03V02B-KNJKANANAME    PIC X(50).
              03  HCM03V02B-KNJNO  PIC X(20).
              03  HCM03V02B-MIDASI PIC X(40).
              03  HCM03V02B-HAKYMD PIC X(22).
              03  HCM03V02B-HOKEN  PIC X(20).
              03  HCM03V02B-FUTAN  PIC X(3).
              03  HCM03V02B-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
              03  HCM03V02B-HONKZKKBN  PIC X(04).
              03  HCM03V02B-TANJYOYMD  PIC X(24).
              03  HCM03V02B-SEX        PIC X(02).
              03  HCM03V02B-HKNRYO1    PIC X(18).
              03  HCM03V02B-HKNRYO2    PIC X(18).
              03  HCM03V02B-HKNRYO3    PIC X(18).
              03  HCM03V02B-HKNRYO4    PIC X(18).
              03  HCM03V02B-HKNRYO5    PIC X(18).
              03  HCM03V02B-HKNRYO6    PIC X(18).
              03  HCM03V02B-HKNRYO7    PIC X(18).
              03  HCM03V02B-HKNRYO8    PIC X(18).
              03  HCM03V02B-HKNRYO9    PIC X(18).
              03  HCM03V02B-HKNRYO10   PIC X(18).
              03  HCM03V02B-HKNRYO11   PIC X(18).
              03  HCM03V02B-HKNRYO12   PIC X(18).
              03  HCM03V02B-HKNRYO13   PIC X(18).
              03  HCM03V02B-HKNRYO14   PIC X(18).
              03  HCM03V02B-HKNRYO15   PIC X(18).
              03  HCM03V02B-HKNRYO16   PIC X(18).
              03  HCM03V02B-GOKTEN PIC X(18).
              03  HCM03V02B-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCM03V02B-HOKENBUN   PIC X(18).
              03  HCM03V02B-HOKENGAI   PIC X(18).
              03  HCM03V02B-HOKENZEI   PIC X(18).
              03  HCM03V02B-JIHIMSG1   PIC X(20).
              03  HCM03V02B-JIHIMSG2   PIC X(20).
              03  HCM03V02B-JIHIMSG3   PIC X(20).
              03  HCM03V02B-JIHIMSG4   PIC X(20).
              03  HCM03V02B-JIHIMSG5   PIC X(20).
              03  HCM03V02B-JIHIMSG6   PIC X(20).
              03  HCM03V02B-JIHIMSG7   PIC X(20).
              03  HCM03V02B-JIHIMSG8   PIC X(20).
              03  HCM03V02B-JIHIMSG9   PIC X(20).
              03  HCM03V02B-JIHIMSG10  PIC X(20).
              03  HCM03V02B-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCM03V02B-YAKUZAI    PIC X(18).
              03  HCM03V02B-ROUJIN PIC X(18).
              03  HCM03V02B-KOUHI  PIC X(18).
              03  HCM03V02B-CHOSEI PIC X(18).
              03  HCM03V02B-KONSEIKYU  PIC X(18).
              03  HCM03V02B-SEIKYUZEI  PIC X(18).
              03  HCM03V02B-ZENMISYU   PIC X(18).
              03  HCM03V02B-GOUKEIGAK  PIC X(18).
              03  HCM03V02B-KONNYUKIN  PIC X(18).
              03  HCM03V02B-IRYOADRS   PIC X(100).
              03  HCM03V02B-IRYOMEI    PIC X(100).
              03  HCM03V02B-IRYOTEL    PIC X(15).
              03  HCM03V02B-MIDASHI    PIC X(10).
              03  HCM03V02B-RSIBUN     PIC X(14).
              03  HCM03V02B-RSIEN      PIC X(02).
              03  HCM03V02B-RSIMIDASI  PIC X(12).
              03  HCM03V02B-RSIMIDASI2  PIC X(02).
              03  HCM03V02B-RSIMIDASI3  PIC X(02).
              03  HCM03V02B-CHUI        PIC X(100).
            02  HCM03V02B-TYPE-B.
              03  HCM03V02B-B-NO         PIC X(7).
              03  HCM03V02B-B-LBLSRYYMD  PIC X(10).
              03  HCM03V02B-B-SRYYMD     PIC X(50).
              03  HCM03V02B-B-KNJNAMW    PIC X(30).
              03  HCM03V02B-B-KNJKANANAME    PIC X(50).
              03  HCM03V02B-B-KNJNO  PIC X(20).
              03  HCM03V02B-B-MIDASI PIC X(40).
              03  HCM03V02B-B-HAKYMD PIC X(22).
              03  HCM03V02B-B-HOKEN  PIC X(20).
              03  HCM03V02B-B-FUTAN  PIC X(3).
              03  HCM03V02B-B-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
              03  HCM03V02B-B-HONKZKKBN  PIC X(04).
              03  HCM03V02B-B-TANJYOYMD  PIC X(24).
              03  HCM03V02B-B-SEX        PIC X(02).
              03  HCM03V02B-B-HKNRYO1    PIC X(18).
              03  HCM03V02B-B-HKNRYO2    PIC X(18).
              03  HCM03V02B-B-HKNRYO3    PIC X(18).
              03  HCM03V02B-B-HKNRYO4    PIC X(18).
              03  HCM03V02B-B-HKNRYO5    PIC X(18).
              03  HCM03V02B-B-HKNRYO6    PIC X(18).
              03  HCM03V02B-B-HKNRYO7    PIC X(18).
              03  HCM03V02B-B-HKNRYO8    PIC X(18).
              03  HCM03V02B-B-HKNRYO9    PIC X(18).
              03  HCM03V02B-B-HKNRYO10   PIC X(18).
              03  HCM03V02B-B-HKNRYO11   PIC X(18).
              03  HCM03V02B-B-HKNRYO12   PIC X(18).
              03  HCM03V02B-B-HKNRYO13   PIC X(18).
              03  HCM03V02B-B-HKNRYO14   PIC X(18).
              03  HCM03V02B-B-HKNRYO15   PIC X(18).
              03  HCM03V02B-B-HKNRYO16   PIC X(18).
              03  HCM03V02B-B-GOKTEN  PIC X(18).
              03  HCM03V02B-B-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCM03V02B-B-HOKENBUN   PIC X(18).
              03  HCM03V02B-B-HOKENGAI   PIC X(18).
              03  HCM03V02B-B-HOKENZEI   PIC X(18).
              03  HCM03V02B-B-JIHIMSG1   PIC X(20).
              03  HCM03V02B-B-JIHIMSG2   PIC X(20).
              03  HCM03V02B-B-JIHIMSG3   PIC X(20).
              03  HCM03V02B-B-JIHIMSG4   PIC X(20).
              03  HCM03V02B-B-JIHIMSG5   PIC X(20).
              03  HCM03V02B-B-JIHIMSG6   PIC X(20).
              03  HCM03V02B-B-JIHIMSG7   PIC X(20).
              03  HCM03V02B-B-JIHIMSG8   PIC X(20).
              03  HCM03V02B-B-JIHIMSG9   PIC X(20).
              03  HCM03V02B-B-JIHIMSG10  PIC X(20).
              03  HCM03V02B-B-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCM03V02B-B-YAKUZAI    PIC X(18).
              03  HCM03V02B-B-ROUJIN PIC X(18).
              03  HCM03V02B-B-KOUHI  PIC X(18).
              03  HCM03V02B-B-CHOSEI PIC X(18).
              03  HCM03V02B-B-KONSEIKYU  PIC X(18).
              03  HCM03V02B-B-SEIKYUZEI  PIC X(18).
              03  HCM03V02B-B-ZENMISYU   PIC X(18).
              03  HCM03V02B-B-GOUKEIGAK  PIC X(18).
              03  HCM03V02B-B-KONNYUKIN  PIC X(18).
              03  HCM03V02B-B-IRYOADRS   PIC X(100).
              03  HCM03V02B-B-IRYOMEI    PIC X(100).
              03  HCM03V02B-B-IRYOTEL    PIC X(15).
              03  HCM03V02B-B-MIDASHI    PIC X(10).
              03  HCM03V02B-B-RSIBUN     PIC X(14).
              03  HCM03V02B-B-RSIEN      PIC X(02).
              03  HCM03V02B-B-RSIMIDASI  PIC X(12).
              03  HCM03V02B-B-RSIMIDASI2  PIC X(02).
              03  HCM03V02B-B-RSIMIDASI3  PIC X(02).
              03  HCM03V02B-B-CHUI        PIC X(100).
            02  HCM03V02B-FUTAN2    PIC X(4).
            02  HCM03V02B-B-FUTAN2  PIC X(4).
