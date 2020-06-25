      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      ************************************************************
      *    ÆþÂà±¡ÅÐÏ¿¡ÝÀÁµá³ÎÇ§²èÌÌ
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ************************************************************
      *
        02  I04.
      *     02  I04-FIXED3.
              03  I04-B05-LABEL            PIC X(10).
              03  I04-B07-LABEL            PIC X(10).
              03  I04-LBLTAIINKEISAN-G.
                04  I04-LBLTAIINKEISAN-STYLE
                                         PIC X(20).
                04  I04-LBLTAIINKEISAN   PIC X(20).
              03  I04-LBLRSITTL        PIC X(30).
              03  I04-LBLRSISYO        PIC X(10).
              03  I04-LBLRSISAI        PIC X(10).
              03  I04-LBLRSISDO        PIC X(10).
              03  I04-LBLRSIETC-STYLE  PIC X(20).
              03  I04-LBLRSIETC        PIC X(10).
              03  I04-LBLGENMEN-G.
	          05   I04-LBLGENMEN-STYLE PIC X(20).
	          05   I04-LBLGENMEN       PIC X(30).
              03  I04-PTNUM.
                04  I04-VALUE   PIC X(20).
              03  I04-KANANAME.
                04  I04-VALUE   PIC X(50).
              03  I04-SEX.
                04  I04-VALUE   PIC X(2).
              03  I04-HKNCOMBI.
                04  I04-VALUE   PIC X(60).
              03  I04-FTNRATE.
                04  I04-VALUE   PIC X(5).
              03  I04-NAME.
                04  I04-VALUE   PIC X(50).
              03  I04-BIRTHDAY.
                04  I04-VALUE   PIC X(9).
              03  I04-NENREI.
                04  I04-VALUE   PIC X(8).
              03  I04-SRYKA.
                04  I04-VALUE   PIC X(10).
              03  I04-TAIINCD.
                04  I04-VALUE   PIC X(10).
              03  I04-HAKYMD.
                04  I04-VALUE   PIC X(9).
              03  I04-NYUINYMD.
                04  I04-VALUE   PIC X(14).
              03  I04-TAIINYMD.
                04  I04-VALUE   PIC X(14).
              03  I04-SSUHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-SDOHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-ZTKHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-TYKHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-CSYHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-SYCHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-SJTHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-MSIHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-KNSHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-GZUHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-ETCHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-SSNHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-HOUHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-BYRHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-NYUHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-RYOHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-TOTALHKNTEN.
                04  I04-VALUE   PIC X(10).
              03  I04-SUUTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-SDOTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-ZTKTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-TYKTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-CSYTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-SYCTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-SJTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-MSITGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-KNSTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-GZUTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-ETCTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-SSNTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-HOUTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-BYRTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-NYUTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-RYOTGMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHIMSG1.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG2.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG3.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG4.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG5.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG6.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG7.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG8.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG9.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHIMSG10.
                04  I04-VALUE   PIC X(12).
              03  I04-JIHI1.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI2.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI3.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI4.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI5.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI6.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI7.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI8.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI9.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHI10.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHIGOK.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX1.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX2.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX3.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX4.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX5.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX6.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX7.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX8.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX9.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX10.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAXGOK.
                04  I04-VALUE   PIC X(10).
              03  I04-JIHITAX.
                04  I04-VALUE   PIC X(10).
              03  I04-RYOYOHI-SKJ.
                04  I04-VALUE   PIC X(10).
              03  I04-RYOYOHI-LIFE.
                04  I04-VALUE   PIC X(10).
              03  I04-SKYMONEY-SKJ.
                04  I04-VALUE   PIC X(10).
              03  I04-SKYMONEY-LIFE.
                04  I04-VALUE   PIC X(10).
              03  I04-RJNFTN.
                04  I04-VALUE   PIC X(10).
              03  I04-KOHFTN.
                04  I04-VALUE   PIC X(10).
              03  I04-FTNTOTAL.
                04  I04-VALUE   PIC X(10).
              03  I04-ROUSYOSIN.
                04  I04-VALUE   PIC X(10).
              03  I04-ROUSAISIN.
                04  I04-VALUE   PIC X(10).
              03  I04-ROUSIDOU.
                04  I04-VALUE   PIC X(10).
              03  I04-ROUETC-STYLE  PIC X(20).
              03  I04-ROUETC.
                04  I04-VALUE   PIC X(10).
              03  I04-SKYMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-NYUKIN.
                04  I04-VALUE   PIC X(10).
              03  I04-NYUKIN-STT
                                PIC S9(9)   BINARY.
              03  I04-CMB-NYKNHOHO.
                04  I04-NYKNHOHO.
                  05  I04-VALUE        PIC X(33).
                04  I04-NYKNHOHO-CNT   PIC S9(9)   BINARY.
                04  I04-NYKNHOHO-ITM   PIC X(33)   OCCURS  99 TIMES.
              03  I04-CMB-NYKNKNRKBN.
                04  I04-NYKNKNRKBN.
                  05  I04-VALUE          PIC X(42).
                04  I04-NYKNKNRKBN-CNT   PIC S9(9)   BINARY.
                04  I04-NYKNKNRKBN-ITM   PIC X(42)   OCCURS  10 TIMES.
              03  I04-HKNTEKMONEY.
                04  I04-VALUE   PIC X(10).
              03  I04-HKNTEKGAI.
                04  I04-VALUE   PIC X(10).
              03  I04-MISYUZAN-GAI-G.
                04  I04-MISYUZAN-GAI-STYLE PIC X(20).
                04  I04-MISYUZAN-GAI   PIC X(40).
              03  I04-MISYUZAN-NYU-G.
                04  I04-MISYUZAN-NYU-STYLE PIC X(20).
                04  I04-MISYUZAN-NYU   PIC X(10).
              03  I04-KANYUZAN-NYU-G.
                04  I04-KANYUZAN-NYU-STYLE PIC X(20).
                04  I04-KANYUZAN-NYU   PIC X(10).
              03  I04-GOKSKY.
                04  I04-VALUE   PIC X(10).
              03  I04-LBLNYUKINMAX.
                04  I04-VALUE   PIC X(12).
              03  I04-NYUKINMAX.
                04  I04-VALUE   PIC X(12).
              03  I04-LBLNYUKINTTL.
                04  I04-VALUE   PIC X(12).
              03  I04-NYUKINTTL.
                04  I04-VALUE   PIC X(12).
              03  I04-RMSAGAKU.
                04  I04-VALUE   PIC X(10).
              03  I04-CHOSEI-G.
                  05  I04-CHOSEI-OCC   OCCURS  2.
                      07   I04-CHOSEI  PIC X(10).
                      07   I04-CHOSEI-STT
                                       PIC S9(9)   BINARY.
              03  I04-SELNUM.
                04  I04-VALUE   PIC S9(2).
              03  I04-CMB-SKYPRTOUT.
                04  I04-SKYPRTOUT.
                  05  I04-VALUE PIC X(30).
                04  I04-SKYPRTOUT-CNT
                                PIC S9(9)   BINARY.
                04  I04-SKYPRTOUT-ITM
                                PIC X(30)   OCCURS  3 TIMES.
              03  I04-CMB-SKYSUMKBN.
                04  I04-SKYSUMKBN.
                  05  I04-VALUE PIC X(30).
                04  I04-SKYSUMKBN-CNT
                                PIC S9(9)   BINARY.
                04  I04-SKYSUMKBN-ITM
                                PIC X(30)   OCCURS  4 TIMES.
              03  I04-CMB-SRYPRTOUT.
                04  I04-SRYPRTOUT.
                  05  I04-VALUE PIC X(30).
                04  I04-SRYPRTOUT-CNT
                                PIC S9(9)   BINARY.
                04  I04-SRYPRTOUT-ITM
                                PIC X(30)   OCCURS  3 TIMES.
              03  I04-CMB-TAIPRTOUT.
                04  I04-TAIPRTOUT.
                  05  I04-VALUE PIC X(20).
                04  I04-TAIPRTOUT-CNT
                                PIC S9(9)   BINARY.
                04  I04-TAIPRTOUT-ITM    PIC X(20)   OCCURS  2 TIMES.
              03  I04-CMB-DR.
                04  I04-DR.
                  05  I04-VALUE   PIC X(29).
                04  I04-DR-CNT   PIC S9(9)   BINARY.
                04  I04-DR-ITM    PIC X(29)   OCCURS  99 TIMES.
              03  I04-CMB-USERPG.
                04  I04-USERPG.
                  05  I04-VALUE   PIC X(22).
                04  I04-USERPG-STT    PIC S9(9)   BINARY.
                04  I04-USERPG-CNT    PIC S9(9)   BINARY.
                04  I04-USERPG-ITM    PIC X(22)   OCCURS  2 TIMES.
                04  I04-USERPG-ITM-STT PIC S9(9)   BINARY.
              03  I04-LBLSKYINF   PIC X(50).
              03  I04-LBLINFO-G.
                04  I04-LBLINFO-STYLE
                                           PIC X(20).
                04  I04-LBLINFO            PIC X(70).
              03  I04-LBLINFO2             PIC X(70).
              03  I04-SCROLLEDWINDOW1.
                04  I04-HKNCOMBILST.
                  05  I04-HKNCOMBILST-CNT  PIC S9(9)   BINARY.
                  05  I04-HKNCOMBILST-ROW  PIC S9(9)   BINARY.
                  05  I04-HKNCOMBILST-ROWATTR PIC S9(9)   BINARY.
                  05  I04-HKNCOMBILST-ITM  OCCURS  15 TIMES.
                    06  I04-TSELNUM        PIC X(02).
                    06  I04-THKNCOMBI      PIC X(50).
                  05  I04-HKNCOMBILST-SELECT
                                           PIC X     OCCURS  15 TIMES.
              03  I04-CMB-SHOHOSEN.
                04  I04-SHOHOSEN.
                  05  I04-VALUE   PIC X(20).
                04  I04-SHOHOSEN-COUNT   PIC S9(9)   BINARY.
                04  I04-SHOHOSEN-ITEM
                        PIC X(20)   OCCURS  2 TIMES.

