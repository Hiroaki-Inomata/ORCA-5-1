      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
      *    ø«Œ≈ ÛΩ∑¿¡µ·ΩÒ     COPY (CPSKT3750TMP.INC)              *
      *
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘  ∆¸…’        ∆‚Õ∆
      *  01.00.01    µ»¿Ó    2008/07/09  ƒ¢…ºÕÕº∞§Œ —ππ§À»º§¶Ω§¿µ
      *  01.00.02    ÃÁ¥÷    2019/01/07  Ω§¿µÕ˙ŒÚ•≥•·•Û•»∫ÔΩ¸
      ************************************************************
       01  SKT3920TMP-REC.
           03  SKT3920TMP-KEY.
               05  SKT3920TMP-SYUTURYOKU            PIC  9(01).
               05  SKT3920TMP-HKNJANUM              PIC  X(06).
               05  SKT3920TMP-ZAITAKU               PIC  X(01).
               05  SKT3920TMP-KYURATE               PIC  9(02).
           03  SKT3920TMP-PAGE-FLG1                 PIC  9(01).
           03  SKT3920TMP-PAGE-FLG2                 PIC  9(01).
           03  SKT3920TMP-HKN-AREA  OCCURS  11.
               05  SKT3920TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3920TMP-KENSU             PIC  9(05).
                   07  SKT3920TMP-NISSU             PIC  9(05).
                   07  SKT3920TMP-TENSU             PIC  9(09).
                   07  SKT3920TMP-YKZFTN            PIC  9(07).
                   07  SKT3920TMP-ITBFTN            PIC  9(07).
               05  SKT3920TMP-S-KENSU               PIC  9(05).
               05  SKT3920TMP-S-NISSU               PIC  9(05).
               05  SKT3920TMP-S-KINGK               PIC  9(09).
               05  SKT3920TMP-S-FTNGK               PIC  9(09).
           03  SKT3920TMP-KOH-AREA  OCCURS  20.
               05  SKT3920TMP-KOHNUM                PIC  X(03).
               05  SKT3920TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT3920TMP-KKENSU            PIC  9(05).
                   07  SKT3920TMP-KNISSU            PIC  9(05).
                   07  SKT3920TMP-KTENSU            PIC  9(09).
                   07  SKT3920TMP-KYKZFTN           PIC  9(07).
                   07  SKT3920TMP-KITBFTN           PIC  9(07).
               05  SKT3920TMP-S-KKENSU              PIC  9(05).
               05  SKT3920TMP-S-KNISSU              PIC  9(05).
               05  SKT3920TMP-S-KKINGK              PIC  9(09).
               05  SKT3920TMP-S-KFTNGK              PIC  9(09).
           03  SKT3920TMP-GENBK     OCCURS 2        PIC  9(04).
