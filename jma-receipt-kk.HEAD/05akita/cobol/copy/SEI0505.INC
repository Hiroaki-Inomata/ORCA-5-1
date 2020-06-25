      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *          ∞ÂŒ≈»Ò¿¡µ·ΩÒ°  £ºÃº∞•Ï•ª°À COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *£À£≈£Ÿ
               05  MF100-CITYNUM             PIC  X(05).
      *¿¡µ·•®•Í•¢
           03  MF100-SEIKYU-AREA.
               05  MF100-NYUGAI-TBL  OCCURS 2.
      *∑ÔøÙ
                   07  MF100-KENSU           PIC  9(05).
      *≈¿øÙ
                   07  MF100-TENSU           PIC  9(08).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                   07  MF100-YKZFTN          PIC  9(06).
      *∞Ï…Ù…È√¥∂‚
                   07  MF100-ITBFTN          PIC  9(07).
