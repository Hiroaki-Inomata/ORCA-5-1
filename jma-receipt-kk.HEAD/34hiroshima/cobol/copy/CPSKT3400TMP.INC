      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *    �����������     COPY (CPSKT3400TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SKT3400TMP-REC.
           03      SKT3400TMP-KEY.
             05    SKT3400TMP-SRYYM                 PIC  X(08).
             05    SKT3400TMP-KBN                   PIC  9(01).
             05    SKT3400TMP-SYUTURYOKU            PIC  9(01).
             05    SKT3400TMP-HKNJANUM              PIC  X(08).
           03      SKT3400TMP-DATA.
             05    SKT3400TMP-NYUIN.
               07  SKT3400TMP-N-KENSU               PIC  9(06).
               07  SKT3400TMP-N-TENSU               PIC  9(10).
             05    SKT3400TMP-GAIRAI.
               07  SKT3400TMP-G-KENSU               PIC  9(06).
               07  SKT3400TMP-G-TENSU               PIC  9(10).
