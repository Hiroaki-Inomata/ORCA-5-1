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
      *         
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *��ס����̶�ʬ
               05  MF100-KBN           PIC  9(01).
      *��Į¼�������ȹ��ʬ
               05  MF100-CITYKBN       PIC  9(01).
      *����ǯ��
               05  MF100-HKNJANUM      PIC  X(06).
      *�ǡ������ꥢ
           03  MF100-SEIKYU-AREA.
      *����ʬ
               05  MF100-NYUGAI-TBL OCCURS 2.
                   07  MF100-KENSU     PIC  9(05).
                   07  MF100-NISSU     PIC  9(05).
                   07  MF100-TENSU     PIC  9(09).
                   07  MF100-ITBFTN    PIC  9(09).