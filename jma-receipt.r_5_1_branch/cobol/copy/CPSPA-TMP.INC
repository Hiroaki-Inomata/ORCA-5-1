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
      *    �ӣУ�����ǡ�����  COPY  (CPSPA-TMP.INC)             *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SPATMP-REC.
           03  SPATMP-KEY.
               05  SPATMP-HOSPNUM           PIC 9(02).
      *��̳�ɣ�
               05  SPATMP-GYOUMUID          PIC X(04).
      *ü���ɣ�
               05  SPATMP-TERMID            PIC X(64).
      *�ե�����̾
               05  SPATMP-FILEMEI           PIC X(20).
      *����
               05  SPATMP-EDANUM            PIC 9(03).
      *�ǡ����ΰ�
           03  SPATMP-DATA-REC              PIC X(600000).
      *
      *����ǯ����
           03  SPATMP-UPYMD                 PIC X(08).
      *��������
           03  SPATMP-UPHMS                 PIC X(06).