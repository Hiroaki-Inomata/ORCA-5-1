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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������     ����      ����
      * 01.00.01     ���       19/01/10  �������򥳥��Ⱥ��
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *�������ô���ֹ�
               05  MF100-FTNJANUM         PIC  X(05).
               05  MF100-FTNJANUM2        PIC  X(05).
      *���ݡ����ݶ�ʬ
               05  MF100-KBN              PIC  X(01).
      *�������ݤΣ����ܤ�ɽ���ե饰
               05  MF100-KENGAI           PIC  9(01).
      *����ơ��֥�
           03  MF100-SEIKYU-TBL OCCURS 3.
      *���ճ��
               05  MF100-KYURATE          PIC  9(02).
      *�������٥ơ��֥�
               05  MF100-KOUOHI-TBL OCCURS 3.
      *���
                   07  MF100-KENSU            PIC  9(04).
      *����
                   07  MF100-NISSU            PIC  9(04).
      *ʡ�����������
                   07  MF100-TENSU            PIC  9(08).
      *�����㡦����ճ�����
                   07  MF100-KYUFUGAI         PIC  9(08).
      *����ʬ
                   07  MF100-YKZFTN           PIC  9(08).
      *������ôʬ
                   07  MF100-ITBFTN           PIC  9(07).
      *���
                   07  MF100-GOKEI            PIC  9(07).
