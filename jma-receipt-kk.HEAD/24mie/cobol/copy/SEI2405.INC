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
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2019/05/23   �������򥳥��Ⱥ��
      ************************************************************
           03  MF100-KEY.
      *���ŵ��أɣ�
               05  MF100-HOSPNUM         PIC  X(02).
      *��ô���ֹ�
               05  MF100-CITYNUM         PIC  X(03).
      *�����ʬ
               05  MF100-KBN             PIC  X(01).
      *�����ֹ�
               05  MF100-PTNUM           PIC  X(20).
      *���Ų�
               05  MF100-SRYKA           PIC  X(02).
      *����ǯ��
               05  MF100-SRYYM           PIC  X(06).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *����ǡ������ꥢ
           03  MF100-SEIKYU-AREA.
      *����ơ��֥�
               05  MF100-SEIKYU-TBL OCCURS 6.
      *    ���
                   07  MF100-KENSU       PIC 9(05).
