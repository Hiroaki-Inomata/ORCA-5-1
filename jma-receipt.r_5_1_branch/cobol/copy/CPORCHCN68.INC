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
      * 113-8681, Japan.
      ********************************************************************
      *-------------------------------------------------------------*
      *    ���Ŷ�ʬ�����ģ̶�ʬɾ��ɼ�ѥ�᥿  COPY (CPORCHCN68.INC)
      *-------------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHCN68AREA.
      *    ������ʬ��0:Ƭ��/1:���ư�����
           03  ORCHCN68-PRTKBN         PIC X(01).
      *    ����ǯ��
           03  ORCHCN68-SRYYM          PIC X(06).
      *    ���ԣɣ�
           03  ORCHCN68-PTID           PIC 9(10).
      *    �����ֹ�
           03  ORCHCN68-PTNUM          PIC X(20).
      *    Ϣ��
           03  ORCHCN68-CNT            PIC 9(06).
      *    �Хå���������(BATCH="1")
           03  ORCHCN68-BATCH          PIC X(1).
           03  ORCHCN68-PRTAREA        PIC X(25000).

