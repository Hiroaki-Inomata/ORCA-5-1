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
      *    ���Ժǽ��象���Խ��ѥ�᥿                            *
      *                         COPY  (CPORCSLASTYMD.INC)        *
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSLASTYMDAREA.
      *
      *    ���ԣɣ�
           03  ORCSLASTYMD-PTID                PIC 9(10).
      *    ���Ų�̾������ʣ������ꤹ���
           03  ORCSLASTYMD-SRYKAFLG            PIC X(01).
      *    ���Ϲ���
           03  ORCSLASTYMD-OUT.
      *        �ǽ��象���������
               05  ORCSLASTYMD-LASTYMD         PIC X(08).
      *        �ǽ��象���������
               05  ORCSLASTYMD-LASTYMDW        PIC X(09).
      *        ������Ų�
               05  ORCSLASTYMD-SRYKA           PIC X(02).
      *        ������Ų�̾
               05  ORCSLASTYMD-SRYKA-MEI       PIC X(10).
      *        �ݸ��ȹ礻
               05  ORCSLASTYMD-HKNCOMBI        PIC X(04).
      *        �ݸ��ʸ����
               05  ORCSLASTYMD-HKNNUM          PIC X(03).
      *        �ݸ��ʸ����̾��
               05  ORCSLASTYMD-HKNCOMBI-MEI    PIC X(20).
      *        �ܿͲ�²̾
               05  ORCSLASTYMD-HONKZKKBN       PIC X(02).
      *        ����ɥ�����
               05  ORCSLASTYMD-DRCD            PIC X(05).
      *        ����ʥ����ƥ������������
               05  ORCSLASTYMD-BRMNUM          PIC X(09).
      *        �ǽ�������
               05  ORCSLASTYMD-NYUINYMD        PIC X(08).
      *        �ǽ��ౡ��
               05  ORCSLASTYMD-TAIINYMD        PIC X(08).
      *
