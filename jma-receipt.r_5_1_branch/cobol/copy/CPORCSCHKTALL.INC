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
      *----------------------------------------------------------*
      *    �����������ť������ѥ�᥿
      *                                      COPY (CPORCSCHKTALL.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSCHKTALLAREA.
      *    ������ʬ�ʣ��������������å���
           03  ORCSCHKTALL-KBN             PIC X(01).
      *    ��������
           03  ORCSCHKTALL-SRYYMD.
               05  ORCSCHKTALL-SRYYM       PIC X(06).
               05  ORCSCHKTALL-SRYDD       PIC X(02).
      *    ���ԣɣ�
           03  ORCSCHKTALL-PTID            PIC 9(10).
      *    �оݤο��Ŷ�ʬ
           03  ORCSCHKTALL-HKTSRYKBN       PIC X(02).
           03  ORCSCHKTALL-HKTSRYSYUKBN    PIC X(03).
      *    �оݤο��ť�����
           03  ORCSCHKTALL-HKTSRYCD-G.
               05  ORCSCHKTALL-HKTSRYCD    PIC X(09)
                                           OCCURS   10.
      *
           03  ORCSCHKTALL-OUTAREA.
      *        ������������礢��
               05  ORCSCHKTALL-NYUHKTKBN       PIC 9(01).
      *        �����������礢��
               05  ORCSCHKTALL-NYUHKTCHK       PIC 9(01).
               05  ORCSCHKTALL-NYUHKT-G.
                   07  ORCSCHKTALL-NYUHKT-OCC  OCCURS   31.
      *                �������ʤ���������礢�ꡢ������票�顼
                       09  ORCSCHKTALL-HKTCHKFLG   PIC 9(01).
      *                ����ʬ
                       09  ORCSCHKTALL-NYUHKTCD    PIC X(03).
      *        ����������̵��
               05   ORCSCHKTALL-NYUERR         PIC 9(01).
      *
