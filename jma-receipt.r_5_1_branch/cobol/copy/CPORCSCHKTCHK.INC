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
      *    ���Ź԰������ť������ѥ�᥿
      *                                      COPY (CPORCSCHKTCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSCHKTCHKAREA.
      *    ������ʬ�ʣ������Υ����å���������ưȯ��ʬ�����å���
           03  ORCSCHKTCHK-SYORI          PIC X(01).
      *    �У�̾
           03  ORCSCHKTCHK-PG             PIC X(06).
      *
           03  ORCSCHKTCHK-OUTAREA.
      *        ����оݤ���
               05  ORCSCHKTCHK-HKTFLG     PIC 9(01).
      *        ���顼����
               05  ORCSCHKTCHK-HKTERR     PIC 9(01).
      *        ����������ʤ�
               05  ORCSCHKTCHK-NYUERR     PIC 9(01).
      *H29.1
      *        �������������ʤ�
               05  ORCSCHKTCHK-NYUERR-2     PIC 9(01).
      *    ������������
           03  ORCSCHKTCHK-NYUINAREA.
               05  ORCSCHKTCHK-NYUHKTCHK       PIC 9(01).
      *        ������������ʬ
               05  ORCSCHKTCHK-NYUHKTCD        PIC X(03).
      *        ��������������������
               05  ORCSCHKTCHK-NYUSRYCD        PIC X(09).
               05  ORCSCHKTCHK-NYUHKT-G.
                   07  ORCSCHKTCHK-NYUHKT-OCC  OCCURS   31.
      *                �����㤦��硢����Ʊ�����
                       09  ORCSCHKTCHK-NYUHKTFLG   PIC 9(01).
      *                ��������⻻����
                       09  ORCSCHKTCHK-DEKIDAFLG   PIC 9(01).
      *����ޤȤỻ�������ΰ�
           03  ORCSCHKTCHK-KT01-AREA.
               05  ORCSCHKTCHK-KT01FLG         PIC X(01).
               05  ORCSCHKTCHK-KT01-OCC        OCCURS   100.
                   07  ORCSCHKTCHK-KT01-SRYCD  PIC X(09).
                   07  ORCSCHKTCHK-KT01-DAY-G.
                       09  ORCSCHKTCHK-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *